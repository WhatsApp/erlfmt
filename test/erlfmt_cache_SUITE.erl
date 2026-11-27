%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Properties of erlfmt_cache's mechanisms: whatever happens to a file and
%% whichever cache answers, an answer is what computing would give; the store
%% keeps what it must; a corrupt file reads as nothing.
-module(erlfmt_cache_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").

-export([suite/0, all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
    every_answer_is_what_computing_gives/1,
    a_same_size_write_keeping_mtime_is_seen/1,
    another_cache_finds_a_result_by_key_and_content/1,
    holds_only_outside_the_racy_window/1,
    store_keeps_every_key_used_within_retention/1,
    store_commit_without_news_changes_nothing/1,
    store_over_the_cap_drops_oldest_but_never_newest/1,
    any_corruption_reads_as_nothing/1,
    different_terms_have_different_digests/1,
    prune_keeps_the_most_recently_written/1,
    a_run_that_changed_nothing_writes_nothing/1
]).

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [
        every_answer_is_what_computing_gives,
        a_same_size_write_keeping_mtime_is_seen,
        another_cache_finds_a_result_by_key_and_content,
        holds_only_outside_the_racy_window,
        store_keeps_every_key_used_within_retention,
        store_commit_without_news_changes_nothing,
        store_over_the_cap_drops_oldest_but_never_newest,
        any_corruption_reads_as_nothing,
        different_terms_have_different_digests,
        prune_keeps_the_most_recently_written,
        a_run_that_changed_nothing_writes_nothing
    ].

init_per_testcase(Case, Config) ->
    Dir = filename:join(?config(priv_dir, Config), Case),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    [{dir, Dir} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Per-file results

%% The contents a file is written with: two of the same size, so a write
%% shows only in timestamps.
-define(CONTENTS, [<<"aaaa">>, <<"bbbb">>, <<"cc">>]).

%% A file, as the property simulates it: its content and its stat's fields.
%% Time is simulated too, in whole seconds, as erlfmt reads timestamps: many
%% steps happen in the same second, which is what the racy window is for.
-record(model, {
    %% The simulated clock.
    now = 1000000 :: integer(),
    content = <<"aaaa">> :: binary(),
    mtime = 1000000 :: integer(),
    ctime = 1000000 :: integer(),
    %% Which cache the next run opens: a new one forgets the last run, as
    %% another run shape would, so only the store can answer.
    name = 0 :: non_neg_integer(),
    %% When a run may keep a file's last result without reading it.
    strategy = metadata :: erlfmt_cache:strategy(),
    %% Counts the answers from a stat, across every case: the property must
    %% exercise them.
    by_stat :: counters:counters_ref(),
    %% Answers that weren't what computing gives.
    wrong = [] :: [term()]
}).

%% What a tool computes for `Content' under `Key': every answer must equal it.
%% It differs by key, so an answer for another key is caught.
compute(Key, <<First, _/binary>>) ->
    (Key + First) rem 2 =:= 0.

step() ->
    proper_types:frequency([
        {3, {advance, proper_types:range(0, 3)}},
        {2, {write, proper_types:oneof(?CONTENTS)}},
        {2, {write_keeping_mtime, proper_types:oneof(?CONTENTS)}},
        {4, {run, proper_types:range(0, 1)}},
        {1, forget}
    ]).

%% With either strategy, through writes, same-size writes that put the mtime
%% back, runs under other keys and runs of other caches: every answer, from a
%% stat, the last run or the store, is what computing gives, and the content
%% strategy never answers from a stat.
every_answer_is_what_computing_gives(Config) ->
    Dir = ?config(dir, Config),
    ByStat = counters:new(1, []),
    Property = ?FORALL(
        {Steps, Strategy},
        {proper_types:list(step()), proper_types:oneof([metadata, content])},
        begin
            CaseDir = filename:join(Dir, integer_to_list(erlang:unique_integer([positive]))),
            Model0 = #model{strategy = Strategy, by_stat = ByStat},
            #model{wrong = Wrong} = lists:foldl(
                fun(Step, Model) -> simulate(CaseDir, Step, Model) end, Model0, Steps
            ),
            ?WHENFAIL(ct:pal("wrong answers: ~p", [Wrong]), Wrong =:= [])
        end
    ),
    ?assert(proper:quickcheck(Property, [{numtests, 1000}, {max_size, 40}, {to_file, user}])),
    ?assert(counters:get(ByStat, 1) > 0, "no answer came from a stat").

simulate(_Dir, {advance, Seconds}, #model{now = Now} = Model) ->
    Model#model{now = Now + Seconds};
simulate(_Dir, {write, Content}, #model{now = Now} = Model) ->
    Model#model{content = Content, mtime = Now, ctime = Now};
simulate(_Dir, {write_keeping_mtime, Content}, #model{now = Now} = Model) ->
    Model#model{content = Content, ctime = Now};
simulate(_Dir, forget, #model{name = Name} = Model) ->
    Model#model{name = Name + 1};
simulate(Dir, {run, Key}, #model{wrong = Wrong} = Model) ->
    case run(Dir, Key, Model) of
        ok -> Model;
        Answer -> Model#model{wrong = [Answer | Wrong]}
    end.

%% One run, started now: every answer, from the stat, the last run or the
%% store, is what computing gives.
run(Dir, Key, #model{now = Now, content = Content, name = Name, strategy = Strategy} = Model) ->
    Options = #{started_at => Now, strategy => Strategy},
    Cache0 = erlfmt_cache:open(Dir, integer_to_list(Name), Options),
    Expected = compute(Key, Content),
    CacheKey = erlfmt_cache:digest_term(Key),
    Stat = stat(Model),
    {Answer, Cache} =
        case erlfmt_cache:get_by_stat(Cache0, <<"file">>, CacheKey, Stat) of
            {ok, _} when Strategy =:= content ->
                {content_strategy_trusted_a_stat, Cache0};
            {ok, Expected} ->
                counters:add(Model#model.by_stat, 1, 1),
                {ok, Cache0};
            {ok, Stale} ->
                {{stale_by_stat, Stale}, Cache0};
            miss ->
                Cache1 = erlfmt_cache:load_store(Cache0),
                Digest = erlfmt_cache:digest(Content),
                case erlfmt_cache:get_by_content(Cache1, <<"file">>, CacheKey, Stat, Digest) of
                    {ok, Expected} ->
                        {ok, Cache1};
                    {ok, Wrong} ->
                        {{wrong_by_content, Wrong}, Cache1};
                    miss ->
                        erlfmt_cache:put(Cache1, <<"file">>, CacheKey, Stat, Digest, Expected),
                        {ok, Cache1}
                end
        end,
    erlfmt_cache:save(Cache),
    Answer.

stat(#model{content = Content, mtime = Mtime, ctime = Ctime}) ->
    erlfmt_cache:stat_of(#file_info{
        size = byte_size(Content),
        mtime = Mtime,
        ctime = Ctime,
        inode = 1,
        major_device = 1,
        mode = 8#100644
    }).

%% The case the property finds only sometimes, on a real file: a same-size
%% write that puts the mtime back, after a stat was trusted. Only the ctime
%% tells. Waits out the racy window, as timestamps are read in seconds.
a_same_size_write_keeping_mtime_is_seen(Config) ->
    Dir = ?config(dir, Config),
    File = filename:join(Dir, "f"),
    ok = file:write_file(File, <<"aaaa">>),
    timer:sleep(3000),
    {Key, Content} = {erlfmt_cache:digest_term(1), erlfmt_cache:digest(<<"aaaa">>)},
    {ok, Stat} = erlfmt_cache:stat(File),
    First = erlfmt_cache:open(Dir, "files", #{}),
    ok = erlfmt_cache:put(First, <<"f">>, Key, Stat, Content, 1),
    ok = erlfmt_cache:save(First),
    Second = erlfmt_cache:open(Dir, "files", #{}),
    ?assertEqual({ok, 1}, erlfmt_cache:get_by_stat(Second, <<"f">>, Key, Stat)),
    {ok, #file_info{mtime = Mtime}} = file:read_file_info(File, [{time, posix}]),
    ok = file:write_file(File, <<"bbbb">>),
    ok = file:write_file_info(File, #file_info{mtime = Mtime, atime = Mtime}, [{time, posix}]),
    {ok, Changed} = erlfmt_cache:stat(File),
    ?assertEqual(miss, erlfmt_cache:get_by_stat(Second, <<"f">>, Key, Changed)),
    ok = erlfmt_cache:save(Second).

another_cache_finds_a_result_by_key_and_content(Config) ->
    Dir = ?config(dir, Config),
    File = filename:join(Dir, "f"),
    ok = file:write_file(File, <<"x">>),
    {ok, Stat} = erlfmt_cache:stat(File),
    Content = erlfmt_cache:digest(<<"x">>),
    {One, Two} = {erlfmt_cache:digest_term(1), erlfmt_cache:digest_term(2)},
    A = erlfmt_cache:open(Dir, "a", #{}),
    ok = erlfmt_cache:put(A, <<"f">>, One, Stat, Content, 7),
    ok = erlfmt_cache:save(A),
    ?assertMatch(
        [#{path := <<"f">>, key := One, content := Content, result := 7}],
        erlfmt_cache:entries(erlfmt_cache:open(Dir, "a", #{}))
    ),
    B = erlfmt_cache:load_store(erlfmt_cache:open(Dir, "b", #{})),
    ?assertEqual([], erlfmt_cache:entries(B)),
    ?assertEqual({ok, 7}, erlfmt_cache:get_by_content(B, <<"f">>, One, Stat, Content)),
    ?assertEqual(miss, erlfmt_cache:get_by_content(B, <<"g">>, One, Stat, Content), "another path"),
    ?assertEqual(miss, erlfmt_cache:get_by_content(B, <<"f">>, Two, Stat, Content), "another key"),
    Other = erlfmt_cache:digest(<<"y">>),
    ?assertEqual(
        miss, erlfmt_cache:get_by_content(B, <<"f">>, One, Stat, Other), "another content"
    ),
    ok = erlfmt_cache:save(B).

holds_only_outside_the_racy_window(_Config) ->
    At = 1000,
    Old = At - 3,
    Stat = fun(Mtime, Ctime) ->
        erlfmt_cache:stat_of(#file_info{
            size = 1, mtime = Mtime, ctime = Ctime, inode = 1, major_device = 1, mode = 0
        })
    end,
    Seen = fun(S) -> erlfmt_cache:observe(S, At) end,
    ?assert(erlfmt_cache:holds(Seen(Stat(Old, Old)), Stat(Old, Old))),
    ?assertNot(erlfmt_cache:holds(Seen(Stat(Old, Old)), Stat(Old, Old + 1)), "a different stat"),
    ?assertNot(erlfmt_cache:holds(Seen(Stat(At - 2, Old)), Stat(At - 2, Old)), "a recent mtime"),
    ?assertNot(erlfmt_cache:holds(Seen(Stat(Old, At - 1)), Stat(Old, At - 1)), "a recent ctime"),
    ?assertNot(erlfmt_cache:holds(Seen(Stat(At + 5, Old)), Stat(At + 5, Old)), "after observation").

a_run_that_changed_nothing_writes_nothing(Config) ->
    Dir = ?config(dir, Config),
    Stat = erlfmt_cache:stat_of(#file_info{
        size = 1, mtime = 10, ctime = 10, inode = 1, major_device = 1, mode = 0
    }),
    Key = erlfmt_cache:digest_term(key),
    Content = erlfmt_cache:digest(<<"a">>),
    First = erlfmt_cache:load_store(erlfmt_cache:open(Dir, "runs/a", #{started_at => 100})),
    miss = erlfmt_cache:get_by_content(First, <<"a">>, Key, Stat, Content),
    erlfmt_cache:put(First, <<"a">>, Key, Stat, Content, formatted),
    erlfmt_cache:save(First),
    Written = file:read_file_info(filename:join(Dir, "runs/a")),
    ok = file:delete(filename:join(Dir, "store")),
    Second = erlfmt_cache:open(Dir, "runs/a", #{started_at => 200}),
    ?assertEqual({ok, formatted}, erlfmt_cache:get_by_stat(Second, <<"a">>, Key, Stat)),
    erlfmt_cache:save(Second),
    ?assertEqual(Written, file:read_file_info(filename:join(Dir, "runs/a"))),
    ?assertEqual({error, enoent}, file:read_file_info(filename:join(Dir, "store"))).

%%--------------------------------------------------------------------
%% The store

-define(GENERATIONS, 128).
-define(SLACK, (?GENERATIONS div 4)).
-define(KEYS, 64).

key(N) ->
    erlfmt_cache:digest_term(N).

%% A run: the keys it answered from, and those it found; and how many other
%% runs, of other shapes, committed before it.
commit() ->
    Keys = proper_types:resize(3, proper_types:list(proper_types:range(0, ?KEYS - 1))),
    {Keys, Keys, proper_types:range(0, 47)}.

%% For any sequence of runs: a key is kept while it was added or used (while
%% kept) within the last `generations', a key never added is never kept, and
%% a commit that says nothing changed changed nothing.
store_keeps_every_key_used_within_retention(_Config) ->
    Retention = erlfmt_cache:retention(?GENERATIONS, 64 bsl 20),
    Property = ?FORALL(
        Runs,
        proper_types:list(commit()),
        begin
            Result = lists:foldl(
                fun(Run, {Store, Generation, Touched, Wrong}) ->
                    commit_run(Run, Retention, Store, Generation, Touched, Wrong)
                end,
                {erlfmt_cache:new_store(), 0, #{}, []},
                Runs
            ),
            Wrong = element(4, Result),
            ?WHENFAIL(ct:pal("wrong: ~p", [Wrong]), Wrong =:= [])
        end
    ),
    ?assert(proper:quickcheck(Property, [{numtests, 300}, {max_size, 400}, {to_file, user}])).

%% Commits `Run' after `Others' commits of other runs, which age the keys
%% without touching them, and checks every key afterwards.
commit_run({Used, Added, Others}, Retention, Store0, Generation0, Touched0, Wrong) ->
    {Store1, Generation1} = age(Store0, Generation0, Others, Retention),
    KeptUsed = [K || K <- Used, erlfmt_cache:store_get(Store1, key(K)) =/= error],
    Commit = erlfmt_cache:new_generation([key(K) || K <- Used], [{key(K), K} || K <- Added]),
    {Store, Generation} =
        case erlfmt_cache:store_commit(Store1, Commit, Retention) of
            {changed, Committed} -> {Committed, Generation1 + 1};
            unchanged -> {Store1, Generation1}
        end,
    Touched = maps:merge(Touched0, maps:from_list([{K, Generation} || K <- KeptUsed ++ Added])),
    Violations = [
        {K, Generation, maps:get(K, Touched, never)}
     || K <- lists:seq(0, ?KEYS - 1),
        violates(erlfmt_cache:store_get(Store, key(K)), maps:get(K, Touched, never), Generation)
    ],
    {Store, Generation, Touched, Violations ++ Wrong}.

violates(error, never, _Generation) -> false;
violates({ok, _}, never, _Generation) -> true;
violates(error, Touched, Generation) -> Generation - Touched < ?GENERATIONS;
violates({ok, _}, _Touched, _Generation) -> false.

%% Commits `N' runs that each add a key of their own, outside the property's
%% key space: the store's generation moves on as other run shapes would move it.
age(Store, Generation, 0, _Retention) ->
    {Store, Generation};
age(Store0, Generation, N, Retention) ->
    Other = key({other, erlang:unique_integer()}),
    Commit = erlfmt_cache:new_generation([], [{Other, other}]),
    {changed, Store} = erlfmt_cache:store_commit(Store0, Commit, Retention),
    age(Store, Generation + 1, N - 1, Retention).

store_commit_without_news_changes_nothing(_Config) ->
    Retention = erlfmt_cache:retention(?GENERATIONS, 64 bsl 20),
    {changed, Store} = erlfmt_cache:store_commit(
        erlfmt_cache:new_store(), erlfmt_cache:new_generation([], [{key(1), a}]), Retention
    ),
    ?assertEqual(
        unchanged,
        erlfmt_cache:store_commit(Store, erlfmt_cache:new_generation([key(1)], []), Retention)
    ),
    ?assertEqual(
        unchanged,
        erlfmt_cache:store_commit(Store, erlfmt_cache:new_generation([], [{key(1), a}]), Retention),
        "a key already kept and fresh isn't new"
    ),
    ?assertMatch(
        {changed, _},
        erlfmt_cache:store_commit(Store, erlfmt_cache:new_generation([], [{key(1), b}]), Retention),
        "a new value is"
    ).

store_over_the_cap_drops_oldest_but_never_newest(_Config) ->
    Value = binary:copy(<<7>>, 100),
    Bytes = erlfmt_cache:store_bytes(
        element(
            2,
            erlfmt_cache:store_commit(
                erlfmt_cache:new_store(),
                erlfmt_cache:new_generation([], [{key(0), Value}]),
                erlfmt_cache:retention(8, 1 bsl 20)
            )
        )
    ),
    Retention = erlfmt_cache:retention(8, 3 * Bytes),
    Store = lists:foldl(
        fun(K, Store0) ->
            Commit = erlfmt_cache:new_generation([], [{key(K), Value}]),
            {changed, Store1} = erlfmt_cache:store_commit(Store0, Commit, Retention),
            Store1
        end,
        erlfmt_cache:new_store(),
        lists:seq(0, 4)
    ),
    ?assertEqual({ok, Value}, erlfmt_cache:store_get(Store, key(4))),
    ?assertEqual(error, erlfmt_cache:store_get(Store, key(0))),
    ?assert(erlfmt_cache:store_bytes(Store) =< 3 * Bytes),
    Huge = binary:copy(<<7>>, 10 * Bytes),
    {changed, Kept} = erlfmt_cache:store_commit(
        Store, erlfmt_cache:new_generation([], [{key(huge), Huge}]), Retention
    ),
    ?assertEqual({ok, Huge}, erlfmt_cache:store_get(Kept, key(huge)), "the newest stays").

%%--------------------------------------------------------------------
%% Files and digests

%% Whatever bytes of a file change, or however it is cut short, it reads as
%% nothing: a corrupt cache is a miss, never a wrong value.
any_corruption_reads_as_nothing(Config) ->
    Dir = ?config(dir, Config),
    Property = ?FORALL(
        {Value, Flip, Cut},
        {
            {proper_types:list(proper_types:binary()), proper_types:integer(), proper_types:bool()},
            {proper_types:non_neg_integer(), proper_types:range(1, 255)},
            proper_types:non_neg_integer()
        },
        begin
            ok = erlfmt_cache:write(Dir, "value", Value),
            Read = erlfmt_cache:read(Dir, "value"),
            Path = filename:join(Dir, "value"),
            {ok, Bytes} = file:read_file(Path),
            ok = file:write_file(Path, flip(Bytes, Flip)),
            Flipped = erlfmt_cache:read(Dir, "value"),
            ok = file:write_file(Path, binary:part(Bytes, 0, Cut rem byte_size(Bytes))),
            Truncated = erlfmt_cache:read(Dir, "value"),
            Read =:= {ok, Value} andalso Flipped =:= error andalso Truncated =:= error
        end
    ),
    ?assert(proper:quickcheck(Property, [{numtests, 300}, {to_file, user}])).

flip(Bytes, {At0, Mask}) ->
    At = At0 rem byte_size(Bytes),
    <<Before:At/binary, Byte, After/binary>> = Bytes,
    <<Before/binary, (Byte bxor Mask), After/binary>>.

%% Keys are made of several fields: no two different terms share a digest,
%% however their bytes line up.
different_terms_have_different_digests(_Config) ->
    Term = {proper_types:binary(), proper_types:list(proper_types:byte()), proper_types:integer()},
    Property = ?FORALL(
        {A, B},
        {Term, Term},
        (erlfmt_cache:digest_term(A) =:= erlfmt_cache:digest_term(B)) =:= (A =:= B)
    ),
    ?assert(proper:quickcheck(Property, [{numtests, 1000}, {to_file, user}])),
    ?assertNotEqual(erlfmt_cache:digest_term({"ab", "c"}), erlfmt_cache:digest_term({"a", "bc"})).

prune_keeps_the_most_recently_written(Config) ->
    Dir = ?config(dir, Config),
    Keep = 8,
    [
        begin
            Name = filename:join("runs", integer_to_list(I)),
            ok = erlfmt_cache:write(Dir, Name, I),
            ok = file:change_time(filename:join(Dir, Name), {{2020, 1, 1}, {0, 0, I}})
        end
     || I <- lists:seq(1, Keep + 5)
    ],
    ok = erlfmt_cache:prune(Dir, "runs", Keep),
    {ok, Left} = file:list_dir(filename:join(Dir, "runs")),
    ?assertEqual(Keep, length(Left)),
    ?assertEqual(error, erlfmt_cache:read(Dir, "runs/5"), "the oldest go"),
    ?assertEqual({ok, Keep + 5}, erlfmt_cache:read(Dir, "runs/" ++ integer_to_list(Keep + 5))).
