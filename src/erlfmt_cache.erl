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

%% Per-file results kept across runs, so a later run only redoes what changed.
%%
%% These are the mechanisms; what to cache, under which keys and where is the
%% caller's policy (see erlfmt_cli).
%%
%% - A file whose `stat' is the one recorded, outside the racy window, keeps
%%   its last result without being read (get_by_stat/4).
%% - A file that changed is read and hashed by the caller, and keeps a result
%%   put for the same key, path and content: by the last run, or by any run
%%   whose cache is in the same directory, through a content-addressed store
%%   (get_by_content/5).
%% - The store is bounded by generations and by size (store_commit/3).
%% - Files are written atomically behind a sha256 checksum: anything missing,
%%   corrupt or unwritable is a miss, never a wrong answer.
%%
%% A cache is opened by one process, which saves it; any process may look up
%% and put results in between.
-module(erlfmt_cache).

-export([
    available/0,
    digest/1,
    digest_term/1,
    build_id/1,
    stat/1,
    stat_of/1,
    observe/2,
    holds/2,
    open/3,
    get_by_stat/4,
    load_store/1,
    get_by_content/5,
    put/6,
    save/1,
    read/2,
    write/3,
    prune/3,
    retention/2,
    new_generation/2,
    new_store/0,
    store_get/2,
    store_commit/3,
    store_bytes/1
]).

-export_type([
    digest/0,
    seconds/0,
    stat/0,
    observation/0,
    path/0,
    cache/0,
    retention/0,
    generation/0,
    store/0
]).

-include_lib("kernel/include/file.hrl").

%% A stat taken within this many seconds of a file's own mtime or ctime is
%% never trusted: a write in the same timestamp tick wouldn't show. The same
%% rule as Git's racy index (https://git-scm.com/docs/racy-git).
-define(RACY_WINDOW, 2).

%% Where a cache's directory keeps every result by key and content, shared
%% by all the caches in it.
-define(STORE, "store").

%% A sha256 digest.
-type digest() :: <<_:256>>.

%% Seconds since the Unix epoch, as file timestamps are given.
-type seconds() :: integer().

%% A path as a cache names it: valid UTF-8, so it can be stored and compared.
-type path() :: binary().

%% A file's identity: whatever changes when it is written, moved or chmoded.
-record(stat, {
    size :: non_neg_integer(),
    mtime :: seconds(),
    ctime :: seconds(),
    inode :: non_neg_integer(),
    device :: non_neg_integer(),
    mode :: non_neg_integer()
}).
-opaque stat() :: #stat{}.

%% A stat, and when it was taken: the start of the run that took it.
-record(observation, {
    stat :: #stat{},
    at :: seconds()
}).
-opaque observation() :: #observation{}.

%% A file's result, as a run put it.
-record(entry, {
    %% The path, as the caller names it.
    path :: path(),
    %% Everything the result depends on besides the file's path and content.
    key :: digest(),
    %% The file's stat when it was read.
    seen :: #observation{},
    %% The digest of the content the result is for.
    content :: digest(),
    %% What the caller computed for that content, under that key.
    result :: term()
}).

%% How long a store keeps keys, and how big it may grow.
-record(retention, {
    %% Keys no run of this many generations used are dropped.
    generations = 128 :: pos_integer(),
    %% Past this size, the oldest generations' keys are dropped, never the newest's.
    max_bytes = 64 bsl 20 :: pos_integer()
}).
-opaque retention() :: #retention{}.

%% What a run used and found, to commit to a store as its next generation.
-record(generation, {
    %% Keys the run answered from.
    used = [] :: [digest()],
    %% Keys the run found, with their values.
    added = [] :: [{digest(), term()}]
}).
-opaque generation() :: #generation{}.

%% Content-addressed values, each with the newest generation that used it.
-record(store, {
    generation = 0 :: non_neg_integer(),
    keys = #{} :: #{digest() => {Stamp :: non_neg_integer(), Value :: term()}}
}).
-opaque store() :: #store{}.

%% An open cache. Its tables are public, so any process can look up and put;
%% the process that opened it owns them and must save it.
-record(cache, {
    %% The directory this cache, its siblings and their shared store are in.
    dir :: file:filename_all(),
    %% The file in `dir' this cache's entries are kept in.
    name :: file:filename_all(),
    %% When the run started: every stat it records counts as taken then.
    started :: seconds(),
    %% How long the store keeps results no run used.
    retention :: #retention{},
    %% The last run's entries: `{Path, Entry, KeptByStat}'.
    last :: ets:tid(),
    %% This run's entries, but those kept by stat: `{Path, Entry}'.
    found :: ets:tid(),
    %% Store keys this run answered from: `{Key}'.
    used :: ets:tid(),
    %% Store keys this run found: `{Key, Value}'.
    added :: ets:tid(),
    %% The store's values by key, once loaded with load_store/1: `{Key, Value}'.
    store = unloaded :: unloaded | ets:tid()
}).
-opaque cache() :: #cache{}.

%%--------------------------------------------------------------------
%% Digests

%% Returns whether the digests this module needs can be computed: they use
%% `crypto', which needs an OTP built with OpenSSL.
-spec available() -> boolean().
available() ->
    try crypto:hash(sha256, <<>>) of
        <<_:256>> -> true
    catch
        error:_ -> false
    end.

%% Returns the digest of `Bytes'.
-spec digest(iodata()) -> digest().
digest(Bytes) ->
    crypto:hash(sha256, Bytes).

%% Returns the digest of `Term''s external format, which is self-delimiting:
%% different terms never hash the same bytes. Maps are encoded in no defined
%% order, so a term to be hashed must not contain any.
-spec digest_term(term()) -> digest().
digest_term(Term) ->
    digest(term_to_binary(Term, [{minor_version, 2}])).

%% Returns the digest of the loaded code of `Modules', loading them if
%% needed: results kept by one build are never used by another.
-spec build_id([module()]) -> digest().
build_id(Modules) ->
    digest_term([{Module, Module:module_info(md5)} || Module <- lists:usort(Modules)]).

%%--------------------------------------------------------------------
%% Stats

%% Returns the stat of the file at `Path', following symbolic links: what is
%% read is the target, so its identity is what matters.
-spec stat(file:name_all()) -> {ok, stat()} | error.
stat(Path) ->
    case file:read_file_info(Path, [raw, {time, posix}]) of
        {ok, Info} -> {ok, stat_of(Info)};
        {error, _} -> error
    end.

%% Returns the stat in `Info', read with `{time, posix}'.
-spec stat_of(#file_info{}) -> stat().
stat_of(#file_info{
    size = Size,
    mtime = Mtime,
    ctime = Ctime,
    inode = Inode,
    major_device = Device,
    mode = Mode
}) when is_integer(Mtime), is_integer(Ctime) ->
    #stat{
        size = Size,
        mtime = Mtime,
        ctime = Ctime,
        inode = Inode,
        device = Device,
        mode = Mode
    }.

%% Returns `Stat', as taken at `At'.
-spec observe(stat(), seconds()) -> observation().
observe(Stat, At) ->
    #observation{stat = Stat, at = At}.

%% Returns whether a file that now has `Current' is the one observed: the same
%% stat, observed late enough after its timestamps that a later change in the
%% same tick would show. A change after the observation always shows in the
%% ctime, which no tool can set back.
-spec holds(observation(), stat()) -> boolean().
holds(#observation{stat = Stat, at = At}, Current) ->
    Stat =:= Current andalso not changed_since(Stat, At - ?RACY_WINDOW).

-spec changed_since(#stat{}, seconds()) -> boolean().
changed_since(#stat{mtime = Mtime, ctime = Ctime}, Time) ->
    Mtime >= Time orelse Ctime >= Time.

%%--------------------------------------------------------------------
%% Per-file results

%% Opens the cache kept in `Name' of `Dir', with the last run's results.
%%
%% Options:
%% - `started_at': when the run started, before any file was read (default:
%%   now). Every stat recorded counts as taken then, so it must not be later.
%% - `retention': how long the store keeps results no run used.
-spec open(file:filename_all(), file:filename_all(), #{
    started_at => seconds(), retention => retention()
}) -> cache().
open(Dir, Name, Options) ->
    Started = maps:get(started_at, Options, os:system_time(second)),
    Last = ets:new(erlfmt_cache_last, [set, public, {read_concurrency, true}]),
    ets:insert(Last, [
        {Path, Entry, false}
     || #entry{path = Path} = Entry <- read_entries(Dir, Name)
    ]),
    #cache{
        dir = Dir,
        name = Name,
        started = Started,
        retention = maps:get(retention, Options, #retention{}),
        last = Last,
        found = ets:new(erlfmt_cache_found, [set, public, {write_concurrency, true}]),
        used = ets:new(erlfmt_cache_used, [set, public, {write_concurrency, true}]),
        added = ets:new(erlfmt_cache_added, [set, public, {write_concurrency, true}])
    }.

%% Returns the last run's result for the file at `Path', which now has `Stat',
%% under `Key', if the file is unchanged by its stat. Such a file needn't be read.
-spec get_by_stat(cache(), path(), digest(), stat()) -> {ok, term()} | miss.
get_by_stat(#cache{last = Last}, Path, Key, Stat) ->
    case ets:lookup(Last, Path) of
        [{Path, #entry{key = Key, seen = Seen, result = Result}, _Kept}] ->
            case holds(Seen, Stat) of
                true ->
                    ets:update_element(Last, Path, {3, true}),
                    {ok, Result};
                false ->
                    miss
            end;
        _ ->
            miss
    end.

%% Reads the store, so get_by_content/5 can answer with results any cache in
%% the directory put. Only needed by a run that looks up by content: a run
%% answered by stat alone never reads it.
-spec load_store(cache()) -> cache().
load_store(#cache{store = unloaded, dir = Dir} = Cache) ->
    #store{keys = Keys} = read_store(Dir),
    Store = ets:new(erlfmt_cache_store, [set, public, {read_concurrency, true}]),
    ets:insert(Store, [{Key, Value} || {Key, {_Stamp, Value}} <- maps:to_list(Keys)]),
    Cache#cache{store = Store};
load_store(Cache) ->
    Cache.

%% Returns the result for the file at `Path', read with `Stat' and hashing to
%% `Content', under `Key': the last run's, or one any run put in the store
%% for that key, path and content. The store must be loaded.
-spec get_by_content(cache(), path(), digest(), stat(), digest()) -> {ok, term()} | miss.
get_by_content(#cache{last = Last, store = Store} = Cache, Path, Key, Stat, Content) when
    Store =/= unloaded
->
    Found =
        case ets:lookup(Last, Path) of
            [{Path, #entry{key = Key, content = Content, result = Result}, _Kept}] ->
                {ok, Result};
            _ ->
                get_from_store(Cache, store_key(Path, Key, Content))
        end,
    case Found of
        {ok, Value} -> push(Cache, Path, Key, Stat, Content, Value);
        miss -> ok
    end,
    Found.

-spec get_from_store(cache(), digest()) -> {ok, term()} | miss.
get_from_store(#cache{store = Store, used = Used}, StoreKey) ->
    case ets:lookup(Store, StoreKey) of
        [{StoreKey, Value}] ->
            ets:insert(Used, {StoreKey}),
            {ok, Value};
        [] ->
            miss
    end.

%% Puts `Result' for the file at `Path', read with `Stat' and hashing to
%% `Content', under `Key': for this cache's next run, and any cache's run.
-spec put(cache(), path(), digest(), stat(), digest(), term()) -> ok.
put(#cache{added = Added} = Cache, Path, Key, Stat, Content, Result) ->
    ets:insert(Added, {store_key(Path, Key, Content), Result}),
    push(Cache, Path, Key, Stat, Content, Result).

%% Saves this run's results, if they aren't the last run's, and commits the
%% results it used and put to the store. The cache is closed: its tables are
%% deleted.
-spec save(cache()) -> ok.
save(#cache{dir = Dir, retention = Retention} = Cache) ->
    save_entries(Cache),
    Generation = generation(Cache),
    case Generation of
        #generation{used = [], added = []} ->
            ok;
        _ ->
            %% Merged into the store as it is on disk now, so runs in
            %% parallel lose as little as possible.
            case store_commit(read_store(Dir), Generation, Retention) of
                {changed, Store} -> write(Dir, ?STORE, Store);
                unchanged -> ok
            end
    end,
    close(Cache).

%% Writes the entries for the next run: those kept by stat, and those found
%% again or put. A run that kept every entry by stat and found nothing writes
%% nothing.
-spec save_entries(cache()) -> ok.
save_entries(#cache{dir = Dir, name = Name, last = Last, found = FoundTable}) ->
    Found = [Entry || {_Path, Entry} <- ets:tab2list(FoundTable)],
    Kept = [Entry || {_Path, Entry, true} <- ets:tab2list(Last)],
    case Found =:= [] andalso length(Kept) =:= ets:info(Last, size) of
        true ->
            ok;
        false ->
            %% A file found again after it was kept is as found.
            Entries = lists:ukeymerge(
                #entry.path,
                lists:ukeysort(#entry.path, Found),
                lists:ukeysort(#entry.path, Kept)
            ),
            write(Dir, Name, Entries)
    end.

-spec generation(cache()) -> generation().
generation(#cache{used = Used, added = Added}) ->
    #generation{
        used = [Key || {Key} <- ets:tab2list(Used)],
        added = ets:tab2list(Added)
    }.

-spec close(cache()) -> ok.
close(#cache{last = Last, found = Found, used = Used, added = Added, store = Store}) ->
    [ets:delete(Table) || Table <- [Last, Found, Used, Added, Store], Table =/= unloaded],
    ok.

%% A result's key in the store, shared by every cache in the directory: the
%% caller's key, the path and the content, so a hit is for the same file,
%% contents and dependencies.
-spec store_key(path(), digest(), digest()) -> digest().
store_key(Path, Key, Content) ->
    digest_term({Key, Path, Content}).

%% Adds this run's entry for a file read with `Stat', observed at the start of the run.
-spec push(cache(), path(), digest(), stat(), digest(), term()) -> ok.
push(#cache{found = Found, started = Started}, Path, Key, Stat, Content, Result) ->
    Entry = #entry{
        path = Path,
        key = Key,
        seen = observe(Stat, Started),
        content = Content,
        result = Result
    },
    ets:insert(Found, {Path, Entry}),
    ok.

%% The entries kept in `Name', or none if they are missing, corrupt or not entries.
-spec read_entries(file:filename_all(), file:filename_all()) -> [#entry{}].
read_entries(Dir, Name) ->
    case read(Dir, Name) of
        {ok, Entries} when is_list(Entries) ->
            case lists:all(fun is_entry/1, Entries) of
                true -> Entries;
                false -> []
            end;
        _ ->
            []
    end.

-spec is_entry(term()) -> boolean().
is_entry(#entry{
    path = Path,
    key = <<_:256>>,
    seen = #observation{stat = #stat{}, at = At},
    content = <<_:256>>
}) ->
    is_binary(Path) andalso is_integer(At);
is_entry(_) ->
    false.

%% The store in `Dir', or an empty one if it is missing or corrupt.
-spec read_store(file:filename_all()) -> #store{}.
read_store(Dir) ->
    case read(Dir, ?STORE) of
        {ok, #store{generation = Generation, keys = Keys} = Store} when
            is_integer(Generation), is_map(Keys)
        ->
            Store;
        _ ->
            new_store()
    end.

%%--------------------------------------------------------------------
%% Files of a cache directory

%% Reads the term in `Name', a path relative to `Dir'. Returns `error' if it is
%% missing or corrupt: a miss, never a wrong value.
-spec read(file:filename_all(), file:filename_all()) -> {ok, term()} | error.
read(Dir, Name) ->
    case file:read_file(filename:join(Dir, Name)) of
        {ok, <<Checksum:32/binary, Payload/binary>>} ->
            case digest(Payload) of
                Checksum -> decode(Payload);
                _ -> error
            end;
        _ ->
            error
    end.

-spec decode(binary()) -> {ok, term()} | error.
decode(Payload) ->
    try binary_to_term(Payload, [safe]) of
        Term -> {ok, Term}
    catch
        error:badarg -> error
    end.

%% Writes `Term' to `Name', a path relative to `Dir', through a temporary file
%% and a rename, so readers see either the old or the new file. Failures are
%% ignored: a cache only saves work.
-spec write(file:filename_all(), file:filename_all(), term()) -> ok.
write(Dir, Name, Term) ->
    Payload = term_to_binary(Term),
    Path = filename:join(Dir, Name),
    Temporary = temporary_name(Path),
    _ = filelib:ensure_dir(Path),
    case file:write_file(Temporary, [digest(Payload), Payload], [raw]) of
        ok ->
            case file:rename(Temporary, Path) of
                ok -> ok;
                {error, _} -> discard(Temporary)
            end;
        {error, _} ->
            discard(Temporary)
    end.

-spec temporary_name(file:filename_all()) -> file:filename_all().
temporary_name(Path) ->
    Unique = io_lib:format(".~s.~b.tmp", [os:getpid(), erlang:unique_integer([positive])]),
    unicode:characters_to_list([Path, Unique]).

-spec discard(file:filename_all()) -> ok.
discard(Path) ->
    _ = file:delete(Path),
    ok.

%% Removes all but the `Keep' most recently written files in `Subdir' of `Dir'.
-spec prune(file:filename_all(), file:filename_all(), non_neg_integer()) -> ok.
prune(Dir, Subdir, Keep) ->
    Path = filename:join(Dir, Subdir),
    case file:list_dir(Path) of
        {ok, Names} when length(Names) > Keep ->
            Files = [
                {modified(filename:join(Path, File)), filename:join(Path, File)}
             || File <- Names
            ],
            {_Newest, Oldest} = lists:split(Keep, lists:reverse(lists:sort(Files))),
            [discard(File) || {_Modified, File} <- Oldest],
            ok;
        _ ->
            ok
    end.

-spec modified(file:filename_all()) -> seconds().
modified(Path) ->
    case file:read_file_info(Path, [raw, {time, posix}]) of
        {ok, #file_info{mtime = Mtime}} -> Mtime;
        {error, _} -> 0
    end.

%%--------------------------------------------------------------------
%% The store

%% Returns a retention keeping keys used within `Generations' runs, in at
%% most about `MaxBytes'.
-spec retention(pos_integer(), pos_integer()) -> retention().
retention(Generations, MaxBytes) ->
    #retention{generations = Generations, max_bytes = MaxBytes}.

%% Returns what a run that answered from the keys `Used' and found `Added'
%% commits to a store.
-spec new_generation([digest()], [{digest(), term()}]) -> generation().
new_generation(Used, Added) ->
    #generation{used = Used, added = Added}.

%% Returns an empty store.
-spec new_store() -> store().
new_store() ->
    #store{}.

%% Returns the value kept for `Key'.
-spec store_get(store(), digest()) -> {ok, term()} | error.
store_get(#store{keys = Keys}, Key) ->
    case Keys of
        #{Key := {_Stamp, Value}} -> {ok, Value};
        _ -> error
    end.

%% Merges `Generation' into `Store' as its next generation, and collects by
%% `Retention'. Returns `unchanged' if nothing changed: then nothing needs
%% writing.
%%
%% A key used within the last quarter of the retention isn't stamped again,
%% so a key every run uses is rewritten once per that many runs; collection
%% keeps that much more, so a key used in one of the last `generations' is
%% never dropped.
-spec store_commit(store(), generation(), retention()) -> {changed, store()} | unchanged.
store_commit(
    #store{generation = Current, keys = Keys0} = Store,
    #generation{used = Used, added = Added},
    Retention
) ->
    Next = Current + 1,
    Stale = max(Next - slack(Retention), 0),
    {Keys1, AddedChanged} = lists:foldl(
        fun({Key, Value}, {Keys, Changed}) ->
            case Keys of
                #{Key := {_Stamp, Value}} -> {Keys, Changed};
                _ -> {Keys#{Key => {Next, Value}}, true}
            end
        end,
        {Keys0, false},
        Added
    ),
    {Keys2, Changed} = lists:foldl(
        fun(Key, {Keys, Changed}) ->
            case Keys of
                #{Key := {Stamp, Value}} when Stamp < Stale -> {Keys#{Key => {Next, Value}}, true};
                _ -> {Keys, Changed}
            end
        end,
        {Keys1, AddedChanged},
        Used ++ [Key || {Key, _Value} <- Added]
    ),
    case Changed of
        true -> {changed, collect(Store#store{generation = Next, keys = Keys2}, Retention)};
        false -> unchanged
    end.

-spec slack(retention()) -> non_neg_integer().
slack(#retention{generations = Generations}) ->
    Generations div 4.

%% Drops keys older than the kept generations, then the oldest generations
%% while over the size cap, always keeping the newest generation's keys.
-spec collect(#store{}, retention()) -> #store{}.
collect(#store{generation = Newest, keys = Keys} = Store, Retention) ->
    #retention{generations = Generations, max_bytes = MaxBytes} = Retention,
    OldestKept = max(Newest - (Generations + slack(Retention)), 0),
    Kept = maps:filter(fun(_Key, {Stamp, _Value}) -> Stamp > OldestKept end, Keys),
    Store#store{keys = shrink(Kept, Newest, MaxBytes)}.

-spec shrink(#{digest() => {non_neg_integer(), term()}}, non_neg_integer(), pos_integer()) ->
    #{digest() => {non_neg_integer(), term()}}.
shrink(Keys, Newest, MaxBytes) ->
    case map_size(Keys) > 0 andalso keys_bytes(Keys) > MaxBytes of
        false ->
            Keys;
        true ->
            Oldest = lists:min([Stamp || {Stamp, _Value} <- maps:values(Keys)]),
            case Oldest >= Newest of
                true ->
                    Keys;
                false ->
                    Younger = maps:filter(fun(_Key, {Stamp, _Value}) -> Stamp > Oldest end, Keys),
                    shrink(Younger, Newest, MaxBytes)
            end
    end.

%% Roughly what a store costs, to bound by its retention's `max_bytes'.
-spec store_bytes(store()) -> non_neg_integer().
store_bytes(#store{keys = Keys}) ->
    keys_bytes(Keys).

-spec keys_bytes(#{digest() => {non_neg_integer(), term()}}) -> non_neg_integer().
keys_bytes(Keys) ->
    maps:fold(
        fun(_Key, {_Stamp, Value}, Bytes) -> Bytes + 32 + 8 + erlang:external_size(Value) end,
        0,
        Keys
    ).
