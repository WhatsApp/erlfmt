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

%% Caching for command-line tools: remember what a run found for each file,
%% so a later run only redoes the files that changed.
%%
%% - Digests: sha256 of file contents, and of the terms keys are made of.
%% - Stats and observations: a file's identity and when it was taken,
%%   trusted only outside its racy window.
%% - The store: content-addressed results, bounded by generations
%%   (its retention).
%% - The cache: per-file results kept by stat or by content (its strategy),
%%   one of them shared through a store.
%% - The cache directory: where it's kept, checksummed and written atomically.
%% - Builds: the running code, so one build's results are never used by
%%   another.
%%
%% What to cache, under which keys, and where, is the caller's (see
%% erlfmt_cli): these are the mechanisms.
%%
%% A cache never changes an answer by failing: anything missing, corrupt or
%% unreadable is a miss, and anything unwritable is skipped.
-module(erlfmt_cache).

-export([
    available/0,
    digest/1,
    digest_term/1,
    stat/1,
    stat_of/1,
    observe/2,
    holds/2,
    open/3,
    entries/1,
    get_by_stat/4,
    load_store/1,
    get_by_content/5,
    put/6,
    save/1,
    retention/2,
    new_generation/2,
    new_store/0,
    store_get/2,
    store_commit/3,
    store_bytes/1,
    read/2,
    write/3,
    prune/3,
    build_id/1
]).

-export_type([
    digest/0,
    seconds/0,
    path/0,
    stat/0,
    observation/0,
    strategy/0,
    entry/0,
    cache/0,
    retention/0,
    generation/0,
    store/0
]).

-include_lib("kernel/include/file.hrl").

%% How long after a change a timestamp may still not tell it from the next
%% change, in seconds: file timestamps are read in whole seconds, and FAT's
%% are 2 s apart.
-define(RACY_WINDOW, 2).

%% Where a cache keeps every result by key, in its directory: shared by the
%% caches there.
-define(STORE, "store").

%%--------------------------------------------------------------------
%% Digests

%% A sha256 digest.
-type digest() :: <<_:256>>.

%% Returns whether digests can be computed: they use `crypto', which needs an
%% OTP built with OpenSSL. Without it, nothing should be cached.
-spec available() -> boolean().
available() ->
    try crypto:hash(sha256, <<>>) of
        <<_:256>> -> true
    catch
        error:_ -> false
    end.

%% Hashes `Bytes'.
-spec digest(iodata()) -> digest().
digest(Bytes) ->
    crypto:hash(sha256, Bytes).

%% Hashes `Term''s external format, which is self-delimiting: two different
%% terms never hash the same bytes. Maps are encoded in no defined order, so a
%% term to be hashed must not contain any.
-spec digest_term(term()) -> digest().
digest_term(Term) ->
    digest(term_to_binary(Term, [{minor_version, 2}])).

%%--------------------------------------------------------------------
%% Stats

%% A point in time, as file timestamps are read: seconds since the Unix epoch.
-type seconds() :: integer().

%% A file's identity and timestamps, as `stat' gives them.
-record(stat, {
    %% The size in bytes.
    size :: non_neg_integer(),
    %% The modification time, which tools can set (and put back).
    mtime :: seconds(),
    %% The inode change time on Unix, which no tool can set; the creation
    %% time on Windows (`file' has no change time there).
    ctime :: seconds(),
    %% The inode number on Unix, `0' elsewhere: a file replaced by a rename
    %% has another.
    inode :: non_neg_integer(),
    %% The device.
    device :: non_neg_integer(),
    %% File type and permission bits.
    mode :: non_neg_integer()
}).
-opaque stat() :: #stat{}.

%% A stat, and when it was taken: the start of the run that took it.
-record(observation, {
    %% The stat taken.
    stat :: #stat{},
    %% When it was taken. A stat is only trusted once its timestamps are
    %% older than this by the racy window.
    at :: seconds()
}).
-opaque observation() :: #observation{}.

%% Returns the stat of the file at `Path', following symbolic links: what a
%% caller reads is the target, so its identity is what tells a change.
%% Returns `error' if there is no file to read there.
-spec stat(file:name_all()) -> {ok, stat()} | error.
stat(Path) ->
    case file:read_file_info(Path, [raw, {time, posix}]) of
        {ok, #file_info{type = regular} = Info} -> {ok, stat_of(Info)};
        _ -> error
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
    #stat{size = Size, mtime = Mtime, ctime = Ctime, inode = Inode, device = Device, mode = Mode}.

%% Returns `Stat', as taken at `At'.
-spec observe(stat(), seconds()) -> observation().
observe(Stat, At) ->
    #observation{stat = Stat, at = At}.

%% Returns whether a file that now has `Current' is the one observed: the same
%% stat, observed late enough after its timestamps that a later change in the
%% same tick would show. Racy, like Git's index:
%% https://git-scm.com/docs/racy-git
-spec holds(observation(), stat()) -> boolean().
holds(#observation{stat = Stat, at = At}, Current) ->
    Stat =:= Current andalso not changed_since(Stat, At - ?RACY_WINDOW).

%% Returns whether the file changed at or after `Time'.
-spec changed_since(#stat{}, seconds()) -> boolean().
changed_since(#stat{mtime = Mtime, ctime = Ctime}, Time) ->
    Mtime >= Time orelse Ctime >= Time.

%%--------------------------------------------------------------------
%% The store
%%
%% A generation is a run that writes the store. Each key carries the newest
%% generation that used it; writing drops the keys no run of the last
%% `generations' used, then the oldest generations while the store is over
%% `max_bytes'.

%% How long a store keeps keys, and how big it may grow.
-record(retention, {
    %% Keys no run of this many generations used are dropped.
    generations = 128 :: pos_integer(),
    %% Past this size, the oldest generations' keys are dropped (never the
    %% newest's).
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

%% A value, and the newest generation that used it.
-record(kept, {
    %% The newest generation that added or used it.
    stamp :: non_neg_integer(),
    %% The result.
    value :: term()
}).

%% Content-addressed values, bounded by generations.
-record(store, {
    %% The newest generation written.
    generation = 0 :: non_neg_integer(),
    %% The values, by key.
    keys = #{} :: #{digest() => #kept{}}
}).
-opaque store() :: #store{}.

%% What an entry costs besides its value: its key and stamp.
-define(ENTRY_BYTES, (32 + 8)).

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
        #{Key := #kept{value = Value}} -> {ok, Value};
        _ -> error
    end.

%% Merges `Generation' into `Store' as its next one, and collects by
%% `Retention'. Returns `unchanged' if nothing changed: then nothing needs
%% writing.
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
                #{Key := #kept{value = Value}} -> {Keys, Changed};
                _ -> {Keys#{Key => #kept{stamp = Next, value = Value}}, true}
            end
        end,
        {Keys0, false},
        Added
    ),
    {Keys2, Changed} = lists:foldl(
        fun(Key, {Keys, Changed}) ->
            case Keys of
                #{Key := #kept{stamp = Stamp} = Kept} when Stamp < Stale ->
                    {Keys#{Key => Kept#kept{stamp = Next}}, true};
                _ ->
                    {Keys, Changed}
            end
        end,
        {Keys1, AddedChanged},
        Used ++ [Key || {Key, _Value} <- Added]
    ),
    case Changed of
        true -> {changed, collect(Store#store{generation = Next, keys = Keys2}, Retention)};
        false -> unchanged
    end.

%% A key used within this many generations isn't stamped again, so a key
%% every run uses is written once per `slack' runs. Collection keeps
%% `generations + slack' generations, so a key used in one of the last
%% `generations' is never dropped.
-spec slack(retention()) -> non_neg_integer().
slack(#retention{generations = Generations}) ->
    Generations div 4.

%% Drops keys older than the kept generations, then the oldest generations
%% while over the size cap, always keeping the newest generation's keys.
-spec collect(#store{}, retention()) -> #store{}.
collect(#store{generation = Newest, keys = Keys} = Store, Retention) ->
    #retention{generations = Generations, max_bytes = MaxBytes} = Retention,
    OldestKept = max(Newest - (Generations + slack(Retention)), 0),
    Kept = maps:filter(fun(_Key, #kept{stamp = Stamp}) -> Stamp > OldestKept end, Keys),
    Store#store{keys = shrink(Kept, Newest, MaxBytes)}.

-spec shrink(#{digest() => #kept{}}, non_neg_integer(), pos_integer()) -> #{digest() => #kept{}}.
shrink(Keys, Newest, MaxBytes) ->
    case map_size(Keys) > 0 andalso keys_bytes(Keys) > MaxBytes of
        false ->
            Keys;
        true ->
            case lists:min([Stamp || #kept{stamp = Stamp} <- maps:values(Keys)]) of
                Oldest when Oldest >= Newest ->
                    Keys;
                Oldest ->
                    Younger = maps:filter(fun(_Key, #kept{stamp = S}) -> S > Oldest end, Keys),
                    shrink(Younger, Newest, MaxBytes)
            end
    end.

%% Roughly what a store costs, to bound by its retention's `max_bytes'.
-spec store_bytes(store()) -> non_neg_integer().
store_bytes(#store{keys = Keys}) ->
    keys_bytes(Keys).

-spec keys_bytes(#{digest() => #kept{}}) -> non_neg_integer().
keys_bytes(Keys) ->
    maps:fold(
        fun(_Key, #kept{value = Value}, Bytes) ->
            Bytes + ?ENTRY_BYTES + erlang:external_size(Value)
        end,
        0,
        Keys
    ).

%% The store in `Dir', or an empty one if it is missing or corrupt.
-spec read_store(file:filename_all()) -> #store{}.
read_store(Dir) ->
    case read(Dir, ?STORE) of
        {ok, #store{generation = Generation, keys = Keys} = Store} when
            is_integer(Generation), is_map(Keys)
        ->
            case lists:all(fun is_kept/1, maps:values(Keys)) of
                true -> Store;
                false -> new_store()
            end;
        _ ->
            new_store()
    end.

-spec is_kept(term()) -> boolean().
is_kept(#kept{stamp = Stamp}) -> is_integer(Stamp);
is_kept(_) -> false.

%%--------------------------------------------------------------------
%% Per-file results
%%
%% A file whose stat is unchanged keeps its last result without being read
%% (the `metadata' strategy); one that changed, or every file with the
%% `content' strategy, is read and keeps a result put for the same content,
%% by the last run or by any run of any cache in the same directory.
%%
%% A result is kept under a key the caller makes of everything it depends on
%% besides the file's path and content: the tool's build and options. A
%% result that can't be keyed so isn't put.

%% A path as a cache names it: UTF-8, as the caller names the file. A path
%% that isn't UTF-8 is never cached.
-type path() :: binary().

%% When a file's last result can be kept without reading it:
%% - `metadata': when its stat is the one recorded, outside the racy window.
%%   The file isn't read.
%% - `content': never. Every file is read and hashed, and kept by content only.
-type strategy() :: metadata | content.

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

%% A file's result, as entries/1 gives it.
-type entry() :: #{
    path := path(),
    key := digest(),
    seen := observation(),
    content := digest(),
    result := term()
}.

%% Per-file results kept across runs. Its tables are public, so any process
%% can look up and put; the process that opened it owns them and saves it.
-record(cache, {
    %% The directory this cache, its siblings and their shared store are in.
    dir :: file:filename_all(),
    %% The file in `dir' this cache's entries are kept in.
    name :: file:filename_all(),
    %% When a file's last result can be kept without reading it.
    strategy :: strategy(),
    %% When the run started: the time every stat it records counts as taken.
    started :: seconds(),
    %% How long the store keeps results no run used.
    retention :: #retention{},
    %% The last run's entries, by path: `{Path, Entry, Kept}', where `Kept'
    %% is whether this run kept it by its stat.
    last :: ets:tid(),
    %% This run's entries but those kept by stat: `{Path, Entry}'. With the
    %% kept ones, the next run's `last'.
    found :: ets:tid(),
    %% The store keys this run answered from: `{Key}'.
    used :: ets:tid(),
    %% The store keys this run added, with their values: `{Key, Value}'.
    added :: ets:tid(),
    %% The store's values by key, `{Key, Value}', once load_store/1 read it:
    %% a run answered by stat alone never reads it.
    store = unloaded :: unloaded | ets:tid()
}).
-opaque cache() :: #cache{}.

%% Opens the cache kept in `Name' of `Dir', with the last run's results.
%%
%% Options:
%% - `strategy': when a file's last result can be kept without reading it
%%   (default: `metadata').
%% - `started_at': when the run started, if before the cache was opened
%%   (default: now). Every stat it records counts as taken then: it must be
%%   before any file is read.
%% - `retention': how long the store keeps results no run used (default:
%%   128 generations, 64 MiB).
-spec open(file:filename_all(), file:filename_all(), #{
    strategy => strategy(), started_at => seconds(), retention => retention()
}) -> cache().
open(Dir, Name, Options) ->
    Last = ets:new(erlfmt_cache_last, [set, public, {read_concurrency, true}]),
    ets:insert(Last, [
        {Path, Entry, false}
     || #entry{path = Path} = Entry <- read_entries(Dir, Name)
    ]),
    Table = fun(Tag) -> ets:new(Tag, [set, public, {write_concurrency, true}]) end,
    #cache{
        dir = Dir,
        name = Name,
        strategy = maps:get(strategy, Options, metadata),
        started = maps:get(started_at, Options, os:system_time(second)),
        retention = maps:get(retention, Options, #retention{}),
        last = Last,
        found = Table(erlfmt_cache_found),
        used = Table(erlfmt_cache_used),
        added = Table(erlfmt_cache_added)
    }.

%% Returns the last run's entries, in no particular order.
-spec entries(cache()) -> [entry()].
entries(#cache{last = Last}) ->
    [
        #{path => Path, key => Key, seen => Seen, content => Content, result => Result}
     || {_, #entry{path = Path, key = Key, seen = Seen, content = Content, result = Result}, _} <-
            ets:tab2list(Last)
    ].

%% Returns the last run's result for the file at `Path', which now has
%% `Stat', under `Key', if the file is unchanged by its stat and the strategy
%% allows that. Such a file needn't be read.
-spec get_by_stat(cache(), path(), digest(), stat()) -> {ok, term()} | miss.
get_by_stat(#cache{strategy = content}, _Path, _Key, _Stat) ->
    miss;
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
%% the directory put. Call it before the first lookup by content: a run
%% answered by stat alone never needs it.
-spec load_store(cache()) -> cache().
load_store(#cache{store = unloaded, dir = Dir} = Cache) ->
    #store{keys = Keys} = read_store(Dir),
    Store = ets:new(erlfmt_cache_store, [set, public, {read_concurrency, true}]),
    ets:insert(Store, [{Key, Value} || {Key, #kept{value = Value}} <- maps:to_list(Keys)]),
    Cache#cache{store = Store};
load_store(Cache) ->
    Cache.

%% Returns the result for the file at `Path', read with `Stat' and hashing to
%% `Content', under `Key': the last run's, or one any run put for that key,
%% path and content. The store must be loaded.
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
%% `Content', under `Key', for this cache's next run and any cache's run.
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
    case generation(Cache) of
        #generation{used = [], added = []} ->
            ok;
        Generation ->
            %% Merged into the store as it is on disk now, so runs in
            %% parallel lose as little as possible.
            case store_commit(read_store(Dir), Generation, Retention) of
                {changed, Store} -> write(Dir, ?STORE, Store);
                unchanged -> ok
            end
    end,
    close(Cache).

%% Writes the entries for the next run: those kept by stat, and those found
%% again or put. A run that kept every entry by stat and found nothing (one
%% that changed nothing) writes nothing.
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

%% What this run used and added, to commit to the store.
-spec generation(cache()) -> generation().
generation(#cache{used = Used, added = Added}) ->
    #generation{used = [Key || {Key} <- ets:tab2list(Used)], added = ets:tab2list(Added)}.

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

%% Adds this run's entry for a file read with `Stat', observed at the start of
%% the run.
-spec push(cache(), path(), digest(), stat(), digest(), term()) -> ok.
push(#cache{found = Found, started = Started}, Path, Key, Stat, Content, Result) ->
    Seen = observe(Stat, Started),
    Entry = #entry{path = Path, key = Key, seen = Seen, content = Content, result = Result},
    ets:insert(Found, {Path, Entry}),
    ok.

%% The entries kept in `Name' of `Dir', or none if they are missing, corrupt
%% or not entries.
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

%%--------------------------------------------------------------------
%% The cache directory: its files are checksummed external terms, written
%% atomically.

%% Reads the term in `Name', a path relative to `Dir'. Returns `error' if it
%% is missing or corrupt: a miss, never a wrong value.
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

%% Removes all but the `Keep' most recently written files in `Subdir' of
%% `Dir'.
-spec prune(file:filename_all(), file:filename_all(), non_neg_integer()) -> ok.
prune(Dir, Subdir, Keep) ->
    Path = filename:join(Dir, Subdir),
    case file:list_dir(Path) of
        {ok, Names} when length(Names) > Keep ->
            Files = [
                {modified(filename:join(Path, Name)), filename:join(Path, Name)}
             || Name <- Names
            ],
            {_Newest, Oldest} = lists:split(Keep, lists:reverse(lists:sort(Files))),
            lists:foreach(fun({_Modified, File}) -> discard(File) end, Oldest);
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
%% Builds

%% Returns the digest of the code loaded for `Modules', loading them if
%% needed: results kept by one build are never used by another (every
%% rebuilt module differs). Cheaper than hashing a binary, and the same
%% whether the code came from an escript or from a rebar3 plugin.
-spec build_id([module()]) -> digest().
build_id(Modules) ->
    digest_term([{Module, Module:module_info(md5)} || Module <- lists:usort(Modules)]).
