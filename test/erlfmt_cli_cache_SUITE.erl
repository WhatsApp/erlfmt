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

%% --cache changes nothing but speed: through a sequence of edits to a small
%% project, every run with --cache exits, prints and writes what the same run
%% without it does.
-module(erlfmt_cli_cache_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-export([suite/0, all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
    check_is_the_same_through_edits/1,
    write_is_the_same_through_edits/1,
    a_trusted_stat_is_the_same_and_writes_nothing/1,
    projects_sharing_a_cache_are_the_same/1,
    rebar_config_enables_the_cache/1
]).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [
        check_is_the_same_through_edits,
        write_is_the_same_through_edits,
        a_trusted_stat_is_the_same_and_writes_nothing,
        projects_sharing_a_cache_are_the_same,
        rebar_config_enables_the_cache
    ].

init_per_testcase(Case, Config) ->
    Root = filename:join(?config(priv_dir, Config), Case),
    Plain = filename:join(Root, "plain"),
    Cached = filename:join(Root, "cached"),
    [make_project(Dir) || Dir <- [Plain, Cached]],
    [{plain, Plain}, {cached, Cached}, {cache_location, filename:join(Root, "cache")} | Config].

end_per_testcase(_Case, Config) ->
    %% Unreadable files would stop the priv dir from being cleaned up.
    [
        file:change_mode(filename:join([?config(Dir, Config), "src", "unreadable.erl"]), 8#644)
     || Dir <- [plain, cached]
    ],
    ok.

%%--------------------------------------------------------------------
%% Scenarios

%% An edit to the project, applied to both copies alike.
-type edit() ::
    nothing
    | {write, file:filename(), iodata()}
    | {write_keeping_stat_times, file:filename(), iodata()}
    | {rename, file:filename(), file:filename()}
    | {delete, file:filename()}
    | {unreadable, file:filename()}.

%% Each edit, and the extra arguments the runs after it are given.
-spec steps() -> [{atom(), edit(), [string()]}].
steps() ->
    [
        {initial, nothing, []},
        {unchanged, nothing, []},
        {new_formatted_file, {write, "src/new.erl", "-module(new).\n"}, []},
        {new_unformatted_file, {write, "src/ugly.erl", "-module( ugly ).\n"}, []},
        %% --write formatted it: the same unformatted content again isn't known formatted.
        {unformatted_again, {write, "src/ugly.erl", "-module( ugly ).\n"}, []},
        {parse_error, {write, "src/new.erl", "-module(new).\nf( -> .\n"}, []},
        {parse_error_fixed, {write, "src/new.erl", "-module(new).\n"}, []},
        {renamed, {rename, "src/new.erl", "src/renamed.erl"}, []},
        {deleted, {delete, "src/renamed.erl"}, []},
        {same_size_edit_keeping_times,
            {write_keeping_stat_times, "src/formatted.erl", "-module( formatte).\n"}, []},
        {changed_back, {write, "src/formatted.erl", "-module(formatted).\n"}, []},
        {print_width, nothing, ["--print-width=20"]},
        {print_width_again, nothing, ["--print-width=20"]},
        {require_pragma, nothing, ["--require-pragma"]},
        {require_pragma_verbose, nothing, ["--require-pragma", "--verbose"]},
        {require_pragma_verbose_again, nothing, ["--require-pragma", "--verbose"]},
        {verbose, nothing, ["--verbose"]},
        {verbose_again, nothing, ["--verbose"]},
        {insert_pragma, nothing, ["--insert-pragma"]},
        {excluded, nothing, ["--exclude-files=src/long.erl"]},
        {unreadable, {unreadable, "src/unreadable.erl"}, []},
        {back_to_defaults, nothing, []}
    ].

check_is_the_same_through_edits(Config) ->
    same_through_edits(["--check"], Config).

write_is_the_same_through_edits(Config) ->
    same_through_edits(["--write"], Config).

same_through_edits(Mode, Config) ->
    [
        begin
            edit_both(Edit, Config),
            assert_same(Step, Mode ++ Args, Config)
        end
     || {Step, Edit, Args} <- steps()
    ],
    ok.

%% Once a file's stat is outside the racy window, a run that changed nothing
%% answers by stat alone, and writes nothing; an edit that keeps the size and
%% the mtime still shows, by its ctime.
a_trusted_stat_is_the_same_and_writes_nothing(Config) ->
    Cached = ?config(cached, Config),
    assert_same(first, ["--check"], Config),
    wait_past_racy_window(Cached),
    assert_same(trusting, ["--check"], Config),
    Runs = filename:join([?config(cache_location, Config), "v1", "runs"]),
    Before = written(Runs),
    assert_same(trusted, ["--check"], Config),
    ?assertEqual(Before, written(Runs), "a run answered by stat alone writes nothing"),
    edit_both({write_keeping_stat_times, "src/formatted.erl", "-module( formatte).\n"}, Config),
    #{stderr := Stderr} = assert_same(edited_keeping_times, ["--check"], Config),
    ?assert(lists:member(<<"[warn] src/formatted.erl">>, Stderr), "the edit is found").

%% Two copies of a project sharing a cache location: the second one's runs
%% answer from what the first put in the store.
projects_sharing_a_cache_are_the_same(Config) ->
    Location = ?config(cache_location, Config),
    Plain = ?config(plain, Config),
    Cached = ?config(cached, Config),
    Other = filename:join(filename:dirname(Cached), "other"),
    make_project(Other),
    run(Other, ["--check", "--cache", "--cache-location", Location]),
    ?assertEqual(
        run(Plain, ["--check"]), run(Cached, ["--check", "--cache", "--cache-location", Location])
    ).

%% `{erlfmt, [cache, {cache_location, ...}]}' in rebar.config reaches the CLI
%% as the flags do: erlfmt_cli:do/3 takes rebar3's options as its defaults.
rebar_config_enables_the_cache(Config) ->
    Location = ?config(cache_location, Config),
    Cached = ?config(cached, Config),
    Script = io_lib:format(
        "erlfmt_cli:do(\"rebar3 fmt\", [check], [cache, {cache_location, ~p}]), halt().",
        [Location]
    ),
    Command = io_lib:format("cd ~s && erl -noshell -pa ~s -eval '~s' 2>&1", [
        Cached, filename:join(code:lib_dir(erlfmt), "ebin"), Script
    ]),
    Output = os:cmd(lists:flatten(Command)),
    ?assertNotEqual(nomatch, string:find(Output, "Checking formatting")),
    {ok, [_Run]} = file:list_dir(filename:join([Location, "v1", "runs"])).

%%--------------------------------------------------------------------
%% Helpers

%% A small project: files that are formatted, unformatted, with a form that
%% doesn't parse (a warning), that can't be formatted (an error), with a
%% pragma, and with a line too long, so --verbose warns.
make_project(Dir) ->
    Files = [
        {"src/formatted.erl", "-module(formatted).\n"},
        {"src/unformatted.erl", "-module(unformatted).\nf()->ok.\n"},
        {"src/broken.erl", "-module(broken).\ng( -> .\n"},
        {"src/failing.erl", "-module(failing).\nf() -> \"unterminated.\n"},
        {"src/latin.erl", <<"-module(latin).\n%% caf", 16#e9, "\n">>},
        {"src/pragma.erl", "%% @format\n\n-module(pragma).\n"},
        {"src/unreadable.erl", "-module(unreadable).\n"},
        {"src/long.erl", [
            "-module(long).\n\n-define(LONG, \"",
            lists:duplicate(120, $x),
            "\").\n"
        ]},
        {"include/header.hrl", "-define(HEADER, true).\n"}
    ],
    [write(filename:join(Dir, Name), Content) || {Name, Content} <- Files],
    ok.

edit_both(Edit, Config) ->
    [edit(?config(Dir, Config), Edit) || Dir <- [plain, cached]],
    ok.

-spec edit(file:filename(), edit()) -> ok.
edit(_Dir, nothing) ->
    ok;
edit(Dir, {write, Name, Content}) ->
    write(filename:join(Dir, Name), Content);
edit(Dir, {write_keeping_stat_times, Name, Content}) ->
    Path = filename:join(Dir, Name),
    {ok, #file_info{mtime = Mtime, atime = Atime}} = file:read_file_info(Path, [{time, posix}]),
    write(Path, Content),
    ok = file:write_file_info(Path, #file_info{mtime = Mtime, atime = Atime}, [{time, posix}]);
edit(Dir, {rename, From, To}) ->
    ok = file:rename(filename:join(Dir, From), filename:join(Dir, To));
edit(Dir, {delete, Name}) ->
    ok = file:delete(filename:join(Dir, Name));
edit(Dir, {unreadable, Name}) ->
    ok = file:change_mode(filename:join(Dir, Name), 8#000).

write(Path, Content) ->
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Content).

%% Runs erlfmt with `Args' on both copies, without and with --cache, and
%% checks they exit, print and leave the files the same. Returns the result.
assert_same(Step, Args, Config) ->
    Plain = ?config(plain, Config),
    Cached = ?config(cached, Config),
    Location = ?config(cache_location, Config),
    Uncached = run(Plain, Args ++ files()),
    WithCache = run(Cached, Args ++ ["--cache", "--cache-location", Location] ++ files()),
    ?assertEqual({Step, Uncached}, {Step, WithCache}),
    ?assertEqual({Step, tree(Plain)}, {Step, tree(Cached)}),
    Uncached.

files() ->
    ["{src,include}/*.{erl,hrl}"].

%% Runs the escript in `Dir': its exit status, and its stdout and stderr
%% lines, sorted, as files are reported in the order they finish formatting.
run(Dir, Args) ->
    Out = filename:join(Dir, "../stdout"),
    Err = filename:join(Dir, "../stderr"),
    Command = lists:flatten([
        "cd ",
        quote(Dir),
        " && ",
        quote(escript()),
        [[" ", quote(Arg)] || Arg <- Args],
        " >",
        quote(Out),
        " 2>",
        quote(Err)
    ]),
    Port = open_port({spawn_executable, "/bin/sh"}, [{args, ["-c", Command]}, exit_status]),
    Exit =
        receive
            {Port, {exit_status, Status}} -> Status
        end,
    #{exit => Exit, stdout => sorted_lines(Out), stderr => sorted_lines(Err)}.

sorted_lines(Path) ->
    {ok, Bytes} = file:read_file(Path),
    lists:sort(binary:split(Bytes, <<"\n">>, [global])).

quote(Arg) ->
    ["'", string:replace(Arg, "'", "'\\''", all), "'"].

%% Every file of a project, with its content, but for those unreadable.
tree(Dir) ->
    Files = filelib:wildcard("**/*", Dir),
    [{File, file:read_file(filename:join(Dir, File))} || File <- lists:sort(Files)].

%% When each file in `Dir''s runs was written: reading them changes their atime.
written(Runs) ->
    {ok, Names} = file:list_dir(Runs),
    [
        {Name, Mtime, Ctime, Inode}
     || Name <- lists:sort(Names),
        {ok, #file_info{mtime = Mtime, ctime = Ctime, inode = Inode}} <- [
            file:read_file_info(filename:join(Runs, Name), [{time, posix}])
        ]
    ].

%% Waits until every file of the project is older than the racy window, so the
%% next run can trust their stats. The racy window is about wall-clock time,
%% so this waits for the clock, not for an event.
wait_past_racy_window(Dir) ->
    Newest = lists:max([
        max(Mtime, Ctime)
     || File <- filelib:wildcard("**/*", Dir),
        {ok, #file_info{mtime = Mtime, ctime = Ctime}} <- [
            file:read_file_info(filename:join(Dir, File), [{time, posix}])
        ]
    ]),
    Wait = Newest + 3 - os:system_time(second),
    timer:sleep(max(Wait, 0) * 1000).

escript() ->
    %% this relies on the _build structure rebar3 uses
    filename:join(code:lib_dir(erlfmt), "../../bin/erlfmt").
