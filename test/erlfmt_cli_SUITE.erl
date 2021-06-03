%% Copyright (c) Facebook, Inc. and its affiliates.
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
-module(erlfmt_cli_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    group/1,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    smoke_test_cli/1,
    smoke_test_stdio_escript/1,
    smoke_test_stdio_regular/1,
    smoke_test_stdio_without_pragma/1,
    smoke_test_stdio_with_pragma/1,
    smoke_test_stdio_insert_pragma_without/1,
    smoke_test_stdio_insert_and_require_pragma/1,
    smoke_test_stdio_delete_pragma/1,
    smoke_test_stdio_delete_pragma_without/1,
    smoke_test_stdio_delete_pragma_with_copyright/1,
    smoke_test_stdio_reinsert_pragma/1,
    smoke_test_stdio_reinsert_pragma_second/1,
    smoke_test_stdio_reinsert_pragma_config/1,
    smoke_test_stdio_unicode/1,
    smoke_test_stdio_check/1,
    exclude_check/1
]).

suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    erlang:system_flag(backtrace_depth, 20),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {smoke_tests, [parallel], [
            smoke_test_cli,
            smoke_test_stdio_escript,
            smoke_test_stdio_regular,
            smoke_test_stdio_without_pragma,
            smoke_test_stdio_with_pragma,
            smoke_test_stdio_insert_pragma_without,
            smoke_test_stdio_insert_and_require_pragma,
            smoke_test_stdio_delete_pragma,
            smoke_test_stdio_delete_pragma_without,
            smoke_test_stdio_delete_pragma_with_copyright,
            smoke_test_stdio_reinsert_pragma,
            smoke_test_stdio_reinsert_pragma_second,
            smoke_test_stdio_reinsert_pragma_config,
            smoke_test_stdio_unicode,
            smoke_test_stdio_check,
            exclude_check
        ]}
    ].

group(_) -> [].

all() ->
    [
        {group, smoke_tests}
    ].

%%--------------------------------------------------------------------
%% TEST CASES

smoke_test_cli(Config) when is_list(Config) ->
    ?assertMatch("Usage: erlfmt " ++ _, os:cmd(escript() ++ " -h")).

smoke_test_stdio_escript(Config) when is_list(Config) ->
    stdio_test("escript.erl", "", Config).

smoke_test_stdio_regular(Config) when is_list(Config) ->
    stdio_test("attributes.erl", "", Config).

smoke_test_stdio_without_pragma(Config) when is_list(Config) ->
    stdio_test("no_pragma.erl", "--require-pragma", Config).

smoke_test_stdio_with_pragma(Config) ->
    stdio_test("pragma.erl", "--require-pragma", Config).

smoke_test_stdio_unicode(Config) ->
    stdio_test("unicode.erl", "", Config),
    stdio_test("unicode.erl", "--require-pragma", Config).

smoke_test_stdio_insert_pragma_without(Config) when is_list(Config) ->
    Formatted = os:cmd("echo '-module(nopragma).' | " ++ escript() ++ " - --insert-pragma"),
    Expected =
        "%%% % @format\n"
        "\n"
        "-module(nopragma).\n",
    ?assertEqual(Expected, Formatted).

smoke_test_stdio_delete_pragma(Config) when is_list(Config) ->
    Formatted = os:cmd(
        "echo '%% @format\n\n-module(nopragma).' | " ++ escript() ++ " - --delete-pragma"
    ),
    Expected =
        "-module(nopragma).\n",
    ?assertEqual(Expected, Formatted).

smoke_test_stdio_delete_pragma_without(Config) when is_list(Config) ->
    Formatted = os:cmd("echo '-module(nopragma).' | " ++ escript() ++ " - --delete-pragma"),
    Expected =
        "-module(nopragma).\n",
    ?assertEqual(Expected, Formatted).

smoke_test_stdio_delete_pragma_with_copyright(Config) when is_list(Config) ->
    Formatted = os:cmd(
        "echo '%% @format\n%% copyright\n\n-module(nopragma).' | " ++ escript() ++
            " - --delete-pragma"
    ),
    Expected =
        "%% copyright\n"
        "\n"
        "-module(nopragma).\n",
    ?assertEqual(Expected, Formatted).

smoke_test_stdio_reinsert_pragma(Config) when is_list(Config) ->
    Formatted = os:cmd(
        "echo '%% @format\n%%% copyright\n\n-module(nopragma).' | " ++ escript() ++
            " - --insert-pragma"
    ),
    Expected =
        "%%% % @format\n"
        "%%% copyright\n"
        "\n"
        "-module(nopragma).\n",
    ?assertEqual(Expected, Formatted).

%% respect the number of percentages when replacing the pragma
smoke_test_stdio_reinsert_pragma_second(Config) when is_list(Config) ->
    Formatted = os:cmd(
        "echo '%% copyright\n%% @format\n\n-module(nopragma).' | " ++ escript() ++
            " - --insert-pragma"
    ),
    Expected =
        "%% copyright\n"
        "%% % @format\n"
        "\n"
        "-module(nopragma).\n",
    ?assertEqual(Expected, Formatted).

smoke_test_stdio_reinsert_pragma_config(Config) when is_list(Config) ->
    Formatted = os:cmd(
        "echo '%% @format\n\n%%% actual comment\n{}.\n' | " ++ escript() ++
            " - --insert-pragma"
    ),
    Expected =
        "%%% % @format\n"
        "\n"
        "%%% actual comment\n"
        "{}.\n",
    ?assertEqual(Expected, Formatted).

smoke_test_stdio_insert_and_require_pragma(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Path = filename:join(DataDir, "pragma.erl"),
    ErrorString = os:cmd(
        "cat " ++ Path ++ " | " ++ escript() ++ " - --insert-pragma --require-pragma"
    ),
    ?assert(
        string:find(ErrorString, "Cannot use both --insert-pragma and --require-pragma") =/=
            nomatch
    ).

smoke_test_stdio_check(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Same = os:cmd(
        "cat " ++
            filename:join(DataDir, "attributes.erl") ++ " | " ++ escript() ++ " - " ++ "--check"
    ),
    ?assertMatch(nomatch, string:find(Same, "[warn]")),
    Warn = os:cmd(
        "cat " ++ filename:join(DataDir, "comments.erl") ++ " | " ++ escript() ++ " - " ++ "--check"
    ),
    ?assertNotMatch(nomatch, string:find(Warn, "[warn]")),
    Skip = os:cmd(
        "cat " ++
            filename:join(DataDir, "comments.erl") ++
            " | " ++ escript() ++ " - " ++ "--check --require-pragma --verbose"
    ),
    ?assertNotMatch(nomatch, string:find(Skip, "Skip")).

exclude_check(Config) when is_list(Config) ->
    Files = filename:join(?config(data_dir, Config), "*.erl"),
    Exclude = filename:join(?config(data_dir, Config), "broken.erl"),
    WithBroken = os:cmd(
        escript() ++ " -c " ++ Files
    ),
    ?assertNotMatch(nomatch, string:find(WithBroken, "[warn]")),
    ?assertNotMatch(nomatch, string:find(WithBroken, "broken.erl")),
    WithoutBroken = os:cmd(
        escript() ++ " -c " ++ Files ++ " --exclude-files=" ++ Exclude
    ),
    ?assertNotMatch(nomatch, string:find(WithoutBroken, "[warn]")),
    ?assertMatch(nomatch, string:find(WithoutBroken, "broken.erl")).

%%--------------------------------------------------------------------
%% HELPERS

stdio_test(FileName, Options, Config) ->
    DataDir = ?config(data_dir, Config),
    Path = filename:join(DataDir, FileName),
    Formatted = os:cmd("cat " ++ Path ++ " | " ++ escript() ++ " - " ++ Options),
    % ?assertEqual(toto, Path),
    {ok, Expected} = file:read_file(Path),
    ?assertEqual(Expected, unicode:characters_to_binary(Formatted)).

escript() ->
    %% this relies on the _build structure rebar3 uses
    filename:join(code:lib_dir(erlfmt), "../../bin/erlfmt").
