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
-module(erlfmt_markdown_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    markdown_files/1,
    markdown_string/1
]).

suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
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
        {markdown, [parallel], [
            markdown_files,
            markdown_string
        ]}
    ].

all() ->
    [{group, markdown}].

markdown_files(Config) when is_list(Config) ->
    {ok, Data} = file:get_cwd(),
    RepoRootPath = filename:join([Data, "..", "..", "..", ".."]),
    DocPath = filename:join(RepoRootPath, "doc"),
    Filenames = lists:append(
        find_markdown_filenames_in(RepoRootPath),
        find_markdown_filenames_in(DocPath)
    ),
    ?assertMatch([_One, _Two, _Three | _AtLeast], Filenames),
    lists:map(
        fun (Filename) ->
            {ok, Content} = file:read_file(Filename),
            check_markdown(binary_to_list(Content))
        end,
        Filenames
    ).

find_markdown_filenames_in(Path) ->
    {ok, BaseFilenames} = file:list_dir_all(Path),
    Filenames =
        lists:map(fun (Filename) -> filename:join([Path, Filename]) end, BaseFilenames),
    lists:filter(fun (Filename) -> filename:extension(Filename) == ".md" end, Filenames).

markdown_string(Config) when is_list(Config) ->
    S =
        "# Heading\n"
        "ignore for now\n"
        "```erlang formatted key\n"
        "hello(mike, joe, robert)\n"
        "```\n"
        "ignore for now\n"
        "```erlang unformatted\n"
        "goodbye(mike, joe, robert)\n"
        "```\n"
        "The previous unformatted version, should match this formatted version\n"
        "```erlang formatted\n"
        "goodbye(mike, joe, robert)\n"
        "```\n"
        "This unformatted version, should match the first formatted version with the same key\n"
        "```erlang unformatted key\n"
        "hello(mike, joe, robert)\n"
        "```\n",
    check_markdown(S).

check_markdown(Content) ->
    Sections = string:split(Content, "```", all),
    {_, Formatted, Unformatted} = lists:foldl(
        fun split_code_into_maps/2,
        {text, maps:new(), maps:new()},
        Sections
    ),
    maps:map(
        fun (Key, FormattedCode) ->
            check_fmt(FormattedCode, FormattedCode),
            case maps:find(Key, Unformatted) of
                error ->
                    ignore;
                {ok, UnformattedCode} ->
                    check_fmt(UnformattedCode, FormattedCode)
            end
        end,
        Formatted
    ),
    % check that there are no unformatted pairs without formatted friends
    ?assertEqual(#{}, maps:without(maps:keys(Formatted), Unformatted)).

split_code_into_maps(_Text, {text, Formatted, Unformatted}) ->
    {code, Formatted, Unformatted};
split_code_into_maps(Text, {code, Formatted, Unformatted}) ->
    [FirstLine, Code0] = string:split(Text, "\n"),
    Code = strip_good_bad(Code0),
    Spec = string:split(FirstLine, " ", all),
    case Spec of
        ["erl" ++ _, "formatted" | Key] ->
            {text, maps:put(Key, Code, Formatted), Unformatted};
        ["erl" ++ _, "unformatted" | Key] ->
            {text, Formatted, maps:put(Key, Code, Unformatted)};
        _ ->
            {text, Formatted, Unformatted}
    end.

strip_good_bad("%% Good\n" ++ Rest) -> Rest;
strip_good_bad("%% Bad\n" ++ Rest) -> Rest;
strip_good_bad(Rest) -> Rest.

check_fmt(Unformatted, Expected) ->
    {ok, Got, []} = erlfmt:format_string(Unformatted, []),
    ?assertEqual(string:trim(Expected), string:trim(Got)).
