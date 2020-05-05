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
    filelib:wildcard(filename:join(Path, "*.md")).

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
    lists:foldl(fun check_section/2, {text, maps:new()}, Sections).

check_section(_Text, {text, State}) ->
    {code, State};
check_section(Text, {code, State}) ->
    [FirstLine, CurrentCode] = string:split(Text, "\n"),
    Spec = string:split(FirstLine, " ", all),
    case Spec of
        ["erl" ++ _, ToFormat | Key] ->
            case maps:find(Key, State) of
                error ->
                    case ToFormat of
                        "formatted" ->
                            check_fmt(CurrentCode, CurrentCode),
                            {text, maps:put(Key, CurrentCode, State)};
                        "unformatted" ->
                            {text, maps:put(Key, CurrentCode, State)}
                    end;
                {ok, PreviousCode} ->
                    case ToFormat of
                        "formatted" ->
                            check_fmt(PreviousCode, CurrentCode);
                        "unformatted" ->
                            check_fmt(CurrentCode, PreviousCode)
                    end,
                    {text, maps:remove(Key, State)}
            end;
        _ ->
            {text, State}
    end.

check_fmt(Unformatted, Expected) ->
    NewlyFormatted = format(Unformatted, 80),
    ?assertEqual(string:trim(NewlyFormatted), string:trim(Expected)).

format(String, PageWidth) ->
    try format_doc(String) of
        Doc ->
            Rendered = erlfmt_algebra:document_render(Doc, [{page_width, PageWidth}]),
            unicode:characters_to_list(Rendered)
    catch
        _:_ ->
            throw("Big Problem Parsing: " ++ String)
    end.

format_doc(String) ->
    case erlfmt:read_forms_string("nofile", "f() ->\n" ++ String ++ ".") of
        {ok, [{function, _, [{clause, _, _, empty, [Expr]}]}], []} ->
            erlfmt_format:expr_to_algebra(Expr);
        _ ->
            {ok, [Form], []} = erlfmt:read_forms_string("nofile", String),
            erlfmt_format:form_to_algebra(Form)
    end.
