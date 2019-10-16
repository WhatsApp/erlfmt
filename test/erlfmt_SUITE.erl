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

-module(erlfmt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).

%% Test cases
-export([
    records/1,
    attributes/1,
    macro_call_exprs/1,
    macro_call_pats/1,
    macro_call_types/1,
    macro_definitions/1,
    functions_and_funs/1,
    smoke_test_cli/1
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
        {parser, [parallel], [
            records,
            attributes,
            macro_call_exprs,
            macro_call_pats,
            macro_call_types,
            macro_definitions,
            functions_and_funs
        ]},
        {smoke_tests, [parallel, {timetrap, {minutes, 1}}], [
            smoke_test_cli
        ]}
    ].

all() ->
    [{group, smoke_tests}, {group, parser}].

%%--------------------------------------------------------------------
%% TEST CASES

records(Config) when is_list(Config) ->
    ?assertMatch(
        {record, _, {atom, _, foo}, []},
        parse_expr("#foo{}")
    ),
    ?assertMatch(
        {record_index, _, {atom, _, foo}, {atom, _, bar}},
        parse_expr("#foo.bar")
    ),
    ?assertMatch(
        {record_field, _, {var, _, 'X'}, {atom, _, foo}, {atom, _, bar}},
        parse_expr("X#foo.bar")
    ),
    ?assertMatch(
        {record, _, {var, _, 'X'}, {atom, _, foo}, []},
        parse_expr("X#foo{}")
    ),
    ?assertMatch(
        {attribute, _, record, {{atom, _, foo}, []}},
        parse_form("-record(foo, {}).")
    ),
    ?assertMatch(
        {attribute, _, type, {foo, {type, _, record, [{atom, _, foo}]}, []}},
        parse_form("-type foo() :: #foo{}.")
    ).

attributes(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, foo, {atom, _, bar}},
        parse_form("-foo(bar).")
    ),
    ?assertMatch(
        {attribute, _, ifdef, {atom, _, foo}},
        parse_form("-ifdef(foo).")
    ),
    ?assertMatch(
        {attribute, _, 'if', {op, _, '==', {macro_call, _, {atom, _, foo}, none}, {integer, _, 2}}},
        parse_form("-if(?foo == 2).")
    ),
    ?assertMatch(
        {attribute, _, else, undefined},
        parse_form("-else.")
    ),
    ?assertMatch(
        {attribute, _, endif, undefined},
        parse_form("-endif.")
    ),
    ?assertMatch(
        {attribute, _, endif, {macro_call, _, {var, _, 'BAR'}, none}},
        parse_form("-endif(?BAR).")
    ).

macro_call_exprs(Config) when is_list(Config) ->
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, none},
        parse_expr("?FOO")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, []},
        parse_expr("?FOO()")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, [{integer, _, 1}]},
        parse_expr("?FOO(1)")
    ),
    ?assertMatch(
        {macro_string, _, {var, _, 'FOO'}},
        parse_expr("??FOO")
    ),
    ?assertMatch(
        {macro_call, _, {atom, _, foo}, [{guard, _, {atom, _, x}, {atom, _, true}}]},
        parse_expr("?foo(x when true)")
    ),
    ?assertMatch(
        {concat, _, [{macro_call, _, {atom, _, foo}, none}, {string, _, "suffix"}]},
        parse_expr("?foo \"suffix\"")
    ),
    ?assertMatch(
        {concat, _, [{macro_string, _, {atom, _, foo}}, {string, _, "suffix"}]},
        parse_expr("??foo \"suffix\"")
    ),
    ?assertMatch(
        {concat, _, [{var, _, 'Var'}, {string, _, "suffix"}]},
        parse_expr("Var \"suffix\"")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {string, _, "suffix"}]},
        parse_expr("\"prefix\" \"suffix\"")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {macro_call, _, {atom, _, foo}, none}]},
        parse_expr("\"prefix\" ?foo")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {macro_string, _, {atom, _, foo}}]},
        parse_expr("\"prefix\" ??foo")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {var, _, 'Var'}]},
        parse_expr("\"prefix\" Var")
    ).

macro_call_pats(Config) when is_list(Config) ->
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, none},
        parse_pat("?FOO")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, []},
        parse_pat("?FOO()")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, [{integer, _, 1}]},
        parse_pat("?FOO(1)")
    ).

macro_call_types(Config) when is_list(Config) ->
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, none},
        parse_type("?FOO")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, []},
        parse_type("?FOO()")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, [{type, _, union, [{integer, _, 1}, {integer, _, 2}]}]},
        parse_type("?FOO(1 | 2)")
    ).

macro_definitions(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, define, {expr, {var, _, 'FOO'}, none, [[{atom, _, foo}]]}},
        parse_form("-define(FOO, foo).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {var, _, 'FOO'}, [], [[{atom, _, foo}]]}},
        parse_form("-define(FOO(), foo).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {var, _, 'FOO'}, [{var, _, 'X'}], [[{atom, _, foo}]]}},
        parse_form("-define(FOO(X), foo).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {atom, _, is_nice}, [{var, _, 'X'}], [[
            {call, _, {atom, _, is_tuple}, [{var, _, 'X'}]},
            {op, _, '=:=', {call, _, {atom, _,element}, [{integer, _, 1},{var, _, 'X'}]}, {atom, _, nice}}
        ]]}},
        parse_form("-define(is_nice(X), is_tuple(X), element(1, X) =:= nice).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {atom, _, foo}, none, {record_name, _, {atom, _,bar}}}},
        parse_form("-define(foo, #bar).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {atom, _, foo}, none, empty}},
        parse_form("-define(foo,).")
    ),
    ?assertMatch(
        {attribute, _, define, {clause, {atom, _, foo}, none, {clause, _, {atom, _, foo}, [], [], [{atom, _, ok}]}}},
        parse_form("-define(foo, foo() -> ok).")
    ).

functions_and_funs(Config) when is_list(Config) ->
    ?assertMatch(
        {'fun', _, {function, {atom, _, foo}, {integer, _, 1}}},
        parse_expr("fun foo/1")
    ),
    ?assertMatch(
        {'fun', _, {function, {var, _, 'Mod'}, {atom, _, foo}, {integer, _, 1}}},
        parse_expr("fun Mod:foo/1")
    ),
    ?assertMatch(
        {'fun', _, {clauses, [{clause, _, 'fun', [], [], [{atom, _, ok}]}]}},
        parse_expr("fun () -> ok end")
    ),
    ?assertMatch(
        {'fun', _, {clauses, [{clause, _, {var, _, 'Foo'}, [], [], [{atom, _, ok}]}]}},
        parse_expr("fun Foo() -> ok end")
    ),
    ?assertMatch(
        {function, _, [{clause, _, {atom, _, foo}, [], [], [{atom, _, ok}]}]},
        parse_form("foo() -> ok.")
    ).

parse_expr(String) ->
    {function, _, [{clause, _, _, [], [], [Expr]}]} =
        parse_form("f() -> " ++ String ++ "."),
    Expr.

parse_pat(String) ->
    {function, _, [{clause, _, _, [Pat], [], [_]}]} =
        parse_form("f(" ++ String ++ ") -> ok."),
    Pat.

parse_type(String) ->
    {attribute, _, type, {_, Type, []}} =
        parse_form("-type foo() :: " ++ String ++ "."),
    Type.

parse_form(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    case erlfmt_parse:parse_form(Tokens) of
        {ok, Form} ->
            Form;
        {error, {_, Mod, Reason}} ->
            ct:fail("Expected successful parse:\n~ts\ngot: ~ts", [String, Mod:format_error(Reason)])
    end.

smoke_test_cli(Config) when is_list(Config) ->
    %% this relies on the _build structure rebar3 uses
    Escript = filename:join(code:lib_dir(erlfmt), "../../bin/erlfmt"),
    ?assertMatch("Usage: erlfmt " ++ _, os:cmd(Escript ++ " -h")).
