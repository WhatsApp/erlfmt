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
-include_lib("test/assert_diagnostic.hrl").

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
    records/1,
    attributes/1,
    specs/1,
    macro_call_exprs/1,
    macro_call_pats/1,
    macro_call_types/1,
    macro_definitions/1,
    functions_and_funs/1,
    operators/1,
    lists/1,
    binaries/1,
    maps/1,
    clauses/1,
    types/1,
    annos/1,
    shebang/1,
    snapshot_simple_comments/1,
    snapshot_big_binary/1,
    snapshot_attributes/1,
    snapshot_escript/1,
    snapshot_emulator_args/1,
    snapshot_emulator_args2/1,
    snapshot_emulator_args_module/1,
    snapshot_pragma/1,
    snapshot_no_pragma/1,
    snapshot_comments/1,
    snapshot_broken/1,
    snapshot_overlong/1,
    snapshot_otp_examples/1,
    snapshot_insert_pragma_with/1,
    snapshot_script/1,
    snapshot_ignore_format/1,
    snapshot_empty/1,
    simple_comments_range/1,
    broken_range/1,
    snapshot_range_whole_comments/1,
    snapshot_range_partial/1,
    snapshot_range_partial2/1,
    snapshot_enclosing_range/1,
    snapshot_enclosing_range2/1,
    snapshot_enclosing_range_no_leak/1,
    contains_pragma/1,
    insert_pragma/1,
    overlong_warning/1,
    do_not_crash_on_bad_record/1,
    raw_string_anno/1
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
        {parser, [parallel], [
            records,
            attributes,
            specs,
            macro_call_exprs,
            macro_call_pats,
            macro_call_types,
            macro_definitions,
            functions_and_funs,
            operators,
            lists,
            binaries,
            maps,
            clauses,
            types,
            annos,
            shebang,
            do_not_crash_on_bad_record,
            raw_string_anno
        ]},
        {snapshot_tests, [parallel], [
            snapshot_simple_comments,
            snapshot_big_binary,
            snapshot_attributes,
            snapshot_escript,
            snapshot_emulator_args,
            snapshot_emulator_args2,
            snapshot_emulator_args_module,
            snapshot_pragma,
            snapshot_no_pragma,
            snapshot_comments,
            snapshot_broken,
            snapshot_overlong,
            snapshot_otp_examples,
            snapshot_insert_pragma_with,
            snapshot_script,
            snapshot_ignore_format,
            snapshot_empty
        ]},
        {range_tests, [parallel], [
            simple_comments_range,
            broken_range,
            snapshot_range_whole_comments,
            snapshot_range_partial,
            snapshot_range_partial2,
            snapshot_enclosing_range,
            snapshot_enclosing_range2,
            snapshot_enclosing_range_no_leak
        ]},
        {pragma_tests, [parallel], [
            contains_pragma,
            insert_pragma
        ]},
        {verbose_warning_tests, [parallel], [
            overlong_warning
        ]}
    ].

group(_) -> [].

all() ->
    [
        {group, parser},
        {group, snapshot_tests},
        {group, range_tests},
        {group, pragma_tests},
        {group, verbose_warning_tests}
    ].

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
        {attribute, _, {atom, _, record}, [
            {atom, _, foo},
            {tuple, _, [
                {op, _, '::', {record_field, _, {atom, _, a}, {integer, _, 1}},
                    {call, _, {atom, _, integer}, []}},
                {op, _, '::', {record_field, _, {atom, _, b}}, {call, _, {atom, _, float}, []}},
                {record_field, _, {atom, _, c}, {integer, _, 2}},
                {record_field, _, {atom, _, d}}
            ]}
        ]},
        parse_form("-record(foo, {a = 1 :: integer(), b :: float(), c  = 2, d}).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, type}, [
            {op, _, '::', {call, _, {atom, _, foo}, []}, {record, _, {atom, _, foo}, []}}
        ]},
        parse_form("-type foo() :: #foo{}.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, opaque}, [
            {op, _, '::', {call, _, {atom, _, foo}, []}, {record, _, {atom, _, foo}, []}}
        ]},
        parse_form("-opaque foo() :: #foo{}.")
    ).

attributes(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, {atom, _, foo}, [{atom, _, bar}]},
        parse_form("-foo(bar).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, ifdef}, [{atom, _, foo}]},
        parse_form("-ifdef(foo).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, 'if'}, [
            {op, _, '==', {macro_call, _, {atom, _, foo}, none}, {integer, _, 2}}
        ]},
        parse_form("-if(?foo == 2).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, else}, no_parens},
        parse_form("-else.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, else}, []},
        parse_form("-else().")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, endif}, no_parens},
        parse_form("-endif.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, endif}, [{macro_call, _, {var, _, 'BAR'}, none}]},
        parse_form("-endif(?BAR).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, export}, [
            {list, _, [
                {op, _, '/', {macro_call, _, {var, _, 'FOO'}, none}, {integer, _, 1}},
                {op, _, '/', {atom, _, foo}, {integer, _, 2}}
            ]}
        ]},
        parse_form("-export([?FOO/1, foo/2]).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, import}, [
            {atom, _, foo},
            {list, _, [{op, _, '/', {atom, _, bar}, {integer, _, 1}}]}
        ]},
        parse_form("-import(foo, [bar/1]).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, something_else}, [
            {tuple, _, [{atom, _, foo}, {op, _, '/', {atom, _, bar}, {integer, _, 1}}]}
        ]},
        parse_form("-something_else({foo, bar/1}).")
    ).

specs(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, {atom, _, spec}, [
            {spec, _, {remote, _, {atom, _, foo}, {atom, _, bar}}, [
                {spec_clause, _, {args, _, []}, {atom, _, ok}, empty}
            ]}
        ]},
        parse_form("-spec foo:bar() -> ok.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, spec}, [
            {spec, _, {atom, _, foo}, [
                {spec_clause, _, {args, _, [{call, _, {atom, _, integer}, []}]}, {atom, _, integer},
                    empty},
                {spec_clause, _, {args, _, [{call, _, {atom, _, atom}, []}]}, {atom, _, atom},
                    empty}
            ]}
        ]},
        parse_form("-spec foo(integer()) -> integer; (atom()) -> atom.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, callback}, [
            {spec, _, {atom, _, foo}, [
                {spec_clause, _, {args, _, [{var, _, 'X'}]}, {var, _, 'Y'},
                    {guard_or, _, [
                        {guard_and, _, [
                            {op, _, '::', {var, _, 'X'}, {call, _, {atom, _, integer}, []}},
                            {op, _, '::', {var, _, 'Y'}, {call, _, {atom, _, atom}, []}}
                        ]}
                    ]}}
            ]}
        ]},
        parse_form("-callback foo(X) -> Y when X :: integer(), Y :: atom().")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, spec}, [
            {spec, _, {macro_call, _, {atom, _, foo}, none}, [
                {spec_clause, _, {args, _, []}, {atom, _, ok}, empty}
            ]}
        ]},
        parse_form("-spec ?foo() -> ok.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, callback}, [
            {spec, _, {macro_call, _, {var, _, 'FOO'}, none}, [
                {spec_clause, _, {args, _, []}, {atom, _, ok}, empty}
            ]}
        ]},
        parse_form("-callback ?FOO() -> ok.")
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
        {macro_call, _, {atom, _, foo}, [{op, _, 'when', {atom, _, x}, {atom, _, true}}]},
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
        {concat, _, [{string, _, "prefix"}, {macro_call, _, {atom, _, foo}, []}]},
        parse_expr("\"prefix\" ?foo()")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {var, _, 'Var'}]},
        parse_expr("\"prefix\" Var")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_expr("?foo{}")
    ),
    ?assertMatch(
        {record_index, _, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_expr("?foo.bar")
    ),
    ?assertMatch(
        {record, _, {var, _, 'S'}, {macro_call, _, {atom, _, foo}, none}, []},
        parse_expr("S?foo{}")
    ),
    ?assertMatch(
        {record_field, _, {var, _, 'S'}, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_expr("S?foo.bar")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_expr("#?foo{}")
    ),
    ?assertMatch(
        {record_index, _, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_expr("#?foo.bar")
    ),
    ?assertMatch(
        {record, _, {var, _, 'S'}, {macro_call, _, {atom, _, foo}, none}, []},
        parse_expr("S#?foo{}")
    ),
    ?assertMatch(
        {record_field, _, {var, _, 'S'}, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_expr("S#?foo.bar")
    ),
    ?assertMatch(
        {record_index, _, {atom, _, foo}, {var, _, 'Bar'}},
        parse_expr("#foo.Bar")
    ),
    ?assertMatch(
        {record_field, _, {var, _, 'S'}, {atom, _, foo}, {var, _, 'Bar'}},
        parse_expr("S#foo.Bar")
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
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_pat("?foo{}")
    ),
    ?assertMatch(
        {record_index, _, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_pat("?foo.bar")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_pat("#?foo{}")
    ),
    ?assertMatch(
        {record_index, _, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_pat("#?foo.bar")
    ),
    ?assertMatch(
        {record_index, _, {atom, _, foo}, {var, _, 'Bar'}},
        parse_pat("#foo.Bar")
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
        {macro_call, _, {var, _, 'FOO'}, [{op, _, '|', {integer, _, 1}, {integer, _, 2}}]},
        parse_type("?FOO(1 | 2)")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_type("?foo{}")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_type("#?foo{}")
    ).

macro_definitions(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{var, _, 'FOO'}, {atom, _, foo}]},
        parse_form("-define(FOO, foo).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{call, _, {var, _, 'FOO'}, []}, {atom, _, foo}]},
        parse_form("-define(FOO(), foo).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {call, _, {var, _, 'FOO'}, [{var, _, 'X'}]},
            {atom, _, foo}
        ]},
        parse_form("-define(FOO(X), foo).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {call, _, {atom, _, is_nice}, [{var, _, 'X'}]},
            {guard_or, _, [
                {guard_and, _, [
                    {call, _, {atom, _, is_tuple}, [{var, _, 'X'}]},
                    {op, _, '=:=', {call, _, {atom, _, element}, [{integer, _, 1}, {var, _, 'X'}]},
                        {atom, _, nice}}
                ]}
            ]}
        ]},
        parse_form("-define(is_nice(X), is_tuple(X), element(1, X) =:= nice).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {atom, _, foo},
            {record_name, _, {atom, _, bar}}
        ]},
        parse_form("-define(foo, #bar).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{atom, _, foo}, empty]},
        parse_form("-define(foo,).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {call, _, {atom, _, pass}, [{var, _, 'Name'}]},
            {'fun', _, {function, _, {var, _, 'Name'}, {integer, _, 2}}}
        ]},
        parse_form("-define(pass(Name), fun Name/2).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {atom, _, foo},
            {clause, _, {call, _, {atom, _, foo}, []}, empty, [{atom, _, ok}]}
        ]},
        parse_form("-define(foo, foo() -> ok).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {call, _, {var, _, 'FOO'}, [{var, _, 'Name'}]},
            {clause, _, {call, _, {var, _, 'Name'}, []}, empty, [{atom, _, ok}]}
        ]},
        parse_form("-define(FOO(Name), Name() -> ok).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {var, _, 'HASH_FUN'},
            {remote, _, {atom, _, erlang}, {atom, _, phash}}
        ]},
        parse_form("-define(HASH_FUN, erlang:phash).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {call, _, {atom, _, record}, [{var, _, 'N'}]},
            {record, _, {var, _, 'N'}, []}
        ]},
        parse_form("-define(record(N), #N{}).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {atom, _, parens},
            {args, _, []}
        ]},
        parse_form("-define(parens, ()).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {var, _, 'TIMEOUT_TYPE'},
            {op, _, '|', {op, _, '..', {integer, _, 0}, {integer, _, 100}}, {atom, _, infinity}}
        ]},
        parse_form("-define(TIMEOUT_TYPE, 0..100 | 'infinity').")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {var, _, 'FOO'},
            {map_field_assoc, _, {atom, _, foo}, {atom, _, bar}}
        ]},
        parse_form("-define(FOO, foo => bar).")
    ).

functions_and_funs(Config) when is_list(Config) ->
    ?assertMatch(
        {'fun', _, {function, _, {atom, _, foo}, {integer, _, 1}}},
        parse_expr("fun foo/1")
    ),
    ?assertMatch(
        {'fun', _, {function, _, {macro_call, _, {atom, _, foo}, none}, {integer, _, 1}}},
        parse_expr("fun ?foo/1")
    ),
    ?assertMatch(
        {'fun', _, {function, _, {atom, _, foo}, {macro_call, _, {atom, _, foo}, none}}},
        parse_expr("fun foo/?foo")
    ),
    ?assertMatch(
        {'fun', _, {function, _, {var, _, 'Mod'}, {atom, _, foo}, {integer, _, 1}}},
        parse_expr("fun Mod:foo/1")
    ),
    ?assertMatch(
        {'fun', _,
            {function, _, {macro_call, _, {atom, _, 'foo'}, none}, {atom, _, foo}, {integer, _, 1}}},
        parse_expr("fun ?foo:foo/1")
    ),
    ?assertMatch(
        {'fun', _,
            {function, _, {atom, _, foo}, {macro_call, _, {atom, _, foo}, none}, {integer, _, 1}}},
        parse_expr("fun foo:?foo/1")
    ),
    ?assertMatch(
        {'fun', _,
            {function, _, {atom, _, foo}, {atom, _, foo}, {macro_call, _, {atom, _, foo}, none}}},
        parse_expr("fun foo:foo/?foo")
    ),
    ?assertMatch(
        {'fun', _, {clauses, _, [{clause, _, {args, _, []}, empty, [{atom, _, ok}]}]}},
        parse_expr("fun () -> ok end")
    ),
    ?assertMatch(
        {'fun', _,
            {clauses, _, [
                {clause, _, {call, _, {var, _, 'Foo'}, []}, empty, [{atom, _, ok}]}
            ]}},
        parse_expr("fun Foo() -> ok end")
    ),
    ?assertMatch(
        {function, _, [{clause, _, {call, _, {atom, _, foo}, []}, empty, [{atom, _, ok}]}]},
        parse_form("foo() -> ok.")
    ),
    ?assertMatch(
        {function, _, [
            {clause, _, {call, _, {macro_call, _, {var, _, 'FOO'}, none}, []}, empty, [
                {atom, _, ok}
            ]}
        ]},
        parse_form("?FOO() -> ok.")
    ),
    ?assertMatch(
        {function, _, [
            {clause, _, {call, _, {atom, _, foo}, []}, empty, [{atom, _, ok}]},
            {macro_call, _, {atom, _, bar}, []}
        ]},
        parse_form("foo() -> ok; ?bar().")
    ),
    ?assertMatch(
        {function, _, [
            {macro_call, _, {var, _, 'TESTS_WITH_SETUP'}, [
                {atom, _, all_tests_},
                {'fun', _, {clauses, _, [{clause, _, {args, _, []}, empty, [{atom, _, ok}]}]}}
            ]}
        ]},
        parse_form("?TESTS_WITH_SETUP(all_tests_, fun() -> ok end).")
    ).

operators(Config) when is_list(Config) ->
    ?assertMatch(
        {op, _, '=', {integer, _, 1}, {integer, _, 2}},
        parse_expr("1 = 2")
    ),
    ?assertMatch(
        {op, _, '=', {integer, _, 1}, {integer, _, 2}},
        parse_pat("1 = 2")
    ),
    ?assertMatch(
        {op, _, 'catch', {integer, _, 1}},
        parse_expr("catch 1")
    ).

lists(Config) when is_list(Config) ->
    ?assertMatch(
        {list, _, []},
        parse_expr("[]")
    ),
    ?assertMatch(
        {list, _, [{integer, _, 1}, {integer, _, 2}]},
        parse_expr("[1,2]")
    ),
    ?assertMatch(
        {list, _, [{cons, _, {integer, _, 1}, {integer, _, 2}}]},
        parse_expr("[1 | 2]")
    ),
    ?assertMatch(
        {list, _, [{integer, _, 1}, {cons, _, {integer, _, 2}, {integer, _, 3}}]},
        parse_expr("[1, 2 | 3]")
    ),
    ?assertMatch(
        {list, _, [
            {cons, _, {op, _, 'catch', {integer, _, 1}},
                {op, _, '!', {integer, _, 2}, {integer, _, 3}}}
        ]},
        parse_expr("[catch 1 | 2 ! 3]")
    ).

binaries(Config) when is_list(Config) ->
    ?assertMatch(
        {bin, _, [{bin_element, _, {integer, _, 1}, {integer, _, 4}, default}]},
        parse_expr("<<1:4>>")
    ),
    ?assertMatch(
        {bin, _, [
            {bin_element, _, {integer, _, 1}, default, [
                {remote, _, {atom, _, unit}, {integer, _, 8}}
            ]}
        ]},
        parse_expr("<<1/unit:8>>")
    ).

maps(Config) when is_list(Config) ->
    ?assertMatch(
        {map, _, [{macro_call, _, {var, _, 'FOO'}, none}]},
        parse_expr("#{?FOO}")
    ).

clauses(Config) when is_list(Config) ->
    ?assertMatch(
        {'if', _, [
            {clause, _, empty, {guard_or, _, [{guard_and, _, [{atom, _, true}]}]}, [
                {atom, _, ok}
            ]}
        ]},
        parse_expr("if true -> ok end")
    ),
    ?assertMatch(
        {'case', _, {var, _, 'X'}, [
            {clause, _, {atom, _, true}, empty, [{atom, _, ok}]}
        ]},
        parse_expr("case X of true -> ok end")
    ),
    ?assertMatch(
        {'receive', _, {clauses, _, [{clause, _, {var, _, '_'}, empty, [{atom, _, true}]}]}},
        parse_expr("receive _ -> true end")
    ),
    ?assertMatch(
        {'try', _, {body, _, [{atom, _, ok}]},
            {clauses, _, [{clause, _, {var, _, '_'}, empty, [{atom, _, ok}]}]},
            {clauses, _, [
                {clause, _, {var, _, '_'}, empty, [{atom, _, ok}]},
                {clause, _, {'catch', _, [{var, _, '_'}, {var, _, '_'}]}, empty, [
                    {atom, _, ok}
                ]},
                {clause, _, {'catch', _, [{var, _, '_'}, {var, _, '_'}, {var, _, '_'}]}, empty, [
                    {atom, _, ok}
                ]}
            ]},
            []},
        parse_expr("try ok of _ -> ok catch _ -> ok; _:_ -> ok; _:_:_ -> ok end")
    ).

types(Config) when is_list(Config) ->
    ?assertMatch(
        {op, _, '::', {var, _, 'Foo'}, {atom, _, foo}},
        parse_type("Foo :: foo")
    ),
    ?assertMatch(
        {op, _, '|', {var, _, 'Foo'}, {var, _, 'Bar'}},
        parse_type("Foo | Bar")
    ),
    ?assertMatch(
        {op, _, '..', {integer, _, 1}, {var, _, 'Bar'}},
        parse_type("1..Bar")
    ),
    ?assertMatch(
        {op, _, '+', {op, _, '-', {integer, _, 1}}, {op, _, '*', {integer, _, 2}, {integer, _, 3}}},
        parse_type("- 1 + 2 * 3")
    ),
    ?assertMatch(
        {tuple, _, [
            {call, _, {atom, _, foo}, []},
            {call, _, {remote, _, {atom, _, foo}, {atom, _, bar}}, []}
        ]},
        parse_type("{foo(), foo:bar()}")
    ),
    ?assertMatch(
        {list, _, [{list, _, [{list, _, []}]}, {'...', _}]},
        parse_type("[[[]], ...]")
    ),
    ?assertMatch(
        {map, _, [
            {map_field_exact, _, {map, _, []}, {integer, _, 1}},
            {map_field_assoc, _, {tuple, _, []}, {integer, _, 2}},
            {macro_call, _, {var, _, 'FOO'}, none}
        ]},
        parse_type("#{#{} := 1, {} => 2, ?FOO}")
    ),
    ?assertMatch(
        {tuple, _, [
            {bin, _, []},
            {bin, _, [{bin_element, _, {var, _, '_'}, {integer, _, 8}, default}]},
            {bin, _, [
                {bin_element, _, {var, _, '_'}, {bin_size, _, {var, _, '_'}, {integer, _, 8}},
                    default}
            ]},
            {bin, _, [
                {bin_element, _, {var, _, '_'}, {integer, _, 8}, default},
                {bin_element, _, {var, _, '_'}, {bin_size, _, {var, _, '_'}, {integer, _, 4}},
                    default}
            ]}
        ]},
        parse_type("{<<>>, <<_:8>>, <<_:_*8>>, <<_:8, _:_*4>>}")
    ),
    ?assertMatch(
        {'fun', _, type},
        parse_type("fun()")
    ),
    ?assertMatch(
        {'fun', _, {type, _, {args, _, [{'...', _}]}, {call, _, {atom, _, integer}, []}}},
        parse_type("fun((...) -> integer())")
    ),
    ?assertMatch(
        {'fun', _, {type, _, {args, _, []}, {atom, _, ok}}},
        parse_type("fun(() -> ok)")
    ),
    ?assertMatch(
        {'fun', _,
            {type, _, {args, _, [{call, _, {atom, _, integer}, []}]},
                {call, _, {atom, _, integer}, []}}},
        parse_type("fun((integer()) -> integer())")
    ).

annos(Config) when is_list(Config) ->
    %% We parse with leading \n to avoid accounting for extra stuff parse_expr does
    ?assertMatch(
        {integer, #{location := {2, 1}, end_location := {2, 4}, text := "100"}, 100},
        parse_expr("\n100")
    ),
    ?assertMatch(
        {float, #{location := {2, 1}, end_location := {2, 5}, text := "10.0"}, 10.0},
        parse_expr("\n10.0")
    ),
    ?assertMatch(
        {char, #{location := {2, 1}, end_location := {2, 4}, text := "$\\s"}, $\s},
        parse_expr("\n$\\s")
    ),
    ?assertMatch(
        {atom, #{location := {2, 1}, end_location := {2, 4}, text := "foo"}, foo},
        parse_expr("\nfoo")
    ),
    ?assertMatch(
        {string, #{location := {2, 1}, end_location := {2, 6}, text := "\"foo\""}, "foo"},
        parse_expr("\n\"foo\"")
    ),
    ?assertMatch(
        {var, #{location := {2, 1}, end_location := {2, 4}, text := "Foo"}, 'Foo'},
        parse_expr("\nFoo")
    ),
    ?assertMatch(
        {atom,
            #{
                pre_comments := [
                    {comment, #{location := {2, 1}, end_location := {3, 6}}, [
                        "%foo",
                        "%bar"
                    ]}
                ]
            },
            ok},
        parse_expr(
            "\n%foo\n"
            " %bar\n"
            "ok"
        )
    ),
    ?assertMatch(
        {op, #{location := {2, 1}, end_location := {2, 6}}, '+', _, _},
        parse_expr("\n1 + 2")
    ),
    ?assertMatch(
        {list, #{location := {2, 1}, end_location := {3, 4}}, [
            {call, #{location := {2, 2}, end_location := {2, 16}}, _, [
                {tuple, #{location := {2, 6}, end_location := {2, 12}}, [_, _]},
                _
            ]},
            {map, #{location := {2, 18}, end_location := {3, 3}}, [
                {map_field_assoc, #{location := {2, 20}, end_location := {3, 2}}, _, _}
            ]}
        ]},
        parse_expr("\n[foo({1, 2}, 3), #{4 =>\n5}]")
    ),
    ?assertMatch(
        {attribute, #{location := {1, 1}, end_location := {1, 11}},
            {atom, #{location := {1, 2}, end_location := {1, 7}, text := "'foo'"}, foo}, [
                _
            ]},
        parse_form("-'foo'(1).")
    ),
    ?assertMatch(
        {function, #{location := {1, 1}, end_location := {2, 14}}, [
            {clause, #{location := {1, 1}, end_location := {1, 12}}, _, empty, [_]},
            {clause, #{location := {2, 1}, end_location := {2, 13}}, _, empty, [_]}
        ]},
        parse_form(
            "foo(a) -> 1;\n"
            "foo(b) -> 12."
        )
    ),
    ?assertMatch(
        [
            {attribute, _, _, _},
            {function, #{pre_comments := [{comment, _, ["% comment"]}]}, _}
        ],
        parse_forms(
            "-spec foo() -> ok.\n"
            "% comment\n"
            "foo() -> ok."
        )
    ),
    ?assertMatch(
        {call, #{location := {1, _}}, _, [
            {list, #{location := {2, _}}, [
                {tuple, #{location := {3, _}}, [
                    {bin, #{location := {4, _}}, [_]}
                ]}
            ]}
        ]},
        parse_expr(
            "foo(\n"
            "    [  \n"
            "        { % foo\n"
            "            <<\n"
            "                x\n"
            "            >>\n"
            "        }"
            "    ]"
            ")"
        )
    ).

shebang(Config) when is_list(Config) ->
    ?assertMatch(
        [
            {shebang, _, "#! /usr/bin/env escript"},
            {function, #{location := {2, 1}}, _}
        ],
        parse_forms(
            "#! /usr/bin/env escript\n"
            "main(_) -> ok."
        )
    ).

parse_expr(String) ->
    {function, _, [{clause, _, _, empty, [Expr]}]} = parse_form("f() -> " ++ String ++ "."),
    Expr.

parse_pat(String) ->
    {function, _, [{clause, _, {call, _, _, [Pat]}, empty, [_]}]} =
        parse_form("f(" ++ String ++ ") -> ok."),
    Pat.

parse_type(String) ->
    {attribute, _, {atom, _, type}, [{op, _, '::', _, Type}]} =
        parse_form("-type foo() :: " ++ String ++ "."),
    Type.

parse_form(String) ->
    case erlfmt:read_nodes_string("nofile", String) of
        {ok, [Form], []} ->
            Form;
        {ok, _, [Warning | _]} ->
            ct:fail(
                "Expected successful parse: \n~ts\n for warning: ~ts",
                [String, erlfmt:format_error_info(Warning)]
            );
        {error, {_, Mod, Reason}} ->
            ct:fail(
                "Expected successful parse:\n~ts\ngot: ~ts",
                [String, Mod:format_error(Reason)]
            )
    end.

parse_forms(String) ->
    case erlfmt:read_nodes_string("nofile", String) of
        {ok, Forms, []} ->
            Forms;
        {ok, _, [Warning | _]} ->
            ct:fail(
                "Expected successful parse: \n~ts\n for warning: ~ts",
                [String, erlfmt:format_error_info(Warning)]
            );
        {error, {_, Mod, Reason}} ->
            ct:fail(
                "Expected successful parse:\n~ts\ngot: ~ts",
                [String, Mod:format_error(Reason)]
            )
    end.

snapshot_simple_comments(Config) -> snapshot_same("simple_comments.erl", Config).

snapshot_big_binary(Config) -> snapshot_same("big_binary.erl", Config).

snapshot_attributes(Config) -> snapshot_same("attributes.erl", Config).

snapshot_escript(Config) -> snapshot_same("escript.erl", Config).

%% On the third line (or second line depending on the presence of the Emacs directive),
%% arguments can be specified to the emulator
%% https://erlang.org/doc/man/escript.html
snapshot_emulator_args(Config) -> snapshot_same("emulator_args.escript", Config).

%% On the third line (or second line depending on the presence of the Emacs directive),
%% arguments can be specified to the emulator
%% https://erlang.org/doc/man/escript.html
snapshot_emulator_args2(Config) -> snapshot_same("emulator_args2.escript", Config).

%% On the third line (or second line depending on the presence of the Emacs directive),
%% arguments can be specified to the emulator
%% https://erlang.org/doc/man/escript.html
snapshot_emulator_args_module(Config) -> snapshot_same("emulator_args_module.escript", Config).

snapshot_pragma(Config) -> snapshot_same("pragma.erl", [{pragma, require} | Config]).

snapshot_no_pragma(Config) -> snapshot_same("no_pragma.erl", [{pragma, require} | Config]).

snapshot_script(Config) -> snapshot_same("rebar.config.script", Config).

snapshot_comments(Config) -> snapshot_formatted("comments.erl", Config).

snapshot_broken(Config) -> snapshot_formatted("broken.erl", Config).

snapshot_overlong(Config) -> snapshot_formatted("overlong.erl", Config).

snapshot_otp_examples(Config) -> snapshot_formatted("otp_examples.erl", Config).

snapshot_ignore_format(Config) -> snapshot_formatted("ignore_format.erl", Config).

snapshot_empty(Config) -> snapshot_same("empty.erl", Config).

snapshot_insert_pragma_with(Config) when is_list(Config) ->
    snapshot_same("pragma.erl", [{pragma, insert} | Config]).

snapshot_same(Module, Config) ->
    Pragma = proplists:get_value(pragma, Config, ignore),
    snapshot_match(Module, Module, Config, [{pragma, Pragma}]).

snapshot_formatted(Module, Config) ->
    % Sanity check via idempotence: the reference file is up-to-date.
    snapshot_same(Module ++ ".formatted", Config),
    snapshot_match(Module ++ ".formatted", Module, Config, []).

snapshot_match(FormattedModule, Module, Config, Options) ->
    % Format `Module` and check it matchs `FormattedModule`.
    DataDir = ?config(data_dir, Config),
    {ok, FormattedBin} = file:read_file(filename:join([DataDir, FormattedModule])),
    {ok, OriginalBin} = file:read_file(filename:join([DataDir, Module])),
    Formatted = unicode:characters_to_list(FormattedBin),
    Original = unicode:characters_to_list(OriginalBin),
    Output = erlfmt:format_string(Original, Options),
    assert_snapshot_match(Formatted, Output).

assert_snapshot_match(Expected, Output) ->
    %% Check the formatted result matches the reference.
    case Output of
        {ok, Expected, _} ->
            ok;
        {skip, _} ->
            ok;
        {ok, Other, _} ->
            % Split by lines.
            Expected2 = string:lexemes(Expected, [$\n]),
            Other2 = string:lexemes(Other, [$\n]),
            % We already know they are not equal,
            % this macro gives a better diagnostic.
            ?assertListEqual(Expected2, Other2);
        Other ->
            ct:fail("unexpected: ~p~n", [Other])
    end.

simple_comments_range(Config) ->
    format_range(Config, "simple_comments.erl").

broken_range(Config) ->
    format_range(Config, "broken.erl").

format_range(Config, File) ->
    DataDir = ?config(data_dir, Config),
    Path = DataDir ++ File,
    case erlfmt:format_file_range(Path, {3, 45}, {47, 1}, []) of
        {ok, _Output, _} -> ok;
        {options, Options} -> range_format_exact(Options, Path)
    end.

range_format_exact([], _Path) ->
    ok;
range_format_exact([{Start, End} | Options], Path) ->
    {ok, _Output, _} = erlfmt:format_file_range(Path, Start, End, []),
    range_format_exact(Options, Path).

snapshot_range_whole_comments(Config) ->
    % When range enclose whole file,
    % the whole file must be formated!
    Module = "comments.erl",
    DataDir = ?config(data_dir, Config),
    {ok, FormattedBin} = file:read_file(filename:join([DataDir, Module ++ ".formatted"])),
    Path = filename:join(DataDir, Module),
    % All forms enclosed, so must return {ok, FormattedOutput, _}.
    Output = erlfmt:format_file_range(Path, {1, 1}, {38, 25}, []),
    assert_snapshot_match(FormattedBin, Output).

snapshot_range_partial(_) ->
    % Check only the specified form is formatted.
    Original =
        "x()->0.\n"
        "y()   ->  1.\n"
        "z()   ->  2.\n",
    % Only x() should be touched.
    Reference = "x() -> 0.\n",
    Output = erlfmt:format_string_range(Original, {1, 1}, {1, 8}, []),
    assert_snapshot_match(list_to_binary(Reference), Output).

snapshot_range_partial2(_) ->
    % Check only the specified form is formatted, too,
    % with a range spanning on two lines.
    Original =
        "x()->\n"
        "0.\n"
        "y()   ->  1.\n",
    % Only x() should be touched.
    Reference =
        "x() ->\n"
        "    0.",
    Output = erlfmt:format_string_range(Original, {1, 1}, {2, 3}, []),
    assert_snapshot_match(list_to_binary(Reference), Output).

snapshot_enclosing_range(_) ->
    % Check we pick format the whole top level form covering passed range.
    % Same Original and reference than snapshot_range_partial2.
    Original =
        "x()->\n"
        "0.\n"
        "y()   ->  1.\n",
    Reference =
        "x() ->\n"
        "    0.",
    % Range is just the first char (mimic e.g. function renaming).
    Output = erlfmt:format_string_range(Original, {1, 1}, {1, 2}, []),
    assert_snapshot_match(list_to_binary(Reference), Output).

snapshot_enclosing_range2(_) ->
    % Same test when picking second part of the form:
    % passed range doesn't intersect 'top level'.
    Original =
        "x()->\n"
        " 0.\n"
        "y()   ->  1.\n",
    Reference =
        "x() ->\n"
        "    0.",
    Output = erlfmt:format_string_range(Original, {2, 2}, {2, 3}, []),
    assert_snapshot_match(list_to_binary(Reference), Output).

snapshot_enclosing_range_no_leak(_) ->
    % Check only the specified form is formatted.
    Original =
        "x()->\n"
        " 0.\n"
        "\n"
        "y()   ->  1.\n",
    Reference =
        "x() ->\n"
        "    0.",
    % End point overshots line 3, but doesn't reach line 4:
    % Only x() must be formatted.
    Output = erlfmt:format_string_range(Original, {1, 1}, {3, 3}, []),
    assert_snapshot_match(list_to_binary(Reference), Output).

contains_pragma(Config) when is_list(Config) ->
    ?assert(
        contains_pragma_string(
            "%% % @format\n"
            "\n"
            "-module(pragma).\n"
            "\n"
            "-export([f/3]).\n"
            "\n"
            "f(_Arg1,_Arg2,   _Arg3) ->\n"
            "ok.\n"
        )
    ),
    ?assert(
        contains_pragma_string(
            "%% @format\n"
            "\n"
            "-module(pragma).\n"
            "\n"
            "-export([f/3]).\n"
            "\n"
            "f(_Arg1,_Arg2,   _Arg3) ->\n"
            "ok.\n"
        )
    ),
    ?assert(
        contains_pragma_string(
            "\n"
            "\n"
            "%% % @format\n"
            "\n"
            "-module(pragma).\n"
        )
    ),
    ?assertNot(
        contains_pragma_string(
            "-module(pragma).\n"
            "-export([f/3]).\n"
            "\n"
            "f(_Arg1,_Arg2,   _Arg3) ->\n"
            "ok.\n"
        )
    ),
    ?assertNot(
        contains_pragma_string(
            "%%% LICENSE\n"
            "%%% LICENSE\n"
            "%%% LICENSE\n"
            "%%% LICENSE\n"
            "\n"
            "-module(pragma)\n."
            "\n"
            "-export([f/3]).\n"
            "\n"
            "f(_Arg1,_Arg2,   _Arg3) ->\n"
            "ok.\n"
        )
    ),
    ?assert(
        contains_pragma_string(
            "%% LICENSE\n"
            "%% LICENSE\n"
            "%% LICENSE\n"
            "%% LICENSE\n"
            "\n"
            "%% % @format\n"
            "\n"
            "-module(pragma).\n"
            "\n"
            "-export([f/3]).\n"
            "\n"
            "f(_Arg1,_Arg2,   _Arg3) ->\n"
            "ok.\n"
        )
    ),
    ?assert(
        contains_pragma_string(
            "% @format\n"
            "-module(pragma).\n"
        )
    ),
    ?assertNot(
        contains_pragma_string(
            "{erl_opts, [debug_info]}\n"
        )
    ),
    ?assert(
        contains_pragma_string(
            "%% % @format\n"
            "\n"
            "{erl_opts, [debug_info]}\n"
        )
    ),
    ?assert(
        contains_pragma_string(
            "#! /usr/bin/env escript\n"
            "\n"
            "%% % @format\n"
            "\n"
            "main(_) -> ok.\n"
        )
    ),
    ?assertNot(
        contains_pragma_string(
            "#! /usr/bin/env escript\n"
            "\n"
            "main(_) -> ok.\n"
        )
    ).

insert_pragma(Config) when is_list(Config) ->
    ?assertEqual(
        "%%% % @format\n"
        "\n"
        "-module(pragma).\n",
        insert_pragma_string(
            "-module(pragma).\n"
        )
    ),
    ?assertEqual(
        "%%% % @format\n"
        "\n"
        "-module(pragma).\n"
        "\n"
        "-export([f/3]).\n"
        "\n"
        "f(_Arg1, _Arg2, _Arg3) ->\n"
        "    ok.\n",
        insert_pragma_string(
            "-module(pragma).\n"
            "\n"
            "-export([f/3]).\n"
            "\n"
            "f(_Arg1,_Arg2,   _Arg3) ->\n"
            "ok.\n"
        )
    ),
    ?assertEqual(
        "%%% attached comment\n"
        "%%% % @format\n"
        "-module(pragma).\n"
        "\n"
        "-export([f/3]).\n",
        insert_pragma_string(
            "%%% attached comment\n"
            "-module(pragma).\n"
            "\n"
            "-export([f/3]).\n"
        )
    ),
    ?assertEqual(
        "%%% single comment\n"
        "%%% % @format\n"
        "\n"
        "-module(pragma).\n"
        "\n"
        "-export([f/3]).\n",
        insert_pragma_string(
            "%%% single comment\n"
            "\n"
            "-module(pragma).\n"
            "\n"
            "-export([f/3]).\n"
        )
    ),
    ?assertEqual(
        "%%% LICENSE\n"
        "%%% LICENSE\n"
        "%%% LICENSE\n"
        "%%% LICENSE\n"
        "%%% % @format\n"
        "\n"
        "-module(pragma).\n"
        "\n"
        "-export([f/3]).\n",
        insert_pragma_string(
            "%%% LICENSE\n"
            "%%% LICENSE\n"
            "%%% LICENSE\n"
            "%%% LICENSE\n"
            "\n"
            "-module(pragma).\n"
            "\n"
            "-export([f/3]).\n"
        )
    ),
    ?assertEqual(
        "%%% % @format\n"
        "\n"
        "{erl_opts, [debug_info]}\n",
        insert_pragma_string(
            "{erl_opts, [debug_info]}\n"
        )
    ),
    ?assertEqual(
        "#! /usr/bin/env escript\n"
        "\n"
        "%%% % @format\n"
        "\n"
        "main(_) -> ok.\n",
        insert_pragma_string(
            "#! /usr/bin/env escript\n"
            "\n"
            "main(_) -> ok.\n"
        )
    ).

contains_pragma_string(String) ->
    case erlfmt:format_string(String, [{pragma, require}]) of
        {skip, _} -> false;
        _ -> true
    end.

insert_pragma_string(String) ->
    {ok, StringWithPragma, []} = erlfmt:format_string(String, [{pragma, insert}]),
    %% check that insert_pragma_nodes doesn't insert a pragma, when one has already been inserted.
    ?assertEqual(
        erlfmt:format_string(StringWithPragma, [{pragma, insert}]),
        {ok, StringWithPragma, []}
    ),
    StringWithPragma.

overlong_warning(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    FileName = filename:join([DataDir, "overlong.erl"]),
    Options = [verbose],
    {ok, Formatted, FileWarnings} = erlfmt:format_file(FileName, Options),
    FormattedList = unicode:characters_to_list(Formatted),
    {ok, _, StringWarnings} = erlfmt:format_string(FormattedList, Options),
    {ok, _, RangeWarnings} = erlfmt:format_file_range(FileName, {1, 1}, {11, 8}, Options),
    FileLongLines = [{LineNo, Length} || {_, LineNo, _, {long_line, Length, _}} <- FileWarnings],
    StringLongLines = [
        {LineNo, Length}
     || {_, LineNo, _, {long_line, Length, _}} <- StringWarnings
    ],
    RangeLongLines = [{LineNo, Length} || {_, LineNo, _, {long_line, Length, _}} <- RangeWarnings],
    % Line 6 is an overlong comment
    % Line 8 is a comment which would be too long if we counted bytes of a utf-8 encoding instead of glyphs
    ?assert(lists:member({6, 121}, FileLongLines)),
    ?assertEqual([], [Line || {8, _} = Line <- FileLongLines]),
    ?assert(lists:member({6, 121}, StringLongLines)),
    ?assertEqual([], [Line || {8, _} = Line <- StringLongLines]),
    ?assert(lists:member({6, 121}, RangeLongLines)),
    ?assertEqual([], [Line || {8, _} = Line <- RangeLongLines]).

do_not_crash_on_bad_record(Config) when is_list(Config) ->
    %% normal record
    ?assertMatch(
        {ok, [{attribute, _, {atom, _, record}, _}], _},
        erlfmt:read_nodes_string("nofile", "-record(r, { field :: string() }).")
    ),
    %% bad record declaration
    {ok, [{exprs, _, _}], _} = erlfmt:read_nodes_string("nofile", "-record(r, [1])."),
    %% bad record field
    {ok, [{exprs, _, _}], _} = erlfmt:read_nodes_string("nofile", "-record(r, {2})."),
    %% bad record macro field
    {ok, [_, Attr], _} = erlfmt:read_nodes_string(
        "nofile",
        "-define(F1, field1).\n"
        "-record(r, {?F1}).\n"
    ),
    ?assertMatch(
        {attribute, _, {atom, _, record}, [
            {atom, _, r},
            {tuple, _, [
                {record_field, _, {macro_call, _, _, _}}
            ]}
        ]},
        Attr
    ),
    %% bad record macro field 2
    {ok, [Attr2], _} = erlfmt:read_nodes_string(
        "nofile",
        "-record(foo, {?FOO, b :: any()})."
    ),
    ?assertMatch(
        {attribute, _, {atom, _, record}, [
            {atom, _, foo},
            {tuple, _, [
                {record_field, _, {macro_call, _, _, _}},
                {op, _, _, {record_field, _, _}, {call, _, _, _}}
            ]}
        ]},
        Attr2
    ).

raw_string_anno(Config) when is_list(Config) ->
    Broken =
        "foo() ->\n"
        " 1 2 3\n"
        ".",
    ?assertMatch(
        {ok, [{raw_string, #{end_location := {3, 2}}, Broken}], _},
        erlfmt:read_nodes_string("nofile", Broken)
    ),

    TrailingNl =
        "foo() -> 1 2 3.\n"
        "\n"
        "\n"
        "\n",
    Trimmed = "foo() -> 1 2 3.",
    EndCol = length(Trimmed) + 1,
    ?assertMatch(
        {ok, [{raw_string, #{end_location := {1, EndCol}}, Trimmed}], _},
        erlfmt:read_nodes_string("nofile", TrailingNl)
    ),

    Incomplete =
        "foo() ->\n"
        " 123",
    ?assertMatch(
        {ok, [{raw_string, #{end_location := {2, 5}}, Incomplete}], _},
        erlfmt:read_nodes_string("nofile", Incomplete)
    ),

    Comment =
        "foo() ->\n"
        " 1 2 3. %% comment\n"
        "\n"
        "bar() -> ok.",
    ?assertMatch(
        {ok,
            [
                {raw_string, #{end_location := {2, 19}}, _},
                {function, #{location := {4, 1}}, _}
            ],
            _},
        erlfmt:read_nodes_string("nofile", Comment)
    ).
