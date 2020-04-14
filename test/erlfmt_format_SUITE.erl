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
-module(erlfmt_format_SUITE).

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
    literals/1,
    string_concat/1,
    unary_operator/1,
    binary_operator/1,
    tuple/1,
    list/1,
    binary/1,
    map_create/1,
    map_update/1,
    record_create/1,
    record_update/1,
    record_index/1,
    record_field/1,
    list_comprehension/1,
    binary_comprehension/1,
    call/1,
    block/1,
    fun_expression/1,
    case_expression/1,
    receive_expression/1,
    try_expression/1,
    if_expression/1,
    macro/1,
    function/1,
    attribute/1,
    comment/1,
    force_break/1
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
        {expressions, [parallel], [
            literals,
            string_concat,
            {group, containers},
            {group, operators},
            {group, comprehensions},
            call,
            block,
            fun_expression,
            case_expression,
            receive_expression,
            try_expression,
            if_expression,
            macro
        ]},
        {forms, [parallel], [
            function,
            attribute
        ]},
        {operators, [parallel], [
            unary_operator,
            binary_operator
        ]},
        {containers, [parallel], [
            tuple,
            list,
            binary,
            map_create,
            map_update,
            {group, records},
            force_break
        ]},
        {records, [parallel], [
            record_create,
            record_update,
            record_index,
            record_field
        ]},
        {comprehensions, [parallel], [
            list_comprehension,
            binary_comprehension
        ]}
    ].

all() ->
    [
        {group, expressions},
        {group, forms},
        comment
    ].

%%--------------------------------------------------------------------
%% TEST CASES

-define(assertSameExpr(String), ?assertSameExpr(String, 80)).
-define(assertSameExpr(String, PageWidth), ?assertEqual(String, format_expr(String, PageWidth))).

-define(assertFormatExpr(Bad, Good), ?assertFormatExpr(Bad, Good, 80)).
-define(assertFormatExpr(Bad, Good, PageWidth), begin
    ?assertEqual(Good, format_expr(Good, PageWidth)),
    ?assertEqual(Good, format_expr(Bad, PageWidth))
end).

-define(assertSameForm(String), ?assertSameForm(String, 80)).
-define(assertSameForm(String, PageWidth), ?assertEqual(String, format_form(String, PageWidth))).

-define(assertFormatForm(Bad, Good), ?assertFormatForm(Bad, Good, 80)).
-define(assertFormatForm(Bad, Good, PageWidth), begin
    ?assertEqual(Good, format_form(Good, PageWidth)),
    ?assertEqual(Good, format_form(Bad, PageWidth))
end).

literals(Config) when is_list(Config) ->
    ?assertSameExpr("100"),
    ?assertSameExpr("007"),
    ?assertSameExpr("10#1000"),
    ?assertSameExpr("2#101"),
    ?assertSameExpr("16#01"),
    ?assertSameExpr("16#deadBEEF"),
    ?assertSameExpr("001.100"),
    ?assertSameExpr("1.0e-1"),
    ?assertSameExpr("001.100e-010"),
    ?assertSameExpr("1.0E01"),
    ?assertSameExpr("$a"),
    ?assertSameExpr("$\x{1F605}"),
    ?assertSameExpr("$\\0"),
    ?assertSameExpr("$\\\\"),
    ?assertSameExpr("$\\t"),
    ?assertSameExpr("$\\z"),
    ?assertSameExpr("$ "),
    ?assertSameExpr("$\\040"),
    ?assertSameExpr("$\\xAb"),
    ?assertSameExpr("$\\x{Ab}"),
    ?assertSameExpr("'foo'"),
    ?assertSameExpr("foo"),
    ?assertSameExpr("'andalso'"),
    ?assertSameExpr("'foo bar'"),
    ?assertSameExpr("'foo\\tbar'"),
    ?assertSameExpr("'\\''"),
    ?assertSameExpr("'foo\\xaB'"),
    ?assertSameExpr("'foo\\x{aB}'"),
    ?assertSameExpr("'foo\\z'"),
    ?assertSameExpr("'foo\\s'"),
    ?assertSameExpr("\"'\""),
    ?assertSameExpr("\"\x{1F631}\""),
    ?assertSameExpr("\" \\40\\x32\\x{0032}\""),
    ?assertSameExpr("\"The quick brown fox jumps over the lazy dog\""),
    ?assertSameExpr("\"\\s \""),
    ?assertSameExpr("Foo"),
    ?assertSameExpr("_Bar").

string_concat(Config) when is_list(Config) ->
    ?assertSameExpr("\"foo\" \"bar\""),
    ?assertFormatExpr(
        "\"foo\" \"bar\"",
        "\"foo\"\n"
        "\"bar\"",
        5
    ),

    %% Measures text size in graphemes (\x{61}\x{301} is á in NFD normalisation)
    ?assertSameExpr("\"\x{61}\x{301}\" \"á\"", 7),
    ?assertFormatExpr(
        "\"\x{61}\x{301}\" \"á\"",
        "\"\x{61}\x{301}\"\n"
        "\"á\"",
        6
    ),

    ?assertSameExpr("\"foo\" Foo \"bar\""),

    %% Multiline strings are converted to sequence of concats
    ?assertFormatExpr(
        "\"foo\nbar\"",
        "\"foo\\n\"\n"
        "\"bar\""
    ),
    ?assertFormatExpr(
        "\"foo\nbar\n\"",
        "\"foo\\n\"\n"
        "\"bar\\n\""
    ),
    ?assertFormatExpr(
        "\"foo\n\"",
        "\"foo\\n\""
    ).

unary_operator(Config) when is_list(Config) ->
    %% Formats symbolic operators without space
    ?assertFormatExpr("+ 1", "+1"),
    ?assertFormatExpr("- 1", "-1"),

    %% Formats word operators with space
    ?assertSameExpr("bnot 1"),
    ?assertSameExpr("not true"),
    ?assertSameExpr("catch 1"),

    %% Does not change parenthesis, only whitespace
    ?assertFormatExpr("bnot+1", "bnot +1"),
    ?assertSameExpr("+ +1"),
    ?assertSameExpr("+(+1)"),
    ?assertSameExpr("+ -1"),
    ?assertSameExpr("-(+1)"),
    ?assertSameExpr("not catch 1"),
    ?assertFormatExpr("not(1 + 1)", "not (1 + 1)"),
    ?assertFormatExpr("(bnot 1)*1", "(bnot 1) * 1"),
    ?assertSameExpr("(catch 1) + 1"),

    %% Unless it's nested not or bnot
    ?assertSameExpr("bnot bnot Var"),
    ?assertSameExpr("not not true").

binary_operator(Config) when is_list(Config) ->
    %% No changes to parens
    ?assertSameExpr("CRC bxor Byte band 16#ff"),
    ?assertSameExpr("(CRC bsl 8) bxor Byte"),
    ?assertSameExpr("Foo ++ Bar ++ Baz -- Bat"),
    ?assertSameExpr("Foo ++ Bar ++ (Baz -- Bat)"),
    ?assertSameExpr("Foo ++ (Bar ++ Baz) -- Bat"),
    ?assertSameExpr("Foo > Bar andalso Baz =:= Bat"),
    ?assertSameExpr("Foo and Bar or (Baz and Bat)"),

    %% Nested same operator right-associative
    ?assertSameExpr("Foo ++ Bar ++ Baz"),
    ?assertFormatExpr(
        "Foo ++ Bar ++ Baz",
        "Foo ++\n"
        "    Bar ++ Baz",
        15
    ),
    ?assertFormatExpr(
        "Foo ++ Bar ++ Baz",
        "Foo ++\n"
        "    Bar ++\n"
        "    Baz",
        5
    ),
    ?assertFormatExpr(
        "(Foo ++ Bar) ++ Baz",
        "(Foo ++ Bar) ++\n"
        "    Baz",
        15
    ),
    ?assertFormatExpr(
        "(Foo ++ Bar) ++ Baz",
        "(Foo ++\n"
        "    Bar) ++\n"
        "    Baz",
        5
    ),

    %% Nested same operator left-associative
    ?assertSameExpr("Foo + Bar + Baz"),
    ?assertFormatExpr(
        "Foo + Bar + Baz",
        "Foo + Bar +\n"
        "    Baz",
        14
    ),
    ?assertFormatExpr(
        "Foo + Bar + Baz",
        "Foo +\n"
        "    Bar +\n"
        "    Baz",
        5
    ),
    ?assertFormatExpr(
        "Foo + (Bar + Baz)",
        "Foo +\n"
        "    (Bar + Baz)",
        15
    ),
    ?assertFormatExpr(
        "Foo + (Bar + Baz)",
        "Foo +\n"
        "    (Bar +\n"
        "        Baz)",
        5
    ),

    %% With precedence
    ?assertFormatExpr("(A + B) == C + D", "(A + B) == C + D"),
    ?assertSameExpr("A + (B == C) + D"),
    ?assertFormatExpr(
        "A + B == (C + D)",
        "A + B ==\n"
        "    (C + D)",
        15
    ),
    ?assertFormatExpr(
        "Foo * (B + C) * D",
        "Foo *\n"
        "    (B + C) *\n"
        "    D",
        13
    ),
    ?assertFormatExpr(
        "A * (B + C) * D",
        "A * (B + C) *\n"
        "    D",
        13
    ),
    ?assertFormatExpr(
        "One * (Two + Three + Four) * Five",
        "One *\n"
        "    (Two + Three +\n"
        "        Four) * Five",
        25
    ),

    %% Next break fits
    ?assertSameExpr(
        "Foo = [\n"
        "    1\n"
        "]"
    ),
    ?assertSameExpr(
        "Foo = foo(\n"
        "    1\n"
        ")"
    ),
    ?assertSameExpr(
        "Foo = ?foo(\n"
        "    1\n"
        ")"
    ),
    ?assertSameExpr(
        "Foo = fun\n"
        "    () -> ok\n"
        "end",
        15
    ),
    ?assertSameExpr(
        "foo() :: #{\n"
        "    a := integer()\n"
        "}"
    ).

tuple(Config) when is_list(Config) ->
    ?assertFormatExpr("{ }", "{}"),
    ?assertFormatExpr("{1,2}", "{1, 2}"),
    ?assertFormatExpr("{1,{2,3}}", "{1, {2, 3}}"),
    ?assertFormatExpr(
        "{long,{2,3,4}}",
        "{\n"
        "    long,\n"
        "    {2, 3, 4}\n"
        "}",
        15
    ),
    ?assertFormatExpr(
        "{{long, word},{long,word}}",
        "{\n"
        "    {\n"
        "        long,\n"
        "        word\n"
        "    },\n"
        "    {\n"
        "        long,\n"
        "        word\n"
        "    }\n"
        "}",
        15
    ),
    ?assertFormatExpr(
        "{short, {long, word}}",
        "{short, {\n"
        "    long,\n"
        "    word\n"
        "}}",
        15
    ).

list(Config) when is_list(Config) ->
    ?assertFormatExpr("[\n]", "[]"),
    ?assertSameExpr("[1 | [2 | []]]"),
    ?assertFormatExpr("[ 1 ,2,3, 4]", "[1, 2, 3, 4]"),
    ?assertFormatExpr("[1,2,3|4]", "[1, 2, 3 | 4]"),
    ?assertFormatExpr(
        "[long,[2,3,4]]",
        "[\n"
        "    long,\n"
        "    [2, 3, 4]\n"
        "]",
        15
    ),
    ?assertFormatExpr(
        "[11,2|3]",
        "[\n"
        "    11,\n"
        "    2 | 3\n"
        "]",
        10
    ),
    ?assertFormatExpr(
        "[11,22|33]",
        "[\n"
        "    11,\n"
        "    22\n"
        "    | 33\n"
        "]",
        10
    ),
    ?assertFormatExpr(
        "[short, [long, word]]",
        "[short, [\n"
        "    long,\n"
        "    word\n"
        "]]",
        15
    ).

binary(Config) when is_list(Config) ->
    ?assertFormatExpr("<< >>", "<<>>"),
    ?assertSameExpr("<<(1 + 1), (#{}), (#foo{}), (#{}#{}), (#foo{}#foo{}), (#foo.bar), (call())>>"),
    ?assertSameExpr("<<(1), 1>>"),
    ?assertSameExpr("<<+1:5/integer-unit:8>>"),
    ?assertSameExpr("<<\"żółć\"/utf8>>"),
    ?assertFormatExpr("<<1/float,<<22,33>>/binary>>", "<<1/float, <<22, 33>>/binary>>"),
    ?assertFormatExpr(
        "<<1/float,<<22,33>>>>",
        "<<\n"
        "    1/float,\n"
        "    <<22, 33>>\n"
        ">>",
        15
    ),
    ?assertFormatExpr(
        "<<1/float,<<22,33>>>>",
        "<<\n"
        "    1/float,\n"
        "    <<\n"
        "        22,\n"
        "        33\n"
        "    >>\n"
        ">>",
        10
    ).

map_create(Config) when is_list(Config) ->
    ?assertFormatExpr("#{\n}", "#{}"),
    ?assertFormatExpr("#{1:=2,  3=>4}", "#{1 := 2, 3 => 4}"),
    ?assertFormatExpr(
        "#{11 => 22, 33 => 44}",
        "#{\n"
        "    11 => 22,\n"
        "    33 => 44\n"
        "}",
        15
    ),
    ?assertFormatExpr(
        "#{11 => 22, 33 => 44}",
        "#{\n"
        "    11 =>\n"
        "        22,\n"
        "    33 =>\n"
        "        44\n"
        "}",
        10
    ).

map_update(Config) when is_list(Config) ->
    ?assertFormatExpr("X # {\n}", "X#{}"),
    ?assertSameExpr("#{}#{}"),
    ?assertSameExpr("#{}#{}#{}"),
    ?assertSameExpr("(X#foo.bar)#{}"),
    ?assertSameExpr("(catch 1)#{}"),
    ?assertSameExpr("X#{A => B, C := D}"),
    ?assertFormatExpr(
        "X#{11 => 22, 33 => 44}",
        "X#{\n"
        "    11 => 22,\n"
        "    33 => 44\n"
        "}",
        15
    ),
    ?assertFormatExpr(
        "#{55 => 66, 77 => 88}#{11 => 22, 33 => 44}",
        "#{\n"
        "    55 => 66,\n"
        "    77 => 88\n"
        "}#{\n"
        "    11 => 22,\n"
        "    33 => 44\n"
        "}",
        15
    ).

record_create(Config) when is_list(Config) ->
    ?assertFormatExpr("#foo{\n}", "#foo{}"),
    ?assertFormatExpr("#foo{_=x}", "#foo{_ = x}"),
    ?assertFormatExpr("#foo{a=1,b=2+3}", "#foo{a = 1, b = 2 + 3}"),
    ?assertFormatExpr(
        "#foo{a=1,b=2+3}",
        "#foo{\n"
        "    a = 1,\n"
        "    b = 2 + 3\n"
        "}",
        10
    ),
    ?assertFormatExpr(
        "#foo{a=1,b=Foo+Bar}",
        "#foo{\n"
        "    a = 1,\n"
        "    b =\n"
        "        Foo +\n"
        "            Bar\n"
        "}",
        10
    ),
    ?assertSameExpr("#?FOO{}"),
    ?assertSameExpr("?FOO{}").

record_update(Config) when is_list(Config) ->
    ?assertFormatExpr("X #foo {\n}", "X#foo{}"),
    ?assertSameExpr("#foo{}#bar{}"),
    ?assertSameExpr("#foo{}#bar{}#baz{}"),
    ?assertSameExpr("X#foo.bar#baz{}"),
    ?assertSameExpr("(catch 1)#foo{}"),
    ?assertFormatExpr(
        "X#foo{aa = aa, bb = bb}",
        "X#foo{\n"
        "    aa = aa,\n"
        "    bb = bb\n"
        "}",
        15
    ),
    ?assertFormatExpr(
        "#foo{cc = cc, dd = dd}#foo{aa = aa, bb = bb}",
        "#foo{\n"
        "    cc = cc,\n"
        "    dd = dd\n"
        "}#foo{\n"
        "    aa = aa,\n"
        "    bb = bb\n"
        "}",
        15
    ),
    ?assertSameExpr("X#?FOO{}"),
    ?assertSameExpr("X?FOO{}").

record_index(Config) when is_list(Config) ->
    ?assertSameExpr("#foo.bar"),
    ?assertSameExpr("#?FOO.bar"),
    ?assertSameExpr("?FOO.bar").

record_field(Config) when is_list(Config) ->
    ?assertSameExpr("X#foo.bar"),
    ?assertSameExpr("X#foo.bar#baz.bak"),
    ?assertSameExpr("(catch X)#foo.bar"),
    ?assertFormatExpr(
        "(Foo + Bar)#foo.bar",
        "(Foo +\n"
        "    Bar)#foo.bar",
        5
    ),
    ?assertSameExpr("X#?FOO.bar"),
    ?assertSameExpr("X?FOO.bar").

force_break(Config) when is_list(Config) ->
    ?assertSameExpr(
        "[\n"
        "    %% foo\n"
        "]"
    ),
    ?assertFormatExpr(
        "[x\n"
        "]",
        "[x]"
    ),
    ?assertFormatExpr(
        "[\n"
        " x]",
        "[\n"
        "    x\n"
        "]"
    ),
    ?assertFormatExpr(
        "{x\n"
        "}",
        "{x}"
    ),
    ?assertFormatExpr(
        "{\n"
        " x}",
        "{\n"
        "    x\n"
        "}"
    ),
    ?assertFormatExpr(
        "<<x\n"
        ">>",
        "<<x>>"
    ),
    ?assertFormatExpr(
        "<<\n"
        " x>>",
        "<<\n"
        "    x\n"
        ">>"
    ),
    ?assertFormatExpr(
        "#{x => y\n"
        "}",
        "#{x => y}"
    ),
    ?assertFormatExpr(
        "X#{\n"
        " x => y}",
        "X#{\n"
        "    x => y\n"
        "}"
    ),
    ?assertFormatExpr(
        "X#{x => y\n"
        "}",
        "X#{x => y}"
    ),
    ?assertFormatExpr(
        "X#{\n"
        " x => y}",
        "X#{\n"
        "    x => y\n"
        "}"
    ),
    ?assertFormatExpr(
        "#foo{x = 1\n"
        "}",
        "#foo{x = 1}"
    ),
    ?assertFormatExpr(
        "#foo{\n"
        " x = 1}",
        "#foo{\n"
        "    x = 1\n"
        "}"
    ),
    ?assertFormatExpr(
        "X#foo{x = 1\n"
        "}",
        "X#foo{x = 1}"
    ),
    ?assertFormatExpr(
        "X#foo{\n"
        " x = 1}",
        "X#foo{\n"
        "    x = 1\n"
        "}"
    ),
    ?assertFormatExpr(
        "foo(x\n"
        ")",
        "foo(x)"
    ),
    ?assertFormatExpr(
        "foo(\n"
        " x)",
        "foo(\n"
        "    x\n"
        ")"
    ),
    ?assertFormatForm(
        "foo(x\n"
        ") -> x.",
        "foo(x) -> x."
    ),
    ?assertFormatForm(
        "foo(\n"
        " x) -> x.",
        "foo(\n"
        "    x\n"
        ") ->\n"
        "    x."
    ),
    ?assertFormatForm(
        "foo(1) -> x;\n"
        "foo(2) ->\n"
        "    y.",
        "foo(1) -> x;\n"
        "foo(2) -> y."
    ),
    ?assertFormatForm(
        "foo(1) ->\n"
        "    x;\n"
        "foo(2) -> y.",
        "foo(1) ->\n"
        "    x;\n"
        "foo(2) ->\n"
        "    y."
    ).

list_comprehension(Config) when is_list(Config) ->
    ?assertFormatExpr("[X||X<-List]", "[X || X <- List]"),
    ?assertSameExpr("[X || {X, Y} <- Results, X >= Y]"),
    ?assertSameExpr("[X || <<X, Y>> <= Results, X >= Y]"),
    ?assertFormatExpr(
        "[{Very, Long, Expression} || X <- Y, X < 10]",
        "[\n"
        "    {\n"
        "        Very,\n"
        "        Long,\n"
        "        Expression\n"
        "    } || X <- Y, X < 10\n"
        "]",
        25
    ),
    ?assertFormatExpr(
        "[X || X <- LongExpr, X < 10]",
        "[\n"
        "    X\n"
        "    || X <- LongExpr,\n"
        "       X < 10\n"
        "]",
        25
    ),
    ?assertFormatExpr(
        "[X || X <- VeryLongExpression, X < 10]",
        "[\n"
        "    X\n"
        "    || X <-\n"
        "           VeryLongExpression,\n"
        "       X < 10\n"
        "]",
        25
    ).

binary_comprehension(Config) when is_list(Config) ->
    ?assertFormatExpr("<<X||X<-List>>", "<<X || X <- List>>"),
    ?assertSameExpr("<<X || <<X, Y>> <= Results, X >= Y>>"),
    ?assertFormatExpr(
        "<<(Long + Expression) || X <- Y, X < 10>>",
        "<<\n"
        "    (Long + Expression)\n"
        "    || X <- Y, X < 10\n"
        ">>",
        25
    ),
    ?assertFormatExpr(
        "<<X || <<X>> <= LongExpr, X < 10>>",
        "<<\n"
        "    X\n"
        "    || <<X>> <= LongExpr,\n"
        "       X < 10\n"
        ">>",
        25
    ),
    ?assertFormatExpr(
        "<<X || <<X>> <= VeryLongExpression, X < 10>>",
        "<<\n"
        "    X\n"
        "    || <<X>> <=\n"
        "           VeryLongExpression,\n"
        "       X < 10\n"
        ">>",
        25
    ).

call(Config) when is_list(Config) ->
    ?assertFormatExpr("foo(\n)", "foo()"),
    ?assertSameExpr("foo(1, 2, 3)"),
    ?assertSameExpr("foo:bar(1, 2, 3)"),
    ?assertSameExpr("Foo:Bar(1, 2, 3)"),
    ?assertSameExpr("(get_module()):(get_fun())()"),
    ?assertFormatExpr(
        "long_name({Long, Expression})",
        "long_name(\n"
        "    {Long, Expression}\n"
        ")",
        25
    ),
    ?assertFormatExpr(
        "very_very_long_name({Very, Long, Expression})",
        "very_very_long_name(\n"
        "    {\n"
        "        Very,\n"
        "        Long,\n"
        "        Expression\n"
        "    }\n"
        ")",
        20
    ),
    ?assertFormatExpr(
        "long_name({Long, Expression}, AnotherArgument)",
        "long_name(\n"
        "    {Long, Expression},\n"
        "    AnotherArgument\n"
        ")",
        25
    ),
    ?assertFormatExpr(
        "long_name(Arg, {Very, Long, Expression})",
        "long_name(Arg, {\n"
        "    Very,\n"
        "    Long,\n"
        "    Expression\n"
        "})",
        25
    ),
    ?assertFormatExpr(
        "foo(bar(\n"
        "    1\n"
        "))",
        "foo(\n"
        "    bar(\n"
        "        1\n"
        "    )\n"
        ")"
    ).

block(Config) when is_list(Config) ->
    ?assertFormatExpr(
        "begin 1 end",
        "begin\n"
        "    1\n"
        "end"
    ),
    ?assertFormatExpr(
        "begin long_expression(with_args), {Short, Expr} end",
        "begin\n"
        "    long_expression(\n"
        "        with_args\n"
        "    ),\n"
        "    {Short, Expr}\n"
        "end",
        25
    ),
    ?assertSameExpr(
        "begin\n"
        "    foo(),\n"
        "\n"
        "    bar()\n"
        "end"
    ).

fun_expression(Config) when is_list(Config) ->
    ?assertSameExpr("fun foo/1"),
    ?assertSameExpr("fun Mod:Name/Arity"),
    ?assertSameExpr("fun () -> ok end"),
    ?assertSameExpr("fun (X) when is_integer(X) -> X end"),
    ?assertSameExpr("fun Foo() -> Foo() end"),
    ?assertFormatExpr(
        "fun (x) -> x; (y) -> y end",
        "fun\n"
        "    (x) -> x;\n"
        "    (y) -> y\n"
        "end",
        100
    ),
    ?assertFormatExpr(
        "fun (Long) -> Expression end",
        "fun\n"
        "    (Long) -> Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "fun (Even, Longer) -> Expression end",
        "fun\n"
        "    (Even, Longer) ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "fun (Even, Longer) when Guarded -> Expression end",
        "fun\n"
        "    (Even, Longer)\n"
        "            when Guarded ->\n"
        "        Expression\n"
        "end",
        30
    ),
    ?assertFormatExpr(
        "fun (The, Longest, Pattern) when Guarded -> Expression end",
        "fun\n"
        "    (\n"
        "        The,\n"
        "        Longest,\n"
        "        Pattern\n"
        "    ) when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "fun (Long, Pattern) when Guard; Is, Long -> Expression end",
        "fun\n"
        "    (Long, Pattern)\n"
        "            when Guard;\n"
        "                 Is, Long ->\n"
        "        Expression\n"
        "end",
        30
    ),
    ?assertFormatExpr(
        "fun (Long, Pattern) when Guard; Is, Even, Longer -> Expression end",
        "fun\n"
        "    (Long, Pattern)\n"
        "            when Guard;\n"
        "                 Is,\n"
        "                 Even,\n"
        "                 Longer ->\n"
        "        Expression\n"
        "end",
        30
    ).

case_expression(Config) when is_list(Config) ->
    ?assertFormatExpr(
        "case 1 of 1 -> ok end",
        "case 1 of\n"
        "    1 -> ok\n"
        "end",
        100
    ),
    ?assertFormatExpr(
        "case 1 of {Long} -> Expression end",
        "case 1 of\n"
        "    {Long} -> Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "case 1 of {Even, Longer} -> Expression end",
        "case 1 of\n"
        "    {Even, Longer} ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "case 1 of Long when Guarded -> Expression end",
        "case 1 of\n"
        "    Long when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "case 1 of {Even, Longer} when Guarded -> Expression end",
        "case 1 of\n"
        "    {Even, Longer}\n"
        "            when Guarded ->\n"
        "        Expression\n"
        "end",
        30
    ),
    ?assertFormatExpr(
        "case 1 of {The, Longest, Pattern} when Guarded -> Expression end",
        "case 1 of\n"
        "    {\n"
        "        The,\n"
        "        Longest,\n"
        "        Pattern\n"
        "    } when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "case 1 of {Long, Pattern} when Guard; Is, Long -> Expression end",
        "case 1 of\n"
        "    {Long, Pattern}\n"
        "            when Guard;\n"
        "                 Is, Long ->\n"
        "        Expression\n"
        "end",
        30
    ),
    ?assertFormatExpr(
        "case 1 of Short -> Expr; {Long, Pattern} -> Expression end",
        "case 1 of\n"
        "    Short ->\n"
        "        Expr;\n"
        "    {Long, Pattern} ->\n"
        "        Expression\n"
        "end",
        25
    ).

receive_expression(Config) when is_list(Config) ->
    ?assertFormatExpr(
        "receive 1 -> ok end",
        "receive\n"
        "    1 -> ok\n"
        "end",
        100
    ),
    ?assertSameExpr(
        "receive\n"
        "after 0 -> ok\n"
        "end"
    ),
    ?assertSameExpr(
        "receive\n"
        "after 0 ->\n"
        "    some:long(Expression)\n"
        "end",
        25
    ),
    ?assertSameExpr(
        "receive\n"
        "    1 -> ok\n"
        "after 0 -> ok\n"
        "end"
    ),
    ?assertFormatExpr(
        "receive {Long} -> Expression end",
        "receive\n"
        "    {Long} -> Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "receive {Even, Longer} -> Expression end",
        "receive\n"
        "    {Even, Longer} ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "receive Long when Guarded -> Expression end",
        "receive\n"
        "    Long when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "receive {Even, Longer} when Guarded -> Expression end",
        "receive\n"
        "    {Even, Longer}\n"
        "            when Guarded ->\n"
        "        Expression\n"
        "end",
        30
    ),
    ?assertFormatExpr(
        "receive {The, Longest, Pattern} when Guarded -> Expression end",
        "receive\n"
        "    {\n"
        "        The,\n"
        "        Longest,\n"
        "        Pattern\n"
        "    } when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "receive {Long, Pattern} when Guard; Is, Long -> Expression end",
        "receive\n"
        "    {Long, Pattern}\n"
        "            when Guard;\n"
        "                 Is, Long ->\n"
        "        Expression\n"
        "end",
        30
    ).

try_expression(Config) when is_list(Config) ->
    ?assertFormatExpr(
        "try ok after Expr end",
        "try ok\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormatExpr(
        "try ok after Expr1, Expr2 end",
        "try ok\n"
        "after\n"
        "    Expr1,\n"
        "    Expr2\n"
        "end"
    ),
    ?assertFormatExpr(
        "try Expr1, Expr2 after Expr end",
        "try\n"
        "    Expr1,\n"
        "    Expr2\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormatExpr(
        "try ok catch _ -> throw end",
        "try ok\n"
        "catch\n"
        "    _ -> throw\n"
        "end"
    ),
    ?assertFormatExpr(
        "try ok catch throw:_ -> throw; error:_ -> error; exit:_ -> exit end",
        "try ok\n"
        "catch\n"
        "    throw:_ -> throw;\n"
        "    error:_ -> error;\n"
        "    exit:_ -> exit\n"
        "end"
    ),
    ?assertFormatExpr(
        "try ok catch error:Reason:Stack -> {error, {Reason, Stack}} end",
        "try ok\n"
        "catch\n"
        "    error:Reason:Stack ->\n"
        "        {error, {Reason, Stack}}\n"
        "end",
        35
    ),
    ?assertFormatExpr(
        "try Expr of _ -> ok after Expr end",
        "try Expr of\n"
        "    _ -> ok\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormatExpr(
        "try Expr of _ -> ok after Expr end",
        "try Expr of\n"
        "    _ -> ok\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormatExpr(
        "try Expr1, Expr2 of _ -> ok after Expr end",
        "try\n"
        "    Expr1,\n"
        "    Expr2\n"
        "of\n"
        "    _ -> ok\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormatExpr(
        "try Expr1, Expr2 of _ -> Expr1, Expr2 catch _ -> Expr1, Expr2 after Expr1, Expr2 end",
        "try\n"
        "    Expr1,\n"
        "    Expr2\n"
        "of\n"
        "    _ ->\n"
        "        Expr1,\n"
        "        Expr2\n"
        "catch\n"
        "    _ ->\n"
        "        Expr1,\n"
        "        Expr2\n"
        "after\n"
        "    Expr1,\n"
        "    Expr2\n"
        "end"
    ).

if_expression(Config) when is_list(Config) ->
    ?assertFormatExpr(
        "if true -> ok end",
        "if\n"
        "    true -> ok\n"
        "end",
        100
    ),
    ?assertFormatExpr(
        "if long() -> Expression end",
        "if\n"
        "    long() -> Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "if even(Longer) -> Expression end",
        "if\n"
        "    even(Longer) ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "if the(Guard); Is, Long -> Expression end",
        "if\n"
        "    the(Guard);\n"
        "    Is, Long ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "if Short -> Expr; long(Guard, And) -> Expression end",
        "if\n"
        "    Short ->\n"
        "        Expr;\n"
        "    long(Guard, And) ->\n"
        "        Expression\n"
        "end",
        25
    ).

macro(Config) when is_list(Config) ->
    ?assertSameExpr("??FOO"),
    ?assertFormatExpr("? Foo", "?Foo"),
    ?assertSameExpr("?foo"),
    ?assertFormatExpr("?FOO(\n)", "?FOO()"),
    ?assertSameExpr("?assertMatch(X when is_integer(X), Y)"),
    ?assertFormatExpr(
        "?assertMatch(X when is_integer(X), Y)",
        "?assertMatch(\n"
        "    X when is_integer(X),\n"
        "    Y\n"
        ")",
        30
    ),
    ?assertFormatExpr(
        "?assertMatch(X when is_integer(X), Y)",
        "?assertMatch(\n"
        "    X when\n"
        "        is_integer(X),\n"
        "    Y\n"
        ")",
        23
    ).

function(Config) when is_list(Config) ->
    ?assertSameForm("f() -> ok."),
    ?assertSameForm(
        "f(1) -> one;\n"
        "f(2) -> two."
    ),
    ?assertFormatForm(
        "f(1) -> one; f(2) -> begin two end.",
        "f(1) ->\n"
        "    one;\n"
        "f(2) ->\n"
        "    begin\n"
        "        two\n"
        "    end."
    ),
    ?assertSameForm(
        "?FOO(1) -> ok;\n"
        "?DEFAULT(?FOO)."
    ).

attribute(Config) when is_list(Config) ->
    ?assertFormatForm("-else .", "-else."),
    ?assertFormatForm("- foo (\n1).", "-foo(1)."),
    ?assertSameForm("-compile([export_all, nowarn_export_all])."),
    ?assertSameForm("-import(foo, [bar/2, baz/0])."),
    ?assertSameForm("-export([bar/2, baz/3])."),
    ?assertFormatForm(
        "-attribute([Long, Value]).",
        "-attribute(\n"
        "    [Long, Value]\n"
        ").",
        25
    ),
    ?assertFormatForm(
        "-attribute([ExceptionallyLong, Value]).",
        "-attribute([\n"
        "    ExceptionallyLong,\n"
        "    Value\n"
        "]).",
        25
    ),
    ?assertFormatForm(
        "-attribute(\n"
        "           [ ExceptionallyLong % comment\n"
        "           , Value]).",
        "-attribute(\n"
        "    % comment\n"
        "    [\n"
        "        ExceptionallyLong,\n"
        "        Value\n"
        "    ]\n"
        ").",
        25
    ),
    ?assertFormatForm(
        "-record(foo, {a = 1 :: integer(), b :: float(), c  = 2, d}).",
        "-record(\n"
        "    foo,\n"
        "    {a = 1 :: integer(), b :: float(), c = 2, d}\n"
        ").",
        50
    ),
    ?assertFormatForm(
        "-record(foo, {a = 1 :: integer(), b :: float(), c  = 2, d}).",
        "-record(foo, {\n"
        "    a = 1 :: integer(),\n"
        "    b :: float(),\n"
        "    c = 2,\n"
        "    d\n"
        "}).",
        30
    ),
    ?assertSameForm(
        "-opaque foo() :: #foo{a :: integer(), b :: module:type()}."
    ),
    ?assertFormatForm(
        "-type foobar() :: #foo{a :: integer(), b :: mod:type()}.",
        "-type foobar() ::\n"
        "    #foo{a :: integer(), b :: mod:type()}.",
        50
    ),
    ?assertSameForm(
        "-type foo() ::\n"
        "    fun((A, B, C) -> return_type(A, B, C)).",
        50
    ),
    ?assertSameForm(
        "-type foo() :: #{\n"
        "    a := integer(),\n"
        "    b => float()\n"
        "}."
    ),
    ?assertSameForm(
        "-spec foo(Int) -> atom() when Int :: integer()."
    ),
    ?assertFormatForm(
        "-spec foo(Int) -> atom() when Int :: integer().",
        "-spec foo(Int) -> atom()\n"
        "    when Int :: integer().",
        40
    ),
    ?assertFormatForm(
        "-spec foo(Int) -> some_very:very(long, type) when Int :: integer().",
        "-spec foo(Int) ->\n"
        "    some_very:very(long, type)\n"
        "    when Int :: integer().",
        40
    ),
    ?assertSameForm(
        "-callback long_name(Bar) -> #{map := type([Bar, ...])}."
    ),
    ?assertFormatForm(
        "-callback long_name(Bar) -> #{map := type([Bar, ...])}.",
        "-callback long_name(Bar) ->\n"
        "    #{map := type([Bar, ...])}.",
        50
    ),
    ?assertFormatForm(
        "-spec foo(integer()) -> some_very:very(long, type); (atom()) -> atom().",
        "-spec foo\n"
        "    (integer()) -> some_very:very(long, type);\n"
        "    (atom()) -> atom()."
    ),
    ?assertFormatForm(
        "-spec foo(integer()) -> some_very:very(long, type); (1..2) -> atom().",
        "-spec foo\n"
        "    (integer()) ->\n"
        "        some_very:very(long, type);\n"
        "    (1..2) ->\n"
        "        atom().",
        40
    ),
    ?assertFormatForm(
        "-spec foo(Int) -> some_very_very:very(long, type) when Int :: integer(); (1..2) -> atom().",
        "-spec foo\n"
        "    (Int) ->\n"
        "        some_very_very:very(long, type)\n"
        "        when Int :: integer();\n"
        "    (1..2) ->\n"
        "        atom().",
        40
    ),
    ?assertSameForm(
        "-opaque foo() :: {<<>>, <<_:8>>, <<_:_*4>>, <<_:8, _:_*4>>}."
    ),
    ?assertSameForm(
        "-define(IN_RANGE(Value, Low, High), Value >= Low andalso Value =< High)."
    ),
    ?assertFormatForm(
        "-define(OUT_OF_RANGE(Value, Low, High), (Value) =< long_expression(Low), Value >= long_expression(High)).",
        "-define(OUT_OF_RANGE(Value, Low, High),\n"
        "    (Value) =< long_expression(Low),\n"
        "    Value >= long_expression(High)\n"
        ").",
        40
    ),
    ?assertSameForm(
        "-define(FOO(X), begin\n"
        "    is_atom(X) orelse is_tuple(X)\n"
        "end)."
    ),
    ?assertSameForm(
        "-define(FOO(X), [1, 2, 3])."
    ),
    ?assertSameForm(
        "-define(FOO(X), [\n"
        "    1,\n"
        "    2,\n"
        "    3\n"
        "])."
    ),
    ?assertSameForm(
        "-type foo() :: {fun(), fun((...) -> mod:bar()), fun(() -> integer())}."
    ).

comment(Config) when is_list(Config) ->
    ?assertSameExpr(
        "%foo\n"
        "1 + 2"
    ),
    ?assertFormatExpr(
        "1 +\n"
        "%% foo\n"
        "2",
        "1 +\n"
        "    %% foo\n"
        "    2"
    ).

format_form(String, PageWidth) ->
    {ok, [Form], []} = erlfmt:read_forms_string("nofile", String),
    Doc = erlfmt_format:form_to_algebra(Form),
    Rendered = erlfmt_algebra:document_render(Doc, [{page_width, PageWidth}]),
    unicode:characters_to_list(Rendered).

format_expr(String, PageWidth) ->
    {ok, [{function, _, [{clause, _, _, empty, [Expr]}]}], []} =
        erlfmt:read_forms_string("nofile", "f() ->\n" ++ String ++ "."),
    Doc = erlfmt_format:expr_to_algebra(Expr),
    Rendered = erlfmt_algebra:document_render(Doc, [{page_width, PageWidth}]),
    unicode:characters_to_list(Rendered).
