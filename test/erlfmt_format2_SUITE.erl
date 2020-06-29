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
-module(erlfmt_format2_SUITE).

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
    record_definition/1,
    spec/1,
    define/1,
    type/1,
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
            % {group, comprehensions},
            call,
            % block,
            % fun_expression,
            % case_expression,
            % receive_expression,
            % try_expression,
            % if_expression,
            macro
        ]},
        % {forms, [parallel], [
        %     % function,
        %     % attribute,
        %     % spec,
        %     % record_definition,
        %     % define,
        %     % type
        % ]},
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
        ]}
        % {comprehensions, [parallel], [
        %     % list_comprehension,
        %     % binary_comprehension
        % ]}
    ].

all() ->
    [
        {group, expressions},
        % {group, forms}
        comment
    ].

%%--------------------------------------------------------------------
%% TEST CASES
-define(assertSame(String), ?assertSame(String, 80)).

-define(assertSame(String, PageWidth),
    ?assertEqual(String, format(String, PageWidth))
).

-define(assertFormat(Bad, Good), ?assertFormat(Bad, Good, 80)).

-define(assertFormat(Bad, Good, PageWidth), begin
    ?assertEqual(Good, format(Good, PageWidth)),
    ?assertEqual(Good, format(Bad, PageWidth))
end).

literals(Config) when is_list(Config) ->
    ?assertSame("100"),
    ?assertSame("007"),
    ?assertSame("10#1000"),
    ?assertSame("2#101"),
    ?assertSame("16#01"),
    ?assertSame("16#deadBEEF"),
    ?assertSame("001.100"),
    ?assertSame("1.0e-1"),
    ?assertSame("001.100e-010"),
    ?assertSame("1.0E01"),
    ?assertSame("$a"),
    ?assertSame("$\x{1F605}"),
    ?assertSame("$\\0"),
    ?assertSame("$\\\\"),
    ?assertSame("$\\t"),
    ?assertSame("$\\z"),
    ?assertSame("$\\040"),
    ?assertSame("$\\xAb"),
    ?assertSame("$\\x{Ab}"),
    ?assertSame("'foo'"),
    ?assertSame("foo"),
    ?assertSame("'andalso'"),
    ?assertSame("'foo bar'"),
    ?assertSame("'foo\\tbar'"),
    ?assertSame("'\\''"),
    ?assertSame("'foo\\xaB'"),
    ?assertSame("'foo\\x{aB}'"),
    ?assertSame("'foo\\z'"),
    ?assertSame("'foo\\s'"),
    ?assertSame("\"'\""),
    ?assertSame("\"\x{1F631}\""),
    ?assertSame("\" \\40\\x32\\x{0032}\""),
    ?assertSame("\"The quick brown fox jumps over the lazy dog\""),
    ?assertSame("\"\\s \""),
    ?assertSame("Foo"),
    ?assertSame("_Bar"),
    ?assertFormat("$ ", "$\\s").

string_concat(Config) when is_list(Config) ->
    ?assertSame("\"foo\" \"bar\""),
    ?assertFormat(
        "\"foo\" \"bar\"",
        "\"foo\"\n"
        "\"bar\"",
        5
    ),

    %% Measures text size in graphemes (\x{61}\x{301} is á in NFD normalisation)
    ?assertSame("\"\x{61}\x{301}\" \"á\"", 7),
    ?assertFormat(
        "\"\x{61}\x{301}\" \"á\"",
        "\"\x{61}\x{301}\"\n"
        "\"á\"",
        6
    ),

    ?assertSame("\"foo\" Foo \"bar\""),

    %% Multiline strings are converted to sequence of concats
    ?assertFormat(
        "\"foo\nbar\"",
        "\"foo\\n\"\n"
        "\"bar\""
    ),
    ?assertFormat(
        "\"foo\nbar\n\"",
        "\"foo\\n\"\n"
        "\"bar\\n\""
    ),
    ?assertFormat(
        "\"foo\n\"",
        "\"foo\\n\""
    ).

unary_operator(Config) when is_list(Config) ->
    %% Formats symbolic operators without space
    ?assertFormat("+ 1", "+1"),
    ?assertFormat("- 1", "-1"),

    % %% Formats word operators with space
    ?assertSame("bnot 1"),
    ?assertSame("not true"),
    ?assertSame("catch 1"),

    % %% Does not change parenthesis, only whitespace
    ?assertFormat("bnot+1", "bnot +1"),
    ?assertSame("+ +1"),
    ?assertSame("+(+1)"),
    ?assertSame("+ -1"),
    ?assertSame("-(+1)"),
    ?assertSame("not catch 1"),
    ?assertFormat("not(1 + 1)", "not (1 + 1)"),
    ?assertFormat("(bnot 1)*1", "(bnot 1) * 1"),
    ?assertSame("(catch 1) + 1"),

    % %% Unless it's nested not or bnot
    ?assertSame("bnot bnot Var"),
    ?assertSame("not not true").

binary_operator(Config) when is_list(Config) ->
    %% No changes to parens
    ?assertSame("CRC bxor Byte band 16#ff"),
    ?assertSame("(CRC bsl 8) bxor Byte"),
    ?assertSame("Foo ++ Bar ++ Baz -- Bat"),
    ?assertSame("Foo ++ Bar ++ (Baz -- Bat)"),
    ?assertSame("Foo ++ (Bar ++ Baz) -- Bat"),
    ?assertSame("Foo > Bar andalso Baz =:= Bat"),
    ?assertSame("Foo and Bar or (Baz and Bat)"),

    %% Nested same operator right-associative
    ?assertSame("Foo ++ Bar ++ Baz"),
    ?assertFormat(
        "Foo ++ Bar ++ Baz",
        "Foo ++\n"
        "    Bar ++ Baz",
        15
    ),
    ?assertFormat(
        "Foo ++ Bar ++ Baz",
        "Foo ++\n"
        "    Bar ++\n"
        "    Baz",
        5
    ),
    ?assertFormat(
        "(Foo ++ Bar) ++ Baz",
        "(Foo ++ Bar) ++\n"
        "    Baz",
        15
    ),
    ?assertFormat(
        "(Foo ++ Bar) ++ Baz",
        "(Foo ++\n"
        "    Bar) ++\n"
        "    Baz",
        5
    ),

    %% Nested same operator left-associative
    ?assertSame("Foo + Bar + Baz"),
    ?assertFormat(
        "Foo + Bar + Baz",
        "Foo + Bar +\n"
        "    Baz",
        14
    ),
    ?assertFormat(
        "Foo + Bar + Baz",
        "Foo +\n"
        "    Bar +\n"
        "    Baz",
        5
    ),
    ?assertFormat(
        "Foo + (Bar + Baz)",
        "Foo +\n"
        "    (Bar + Baz)",
        15
    ),
    ?assertFormat(
        "Foo + (Bar + Baz)",
        "Foo +\n"
        "    (Bar +\n"
        "        Baz)",
        5
    ),

    %% With precedence
    ?assertFormat("(A + B) == C + D", "(A + B) == C + D"),
    ?assertSame("A + (B == C) + D"),
    ?assertFormat(
        "A + B == (C + D)",
        "A + B ==\n"
        "    (C + D)",
        15
    ),
    ?assertFormat(
        "Foo * (B + C) * D",
        "Foo *\n"
        "    (B + C) *\n"
        "    D",
        12
    ),
    ?assertFormat(
        "A * (B + C) * D",
        "A * (B + C) *\n"
        "    D",
        12
    ),
    ?assertFormat(
        "One * (Two + Three + Four) * Five",
        "One *\n"
        "    (Two + Three +\n"
        "        Four) * Five",
        20
    ),

    %% Next break fits
    ?assertSame(
        "Foo = [\n"
        "    1\n"
        "]"
    ),
    ?assertSame(
        "Foo = foo(\n"
        "    1\n"
        ")"
    ),
    ?assertSame(
        "Foo = ?foo(\n"
        "    1\n"
        ")"
    ),
    % ?assertSame(
    %     "Foo = fun () ->\n"
    %     "    ok\n"
    %     "end",
    %     15
    % ),
    % ?assertSame(
    %     "foo() :: #{\n"
    %     "    a := integer()\n"
    %     "}"
    % ),

    %% Keeps existing breaks
    ?assertSame(
        "Foo andalso\n"
        "    Bar andalso\n"
        "    Baz"
    ).

tuple(Config) when is_list(Config) ->
    ?assertFormat("{ }", "{}"),
    ?assertFormat("{1,2}", "{1, 2}"),
    ?assertFormat("{1,{2,3}}", "{1, {2, 3}}"),
    ?assertFormat(
        "{verylong,{22,33,44}}",
        "{verylong,\n"
        "    {22, 33, 44}}",
        20
    ),
    ?assertFormat(
        "{long,x,{22,33,44},y}",
        "{long, x,\n"
        "    {22, 33, 44}, y}",
        20
    ),
    ?assertFormat(
        "{verylong,x,{22,33,44},y,{55,66,77},z}",
        "{verylong, x,\n"
        "    {22, 33, 44}, y,\n"
        "    {55, 66, 77}, z}",
        20
    ),
    ?assertFormat(
        "{{a,long,tuple},{nested,1,long,tuple}}",
        "{{a, long, tuple},\n"
        "    {nested, 1,\n"
        "        long,\n"
        "        tuple}}",
        20
    ),
    ?assertFormat(
        "{{a,long,tuple},[nested,1,long,list]}",
        "{{a, long, tuple},\n"
        "    [\n"
        "        nested,\n"
        "        1,\n"
        "        long,\n"
        "        list\n"
        "    ]}",
        20
    ),
    ?assertFormat(
        "{{a,long,tuple},{another,tuple},[nested,1,long,list]}",
        "{{a, long, tuple},\n"
        "    {another, tuple},\n"
        "    [\n"
        "        nested,\n"
        "        1,\n"
        "        long,\n"
        "        list\n"
        "    ]}",
        25
    ),
    ?assertFormat(
        "{{a,long,tuple},bare,atoms,[nested,long,list],bare,atoms,this_does_not_fit}",
        "{{a, long, tuple},\n"
        "    bare, atoms,\n"
        "    [\n"
        "        nested,\n"
        "        long,\n"
        "        list\n"
        "    ],\n"
        "    bare, atoms,\n"
        "    this_does_not_fit}",
        20
    ),
    ?assertFormat(
        "{short, {a,long,tuple}}",
        "{short,\n"
        "    {a, long,\n"
        "        tuple}}",
        15
    ).

list(Config) when is_list(Config) ->
    ?assertFormat("[\n]", "[]"),
    ?assertSame("[1 | [2 | []]]"),
    ?assertFormat("[ 1 ,2,3, 4]", "[1, 2, 3, 4]"),
    ?assertFormat("[1,2,3|4]", "[1, 2, 3 | 4]"),
    ?assertFormat(
        "[long,[2,3,4]]",
        "[\n"
        "    long,\n"
        "    [2, 3, 4]\n"
        "]",
        15
    ),
    ?assertFormat(
        "[\n"
        "long,[2,3,4]]",
        "[\n"
        "    long,\n"
        "    [2, 3, 4]\n"
        "]",
        15
    ),
    ?assertFormat(
        "[11,2|3]",
        "[\n"
        "    11,\n"
        "    2\n"
        "    | 3\n"
        "]",
        10
    ),
    ?assertFormat(
        "[11,22|33]",
        "[\n"
        "    11,\n"
        "    22\n"
        "    | 33\n"
        "]",
        10
    ),
    ?assertFormat(
        "[short, [long, word]]",
        "[\n"
        "    short,\n"
        "    [\n"
        "        long,\n"
        "        word\n"
        "    ]\n"
        "]",
        15
    ).

binary(Config) when is_list(Config) ->
    ?assertFormat("<< >>", "<<>>"),
    ?assertSame(
        "<<(1 + 1), (#{}), (#foo{}), (#{}#{}), (#foo{}#foo{}), (#foo.bar), (call())>>"
    ),
    ?assertSame("<<(1), 1>>"),
    ?assertSame("<<+1:5/integer-unit:8>>"),
    ?assertSame("<<\"żółć\"/utf8>>"),
    ?assertFormat("<<1/float,<<22,33>>/binary>>", "<<1/float, <<22, 33>>/binary>>"),
    ?assertFormat(
        "<<1/float,<<222,333>>>>",
        "<<1/float,\n"
        "    <<222,\n"
        "        333>>>>",
        15
    ),
    ?assertSame(
        "<<\n"
        "    1/float,\n"
        "    <<222, 333>>\n"
        ">>"
    ),
    ?assertSame(
        "<<\n"
        "    1/float,\n"
        "    <<\n"
        "        222,\n"
        "        333\n"
        "    >>\n"
        ">>"
    ).

map_create(Config) when is_list(Config) ->
    ?assertFormat("#{\n}", "#{}"),
    ?assertFormat("#{1:=2,  3=>4}", "#{1 := 2, 3 => 4}"),
    ?assertFormat(
        "#{11 => 22, 33 => 44}",
        "#{\n"
        "    11 => 22,\n"
        "    33 => 44\n"
        "}",
        15
    ),
    ?assertFormat(
        "#{11 => 22, 33 => 44}",
        "#{\n"
        "    11 =>\n"
        "        22,\n"
        "    33 =>\n"
        "        44\n"
        "}",
        10
    ),
    ?assertSame(
        "#{\n"
        "    1 => [\n"
        "        2\n"
        "    ],\n"
        "    3 => 4\n"
        "}"
    ).

map_update(Config) when is_list(Config) ->
    ?assertFormat("X # {\n}", "X#{}"),
    ?assertSame("#{}#{}"),
    ?assertSame("#{}#{}#{}"),
    ?assertSame("(X#foo.bar)#{}"),
    ?assertSame("(catch 1)#{}"),
    ?assertSame("X#{A => B, C := D}"),
    ?assertFormat(
        "X#{11 => 22, 33 => 44}",
        "X#{\n"
        "    11 => 22,\n"
        "    33 => 44\n"
        "}",
        15
    ),
    ?assertFormat(
        "#{55 => 66, 77 => 88}#{11 => 22, 33 => 44}",
        "#{\n"
        "    55 => 66,\n"
        "    77 => 88\n"
        "}#{\n"
        "    11 => 22,\n"
        "    33 => 44\n"
        "}",
        15
    ),
    ?assertSame(
        "#{\n"
        "    a => [\n"
        "        b\n"
        "    ],\n"
        "    c := d(\n"
        "        e\n"
        "    )\n"
        "}"
    ).

record_create(Config) when is_list(Config) ->
    ?assertFormat("#foo{\n}", "#foo{}"),
    ?assertFormat("#foo{_=x}", "#foo{_ = x}"),
    ?assertFormat("#foo{a=1,b=2+3}", "#foo{a = 1, b = 2 + 3}"),
    ?assertFormat(
        "#foo{a=1,b=2+3}",
        "#foo{\n"
        "    a = 1,\n"
        "    b = 2 + 3\n"
        "}",
        15
    ),
    ?assertFormat(
        "#foo{a=1,b=Foo+Bar}",
        "#foo{\n"
        "    a = 1,\n"
        "    b =\n"
        "        Foo +\n"
        "            Bar\n"
        "}",
        15
    ),
    ?assertSame("#?FOO{}"),
    ?assertSame("?FOO{}"),
    ?assertSame(
        "#foo{\n"
        "    a = [\n"
        "        b\n"
        "    ]\n"
        "}"
    ),
    ?assertSame(
        "#foo{\n"
        "    long_key = [\n"
        "        long_value\n"
        "    ]\n"
        "}",
        20
    ).

record_update(Config) when is_list(Config) ->
    ?assertFormat("X #foo {\n}", "X#foo{}"),
    ?assertSame("#foo{}#bar{}"),
    ?assertSame("#foo{}#bar{}#baz{}"),
    ?assertSame("X#foo.bar#baz{}"),
    ?assertSame("(catch 1)#foo{}"),
    ?assertFormat(
        "X#foo{aa = aa, bb = bb}",
        "X#foo{\n"
        "    aa = aa,\n"
        "    bb = bb\n"
        "}",
        15
    ),
    ?assertFormat(
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
    ?assertSame("X#?FOO{}"),
    ?assertSame("X?FOO{}").

record_index(Config) when is_list(Config) ->
    ?assertSame("#foo.bar"),
    ?assertSame("#?FOO.bar"),
    ?assertSame("?FOO.bar").

record_field(Config) when is_list(Config) ->
    ?assertSame("X#foo.bar"),
    ?assertSame("X#foo.bar#baz.bak"),
    ?assertSame("(catch X)#foo.bar"),
    ?assertFormat(
        "(Foo + Bar)#foo.bar",
        "(Foo +\n"
        "    Bar)#foo.bar",
        5
    ),
    ?assertSame("X#?FOO.bar"),
    ?assertSame("X?FOO.bar").

force_break(Config) when is_list(Config) ->
    ?assertSame(
        "[\n"
        "    %% foo\n"
        "]"
    ),

    ?assertFormat(
        "[1, 2\n"
        "    %% foo\n"
        "]",
        "[\n"
        "    1,\n"
        "    2\n"
        "    %% foo\n"
        "]"
    ),
    ?assertFormat(
        "[x\n"
        "]",
        "[x]"
    ),
    ?assertFormat(
        "[\n"
        " x]",
        "[\n"
        "    x\n"
        "]"
    ),
    ?assertFormat(
        "{x\n"
        "}",
        "{x}"
    ),
    ?assertFormat(
        "{\n"
        " x}",
        "{\n"
        "    x\n"
        "}"
    ),
    ?assertFormat(
        "<<x\n"
        ">>",
        "<<x>>"
    ),
    ?assertFormat(
        "<<\n"
        " x>>",
        "<<\n"
        "    x\n"
        ">>"
    ),
    ?assertFormat(
        "#{x => y\n"
        "}",
        "#{x => y}"
    ),
    ?assertFormat(
        "X#{\n"
        " x => y}",
        "X#{\n"
        "    x => y\n"
        "}"
    ),
    ?assertFormat(
        "X#{x => y\n"
        "}",
        "X#{x => y}"
    ),
    ?assertFormat(
        "X#{\n"
        " x => y}",
        "X#{\n"
        "    x => y\n"
        "}"
    ),
    ?assertFormat(
        "#foo{x = 1\n"
        "}",
        "#foo{x = 1}"
    ),
    ?assertFormat(
        "#foo{\n"
        " x = 1}",
        "#foo{\n"
        "    x = 1\n"
        "}"
    ),
    ?assertFormat(
        "X#foo{x = 1\n"
        "}",
        "X#foo{x = 1}"
    ),
    ?assertFormat(
        "X#foo{\n"
        " x = 1}",
        "X#foo{\n"
        "    x = 1\n"
        "}"
    ),
    ?assertFormat(
        "foo(x\n"
        ")",
        "foo(x)"
    ),
    ?assertFormat(
        "foo(\n"
        " x)",
        "foo(\n"
        "    x\n"
        ")"
    ).
    % ?assertFormat(
    %     "foo(x\n"
    %     ") -> x.",
    %     "foo(x) -> x."
    % ),
    % ?assertFormat(
    %     "foo(\n"
    %     " x) -> x.",
    %     "foo(\n"
    %     "    x\n"
    %     ") ->\n"
    %     "    x."
    % ),
    % ?assertFormat(
    %     "foo(1) -> x;\n"
    %     "foo(2) ->\n"
    %     "    y.",
    %     "foo(1) -> x;\n"
    %     "foo(2) -> y."
    % ),
    % ?assertFormat(
    %     "foo(1) ->\n"
    %     "    x;\n"
    %     "foo(2) -> y.",
    %     "foo(1) ->\n"
    %     "    x;\n"
    %     "foo(2) ->\n"
    %     "    y."
    % ).

list_comprehension(Config) when is_list(Config) ->
    ?assertFormat("[X||X<-List]", "[X || X <- List]"),
    ?assertSame("[X || {X, Y} <- Results, X >= Y]"),
    ?assertSame("[X || <<X, Y>> <= Results, X >= Y]"),
    ?assertFormat(
        "[[Very, Long, Expression] || X <- Y, X < 10]",
        "[\n"
        "    [\n"
        "        Very,\n"
        "        Long,\n"
        "        Expression\n"
        "    ] || X <- Y, X < 10\n"
        "]",
        25
    ),
    ?assertFormat(
        "[{Very, Long, Expression} || X <- Y, X < 10]",
        "[\n"
        "    {Very, Long,\n"
        "        Expression}\n"
        "    || X <- Y, X < 10\n"
        "]",
        25
    ),
    ?assertFormat(
        "[X || X <- LongExpr, X < 10]",
        "[\n"
        "    X\n"
        "    || X <- LongExpr,\n"
        "       X < 10\n"
        "]",
        25
    ),
    ?assertFormat(
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
    ?assertFormat("<<X||X<-List>>", "<<X || X <- List>>"),
    ?assertSame("<<X || <<X, Y>> <= Results, X >= Y>>"),
    ?assertFormat(
        "<<(Long + Expression) || X <- Y, X < 10>>",
        "<<\n"
        "    (Long + Expression)\n"
        "    || X <- Y, X < 10\n"
        ">>",
        25
    ),
    ?assertFormat(
        "<<X || <<X>> <= LongExpr, X < 10>>",
        "<<\n"
        "    X\n"
        "    || <<X>> <= LongExpr,\n"
        "       X < 10\n"
        ">>",
        25
    ),
    ?assertFormat(
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
    ?assertFormat("foo(\n)", "foo()"),
    ?assertSame("foo(1, 2, 3)"),
    ?assertSame("foo:bar(1, 2, 3)"),
    ?assertSame("Foo:Bar(1, 2, 3)"),
    ?assertSame("(get_module()):(get_fun())()"),
    ?assertFormat(
        "long_name({Long, Expression})",
        "long_name(\n"
        "    {Long, Expression}\n"
        ")",
        25
    ),
    ?assertFormat(
        "very_very_long_name([Very, Long, Expression])",
        "very_very_long_name(\n"
        "    [\n"
        "        Very,\n"
        "        Long,\n"
        "        Expression\n"
        "    ]\n"
        ")",
        20
    ),
    ?assertFormat(
        "very_very_long_name({Very, Long, Expression})",
        "very_very_long_name(\n"
        "    {Very, Long,\n"
        "        Expression}\n"
        ")",
        20
    ),
    ?assertFormat(
        "long_name({Long, Expression}, AnotherArgument)",
        "long_name(\n"
        "    {Long, Expression},\n"
        "    AnotherArgument\n"
        ")",
        25
    ),
    ?assertFormat(
        "long_name(Arg, [Very, Long, Expression])",
        "long_name(Arg, [\n"
        "    Very,\n"
        "    Long,\n"
        "    Expression\n"
        "])",
        25
    ),
    ?assertFormat(
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
    ?assertFormat(
        "begin 1 end",
        "begin\n"
        "    1\n"
        "end"
    ),
    ?assertFormat(
        "begin long_expression(with_args), {Short, Expr} end",
        "begin\n"
        "    long_expression(\n"
        "        with_args\n"
        "    ),\n"
        "    {Short, Expr}\n"
        "end",
        25
    ),
    ?assertSame(
        "begin\n"
        "    foo(),\n"
        "\n"
        "    bar()\n"
        "end"
    ).

fun_expression(Config) when is_list(Config) ->
    ?assertSame("fun foo/1"),
    ?assertSame("fun Mod:Name/Arity"),
    ?assertSame("fun () -> ok end"),
    ?assertSame("fun (X) when is_integer(X) -> X end"),
    ?assertSame("fun Foo() -> Foo() end"),
    ?assertFormat(
        "fun()->\n"
        "ok end",
        "fun () ->\n"
        "    ok\n"
        "end"
    ),
    ?assertFormat(
        "fun (x) -> x; (y) -> y end",
        "fun\n"
        "    (x) -> x;\n"
        "    (y) -> y\n"
        "end",
        100
    ),
    ?assertFormat(
        "fun (Long) -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Long) -> Expression;\n"
        "    (ok) -> ok\n"
        "end",
        25
    ),
    ?assertFormat(
        "fun (Even, Longer) -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Even, Longer) ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end",
        25
    ),
    ?assertFormat(
        "fun (Even, Longer) when Guarded -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Even, Longer)\n"
        "            when Guarded ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end",
        30
    ),
    ?assertFormat(
        "fun (The, Longest, Pattern) when Guarded -> Expression; (ok) -> ok end",
        "fun\n"
        "    (\n"
        "        The,\n"
        "        Longest,\n"
        "        Pattern\n"
        "    ) when Guarded ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end",
        25
    ),
    ?assertFormat(
        "fun (Long, Pattern) when Guard; Is, Long -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Long, Pattern)\n"
        "            when Guard;\n"
        "                 Is, Long ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end",
        30
    ),
    ?assertFormat(
        "fun (Long, Pattern) when Guard; Is, Even, Longer -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Long, Pattern)\n"
        "            when Guard;\n"
        "                 Is,\n"
        "                 Even,\n"
        "                 Longer ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end",
        30
    ),
    ?assertFormat(
        "fun (Long) -> Expression end",
        "fun (Long) ->\n"
        "    Expression\n"
        "end",
        20
    ),
    ?assertFormat(
        "fun (Even, Longer) -> Expression end",
        "fun (Even, Longer) ->\n"
        "    Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "fun (Even, Longer) when Guarded -> Expression end",
        "fun (Even, Longer)\n"
        "        when Guarded ->\n"
        "    Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "fun (The, Longest, Pattern) when Guarded -> Expression end",
        "fun (\n"
        "    The,\n"
        "    Longest,\n"
        "    Pattern\n"
        ") when Guarded ->\n"
        "    Expression\n"
        "end",
        20
    ),
    ?assertFormat(
        "fun (Long, Pattern) when Guard; Is, Long -> Expression end",
        "fun (Long, Pattern)\n"
        "        when Guard;\n"
        "             Is, Long ->\n"
        "    Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "fun (Long, Pattern) when Guard; Is, Even, Longer -> Expression end",
        "fun (Long, Pattern)\n"
        "        when Guard;\n"
        "             Is,\n"
        "             Even,\n"
        "             Longer ->\n"
        "    Expression\n"
        "end",
        30
    ).

case_expression(Config) when is_list(Config) ->
    ?assertFormat(
        "case 1 of 1 -> ok end",
        "case 1 of\n"
        "    1 -> ok\n"
        "end",
        100
    ),
    ?assertFormat(
        "case 1 of {Long} -> Expression end",
        "case 1 of\n"
        "    {Long} -> Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "case 1 of {Even, Longer} -> Expression end",
        "case 1 of\n"
        "    {Even, Longer} ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "case 1 of Long when Guarded -> Expression end",
        "case 1 of\n"
        "    Long when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "case 1 of {Even, Longer} when Guarded -> Expression end",
        "case 1 of\n"
        "    {Even, Longer}\n"
        "            when Guarded ->\n"
        "        Expression\n"
        "end",
        30
    ),
    ?assertFormat(
        "case 1 of [The, Longest, Pattern] when Guarded -> Expression end",
        "case 1 of\n"
        "    [\n"
        "        The,\n"
        "        Longest,\n"
        "        Pattern\n"
        "    ] when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "case 1 of {Long, Pattern} when Guard; Is, Long -> Expression end",
        "case 1 of\n"
        "    {Long, Pattern}\n"
        "            when Guard;\n"
        "                 Is, Long ->\n"
        "        Expression\n"
        "end",
        30
    ),
    ?assertFormat(
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
    ?assertFormat(
        "receive 1 -> ok end",
        "receive\n"
        "    1 -> ok\n"
        "end",
        100
    ),
    ?assertSame(
        "receive\n"
        "after 0 -> ok\n"
        "end"
    ),
    ?assertSame(
        "receive\n"
        "after 0 ->\n"
        "    some:long(Expression)\n"
        "end",
        25
    ),
    ?assertSame(
        "receive\n"
        "    1 -> ok\n"
        "after 0 -> ok\n"
        "end"
    ),
    ?assertFormat(
        "receive {Long} -> Expression end",
        "receive\n"
        "    {Long} -> Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "receive {Even, Longer} -> Expression end",
        "receive\n"
        "    {Even, Longer} ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "receive Long when Guarded -> Expression end",
        "receive\n"
        "    Long when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "receive {Even, Longer} when Guarded -> Expression end",
        "receive\n"
        "    {Even, Longer}\n"
        "            when Guarded ->\n"
        "        Expression\n"
        "end",
        30
    ),
    ?assertFormat(
        "receive [The, Longest, Pattern] when Guarded -> Expression end",
        "receive\n"
        "    [\n"
        "        The,\n"
        "        Longest,\n"
        "        Pattern\n"
        "    ] when Guarded ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormat(
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
    ?assertFormat(
        "try ok after Expr end",
        "try ok\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormat(
        "try ok after Expr1, Expr2 end",
        "try ok\n"
        "after\n"
        "    Expr1,\n"
        "    Expr2\n"
        "end"
    ),
    ?assertFormat(
        "try Expr1, Expr2 after Expr end",
        "try\n"
        "    Expr1,\n"
        "    Expr2\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormat(
        "try ok catch _ -> throw end",
        "try ok\n"
        "catch\n"
        "    _ -> throw\n"
        "end"
    ),
    ?assertFormat(
        "try ok catch throw:_ -> throw; error:_ -> error; exit:_ -> exit end",
        "try ok\n"
        "catch\n"
        "    throw:_ -> throw;\n"
        "    error:_ -> error;\n"
        "    exit:_ -> exit\n"
        "end"
    ),
    ?assertFormat(
        "try ok catch error:Reason:Stack -> {error, {Reason, Stack}} end",
        "try ok\n"
        "catch\n"
        "    error:Reason:Stack ->\n"
        "        {error, {Reason, Stack}}\n"
        "end",
        35
    ),
    ?assertFormat(
        "try Expr of _ -> ok after Expr end",
        "try Expr of\n"
        "    _ -> ok\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormat(
        "try Expr of _ -> ok after Expr end",
        "try Expr of\n"
        "    _ -> ok\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormat(
        "try Expr1, Expr2 of _ -> ok after Expr end",
        "try\n"
        "    Expr1,\n"
        "    Expr2\n"
        "of\n"
        "    _ -> ok\n"
        "after Expr\n"
        "end"
    ),
    ?assertFormat(
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
    ?assertFormat(
        "if true -> ok end",
        "if\n"
        "    true -> ok\n"
        "end",
        100
    ),
    ?assertFormat(
        "if long() -> Expression end",
        "if\n"
        "    long() -> Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "if even(Longer) -> Expression end",
        "if\n"
        "    even(Longer) ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormat(
        "if the(Guard); Is, Long -> Expression end",
        "if\n"
        "    the(Guard);\n"
        "    Is, Long ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormat(
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
    ?assertSame("??FOO"),
    ?assertFormat("? Foo", "?Foo"),
    ?assertSame("?foo"),
    ?assertFormat("?FOO(\n)", "?FOO()"),
    ?assertSame("?assertMatch(X when is_integer(X), Y)"),
    ?assertFormat(
        "?assertMatch(X when is_integer(X), Y)",
        "?assertMatch(\n"
        "    X when is_integer(X),\n"
        "    Y\n"
        ")",
        30
    ),
    ?assertFormat(
        "?assertMatch(X when is_integer(X), Y)",
        "?assertMatch(\n"
        "    X when\n"
        "        is_integer(X),\n"
        "    Y\n"
        ")",
        23
    ).

function(Config) when is_list(Config) ->
    ?assertSame("f() -> ok."),
    ?assertSame(
        "f(1) -> one;\n"
        "f(2) -> two."
    ),
    ?assertFormat(
        "f(1) -> one; f(2) -> begin two end.",
        "f(1) ->\n"
        "    one;\n"
        "f(2) ->\n"
        "    begin\n"
        "        two\n"
        "    end."
    ),
    ?assertSame(
        "?FOO(1) -> ok;\n"
        "?DEFAULT(?FOO)."
    ).

attribute(Config) when is_list(Config) ->
    ?assertFormat("-else .", "-else."),
    ?assertFormat("- foo (\n1).", "-foo(1)."),
    ?assertSame("-compile([export_all, nowarn_export_all])."),
    ?assertSame("-import(foo, [bar/2, baz/0])."),
    ?assertSame("-export([bar/2, baz/3])."),
    ?assertFormat(
        "-attribute([Long, Value]).",
        "-attribute(\n"
        "    [Long, Value]\n"
        ").",
        25
    ),
    ?assertFormat(
        "-attribute([ExceptionallyLong, Value]).",
        "-attribute([\n"
        "    ExceptionallyLong,\n"
        "    Value\n"
        "]).",
        25
    ),
    ?assertFormat(
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
    ).

record_definition(Config) when is_list(Config) ->
    ?assertSame(
        "-record(foo, {a = 1 :: integer(), b :: float(), c = 2, d}).",
        60
    ),
    ?assertFormat(
        "-record(foo, {a = 1 :: integer(), b :: float(), c  = 2, d}).",
        "-record(foo, {\n"
        "    a = 1 :: integer(),\n"
        "    b :: float(),\n"
        "    c = 2,\n"
        "    d\n"
        "}).",
        50
    ),
    ?assertSame(
        "-record(foo,\n"
        "    %% comment\n"
        "    {a = 1 :: integer(), b :: float(), c = 2, d}\n"
        ").",
        60
    ),
    ?assertSame(
        "-record(foo,\n"
        "    %% comment\n"
        "    {\n"
        "        a = 1 :: integer(),\n"
        "        b :: float(),\n"
        "        c = 2,\n"
        "        d\n"
        "    }\n"
        ").",
        60
    ).

spec(Config) when is_list(Config) ->
    ?assertSame(
        "-spec foo(Int) -> atom() when Int :: integer()."
    ),
    ?assertFormat(
        "-spec foo(Int) -> atom() when Int :: integer().",
        "-spec foo(Int) -> atom()\n"
        "    when Int :: integer().",
        40
    ),
    ?assertFormat(
        "-spec foo(Int) -> some_very:very(long, type) when Int :: integer().",
        "-spec foo(Int) ->\n"
        "    some_very:very(long, type)\n"
        "    when Int :: integer().",
        40
    ),
    ?assertSame(
        "-callback long_name(Bar) -> #{map := type([Bar, ...])}."
    ),
    ?assertFormat(
        "-callback long_name(Bar) -> #{map := type([Bar, ...])}.",
        "-callback long_name(Bar) ->\n"
        "    #{map := type([Bar, ...])}.",
        50
    ),
    ?assertFormat(
        "-spec foo(integer()) -> some_very:very(long, type); (atom()) -> atom().",
        "-spec foo\n"
        "    (integer()) -> some_very:very(long, type);\n"
        "    (atom()) -> atom()."
    ),
    ?assertFormat(
        "-spec foo(integer()) -> some_very:very(long, type); (1..2) -> atom().",
        "-spec foo\n"
        "    (integer()) ->\n"
        "        some_very:very(long, type);\n"
        "    (1..2) ->\n"
        "        atom().",
        40
    ),
    ?assertFormat(
        "-spec foo(Int) -> some_very_very:very(long, type) when Int :: integer(); (1..2) -> atom().",
        "-spec foo\n"
        "    (Int) ->\n"
        "        some_very_very:very(long, type)\n"
        "        when Int :: integer();\n"
        "    (1..2) ->\n"
        "        atom().",
        40
    ).

define(Config) when is_list(Config) ->
    ?assertSame(
        "-define(IN_RANGE(Value, Low, High), Value >= Low andalso Value =< High)."
    ),
    ?assertFormat(
        "-define(OUT_OF_RANGE(Value, Low, High), (Value) =< long_expression(Low), Value >= long_expression(High)).",
        "-define(OUT_OF_RANGE(Value, Low, High),\n"
        "    (Value) =< long_expression(Low),\n"
        "    Value >= long_expression(High)\n"
        ").",
        40
    ),
    ?assertSame(
        "-define(FOO(X), begin\n"
        "    is_atom(X) orelse is_tuple(X)\n"
        "end)."
    ),
    ?assertSame(
        "-define(FOO(X), [1, 2, 3])."
    ),
    ?assertSame(
        "-define(FOO(X), [\n"
        "    1,\n"
        "    2,\n"
        "    3\n"
        "])."
    ).

type(Config) when is_list(Config) ->
    ?assertSame(
        "-opaque foo() :: #foo{a :: integer(), b :: module:type()}."
    ),
    ?assertFormat(
        "-type foobar() :: #foo{a :: integer(), b :: mod:type()}.",
        "-type foobar() ::\n"
        "    #foo{a :: integer(), b :: mod:type()}.",
        50
    ),
    ?assertSame(
        "-type foo() ::\n"
        "    fun((A, B, C) -> return_type(A, B, C)).",
        50
    ),
    ?assertSame(
        "-type foo() :: #{\n"
        "    a := integer(),\n"
        "    b => float()\n"
        "}."
    ),
    ?assertSame(
        "-opaque foo() :: {<<>>, <<_:8>>, <<_:_*4>>, <<_:8, _:_*4>>}."
    ),
    ?assertSame(
        "-type foo() :: {fun(), fun((...) -> mod:bar()), fun(() -> integer())}."
    ).

comment(Config) when is_list(Config) ->
    ?assertSame(
        "%foo\n"
        "1 + 2"
    ),
    ?assertFormat(
        "1 +\n"
        "%% foo\n"
        "2",
        "1 +\n"
        "    %% foo\n"
        "    2"
    ),
    ?assertFormat(
        "[%% foo\n"
        "]",
        "%% foo\n"
        "[]"
    ),
    ?assertFormat(
        "[\n"
        "    1 %% foo\n"
        "]",
        "[\n"
        "    %% foo\n"
        "    1\n"
        "]"
    ),
    ?assertFormat(
        "[1,2,3 %% foo\n"
        "]",
        "%% foo\n"
        "[1, 2, 3]"
    ).

format(String, PageWidth) ->
    {ok, [Node], []} = erlfmt:read_nodes_string("nofile", String),
    Doc = erlfmt_format2:to_algebra(Node),
    Rendered = erlfmt_algebra2:format(Doc, PageWidth),
    unicode:characters_to_list(Rendered).
