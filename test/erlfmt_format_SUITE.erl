%%%-------------------------------------------------------------------
%%% @author Michał Muskała <micmus@whatsapp.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%%     Tests erlfmt_format
%%% @end
%%% @see http://erlang.org/doc/man/common_test.html
%%% -------------------------------------------------------------------

-module(erlfmt_format_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-oncall("whatsapp_erlang").

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
    int_decimal_base/1,
    int_binary_base/1,
    int_hex_base/1,
    float_normal/1,
    float_scientific/1,
    char/1,
    atom_keywords/1,
    atom_escapes/1,
    string_escapes/1,
    string_concat/1,
    variable/1,
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
    attribute/1
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
            {group, literals},
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
        {literals, [parallel], [
            {group, integers},
            {group, floats},
            char,
            {group, atoms},
            {group, strings},
            variable
        ]},
        {integers, [parallel], [
            int_decimal_base,
            int_binary_base,
            int_hex_base
        ]},
        {floats, [parallel], [
            float_normal,
            float_scientific
        ]},
        {atoms, [parallel], [
            atom_keywords,
            atom_escapes
        ]},
        {strings, [parallel], [
            string_escapes,
            string_concat
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
            {group, records}
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
        {group, forms}
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


int_decimal_base(Config) when is_list(Config) ->
    ?assertSameExpr("0"),
    ?assertSameExpr("100"),
    ?assertSameExpr("007"),
    ?assertSameExpr("100000"),
    ?assertSameExpr("10#1000").

int_binary_base(Config) when is_list(Config) ->
    ?assertSameExpr("2#0"),
    ?assertSameExpr("2#1"),
    ?assertSameExpr("2#101"),
    ?assertSameExpr("2#01").

int_hex_base(Config) when is_list(Config) ->
    ?assertSameExpr("16#1"),
    ?assertSameExpr("16#01"),
    ?assertFormatExpr("16#deadbeef", "16#DEADBEEF").

float_normal(Config) when is_list(Config) ->
    ?assertSameExpr("0.0"),
    ?assertSameExpr("1.0"),
    ?assertSameExpr("123.456"),
    ?assertSameExpr("001.100").

float_scientific(Config) when is_list(Config) ->
    ?assertSameExpr("1.0e1"),
    ?assertSameExpr("1.0e-1"),
    ?assertSameExpr("1.0e+1"),
    ?assertSameExpr("1.0e01"),
    ?assertSameExpr("001.100e-010"),
    ?assertFormatExpr("1.0E01", "1.0e01"),
    ?assertFormatExpr("1.0E-01", "1.0e-01").

char(Config) when is_list(Config) ->
    ?assertSameExpr("$a"),
    ?assertSameExpr("$Z"),
    ?assertSameExpr("$😅"),
    ?assertSameExpr("$\\0"),
    ?assertSameExpr("$\\\\"),
    ?assertSameExpr("$\\n"),
    ?assertSameExpr("$\\r"),
    ?assertSameExpr("$\\t"),
    ?assertSameExpr("$\\v"),
    ?assertSameExpr("$\\b"),
    ?assertSameExpr("$\\f"),
    ?assertSameExpr("$\\e"),
    ?assertSameExpr("$\\d"),
    ?assertFormatExpr("$\\z", "$z"),
    ?assertFormatExpr("$ ", "$\\s"),
    ?assertFormatExpr("$\\040", "$\\040"),
    ?assertFormatExpr("$\\xab", "$\\xAB"),
    ?assertFormatExpr("$\\x{ab}", "$\\x{AB}").

atom_keywords(Config) when is_list(Config) ->
    ?assertSameExpr("'after'"),
    ?assertSameExpr("'begin'"),
    ?assertSameExpr("'case'"),
    ?assertSameExpr("'try'"),
    ?assertSameExpr("'cond'"),
    ?assertSameExpr("'catch'"),
    ?assertSameExpr("'andalso'"),
    ?assertSameExpr("'orelse'"),
    ?assertSameExpr("'end'"),
    ?assertSameExpr("'fun'"),
    ?assertSameExpr("'if'"),
    ?assertSameExpr("'let'"),
    ?assertSameExpr("'of'"),
    ?assertSameExpr("'receive'"),
    ?assertSameExpr("'when'"),
    ?assertSameExpr("'bnot'"),
    ?assertSameExpr("'not'"),
    ?assertSameExpr("'div'"),
    ?assertSameExpr("'rem'"),
    ?assertSameExpr("'band'"),
    ?assertSameExpr("'and'"),
    ?assertSameExpr("'bor'"),
    ?assertSameExpr("'bxor'"),
    ?assertSameExpr("'bsl'"),
    ?assertSameExpr("'bsr'"),
    ?assertSameExpr("'or'"),
    ?assertSameExpr("'xor'"),
    ?assertFormatExpr("'not_A_keyword'", "not_A_keyword").

atom_escapes(Config) when is_list(Config) ->
    ?assertSameExpr("foobar"),
    ?assertSameExpr("foo123456789"),
    ?assertSameExpr("'foo bar'"),
    ?assertSameExpr("'foo\\tbar'"),
    ?assertSameExpr("''"),
    ?assertSameExpr("'\\''"),
    ?assertSameExpr("'\\\\'"),
    ?assertSameExpr("'Var'"),
    ?assertSameExpr("'\\1a'"),
    ?assertSameExpr("'\\12a'"),
    ?assertSameExpr("'\\123a'"),
    ?assertFormatExpr("'foo\\xab'", "'foo\\xAB'"),
    ?assertFormatExpr("'foo\\x{ab}'", "'foo\\x{AB}'"),
    ?assertFormatExpr("'foo\\z'", "fooz"),
    ?assertFormatExpr("'foo\\s'", "'foo '"),
    ?assertFormatExpr("'foo\\ '", "'foo '").

string_escapes(Config) when is_list(Config) ->
    ?assertSameExpr("\"'\""),
    ?assertSameExpr("\"\\\"\""),
    ?assertSameExpr("\"😱\""),
    ?assertSameExpr("\" \\40\\x32\\x{0032}\""),
    ?assertSameExpr("\"The quick brown fox jumps over the lazy dog\""),
    ?assertFormatExpr("\"\\s\"", "\" \"").

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

    ?assertSameExpr("\"foo\" Foo \"bar\"").

variable(Config) when is_list(Config) ->
    ?assertSameExpr("Foo"),
    ?assertSameExpr("_Bar").

unary_operator(Config) when is_list(Config) ->
    %% Formats symbolic operators without space
    ?assertFormatExpr("+ 1", "+1"),
    ?assertFormatExpr("- 1", "-1"),

    %% Formats word operators with space
    ?assertSameExpr("bnot 1"),
    ?assertSameExpr("not true"),
    ?assertSameExpr("catch 1"),

    %% Wraps nested operators
    ?assertFormatExpr("bnot+1", "bnot (+1)"),
    ?assertFormatExpr("+ +1", "+(+1)"),
    ?assertFormatExpr("not catch 1", "not (catch 1)"),
    ?assertFormatExpr("not(1 + 1)", "not (1 + 1)"),
    ?assertFormatExpr("(bnot 1) * 1", "bnot 1 * 1"),
    ?assertSameExpr("(catch 1) + 1"),

    %% Unless it's nested not or bnot
    ?assertSameExpr("bnot bnot Var"),
    ?assertSameExpr("not not true").

binary_operator(Config) when is_list(Config) ->
    %% Bitwise operators force parens
    ?assertFormatExpr("CRC bxor Byte band 16#ff", "CRC bxor (Byte band 16#FF)"),
    ?assertSameExpr("(CRC bsl 8) bxor Byte"),

    %% Mixed operators force parens only when mixed
    ?assertFormatExpr("Foo ++ Bar ++ Baz -- Bat", "Foo ++ Bar ++ (Baz -- Bat)"),
    ?assertSameExpr("Foo > Bar andalso Baz =:= Bat"),
    ?assertFormatExpr("Foo and Bar or Baz and Bat", "(Foo and Bar) or (Baz and Bat)"),

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
        "((Foo ++ Bar) ++ Baz)",
        "(Foo ++ Bar) ++\n"
        "    Baz",
        15
    ),
    ?assertFormatExpr(
        "((Foo ++ Bar) ++ Baz)",
        "(Foo ++\n"
        "     Bar) ++\n"
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
        "(Foo + (Bar + Baz))",
        "Foo +\n"
        "    (Bar + Baz)",
        15
    ),
    ?assertFormatExpr(
        "(Foo + (Bar + Baz))",
        "Foo +\n"
        "    (Bar +\n"
        "         Baz)",
        5
    ),

    %% With precedence
    ?assertFormatExpr("(A + B) == (C + D)", "A + B == C + D"),
    ?assertSameExpr("A + (B == C) + D"),
    ?assertFormatExpr(
        "(A + B) == (C + D)",
        "A + B ==\n"
        "    C + D",
        10
    ),
    ?assertFormatExpr(
        "Foo * (B + C) * D",
        "Foo *\n"
        "    (B + C) *\n"
        "    D",
        10
    ),
    ?assertFormatExpr(
        "A * (B + C) * D",
        "A * (B + C) *\n"
        "    D",
        10
    ),
    ?assertFormatExpr(
        "One * (Two + Three + Four) * Five",
        "One *\n"
        "    (Two + Three +\n"
        "         Four) * Five",
        25
    ).

tuple(Config) when is_list(Config) ->
    ?assertFormatExpr("{ }", "{}"),
    ?assertFormatExpr("{1,2}", "{1, 2}"),
    ?assertFormatExpr("{1,{2,3}}", "{1, {2, 3}}"),
    ?assertFormatExpr(
        "{1,{2,3}}",
        "{\n"
        "    1,\n"
        "    {2, 3}\n"
        "}",
        10
    ),
    ?assertFormatExpr(
        "{1,{long,word}}",
        "{\n"
        "    1,\n"
        "    {\n"
        "        long,\n"
        "        word\n"
        "    }\n"
        "}",
        15
    ).

list(Config) when is_list(Config) ->
    ?assertFormatExpr("[\n]", "[]"),
    ?assertSameExpr("[1 | [2 | []]]"),
    ?assertFormatExpr("[ 1 ,2,3, 4]", "[1, 2, 3, 4]"),
    ?assertFormatExpr("[1,2,3|4]", "[1, 2, 3 | 4]"),
    ?assertFormatExpr(
        "[1,[2,3]]",
        "[\n"
        "    1,\n"
        "    [2, 3]\n"
        "]",
        10
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
    ).

binary(Config) when is_list(Config) ->
    ?assertFormatExpr("<< >>", "<<>>"),
    ?assertSameExpr("<<(1 + 1), (#{}), (#foo{}), (#{}#{}), (#foo{}#foo{}), (#foo.bar), (call())>>"),
    ?assertFormatExpr("<<(1)>>", "<<1>>"),
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
        "     Bar)#foo.bar",
        5
    ),
    ?assertSameExpr("X#?FOO.bar"),
    ?assertSameExpr("X?FOO.bar").

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
        "long_name({Very, Long, Expression})",
        "long_name(\n"
        "    {\n"
        "        Very,\n"
        "        Long,\n"
        "        Expression\n"
        "    }\n"
        ")",
        25
    ),
    ?assertFormatExpr(
        "long_name({Long, Expression}, AnotherArgument)",
        "long_name(\n"
        "    {Long, Expression},\n"
        "    AnotherArgument\n"
        ")",
        25
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
        "    when Guarded ->\n"
        "        Expression\n"
        "end",
        25
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
        "fun (Pattern) when Guard; Is, Long -> Expression end",
        "fun\n"
        "    (Pattern)\n"
        "    when Guard;\n"
        "         Is, Long ->\n"
        "        Expression\n"
        "end",
        25
    ),
    ?assertFormatExpr(
        "fun (Pattern) when Guard; Is, Even, Longer -> Expression end",
        "fun\n"
        "    (Pattern)\n"
        "    when Guard;\n"
        "         Is,\n"
        "         Even,\n"
        "         Longer ->\n"
        "        Expression\n"
        "end",
        25
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
        "    when Guarded ->\n"
        "        Expression\n"
        "end",
        25
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
        "case 1 of Pattern when Guard; Is, Long -> Expression end",
        "case 1 of\n"
        "    Pattern\n"
        "    when Guard;\n"
        "         Is, Long ->\n"
        "        Expression\n"
        "end",
        25
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
        "    when Guarded ->\n"
        "        Expression\n"
        "end",
        25
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
        "receive Pattern when Guard; Is, Long -> Expression end",
        "receive\n"
        "    Pattern\n"
        "    when Guard;\n"
        "         Is, Long ->\n"
        "        Expression\n"
        "end",
        25
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
        "    X\n"
        "    when is_integer(X),\n"
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
    %% TODO: fix the spaces
    ?assertSameForm("-import(foo, [bar / 2, baz / 0])."),
    ?assertSameForm("-export([bar / 2, baz / 3])."),
    ?assertFormatForm(
        "-attribute([Long, Value]).",
        "-attribute(\n"
        "    [Long, Value]\n"
        ").",
        25
    ),
    ?assertFormatForm(
        "-attribute([ExceptionallyLong, Value]).",
        "-attribute(\n"
        "    [\n"
        "        ExceptionallyLong,\n"
        "        Value\n"
        "    ]\n"
        ").",
        25
    ).

format_form(String, PageWidth) ->
    {ok, Tokens, _} = erl_scan:string(String, 1, [text]),
    {ok, Form} = erlfmt_parse:parse_form(Tokens),
    Doc = erlfmt_format:form_to_algebra(Form),
    Rendered = erlfmt_algebra:document_render(Doc, [{page_width, PageWidth}]),
    unicode:characters_to_list(Rendered).

format_expr(String, PageWidth) ->
    {ok, Tokens, _} = erl_scan:string("f() -> " ++ String ++ ".", 1, [text]),
    {ok, {function, _, [{clause, _, _, [], [], [Expr]}]}} =
        erlfmt_parse:parse_form(Tokens),
    Doc = erlfmt_format:expr_to_algebra(Expr),
    Rendered = erlfmt_algebra:document_render(Doc, [{page_width, PageWidth}]),
    unicode:characters_to_list(Rendered).
