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
    tuple/1
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
        {literals, [parallel], [
            {group, integers},
            {group, floats},
            char,
            {group, atoms},
            {group, strings},
            variable,
            {group, operators},
            {group, containers}
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
            tuple
        ]}
    ].

all() ->
    [{group, literals}].

%%--------------------------------------------------------------------
%% TEST CASES

-define(assertSameExpr(String), ?assertSameExpr(String, 80)).
-define(assertSameExpr(String, PageWidth), ?assertEqual(String, format_expr(String, PageWidth))).

-define(assertFormatExpr(Bad, Good), ?assertFormatExpr(Bad, Good, 80)).
-define(assertFormatExpr(Bad, Good, PageWidth), begin
    ?assertEqual(Good, format_expr(Good, PageWidth)),
    ?assertEqual(Good, format_expr(Bad, PageWidth))
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
    ?assertFormatExpr("\"foo\" \"bar\"", "\"foo\"\n\"bar\"", 5),

    %% Measures text size in graphemes (\x{61}\x{301} is á in NFD normalisation)
    ?assertSameExpr("\"\x{61}\x{301}\" \"á\"", 7),
    ?assertFormatExpr("\"\x{61}\x{301}\" \"á\"", "\"\x{61}\x{301}\"\n\"á\"", 6),

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
    ?assertFormatExpr("Foo ++ Bar ++ Baz", "Foo ++\n    Bar ++ Baz", 15),
    ?assertFormatExpr("Foo ++ Bar ++ Baz", "Foo ++\n    Bar ++\n    Baz", 5),
    ?assertFormatExpr("((Foo ++ Bar) ++ Baz)", "(Foo ++ Bar) ++\n    Baz", 15),
    ?assertFormatExpr("((Foo ++ Bar) ++ Baz)", "(Foo ++\n     Bar) ++\n    Baz", 5),

    %% Nested same operator left-associative
    ?assertSameExpr("Foo + Bar + Baz"),
    ?assertFormatExpr("Foo + Bar + Baz", "Foo + Bar +\n    Baz", 14),
    ?assertFormatExpr("Foo + Bar + Baz", "Foo +\n    Bar +\n    Baz", 5),
    ?assertFormatExpr("(Foo + (Bar + Baz))", "Foo +\n    (Bar + Baz)", 15),
    ?assertFormatExpr("(Foo + (Bar + Baz))", "Foo +\n    (Bar +\n         Baz)", 5),

    %% With precedence
    ?assertFormatExpr("(A + B) == (C + D)", "A + B == C + D"),
    ?assertSameExpr("A + (B == C) + D"),
    ?assertFormatExpr("(A + B) == (C + D)", "A + B ==\n    C + D", 10),
    ?assertFormatExpr("Foo * (B + C) * D", "Foo *\n    (B + C) *\n    D", 10),
    ?assertFormatExpr("A * (B + C) * D", "A * (B + C) *\n    D", 10),
    ?assertFormatExpr("One * (Two + Three + Four) * Five", "One *\n    (Two + Three +\n         Four) * Five", 25).

tuple(Config) when is_list(Config) ->
    ?assertFormatExpr("{ }", "{}"),
    ?assertFormatExpr("{1,2}", "{1, 2}"),
    ?assertFormatExpr("{1,{2,3}}", "{1, {2, 3}}"),
    ?assertFormatExpr("{1,{2,3}}", "{\n    1,\n    {2, 3}\n}", 10),
    ?assertFormatExpr("{1,{long,word}}", "{\n    1,\n    {\n        long,\n        word\n    }\n}", 15).

format_expr(String, PageWidth) ->
    {ok, Tokens, _} = erl_scan:string("f() -> " ++ String ++ ".", 1, [text]),
    {ok, {function, _, [{clause, _, _, [], [], [Expr]}]}} =
        erlfmt_parse:parse_form(Tokens),
    Doc = erlfmt_format:expr_to_algebra(Expr),
    Rendered = erlfmt_algebra:document_render(Doc, [{page_width, PageWidth}]),
    unicode:characters_to_list(Rendered).
