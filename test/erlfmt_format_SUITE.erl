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
    variable/1
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
            string_escapes
        ]}
    ].

all() ->
    [{group, literals}].

%%--------------------------------------------------------------------
%% TEST CASES

-define(assertSameExpr(String), ?assertEqual(String, format_expr(String))).

-define(assertFormatExpr(Bad, Good), begin
    ?assertEqual(Good, format_expr(Good)),
    ?assertEqual(Good, format_expr(Bad))
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

variable(Config) when is_list(Config) ->
    ?assertSameExpr("Foo"),
    ?assertSameExpr("_Bar").

format_expr(String) ->
    {ok, Tokens, _} = erl_scan:string("f() -> " ++ String ++ ".", 1, [text]),
    {ok, {function, _, [{clause, _, _, [], [], [Expr]}]}} =
        erlfmt_parse:parse_form(Tokens),
    Doc = erlfmt_format:expr_to_algebra(Expr),
    Rendered = erlfmt_algebra:document_render(Doc, [{page_width, 80}]),
    unicode:characters_to_list(Rendered).
