%% Copyright (c) Meta Platforms, Inc. and its affiliates.
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
    dotted/1,
    string_concat/1,
    unary_operator/1,
    binary_operator/1,
    tuple/1,
    untagged_tuple/1,
    list/1,
    binary/1,
    map_create/1,
    map_update/1,
    record_create/1,
    record_update/1,
    record_index/1,
    record_field/1,
    list_comprehension/1,
    map_comprehension/1,
    binary_comprehension/1,
    call/1,
    block/1,
    fun_expression/1,
    case_expression/1,
    maybe_expression/1,
    receive_expression/1,
    try_expression/1,
    if_expression/1,
    macro/1,
    function/1,
    attribute/1,
    exportimport/1,
    ifdef/1,
    record_definition/1,
    spec/1,
    define/1,
    type/1,
    exprs/1,
    comment/1,
    force_break/1,
    binary_operator_more/1,
    binary_operator_equal/1,
    update_edgecase/1,
    sigils/1,
    doc_attributes/1,
    doc_macros/1
]).

suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    Features = get_features(),
    [{features, Features} | Config].

end_per_suite(_Config) ->
    ok.

init_per_group(otp_27_features, Config) ->
    case erlang:system_info(otp_release) >= "27" of
        true -> Config;
        false -> {skip, "Skipping tests for features from OTP >= 27"}
    end;
init_per_group(_, Config) ->
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(maybe_expression, Config) ->
    case has_feature(maybe_expr, Config) of
        true -> Config;
        false -> {skip, "Maybe feature not present in the runtime system"}
    end;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {expressions, [parallel], [
            literals,
            dotted,
            string_concat,
            {group, containers},
            {group, operators},
            {group, comprehensions},
            call,
            block,
            fun_expression,
            case_expression,
            maybe_expression,
            receive_expression,
            try_expression,
            if_expression,
            macro,
            doc_macros
        ]},
        {forms, [parallel], [
            function,
            attribute,
            exportimport,
            ifdef,
            spec,
            record_definition,
            define,
            type,
            exprs
        ]},
        {operators, [parallel], [
            unary_operator,
            binary_operator,
            binary_operator_more,
            binary_operator_equal
        ]},
        {containers, [parallel], [
            tuple,
            untagged_tuple,
            list,
            binary,
            map_create,
            map_update,
            {group, records},
            force_break,
            update_edgecase
        ]},
        {records, [parallel], [
            record_create,
            record_update,
            record_index,
            record_field
        ]},
        {comprehensions, [parallel], [
            list_comprehension,
            map_comprehension,
            binary_comprehension
        ]},
        {otp_27_features, [parallel], [
            sigils,
            doc_attributes
        ]}
    ].

all() ->
    [
        {group, expressions},
        {group, forms},
        comment
    ].

get_features() ->
    case erlang:function_exported(erl_features, all, 0) of
        true -> erl_features:all();
        false -> []
    end.

has_feature(Feature, Config) ->
    lists:member(Feature, proplists:get_value(features, Config)).

%%--------------------------------------------------------------------
%% TEST CASES
-define(assertSame(String), ?assertSame(String, 80)).
-define(assertSame(String, PrintWidth),
    ?assertEqual(String, format_string(String, [{print_width, PrintWidth}]))
).

-define(assertFormat(Bad, Good), ?assertFormat(Bad, Good, 80)).
-define(assertFormat(Bad, Good, PrintWidth), begin
    ?assertEqual(Good, format_string(Good, [{print_width, PrintWidth}])),
    ?assertEqual(Good, format_string(Bad, [{print_width, PrintWidth}]))
end).

format_string(String, Options) ->
    {ok, Formatted, []} = erlfmt:format_string(String, Options),
    Formatted.

literals(Config) when is_list(Config) ->
    ?assertSame("100\n"),
    ?assertSame("007\n"),
    ?assertSame("10#1000\n"),
    ?assertSame("2#101\n"),
    ?assertSame("16#01\n"),
    ?assertSame("16#deadBEEF\n"),
    ?assertSame("001.100\n"),
    ?assertSame("1.0e-1\n"),
    ?assertSame("001.100e-010\n"),
    ?assertSame("1.0E01\n"),
    ?assertSame("$a\n"),
    ?assertSame("$\x{1F605}\n"),
    ?assertSame("$\\0\n"),
    ?assertSame("$\\\\\n"),
    ?assertSame("$\\t\n"),
    ?assertSame("$\\z\n"),
    ?assertSame("$\\040\n"),
    ?assertSame("$\\xAb\n"),
    ?assertSame("$\\x{Ab}\n"),
    ?assertSame("'foo'\n"),
    ?assertSame("foo\n"),
    ?assertSame("'andalso'\n"),
    ?assertSame("'foo bar'\n"),
    ?assertSame("'foo\\tbar'\n"),
    ?assertSame("'\\''\n"),
    ?assertSame("'foo\\xaB'\n"),
    ?assertSame("'foo\\x{aB}'\n"),
    ?assertSame("'foo\\z'\n"),
    ?assertSame("'foo\\s'\n"),
    ?assertSame("\"'\"\n"),
    ?assertSame("\"\x{1F631}\"\n"),
    ?assertSame("\" \\40\\x32\\x{0032}\"\n"),
    ?assertSame("\"The quick brown fox jumps over the lazy dog\"\n"),
    ?assertSame("\"\\s \"\n"),
    ?assertSame("Foo\n"),
    ?assertSame("_Bar\n"),
    ?assertFormat("$ ", "$\\s\n").

sigils(Config) when is_list(Config) ->
    %% https://www.erlang.org/blog/highlights-otp-27/#sigils
    ?assertSame("~b\"abc\\txyz\"\n"),
    ?assertSame("~\"abc\\txyz\"\n"),
    ?assertSame("~s{\"abc\\txyz\"}\n"),
    %% The modifier X does not technically exist, but there seems to be no supported
    %% modifiers yet even though they are correctly parsed.
    ?assertSame("~b\"abc\\txyz\"x\n"),
    ?assertSame("~s\"\"\"\n\\tabc\n\\tdef\n\"\"\"\n"),
    %% https://www.erlang.org/blog/highlights-otp-27/#triple-quoted-strings
    ?assertFormat(
        "\"\"\"\n"
        "Test\n"
        "\"\"\"\n",
        "\"Test\"\n"
    ),
    ?assertSame("\"\"\"\nTest\nMultiline\n\"\"\"\n"),
    ?assertSame("~\"\"\"\nTest\nMultiline\n\"\"\"\n").

dotted(Config) when is_list(Config) ->
    ?assertSame("<0.1.2>\n"),
    ?assertSame("#Ref<0.1.2.3>\n"),
    ?assertSame("#Port<0.1>\n").

string_concat(Config) when is_list(Config) ->
    ?assertSame("\"foo\" \"bar\"\n"),
    ?assertSame("<<\"foo\" ?foo()>>\n"),
    ?assertFormat(
        "\"foo\" \"bar\"",
        "\"foo\"\n"
        "\"bar\"\n",
        5
    ),

    %% Measures text size in graphemes (\x{61}\x{301} is á in NFD normalisation)
    ?assertSame("\"\x{61}\x{301}\" \"á\"\n", 7),
    ?assertFormat(
        "\"\x{61}\x{301}\" \"á\"",
        "\"\x{61}\x{301}\"\n"
        "\"á\"\n",
        6
    ),

    ?assertSame("\"foo\" Foo \"bar\"\n"),

    %% Multiline strings are converted to sequence of concats
    ?assertFormat(
        "\"foo\nbar\"",
        "\"foo\\n\"\n"
        "\"bar\"\n"
    ),
    ?assertFormat(
        "\"foo\nbar\n\"",
        "\"foo\\n\"\n"
        "\"bar\\n\"\n"
    ),
    ?assertFormat(
        "\"foo\n\"",
        "\"foo\\n\"\n"
    ),

    ?assertFormat(
        "foo(\"foo\"\n"
        "\"bar\")",
        "foo(\n"
        "    \"foo\"\n"
        "    \"bar\"\n"
        ")\n"
    ),
    ?assertFormat(
        "x() ->\n"
        "    \"A\n"
        "B\"\n"
        "    \"C\".",
        "x() ->\n"
        "    \"A\\n\"\n"
        "    \"B\"\n"
        "    \"C\".\n"
    ),
    ?assertFormat(
        "X = \"foo\n"
        "bar\"",
        "X =\n"
        "    \"foo\\n\"\n"
        "    \"bar\"\n"
    ),
    ?assertFormat(
        "-define(R,\n"
        "    \"a\"  % a\n"
        "    \"b\" % b\n"
        "    \"c\" % c\n"
        "    \"d\"). % d\n",
        "-define(R,\n"
        "    % a\n"
        "    \"a\"\n"
        "    % b\n"
        "    \"b\"\n"
        "    % c\n"
        "    \"c\"\n"
        "    % d\n"
        "    \"d\"\n"
        ").\n"
    ).

unary_operator(Config) when is_list(Config) ->
    %% Formats symbolic operators without space
    ?assertFormat("+ 1", "+1\n"),
    ?assertFormat("- 1", "-1\n"),

    %% Formats word operators with space
    ?assertSame("bnot 1\n"),
    ?assertSame("not true\n"),
    ?assertSame("catch 1\n"),

    %% Does not change parenthesis, only whitespace
    ?assertFormat("bnot+1", "bnot +1\n"),
    ?assertSame("+ +1\n"),
    ?assertSame("+(+1)\n"),
    ?assertSame("+ -1\n"),
    ?assertSame("-(+1)\n"),
    ?assertSame("not catch 1\n"),
    ?assertFormat("not(1 + 1)", "not (1 + 1)\n"),
    ?assertFormat("(bnot 1)*1", "(bnot 1) * 1\n"),
    ?assertSame("(catch 1) + 1\n"),

    %% Unless it's nested not or bnot
    ?assertSame("bnot bnot Var\n"),
    ?assertSame("not not true\n").

binary_operator(Config) when is_list(Config) ->
    %% No changes to parens
    ?assertSame("CRC bxor Byte band 16#ff\n"),
    ?assertSame("(CRC bsl 8) bxor Byte\n"),
    ?assertSame("Foo ++ Bar ++ Baz -- Bat\n"),
    ?assertSame("Foo ++ Bar ++ (Baz -- Bat)\n"),
    ?assertSame("Foo ++ (Bar ++ Baz) -- Bat\n"),
    ?assertSame("Foo > Bar andalso Baz =:= Bat\n"),
    ?assertSame("Foo and Bar or (Baz and Bat)\n"),

    %% Nested same operator right-associative
    ?assertSame("Foo ++ Bar ++ Baz\n"),
    ?assertFormat(
        "Foo ++ Bar ++ Baz",
        "Foo ++ Bar ++\n"
        "    Baz\n",
        15
    ),
    ?assertFormat(
        "Foo ++ Bar ++ Baz",
        "Foo ++\n"
        "    Bar ++\n"
        "    Baz\n",
        5
    ),
    ?assertFormat(
        "(Foo ++ Bar) ++ Baz",
        "(Foo ++ Bar) ++\n"
        "    Baz\n",
        15
    ),
    ?assertFormat(
        "(Foo ++ Bar) ++ Baz",
        "(Foo ++\n"
        "    Bar) ++\n"
        "    Baz\n",
        5
    ),

    %% Nested same operator left-associative
    ?assertSame("Foo + Bar + Baz\n"),
    ?assertFormat(
        "Foo + Bar + Baz",
        "Foo + Bar +\n"
        "    Baz\n",
        14
    ),
    ?assertFormat(
        "Foo + Bar + Baz",
        "Foo +\n"
        "    Bar +\n"
        "    Baz\n",
        5
    ),
    ?assertFormat(
        "Foo + (Bar + Baz)",
        "Foo +\n"
        "    (Bar + Baz)\n",
        15
    ),
    ?assertFormat(
        "Foo + (Bar + Baz)",
        "Foo +\n"
        "    (Bar +\n"
        "        Baz)\n",
        5
    ),

    %% With precedence
    ?assertFormat("(A + B) == C + D", "(A + B) == C + D\n"),
    ?assertSame("A + (B == C) + D\n"),
    ?assertFormat(
        "A + B == (C + D)",
        "A + B ==\n"
        "    (C + D)\n",
        15
    ),
    ?assertFormat(
        "Foo * (B + C) * D",
        "Foo *\n"
        "    (B + C) *\n"
        "    D\n",
        12
    ),
    ?assertFormat(
        "A * (B + C) * D",
        "A * (B + C) *\n"
        "    D\n",
        12
    ),
    ?assertFormat(
        "One * (Two + Three + Four) * Five",
        "One *\n"
        "    (Two + Three +\n"
        "        Four) * Five\n",
        20
    ),

    %% Next break fits
    ?assertSame(
        "Foo = [\n"
        "    1\n"
        "]\n"
    ),
    ?assertSame(
        "Foo = foo(\n"
        "    1\n"
        ")\n"
    ),
    ?assertSame(
        "Foo = ?foo(\n"
        "    1\n"
        ")\n"
    ),
    ?assertSame(
        "Foo = fun() ->\n"
        "    ok\n"
        "end\n",
        15
    ),
    ?assertSame(
        "foo() :: #{\n"
        "    a := integer()\n"
        "}\n"
    ),
    ?assertSame(
        "Foo =\n"
        "    [\n"
        "        1\n"
        "    ]\n"
    ),
    ?assertSame(
        "Foo = [\n"
        "    1\n"
        "]\n"
    ),
    ?assertSame(
        "cl(Opts) ->\n"
        "    F = fun() ->\n"
        "        {Ret, _Warnings} = dialyzer_cl:start(Opts),\n"
        "        Ret\n"
        "    end,\n"
        "    doit(F).\n"
    ),
    ?assertSame(
        "cl(Opts) ->\n"
        "    F =\n"
        "        fun() ->\n"
        "            {Ret, _Warnings} = dialyzer_cl:start(Opts),\n"
        "            Ret\n"
        "        end,\n"
        "    doit(F).\n"
    ),
    ?assertSame(
        "Foo = {\n"
        "    long_element_1,\n"
        "    long_element_2,\n"
        "    long_element_3\n"
        "}\n"
    ),
    ?assertSame(
        "Foo = <<\n"
        "    1/float,\n"
        "    <<222, 333>>\n"
        ">>\n"
    ),
    ?assertSame(
        "f() ->\n"
        "    {PreComments, InnerComments, PostComments} = split_comments(\n"
        "        element(2, Expr0),\n"
        "        Comments\n"
        "    ).\n"
    ),
    ?assertSame(
        "{function, _, [{clause, _, {call, _, _, [Pat]}, empty, [_]}]} =\n"
        "    parse_form(\"f(\" ++ String ++ \") -> ok.\")\n"
    ),
    ?assertSame(
        "[\n"
        "    Foo\n"
        " || {Long, Pattern, YesVeryVeryLong, Pattern, EvenLonger, Longest} <-\n"
        "        foo(1, 2)\n"
        "]\n",
        80
    ),

    %% Not next break fits
    ?assertSame(
        "Foo = {\n"
        "    foo,\n"
        "    bar,\n"
        "    baz\n"
        "}\n"
    ),
    ?assertSame(
        "Foo =\n"
        "    {\n"
        "        foo,\n"
        "        bar,\n"
        "        baz\n"
        "    }\n"
    ),
    ?assertSame(
        "Foo = {\n"
        "    foo,\n"
        "    bar,\n"
        "    baz\n"
        "}\n"
    ),
    ?assertFormat(
        "Foo = {\n"
        "        foo,\n"
        "        bar,\n"
        "        baz\n"
        "    }\n",
        "Foo = {\n"
        "    foo,\n"
        "    bar,\n"
        "    baz\n"
        "}\n"
    ),
    ?assertFormat(
        "Foo = {foo, bar, verylong, morelonger, baz}\n",
        "Foo =\n"
        "    {foo, bar, verylong, morelonger,\n"
        "        baz}\n",
        40
    ),

    %% Keeps existing breaks
    ?assertSame(
        "Foo andalso\n"
        "    Bar andalso\n"
        "    Baz\n"
    ),

    %% comments
    ?assertSame(
        "Foo\n"
        "%% binary op comment"
        "++ Bar.\n"
    ),
    ?assertFormat(
        "Foo\n"
        "++\n"
        "%% binary op comment\n"
        "Bar.\n",
        "Foo ++\n"
        "    %% binary op comment\n"
        "    Bar.\n"
    ),
    ?assertFormat(
        "Foo\n"
        "++ %% binary op comment\n"
        "Bar.\n",
        "Foo ++\n"
        "    %% binary op comment\n"
        "    Bar.\n"
    ),
    ?assertSame(
        "Foo\n"
        "%% binary op comment"
        "= Bar.\n"
    ),
    ?assertFormat(
        "Foo\n"
        "= %% binary op comment\n"
        "Bar.\n",
        "Foo =\n"
        "    %% binary op comment\n"
        "    Bar.\n"
    ).

binary_operator_more(Config) when is_list(Config) ->
    ?assertSame(
        "a_function_abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc andalso\n"
        "    a_short_one andalso another_very_very_long_one andalso and_another\n"
    ),
    ?assertSame(
        "((a_function_abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc andalso\n"
        "    a_short_one) andalso another_very_very_long_one) andalso and_another\n"
    ),
    ?assertFormat(
        "a_function_abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc andalso a_short_one andalso another_very_very_long_one andalso and_another\n",
        "a_function_abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc andalso\n"
        "    a_short_one andalso another_very_very_long_one andalso and_another\n"
    ),
    ?assertFormat(
        "Foo orelse Bar orelse Baz\n",
        "Foo orelse\n"
        "    Bar orelse\n"
        "    Baz\n",
        10
    ),
    ?assertFormat(
        "Foo orelse Bar orelse Baz\n",
        "Foo orelse Bar orelse\n"
        "    Baz\n",
        23
    ),
    ?assertFormat(
        "Foo orelse Bar orelse Baz orelse Abc orelse Cde\n",
        "Foo orelse Bar orelse Baz orelse\n"
        "    Abc orelse Cde\n",
        30
    ),
    ?assertSame(
        "Foo andalso\n"
        "    Bar andalso Baz\n"
    ),
    ?assertSame(
        "s2c(<<C, Rest/binary>>, Acc) when\n"
        "    C == $\; orelse C == $\$ orelse C == $\> orelse C == $\< orelse C == $\/\n"
        "->\n"
        "    ok.\n"
    ),
    ?assertSame(
        "s2c(<<C, Rest/binary>>, Acc) when\n"
        "    C == $\; orelse\n"
        "        C == $\$ orelse\n"
        "        C == $\> orelse\n"
        "        C == $\< orelse\n"
        "        C == $\/\n"
        "->\n"
        "    ok.\n"
    ),
    ?assertFormat(
        "s2c(<<C, Rest/binary>>, Acc) when\n"
        "    C == $\; orelse C == $\$ orelse C == $\> orelse C == $\< orelse C == $\/\n"
        "->\n"
        "    ok.\n",
        "s2c(<<C, Rest/binary>>, Acc) when\n"
        "    C == $\; orelse C == $\$ orelse C == $\> orelse\n"
        "        C == $\< orelse C == $\/\n"
        "->\n"
        "    ok.\n",
        42
    ),
    ?assertFormat(
        "[\n"
        "    Slash ++ binary_to_list(A2) ++ SlashArraySlash ++ binary_to_list(A1) ++ Slash ++ graphviz_edge_label(L)\n"
        " || {_, A1, A2, L} <- Es\n"
        "].\n",
        "[\n"
        "    Slash ++ binary_to_list(A2) ++ SlashArraySlash ++ binary_to_list(A1) ++\n"
        "        Slash ++ graphviz_edge_label(L)\n"
        " || {_, A1, A2, L} <- Es\n"
        "].\n"
    ),
    ?assertSame(
        "[\n"
        "    Slash ++\n"
        "        binary_to_list(A2) ++\n"
        "        SlashArraySlash ++\n"
        "        binary_to_list(A1) ++\n"
        "        Slash ++\n"
        "        graphviz_edge_label(L)\n"
        " || {_, A1, A2, L} <- Es\n"
        "].\n"
    ),
    ?assertFormat(
        "AVariable andalso AnotherVariable andalso AnotherVariableMore andalso AnotherVariableExtra\n",
        "AVariable andalso AnotherVariable andalso\n"
        "    AnotherVariableMore andalso\n"
        "    AnotherVariableExtra\n",
        40
    ),
    ?assertFormat(
        "is_registered(User)\n"
        "    andalso password_matches(User, Password)\n"
        "    andalso (has_active_session(User) orelse is_admin(User))\n"
        "    andalso\n"
        "        begin\n"
        "             register_login(User, dates:now()),\n"
        "             [ open_admin_session(User) || is_admin(User) ]\n"
        "        end\n",
        "is_registered(User) andalso\n"
        "    password_matches(User, Password) andalso\n"
        "    (has_active_session(User) orelse is_admin(User)) andalso\n"
        "    begin\n"
        "        register_login(User, dates:now()),\n"
        "        [open_admin_session(User) || is_admin(User)]\n"
        "    end\n"
    ),
    ?assertSame(
        "convert_meta(Key, Value) when\n"
        "    is_list(Key) orelse is_binary(Key),\n"
        "    is_list(Key) orelse\n"
        "        is_binary(Value) orelse\n"
        "        is_atom(Value) orelse\n"
        "        is_integer(Value)\n"
        "->\n"
        "    ok.\n"
    ),
    ?assertSame(
        "equivalent(L1, R1) andalso equivalent(L2, R2) andalso equivalent(L3, R3) andalso\n"
        "    equivalent(L4, R4)\n"
    ),
    ?assertSame(
        "equivalent(L1, R1) andalso\n"
        "    equivalent(L2, R2) andalso equivalent(L3, R3) andalso equivalent(L4, R4)\n"
    ),
    ?assertFormat(
        "equivalent(L1, R1) andalso equivalent(L2, R2) andalso equivalent(L3, R3) andalso equivalent(L4, R4)\n",
        "equivalent(L1, R1) andalso equivalent(L2, R2) andalso equivalent(L3, R3) andalso\n"
        "    equivalent(L4, R4)\n"
    ),
    ?assertFormat(
        "the:ever(funny) andalso begin blocksof:my(code), that:appear(between), your:boolean(expressions) end andalso are:terrible(things, to, debug)\n",
        "the:ever(funny) andalso\n"
        "    begin\n"
        "        blocksof:my(code),\n"
        "        that:appear(between),\n"
        "        your:boolean(expressions)\n"
        "    end andalso are:terrible(things, to, debug)\n"
    ),
    ?assertFormat(
        "a_function:with_a(<<\"very\">>, Long, list, \"of\", <<\"attributes\">>) andalso a:short(one) andalso another:very_very_long_one() andalso Another\n",
        "a_function:with_a(<<\"very\">>, Long, list, \"of\", <<\"attributes\">>) andalso\n"
        "    a:short(one) andalso another:very_very_long_one() andalso Another\n"
    ),
    ?assertSame(
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA orelse\n"
        "    BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB andalso\n"
        "        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC.\n"
    ),
    ?assertSame(
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa andalso\n"
        "    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb orelse\n"
        "    cccccccccccccccccccccccccccccccccc.\n"
    ),
    ?assertSame(
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa andalso\n"
        "    (bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb orelse\n"
        "        cccccccccccccccccccccccccccccccccc).\n"
    ),
    ?assertSame(
        "(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa andalso\n"
        "    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb) orelse\n"
        "    cccccccccccccccccccccccccccccccccc.\n"
    ),
    ?assertFormat(
        "Foo orelse\n"
        "    Bar orelse %% orelse comment\n"
        "    Baz\n",
        "Foo orelse\n"
        "    %% orelse comment\n"
        "    Bar orelse\n"
        "    Baz\n"
    ),
    ?assertSame(
        "Foo orelse\n"
        "    %% orelse comment\n"
        "    Bar orelse\n"
        "    %% orelse comment\n"
        "    Baz\n"
    ),
    ?assertFormat(
        "equivalent(L1, R1) andalso %% checking L1 and R1\n"
        "equivalent(L2, R2) andalso %% checking L2 and R2\n"
        "equivalent(L3, R3) andalso %% checking L3 and R3\n"
        "equivalent(L4, R4) %% checking L4 and R4\n",
        "%% checking L1 and R1\n"
        "equivalent(L1, R1) andalso\n"
        "    %% checking L2 and R2\n"
        "    equivalent(L2, R2) andalso\n"
        "    %% checking L3 and R3\n"
        "    equivalent(L3, R3) andalso\n"
        "    %% checking L4 and R4\n"
        "    equivalent(L4, R4)\n"
    ),
    ?assertFormat(
        "A orelse\n"
        "    B orelse c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n",
        "A orelse\n"
        "    B orelse\n"
        "    c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n"
    ),
    ?assertFormat(
        "A orelse B orelse c(\n"
        "D, E).\n",
        "A orelse B orelse\n"
        "    c(\n"
        "        D, E\n"
        "    ).\n"
    ).

%% These formattings might not be exactly what we want in some cases,
%% see our reasoning for why we stuck to this format here:
%% https://github.com/WhatsApp/erlfmt/blob/main/doc/FormattingDecisionAssociative.md
binary_operator_equal(Config) when is_list(Config) ->
    ?assertFormat(
        "A = B = c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n",
        "A =\n"
        "    B = c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n"
    ),
    ?assertSame(
        "case maps:get(AbcdefghId, Abcdefghs0, undefined) of\n"
        "    #abcdefgh{abcdefgh_type = AbcdefghType, specs = Specs, abc_de_abcde = [AbcdefgId | Rest]} =\n"
        "            Abcdefgh = #abcdefgh{\n"
        "                abcdefgh_type = AbcdefghType,\n"
        "                specs = Specs,\n"
        "                abc_de_abcde = [AbcdefgId | Rest]\n"
        "            } ->\n"
        "        {reply, ok, State}\n"
        "end.\n",
        100
    ),
    ?assertSame(
        "{B, C} =\n"
        "    case X of\n"
        "        true -> f();\n"
        "        _ -> g()\n"
        "    end\n"
    ),
    ?assertFormat(
        "A = {B, C} =\n"
        "    case X of\n"
        "        true -> f();\n"
        "        _ -> g()\n"
        "    end.\n",
        "A =\n"
        "    {B, C} =\n"
        "    case X of\n"
        "        true -> f();\n"
        "        _ -> g()\n"
        "    end.\n"
    ),
    ?assertFormat(
        "A = {{B1, B2}, C} = {B, C} =\n"
        "    case X of\n"
        "        true -> f();\n"
        "        _ -> g()\n"
        "    end.\n",
        "A =\n"
        "    {{B1, B2}, C} =\n"
        "    {B, C} =\n"
        "    case X of\n"
        "        true -> f();\n"
        "        _ -> g()\n"
        "    end.\n"
    ),
    ?assertSame(
        "A =\n"
        "    ({{B1, B2}, C} =\n"
        "        ({B, C} =\n"
        "            case X of\n"
        "                true -> f();\n"
        "                _ -> g()\n"
        "            end)).\n"
    ),
    ?assertFormat(
        "A = B = C = D = E = F\n.",
        "A =\n"
        "    B =\n"
        "    C =\n"
        "    D =\n"
        "    E = F.\n",
        10
    ),
    ?assertFormat(
        "A = B = C =< D = E = F\n.",
        "A =\n"
        "    B =\n"
        "    C =< D =\n"
        "    E = F.\n",
        10
    ),
    ?assertFormat(
        "A =\n"
        "    B = C = D = E = F\n.",
        "A =\n"
        "    B =\n"
        "    C =\n"
        "    D =\n"
        "    E = F.\n",
        10
    ),
    ?assertFormat(
        "A = B = c(D, E).\n",
        "A =\n"
        "    B = c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n",
        5
    ),
    ?assertSame(
        "A =\n"
        "    B = c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n"
    ),
    ?assertSame(
        "A =\n"
        "    B =\n"
        "    c(\n"
        "        D,\n"
        "        E\n"
        "    ) = f(\n"
        "        G\n"
        "    ).\n"
    ),
    ?assertSame(
        "A ::\n"
        "    B :: c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n"
    ),
    ?assertFormat(
        "\"abc\" ++\n"
        "    B ++ c(\n"
        "        A,\n"
        "        B\n"
        "    ).\n",
        "\"abc\" ++\n"
        "    B ++\n"
        "    c(\n"
        "        A,\n"
        "        B\n"
        "    ).\n"
    ),
    ?assertFormat(
        "\"abc\" ++\n"
        "    B = c(\n"
        "        A,\n"
        "        B\n"
        "    ).\n",
        "\"abc\" ++\n"
        "    B = c(\n"
        "    A,\n"
        "    B\n"
        ").\n"
    ),
    ?assertFormat(
        "\"abc\" =\n"
        "    B ++ c(\n"
        "        A,\n"
        "        B\n"
        "    ).\n",
        "\"abc\" =\n"
        "    B ++\n"
        "        c(\n"
        "            A,\n"
        "            B\n"
        "        ).\n"
    ),
    ?assertFormat(
        "\"abc\" =\n"
        "    B ++ c(\n"
        "        A,\n"
        "        B\n"
        "    ) ++ f(\n"
        "        G\n"
        "    ).\n",
        "\"abc\" =\n"
        "    B ++\n"
        "        c(\n"
        "            A,\n"
        "            B\n"
        "        ) ++\n"
        "        f(\n"
        "            G\n"
        "        ).\n"
    ),
    ?assertSame(
        "A =\n"
        "    B = #c{\n"
        "        d = D,\n"
        "        e = E\n"
        "    }.\n"
    ),
    ?assertSame(
        "A =\n"
        "    B =\n"
        "    %% comment\n"
        "    c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n"
    ),
    ?assertSame(
        "A =\n"
        "    %% comment\n"
        "    B =\n"
        "    %% comment\n"
        "    c(\n"
        "        D,\n"
        "        E\n"
        "    ).\n"
    ).

tuple(Config) when is_list(Config) ->
    ?assertFormat("{ }", "{}\n"),
    ?assertFormat("{1,2}", "{1, 2}\n"),
    ?assertFormat("{1,{2,3}}", "{1, {2, 3}}\n"),
    ?assertFormat(
        "{verylong,{22,33,44}}",
        "{verylong,\n"
        "    {22, 33, 44}}\n",
        20
    ),
    ?assertFormat(
        "{long,x,{22,33,44},y}",
        "{long, x,\n"
        "    {22, 33, 44}, y}\n",
        20
    ),
    ?assertFormat(
        "{verylong,x,{22,33,44},y,{55,66,77},z}",
        "{verylong, x,\n"
        "    {22, 33, 44}, y,\n"
        "    {55, 66, 77}, z}\n",
        20
    ),
    ?assertFormat(
        "{{a,long,tuple},{nested,1,long,tuple}}",
        "{\n"
        "    {a, long,\n"
        "        tuple},\n"
        "    {nested, 1,\n"
        "        long, tuple}\n"
        "}\n",
        20
    ),
    ?assertFormat(
        "{{a,long,tuple},[nested,1,long,list]}",
        "{{a, long, tuple}, [\n"
        "    nested,\n"
        "    1,\n"
        "    long,\n"
        "    list\n"
        "]}\n",
        20
    ),
    ?assertFormat(
        "{{a,long,tuple},{another,tuple},[nested,1,long,list]}",
        "{\n"
        "    {a, long, tuple},\n"
        "    {another, tuple},\n"
        "    [\n"
        "        nested,\n"
        "        1,\n"
        "        long,\n"
        "        list\n"
        "    ]\n"
        "}\n",
        25
    ),
    ?assertFormat(
        "{{a,long,tuple},bare,atoms,[nested,long,list],bare,atoms,this_does_not_fit}",
        "{\n"
        "    {a, long,\n"
        "        tuple},\n"
        "    bare,\n"
        "    atoms,\n"
        "    [\n"
        "        nested,\n"
        "        long,\n"
        "        list\n"
        "    ],\n"
        "    bare,\n"
        "    atoms,\n"
        "    this_does_not_fit\n"
        "}\n",
        20
    ),
    ?assertFormat(
        "{short, {a,long,tuple}}",
        "{short,\n"
        "    {a, long,\n"
        "        tuple}}\n",
        15
    ),
    ?assertSame(
        "{\n"
        "    foo\n"
        "}\n"
    ),
    ?assertSame(
        "{\n"
        "    foo, bar, baz\n"
        "}\n"
    ),
    ?assertSame(
        "{\n"
        "    foo,\n"
        "    bar,\n"
        "    baz\n"
        "}\n"
    ),
    ?assertFormat(
        "{\n"
        "    foo, bar,\n"
        "    baz\n"
        "}\n",
        "{\n"
        "    foo,\n"
        "    bar,\n"
        "    baz\n"
        "}\n"
    ),
    ?assertFormat(
        "{foo\n"
        " %% bar\n"
        "}",
        "{\n"
        "    foo\n"
        "    %% bar\n"
        "}\n"
    ),
    ?assertFormat(
        "{true, lists:reverse(Acc, [concat(force_breaks(), line(expr_to_algebra(Value), comments_to_algebra(Comments)))])}",
        "{true,\n"
        "    lists:reverse(Acc, [\n"
        "        concat(\n"
        "            force_breaks(),\n"
        "            line(expr_to_algebra(Value), comments_to_algebra(Comments))\n"
        "        )\n"
        "    ])}\n"
    ).

%% tagged vs untagged tuples
untagged_tuple(Config) when is_list(Config) ->
    ?assertSame(
        "{foo(A), B}.\n"
    ),
    ?assertFormat(
        "{ajshdjkasdjkasdhkafoo(\n"
        "    A\n"
        "), abc}.\n",
        "{\n"
        "    ajshdjkasdjkasdhkafoo(\n"
        "        A\n"
        "    ),\n"
        "    abc\n"
        "}.\n"
    ),
    ?assertFormat(
        "{a(\n"
        "    A\n"
        "), abc}.\n",
        "{\n"
        "    a(\n"
        "        A\n"
        "    ),\n"
        "    abc\n"
        "}.\n"
    ),
    ?assertFormat(
        "{#{\n"
        "    a => b\n"
        "}, abc}.\n",
        "{\n"
        "    #{\n"
        "        a => b\n"
        "    },\n"
        "    abc\n"
        "}.\n"
    ),
    ?assertFormat(
        "{[\n"
        "    A\n"
        "], abc}.\n",
        "{\n"
        "    [\n"
        "        A\n"
        "    ],\n"
        "    abc\n"
        "}.\n"
    ),
    ?assertFormat(
        "{{\n"
        "    A\n"
        "}, abc}.\n",
        "{\n"
        "    {\n"
        "        A\n"
        "    },\n"
        "    abc\n"
        "}.\n"
    ),
    ?assertFormat(
        "{{A,\n"
        "[\n"
        "A\n"
        "]},\n"
        "abc}.\n",
        "{\n"
        "    {A, [\n"
        "        A\n"
        "    ]},\n"
        "    abc\n"
        "}.\n"
    ),
    ?assertFormat(
        "{\n"
        "{A,\n"
        "[\n"
        "A\n"
        "]},\n"
        "abc}.\n",
        "{\n"
        "    {A, [\n"
        "        A\n"
        "    ]},\n"
        "    abc\n"
        "}.\n"
    ),
    ?assertFormat(
        "{{A, B,\n"
        "% a\n"
        "    C\n"
        "}}.\n",
        "{{A, B,\n"
        "    % a\n"
        "    C}}.\n"
    ),
    ?assertFormat(
        "{\n"
        "{A, B,\n"
        "% a\n"
        "    C\n"
        "}}.\n",
        "{\n"
        "    {A, B,\n"
        "        % a\n"
        "        C}\n"
        "}.\n"
    ),
    ?assertFormat(
        "{ajdsdjhasd, askjdasjkd, [\n"
        "A, B\n"
        "], c}.\n",
        "{ajdsdjhasd, askjdasjkd,\n"
        "    [\n"
        "        A, B\n"
        "    ],\n"
        "    c}.\n"
    ),
    ?assertFormat(
        "{ajdsdjhasd, askjdasjkd, [\n"
        "A,\n"
        "B\n"
        "], c}.\n",
        "{ajdsdjhasd, askjdasjkd,\n"
        "    [\n"
        "        A,\n"
        "        B\n"
        "    ],\n"
        "    c}.\n"
    ),
    ?assertFormat(
        "{A,\n"
        "%foo\n"
        "B,\n"
        "[\n"
        "A,\n"
        "B\n"
        "],\n"
        "c}.\n",
        "{A,\n"
        "    %foo\n"
        "    B,\n"
        "    [\n"
        "        A,\n"
        "        B\n"
        "    ],\n"
        "    c}.\n"
    ),
    ?assertFormat(
        "{foo(A),\n"
        "[\n"
        "A,\n"
        "B]}.\n",
        "{foo(A), [\n"
        "    A,\n"
        "    B\n"
        "]}.\n"
    ),
    ?assertFormat(
        "{foo(A),\n"
        "    [\n"
        "        A,\n"
        "        B\n"
        "    ],\n"
        "ghi}.\n",
        "{\n"
        "    foo(A),\n"
        "    [\n"
        "        A,\n"
        "        B\n"
        "    ],\n"
        "    ghi\n"
        "}.\n"
    ),
    ?assertFormat(
        "{foo(A), aksjhdaksjhdas, askjdhaskjdhaskjdhas, askjhdaskjdhkajshdas, askjdhakjhdkjashd,\n"
        "    askjhdasjkhdjsakhd}.\n",
        "{\n"
        "    foo(A),\n"
        "    aksjhdaksjhdas,\n"
        "    askjdhaskjdhaskjdhas,\n"
        "    askjhdaskjdhkajshdas,\n"
        "    askjdhakjhdkjashd,\n"
        "    askjhdasjkhdjsakhd\n"
        "}.\n"
    ),
    ?assertFormat(
        "{foo(), ajkshdjkasdhas, aksjhdakjdhkasj, askjhdajkshdkajsdh, bkadajkshdkasjhdaskh,\n"
        "    asjdhakjshdaskjhdask}.\n",
        "{\n"
        "    foo(),\n"
        "    ajkshdjkasdhas,\n"
        "    aksjhdakjdhkasj,\n"
        "    askjhdajkshdkajsdh,\n"
        "    bkadajkshdkasjhdaskh,\n"
        "    asjdhakjshdaskjhdask\n"
        "}.\n"
    ),
    ?assertFormat(
        "{?FOO,\n"
        "    [\n"
        "A,\n"
        "B\n"
        "]}.\n",
        "{?FOO, [\n"
        "    A,\n"
        "    B\n"
        "]}.\n"
    ),
    ?assertFormat(
        "{[],\n"
        "[\n"
        "A,\n"
        "B\n"
        "]}.\n",
        "{[], [\n"
        "    A,\n"
        "    B\n"
        "]}.\n"
    ),
    ?assertSame(
        "{[\n"
        "    A,\n"
        "    B\n"
        "]}.\n"
    ),
    ?assertSame(
        "{{A,\n"
        "    [\n"
        "        A,\n"
        "        B\n"
        "    ],\n"
        "    B}}.\n"
    ),
    ?assertFormat(
        "foo(A, B, {foo(A),\n"
        "%foo\n"
        "A,\n"
        "B\n"
        "}).\n",
        "foo(A, B, {\n"
        "    foo(A),\n"
        "    %foo\n"
        "    A,\n"
        "    B\n"
        "}).\n"
    ),
    ?assertFormat(
        "{<<\"foo\">>,\n"
        "[\n"
        "A,\n"
        "B\n"
        "], a}.\n",
        "{<<\"foo\">>,\n"
        "    [\n"
        "        A,\n"
        "        B\n"
        "    ],\n"
        "    a}.\n"
    ),
    ?assertFormat(
        "<<\"abc\"\n"
        "    \"def\">>.\n",
        "<<\n"
        "    \"abc\"\n"
        "    \"def\"\n"
        ">>.\n"
    ),
    ?assertFormat(
        "{<<\"foo\"\n"
        "   \"bar\">>, a}.\n",
        "{\n"
        "    <<\n"
        "        \"foo\"\n"
        "        \"bar\"\n"
        "    >>,\n"
        "    a\n"
        "}.\n"
    ),
    ?assertFormat(
        "foo({{A,\n"
        "    %foo\n"
        "    B}}).\n",
        "foo(\n"
        "    {{A,\n"
        "        %foo\n"
        "        B}}\n"
        ").\n"
    ),
    ?assertSame(
        "<<\"aaaaa\">>\n",
        5
    ),
    ?assertFormat(
        "{{<<\"key3\">>}, {[{<<\"true\">>, true}, {<<\"false\">>, false}, {<<\"nullqweqwewqdqwqdwqdq\">>, null}, {<<\"numberqweqwrqwdqd\">>, 1}]}}.\n",
        "{\n"
        "    {<<\"key3\">>},\n"
        "    {[\n"
        "        {<<\"true\">>, true},\n"
        "        {<<\"false\">>, false},\n"
        "        {<<\"nullqweqwewqdqwqdwqdq\">>, null},\n"
        "        {<<\"numberqweqwrqwdqd\">>, 1}\n"
        "    ]}\n"
        "}.\n"
    ).

list(Config) when is_list(Config) ->
    ?assertFormat("[\n]", "[]\n"),
    ?assertSame("[1 | [2 | []]]\n"),
    ?assertFormat("[ 1 ,2,3, 4]", "[1, 2, 3, 4]\n"),
    ?assertFormat("[1,2,3|4]", "[1, 2, 3 | 4]\n"),
    ?assertFormat(
        "[long,[2,3,4]]",
        "[\n"
        "    long,\n"
        "    [2, 3, 4]\n"
        "]\n",
        15
    ),
    ?assertFormat(
        "[\n"
        "long,[2,3,4]]",
        "[\n"
        "    long,\n"
        "    [2, 3, 4]\n"
        "]\n",
        15
    ),
    ?assertFormat(
        "[11,2|3]",
        "[\n"
        "    11,\n"
        "    2\n"
        "    | 3\n"
        "]\n",
        10
    ),
    ?assertFormat(
        "[11,22|33]",
        "[\n"
        "    11,\n"
        "    22\n"
        "    | 33\n"
        "]\n",
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
        "]\n",
        15
    ),
    ?assertFormat(
        "[1, begin 2 end, 3]",
        "[\n"
        "    1,\n"
        "    begin\n"
        "        2\n"
        "    end,\n"
        "    3\n"
        "]\n"
    ),
    ?assertFormat(
        "[ 1\n"
        ", 2\n"
        "\n"
        ", 3\n"
        "]\n",
        "[\n"
        "    1,\n"
        "    2,\n"
        "\n"
        "    3\n"
        "]\n"
    ),
    ?assertSame(
        "[\n"
        "    1\n"
        "    | 2\n"
        "]\n"
    ),
    ?assertFormat(
        "gen_part_decode_funcs({constructed,bif}, TypeName, {_Name,parts,Tag,_Type}) ->\n"
        "   emit([\n"
        "       [\"  case Data of\",nl],\n"
        "       [\"    L when is_list(L) ->\",nl],\n"
        "       [\"      'dec_\",TypeName,\"'(lists:map(fun(X) -> element(1, \"],\n"
        "       [{call,ber,ber_decode_erlang,[\"X\"]},\") end, L),\",{asis,Tag},\");\",nl],\n"
        "       [\"    _ ->\",nl],\n"
        "       [\"      [Res] = 'dec_\",TypeName,\"'([Data],\",{asis,Tag},\"),\",nl],\n"
        "       [\"      Res\",nl],\n"
        "       [\"  end\"]\n"
        "   ]).\n",
        "gen_part_decode_funcs({constructed, bif}, TypeName, {_Name, parts, Tag, _Type}) ->\n"
        "    emit([\n"
        "        [\"  case Data of\", nl],\n"
        "        [\"    L when is_list(L) ->\", nl],\n"
        "        [\"      'dec_\", TypeName, \"'(lists:map(fun(X) -> element(1, \"],\n"
        "        [{call, ber, ber_decode_erlang, [\"X\"]}, \") end, L),\", {asis, Tag}, \");\", nl],\n"
        "        [\"    _ ->\", nl],\n"
        "        [\"      [Res] = 'dec_\", TypeName, \"'([Data],\", {asis, Tag}, \"),\", nl],\n"
        "        [\"      Res\", nl],\n"
        "        [\"  end\"]\n"
        "    ]).\n",
        92
    ),
    ?assertSame(
        "[\n"
        "    Head\n"
        "    % comment before cons\n"
        "    | Rest\n"
        "].\n"
    ),
    ?assertFormat(
        "[\n"
        "    Head\n"
        "    |\n"
        "    % comment after cons\n"
        "    Rest\n"
        "].\n",
        "[\n"
        "    Head\n"
        "    % comment after cons\n"
        "    | Rest\n"
        "].\n"
    ),
    ?assertSame(
        "[\n"
        "    % comment before head\n"
        "    {an, element}\n"
        "    % comment after pipe\n"
        "    % comment below\n"
        "    | Rest\n"
        "].\n"
    ),
    ?assertFormat(
        "[\n"
        "    % comment before head\n"
        "    {an, element} |\n"
        "    % comment after pipe\n"
        "    % comment below\n"
        "    Rest\n"
        "].\n",
        "[\n"
        "    % comment before head\n"
        "    {an, element}\n"
        "    % comment after pipe\n"
        "    % comment below\n"
        "    | Rest\n"
        "].\n"
    ),
    ?assertFormat(
        "[\n"
        "    % comment before head\n"
        "    {an, element}\n"
        "    % comment before pipe\n"
        "    |\n"
        "    % comment below\n"
        "    Rest\n"
        "].\n",
        "[\n"
        "    % comment before head\n"
        "    {an, element}\n"
        "    % comment before pipe\n"
        "\n"
        "    % comment below\n"
        "    | Rest\n"
        "].\n"
    ).

binary(Config) when is_list(Config) ->
    ?assertFormat("<< >>", "<<>>\n"),
    ?assertSame(
        "<<(1 + 1), (#{}), (#foo{}), (#{}#{}), (#foo{}#foo{}), (#foo.bar), (call())>>\n"
    ),
    ?assertSame("<<(1), 1>>\n"),
    ?assertSame("<<+1:5/integer-unit:8>>\n"),
    ?assertSame("<<\"żółć\"/utf8>>\n"),
    ?assertFormat("<<1/float,<<22,33>>/binary>>", "<<1/float, <<22, 33>>/binary>>\n"),
    ?assertFormat(
        "<<1/float,<<222,333>>>>",
        "<<1/float,\n"
        "    <<222,\n"
        "        333>>>>\n",
        15
    ),
    ?assertSame(
        "<<\n"
        "    1/float,\n"
        "    <<222, 333>>\n"
        ">>\n"
    ),
    ?assertSame(
        "<<\n"
        "    1/float,\n"
        "    <<\n"
        "        222,\n"
        "        333\n"
        "    >>\n"
        ">>\n"
    ).

map_create(Config) when is_list(Config) ->
    ?assertFormat("#{\n}", "#{}\n"),
    ?assertFormat("#{1:=2,  3=>4}", "#{1 := 2, 3 => 4}\n"),
    ?assertFormat(
        "#{11 => 22, 33 => 44}",
        "#{\n"
        "    11 => 22,\n"
        "    33 => 44\n"
        "}\n",
        15
    ),
    ?assertFormat(
        "#{11 => 22, 33 => 44}",
        "#{\n"
        "    11 =>\n"
        "        22,\n"
        "    33 =>\n"
        "        44\n"
        "}\n",
        10
    ),
    ?assertSame(
        "#{\n"
        "    1 => [\n"
        "        2\n"
        "    ],\n"
        "    3 => 4\n"
        "}\n"
    ),
    ?assertSame(
        "#{\n"
        "    ?FOO\n"
        "}\n"
    ).

map_update(Config) when is_list(Config) ->
    ?assertFormat("X # {\n}", "X#{}\n"),
    ?assertSame("#{}#{}\n"),
    ?assertSame("#{}#{}#{}\n"),
    ?assertSame("(X#foo.bar)#{}\n"),
    ?assertSame("(catch 1)#{}\n"),
    ?assertSame("X#{A => B, C := D}\n"),
    ?assertFormat(
        "X#{11 => 22, 33 => 44}",
        "X#{\n"
        "    11 => 22,\n"
        "    33 => 44\n"
        "}\n",
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
        "}\n",
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
        "}\n"
    ).

record_create(Config) when is_list(Config) ->
    ?assertFormat("#foo{\n}", "#foo{}\n"),
    ?assertFormat("#foo{_=x}", "#foo{_ = x}\n"),
    ?assertFormat("#foo{a=1,b=2+3}", "#foo{a = 1, b = 2 + 3}\n"),
    ?assertFormat(
        "#foo{a=1,b=2+3}",
        "#foo{\n"
        "    a = 1,\n"
        "    b = 2 + 3\n"
        "}\n",
        15
    ),
    ?assertFormat(
        "#foo{a=1,b=Foo+Bar}",
        "#foo{\n"
        "    a = 1,\n"
        "    b =\n"
        "        Foo +\n"
        "            Bar\n"
        "}\n",
        15
    ),
    ?assertSame("#?FOO{}\n"),
    ?assertSame("?FOO{}\n"),
    ?assertSame(
        "#foo{\n"
        "    a = [\n"
        "        b\n"
        "    ]\n"
        "}\n"
    ),
    ?assertSame(
        "#foo{\n"
        "    long_key = [\n"
        "        long_value\n"
        "    ]\n"
        "}\n",
        20
    ),
    ?assertSame(
        "#foo{\n"
        "    a = 1,\n"
        "    b = 1,\n"
        "\n"
        "    c = 2\n"
        "}\n",
        15
    ),
    ?assertSame(
        "#foo{\n"
        "    a = 1,\n"
        "\n"
        "    % comment\n"
        "    b = 2,\n"
        "    % a non splitting comment\n"
        "    c = 3\n"
        "}\n",
        15
    ).

record_update(Config) when is_list(Config) ->
    ?assertFormat("X #foo {\n}", "X#foo{}\n"),
    ?assertSame("#foo{}#bar{}\n"),
    ?assertSame("#foo{}#bar{}#baz{}\n"),
    ?assertSame("X#foo.bar#baz{}\n"),
    ?assertSame("(catch 1)#foo{}\n"),
    ?assertFormat(
        "X#foo{aa = aa, bb = bb}",
        "X#foo{\n"
        "    aa = aa,\n"
        "    bb = bb\n"
        "}\n",
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
        "}\n",
        15
    ),
    ?assertSame("X#?FOO{}\n"),
    ?assertSame("X?FOO{}\n").

record_index(Config) when is_list(Config) ->
    ?assertSame("#foo.bar\n"),
    ?assertSame("#?FOO.bar\n"),
    ?assertSame("?FOO.bar\n").

record_field(Config) when is_list(Config) ->
    ?assertSame("X#foo.bar\n"),
    ?assertSame("X#foo.bar#baz.bak\n"),
    ?assertSame("(catch X)#foo.bar\n"),
    ?assertFormat(
        "(Foo + Bar)#foo.bar",
        "(Foo +\n"
        "    Bar)#foo.bar\n",
        5
    ),
    ?assertSame("X#?FOO.bar\n"),
    ?assertSame("X?FOO.bar\n").

force_break(Config) when is_list(Config) ->
    ?assertSame(
        "[\n"
        "    %% foo\n"
        "]\n"
    ),
    ?assertFormat(
        "[1, 2\n"
        "    %% foo\n"
        "]",
        "[\n"
        "    1,\n"
        "    2\n"
        "    %% foo\n"
        "]\n"
    ),
    ?assertFormat(
        "[x\n"
        "]",
        "[x]\n"
    ),
    ?assertFormat(
        "[\n"
        " x]",
        "[\n"
        "    x\n"
        "]\n"
    ),
    ?assertFormat(
        "{x\n"
        "}",
        "{x}\n"
    ),
    ?assertFormat(
        "{\n"
        " x}",
        "{\n"
        "    x\n"
        "}\n"
    ),
    ?assertFormat(
        "<<x\n"
        ">>",
        "<<x>>\n"
    ),
    ?assertFormat(
        "<<\n"
        " x>>",
        "<<\n"
        "    x\n"
        ">>\n"
    ),
    ?assertFormat(
        "#{x => y\n"
        "}",
        "#{x => y}\n"
    ),
    ?assertFormat(
        "X#{\n"
        " x => y}",
        "X#{\n"
        "    x => y\n"
        "}\n"
    ),
    ?assertFormat(
        "X#{x => y\n"
        "}",
        "X#{x => y}\n"
    ),
    ?assertFormat(
        "X#{\n"
        " x => y}",
        "X#{\n"
        "    x => y\n"
        "}\n"
    ),
    ?assertFormat(
        "#foo{x = 1\n"
        "}",
        "#foo{x = 1}\n"
    ),
    ?assertFormat(
        "#foo{\n"
        " x = 1}",
        "#foo{\n"
        "    x = 1\n"
        "}\n"
    ),
    ?assertFormat(
        "X#foo{x = 1\n"
        "}",
        "X#foo{x = 1}\n"
    ),
    ?assertFormat(
        "X#foo{\n"
        " x = 1}",
        "X#foo{\n"
        "    x = 1\n"
        "}\n"
    ),
    ?assertFormat(
        "foo(x\n"
        ")",
        "foo(x)\n"
    ),
    ?assertFormat(
        "foo(\n"
        " x)",
        "foo(\n"
        "    x\n"
        ")\n"
    ),
    ?assertFormat(
        "foo(x\n"
        ") -> x.",
        "foo(x) -> x.\n"
    ),
    ?assertFormat(
        "foo(\n"
        " x) -> x.",
        "foo(\n"
        "    x\n"
        ") ->\n"
        "    x.\n"
    ),
    ?assertFormat(
        "foo(1) -> x;\n"
        "foo(2) ->\n"
        "    y.",
        "foo(1) -> x;\n"
        "foo(2) -> y.\n"
    ),
    ?assertFormat(
        "foo(1) ->\n"
        "    x;\n"
        "foo(2) -> y.",
        "foo(1) ->\n"
        "    x;\n"
        "foo(2) ->\n"
        "    y.\n"
    ).

update_edgecase(Config) when is_list(Config) ->
    ?assertFormat("2 #{3 := 4}\n", "(2)#{3 := 4}\n"),
    ?assertFormat("2 #record.field\n", "(2)#record.field\n"),
    ?assertFormat("2 #record{}\n", "(2)#record{}\n").

list_comprehension(Config) when is_list(Config) ->
    ?assertFormat("[X||X<-List]", "[X || X <- List]\n"),
    ?assertSame("[X || {X, Y} <- Results, X >= Y]\n"),
    ?assertSame("[X || X := Y <- ResultMap, X >= Y]\n"),
    ?assertSame("[X || <<X, Y>> <= Results, X >= Y]\n"),
    ?assertFormat(
        "[[Very, Long, Expression] || X <- Y, X < 10]",
        "[\n"
        "    [\n"
        "        Very,\n"
        "        Long,\n"
        "        Expression\n"
        "    ]\n"
        " || X <- Y, X < 10\n"
        "]\n",
        25
    ),
    ?assertFormat(
        "[X || X <- Y, X < 10\n"
        " % trailing comment\n"
        "]",
        "[\n"
        "    X\n"
        " || X <- Y,\n"
        "    X < 10\n"
        "    % trailing comment\n"
        "]\n"
    ),
    ?assertFormat(
        "[{Very, Long, Expression} || X <- Y, X < 10]",
        "[\n"
        "    {Very, Long,\n"
        "        Expression}\n"
        " || X <- Y, X < 10\n"
        "]\n",
        25
    ),
    ?assertFormat(
        "[X || X <- LongExpr123, X < 10]",
        "[\n"
        "    X\n"
        " || X <- LongExpr123,\n"
        "    X < 10\n"
        "]\n",
        25
    ),
    ?assertFormat(
        "[X || X <- VeryLongExpression, X < 10]",
        "[\n"
        "    X\n"
        " || X <-\n"
        "        VeryLongExpression,\n"
        "    X < 10\n"
        "]\n",
        25
    ),
    ?assertFormat(
        "lists:unzip([{ALong, B}|| ALong = {_, _, _, {B, _}} <- All, lists:member(B, Keep)])",
        "lists:unzip([\n"
        "    {ALong, B}\n"
        " || ALong = {_, _, _, {B, _}} <- All, lists:member(B, Keep)\n"
        "])\n"
    ),
    ?assertSame(
        "[\n"
        "    a\n"
        " || {a, b} <- es\n"
        "].\n"
    ),
    ?assertSame(
        "[\n"
        "    X\n"
        " || true, true, true\n"
        "].\n"
    ),
    ?assertSame(
        "A = [\n"
        "    a\n"
        " || {a, b} <- es,\n"
        "    filter(b)\n"
        "]\n"
    ),
    ?assertFormat(
        "string:equal([Value || {string, _, Value} <- ValuesL], [Value || {string, _, Value} <- ValuesR]).\n",
        "string:equal([Value || {string, _, Value} <- ValuesL], [\n"
        "    Value\n"
        " || {string, _, Value} <- ValuesR\n"
        "]).\n"
    ).

map_comprehension(Config) when is_list(Config) ->
    ?assertFormat("#{X=>X||X<-List}", "#{X => X || X <- List}\n"),
    ?assertFormat("#{X=>Y||X:=Y<-Map}", "#{X => Y || X := Y <- Map}\n"),
    ?assertSame("#{X => X || {X, Y} <- Results, X >= Y}\n"),
    ?assertSame("#{X => X || <<X, Y>> <= Results, X >= Y}\n"),
    ?assertFormat(
        "#{[Very, Long, Expression] => X || X <- Y, X < 10}",
        "#{\n"
        "    [\n"
        "        Very,\n"
        "        Long,\n"
        "        Expression\n"
        "    ] => X\n"
        " || X <- Y, X < 10\n"
        "}\n",
        25
    ),
    ?assertFormat(
        "#{X => X || X <- Y, X < 10\n"
        " % trailing comment\n"
        "}",
        "#{\n"
        "    X => X\n"
        " || X <- Y,\n"
        "    X < 10\n"
        "    % trailing comment\n"
        "}\n"
    ),
    ?assertFormat(
        "#{{Very, Long, Expression} => X || X <- Y, X < 10}",
        "#{\n"
        "    {Very, Long,\n"
        "        Expression} => X\n"
        " || X <- Y, X < 10\n"
        "}\n",
        25
    ),
    ?assertFormat(
        "#{X => X || X <- LongExpr123, X < 10}",
        "#{\n"
        "    X => X\n"
        " || X <- LongExpr123,\n"
        "    X < 10\n"
        "}\n",
        25
    ),
    ?assertFormat(
        "#{X => X || X <- VeryLongExpression, X < 10}",
        "#{\n"
        "    X => X\n"
        " || X <-\n"
        "        VeryLongExpression,\n"
        "    X < 10\n"
        "}\n",
        25
    ),
    ?assertFormat(
        "#{X => X || X := VeryLongExpression <- VeryLongExpression, X < 10}",
        "#{\n"
        "    X => X\n"
        " || X :=\n"
        "        VeryLongExpression <-\n"
        "        VeryLongExpression,\n"
        "    X < 10\n"
        "}\n",
        25
    ).

binary_comprehension(Config) when is_list(Config) ->
    ?assertFormat("<<X||X<-List>>", "<<X || X <- List>>\n"),
    ?assertSame("<<X || <<X, Y>> <= Results, X >= Y>>\n"),
    ?assertSame("<<X || X := Y <- ResultMap, X >= Y>>\n"),
    ?assertSame(
        "<<\n"
        "    X\n"
        " || <<X, Y>> <= Results,\n"
        "    X >= Y\n"
        ">>\n"
    ),
    ?assertFormat(
        "<<(Long + Expression) || X <- Y, X < 10>>",
        "<<\n"
        "    (Long + Expression)\n"
        " || X <- Y, X < 10\n"
        ">>\n",
        25
    ),
    ?assertFormat(
        "<<X || <<X>> <= LongExpr123, X < 10>>",
        "<<\n"
        "    X\n"
        " || <<X>> <= LongExpr123,\n"
        "    X < 10\n"
        ">>\n",
        25
    ),
    ?assertFormat(
        "<<X || <<X>> <= VeryLongExpression, X < 10>>",
        "<<\n"
        "    X\n"
        " || <<X>> <=\n"
        "        VeryLongExpression,\n"
        "    X < 10\n"
        ">>\n",
        25
    ),
    ?assertFormat(
        "lists:unzip(<<<<ALong, B>>|| ALong = {_, _, _, {B, _}} <- All, lists:member(B, Keep)>>)",
        "lists:unzip(<<\n"
        "    <<ALong, B>>\n"
        " || ALong = {_, _, _, {B, _}} <- All, lists:member(B, Keep)\n"
        ">>)\n"
    ).

call(Config) when is_list(Config) ->
    ?assertFormat("foo(\n)", "foo()\n"),
    ?assertSame("foo(1, 2, 3)\n"),
    ?assertSame("foo:bar(1, 2, 3)\n"),
    ?assertSame("Foo:Bar(1, 2, 3)\n"),
    ?assertSame("(get_module()):(get_fun())()\n"),
    ?assertFormat(
        "long_name({Long, Expression})",
        "long_name(\n"
        "    {Long, Expression}\n"
        ")\n",
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
        ")\n",
        20
    ),
    ?assertFormat(
        "very_very_long_name({Very, Long, Expression})",
        "very_very_long_name(\n"
        "    {Very, Long,\n"
        "        Expression}\n"
        ")\n",
        20
    ),
    ?assertFormat(
        "long_name({Long, Expression}, AnotherArgument)",
        "long_name(\n"
        "    {Long, Expression},\n"
        "    AnotherArgument\n"
        ")\n",
        25
    ),
    ?assertFormat(
        "long_name(Arg, [Very, Long, Expression])",
        "long_name(Arg, [\n"
        "    Very,\n"
        "    Long,\n"
        "    Expression\n"
        "])\n",
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
        ")\n"
    ),
    ?assertSame(
        "?MAKE_FUN(\n"
        "    foo\n"
        ")(\n"
        "    bar\n"
        ").\n"
    ),
    ?assertFormat(
        "render(fold_doc(fun(D, Acc) -> concat([D,X,Acc]) end, [A,B,C]),80)",
        "render(\n"
        "    fold_doc(fun(D, Acc) -> concat([D, X, Acc]) end, [\n"
        "        A, B, C\n"
        "    ]),\n"
        "    80\n"
        ")\n",
        60
    ),
    ?assertFormat(
        "long_name(Long, Arguments)",
        "long_name(\n"
        "    Long, Arguments\n"
        ")\n",
        25
    ),
    ?assertSame(
        "long_name(\n"
        "    Long, Arguments\n"
        ")\n"
    ),
    ?assertFormat(
        "long_name(\n"
        "    Long, Arguments\n"
        ")\n",
        "long_name(\n"
        "    Long,\n"
        "    Arguments\n"
        ")\n",
        10
    ),
    ?assertSame(
        "long_name(\n"
        "    Long,\n"
        "    Arguments\n"
        ")\n"
    ).

block(Config) when is_list(Config) ->
    ?assertFormat(
        "begin 1 end",
        "begin\n"
        "    1\n"
        "end\n"
    ),
    ?assertSame(
        "begin\n"
        "    1\n"
        "end\n"
    ),
    ?assertFormat(
        "begin 1, 2 end",
        "begin\n"
        "    1,\n"
        "    2\n"
        "end\n"
    ),
    ?assertFormat(
        "begin long_expression(with_args), {Short, Expr} end",
        "begin\n"
        "    long_expression(\n"
        "        with_args\n"
        "    ),\n"
        "    {Short, Expr}\n"
        "end\n",
        25
    ),
    ?assertSame(
        "begin\n"
        "    foo(),\n"
        "\n"
        "    bar()\n"
        "end\n"
    ).

fun_expression(Config) when is_list(Config) ->
    ?assertSame("fun foo/1\n"),
    ?assertSame("fun Mod:Name/Arity\n"),
    ?assertSame("fun() -> ok end\n"),
    ?assertSame("fun(X) when is_integer(X) -> X end\n"),
    ?assertSame("fun Foo() -> Foo() end\n"),
    ?assertFormat(
        "fun()->\n"
        "ok end",
        "fun() ->\n"
        "    ok\n"
        "end\n"
    ),
    ?assertFormat(
        "fun (x) -> x; (y) -> y end",
        "fun\n"
        "    (x) -> x;\n"
        "    (y) -> y\n"
        "end\n",
        100
    ),
    ?assertFormat(
        "fun (Long) -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Long) -> Expression;\n"
        "    (ok) -> ok\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "fun (Even, Longer) -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Even, Longer) ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "fun (Even, Longer) when Guarded -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Even, Longer) when\n"
        "        Guarded\n"
        "    ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end\n",
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
        "end\n",
        25
    ),
    ?assertFormat(
        "fun (Long, Pattern) when VeryLongGuard; Is, Very, Long -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Long, Pattern) when\n"
        "        VeryLongGuard;\n"
        "        Is, Very, Long\n"
        "    ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end\n",
        30
    ),
    ?assertFormat(
        "fun (Long, Pattern) when VeryLongGuard; Is, Even, Loooooooonger -> Expression; (ok) -> ok end",
        "fun\n"
        "    (Long, Pattern) when\n"
        "        VeryLongGuard;\n"
        "        Is,\n"
        "        Even,\n"
        "        Loooooooonger\n"
        "    ->\n"
        "        Expression;\n"
        "    (ok) ->\n"
        "        ok\n"
        "end\n",
        30
    ),
    ?assertFormat(
        "fun (Long) -> Expression end",
        "fun(Long) ->\n"
        "    Expression\n"
        "end\n",
        20
    ),
    ?assertFormat(
        "fun (Even, Longer) -> Expression end",
        "fun(Even, Longer) ->\n"
        "    Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "fun (Even, Longer) when Guarded -> Expression end",
        "fun(Even, Longer) when\n"
        "    Guarded\n"
        "->\n"
        "    Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "fun (The, Longest, Pattern) when Guarded -> Expression end",
        "fun(\n"
        "    The,\n"
        "    Longest,\n"
        "    Pattern\n"
        ") when Guarded ->\n"
        "    Expression\n"
        "end\n",
        20
    ),
    ?assertFormat(
        "fun (Long, Pattern) when LongerGuard; Is, Very, Long -> Expression end",
        "fun(Long, Pattern) when\n"
        "    LongerGuard;\n"
        "    Is, Very, Long\n"
        "->\n"
        "    Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "fun (Long, Pattern) when VeryLongGuard; Is, Even, Loooooooonger -> Expression end",
        "fun(Long, Pattern) when\n"
        "    VeryLongGuard;\n"
        "    Is,\n"
        "    Even,\n"
        "    Loooooooonger\n"
        "->\n"
        "    Expression\n"
        "end\n",
        25
    ),
    ?assertSame(
        "fun() ->\n"
        "    %% comment 4\n"
        "    ok\n"
        "%% comment 5\n"
        "end.\n"
    ),
    ?assertSame(
        "fun(X) when\n"
        "    is_integer(X);\n"
        "    is_string(X)\n"
        "->\n"
        "    X\n"
        "end\n"
    ).

case_expression(Config) when is_list(Config) ->
    ?assertFormat(
        "case 1 of 1 -> ok end",
        "case 1 of\n"
        "    1 -> ok\n"
        "end\n",
        100
    ),
    ?assertFormat(
        "case 1 of 1 -> ok, ok end",
        "case 1 of\n"
        "    1 ->\n"
        "        ok,\n"
        "        ok\n"
        "end\n",
        100
    ),
    ?assertFormat(
        "case 1 of {Long} -> Expression end",
        "case 1 of\n"
        "    {Long} -> Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "case 1 of {Even, Longer} -> Expression end",
        "case 1 of\n"
        "    {Even, Longer} ->\n"
        "        Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "case 1 of Long when Guarded -> Expression end",
        "case 1 of\n"
        "    Long when Guarded ->\n"
        "        Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "case 1 of {Even, Longer} when Guarded -> Expression end",
        "case 1 of\n"
        "    {Even, Longer} when\n"
        "        Guarded\n"
        "    ->\n"
        "        Expression\n"
        "end\n",
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
        "end\n",
        25
    ),
    ?assertFormat(
        "case 1 of {Long, Pattern} when LongGuard; Is, Very, Very, Long -> Expression end",
        "case 1 of\n"
        "    {Long, Pattern} when\n"
        "        LongGuard;\n"
        "        Is, Very, Very, Long\n"
        "    ->\n"
        "        Expression\n"
        "end\n",
        30
    ),
    ?assertFormat(
        "case 1 of Short -> Expr; {Long, Pattern} -> Expression end",
        "case 1 of\n"
        "    Short ->\n"
        "        Expr;\n"
        "    {Long, Pattern} ->\n"
        "        Expression\n"
        "end\n",
        25
    ),
    ?assertSame(
        "case 1 of\n"
        "    1 -> ok\n"
        "    %% comment\n"
        "end\n"
    ),
    ?assertSame(
        "case 1 of\n"
        "    1 -> ok\n"
        "\n"
        "    %% comment\n"
        "end\n"
    ),
    ?assertSame(
        "case\n"
        "    [\n"
        "        value\n"
        "    ]\n"
        "of\n"
        "    _ -> ok\n"
        "end.\n"
    ),
    ?assertSame(
        "case X of\n"
        "    _ when\n"
        "        X;\n"
        "        Y\n"
        "    ->\n"
        "        ok;\n"
        "    _ ->\n"
        "        error\n"
        "end\n"
    ),
    %% when a pattern is in a clause and it breaks we want to prevent issue #211
    ?assertSame(
        "handle_call({start_abcdefgh, AbcdefghId}, _From, #state{abcdefghs = Abcdefghs0} = State0) ->\n"
        "    case maps:get(AbcdefghId, Abcdefghs0, undefined) of\n"
        "        undefined ->\n"
        "            {reply, {error, {unknown, \"Unknown abcdefgh id\"}}, State0};\n"
        "        #abcdefgh{abcdefgh_type = AbcdefghType, specs = Specs, abc_de_abcde = [AbcdefgId | Rest]} =\n"
        "                Abcdefgh ->\n"
        "            {reply, ok, State}\n"
        "    end.\n",
        100
    ),
    ?assertSame(
        "handle_call({start_abcdefgh, AbcdefghId}, _From, #state{abcdefghs = Abcdefghs0} = State0) ->\n"
        "    case maps:get(AbcdefghId, Abcdefghs0, undefined) of\n"
        "        undefined ->\n"
        "            {reply, {error, {unknown, \"Unknown abcdefgh id\"}}, State0};\n"
        "        #abcdefgh{abcdefgh_type = AbcdefghType, specs = Specs, abc_de_abcde = [AbcdefgId | Rest]} =\n"
        "                Abcdefgh =\n"
        "                    Bcdef123123123123123123123123123123123123123123123123123123123123123123123123 ->\n"
        "            {reply, ok, State}\n"
        "    end.\n",
        100
    ),
    ?assertSame(
        "handle_call({start_abcdefgh, AbcdefghId}, _From, #state{abcdefghs = Abcdefghs0} = State0) ->\n"
        "    case maps:get(AbcdefghId, Abcdefghs0, undefined) of\n"
        "        undefined ->\n"
        "            {reply, {error, {unknown, \"Unknown abcdefgh id\"}}, State0};\n"
        "        #abcdefgh{\n"
        "            abcdefgh_type = AbcdefghType\n"
        "        } =\n"
        "                Abcdefgh ->\n"
        "            {reply, ok, State}\n"
        "    end.\n",
        100
    ),
    ?assertSame(
        "handle_call({start_abcdefgh, AbcdefghId}, _From, #state{abcdefghs = Abcdefghs0} = State0) ->\n"
        "    case maps:get(AbcdefghId, Abcdefghs0, undefined) of\n"
        "        undefined ->\n"
        "            {reply, {error, {unknown, \"Unknown abcdefgh id\"}}, State0};\n"
        "        Abcdefgh =\n"
        "                #abcdefgh{\n"
        "                    abcdefgh_type = AbcdefghType,\n"
        "                    specs = Specs,\n"
        "                    abc_de_abcde = [AbcdefgId | Rest]\n"
        "                } ->\n"
        "            {reply, ok, State}\n"
        "    end.\n",
        100
    ),
    ?assertSame(
        "handle_call({start_abcdefgh, AbcdefghId}, _From, #state{abcdefghs = Abcdefghs0} = State0) ->\n"
        "    case maps:get(AbcdefghId, Abcdefghs0, undefined) of\n"
        "        undefined ->\n"
        "            {reply, {error, {unknown, \"Unknown abcdefgh id\"}}, State0};\n"
        "        Abcdefgh = #abcdefgh{\n"
        "            abcdefgh_type = AbcdefghType,\n"
        "            specs = Specs,\n"
        "            abc_de_abcde = [AbcdefgId | Rest]\n"
        "        } ->\n"
        "            {reply, ok, State}\n"
        "    end.\n",
        100
    ),
    ?assertSame(
        "handle_call({start_abcdefgh, AbcdefghId}, _From, #state{abcdefghs = Abcdefghs0} = State0) ->\n"
        "    case maps:get(AbcdefghId, Abcdefghs0, undefined) of\n"
        "        undefined ->\n"
        "            {reply, {error, {unknown, \"Unknown abcdefgh id\"}}, State0};\n"
        "        \"abcdefgh{abcdefgh_type = AbcdefghType, specs = Specs, abc_de_abcde = AbcdefgId | Rest\" ++\n"
        "                Abcdefgh ->\n"
        "            {reply, ok, State}\n"
        "    end.\n",
        100
    ),
    ?assertFormat(
        "case 1 of ?macro(1); ?macro(2) end",
        "case 1 of\n"
        "    ?macro(1);\n"
        "    ?macro(2)\n"
        "end\n"
    ).

maybe_expression(Config) when is_list(Config) ->
    %% automatically upgrades
    ?assertFormat("foo(maybe, else)", "foo('maybe', 'else')\n"),

    ?assertFormat(
        "maybe 1 ?= 1, 2 = 2, 3, ok\nelse 4 -> ok\nend",
        "maybe\n"
        "    1 ?= 1,\n"
        "    2 = 2,\n"
        "    3,\n"
        "    ok\n"
        "else\n"
        "    4 -> ok\n"
        "end\n",
        100
    ),

    ?assertFormat(
        "maybe\nok ?= {error, #{extremely_long_name => that_forces_a_break, for_the_expression => because_it_goes_over, the_line => limit}}, ok\nend",
        "maybe\n"
        "    ok ?=\n"
        "        {error, #{\n"
        "            extremely_long_name => that_forces_a_break,\n"
        "            for_the_expression => because_it_goes_over,\n"
        "            the_line => limit\n"
        "        }},\n"
        "    ok\n"
        "end\n",
        100
    ),

    ?assertSame(
        "maybe\n"
        "    % comment before\n"
        "    ok ?= ok,\n"
        "    % comment end\n"
        "    ok\n"
        "else\n"
        "    % comment else before\n"
        "    1 -> ok\n"
        "    % comment else after\n"
        "end\n"
    ).

receive_expression(Config) when is_list(Config) ->
    ?assertFormat(
        "receive 1 -> ok end",
        "receive\n"
        "    1 -> ok\n"
        "end\n",
        100
    ),
    ?assertSame(
        "receive\n"
        "after 0 -> ok\n"
        "end\n"
    ),
    ?assertSame(
        "receive\n"
        "%% comment\n"
        "after 0 -> ok\n"
        "end\n"
    ),
    ?assertSame(
        "receive\n"
        "after 0 ->\n"
        "    some:long(\n"
        "        Expression\n"
        "    )\n"
        "end\n",
        25
    ),
    ?assertSame(
        "receive\n"
        "    1 -> ok\n"
        "after 0 -> ok\n"
        "end\n"
    ),
    ?assertSame(
        "receive\n"
        "    1 -> ok\n"
        "after 0 ->\n"
        "    ok\n"
        "end\n"
    ),
    ?assertFormat(
        "receive {Long} -> Expression end",
        "receive\n"
        "    {Long} -> Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "receive {Even, Longer} -> Expression end",
        "receive\n"
        "    {Even, Longer} ->\n"
        "        Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "receive Long when Guarded -> Expression end",
        "receive\n"
        "    Long when Guarded ->\n"
        "        Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "receive {Even, Longer} when Guarded -> Expression end",
        "receive\n"
        "    {Even, Longer} when\n"
        "        Guarded\n"
        "    ->\n"
        "        Expression\n"
        "end\n",
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
        "end\n",
        25
    ),
    ?assertFormat(
        "receive {Long, Pattern} when VeryLongGuard; Is, Very, Long -> Expression end",
        "receive\n"
        "    {Long, Pattern} when\n"
        "        VeryLongGuard;\n"
        "        Is, Very, Long\n"
        "    ->\n"
        "        Expression\n"
        "end\n",
        30
    ),
    ?assertFormat(
        "receive\n"
        "    _ -> ok % foo\n"
        "end\n",
        "receive\n"
        "    % foo\n"
        "    _ -> ok\n"
        "end\n"
    ),
    ?assertFormat(
        "receive\n"
        "    1 -> one; % one\n"
        "    2 -> two % two\n"
        "end\n",
        "receive\n"
        "    % one\n"
        "    1 -> one;\n"
        "    % two\n"
        "    2 -> two\n"
        "end\n"
    ),
    ?assertFormat(
        "receive\n"
        "after\n"
        "    0 -> ok\n"
        "    % receive post comment\n"
        "end\n",
        "receive\n"
        "after 0 ->\n"
        "    ok\n"
        "    % receive post comment\n"
        "end\n"
    ),
    ?assertFormat(
        "receive\n"
        "after\n"
        "    % before zero\n"
        "    0 -> ok\n"
        "end\n",
        "receive\n"
        "after\n"
        "    % before zero\n"
        "    0 -> ok\n"
        "end\n"
    ),
    ?assertFormat(
        "receive\n"
        "after\n"
        "    %% foo\n"
        "    0 ->\n"
        "        \"abc\"\n"
        "        \"def\"\n"
        "end.\n",
        "receive\n"
        "after\n"
        "    %% foo\n"
        "    0 ->\n"
        "        \"abc\"\n"
        "        \"def\"\n"
        "end.\n"
    ),
    ?assertFormat(
        "receive\n"
        "    1 -> ok\n"
        "    %% after receive\n"
        "after\n"
        "    0 -> ok\n"
        "    %% after after for receive\n"
        "end\n",
        "receive\n"
        "    1 -> ok\n"
        "    %% after receive\n"
        "after 0 ->\n"
        "    ok\n"
        "    %% after after for receive\n"
        "end\n"
    ),
    ?assertFormat(
        "receive\n"
        "    1 -> ok\n"
        "    %% after receive\n"
        "after\n"
        "    %% before zero\n"
        "    0 -> ok\n"
        "    %% after after for receive\n"
        "end\n",
        "receive\n"
        "    1 -> ok\n"
        "    %% after receive\n"
        "after\n"
        "    %% before zero\n"
        "    0 ->\n"
        "        ok\n"
        "        %% after after for receive\n"
        "end\n"
    ),
    ?assertFormat(
        "receive\n"
        "    1 -> ok\n"
        "after 0 -> % after arrow moves one line above\n"
        "    a(\n"
        "        b,\n"
        "        c\n"
        "    )\n"
        "end\n",
        "receive\n"
        "    1 -> ok\n"
        "    % after arrow moves one line above\n"
        "after 0 ->\n"
        "    a(\n"
        "        b,\n"
        "        c\n"
        "    )\n"
        "end\n"
    ),
    ?assertFormat(
        "receive\n"
        "    1 -> ok\n"
        "    %% after receive\n"
        "after\n"
        "    %% before zero\n"
        "    0 -> a(\n"
        "        b,\n"
        "        c\n"
        "    )\n"
        "    %% after after for receive\n"
        "end\n",
        "receive\n"
        "    1 -> ok\n"
        "    %% after receive\n"
        "after\n"
        "    %% before zero\n"
        "    0 ->\n"
        "        a(\n"
        "            b,\n"
        "            c\n"
        "        )\n"
        "        %% after after for receive\n"
        "end\n"
    ).

try_expression(Config) when is_list(Config) ->
    ?assertFormat(
        "try ok after Expr end",
        "try\n"
        "    ok\n"
        "after\n"
        "    Expr\n"
        "end\n"
    ),
    ?assertFormat(
        "try ok after Expr1, Expr2 end",
        "try\n"
        "    ok\n"
        "after\n"
        "    Expr1,\n"
        "    Expr2\n"
        "end\n"
    ),
    ?assertFormat(
        "try Expr1, Expr2 after Expr end",
        "try\n"
        "    Expr1,\n"
        "    Expr2\n"
        "after\n"
        "    Expr\n"
        "end\n"
    ),
    ?assertFormat(
        "try ok catch _ -> throw end",
        "try\n"
        "    ok\n"
        "catch\n"
        "    _ -> throw\n"
        "end\n"
    ),
    ?assertFormat(
        "try ok catch throw:_ -> throw; error:_ -> error; exit:_ -> exit end",
        "try\n"
        "    ok\n"
        "catch\n"
        "    throw:_ -> throw;\n"
        "    error:_ -> error;\n"
        "    exit:_ -> exit\n"
        "end\n"
    ),
    ?assertFormat(
        "try ok catch error:Reason:Stack -> {error, {Reason, Stack}} end",
        "try\n"
        "    ok\n"
        "catch\n"
        "    error:Reason:Stack ->\n"
        "        {error, {Reason, Stack}}\n"
        "end\n",
        35
    ),
    ?assertSame(
        "try 2 of\n"
        "    true -> ok\n"
        "catch\n"
        "    _ ->\n"
        "        []\n"
        "    %% comment\n"
        "end\n"
    ),
    ?assertFormat(
        "try Expr of _ -> ok after Expr end",
        "try Expr of\n"
        "    _ -> ok\n"
        "after\n"
        "    Expr\n"
        "end\n"
    ),
    ?assertFormat(
        "try Expr1, Expr2 of _ -> ok after Expr end",
        "try\n"
        "    Expr1,\n"
        "    Expr2\n"
        "of\n"
        "    _ -> ok\n"
        "after\n"
        "    Expr\n"
        "end\n"
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
        "end\n"
    ),
    ?assertSame(
        "try\n"
        "    2\n"
        "catch\n"
        "    _ ->\n"
        "        undefined\n"
        "    %% after catch\n"
        "after\n"
        "    ok\n"
        "    %% after after\n"
        "end\n"
    ),
    ?assertSame(
        "try 2 of\n"
        "    _ ->\n"
        "        undefined\n"
        "    %% after of\n"
        "after\n"
        "    ok\n"
        "    %% after after\n"
        "end\n"
    ),
    ?assertSame(
        "try\n"
        "    2\n"
        "    % after expr\n"
        "after\n"
        "    ok\n"
        "    %% after after\n"
        "end\n"
    ).

if_expression(Config) when is_list(Config) ->
    ?assertFormat(
        "if true -> ok end",
        "if\n"
        "    true -> ok\n"
        "end\n",
        100
    ),
    ?assertFormat(
        "if long() -> Expression end",
        "if\n"
        "    long() -> Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "if even(Longer) -> Expression end",
        "if\n"
        "    even(Longer) ->\n"
        "        Expression\n"
        "end\n",
        25
    ),
    ?assertFormat(
        "if the(Guard); Is, Long -> Expression end",
        "if\n"
        "    the(Guard);\n"
        "    Is, Long ->\n"
        "        Expression\n"
        "end\n",
        20
    ),
    ?assertFormat(
        "if Short -> Expr; long(Guard, And) -> Expression end",
        "if\n"
        "    Short ->\n"
        "        Expr;\n"
        "    long(Guard, And) ->\n"
        "        Expression\n"
        "end\n",
        25
    ).

macro(Config) when is_list(Config) ->
    ?assertSame("??FOO\n"),
    ?assertFormat("? Foo", "?Foo\n"),
    ?assertSame("?foo\n"),
    ?assertFormat("?FOO(\n)", "?FOO()\n"),
    ?assertSame("?assertMatch(X when is_integer(X), Y)\n"),
    ?assertFormat(
        "?assertMatch(X when is_integer(X), Y)",
        "?assertMatch(\n"
        "    X when is_integer(X),\n"
        "    Y\n"
        ")\n",
        25
    ),
    ?assertFormat(
        "?assertMatch(X when is_integer(X), Y)",
        "?assertMatch(\n"
        "    X when\n"
        "        is_integer(X),\n"
        "    Y\n"
        ")\n",
        23
    ),
    ?assertSame("?macro(Expr when Guard1; Guard2)\n").

function(Config) when is_list(Config) ->
    ?assertSame("f() -> ok.\n"),
    ?assertSame(
        "f(1) -> one;\n"
        "f(2) -> two.\n"
    ),
    ?assertFormat(
        "f(1) -> one; f(2) -> begin two end.",
        "f(1) ->\n"
        "    one;\n"
        "f(2) ->\n"
        "    begin\n"
        "        two\n"
        "    end.\n"
    ),
    ?assertSame(
        "?FOO(1) -> ok;\n"
        "?DEFAULT(?FOO).\n"
    ),
    ?assertSame(
        "?DEFAULT(?FOO);\n"
        "?FOO(1) -> ok.\n"
    ),
    ?assertSame(
        "?DEFAULT(\n"
        "    ?FOO\n"
        ");\n"
        "?FOO(1) ->\n"
        "    ok.\n"
    ),
    ?assertSame(
        "%% comment\n"
        "bar(X) ->\n"
        "    ok;\n"
        "%% comment\n"
        "bar(X) ->\n"
        "    ok.\n"
    ),
    ?assertFormat(
        "bar(X) -> ok. % comment\n",
        "% comment\n"
        "bar(X) -> ok.\n"
    ).

attribute(Config) when is_list(Config) ->
    ?assertFormat("-else .", "-else.\n"),
    ?assertFormat("-else () .", "-else().\n"),
    ?assertFormat("- foo ( 1 ).", "-foo(1).\n"),
    ?assertFormat(
        "- foo (\n"
        "1).",
        "-foo(\n"
        "    1\n"
        ").\n"
    ),
    ?assertSame("-compile([export_all, nowarn_export_all]).\n"),
    ?assertSame("-import(foo, [bar/2, baz/0]).\n"),
    ?assertSame("-import_type(foo, [bar/2, baz/0]).\n"),
    ?assertFormat(
        "-attribute({Very, Long, Value}).",
        "-attribute(\n"
        "    {Very, Long, Value}\n"
        ").\n",
        25
    ),
    ?assertFormat(
        "-attribute([ExceptionallyLong, Value]).",
        "-attribute([\n"
        "    ExceptionallyLong,\n"
        "    Value\n"
        "]).\n",
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
        ").\n",
        25
    ),
    ?assertSame(
        "-export_type([\n"
        "    baz/1,\n"
        "    foo/2, foo/3,\n"
        "    bar/2\n"
        "]).\n"
    ),
    ?assertFormat(
        "-type str() :: string().\n"
        "\n"
        "\n"
        "-type int() :: integer().\n",
        "-type str() :: string().\n"
        "\n"
        "-type int() :: integer().\n"
    ).

exportimport(Config) when is_list(Config) ->
    ?assertSame("-export([bar/2, baz/3]).\n"),
    ?assertSame(
        "-export([\n"
        "    bar/2, bar/3\n"
        "]).\n"
    ),
    ?assertSame(
        "-export([\n"
        "    %% comment\n"
        "    bar/2, bar/3\n"
        "]).\n"
    ),
    ?assertSame(
        "-export([\n"
        "    % comment\n"
        "    bar/2,\n"
        "    baz/3\n"
        "]).\n"
    ),
    ?assertSame(
        "-import(modulename, [\n"
        "    foo/1,\n"
        "    bar/2,\n"
        "    foo/2, foo/3\n"
        "]).\n"
    ),
    ?assertSame(
        "-export([\n"
        "    baz/1,\n"
        "    foo/2, foo/3,\n"
        "    bar/2\n"
        "]).\n"
    ),
    ?assertSame(
        "-export([\n"
        "    baz/1,\n"
        "    % comment foo\n"
        "    foo/2, foo/3,\n"
        "    % comment bar\n"
        "    bar/2\n"
        "]).\n"
    ),
    ?assertFormat(
        "-export([ drop/4 %% comment\n"
        "]).\n"
        "%% Another comment\n"
        "-export([ a/1,\n"
        "        b/1\n"
        "]).",
        "%% comment\n"
        "-export([drop/4]).\n"
        "%% Another comment\n"
        "-export([\n"
        "    a/1,\n"
        "    b/1\n"
        "]).\n",
        80
    ),
    ?assertFormat(
        "-export([ drop/4 %% comment\n"
        "]).\n"
        "\n"
        "%% Another comment\n"
        "-export([ a/1,\n"
        "        b/1\n"
        "]).",
        "%% comment\n"
        "-export([drop/4]).\n"
        "\n"
        "%% Another comment\n"
        "-export([\n"
        "    a/1,\n"
        "    b/1\n"
        "]).\n",
        80
    ),
    ?assertSame(
        "-export([\n"
        "    % comment\n"
        "    a/2,\n"
        "\n"
        "    % another group\n"
        "    b/1\n"
        "]).\n"
    ),
    ?assertSame(
        "-export([\n"
        "    a/2,\n"
        "    c/2,\n"
        "\n"
        "    a/1,\n"
        "    c/1,\n"
        "\n"
        "    b/1,\n"
        "    d/2\n"
        "    % trailing comment\n"
        "]).\n"
    ),
    ?assertSame(
        "-import(erlfmt_algebra, [\n"
        "    a/1,\n"
        "    a/2,\n"
        "    b/1,\n"
        "    b/2,\n"
        "    c/1,\n"
        "    c/2\n"
        "]).\n"
    ),
    ?assertSame(
        "-export([\n"
        "    bar/1, baz/1\n"
        "]).\n"
    ),
    ?assertFormat(
        "-export([\n"
        "    foo/1, bar/1,\n"
        "    baz/1, baz/2\n"
        "]).\n",
        "-export([\n"
        "    foo/1,\n"
        "    bar/1,\n"
        "    baz/1, baz/2\n"
        "]).\n"
    ).

ifdef(Config) when is_list(Config) ->
    %% preserves empty line after if, ifdef, ifndef, else
    ?assertSame(
        "-if(true).\n"
        "ok() -> ok.\n"
        "\n"
        "-if(true).\n"
        "\n"
        "ok() -> ok.\n"
    ),
    ?assertSame(
        "-ifdef(FOO).\n"
        "ok() -> ok.\n"
        "\n"
        "-ifdef(FOO).\n"
        "\n"
        "ok() -> ok.\n"
    ),
    ?assertSame(
        "-ifndef(FOO).\n"
        "ok() -> ok.\n"
        "\n"
        "-ifndef(FOO).\n"
        "\n"
        "ok() -> ok.\n"
    ),
    %% preserves empty line before else, endif
    ?assertSame(
        "ok() -> ok.\n"
        "-else.\n"
        "\n"
        "ok() -> ok.\n"
        "\n"
        "-else.\n"
    ),
    ?assertSame(
        "ok() -> ok.\n"
        "-endif.\n"
        "\n"
        "ok() -> ok.\n"
        "\n"
        "-endif.\n"
    ),
    %% preserves no empty line before endif
    ?assertSame(
        "-ifdef(TEST).\n"
        "start(_StartType, _StartArgs) ->\n"
        "    mylib_sup:start_link().\n"
        "\n"
        "stop(_State) ->\n"
        "    ok.\n"
        "-endif().\n"
    ).

record_definition(Config) when is_list(Config) ->
    ?assertSame(
        "-record(foo, {a = 1 :: integer(), b :: float(), c = 2, d}).\n",
        60
    ),
    ?assertFormat(
        "-record(foo, {a = 1 :: integer(), b :: float(), c  = 2, d}).",
        "-record(foo, {\n"
        "    a = 1 :: integer(), b :: float(), c = 2, d\n"
        "}).\n",
        50
    ),
    ?assertFormat(
        "-record(foo, {a = 1 :: integer(), b :: float(), c  = 2, d}).",
        "-record(foo, {\n"
        "    a = 1 :: integer(),\n"
        "    b :: float(),\n"
        "    c = 2,\n"
        "    d\n"
        "}).\n",
        40
    ),
    ?assertSame(
        "-record(foo,\n"
        "    %% comment\n"
        "    {a = 1 :: integer(), b :: float(), c = 2, d}\n"
        ").\n",
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
        ").\n",
        60
    ),
    ?assertSame(
        "-record(foo, {\n"
        "    field = #{\n"
        "        key1 => [],\n"
        "        key2 => []\n"
        "    } :: type()\n"
        "}).\n"
    ),
    %% macro in record should not crash during formatting
    ?assertSame(
        "-record(foo, {?FOO}).\n"
    ),
    ?assertSame(
        "-record(foo, {?FOO, b :: any()}).\n"
    ).

spec(Config) when is_list(Config) ->
    ?assertSame(
        "-spec foo() ->\n"
        "    atom().\n"
    ),
    ?assertFormat(
        "-spec child_spec(#{\n"
        "    name => {local, Name :: atom()} | {global, GlobalName :: any()} | {via, Module :: atom(), ViaName :: any()},\n"
        "    another_field => atom()\n"
        "}) -> supervisor:child_spec().\n",
        "-spec child_spec(#{\n"
        "    name =>\n"
        "        {local, Name :: atom()}\n"
        "        | {global, GlobalName :: any()}\n"
        "        | {via, Module :: atom(), ViaName :: any()},\n"
        "    another_field => atom()\n"
        "}) -> supervisor:child_spec().\n"
    ),
    ?assertSame(
        "-spec foo(Int) -> atom() when Int :: integer().\n"
    ),
    ?assertFormat(
        "-spec foo(Int) -> atom() when Int :: integer().",
        "-spec foo(Int) -> atom() when\n"
        "    Int :: integer().\n",
        40
    ),
    ?assertFormat(
        "-spec foo(Int) ->\n"
        "    atom() when\n"
        "    Int :: integer().\n",
        "-spec foo(Int) ->\n"
        "    atom()\n"
        "when\n"
        "    Int :: integer().\n"
    ),
    ?assertFormat(
        "-spec foo(Int) ->\n"
        "    atom() when Int :: integer().\n",
        "-spec foo(Int) ->\n"
        "    atom()\n"
        "when\n"
        "    Int :: integer().\n"
    ),
    ?assertFormat(
        "-spec foo(Int) -> atom() when\n"
        "    Int :: integer().\n",
        "-spec foo(Int) -> atom() when\n"
        "    Int :: integer().\n"
    ),
    ?assertSame(
        "bar(X) when is_list(X) ->\n"
        "    ok.\n"
    ),
    ?assertFormat(
        "bar(X) when\n"
        "   is_list(X) -> ok.\n",
        "bar(X) when\n"
        "    is_list(X)\n"
        "->\n"
        "    ok.\n"
    ),
    ?assertFormat(
        "-spec foo(Int) -> some_very:very(long, type) when Int :: integer().",
        "-spec foo(Int) ->\n"
        "    some_very:very(long, type)\n"
        "when\n"
        "    Int :: integer().\n",
        40
    ),
    ?assertSame(
        "-callback long_name(Bar) -> #{map := type([Bar, ...])}.\n"
    ),
    ?assertFormat(
        "-callback long_name(Bar) -> #{map := type([Bar, ...])}.",
        "-callback long_name(Bar) ->\n"
        "    #{map := type([Bar, ...])}.\n",
        50
    ),
    ?assertFormat(
        "-spec foo(integer()) -> some_very:very(long, type); (atom()) -> atom().",
        "-spec foo\n"
        "    (integer()) -> some_very:very(long, type);\n"
        "    (atom()) -> atom().\n"
    ),
    ?assertFormat(
        "-spec foo(integer()) -> some_very:very(long, type); (1..2) -> atom().",
        "-spec foo\n"
        "    (integer()) ->\n"
        "        some_very:very(long, type);\n"
        "    (1..2) -> atom().\n",
        40
    ),
    ?assertFormat(
        "-spec foo(Int) -> some_very_very:very(long, type) when Int :: integer(); (1..2) -> atom().",
        "-spec foo\n"
        "    (Int) ->\n"
        "        some_very_very:very(long, type)\n"
        "    when\n"
        "        Int :: integer();\n"
        "    (1..2) -> atom().\n",
        40
    ),
    ?assertFormat(
        "-spec compute_rates(Secs, L1, L1) -> L2\n"
        "    when Secs :: pos_integer(),\n"
        "    L1 :: [{K, non_neg_integer()}],\n"
        "    L2 :: [{K, number()}], % also non-negative, but may be float\n"
        "    K :: wa_stats:key().",
        "-spec compute_rates(Secs, L1, L1) -> L2 when\n"
        "    Secs :: pos_integer(),\n"
        "    L1 :: [{K, non_neg_integer()}],\n"
        "    % also non-negative, but may be float\n"
        "    L2 :: [{K, number()}],\n"
        "    K :: wa_stats:key().\n"
    ),
    ?assertFormat(
        "-spec foo(Int) -> some_very:very(long, type) when Int :: integer().",
        "-spec foo(Int) -> some_very:very(long, type) when\n"
        "    Int :: integer().\n",
        50
    ),
    ?assertFormat(
        "-spec foo(very_very_long_type(with_argument), yet_another_long_type()) -> some_very:very(long, type) when Int :: integer().",
        "-spec foo(\n"
        "    very_very_long_type(with_argument),\n"
        "    yet_another_long_type()\n"
        ") -> some_very:very(long, type) when\n"
        "    Int :: integer().\n",
        50
    ),
    ?assertFormat(
        "-spec foo(very_long_type(), another_long_type()) -> some_very:very(long, type).",
        "-spec foo(very_long_type(), another_long_type()) ->\n"
        "    some_very:very(long, type).\n",
        50
    ),
    ?assertFormat(
        "-spec encode(#a{} | #blonglonglong{} | #c{} | #d{}) -> binary().",
        "-spec encode(\n"
        "    #a{}\n"
        "    | #blonglonglong{}\n"
        "    | #c{}\n"
        "    | #d{}\n"
        ") -> binary().\n",
        30
    ),
    ?assertSame(
        "-spec use_credit(\n"
        "    store:id(),\n"
        "    decimal:decimal(),\n"
        "    binary() | null,\n"
        "    credit | {refund | dispute, binary()},\n"
        "    binary() | null\n"
        ") -> ok.\n"
    ),
    ?assertSame(
        "-spec use_credit(\n"
        "    store:id(),\n"
        "    decimal:decimal(),\n"
        "    binary() | null,\n"
        "    credit | {refund | dispute, binary()},\n"
        "    binary() | null\n"
        ") ->\n"
        "    ok.\n"
    ),
    ?assertSame(
        "-spec my_fun(TypeA, TypeB) -> ok when\n"
        "    TypeA :: some_type(),\n"
        "    TypeB :: some_other_type().\n"
    ),
    ?assertSame(
        "-spec f(A, B) -> result() when B :: term() | (List :: [term()]).\n"
    ),
    ?assertSame(
        "-spec do_stuff(Arg :: binary()) -> binary().\n"
        "-ifdef(TEST).\n"
        "do_stuff(Arg) -> Arg.\n"
        "-else.\n"
        "do_stuff(_Arg) -> <<\"ok\">>.\n"
        "-endif.\n"
    ),
    ?assertSame(
        "-spec ?MODULE:foo() -> ok.\n"
    ),
    ?assertSame(
        "-spec foo:?BAR() -> ok.\n"
    ),
    ?assertSame(
        "-spec fits(Width :: integer(), Column :: integer(), HasBreaks :: boolean(), Entries) ->\n"
        "    boolean()\n"
        "when\n"
        "    Entries ::\n"
        "        maybe_improper_list(\n"
        "            {integer(), mode(), doc()},\n"
        "            {tail, boolean(), Entries} | []\n"
        "        ).\n",
        100
    ),
    ?assertSame(
        "-spec fits(Width :: integer(), Column :: integer(), HasBreaks :: boolean(), Entries) ->\n"
        "    boolean()\n"
        "when\n"
        "    Entries :: maybe_improper_list(\n"
        "        {integer(), mode(), doc()},\n"
        "        {tail, boolean(), Entries} | []\n"
        "    ).\n",
        100
    ),
    ?assertFormat(
        "-spec fits() -> boolean() when\n"
        "    Entries :: {VeryLongTuple, EvenLonger}.\n",
        "-spec fits() -> boolean() when\n"
        "    Entries ::\n"
        "        {VeryLongTuple,\n"
        "            EvenLonger}.\n",
        30
    ),
    ?assertSame(
        "-spec foo(Int) -> atom() when\n"
        "    %% comment\n"
        "    Int :: integer().\n"
    ),
    ?assertFormat(
        "-spec foo(Int) -> atom() when\n"
        "    Int :: integer()\n"
        "    %% after clause comment\n"
        "    .\n",
        "-spec foo(Int) -> atom() when\n"
        "    Int :: integer()\n"
        "%% after clause comment\n"
        ".\n"
    ),
    ?assertFormat(
        "-spec foo(Int) -> atom()\n"
        "    %% before when comment\n"
        "    when Int :: integer().\n",
        "-spec foo(Int) -> atom() when\n"
        "    %% before when comment\n"
        "    Int :: integer().\n"
    ),
    ?assertFormat(
        "-spec foo(Int) -> atom()\n"
        "    %% before when comment\n"
        "    when\n"
        "    %% after when comment\n"
        "    Int :: integer().\n",
        "-spec foo(Int) -> atom() when\n"
        "    %% before when comment\n"
        "\n"
        "    %% after when comment\n"
        "    Int :: integer().\n"
    ),
    ?assertFormat(
        "-spec to_atom\n"
        "    %% int to atom\n"
        "    (Int) -> atom() when\n"
        "    %% int is integer\n"
        "    Int :: integer();\n"
        "    %% string to atom\n"
        "    (String) -> atom() when\n"
        "    %% string is string\n"
        "    String :: string().\n",
        "-spec to_atom\n"
        "    %% int to atom\n"
        "    (Int) -> atom() when\n"
        "        %% int is integer\n"
        "        Int :: integer();\n"
        "    %% string to atom\n"
        "    (String) -> atom() when\n"
        "        %% string is string\n"
        "        String :: string().\n"
    ),
    ?assertFormat(
        "-spec to_atom\n"
        "    %% int to atom\n"
        "    (Int) -> atom() when\n"
        "    %% int is integer\n"
        "    Int :: integer()\n"
        "    %% before semi colon\n"
        "    ;\n"
        "    %% string to atom\n"
        "    (String) -> atom() when\n"
        "    %% string is string\n"
        "    String :: string()\n"
        "    %% before dot\n"
        "    .\n",
        "-spec to_atom\n"
        "    %% int to atom\n"
        "    (Int) -> atom() when\n"
        "        %% int is integer\n"
        "        Int :: integer();\n"
        "    %% before semi colon\n"
        "\n"
        "    %% string to atom\n"
        "    (String) -> atom() when\n"
        "        %% string is string\n"
        "        String :: string()\n"
        "%% before dot\n"
        ".\n"
    ),
    ?assertFormat(
        "-spec long_name(Long, Arguments) -> ok.",
        "-spec long_name(\n"
        "    Long, Arguments\n"
        ") -> ok.\n",
        25
    ),
    ?assertSame(
        "-spec long_name(\n"
        "    Long, Arguments\n"
        ") -> ok.\n"
    ),
    ?assertFormat(
        "-spec long_name(\n"
        "    Long, Arguments\n"
        ") -> ok.\n",
        "-spec long_name(\n"
        "    Long,\n"
        "    Arguments\n"
        ") -> ok.\n",
        10
    ),
    ?assertSame(
        "-spec long_name(\n"
        "    Long,\n"
        "    Arguments\n"
        ") -> ok.\n"
    ).

define(Config) when is_list(Config) ->
    ?assertSame(
        "-define(IN_RANGE(Value, Low, High), Value >= Low andalso Value =< High).\n"
    ),
    ?assertFormat(
        "-define(OUT_OF_RANGE(Value, Low, High), (Value) =< long_expression(Low), Value >= long_expression(High)).",
        "-define(OUT_OF_RANGE(Value, Low, High),\n"
        "    (Value) =< long_expression(Low),\n"
        "    Value >= long_expression(High)\n"
        ").\n",
        40
    ),
    ?assertSame(
        "-define(FOO(X), begin\n"
        "    is_atom(X) orelse is_tuple(X)\n"
        "end).\n"
    ),
    ?assertSame(
        "-define(FOO(X), [1, 2, 3]).\n"
    ),
    ?assertSame(
        "-define(FOO(X), [\n"
        "    1,\n"
        "    2,\n"
        "    3\n"
        "]).\n"
    ),
    ?assertSame("-define(Foo,).\n"),
    ?assertSame(
        "%comment\n"
        "-define(Foo,).\n"
    ),
    ?assertSame("-define(FOO, #foo).\n"),
    ?assertSame("-define(parens, ()).\n"),
    ?assertSame("-define(record(N), #N{}).\n"),
    ?assertSame("-define(TIMEOUT_TYPE, 0..?MAX_INT_TIMEOUT | 'infinity').\n"),
    ?assertSame(
        "-define(DEFAULT(Name),\n"
        "    Name(X) ->\n"
        "        X\n"
        ").\n"
    ),
    ?assertSame("-define(ANY_MODE(Mode), Mode when Mode =:= inline; Mode =:= async).\n"),
    ?assertSame("-define(CATCH, C:E:S).\n").

type(Config) when is_list(Config) ->
    ?assertSame(
        "-opaque foo() :: #foo{a :: integer(), b :: module:type()}.\n"
    ),
    ?assertFormat(
        "-type foobar() :: #foo{a :: integer(), b :: mod:type()}.",
        "-type foobar() :: #foo{\n"
        "    a :: integer(), b :: mod:type()\n"
        "}.\n",
        50
    ),
    ?assertFormat(
        "-type foobar() :: #foo{a :: integer(), b :: mod:type()}.",
        "-type foobar() :: #foo{\n"
        "    a :: integer(),\n"
        "    b :: mod:type()\n"
        "}.\n",
        30
    ),
    ?assertSame(
        "-type foo() :: fun(\n"
        "    (A, B, C) -> return_type(A, B, C)\n"
        ").\n",
        50
    ),
    ?assertSame(
        "-type foo() :: #{\n"
        "    a := integer(),\n"
        "    b => float()\n"
        "}.\n"
    ),
    ?assertSame(
        "-opaque foo() :: {<<>>, <<_:8>>, <<_:_*4>>, <<_:8, _:_*4>>}.\n"
    ),
    ?assertSame(
        "-type foo() :: {fun(), fun((...) -> mod:bar()), fun(() -> integer())}.\n"
    ),
    ?assertSame(
        "-type bar() :: fun(\n"
        "    (\n"
        "        %% foo\n"
        "        ...\n"
        "    ) -> float()\n"
        ").\n"
    ),
    ?assertSame(
        "-type bar() :: fun(\n"
        "    () -> [\n"
        "        atom()\n"
        "    ]\n"
        ").\n"
    ),
    ?assertSame(
        "-type bar() :: fun(\n"
        "    (\n"
        "        %% foo\n"
        "        ...\n"
        "    ) -> [\n"
        "        atom()\n"
        "    ]\n"
        ").\n"
    ),
    ?assertFormat(
        "-type bar() :: fun((\n"
        "        %% foo\n"
        "        ...\n"
        "    ) -> float()\n"
        ").\n",
        "-type bar() :: fun(\n"
        "    (\n"
        "        %% foo\n"
        "        ...\n"
        "    ) -> float()\n"
        ").\n"
    ),
    ?assertFormat(
        "-type bar() ::\n"
        "    fun((\n"
        "            %% foo\n"
        "            ...\n"
        "        ) -> float()\n"
        "    ).\n",
        "-type bar() ::\n"
        "    fun(\n"
        "        (\n"
        "            %% foo\n"
        "            ...\n"
        "        ) -> float()\n"
        "    ).\n"
    ),
    ?assertFormat(
        "-type bar() :: fun((\n"
        "        ...\n"
        "    ) -> float()\n"
        ").",
        "-type bar() :: fun(\n"
        "    (\n"
        "        ...\n"
        "    ) -> float()\n"
        ").\n"
    ),
    ?assertSame(
        "-type foo() :: fun(\n"
        "    (\n"
        "        %% comment\n"
        "        ...\n"
        "    ) ->\n"
        "        %% comment\n"
        "        bar()\n"
        ").\n"
    ),
    ?assertSame(
        "-type foo() ::\n"
        "    %% comment 1\n"
        "    fun(\n"
        "        (\n"
        "            %% comment 2\n"
        "            ...\n"
        "        ) -> bar()\n"
        "    ).\n"
    ),
    ?assertSame(
        "-type bar() :: fun(\n"
        "    (\n"
        "        %% foo\n"
        "        ...\n"
        "        %% after\n"
        "    ) -> float()\n"
        ").\n"
    ),
    ?assertSame(
        "-type t() ::\n"
        "    a\n"
        "    | b\n"
        "% | c\n"
        ".\n"
    ),
    ?assertSame(
        "-attr(\n"
        "    bla\n"
        "    % comment\n"
        ").\n"
    ),
    ?assertSame(
        "-opaque t() ::\n"
        "    a\n"
        "% comment\n"
        ".\n"
    ),
    ?assertSame(
        "-type ps() ::\n"
        "    [p()].\n"
    ),
    ?assertSame(
        "-type foo() :: ?FOO().\n"
    ),
    ?assertSame(
        "-type foo() :: ?FOO:?BAR().\n"
    ).

exprs(Config) when is_list(Config) ->
    ?assertSame(
        "1,\n"
        "2,\n"
        "3.\n"
    ),
    ?assertSame(
        "1,\n"
        "2,\n"
        "3\n"
    ).

comment(Config) when is_list(Config) ->
    ?assertSame(
        "a\n"
        "%% pre dot comment\n"
        ".\n"
    ),
    ?assertFormat(
        "a() ->\n"
        "    b\n"
        "    %% pre dot comment\n"
        "    .\n",
        "a() ->\n"
        "    b\n"
        "%% pre dot comment\n"
        ".\n"
    ),
    ?assertFormat(
        "a() ->\n"
        "    b\n"
        "    %% pre dot comment\n"
        "    .\n"
        "%% post comment\n",
        "a() ->\n"
        "    b\n"
        "%% pre dot comment\n"
        ".\n"
        "%% post comment\n"
    ),
    ?assertSame(
        "A\n"
        "% comment\n"
        ".\n"
    ),
    ?assertSame(
        "A = b\n"
        "% comment\n"
        ".\n"
    ),
    ?assertSame(
        "%foo\n"
        "1 + 2\n"
    ),
    ?assertFormat(
        "1 +\n"
        "%% foo\n"
        "2",
        "1 +\n"
        "    %% foo\n"
        "    2\n"
    ),
    ?assertFormat(
        "[%% foo\n"
        "]",
        "%% foo\n"
        "[]\n"
    ),
    ?assertFormat(
        "[\n"
        "    1 %% foo\n"
        "]",
        "[\n"
        "    %% foo\n"
        "    1\n"
        "]\n"
    ),
    ?assertFormat(
        "[1,2,3 %% foo\n"
        "]",
        "%% foo\n"
        "[1, 2, 3]\n"
    ),
    ?assertSame(
        "% foo\n"
        "\n"
        "1\n"
    ),
    ?assertSame(
        "1\n"
        "\n"
        "% foo\n"
    ),
    ?assertSame(
        "[\n"
        "    1\n"
        "\n"
        "    %% foo\n"
        "]\n"
    ),
    ?assertFormat(
        "% foo     \n"
        "1",
        "% foo\n"
        "1\n"
    ),
    ?assertSame(
        "a =\n"
        "    %% comment\n"
        "    {\n"
        "        b\n"
        "    }\n"
    ),
    ?assertSame(
        "-spec foo() ->\n"
        "    %% comment\n"
        "    term()\n"
        "    %% other comment\n"
        "    | [term()]\n"
        "    %% error comment\n"
        "    | error.\n"
    ),
    ?assertFormat(
        "a, % trailing comment\n"
        "%% post comment\n"
        "b.\n",
        "% trailing comment\n"
        "a,\n"
        "%% post comment\n"
        "b.\n"
    ),
    ?assertSame(
        "a\n"
        "%% post comment\n"
        ".\n"
    ),
    ?assertFormat(
        "\"a,\n"
        "b\" % c\n"
        ".\n",
        "% c\n"
        "\"a,\\n\"\n"
        "\"b\".\n"
    ),
    ?assertFormat(
        "%% pre\n"
        "\"a,\n"
        "b\" % c\n"
        ".\n",
        "%% pre\n"
        "% c\n"
        "\"a,\\n\"\n"
        "\"b\".\n"
    ).

doc_attributes(Config) when is_list(Config) ->
    ?assertSame("-moduledoc(\"Test\").\n-moduledoc(#{since => <<\"1.0.0\">>}).\n"),
    ?assertSame("-moduledoc(\"\"\"\nTest\nMultiline\n\"\"\").\n"),
    ?assertSame("-doc(\"Test\").\n-doc(#{since => <<\"1.0.0\">>}).\ntest() -> ok.\n"),
    ?assertSame("-doc(\"Test\").\n-doc(#{since => <<\"1.0.0\">>}).\n-type t() :: ok.\n").

doc_macros(Config) when is_list(Config) ->
    %% Doc Attributes as macros is a common pattern for OTP < 27 compatibility.
    ?assertSame("?MODULEDOC(\"Test\").\n?MODULEDOC(#{since => <<\"1.0.0\">>}).\n"),
    ?assertSame("?DOC(\"Test\").\n?DOC(#{since => <<\"1.0.0\">>}).\ntest() -> ok.\n"),
    ?assertSame("?DOC(\"Test\").\n?DOC(#{since => <<\"1.0.0\">>}).\n-type t() :: ok.\n").
