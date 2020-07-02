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
-module(erlfmt_algebra_SUITE).

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
    test_empty/1,
    test_binary/1,
    test_string/1,
    test_strict_break/1,
    test_flex_break/1,
    test_break/1,
    test_space/1,
    test_always_nest/1,
    test_break_nest/1,
    test_line/1,
    test_group/1,
    test_force_and_cancel/1,
    test_groups_with_lines/1,
    test_infinite_width/1,
    test_concat/1,
    test_docs/1,
    test_group_string_concat/1
]).

-import(erlfmt_algebra, [
    force_breaks/0,
    group/1,
    format/2,
    string/1,
    empty/0,
    break/0,
    break/1,
    break/2,
    break/3,
    flex_break/1,
    flex_break/0,
    flex_break/2,
    flex_break/3,
    space/2,
    nest/2,
    nest/3,
    line/0,
    line/1,
    line/2,
    concat/1,
    concat/2,
    concat/3,
    next_break_fits/1,
    next_break_fits/2,
    fold_doc/2
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
    [].

all() ->
    [
        test_empty,
        test_binary,
        test_string,
        test_strict_break,
        test_flex_break,
        test_break,
        test_flex_break,
        test_space,
        test_always_nest,
        test_break_nest,
        test_line,
        test_group,
        test_force_and_cancel,
        test_groups_with_lines,
        test_infinite_width,
        test_concat,
        test_docs,
        test_group_string_concat
    ].

% doctest Inspect.Algebra

test_empty(Config) when is_list(Config) ->
    ?assertEqual(doc_nil, empty()),
    ?assertEqual(<<"">>, render(empty(), 80)).

test_binary(Config) when is_list(Config) ->
    ?assertEqual(<<"_">>, render(<<"_">>, 80)).

test_string(Config) when is_list(Config) ->
    ?assertEqual({doc_string, <<"ólá"/utf8>>, 3}, string(<<"ólá"/utf8>>)),
    ?assertEqual(
        <<"ólá mundo"/utf8>>,
        render(break(string(<<"ólá"/utf8>>), <<" ">>, string(<<"mundo">>)), 80)
    ).

test_strict_break(Config) when is_list(Config) ->
    ?assertEqual({doc_break, <<"break">>, strict}, break(<<"break">>)),
    ?assertEqual({doc_break, <<" ">>, strict}, break()),

    ?assertEqual(<<"_">>, render(break(<<"_">>), 80)),
    ?assertEqual(
        <<"foo\nbar\nbaz">>,
        render(break(<<"foo">>, <<" ">>, break(<<"bar">>, <<" ">>, <<"baz">>)), 10)
    ).

test_flex_break(Config) when is_list(Config) ->
    ?assertEqual({doc_break, <<"break">>, flex}, flex_break(<<"break">>)),
    ?assertEqual({doc_break, <<" ">>, flex}, flex_break()),

    ?assertEqual(<<"_">>, render(flex_break(<<"_">>), 80)),
    ?assertEqual(
        <<"foo bar\nbaz">>,
        render(
            flex_break(<<"foo">>, <<" ">>, flex_break(<<"bar">>, <<" ">>, <<"baz">>)),
            10
        )
    ),

    ?assertEqual(
        {doc_cons, <<"a">>, {doc_cons, {doc_break, <<"->">>, flex}, <<"b">>}},
        flex_break(<<"a">>, <<"->">>, <<"b">>)
    ),
    ?assertEqual(flex_break(<<"a">>, <<"b">>), flex_break(<<"a">>, <<" ">>, <<"b">>)).

test_break(Config) when is_list(Config) ->
    ?assertEqual(
        {doc_cons, <<"a">>, {doc_cons, {doc_break, <<"->">>, strict}, <<"b">>}},
        break(<<"a">>, <<"->">>, <<"b">>)
    ),
    ?assertEqual(break(<<"a">>, <<"b">>), break(<<"a">>, <<" ">>, <<"b">>)).

test_space(Config) when is_list(Config) ->
    ?assertEqual({doc_string, [<<"a">>, <<" ">> | <<"b">>], 3}, space(<<"a">>, <<"b">>)),

    ?assertEqual(<<"a b">>, render(space(<<"a">>, <<"b">>), 80)).

test_always_nest(Config) when is_list(Config) ->
    ?assertEqual({doc_nest, empty(), 1, always}, nest(empty(), 1)),
    ?assertEqual(empty(), nest(empty(), 0)),

    ?assertEqual(<<"a">>, render(nest(<<"a">>, 1), 80)),
    ?assertEqual(<<"a\n b">>, render(nest(break(<<"a">>, <<"b">>), 1), 2)),
    ?assertEqual(<<"a\n b">>, render(nest(line(<<"a">>, <<"b">>), 1), 20)).

test_break_nest(Config) when is_list(Config) ->
    ?assertEqual({doc_nest, empty(), 1, break}, nest(empty(), 1, break)),
    ?assertEqual(empty(), nest(empty(), 0, break)),

    ?assertEqual(<<"a">>, render(nest(<<"a">>, 1, break), 80)),
    ?assertEqual(<<"a\n b">>, render(nest(break(<<"a">>, <<"b">>), 1, break), 2)),
    ?assertEqual(<<"a\nb">>, render(nest(line(<<"a">>, <<"b">>), 1, break), 20)).

test_line(Config) when is_list(Config) ->
    ?assertEqual(
        {doc_cons, <<"a">>, {doc_cons, {doc_line, 1}, <<"b">>}},
        line(<<"a">>, <<"b">>)
    ),
    ?assertEqual({doc_line, 2}, line(2)),

    ?assertEqual(
        <<"aaa bbb\nccc ddd">>,
        render(line(break(<<"aaa">>, <<"bbb">>), break(<<"ccc">>, <<"ddd">>)), 10)
    ),
    ?assertEqual(<<"a\n\nb">>, render(concat([<<"a">>, line(2), <<"b">>]), 80)).

test_group(Config) when is_list(Config) ->
    ?assertEqual({doc_group, <<"ab">>}, group(<<"ab">>)),
    ?assertEqual({doc_group, empty()}, group(empty())),

    Doc = concat(break(break(break(<<"hello">>, <<"a">>), <<"b">>), <<"c">>), <<"d">>),
    ?assertEqual(<<"hello\na\nb\ncd">>, render(group(Doc), 5)).

test_force_and_cancel(Config) when is_list(Config) ->
    ?assertEqual(doc_force_breaks, force_breaks()),

    ?assertEqual({doc_fits, <<"ab">>, enabled}, next_break_fits(<<"ab">>)),
    ?assertEqual({doc_fits, empty(), enabled}, next_break_fits(empty())),

    ?assertEqual({doc_fits, <<"ab">>, disabled}, next_break_fits(<<"ab">>, disabled)),
    ?assertEqual({doc_fits, empty(), disabled}, next_break_fits(empty(), disabled)),

    Doc = concat(
        force_breaks(),
        break(break(break(<<"hello">>, <<"a">>), <<"b">>), <<"c">>),
        <<"d">>
    ),
    ?assertEqual(<<"hello\na\nb\ncd">>, render(Doc, 20)),
    ?assertEqual(<<"hello a b cd">>, render(next_break_fits(Doc, enabled), 20)),
    ?assertEqual(
        <<"hello\na\nb\ncd">>,
        render(next_break_fits(next_break_fits(Doc, enabled), disabled), 20)
    ).

test_groups_with_lines(Config) when is_list(Config) ->
    Doc = line(break(<<"a">>, <<"b">>), break(<<"hello">>, <<"world">>)),
    ?assertEqual(<<"a\nb\nhello\nworld">>, render(group(Doc), 5)),
    ?assertEqual(<<"a b\nhello world">>, render(group(Doc), 100)).

test_infinite_width(Config) when is_list(Config) ->
    Str = binary:copy(<<"x">>, 50),

    Doc = group(
        break(
            break(break(break(Str, <<";">>, Str), <<";">>, Str), <<";">>, Str),
            <<";">>,
            Str
        )
    ),
    ?assertEqual(
        <<Str/binary, ";", Str/binary, ";", Str/binary, ";", Str/binary, ";", Str/binary>>,
        render(Doc, infinity)
    ).

test_concat(Config) when is_list(Config) ->
    ?assertEqual(<<"foo">>, render(concat(empty(), <<"foo">>), 80)).

test_docs(Config) when is_list(Config) ->
    ?assertEqual(<<"a b">>, render(group(break(<<"a">>, <<" ">>, <<"b">>)), 80)),

    % A break inserts a break between two documents. A group
    % indicates a document that must fit the current line, otherwise
    % breaks are rendered as new lines. Let's break two docs together
    % with a break, group it and then render it.
    ?assertEqual(
        <<"aaaaaaaaaaaaaaaaaaaa b">>,
        render(group(break(binary:copy(<<"a">>, 20), <<" ">>, <<"b">>)), 80)
    ),
    % Notice the break was represented as is, because we haven't reached
    % a line limit. Once we do, it is replaced by a newline:
    ?assertEqual(
        <<"aaaaaaaaaaaaaaaaaaaa\nb">>,
        render(group(break(binary:copy(<<"a">>, 20), <<" ">>, <<"b">>)), 10)
    ),

    ?assertEqual(
        <<"olá\nmundo"/utf8>>,
        render(group(break(<<"olá"/utf8>>, <<" ">>, <<"mundo">>)), 9)
    ),
    ?assertEqual(
        <<"olá mundo"/utf8>>,
        render(group(break(string(<<"olá"/utf8>>), <<" ">>, <<"mundo">>)), 9)
    ),

    ?assertEqual(
        <<"helloworld">>,
        render(concat(<<"hello">>, <<"world">>), 80)
    ),

    ?assertEqual(
        <<"abc">>,
        render(concat([<<"a">>, <<"b">>, <<"c">>]), 80)
    ),

    ?assertEqual(
        <<"hello\n     world">>,
        render(group(nest(break(<<"hello">>, <<"world">>), 5)), 5)
    ),

    ?assertEqual(
        <<"a\tb">>,
        render(concat([<<"a">>, break(<<"\t">>), <<"b">>]), 80)
    ),
    ?assertEqual(
        <<"aaaaaaaaaaaaaaaaaaaa\nb">>,
        render(group(concat([binary:copy(<<"a">>, 20), break(<<"\t">>), <<"b">>])), 10)
    ),

    ?assertEqual(<<"hello world">>, render(break(<<"hello">>, <<"world">>), 80)),
    ?assertEqual(<<"hello\tworld">>, render(break(<<"hello">>, <<"\t">>, <<"world">>), 80)),

    Doc1 = group(
        concat(
            group(concat(<<"Hello,">>, concat(break(), <<"A">>))),
            concat(break(), <<"B">>)
        )
    ),
    ?assertEqual(<<"Hello, A B">>, render(Doc1, 80)),
    ?assertEqual(<<"Hello,\nA\nB">>, render(Doc1, 6)),

    ?assertEqual(<<"Hughes Wadler">>, render(space(<<"Hughes">>, <<"Wadler">>), 5)),
    ?assertEqual(
        <<"Hughes\nWadler">>,
        render(concat(concat(<<"Hughes">>, line()), <<"Wadler">>), 80)
    ),
    ?assertEqual(<<"Hughes\nWadler">>, render(line(<<"Hughes">>, <<"Wadler">>), 80)),

    ?assertEqual(
        <<"A!B!C">>,
        render(
            fold_doc(fun (D, Acc) -> concat([D, <<"!">>, Acc]) end, [
                <<"A">>,
                <<"B">>,
                <<"C">>
            ]),
            80
        )
    ),

    Doc2 = group(break(<<"hello">>, <<" ">>, <<"world">>)),
    ?assertEqual(<<"hello world">>, render(Doc2, 30)),
    ?assertEqual(<<"hello\nworld">>, render(Doc2, 10)).

test_group_string_concat(Config) when is_list(Config) ->
    A20 = binary:copy(<<"a">>, 20),
    Doc = concat(group(concat([A20, break(), <<"b">>])), <<"c">>),

    ?assertEqual(
        <<A20/binary, "\nbc">>,
        render(Doc, 10)
    ),
    ?assertEqual(
        <<A20/binary, "\nbc">>,
        render(Doc, 22)
    ),
    ?assertEqual(
        <<A20/binary, " bc">>,
        render(Doc, 23)
    ).

render(Doc, Limit) ->
    unicode:characters_to_binary(format(group(Doc), Limit)).
