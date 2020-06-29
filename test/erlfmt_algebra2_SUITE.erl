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
-module(erlfmt_algebra2_SUITE).

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
    test_glue/1,
    test_flex_glue/1,
    test_space/1,
    test_always_nest/1,
    test_break_nest/1,
    test_cursor_nest/1,
    test_reset_nest/1,
    test_line/1,
    test_self_group/1,
    test_inherit_group/1,
    test_collapse/1,
    test_force_and_cancel/1,
    test_groups_with_lines/1,
    test_infinite_width/1,
    test_container/1,
    test_concat/1,
    test_docs/1
]).

-import(erlfmt_algebra2, [
    group/1,
    group/2,
    format/2,
    string/1,
    empty/0,
    break/0,
    break/1,
    flex_break/1,
    flex_break/0,
    glue/2,
    glue/3,
    flex_glue/2,
    flex_glue/3,
    space/2,
    nest/2,
    nest/3,
    line/0,
    line/2,
    concat/1,
    concat/2,
    collapse_lines/1,
    force_unfit/1,
    next_break_fits/1,
    next_break_fits/2,
    container_doc/3,
    container_doc/4,
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
        test_glue,
        test_flex_glue,
        test_space,
        test_always_nest,
        test_break_nest,
        test_cursor_nest,
        test_reset_nest,
        test_line,
        test_self_group,
        test_inherit_group,
        test_collapse,
        test_force_and_cancel,
        test_groups_with_lines,
        test_infinite_width,
        test_container,
        test_concat,
        test_docs
    ].

% doctest Inspect.Algebra

test_empty(Config) when is_list(Config) ->
    ?assertEqual(doc_nil, empty()),
    ?assertEqual(<<"">>, render(empty(), 80)).

test_binary(Config) when is_list(Config) ->
    ?assertEqual(<<"_">>, render(<<"_">>, 80)).

test_string(Config) when is_list(Config) ->
    ?assertEqual({doc_string, <<"ólá"/utf8>>, 3}, string(<<"ólá"/utf8>>)),
    ?assertEqual(<<"ólá mundo"/utf8>>, render(glue(string(<<"ólá"/utf8>>), <<" ">>, string(<<"mundo">>)), 80)).

test_strict_break(Config) when is_list(Config) ->
    ?assertEqual({doc_break, <<"break">>, strict}, break(<<"break">>)),
    ?assertEqual({doc_break, <<" ">>, strict}, break()),

    ?assertEqual(<<"_">>, render(break(<<"_">>), 80)),
    ?assertEqual(<<"foo\nbar\nbaz">>, render(glue(<<"foo">>, <<" ">>, glue(<<"bar">>, <<" ">>, <<"baz">>)), 10)).

test_flex_break(Config) when is_list(Config) ->
    ?assertEqual({doc_break, <<"break">>, flex}, flex_break(<<"break">>)),
    ?assertEqual({doc_break, <<" ">>, flex}, flex_break()),

    ?assertEqual(<<"_">>, render(flex_break(<<"_">>), 80)),
    ?assertEqual(<<"foo bar\nbaz">>, render(flex_glue(<<"foo">>, <<" ">>, flex_glue(<<"bar">>, <<" ">>, <<"baz">>)), 10)).

test_glue(Config) when is_list(Config) ->
    ?assertEqual({doc_cons, <<"a">>, {doc_cons, {doc_break, <<"->">>, strict}, <<"b">>}}, glue(<<"a">>, <<"->">>, <<"b">>)),
    ?assertEqual(glue(<<"a">>, <<"b">>), glue(<<"a">>, <<" ">>, <<"b">>)).

test_flex_glue(Config) when is_list(Config) ->
    ?assertEqual({doc_cons, <<"a">>, {doc_cons, {doc_break, <<"->">>, flex}, <<"b">>}}, flex_glue(<<"a">>, <<"->">>, <<"b">>)),
    ?assertEqual(flex_glue(<<"a">>, <<"b">>), flex_glue(<<"a">>, <<" ">>, <<"b">>)).

test_space(Config) when is_list(Config) ->
    ?assertEqual({doc_cons, <<"a">>, {doc_cons, <<" ">>, <<"b">>}}, space(<<"a">>, <<"b">>)).

test_always_nest(Config) when is_list(Config) ->
    ?assertEqual({doc_nest, empty(), 1, always}, nest(empty(), 1)),
    ?assertEqual(empty(), nest(empty(), 0)),

    ?assertEqual(<<"a">>, render(nest(<<"a">>, 1), 80)),
    ?assertEqual(<<"a\n b">>, render(nest(glue(<<"a">>, <<"b">>), 1), 2)),
    ?assertEqual(<<"a\n b">>, render(nest(line(<<"a">>, <<"b">>), 1), 20)).

test_break_nest(Config) when is_list(Config) ->
    ?assertEqual({doc_nest, empty(), 1, break}, nest(empty(), 1, break)),
    ?assertEqual(empty(), nest(empty(), 0, break)),

    ?assertEqual(<<"a">>, render(nest(<<"a">>, 1, break), 80)),
    ?assertEqual(<<"a\n b">>, render(nest(glue(<<"a">>, <<"b">>), 1, break), 2)),
    ?assertEqual(<<"a\nb">>, render(nest(line(<<"a">>, <<"b">>), 1, break), 20)).

test_cursor_nest(Config) when is_list(Config) ->
    ?assertEqual({doc_nest, empty(), cursor, always}, nest(empty(), cursor)),

    ?assertEqual(<<"a">>, render(nest(<<"a">>, cursor), 80)),
    ?assertEqual(<<"prefix a\n       b">>, render(concat(<<"prefix ">>, nest(glue(<<"a">>, <<"b">>), cursor)), 2)),
    ?assertEqual(<<"prefix a\n       b">>, render(concat(<<"prefix ">>, nest(line(<<"a">>, <<"b">>), cursor)), 2)).

test_reset_nest(Config) when is_list(Config) ->
    ?assertEqual({doc_nest, empty(), reset, always}, nest(empty(), reset)),

    ?assertEqual(<<"a">>, render(nest(<<"a">>, reset), 80)),
    ?assertEqual(<<"a\nb">>, render(nest(nest(glue(<<"a">>, <<"b">>), reset), 10), 2)),
    ?assertEqual(<<"a\nb">>, render(nest(nest(line(<<"a">>, <<"b">>), reset), 10), 2)).

test_line(Config) when is_list(Config) ->
    ?assertEqual({doc_cons, <<"a">>, {doc_cons, doc_line, <<"b">>}}, line(<<"a">>, <<"b">>)),

    ?assertEqual(<<"aaa bbb\nccc ddd">>, render(line(glue(<<"aaa">>, <<"bbb">>), glue(<<"ccc">>, <<"ddd">>)), 10)).

test_self_group(Config) when is_list(Config) ->
    ?assertEqual({doc_group, <<"ab">>, self}, group(<<"ab">>)),
    ?assertEqual({doc_group, empty(), self}, group(empty())),

    Doc = concat(glue(glue(glue(<<"hello">>, <<"a">>), <<"b">>), <<"c">>), <<"d">>),
    ?assertEqual(<<"hello\na\nb\ncd">>, render(group(Doc), 5)).

test_inherit_group(Config) when is_list(Config) ->
    ?assertEqual({doc_group, <<"ab">>, inherit}, group(<<"ab">>, inherit)),
    ?assertEqual({doc_group, empty(), inherit}, group(empty(), inherit)),

    Doc1 = concat(glue(glue(group(glue(<<"a">>, <<"b">>), self), <<"c">>), <<"d">>), <<"hello">>),
    ?assertEqual(<<"a b\nc\ndhello">>, render(group(Doc1), 5)),

    Doc2 = concat(glue(glue(group(glue(<<"a">>, <<"b">>), inherit), <<"c">>), <<"d">>), <<"hello">>),
    ?assertEqual(<<"a\nb\nc\ndhello">>, render(group(Doc2), 5)).

test_collapse(Config) when is_list(Config) ->
    ?assertEqual({doc_collapse, 3}, collapse_lines(3)),

    Doc1 = concat([collapse_lines(2), line(), line(), line()]),
    ?assertEqual(<<"\n\n">>, render(Doc1, 10)),
    ?assertEqual(<<"\n\n  ">>, render(nest(Doc1, 2), 10)),

    Doc2 = concat([collapse_lines(2), line(), line()]),
    ?assertEqual(<<"\n\n">>, render(Doc2, 10)),
    ?assertEqual(<<"\n\n  ">>, render(nest(Doc2, 2), 10)),

    Doc3 = concat([collapse_lines(2), line()]),
    ?assertEqual(<<"\n">>, render(Doc3, 10)),
    ?assertEqual(<<"\n  ">>, render(nest(Doc3, 2), 10)),

    Doc4 = concat([collapse_lines(2), line(), <<"">>, line(), <<"">>, line()]),
    ?assertEqual(<<"\n\n">>, render(Doc4, 10)),
    ?assertEqual(<<"\n\n  ">>, render(nest(Doc4, 2), 10)),

    Doc5 = concat([collapse_lines(2), line(), <<"foo">>, line(), <<"bar">>, line()]),
    ?assertEqual(<<"\nfoo\nbar\n">>, render(Doc5, 10)),
    ?assertEqual(<<"\n  foo\n  bar\n  ">>, render(nest(Doc5, 2), 10)).

test_force_and_cancel(Config) when is_list(Config) ->
    ?assertEqual({doc_force, <<"ab">>}, force_unfit(<<"ab">>)),
    ?assertEqual({doc_force, empty()}, force_unfit(empty())),

    ?assertEqual({doc_fits, <<"ab">>, enabled}, next_break_fits(<<"ab">>)),
    ?assertEqual({doc_fits, empty(), enabled}, next_break_fits(empty())),

    ?assertEqual({doc_fits, <<"ab">>, disabled}, next_break_fits(<<"ab">>, disabled)),
    ?assertEqual({doc_fits, empty(), disabled}, next_break_fits(empty(), disabled)),

    Doc = force_unfit(concat(glue(glue(glue(<<"hello">>, <<"a">>), <<"b">>), <<"c">>), <<"d">>)),
    ?assertEqual(<<"hello\na\nb\ncd">>, render(Doc, 20)),
    ?assertEqual(<<"hello a b cd">>, render(next_break_fits(Doc, enabled), 20)),
    ?assertEqual(<<"hello\na\nb\ncd">>, render(next_break_fits(next_break_fits(Doc, enabled), disabled), 20)).

test_groups_with_lines(Config) when is_list(Config) ->
    Doc = line(glue(<<"a">>, <<"b">>), glue(<<"hello">>, <<"world">>)),
    ?assertEqual(<<"a\nb\nhello\nworld">>, render(group(Doc), 5)),
    ?assertEqual(<<"a b\nhello world">>, render(group(Doc), 100)).

test_infinite_width(Config) when is_list(Config) ->
    Str = binary:copy(<<"x">>, 50),

    Doc = group(glue(glue(glue(glue(Str, <<";">>, Str), <<";">>, Str), <<";">>, Str), <<";">>, Str)),
    ?assertEqual(
        <<Str/binary, ";", Str/binary, ";", Str/binary, ";", Str/binary, ";", Str/binary>>,
        render(Doc, infinity)
    ).

test_container(Config) when is_list(Config) ->
    Render = fun (Docs) -> render(container_doc(<<"[">>, Docs, <<"]">>, #{separator => <<",">>}), 80) end,

    ?assertEqual(<<"[]">>, Render([])),
    ?assertEqual(<<"[]">>, Render([empty()])),
    ?assertEqual(<<"[]">>, Render([empty(), empty()])),
    ?assertEqual(<<"[a]">>, Render([<<"a">>])),
    ?assertEqual(<<"[a]">>, Render([<<"a">>, empty()])),
    ?assertEqual(<<"[a]">>, Render([empty(), <<"a">>])),
    ?assertEqual(<<"[a, b]">>, Render([<<"a">>, empty(), <<"b">>])),
    ?assertEqual(<<"[a, b]">>, Render([empty(), <<"a">>, <<"b">>])),
    ?assertEqual(<<"[a, b]">>, Render([<<"a">>, <<"b">>, empty()])),
    ?assertEqual(<<"[a, b | c]">>, Render([<<"a">>, <<"b">> | <<"c">>])),
    ?assertEqual(<<"[a | b]">>, Render([<<"a">> | <<"b">>])),
    ?assertEqual(<<"[a]">>, Render([<<"a">> | empty()])),
    ?assertEqual(<<"[b]">>, Render([empty() | <<"b">>])).

test_concat(Config) when is_list(Config) ->
    ?assertEqual(<<"foo">>, render(concat(empty(), <<"foo">>), 80)).

test_docs(Config) when is_list(Config) ->
    ?assertEqual(<<"a b">>, render(group(glue(<<"a">>, <<" ">>, <<"b">>)), 80)),

    % A glue inserts a break between two documents. A group
    % indicates a document that must fit the current line, otherwise
    % breaks are rendered as new lines. Let's glue two docs together
    % with a break, group it and then render it.
    ?assertEqual(<<"aaaaaaaaaaaaaaaaaaaa b">>, render(group(glue(binary:copy(<<"a">>, 20), <<" ">>, <<"b">>)), 80)),
    % Notice the break was represented as is, because we haven't reached
    % a line limit. Once we do, it is replaced by a newline:
    ?assertEqual(<<"aaaaaaaaaaaaaaaaaaaa\nb">>, render(group(glue(binary:copy(<<"a">>, 20), <<" ">>, <<"b">>)), 10)),

    ?assertEqual(
        <<"[1,\n 2,\n 3,\n 4,\n 5]">>,
        render(container_doc(<<"[">>, [integer_to_binary(I) || I <- lists:seq(1, 5)], <<"]">>), 5)
    ),
    ?assertEqual(
        <<"[a! b]">>,
        render(container_doc(<<"[">>, [<<"a">>, <<"b">>], <<"]">>, #{separator => <<"!">>}), 20)
    ),

    ?assertEqual(
        <<"olá\nmundo"/utf8>>,
        render(group(glue(<<"olá"/utf8>>, <<" ">>, <<"mundo">>)), 9)
    ),
    ?assertEqual(
       <<"olá mundo"/utf8>>,
       render(group(glue(string(<<"olá"/utf8>>), <<" ">>, <<"mundo">>)), 9)
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
        render(group(nest(glue(<<"hello">>, <<"world">>), 5)), 5)
    ),

    ?assertEqual(
        <<"a\tb">>,
        render(concat([<<"a">>, break(<<"\t">>), <<"b">>]), 80)
    ),
    ?assertEqual(
        <<"aaaaaaaaaaaaaaaaaaaa\nb">>,
        render(group(concat([binary:copy(<<"a">>, 20), break(<<"\t">>), <<"b">>])), 10)
    ),

    ?assertEqual(<<"hello world">>, render(glue(<<"hello">>, <<"world">>), 80)),
    ?assertEqual(<<"hello\tworld">>, render(glue(<<"hello">>, <<"\t">>, <<"world">>), 80)),

    Doc1 = group(concat(
        group(concat(<<"Hello,">>, concat(break(), <<"A">>))),
        concat(break(), <<"B">>)
    )),
    ?assertEqual(<<"Hello, A B">>, render(Doc1, 80)),
    ?assertEqual(<<"Hello,\nA\nB">>, render(Doc1, 6)),

    ?assertEqual(<<"Hughes Wadler">>, render(space(<<"Hughes">>, <<"Wadler">>), 5)),
    ?assertEqual(<<"Hughes\nWadler">>, render(concat(concat(<<"Hughes">>, line()), <<"Wadler">>), 80)),
    ?assertEqual(<<"Hughes\nWadler">>, render(line(<<"Hughes">>, <<"Wadler">>), 80)),

    ?assertEqual(
        <<"A!B!C">>,
        render(fold_doc(fun (D, Acc) -> concat([D, <<"!">>, Acc]) end, [<<"A">>, <<"B">>, <<"C">>]), 80)
    ),

    Doc2 = group(glue(<<"hello">>, <<" ">>, <<"world">>)),
    ?assertEqual(<<"hello world">>, render(Doc2, 30)),
    ?assertEqual(<<"hello\nworld">>, render(Doc2, 10)).

render(Doc, Limit) ->
    unicode:characters_to_binary(format(group(Doc), Limit)).
