%%%-------------------------------------------------------------------
%%% @author Michał Muskała <micmus@whatsapp.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%%     Tests erlfmt_algebra
%%% @end
%%% @see http://erlang.org/doc/man/common_test.html
%%% -------------------------------------------------------------------

-module(erlfmt_algebra_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-oncall("whatsapp_erlang_team").

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
    string_append_case/1,
    string_spaces_case/1
]).

-import(erlfmt_algebra, [string_new/1, string_append/2, string_text/1, string_spaces/1, string_length/1]).

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
        {string_api, [parallel], [string_append_case, string_spaces_case]}
    ].

all() ->
    [{group, string_api}].

%%--------------------------------------------------------------------
%% TEST CASESS

string_append_equal_prop() ->
    ?FORALL({Left, Right}, {utf8(), utf8()}, begin
        Appended = string_append(string_new(Left), string_new(Right)),
        string:equal(string_text(Appended), [Left | Right])
    end).

string_append_length_prop() ->
    ?FORALL({Left, Right}, {closed_utf8(), closed_utf8()}, begin
        Appended = string_append(string_new(Left), string_new(Right)),
        string_length(Appended) =:= string:length(string_text(Appended))
    end).

string_append_case(Config) when is_list(Config) ->
    ct_proper:quickcheck(string_append_equal_prop()),
    ct_proper:quickcheck(string_append_length_prop()).

string_spaces_prop() ->
    ?FORALL(Count, non_neg_integer(), begin
        string:length(string_text(string_spaces(Count))) =:= Count
    end).

string_spaces_case(Config) when is_list(Config) ->
    ct_proper:quickcheck(string_spaces_prop()).


%% It's possible for the utf8 generator to produce strings that start or end with
%% a decomposed accent or something else like this - this means that when appended
%% it composes into one grapheme with the other string and lengths are off.
closed_utf8() ->
    ?SUCHTHAT(Str, utf8(), begin
        Length = string:length(Str),
        string:length([" " | Str]) =/= Length andalso string:length([Str | " "]) =/= Length
    end).
