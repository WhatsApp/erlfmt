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
-module(assert_diagnostic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("test/assert_diagnostic.hrl").

%% Test server callbacks
-export([
    all/0,
    groups/0
]).

%% Test cases
-export([
    test_equal/1,
    test_expected_is_longer/1,
    test_actual_is_longer/1,
    test_one_distinc_item/1
]).

groups() ->
    [
        {my_test_group, [parallel], [
            test_equal,
            test_expected_is_longer,
            test_actual_is_longer,
            test_one_distinc_item
        ]}
    ].

all() ->
    [{group, my_test_group}].

test_equal(_) ->
    L = [rifi, fifi, loulou],
    % Must return 'ok' to respect ?assert API.
    % NB: Must use a temporary result variable, doesn't work inline!
    Res = ?assertListEqual(L, L),
    ?assertEqual(ok, Res).

test_expected_is_longer(_) ->
    L0 = [coffee, brownie],
    L1 = [coffee],
    check_list_equal_message(L0, L1, ["Actual list lacks 1 expected items: [brownie]"]).

test_actual_is_longer(_) ->
    L0 = [breath],
    L1 = [breath, explode],
    check_list_equal_message(L0, L1, ["Actual list has 1 unexpected items: [explode]"]).

test_one_distinc_item(_) ->
    L0 = [sea, sax, sun],
    L1 = [sea, tex, sun],
    check_list_equal_message(L0, L1, [
        "Item 2 differs:\n"
        "Expected: sax\n"
        "Value:    tex"
    ]).

%% Helper ensuring comparison of mismatching lists give the expected message.
check_list_equal_message(L0, L1, Msg) ->
    case (catch ?assertListEqual(L0, L1)) of
        ok ->
            ct:fail("Got 'ok', was expecting exception: ~p", [Msg]);
        % Erlang doc: For exceptions of class error, that is, run-time errors,
        %             {'EXIT',{Reason,Stack}} is returned.
        % ?assertListEqual follows ?assert API and returns Reason as:
        %             {assert, [infos]}
        {'EXIT', {{assert, Info}, _ST}} ->
            Comment = [C || {comment, C} <- Info],
            ?assertEqual([Msg], Comment);
        X ->
            ct:fail("Expected exception: ~p~nGot: ~p", [Msg, X])
    end.
