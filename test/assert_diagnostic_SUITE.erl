-module(assert_diagnostic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("src/assert_diagnostic.hrl").

%% TestServer callbacks ++ test cases.
-compile(export_all).

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

%% Helper ensuring comparison of mismatching lists give the expected message.
%% Implemented as a macro to have proper line report if the test case fails.
-define(checkListEqualMessage(L0, L1, Msg),
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
    end
).

test_equal(_) ->
    L = [rifi, fifi, loulou],
    % Must return 'ok' to respect ?assert API.
    % NB: Must use a temporary result variable, doesn't work inline!
    Res = ?assertListEqual(L, L),
    ?assertEqual(ok, Res).

test_expected_is_longer(_) ->
    L0 = [coffee, brownie],
    L1 = [coffee],
    ?checkListEqualMessage(L0, L1, ["Actual list lacks 1 expected items: [brownie]"]).

test_actual_is_longer(_) ->
    L0 = [breath],
    L1 = [breath, explode],
    ?checkListEqualMessage(L0, L1, ["Actual list has 1 unexpected items: [explode]"]).

test_one_distinc_item(_) ->
    L0 = [sea, sax, sun],
    L1 = [sea, tex, sun],
    ?checkListEqualMessage(L0, L1, [
        "Item 2 differs:\n"
        "Expected: sax\n"
        "Value:    tex"
    ]).
