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
%% limitations under the License

%% See assert_diagnostic.hrl.

-module(assert_diagnostic).

-export([
    format/2,
    check_length/2,
    check_elements/3
]).

format(T, Args) ->
    lists:flatten(io_lib:format(T, Args)).

check_length(A, B) ->
    case length(B) - length(A) of
        0 ->
            [];
        D when D > 0 ->
            [
                format("Actual list has ~p unexpected items: ~p", [
                    D,
                    lists:sublist(B, length(A) + 1, D)
                ])
            ];
        D ->
            [
                format("Actual list lacks ~p expected items: ~p", [
                    -D,
                    lists:sublist(A, length(B) + 1, -D)
                ])
            ]
    end.

%% Collect all differences for each index I.
%% Very naive for now. Don't try to 'diff':
%% No detection of inserted nor deleted items.
check_elements([], _, _) ->
    [];
check_elements(_, [], _) ->
    [];
check_elements([H | T1], [H | T2], I) ->
    check_elements(T1, T2, I + 1);
check_elements([H1 | T1], [H2 | T2], I) ->
    [
        format(
            "Item ~p differs:~n"
            "Expected: ~p~n"
            "Value:    ~p",
            [I, H1, H2]
        )
        | check_elements(T1, T2, I + 1)
    ].
