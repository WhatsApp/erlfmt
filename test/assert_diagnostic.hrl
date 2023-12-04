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
%% limitations under the License

%% Smarter comparisons for lists and maps.
%% It reports main differences rather whole structures,
%% making the diagnostic easier to read.

%% Naive report:
%% * Detect length mismatch
%%   (supernumerous items or missing ones at **end** of list).
%% * Detect item mismatch for each index.
%% * Does *not* detect item insertion/deletion (a la diff).
-define(assertListEqual(A, B),
    % In test framework and assert context:
    % - A is expected list
    % - B is actual list
    case A of
        B ->
            ok;
        _ ->
            Errs =
                assert_diagnostic:check_length(A, B) ++
                    assert_diagnostic:check_elements(A, B, 1),
            ?assert(false, Errs)
    end
).

%% Naive report:
%% * Stop after first mismatch rather than collecting all differences.
-define(assertMapsEqual(A, B),
    [
        ?assert(
            maps:is_key(K, B),
            assert_diagnostic:format("Item ~p => ~p not in result ~p", [K, V, B])
        )
     || {K, V} <- maps:to_list(A)
    ],
    [
        ?assert(
            maps:is_key(K, A),
            assert_diagnostic:format("Item ~p => ~p not in expected ~p", [K, V, A])
        )
     || {K, V} <- maps:to_list(B)
    ],
    [
        ?assertEqual(maps:get(K, A), maps:get(K, B), assert_diagnostic:format("For key ~p", [K]))
     || K <- maps:keys(A)
    ]
).
