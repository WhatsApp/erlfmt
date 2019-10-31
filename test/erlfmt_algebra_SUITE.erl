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
-include_lib("proper/include/proper.hrl").

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
    string_spaces_case/1,
    lines_combine_case/1
]).

-define(alg, erlfmt_algebra).

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
        {string_api, [parallel], [string_append_case, string_spaces_case]},
        {lines_api, [parallel], [lines_combine_case]}
    ].

all() ->
    [{group, string_api}, {group, lines_api}].

%%--------------------------------------------------------------------
%% TEST CASESS

string_append_equal_prop() ->
    ?FORALL({Left, Right}, {str(), str()}, begin
        Appended = ?alg:string_append(Left, Right),
        string:equal(?alg:string_text(Appended), [?alg:string_text(Left) | ?alg:string_text(Right)])
    end).

string_append_length_prop() ->
    ?FORALL({Left, Right}, {str(), str()}, begin
        Appended = ?alg:string_append(Left, Right),
        ?alg:string_length(Appended) =:= string:length(?alg:string_text(Appended))
    end).

string_append_case(Config) when is_list(Config) ->
    ct_proper:quickcheck(string_append_equal_prop()),
    ct_proper:quickcheck(string_append_length_prop()).

string_spaces_prop() ->
    ?FORALL(Count, non_neg_integer(), begin
        string:length(?alg:string_text(?alg:string_spaces(Count))) =:= Count
    end).

string_spaces_case(Config) when is_list(Config) ->
    ct_proper:quickcheck(string_spaces_prop()).

lines_combine_assoc_prop() ->
    ?FORALL({Lines1, Lines2, Lines3}, {lines(), lines(), lines()}, begin
        Combined1 = ?alg:lines_combine(Lines1, ?alg:lines_combine(Lines2, Lines3)),
        Combined2 = ?alg:lines_combine(?alg:lines_combine(Lines1, Lines2), Lines3),
        string:equal(?alg:lines_render(Combined1), ?alg:lines_render(Combined2))
    end).

lines_combine_flush_prop() ->
    ?FORALL({Lines1, Lines2}, {lines(), lines()}, begin
        Combined1 = ?alg:lines_combine(?alg:lines_flush(Lines1), ?alg:lines_flush(Lines2)),
        Combined2 = ?alg:lines_flush(?alg:lines_combine(?alg:lines_flush(Lines1), Lines2)),
        string:equal(?alg:lines_render(Combined1), ?alg:lines_render(Combined2))
    end).

lines_combine_case(Config) when is_list(Config) ->
    ct_proper:quickcheck(lines_combine_assoc_prop()),
    ct_proper:quickcheck(lines_combine_flush_prop()).


%% It's possible for the utf8 generator to produce strings that start or end with
%% a decomposed accent or something else like this - this means that when appended
%% it composes into one grapheme with the other string and lengths are off.
str() ->
    ClosedUTF8 = ?SUCHTHAT(Str, utf8(), begin
        Length = string:length(Str),
        string:length([" " | Str]) =/= Length andalso string:length([Str | " "]) =/= Length
    end),
    ?LET(Str, ClosedUTF8, ?alg:string_new(string:replace(Str, [<<"\n">>, <<"\r">>], <<>>))).

lines() ->
    ?SIZED(Size, limited_lines(Size)).

limited_lines(Size) when Size =< 1 ->
    ?LET(Str, str(), ?alg:lines_new(Str));
limited_lines(Size) ->
    Self = ?LAZY(limited_lines(Size - 1)),
    union([
        ?LET(Str, str(), ?alg:lines_new(Str)),
        ?LET(Lines, Self, ?alg:lines_flush(Lines)),
        ?LET({Left, Right}, {Self, Self}, ?alg:lines_combine(Left, Right))
    ]).
