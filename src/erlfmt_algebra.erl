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
-module(erlfmt_algebra).

-export([string_new/1, string_append/2, string_spaces/1, string_text/1, string_length/1]).
-export([lines_new/1, lines_combine/2, lines_flush/1, lines_render/1]).

-export_type([text/0, str/0, lines/0]).

-type text() :: unicode:chardata().

-record(string, {length :: non_neg_integer(), text :: text()}).

-opaque str() :: #string{}.

-record(lines_combine, {left :: lines(), right :: lines()}).

-record(lines_flush, {lines :: lines()}).

-opaque lines() :: #string{} | #lines_combine{} | #lines_flush{}.

-spec string_new(text()) -> str().
string_new(Text) -> #string{length = string:length(Text), text = Text}.

-spec string_spaces(integer()) -> str().
string_spaces(Count) -> #string{length = Count, text = binary:copy(<<" ">>, Count)}.

-spec string_append(str(), str()) -> str().
string_append(Left, Right) ->
    #string{
        length = Left#string.length + Right#string.length,
        text = [Left#string.text | Right#string.text]
    }.

-spec string_text(str()) -> text().
string_text(#string{text = Text}) -> Text.

-spec string_length(str()) -> non_neg_integer().
string_length(#string{length = Length}) -> Length.

string_empty() -> #string{length = 0, text = ""}.

-spec lines_new(str()) -> lines().
lines_new(#string{} = String) -> String.

-spec lines_combine(lines(), lines()) -> lines().
lines_combine(#string{} = Left, #string{} = Right) ->
    string_append(Left, Right);
lines_combine(Left, Right) ->
    #lines_combine{left = Left, right = Right}.

-spec lines_flush(lines()) -> lines().
lines_flush(Lines) ->
    #lines_flush{lines = Lines}.

-spec lines_render(lines()) -> text().
lines_render(Lines) ->
    tl(strings_to_text(do_lines_render(Lines))).

strings_to_text([#string{text = Text} | Rest]) -> [$\n, Text | strings_to_text(Rest)];
strings_to_text([]) -> [].

%% TODO: there's probably a more efficient way to do this, possibly
%% by using a better data structure or making it tail-recursive
-spec do_lines_render(lines()) -> [str()].
do_lines_render(#string{} = Line) ->
    [Line];
do_lines_render(#lines_combine{left = Left, right = Right}) ->
    {LeftLast, LeftHeadRev} = split_last(do_lines_render(Left)),
    {RightFirst, RightTail} = split_first(do_lines_render(Right)),
    CombinedLine = string_append(LeftLast, RightFirst),
    IndentedLines = indent_lines(LeftLast#string.length, RightTail),
    lists:reverse(LeftHeadRev, [CombinedLine | IndentedLines]);
do_lines_render(#lines_flush{lines = Lines}) ->
    do_lines_render(Lines) ++ [string_empty()].

split_first([First | Rest]) -> {First, Rest}.

split_last([Line | Lines]) -> split_last(Lines, Line, []).

split_last([Line | Rest], Last, LinesRev) -> split_last(Rest, Line, [Last | LinesRev]);
split_last([], Last, LinesRev) -> {Last, LinesRev}.

indent_lines(0, Lines) -> Lines;
indent_lines(N, Lines) ->
    Offset = string_spaces(N),
    [string_append(Offset, Line) || Line <- Lines].
