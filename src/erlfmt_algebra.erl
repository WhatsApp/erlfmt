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

-export_type([text/0, str/0]).

-type text() :: unicode:chardata().

-record(string, {length :: non_neg_integer(), text :: text()}).

-opaque str() :: #string{}.

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
