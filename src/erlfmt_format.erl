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
-module(erlfmt_format).

-export([expr_to_algebra/1]).

-import(erlfmt_algebra, [
    document_text/1,
    document_combine/2,
    document_flush/1,
    document_choice/2
]).

-spec expr_to_algebra(erlfmt_parse:abstract_form()) -> erlfmt_algebra:document().
expr_to_algebra({integer, Meta, _Value}) ->
    {text, Text} = proplists:lookup(text, Meta),
    document_text(format_integer(Text)).

%% TODO: handle underscores once on OTP 23
format_integer([B1, B2, $# | Digits]) -> [B1, B2, $# | string:uppercase(Digits)];
format_integer(Other) -> Other.
