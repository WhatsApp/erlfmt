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

-import(erl_anno, [text/1]).

-define(IN_RANGE(Value, Low, High), (Value) >= (Low) andalso (Value) =< (High)).
-define(IS_OCT_DIGIT(C), ?IN_RANGE(C, $0, $7)).

-spec expr_to_algebra(erlfmt_parse:abstract_form()) -> erlfmt_algebra:document().
expr_to_algebra({integer, Meta, _Value}) ->
    document_text(format_integer(text(Meta)));
expr_to_algebra({float, Meta, _Value}) ->
    document_text(format_float(text(Meta)));
expr_to_algebra({char, Meta, Value}) ->
    document_text(format_char(text(Meta), Value));
expr_to_algebra({atom, Meta, Value}) ->
    document_text(format_atom(text(Meta), Value));
expr_to_algebra({string, Meta, Value}) ->
    document_text(format_string(text(Meta), Value)).

%% TODO: handle underscores once on OTP 23
format_integer([B1, B2, $# | Digits]) -> [B1, B2, $# | string:uppercase(Digits)];
format_integer(Other) -> Other.

%% TODO: handle underscores in int part on OTP 23
format_float(FloatText) ->
    [IntPart, DecimalPart] = string:split(FloatText, "."),
    [IntPart, "." | string:lowercase(DecimalPart)].

format_char("$ ", $\s) -> "$\\s";
format_char("$\\s", $\s) -> "$\\s";
format_char([$$ | String], Value) ->
    [$$ | escape_string_loop(String, [Value], -1)].

format_atom(Text, Atom) ->
    RawString = atom_to_list(Atom),
    case erl_scan:reserved_word(Atom) orelse atom_needs_quotes(RawString) of
        true -> escape_string(Text, RawString, $');
        false -> RawString
    end.

format_string(String, Original) ->
    escape_string(String, Original, $").

atom_needs_quotes([C0 | Cs]) when C0 >= $a, C0 =< $z ->
    lists:any(fun
        (C) when ?IN_RANGE(C, $a, $z); ?IN_RANGE(C, $A, $Z); ?IN_RANGE(C, $0, $9); C =:= $_; C=:= $@ -> false;
        (_) -> true
    end, Cs);
atom_needs_quotes(_) -> true.

escape_string([Quote | Rest], Original, Quote) ->
    [Quote | escape_string_loop(Rest, Original, Quote)].

%% Remove unneeded escapes, upcase hex escapes
escape_string_loop(Tail, [], _Quote) -> Tail;
escape_string_loop([$\\, $x | EscapeAndRest], [_Escaped | Original], Quote) ->
    {Escape, Rest} = escape_hex(EscapeAndRest),
    [$\\, $x, Escape | escape_string_loop(Rest, Original, Quote)];
escape_string_loop([$\\, Escape | Rest], [Value | Original], Quote) ->
    if
        ?IS_OCT_DIGIT(Escape) ->
            case Rest of
                [D2, D3 | Rest1] when ?IS_OCT_DIGIT(D2), ?IS_OCT_DIGIT(D3) ->
                    [$\\, Escape, D2, D3 | escape_string_loop(Rest1, Original, Quote)];
                [D2 | Rest1] when ?IS_OCT_DIGIT(D2) ->
                    [$\\, Escape, D2 | escape_string_loop(Rest1, Original, Quote)];
                _ ->
                    [$\\, Escape | escape_string_loop(Rest, Original, Quote)]
            end;
        Escape =:= $s ->
            [Value | escape_string_loop(Rest, Original, Quote)];
        Escape =:= Quote; Escape =:= $\\; Escape =/= Value ->
            [$\\, Escape | escape_string_loop(Rest, Original, Quote)];
        true ->
            [Escape | escape_string_loop(Rest, Original, Quote)]
    end;
escape_string_loop([C | Rest], [C | Original], Quote) ->
    [C | escape_string_loop(Rest, Original, Quote)].

escape_hex([${ | Rest0]) ->
    [Escape, Rest] = string:split(Rest0, "}"),
    {[${, string:uppercase(Escape), $}], Rest};
escape_hex([X1, X2 | Rest]) ->
    {string:uppercase([X1, X2]), Rest}.
