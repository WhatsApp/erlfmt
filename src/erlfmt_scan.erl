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
-module(erlfmt_scan).

-export([io_form/1, string_form/1, continue/1, last_form_string/1]).

-define(ERL_SCAN_OPTS, [text, return_white_spaces, return_comments]).

-define(START_LOCARION, {1, 1}).

-record(state, {cont, state, loc, original}).

%% FIXME
-type comment() :: tuple().

io_form(IO) -> io_continue(IO, ?START_LOCARION).

io_continue(IO, Loc0) ->
    case io:scan_erl_form(IO, "", Loc0, ?ERL_SCAN_OPTS) of
        {ok, Tokens0, Loc} ->
            {Tokens, Comments} = preprocess_tokens(Tokens0),
            State = #state{cont = fun io_continue/2, state = IO, loc = Loc, original = Tokens0},
            {ok, Tokens, Comments, State};
        {error, Reason} ->
            {error, {Loc0, file, Reason}};
        Other ->
            Other
    end.

string_form(String) -> string_continue(String, ?START_LOCARION).

string_continue(String, Loc0) ->
    case erl_scan:tokens([], String, Loc0, ?ERL_SCAN_OPTS) of
        {done, {ok, Tokens0, Loc}, Rest} ->
            {Tokens, Comments} = preprocess_tokens(Tokens0),
            State = #state{cont = fun string_continue/2, state = Rest, loc = Loc, original = Tokens0},
            {ok, Tokens, Comments, State};
        {done, Other, _Rest} ->
            Other;
        {more, Cont} ->
            case erl_scan:tokens(Cont, eof, Loc0, ?ERL_SCAN_OPTS) of
                {done, {ok, Tokens0, Loc}, Rest} ->
                    {Tokens, Comments} = preprocess_tokens(Tokens0),
                    State = #state{cont = fun string_continue/2, state = Rest, loc = Loc, original = Tokens0},
                    {ok, Tokens, Comments, State};
                {done, Other, _Rest} ->
                    Other
            end
    end.

continue(#state{cont = Fun, state = State, loc = Loc}) ->
    Fun(State, Loc).

last_form_string(#state{original = Tokens}) ->
    [stringify_token(Token) || Token <- Tokens].

%% TODO: make smarter
stringify_token(Token) -> erl_anno:text(element(2, Token)).

-spec preprocess_tokens([erl_scan:token()]) -> {[erl_scan:token()], [comment()]}.
preprocess_tokens(Tokens) -> preprocess_tokens(Tokens, [], []).

%% TODO: annotate [, {, <<, (, -> with following newline info to control user folding/expanding
preprocess_tokens([{comment, _, _} = Comment0 | Rest0], Acc, CAcc) ->
    {Comment, Rest} = collect_comments(Rest0, Comment0),
    preprocess_tokens(Rest, Acc, [Comment | CAcc]);
preprocess_tokens([{white_space, _, _} | Rest], Acc, CAcc) ->
    preprocess_tokens(Rest, Acc, CAcc);
preprocess_tokens([Other | Rest], Acc, CAcc) ->
    preprocess_tokens(Rest, [Other | Acc], CAcc);
preprocess_tokens([], Acc, CAcc) ->
    {lists:reverse(Acc), lists:reverse(CAcc)}.

collect_comments(Tokens, {comment, Meta, Text}) ->
    Line = erl_anno:line(Meta),
    {Texts, Rest} = collect_comments(Tokens, Line, [Text]),
    {{comment, erlfmt_recomment:delete_anno(text, Meta), Texts}, Rest}.

collect_comments([{comment, Meta, Text} = Comment | Rest], Line, Acc) ->
    case erl_anno:line(Meta) of
        NextLine when NextLine =:= Line + 1 ->
            collect_comments(Rest, NextLine, [Text | Acc]);
        _ ->
            {lists:reverse(Acc), [Comment | Rest]}
    end;
collect_comments(Other, _Line, Acc) ->
    {lists:reverse(Acc), Other}.
