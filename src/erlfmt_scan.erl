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
-export([put_anno/3, delete_anno/2, delete_annos/2, get_anno/2, get_anno/3]).

-export_type([state/0, anno/0, token/0, comment/0]).

-define(ERL_SCAN_OPTS, [text, return_white_spaces, return_comments]).

-define(START_LOCATION, {1, 1}).

-record(state, {
    cont :: fun((term(), erl_anno:location(), [erl_scan:token()]) -> form_ret()),
    state :: term(),
    loc :: erl_anno:location(),
    original :: [erl_scan:token()],
    buffer :: [erl_scan:token()]
}).

-type comment() :: {comment, anno(), [string()]}.

-type anno() :: #{location := location(), end_location := location(), text => string(), atom() => term()}.

-type location() :: {pos_integer(), pos_integer()}.

-type token() :: {atom(), anno(), term()} | {atom(), anno()}.

-type form_ret() ::
    {ok, [token()], [comment()], state()} |
    {error, {erl_anno:location(), module(), term()}, erl_anno:location()} |
    {eof, erl_anno:location()}.

-opaque state() :: #state{}.

-spec io_form(file:io_device()) -> form_ret().
io_form(IO) ->
    case io:scan_erl_form(IO, "", ?START_LOCATION, ?ERL_SCAN_OPTS) of
        {ok, Tokens, Loc} ->
            io_continue(IO, Loc, Tokens);
        {error, Reason} ->
            {error, {?START_LOCATION, file, Reason}};
        Other ->
            Other
    end.

io_continue(IO, Loc0, Buffer0) ->
    case io:scan_erl_form(IO, "", Loc0, ?ERL_SCAN_OPTS) of
        {ok, Tokens0, Loc} ->
            {Tokens, FormTokens, Comments, Buffer} = split_tokens(Buffer0, Tokens0),
            State = #state{cont = fun io_continue/3, state = IO, loc = Loc, original = FormTokens, buffer = Buffer},
            {ok, Tokens, Comments, State};
        {eof, Loc} ->
            {Tokens, FormTokens, Comments, []} = split_tokens(Buffer0, []),
            State = #state{cont = fun eof/3, state = undefined, loc = Loc, original = FormTokens, buffer = []},
            {ok, Tokens, Comments, State};
        {error, Reason} ->
            {error, {Loc0, file, Reason}, Loc0};
        Other ->
            Other
    end.

eof(_State, Loc, _Buffer) ->
    {eof, Loc}.

-spec string_form(string()) -> form_ret().
string_form(String) ->
    case erl_scan:tokens([], String, ?START_LOCATION, ?ERL_SCAN_OPTS) of
        {done, {ok, Tokens, Loc}, Rest} ->
            string_continue(Rest, Loc, Tokens);
        {more, Cont} ->
            case erl_scan:tokens(Cont, eof, ?START_LOCATION, ?ERL_SCAN_OPTS) of
                {done, {ok, Tokens0, Loc}, eof} ->
                    {Tokens, FormTokens, Comments, []} = split_tokens(Tokens0, []),
                    State = #state{cont = fun eof/3, state = undefined, loc = Loc, original = FormTokens, buffer = []},
                    {ok, Tokens, Comments, State};
                {done, Other, _Rest} ->
                    Other
            end;
        {done, Other, _Rest} ->
            Other
    end.

string_continue(String, Loc0, Buffer0) ->
    case erl_scan:tokens([], String, Loc0, ?ERL_SCAN_OPTS) of
        {done, {ok, Tokens0, Loc}, Rest} ->
            {Tokens, FormTokens, Comments, Buffer} = split_tokens(Buffer0, Tokens0),
            State = #state{cont = fun string_continue/3, state = Rest, loc = Loc, original = FormTokens, buffer = Buffer},
            {ok, Tokens, Comments, State};
        {done, {eof, Loc}, _Rest} ->
            {Tokens, FormTokens, Comments, []} = split_tokens(Buffer0, []),
            State = #state{cont = fun eof/3, state = undefined, loc = Loc, original = FormTokens, buffer = []},
            {ok, Tokens, Comments, State};
        {done, Other, _Rest} ->
            Other;
        {more, Cont} ->
            case erl_scan:tokens(Cont, eof, Loc0, ?ERL_SCAN_OPTS) of
                {done, {ok, Tokens0, Loc}, eof} ->
                    {Tokens, FormTokens, Comments, []} = split_tokens(Buffer0, Tokens0),
                    State = #state{cont = fun eof/3, state = undefined, loc = Loc, original = FormTokens, buffer = []},
                    {ok, Tokens, Comments, State};
                {done, Other, _Rest} ->
                    Other
            end
    end.

-spec continue(state()) -> form_ret().
continue(#state{cont = Fun, state = State, loc = Loc, buffer = Buffer}) ->
    Fun(State, Loc, Buffer).

-spec last_form_string(state()) -> unicode:chardata().
last_form_string(#state{original = Tokens}) ->
    [stringify_token(Token) || Token <- Tokens].

%% TODO: make smarter
stringify_token(Token) -> erl_anno:text(element(2, Token)).

-spec split_tokens([erl_scan:token()], [erl_scan:token()]) -> {[token()], [erl_scan:token()], [comment()], [token()]}.
split_tokens(Tokens, ExtraTokens0) ->
    case split_tokens(Tokens, [], []) of
        {[], Comments} ->
            {[], Tokens, Comments, ExtraTokens0};
        {TransformedTokens, Comments} ->
            #{end_location := {LastLine, _}} = element(2, lists:last(TransformedTokens)),
            {ExtraComments, ExtraTokens, ExtraRest} = split_extra(ExtraTokens0, LastLine, []),
            {TransformedTokens, Tokens ++ ExtraTokens, Comments ++ ExtraComments, ExtraRest}
    end.

%% TODO: annotate [, {, <<, (, -> with following newline info to control user folding/expanding
split_tokens([{comment, _, _} = Comment0 | Rest0], Acc, CAcc) ->
    {Comment, Rest} = collect_comments(Rest0, Comment0),
    split_tokens(Rest, Acc, [Comment | CAcc]);
split_tokens([{white_space, _, _} | Rest], Acc, CAcc) ->
    split_tokens(Rest, Acc, CAcc);
split_tokens([{Atomic, Meta, Value} | Rest], Acc, CAcc)
when Atomic =:= integer; Atomic =:= float; Atomic =:= char; Atomic =:= atom; Atomic =:= string; Atomic =:= var ->
    split_tokens(Rest, [{Atomic, atomic_anno(erl_anno:to_term(Meta)), Value} | Acc], CAcc);
split_tokens([{Type, Meta, Value} | Rest], Acc, CAcc) ->
    split_tokens(Rest, [{Type, token_anno(erl_anno:to_term(Meta)), Value} | Acc], CAcc);
%% Keep the `text` value for if in case it's used as an attribute
split_tokens([{Type, Meta} | Rest], Acc, CAcc) when Type =:= 'if' ->
    split_tokens(Rest, [{Type, atomic_anno(erl_anno:to_term(Meta))} | Acc], CAcc);
split_tokens([{Type, Meta} | Rest], Acc, CAcc) ->
    split_tokens(Rest, [{Type, token_anno(erl_anno:to_term(Meta))} | Acc], CAcc);
split_tokens([], Acc, CAcc) ->
    {lists:reverse(Acc), lists:reverse(CAcc)}.

split_extra([{comment, Meta, Text} = Token | Rest], Line, Acc) ->
    case erl_anno:line(Meta) of
        Line ->
            Meta1 = erl_anno:to_term(Meta),
            Extra = lists:reverse(Acc, [Token]),
            {[{comment, comment_anno(Meta1, Meta1), [Text]}], Extra, Rest};
        _ ->
            {[], Acc, [Token | Rest]}
    end;
split_extra([{white_space, Meta, _} = Token | Rest], Line, Acc) ->
    case erl_anno:line(Meta) of
        Line ->
            split_extra(Rest, Line, [Token | Acc]);
        _ ->
            {[], Acc, [Token | Rest]}
    end;
split_extra(Rest, _Line, Acc) ->
    {[], Acc, Rest}.

collect_comments(Tokens, {comment, Meta, Text}) ->
    Line = erl_anno:line(Meta),
    {Texts, LastMeta, Rest} = collect_comments(Tokens, Line, Meta, [Text]),
    Anno = comment_anno(erl_anno:to_term(Meta), erl_anno:to_term(LastMeta)),
    {{comment, Anno, Texts}, Rest}.

collect_comments([{white_space, _, _} | Rest], Line, LastMeta, Acc) ->
    collect_comments(Rest, Line, LastMeta, Acc);
collect_comments([{comment, Meta, Text} = Comment | Rest], Line, LastMeta, Acc) ->
    case erl_anno:line(Meta) of
        NextLine when NextLine =:= Line + 1 ->
            collect_comments(Rest, NextLine, Meta, [Text | Acc]);
        _ ->
            {lists:reverse(Acc), LastMeta, [Comment | Rest]}
    end;
collect_comments(Other, _Line, LastMeta, Acc) ->
    {lists:reverse(Acc), LastMeta, Other}.

atomic_anno([{text, Text}, {location, {Line, Col} = Location}]) ->
    #{text => Text, location => Location, end_location => end_location(Text, Line, Col)}.

token_anno([{text, Text}, {location, {Line, Col} = Location}]) ->
    #{location => Location, end_location => end_location(Text, Line, Col)}.

comment_anno([{text, _}, {location, Location}], [{text, Text}, {location, {Line, Col}}]) ->
    #{location => Location, end_location => end_location(Text, Line, Col)}.

put_anno(Key, Value, Anno) when is_map(Anno) ->
    Anno#{Key => Value};
put_anno(Key, Value, Node) when is_tuple(Node) ->
    setelement(2, Node, (element(2, Node))#{Key => Value}).

delete_anno(Key, Anno) when is_map(Anno) ->
    maps:remove(Key, Anno);
delete_anno(Key, Node) when is_tuple(Node) ->
    setelement(2, Node, maps:remove(Key, element(2, Node))).

delete_annos(Keys, Anno) when is_map(Anno) ->
    maps:without(Keys, Anno);
delete_annos(Keys, Node) when is_tuple(Node) ->
    setelement(2, Node, maps:without(Keys, element(2, Node))).

get_anno(Key, Anno) when is_map(Anno) ->
    map_get(Key, Anno);
get_anno(Key, Node) when is_tuple(Node) ->
    map_get(Key, element(2, Node)).

get_anno(Key, Anno, Default) when is_map(Anno) ->
    maps:get(Key, Anno, Default);
get_anno(Key, Node, Default) when is_tuple(Node) ->
    maps:get(Key, element(2, Node), Default).

end_location("", Line, Column) ->
    {Line, Column};
end_location([$\n|String], Line, _Column) ->
    end_location(String, Line+1, 1);
end_location([_|String], Line, Column) ->
    end_location(String, Line, Column+1).
