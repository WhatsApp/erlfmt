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

-include("erlfmt.hrl").

-export([io_node/1, string_node/1, continue/1, last_node_string/1]).
-export([
    put_anno/3,
    merge_anno/2,
    delete_anno/2,
    delete_annos/2,
    get_anno/2,
    get_anno/3,
    get_line/1,
    get_end_line/1,
    get_inner_line/1,
    get_inner_end_line/1,
    update_anno/3
]).

-export_type([state/0, anno/0, token/0, comment/0]).

-define(ERL_SCAN_OPTS, [text, return_white_spaces, return_comments]).
-define(START_LOCATION, {1, 1}).

-type inner() :: term().
-type scan() :: fun((inner(), erl_anno:location()) ->
        {erl_scan:tokens_result() | {error, term()}, inner()}
).

-record(state, {
    scan :: scan(),
    inner :: inner(),
    loc = ?START_LOCATION :: erl_anno:location(),
    original :: [erl_scan:token()],
    buffer :: [erl_scan:token()]
}).

-type comment() :: {comment, anno(), [string()]}.
-type anno() :: #{
    location := location(),
    end_location := location(),
    inner_location => location(),
    text => string(),
    newline => true,
    atom() => term()
}.

-type location() :: {pos_integer(), pos_integer()}.
-type token() :: {atom(), anno(), term()} | {atom(), anno()}.
-type node_ret() ::
    {ok, [token()], [comment()], state()} |
    {error, {erl_anno:location(), module(), term()}, erl_anno:location()} |
    {eof, erl_anno:location()}.

-opaque state() :: #state{}.

-define(TRACK_NEWLINE_TOKEN(Token),
    Token =:= '(' orelse
        Token =:= '[' orelse
        Token =:= '{' orelse
        Token =:= '<<' orelse
        Token =:= '->'
).

-spec io_node(file:io_device()) -> node_ret().
io_node(IO) -> node(fun read_shebang_io/1, fun io_scan_erl_node/2, IO).

-spec string_node(string()) -> node_ret().
string_node(String) -> node(fun read_shebang_string/1, fun erl_scan_tokens/2, String).

read_shebang_io(IO) ->
    case file:read_line(IO) of
        {ok, Line} ->
            case read_shebang_string(Line) of
                {ok, Token, "", Loc} ->
                    {ok, Token, IO, Loc};
                error ->
                    {ok, _} = file:position(IO, bof),
                    error
            end;
        _ ->
            error
    end.

read_shebang_string("#!" ++ _ = String) ->
    [Shebang, Rest] = string:split(String, "\n"),
    Anno = [{text, Shebang}, {location, ?START_LOCATION}],
    Token = {shebang, Anno, Shebang},
    {ok, Token, Rest, {2, 1}};
read_shebang_string(_) ->
    error.

node(ReadShebang, Scan, Inner0) ->
    case ReadShebang(Inner0) of
        {ok, Shebang, Inner, Loc} ->
            continue(Scan, Inner, Loc, [Shebang]);
        error ->
            continue(Scan, Inner0, ?START_LOCATION, [])
    end.

-spec continue(state()) -> node_ret().
continue(#state{scan = Scan, inner = Inner, loc = Loc, buffer = Buffer}) ->
    continue(Scan, Inner, Loc, Buffer).

continue(Scan, Inner0, Loc0, []) ->
    case Scan(Inner0, Loc0) of
        {{ok, Tokens, Loc}, Inner} ->
            continue(Scan, Inner, Loc, Tokens);
        {{error, Reason}, _Inner} ->
            {error, {Loc0, file, Reason}};
        {Other, _Inner} ->
            Other
    end;
continue(Scan, Inner0, Loc0, Buffer0) ->
    case Scan(Inner0, Loc0) of
        {{ok, Tokens0, Loc}, Inner} ->
            {Tokens, NodeTokens, Comments, Buffer} = split_tokens(Buffer0, Tokens0),
            State = #state{
                scan = Scan,
                inner = Inner,
                loc = Loc,
                original = NodeTokens,
                buffer = Buffer
            },
            {ok, Tokens, Comments, State};
        {{eof, Loc}, _Inner} ->
            {Tokens, NodeTokens, Comments, []} = split_tokens(Buffer0, []),
            State = #state{
                scan = fun eof/2,
                inner = undefined,
                loc = Loc,
                original = NodeTokens,
                buffer = []
            },
            {ok, Tokens, Comments, State};
        {{error, Reason}, _Inner} ->
            {error, {Loc0, file, Reason}};
        {Other, _Rest} ->
            Other
    end.

io_scan_erl_node(IO, Loc) ->
    {io:scan_erl_form(IO, "", Loc, ?ERL_SCAN_OPTS), IO}.

erl_scan_tokens(String, Loc) ->
    case erl_scan:tokens([], String, Loc, ?ERL_SCAN_OPTS) of
        {more, Cont} ->
            {done, Resp, eof} = erl_scan:tokens(Cont, eof, Loc, ?ERL_SCAN_OPTS),
            {Resp, eof};
        {done, Resp, Rest} ->
            {Resp, Rest}
    end.

eof(undefined, Loc) ->
    {{eof, Loc}, undefined}.

-spec last_node_string(state()) -> {unicode:chardata(), anno()}.
last_node_string(#state{original = Tokens}) ->
    stringify_tokens(Tokens).

%% TODO: make smarter
stringify_tokens([Token | _] = Tokens) ->
    Anno = element(2, Token),
    stringify_tokens(Tokens, erl_anno:location(Anno), []).

stringify_tokens([LastToken], Location, Acc) ->
    Anno = element(2, LastToken),
    String = lists:reverse([erl_anno:text(Anno) | Acc]),
    {String, token_anno([{text, String}, {location, Location}], #{})};
stringify_tokens([Token | Tokens], Location, Acc) ->
    Anno = element(2, Token),
    stringify_tokens(Tokens, Location, [erl_anno:text(Anno) | Acc]).

-spec split_tokens([erl_scan:token()], [erl_scan:token()]) ->
    {[token()], [erl_scan:token()], [comment()], [token()]}.
split_tokens(Tokens, ExtraTokens0) ->
    case split_tokens(Tokens, [], []) of
        {[], Comments} ->
            {[], Tokens, Comments, ExtraTokens0};
        {TransformedTokens, Comments} ->
            #{end_location := {LastLine, _}} = element(2, lists:last(TransformedTokens)),
            {ExtraComments, ExtraTokens, ExtraRest} =
                split_extra(ExtraTokens0, LastLine, [], []),
            {TransformedTokens, Tokens ++ ExtraTokens, Comments ++ ExtraComments, ExtraRest}
    end.

split_tokens([{comment, _, _} = Comment0 | Rest0], Acc, CAcc) ->
    {Comment, Rest} = collect_comments(Rest0, Comment0),
    split_tokens(Rest, Acc, [Comment | CAcc]);
split_tokens([{white_space, _, _} | Rest], Acc, CAcc) ->
    split_tokens(Rest, Acc, CAcc);
split_tokens([{Atomic, Meta, Value} | Rest], Acc, CAcc) when ?IS_ATOMIC(Atomic) ->
    split_tokens(Rest, [{Atomic, atomic_anno(erl_anno:to_term(Meta)), Value} | Acc], CAcc);
split_tokens([{Type, Meta, Value} | Rest], Acc, CAcc) ->
    Token = {Type, token_anno(erl_anno:to_term(Meta), #{}), Value},
    split_tokens(Rest, [Token | Acc], CAcc);
%% Keep the `text` value for if in case it's used as an attribute
split_tokens([{Type, Meta} | Rest], Acc, CAcc) when Type =:= 'if' ->
    split_tokens(Rest, [{Type, atomic_anno(erl_anno:to_term(Meta))} | Acc], CAcc);
split_tokens([{Type, Meta} | Rest], Acc, CAcc) when Type =:= 'dot' ->
    split_tokens(Rest, [{Type, dot_anno(erl_anno:to_term(Meta))} | Acc], CAcc);
split_tokens([{Type, Meta} | Rest], Acc, CAcc) when ?TRACK_NEWLINE_TOKEN(Type) ->
    case has_newline(Rest) of
        true ->
            Anno = token_anno(erl_anno:to_term(Meta), #{newline => true}),
            split_tokens(Rest, [{Type, Anno} | Acc], CAcc);
        false ->
            Anno = token_anno(erl_anno:to_term(Meta), #{}),
            split_tokens(Rest, [{Type, Anno} | Acc], CAcc)
    end;
split_tokens([{Type, Meta} | Rest], Acc, CAcc) ->
    split_tokens(Rest, [{Type, token_anno(erl_anno:to_term(Meta), #{})} | Acc], CAcc);
split_tokens([], Acc, CAcc) ->
    {lists:reverse(Acc), lists:reverse(CAcc)}.

split_extra([{comment, Meta, Text} = Token | Rest], Line, Acc, CAcc) ->
    case erl_anno:line(Meta) of
        Line ->
            MetaTerm = erl_anno:to_term(Meta),
            Comment = {comment, comment_anno(MetaTerm, MetaTerm), [Text]},
            split_extra(Rest, Line, Acc, [Comment | CAcc]);
        _ ->
            {lists:reverse(CAcc), lists:reverse(Acc), [Token | Rest]}
    end;
split_extra([{white_space, _, _} = Token | Rest], Line, Acc, CAcc) ->
    split_extra(Rest, Line, [Token | Acc], CAcc);
split_extra(Rest, _Line, Acc, CAcc) ->
    {lists:reverse(CAcc), lists:reverse(Acc), Rest}.

%% newline is always a first character in a white_space token,
%% but there could be multiple such tokens
has_newline([{white_space, _, WhiteSpace} | Rest]) ->
    hd(WhiteSpace) =:= $\n orelse has_newline(Rest);
has_newline([{comment, _, _} | _]) ->
    true;
has_newline(_) ->
    false.

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

token_anno([{text, Text}, {location, {Line, Col} = Location}], Default) ->
    Default#{location => Location, end_location => end_location(Text, Line, Col)}.

comment_anno([{text, _}, {location, Location}], [{text, Text}, {location, {Line, Col}}]) ->
    #{location => Location, end_location => end_location(Text, Line, Col)}.

%% Special handling for dot tokens - we don't want to count final newline as part of the form
dot_anno([{text, _}, {location, {Line, Col} = Location}]) ->
    #{location => Location, end_location => {Line, Col + 1}}.

put_anno(Key, Value, Anno) when is_map(Anno) ->
    Anno#{Key => Value};
put_anno(Key, Value, Node) when is_tuple(Node) ->
    setelement(2, Node, (element(2, Node))#{Key => Value}).

merge_anno(Map, Anno) when is_map(Anno) ->
    maps:merge(Anno, Map);
merge_anno(Map, Node) when is_tuple(Node) ->
    setelement(2, Node, maps:merge(element(2, Node), Map)).

delete_anno(Key, Anno) when is_map(Anno) ->
    maps:remove(Key, Anno);
delete_anno(Key, Node) when is_tuple(Node) ->
    setelement(2, Node, maps:remove(Key, element(2, Node))).

delete_annos(Keys, Anno) when is_map(Anno) ->
    maps:without(Keys, Anno);
delete_annos(Keys, Node) when is_tuple(Node) ->
    setelement(2, Node, maps:without(Keys, element(2, Node))).

get_line(Anno) -> element(1, get_anno(location, Anno)).

get_end_line(Anno) -> element(1, get_anno(end_location, Anno)).

get_inner_line(Anno) ->
    element(1, get_anno(inner_location, Anno, get_anno(location, Anno))).

get_inner_end_line(Anno) ->
    element(1, get_anno(inner_end_location, Anno, get_anno(end_location, Anno))).

get_anno(Key, Anno) when is_map(Anno) ->
    map_get(Key, Anno);
get_anno(Key, Node) when is_tuple(Node) ->
    map_get(Key, element(2, Node)).

get_anno(Key, Anno, Default) when is_map(Anno) ->
    maps:get(Key, Anno, Default);
get_anno(Key, Node, Default) when is_tuple(Node) ->
    maps:get(Key, element(2, Node), Default).

update_anno(Key, Fun, Anno) when is_map(Anno) ->
    Anno#{Key => Fun(map_get(Key, Anno))};
update_anno(Key, Fun, Node) when is_tuple(Node) ->
    setelement(2, Node, update_anno(Key, Fun, element(2, Node))).

end_location("", Line, Column) ->
    {Line, Column};
end_location([$\n | String], Line, _Column) ->
    end_location(String, Line + 1, 1);
end_location([_ | String], Line, Column) ->
    end_location(String, Line, Column + 1).
