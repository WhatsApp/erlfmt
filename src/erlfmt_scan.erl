-module(erlfmt_scan).

-oncall("whatsapp_erlang").

-typing([dialyzer]).

-export([io_form/1, string_form/1, continue/1, last_form_string/1]).
-export([put_anno/3, delete_anno/2, get_anno/2, get_anno/3, text/1]).

-export_type([state/0, anno/0, token/0]).

-define(ERL_SCAN_OPTS, [text, return_white_spaces, return_comments]).

-define(START_LOCATION, {1, 1}).

-record(state, {
    cont :: fun((term(), erl_anno:location()) -> form_ret()),
    state :: term(),
    loc :: erl_anno:location(),
    original :: [erl_scan:token()]
}).

-type comment() :: {comment, anno(), [string()]}.

-type anno() :: #{atom() => term()}.

-type token() :: {atom(), anno(), term()} | {atom(), anno()}.

-type form_ret() ::
    {ok, [token()], [comment()], state()} |
    {error, {erl_anno:location(), module(), term()}, erl_anno:location()} |
    {eof, erl_anno:location()}.

-opaque state() :: #state{}.

-spec io_form(file:io_device()) -> form_ret().
io_form(IO) -> io_continue(IO, ?START_LOCATION).

io_continue(IO, Loc0) ->
    case io:scan_erl_form(IO, "", Loc0, ?ERL_SCAN_OPTS) of
        {ok, Tokens0, Loc} ->
            {Tokens, Comments} = preprocess_tokens(Tokens0),
            State = #state{cont = fun io_continue/2, state = IO, loc = Loc, original = Tokens0},
            {ok, Tokens, Comments, State};
        {error, Reason} ->
            {error, {Loc0, file, Reason}, Loc0};
        Other ->
            Other
    end.

-spec string_form(string()) -> form_ret().
string_form(String) -> string_continue(String, ?START_LOCATION).

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

-spec continue(state()) -> form_ret().
continue(#state{cont = Fun, state = State, loc = Loc}) ->
    Fun(State, Loc).

-spec last_form_string(state()) -> unicode:chardata().
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
preprocess_tokens([{Atomic, Meta, Value} | Rest], Acc, CAcc)
when Atomic =:= integer; Atomic =:= float; Atomic =:= char; Atomic =:= atom; Atomic =:= string; Atomic =:= var ->
    preprocess_tokens(Rest, [{Atomic, atomic_anno(erl_anno:to_term(Meta)), Value} | Acc], CAcc);
preprocess_tokens([{Type, Meta, Value} | Rest], Acc, CAcc) ->
    preprocess_tokens(Rest, [{Type, token_anno(erl_anno:to_term(Meta)), Value} | Acc], CAcc);
preprocess_tokens([{Type, Meta} | Rest], Acc, CAcc) ->
    preprocess_tokens(Rest, [{Type, token_anno(erl_anno:to_term(Meta))} | Acc], CAcc);
preprocess_tokens([], Acc, CAcc) ->
    {lists:reverse(Acc), lists:reverse(CAcc)}.

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

get_anno(Key, Anno) when is_map(Anno) ->
    map_get(Key, Anno);
get_anno(Key, Node) when is_tuple(Node) ->
    map_get(Key, element(2, Node)).

get_anno(Key, Anno, Default) when is_map(Anno) ->
    maps:get(Key, Anno, Default);
get_anno(Key, Node, Default) when is_tuple(Node) ->
    maps:get(Key, element(2, Node), Default).

text(AnnoOrNode) -> get_anno(text, AnnoOrNode).

end_location("", Line, Column) ->
    {Line, Column};
end_location([$\n|String], Line, _Column) ->
    end_location(String, Line+1, 1);
end_location([_|String], Line, Column) ->
    end_location(String, Line, Column+1).
