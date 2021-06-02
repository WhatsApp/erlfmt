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
-module(erlfmt).

%% API exports
-export([
    main/1,
    init/1,
    format_file/2,
    format_string/2,
    format_nodes/2,
    read_nodes/1,
    read_nodes_string/2,
    format_error/1,
    format_error_info/1
]).

% For unit tests
-export([
    format_file_range/4,
    format_string_range/4
]).

-export_type([error_info/0, config/0, pragma/0]).

-type error_info() :: {file:name_all(), erl_anno:location(), module(), Reason :: any()}.
-type pragma() :: require | insert | delete | ignore.
-type config() :: [{pragma, pragma()} | {print_width, pos_integer()} | verbose].

-define(DEFAULT_WIDTH, 100).

%% escript entry point
-spec main([string()]) -> no_return().
main(Argv) ->
    application:ensure_all_started(erlfmt),
    %% operate stdio in purely unicode mode
    io:setopts([binary, {encoding, unicode}]),
    Opts = erlfmt_cli:opts(),
    case getopt:parse(Opts, Argv) of
        {ok, {ArgOpts, []}} ->
            erlfmt_cli:do("erlfmt", ArgOpts);
        {ok, {ArgOpts, ExtraFiles}} ->
            erlfmt_cli:do("erlfmt", [{files, ExtraFiles} | ArgOpts]);
        {error, Error} ->
            io:put_chars(standard_error, [getopt:format_error(Opts, Error), "\n\n"]),
            getopt:usage(Opts, "erlfmt")
    end.

%% rebar3 plugin entry point
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_fmt_prv:init(State).

%% API entry point
-spec format_file(file:name_all() | stdin, config()) ->
    {ok, [unicode:chardata()], [error_info()]} | {skip, string()} | {error, error_info()}.
format_file(FileName, Options) ->
    Range = proplists:get_value(range, Options),
    case Range of
        undefined ->
            % Whole file (default).
            format_file_full(FileName, Options);
        {Start, End} ->
            format_file_range(FileName, {Start, 1}, {End, ?DEFAULT_WIDTH}, Options)
    end.

-spec format_file_full(file:name_all() | stdin, config()) ->
    {ok, [unicode:chardata()], [error_info()]} | {skip, string()} | {error, error_info()}.
format_file_full(FileName, Options) ->
    PrintWidth = proplists:get_value(print_width, Options, ?DEFAULT_WIDTH),
    Pragma = proplists:get_value(pragma, Options, ignore),
    try
        case file_read_nodes(FileName, Pragma) of
            {ok, Nodes0, Warnings} ->
                Nodes =
                    case Pragma of
                        insert -> insert_pragma_nodes(Nodes0);
                        delete -> remove_pragma_nodes(Nodes0);
                        _ -> Nodes0
                    end,
                Formatted = format_nodes(Nodes, PrintWidth),
                verify_nodes(FileName, Nodes, Formatted),
                VerboseWarnings =
                    case proplists:get_bool(verbose, Options) of
                        true -> check_line_lengths(FileName, PrintWidth, Formatted);
                        false -> []
                    end,
                {ok, Formatted, Warnings ++ VerboseWarnings};
            {skip, RawString} ->
                {skip, RawString}
        end
    catch
        {error, Error} -> {error, Error}
    end.

-spec format_string(string(), config()) ->
    {ok, string(), [error_info()]} | {skip, string()} | {error, error_info()}.
format_string(String, Options) ->
    Range = proplists:get_value(range, Options),
    case Range of
        undefined ->
            % Whole file (default).
            format_string_full(String, Options);
        {Start, End} ->
            format_string_range(String, {Start, 1}, {End, ?DEFAULT_WIDTH}, Options)
    end.

-spec format_string_full(string(), config()) ->
    {ok, string(), [error_info()]} | {skip, string()} | {error, error_info()}.
format_string_full(String, Options) ->
    PrintWidth = proplists:get_value(print_width, Options, ?DEFAULT_WIDTH),
    Pragma = proplists:get_value(pragma, Options, ignore),
    try
        case read_nodes_string("nofile", String, Pragma) of
            {ok, Nodes0, Warnings} ->
                Nodes =
                    case Pragma of
                        insert -> insert_pragma_nodes(Nodes0);
                        delete -> remove_pragma_nodes(Nodes0);
                        _ -> Nodes0
                    end,
                Formatted = format_nodes(Nodes, PrintWidth),
                verify_nodes("nofile", Nodes, Formatted),
                VerboseWarnings =
                    case proplists:get_bool(verbose, Options) of
                        true -> check_line_lengths("nofile", PrintWidth, Formatted);
                        false -> []
                    end,
                {ok, unicode:characters_to_list(Formatted), Warnings ++ VerboseWarnings};
            {skip, RawString} ->
                {skip, RawString}
        end
    catch
        {error, Error} -> {error, Error}
    end.

contains_pragma_node(Node) ->
    {PreComments, _, PostComments} = erlfmt_format:comments_with_pre_dot(Node),
    lists:any(fun contains_pragma_comment/1, PreComments ++ PostComments).

contains_pragma_comment({comment, _Loc, Comments}) ->
    string:find(Comments, "@format") =/= nomatch.

%% insert_pragma_nodes only inserts an @format comment,
%% if one has not already been inserted.
insert_pragma_nodes([]) ->
    [];
insert_pragma_nodes([{shebang, _, _} = Node | Nodes]) ->
    case contains_pragma_node(Node) of
        true -> [replace_pragma_node(Node) | Nodes];
        false -> [Node | insert_pragma_nodes(Nodes)]
    end;
insert_pragma_nodes([Node | Nodes]) ->
    case contains_pragma_node(Node) of
        true -> [replace_pragma_node(Node) | Nodes];
        false -> [insert_pragma_node(Node) | Nodes]
    end.

insert_pragma_node(Node) ->
    PreComments = erlfmt_scan:get_anno(pre_comments, Node, []),
    NewPreComments =
        case PreComments of
            [] ->
                Loc = erlfmt_scan:get_anno(location, Node),
                [{comment, #{location => Loc, end_location => Loc}, ["%%% % @format", ""]}];
            _ ->
                {comment, Loc, LastComments} = lists:last(PreComments),
                {Prefix, _} = string:take(lists:last(LastComments), "%"),
                lists:droplast(PreComments) ++
                    [{comment, Loc, LastComments ++ [Prefix ++ " % @format"]}]
        end,
    erlfmt_scan:put_anno(pre_comments, NewPreComments, Node).

remove_pragma_nodes([]) ->
    [];
remove_pragma_nodes([{shebang, _, _} = Node | Nodes]) ->
    case contains_pragma_node(Node) of
        true -> [remove_pragma_node(Node) | Nodes];
        false -> [Node | remove_pragma_nodes(Nodes)]
    end;
remove_pragma_nodes([Node | Nodes]) ->
    case contains_pragma_node(Node) of
        true -> [remove_pragma_node(Node) | Nodes];
        false -> [Node | Nodes]
    end.

remove_pragma_node(Node0) ->
    {PreComments0, _, PostComments0} = erlfmt_format:comments_with_pre_dot(Node0),
    PreComments = remove_pragma_comment_blocks(PreComments0),
    PostComments = remove_pragma_comment_blocks(PostComments0),
    Node = erlfmt_scan:put_anno(pre_comments, PreComments, Node0),
    erlfmt_scan:put_anno(post_comments, PostComments, Node).

remove_pragma_comment_blocks([]) ->
    [];
remove_pragma_comment_blocks([{comment, Loc, Comments} | Rest]) ->
    case remove_pragma_comment_block(Comments) of
        [] -> remove_pragma_comment_block(Rest);
        CleanComments -> [{comment, Loc, CleanComments} | remove_pragma_comment_block(Rest)]
    end.

remove_pragma_comment_block([]) ->
    [];
remove_pragma_comment_block([Head | Tail]) ->
    case string:find(Head, "@format") of
        nomatch -> [Head | remove_pragma_comment_block(Tail)];
        _ -> Tail
    end.

replace_pragma_node(Node0) ->
    {PreComments0, _, PostComments0} = erlfmt_format:comments_with_pre_dot(Node0),
    PreComments = replace_pragma_comment_blocks("%%%", PreComments0),
    PostComments = replace_pragma_comment_blocks("%%%", PostComments0),
    Node = erlfmt_scan:put_anno(pre_comments, PreComments, Node0),
    erlfmt_scan:put_anno(post_comments, PostComments, Node).

replace_pragma_comment_blocks(_Prefix, []) ->
    [];
replace_pragma_comment_blocks(Prefix, [{comment, Loc, Comments} | Rest]) ->
    CleanComments = replace_pragma_comment_block(Prefix, Comments),
    {Prefix0, _} = string:take(lists:last(CleanComments), "%"),
    [{comment, Loc, CleanComments} | replace_pragma_comment_block(Prefix0, Rest)].

replace_pragma_comment_block(_Prefix, []) ->
    [];
replace_pragma_comment_block(Prefix, ["%% @format" | Tail]) ->
    [(Prefix ++ " % @format") | Tail];
replace_pragma_comment_block(_Prefix, [("%" ++ _) = Head | Tail]) ->
    {Prefix, _} = string:take(Head, "%"),
    [Head | replace_pragma_comment_block(Prefix, Tail)];
replace_pragma_comment_block(Prefix, [Head | Tail]) ->
    [Head | replace_pragma_comment_block(Prefix, Tail)].

% Format the minimum number of top-level forms
% that cover the passed range.
% Rationale: top-level forms is the smallest
%            granularity we support now.
-spec format_file_range(
    file:name_all(),
    erlfmt_scan:location(),
    erlfmt_scan:location(),
    config()
) ->
    {ok, string(), [error_info()]}
    | {error, error_info()}.
format_file_range(FileName, StartLocation, EndLocation, Options) ->
    {ok, Nodes, Warnings} = file_read_nodes(FileName, ignore),
    format_enclosing_range(FileName, StartLocation, EndLocation, Options, Nodes, Warnings).

-spec format_string_range(
    string(),
    erlfmt_scan:location(),
    erlfmt_scan:location(),
    config()
) ->
    {ok, string(), [error_info()]}
    | {error, error_info()}.
format_string_range(String, StartLocation, EndLocation, Options) ->
    FileName = "nofile",
    Pragma = proplists:get_value(pragma, Options, ignore),
    {ok, Nodes, Warnings} = read_nodes_string(FileName, String, Pragma),
    format_enclosing_range(FileName, StartLocation, EndLocation, Options, Nodes, Warnings).

format_enclosing_range(FileName, StartLocation, EndLocation, Options, Nodes, Warnings) ->
    case format_range(FileName, StartLocation, EndLocation, Options, Nodes, Warnings) of
        {options, PossibleRanges} ->
            % Pick the largest range, so all intersected forms are covered.
            {Starts, Ends} = lists:unzip(PossibleRanges),
            Start = lists:min(Starts),
            End = lists:max(Ends),
            Res = format_range(FileName, Start, End, Options, Nodes, Warnings),
            % Poor man's assert (to avoid including assert.hrl)
            % This time we must have the formatted result.
            {ok, _, _} = Res,
            Res;
        X ->
            % Already ok or error: pass as is.
            X
    end.

format_range(FileName, StartLocation, EndLocation, Options, Nodes, Warnings) ->
    PrintWidth = proplists:get_value(print_width, Options, ?DEFAULT_WIDTH),
    try
        case verify_ranges(Nodes, StartLocation, EndLocation) of
            {ok, NodesInRange} ->
                Result = format_nodes(NodesInRange, PrintWidth),
                verify_nodes(FileName, NodesInRange, Result),
                VerboseWarnings =
                    case proplists:get_bool(verbose, Options) of
                        true -> check_line_lengths(FileName, PrintWidth, Result, StartLocation);
                        false -> []
                    end,
                {ok, unicode:characters_to_binary(Result), Warnings ++ VerboseWarnings};
            {options, PossibleRanges} ->
                {options, PossibleRanges}
        end
    catch
        {error, Error} -> {error, Error}
    end.

%% API entry point
-spec read_nodes(file:name_all()) ->
    {ok, [erlfmt_parse:abstract_form()], [error_info()]} | {error, error_info()}.
read_nodes(FileName) ->
    try
        file_read_nodes(FileName, ignore)
    catch
        {error, Error} -> {error, Error}
    end.

file_read_nodes(FileName, Pragma) ->
    read_file(FileName, fun(File) ->
        read_nodes(erlfmt_scan:io_node(File), FileName, Pragma)
    end).

read_file(stdin, Action) ->
    Action(standard_io);
read_file(FileName, Action) ->
    case file:open(FileName, [read, binary, {encoding, unicode}]) of
        {ok, File} ->
            try
                Action(File)
            after
                file:close(File)
            end;
        {error, Reason} ->
            throw({error, {FileName, 0, file, Reason}})
    end.

%% API entry point
-spec read_nodes_string(file:name_all(), string()) ->
    {ok, [erlfmt_parse:abstract_form()], [error_info()]} | {error, error_info()}.
read_nodes_string(FileName, String) ->
    try
        read_nodes_string(FileName, String, ignore)
    catch
        {error, Error} -> {error, Error}
    end.

read_nodes_string(FileName, String, Pragma) ->
    read_nodes(erlfmt_scan:string_node(String), FileName, Pragma).

read_nodes(State, FileName, Pragma) ->
    read_nodes(State, FileName, Pragma, [], [], []).

read_nodes({ok, Tokens, Comments, Cont}, FileName, Pragma, [], Warnings0, TextAcc) ->
    {Node, Warnings} = parse_node(Tokens, Comments, FileName, Cont, Warnings0),
    case {Pragma, contains_pragma_node(Node), Node} of
        {_, _, {shebang, _, _}} ->
            {LastString, _Anno} = erlfmt_scan:last_node_string(Cont),
            read_nodes(
                erlfmt_scan:continue(Cont),
                FileName,
                Pragma,
                [Node],
                Warnings,
                TextAcc ++ LastString
            );
        {_, false, _} when Pragma =:= require; Pragma =:= delete ->
            {LastString, _Anno} = erlfmt_scan:last_node_string(Cont),
            case erlfmt_scan:read_rest(Cont) of
                {ok, Rest} ->
                    {skip, [TextAcc, LastString | Rest]};
                {error, {ErrLoc, Mod, Reason}} ->
                    throw({error, {FileName, ErrLoc, Mod, Reason}})
            end;
        _ ->
            read_nodes_loop(
                erlfmt_scan:continue(Cont),
                FileName,
                [Node],
                Warnings
            )
    end;
read_nodes(
    {ok, Tokens, Comments, Cont},
    FileName,
    Pragma,
    [{shebang, _, _} = ShebangNode],
    Warnings0,
    TextAcc
) ->
    {Node, Warnings} = parse_node(Tokens, Comments, FileName, Cont, Warnings0),
    case {Pragma, contains_pragma_node(Node)} of
        {_, false} when Pragma =:= require; Pragma =:= delete ->
            {LastString, _Anno} = erlfmt_scan:last_node_string(Cont),
            case erlfmt_scan:read_rest(Cont) of
                {ok, Rest} ->
                    {skip, [TextAcc, LastString | Rest]};
                {error, {ErrLoc, Mod, Reason}} ->
                    throw({error, {FileName, ErrLoc, Mod, Reason}})
            end;
        _ ->
            read_nodes_loop(
                erlfmt_scan:continue(Cont),
                FileName,
                [Node, ShebangNode],
                Warnings
            )
    end;
read_nodes(Other, FileName, _Pragma, NodeAcc, Warnings, _TextAcc) ->
    read_nodes_loop(Other, FileName, NodeAcc, Warnings).

read_nodes_loop({ok, Tokens, Comments, Cont}, FileName, Acc, Warnings0) ->
    {Node, Warnings} = parse_node(Tokens, Comments, FileName, Cont, Warnings0),
    read_nodes_loop(erlfmt_scan:continue(Cont), FileName, [Node | Acc], Warnings);
read_nodes_loop({eof, _Loc}, _FileName, Acc, Warnings) ->
    {ok, lists:reverse(Acc), lists:reverse(Warnings)};
read_nodes_loop({error, {ErrLoc, Mod, Reason}, _Loc}, FileName, _Acc, _Warnings) ->
    throw({error, {FileName, ErrLoc, Mod, Reason}}).

parse_node([], _Comments, _FileName, Cont, Warnings) ->
    {node_string(Cont), Warnings};
parse_node([{shebang, Meta, String}], Comments, _FileName, _Cont, Warnings) ->
    {{shebang, erlfmt_recomment:put_post_comments(Meta, Comments), String}, Warnings};
parse_node([Token | _] = Tokens, Comments, FileName, Cont, Warnings) ->
    {PreComments, _} = erlfmt_recomment:take_comments(erlfmt_scan:get_line(Token), Comments),
    case lists:any(fun contains_ignore_comment/1, PreComments) of
        false ->
            case erlfmt_parse:parse_node(Tokens) of
                {ok, Node} ->
                    {erlfmt_recomment:recomment(Node, Comments), Warnings};
                {error, {ErrLoc, Mod, Reason}} ->
                    Warning = {FileName, ErrLoc, Mod, Reason},
                    {node_string(Cont), [Warning | Warnings]}
            end;
        true ->
            {node_string(Cont), Warnings}
    end.

contains_ignore_comment({comment, _Loc, Comments}) ->
    lists:any(
        fun(Comment) -> string:trim(Comment, both, "% ") == "erlfmt-ignore" end,
        Comments
    ).

node_string(Cont) ->
    {String, Anno} = erlfmt_scan:last_node_string_trimmed(Cont),
    {raw_string, Anno, String}.

-spec format_nodes([erlfmt_parse:abstract_form()], pos_integer()) -> [unicode:chardata()].
format_nodes([], _PrintWidth) ->
    [];
format_nodes(Nodes, PrintWidth) ->
    [$\n | Formatted] = format_nodes_loop(Nodes, PrintWidth),
    Formatted.

format_nodes_loop([Node | [Next | _] = Rest], PrintWidth) ->
    [
        $\n,
        format_node(Node, PrintWidth),
        maybe_empty_line(Node, Next)
        | format_nodes_loop(Rest, PrintWidth)
    ];
format_nodes_loop([Node], PrintWidth) ->
    [$\n, format_node(Node, PrintWidth), $\n];
format_nodes_loop([], _PrintWidth) ->
    [].

maybe_empty_line(Node, Next) ->
    case has_empty_line_between(Node, Next) of
        true -> "\n";
        false -> ""
    end.

-spec format_node(erlfmt_parse:abstract_form(), pos_integer()) -> unicode:chardata().
format_node({raw_string, _Anno, String}, _PrintWidth) ->
    String;
format_node(Node, PrintWidth) ->
    Doc = erlfmt_format:to_algebra(Node),
    erlfmt_algebra:format(Doc, PrintWidth).

has_empty_line_between(Left, Right) ->
    erlfmt_scan:get_end_line(Left) + 1 < erlfmt_scan:get_line(Right).

verify_nodes(FileName, Nodes, Formatted) ->
    Flattened = unicode:characters_to_list(Formatted),
    case read_nodes_string(FileName, Flattened) of
        {ok, Nodes2, _} ->
            try
                equivalent_list(Nodes, Nodes2)
            catch
                {not_equivalent, Left, Right} ->
                    Location = try_location(Left, Right),
                    Msg = {not_equivalent, Left, Right, Flattened},
                    throw({error, {FileName, Location, ?MODULE, Msg}})
            end;
        {error, _} ->
            throw({error, {FileName, 0, ?MODULE, could_not_reparse}})
    end.

equivalent(Element, Element) ->
    true;
equivalent({raw_string, _AnnoL, RawL}, {raw_string, _AnnoR, RawR}) ->
    string:equal(RawL, RawR) orelse throw({not_equivalent, RawL, RawR});
equivalent({Type, _}, {Type, _}) ->
    true;
equivalent({concat, _, Left} = L, {concat, _, Right} = R) ->
    concat_equivalent(Left, Right) orelse throw({not_equivalent, L, R});
equivalent({string, _, String} = L, {concat, _, Values} = R) ->
    string_concat_equivalent(String, Values) orelse throw({not_equivalent, L, R});
equivalent({op, _, Op1, _, _} = L, {op, _, Op2, _, _} = R) when Op1 =/= Op2 ->
    throw({not_equivalent, L, R});
equivalent({Type, _, L}, {Type, _, R}) ->
    equivalent(L, R);
equivalent({Type, _, L1, L2}, {Type, _, R1, R2}) ->
    equivalent(L1, R1) andalso equivalent(L2, R2);
equivalent({Type, _, L1, L2, L3}, {Type, _, R1, R2, R3}) ->
    equivalent(L1, R1) andalso equivalent(L2, R2) andalso equivalent(L3, R3);
equivalent({Type, _, L1, L2, L3, L4}, {Type, _, R1, R2, R3, R4}) ->
    equivalent(L1, R1) andalso
        equivalent(L2, R2) andalso equivalent(L3, R3) andalso equivalent(L4, R4);
equivalent(Ls, Rs) when is_list(Ls), is_list(Rs) ->
    equivalent_list(Ls, Rs);
equivalent(L, R) ->
    throw({not_equivalent, L, R}).

string_concat_equivalent(String, Values) ->
    string:equal(String, [Value || {string, _, Value} <- Values]).

concat_equivalent(ValuesL, ValuesR) ->
    string:equal([Value || {string, _, Value} <- ValuesL], [Value || {string, _, Value} <- ValuesR]).

equivalent_list([L | Ls], [R | Rs]) ->
    equivalent(L, R) andalso equivalent_list(Ls, Rs);
equivalent_list([], []) ->
    true;
equivalent_list(Ls, Rs) ->
    throw({not_equivalent, Ls, Rs}).

try_location(Node, _) when is_tuple(Node) -> erlfmt_scan:get_anno(location, Node);
try_location([Node | _], _) when is_tuple(Node) -> erlfmt_scan:get_anno(location, Node);
try_location(_, Node) when is_tuple(Node) -> erlfmt_scan:get_anno(location, Node);
try_location(_, [Node | _]) when is_tuple(Node) -> erlfmt_scan:get_anno(location, Node);
try_location(_, _) -> 0.

-spec format_error_info(error_info()) -> unicode:chardata().
format_error_info({FileName, Anno, Mod, Reason}) ->
    io_lib:format("~ts~s: ~ts", [FileName, format_loc(Anno), Mod:format_error(Reason)]).

format_loc(0) -> "";
format_loc({Line, Col}) -> io_lib:format(":~B:~B", [Line, Col]);
format_loc(#{location := {Line, Col}}) -> io_lib:format(":~B:~B", [Line, Col]);
format_loc(Line) when is_integer(Line) -> io_lib:format(":~B", [Line]).

format_error({not_equivalent, Node1, Node2, Formatted}) ->
    io_lib:format(
        "formatter result not equivalent. Please report this bug.~n~n~ts~n~n~p~n~n~p",
        [Formatted, Node1, Node2]
    );
format_error(could_not_reparse) ->
    "formatter result invalid, could not reparse";
format_error({long_line, Length, Width}) ->
    io_lib:format("Line too long (~p > ~p)", [Length, Width]).

verify_ranges(Nodes, StartLocation, EndLocation) ->
    ApplicableNodes = nodes_in_range(Nodes, StartLocation, EndLocation),
    case possible_ranges(ApplicableNodes, StartLocation, EndLocation) of
        [{StartLocation, EndLocation}] ->
            {ok, ApplicableNodes};
        Options ->
            {options, Options}
    end.

% Returns ranges which starts with the start of a node and ends with the end of node
possible_ranges(Nodes, StartLocation, EndLocation) ->
    case Nodes of
        [] ->
            [];
        [OnlyNode] ->
            [get_location_range(OnlyNode)];
        MultipleNodes ->
            combine(
                get_possible_locations(MultipleNodes, StartLocation, fun get_location/1),
                get_possible_locations(
                    lists:reverse(MultipleNodes),
                    EndLocation,
                    fun get_end_location/1
                )
            )
    end.

nodes_in_range(Nodes, StartLocation, EndLocation) ->
    [Node || Node <- Nodes, node_intersects_range(Node, StartLocation, EndLocation)].

node_intersects_range(Node, StartLocation, EndLocation) ->
    {Start, End} = get_location_range(Node),
    ((Start < StartLocation) and (End > StartLocation)) or
        ((Start >= StartLocation) and (Start < EndLocation)).

get_possible_locations([Option1, Option2 | _], Location, GetLoc) ->
    case GetLoc(Option1) of
        Location ->
            [Location];
        OptionalLocation ->
            [OptionalLocation, GetLoc(Option2)]
    end.

combine(L1, L2) ->
    [{X1, X2} || X1 <- L1, X2 <- L2].

get_location_range(Node) ->
    {get_location(Node), get_end_location(Node)}.

get_location(Node) ->
    erlfmt_scan:get_anno(location, Node).

get_end_location(Node) ->
    erlfmt_scan:get_anno(end_location, Node).

check_line_lengths(FileName, Width, String) ->
    check_line_lengths(FileName, Width, String, {1, 0}).

check_line_lengths(FileName, Width, String, {FirstLineNo, _}) ->
    Lines = string:split(String, "\n", all),
    LastLineNo = FirstLineNo + length(Lines) - 1,
    [
        {FileName, LineNo, ?MODULE, {long_line, string:length(Line), Width}}
     || {LineNo, Line} <- lists:zip(
            lists:seq(FirstLineNo, LastLineNo),
            Lines
        ),
        string:length(Line) > Width
    ].
