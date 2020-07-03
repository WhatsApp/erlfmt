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
    format_range/3,
    read_nodes/1,
    read_nodes_string/2,
    format_error/1,
    format_error_info/1,
    contains_pragma_file/1,
    contains_pragma_string/2
]).

-export_type([error_info/0, out/0, config/0]).

-type error_info() :: {file:name_all(), erl_anno:location(), module(), Reason :: any()}.

-type out() :: standard_out | {path, file:name_all()} | replace.

-type pragma() :: require | ignore.

-type config() :: {Pragma :: pragma(), Out :: out()}.

-define(PAGE_WIDTH, 92).

-define(FIRST_LINE, 1).

-define(FIRST_COLUMN, 1).

-define(SCAN_START, {?FIRST_LINE, ?FIRST_COLUMN}).

-define(SCAN_OPTS, [text, return_comments]).

-define(COMMAND_NAME, "erlfmt").

%% escript entry point
-spec main([string()]) -> no_return().
main(Argv) ->
    application:ensure_all_started(erlfmt),
    Opts = erlfmt_cli:opts(),
    case getopt:parse(Opts, Argv) of
        {ok, {ArgOpts, []}} ->
            erlfmt_cli:do(ArgOpts, "erlfmt");
        {ok, {ArgOpts, ExtraFiles}} ->
            erlfmt_cli:do([{files, ExtraFiles} | ArgOpts], "erlfmt");
        {error, Error} ->
            io:put_chars(standard_error, [getopt:format_error(Opts, Error), "\n\n"]),
            getopt:usage(Opts, "erlfmt")
    end.

%% rebar3 plugin entry point
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_fmt_prv:init(State).

%% API entry point
-spec format_file(file:name_all(), config()) ->
    {ok, [error_info()]} | {error, error_info()}.
format_file(FileName, {Pragma, Out}) ->
    try
        ShouldFormat = (Pragma == ignore) orelse contains_pragma_file(FileName),
        case ShouldFormat of
            true ->
                {ok, Nodes, Warnings} = file_read_nodes(FileName),
                [$\n | Formatted] = format_nodes(Nodes),
                verify_nodes(FileName, Nodes, Formatted),
                write_formatted(FileName, Formatted, Out),
                {ok, Warnings};
            false ->
                {ok, []}
        end
    catch
        {error, Error} -> {error, Error}
    end.

-spec contains_pragma_file(file:name_all()) -> boolean().
contains_pragma_file(FileName) ->
    read_file(FileName, fun (File) ->
        read_pragma_nodes(erlfmt_scan:io_node(File), FileName, [], [])
    end).

-spec contains_pragma_string(file:name_all(), string()) -> boolean().
contains_pragma_string(FileName, String) ->
    read_pragma_nodes(erlfmt_scan:string_node(String), FileName, [], []).

read_pragma_nodes({ok, Tokens, Comments, Cont}, FileName, _Acc, Warnings0) ->
    {Node, _Warnings} = parse_nodes(Tokens, Comments, FileName, Cont, Warnings0),
    contains_pragma_node(Node);
read_pragma_nodes(_, _, _, _) ->
    false.

contains_pragma_node({attribute, Meta, _AfAtom, _AbstractExprs}) ->
    {PreComments, PostComments} = erlfmt_format:comments(Meta),
    lists:any(fun contains_pragma_comment/1, PreComments ++ PostComments);
contains_pragma_node(_) ->
    false.

contains_pragma_comment({comment, _Loc, Comments}) ->
    string:find(Comments, "@format") =/= nomatch;
contains_pragma_comment(_) ->
    false.

-spec format_range(
    file:name_all(),
    erlfmt_scan:location(),
    erlfmt_scan:location()
) ->
    {ok, string(), [error_info()]} |
    {error, error_info()} |
    {options, [{erlfmt_scan:location(), erlfmt_scan:location()}]}.
format_range(FileName, StartLocation, EndLocation) ->
    try
        {ok, Nodes, Warnings} = file_read_nodes(FileName),
        case verify_ranges(Nodes, StartLocation, EndLocation) of
            {ok, NodesInRange} ->
                [$\n | Result] = format_nodes(NodesInRange),
                verify_nodes(FileName, NodesInRange, Result),
                {ok, unicode:characters_to_binary(Result), Warnings};
            {options, Options} ->
                {options, Options}
        end
    catch
        {error, Error} -> {error, Error}
    end.

%% API entry point
-spec read_nodes(file:name_all()) ->
    {ok, [erlfmt_parse:abstract_form()], [error_info()]} | {error, error_info()}.
read_nodes(FileName) ->
    try file_read_nodes(FileName)
    catch
        {error, Error} -> {error, Error}
    end.

file_read_nodes(FileName) ->
    read_file(FileName, fun (File) ->
        read_nodes(erlfmt_scan:io_node(File), FileName, [], [])
    end).

read_file(FileName, Action) ->
    case file:open(FileName, [read, {encoding, utf8}]) of
        {ok, File} ->
            try Action(File)
            after file:close(File)
            end;
        {error, Reason} ->
            throw({error, {FileName, 0, file, Reason}})
    end.

%% API entry point
-spec read_nodes_string(file:name_all(), string()) ->
    {ok, [erlfmt_parse:abstract_form()], [error_info()]} | {error, error_info()}.
read_nodes_string(FileName, String) ->
    try read_nodes(erlfmt_scan:string_node(String), FileName, [], [])
    catch
        {error, Error} -> {error, Error}
    end.

read_nodes({ok, Tokens, Comments, Cont}, FileName, Acc, Warnings0) ->
    {Node, Warnings} = parse_nodes(Tokens, Comments, FileName, Cont, Warnings0),
    read_nodes(erlfmt_scan:continue(Cont), FileName, [Node | Acc], Warnings);
read_nodes({eof, _Loc}, _FileName, Acc, Warnings) ->
    {ok, lists:reverse(Acc), lists:reverse(Warnings)};
read_nodes({error, {ErrLoc, Mod, Reason}, _Loc}, FileName, _Acc, _Warnings) ->
    throw({error, {FileName, ErrLoc, Mod, Reason}}).

parse_nodes([], _Comments, _FileName, Cont, Warnings) ->
    {node_string(Cont), Warnings};
parse_nodes(Tokens, Comments, FileName, Cont, Warnings) ->
    case erlfmt_parse:parse_node(Tokens) of
        {ok, Node} ->
            {erlfmt_recomment:recomment(Node, Comments), Warnings};
        {error, {ErrLoc, Mod, Reason}} ->
            Warning = {FileName, ErrLoc, Mod, Reason},
            {node_string(Cont), [Warning | Warnings]}
    end.

node_string(Cont) ->
    {String, Anno} = erlfmt_scan:last_node_string(Cont),
    {raw_string, Anno, string:trim(String, both, "\n")}.

format_nodes([{attribute, _, {atom, _, spec}, _} = Attr, {function, _, _} = Fun | Rest]) ->
    [$\n, format_node(Attr), $\n, format_node(Fun), $\n | format_nodes(Rest)];
format_nodes([{attribute, _, {atom, _, Name}, _} | _ ] = Nodes) ->
    {Attrs, Rest} = split_attrs(Name, Nodes),
    format_attrs(Attrs) ++ [$\n | format_nodes(Rest)];
format_nodes([Node | Rest]) ->
    [$\n, format_node(Node), $\n | format_nodes(Rest)];
format_nodes([]) ->
    [].

format_node({raw_string, _Anno, String}) ->
    String;
format_node(Node) ->
    Doc = erlfmt_format:to_algebra(Node),
    erlfmt_algebra:format(Doc, ?PAGE_WIDTH).

split_attrs(PredName, Nodes) ->
    lists:splitwith(fun
        ({attribute, _, {atom, _, Name}, _}) -> PredName =:= Name;
        (_) -> false
    end, Nodes).

format_attrs([Attr]) -> [$\n, format_node(Attr)];
format_attrs([Attr | Rest]) ->
    FAttr = format_node(Attr),
    case has_non_comment_newline(FAttr) of
        true -> [$\n, FAttr, $\n | format_attrs(Rest)];
        false -> [$\n, FAttr | format_attrs(Rest)]
    end.

has_non_comment_newline(String) ->
    length(lists:filter(fun is_not_comment/1, string:split(String, "\n", all))) >= 2.

is_not_comment(String) ->
    not (string:is_empty(String) orelse string:equal(string:slice(String, 0, 1), "%")).

verify_nodes(FileName, Nodes, Formatted) ->
    case read_nodes_string(FileName, unicode:characters_to_list(Formatted)) of
        {ok, Nodes2, _} ->
            try equivalent_list(Nodes, Nodes2)
            catch
                {not_equivalent, Left, Right} ->
                    Location = try_location(Left, Right),
                    Msg = {not_equivalent, Left, Right},
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
equivalent({string, _, String} = L, {concat, _, Values} = R) ->
    string_concat_equivalent(String, Values) orelse throw({not_equivalent, L, R});
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

write_formatted(_FileName, Formatted, standard_out) ->
    io:put_chars(Formatted);
write_formatted(FileName, Formatted, Out) ->
    OutFileName = out_file(FileName, Out),
    case filelib:ensure_dir(OutFileName) of
        ok -> ok;
        {error, Reason1} -> throw({error, {OutFileName, 0, file, Reason1}})
    end,
    case file:write_file(OutFileName, unicode:characters_to_binary(Formatted)) of
        ok -> ok;
        {error, Reason2} -> throw({error, {OutFileName, 0, file, Reason2}})
    end.

out_file(FileName, replace) ->
    FileName;
out_file(FileName, {path, Path}) ->
    filename:join(Path, filename:basename(FileName)).

-spec format_error_info(error_info()) -> unicode:chardata().
format_error_info({FileName, Anno, Mod, Reason}) ->
    io_lib:format("~ts~s: ~ts", [FileName, format_loc(Anno), Mod:format_error(Reason)]).

format_loc(0) -> "";
format_loc({Line, Col}) -> io_lib:format(":~B:~B", [Line, Col]);
format_loc(#{location := {Line, Col}}) -> io_lib:format(":~B:~B", [Line, Col]);
format_loc(Line) when is_integer(Line) -> io_lib:format(":~B", [Line]).

format_error({not_equivalent, Node1, Node2}) ->
    io_lib:format(
        "formatter result not equivalent. Please report this bug.~n~n~p~n~n~p",
        [Node1, Node2]
    );
format_error(could_not_reparse) ->
    "formatter result invalid, could not reparse".

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
    ((Start < StartLocation) and (End >= StartLocation)) or
        ((Start >= StartLocation) and (Start =< EndLocation)).

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
