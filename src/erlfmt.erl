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

-export_type([error_info/0, config_option/0, config/0, pragma/0]).

-type error_info() :: {file:name_all(), erl_anno:location(), module(), Reason :: any()}.
-type pragma() :: require | insert | delete | ignore.
-type config_option() :: {pragma, pragma()} | {print_width, pos_integer()} | verbose.
-type config() :: [config_option()].

%% needed because of getopt being weird
-dialyzer({nowarn_function, [init/1, main/1]}).

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
            getopt:usage(Opts, "erlfmt"),
            erlang:halt(2)
    end.

%% rebar3 plugin entry point
-spec init(term()) -> {ok, term()}.
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
            % Remove 'range' property: when applicable we pass explicitly the range instead.
            % Also, match specifition of format_string_range.
            Options2 = proplists:delete(range, Options),
            format_file_range(FileName, {Start, 1}, {End, ?DEFAULT_WIDTH}, Options2)
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

-spec format_string(string(), [
    config_option() | {filename, string()} | {range, erlfmt_scan:location()}
]) ->
    {ok, string(), [error_info()]} | {skip, string()} | {error, error_info()}.
format_string(String, Options) ->
    Range = proplists:get_value(range, Options),
    case Range of
        undefined ->
            % Whole file (default).
            format_string_full(String, Options);
        {Start, End} ->
            % Remove 'range' property: when applicable we pass explicitly the range instead.
            % Also, match specifition of format_string_range.
            Options2 = proplists:delete(range, Options),
            format_string_range(String, {Start, 1}, {End, ?DEFAULT_WIDTH}, Options2)
    end.

-spec format_string_full(string(), [config_option() | {filename, string()}]) ->
    {ok, string(), [error_info()]} | {skip, string()} | {error, error_info()}.
format_string_full(String, Options) ->
    Filename = proplists:get_value(filename, Options, "nofile"),
    PrintWidth = proplists:get_value(print_width, Options, ?DEFAULT_WIDTH),
    Pragma = proplists:get_value(pragma, Options, ignore),
    try
        case read_nodes_string(Filename, String, Pragma) of
            {ok, Nodes0, Warnings} ->
                Nodes =
                    case Pragma of
                        insert -> insert_pragma_nodes(Nodes0);
                        delete -> remove_pragma_nodes(Nodes0);
                        _ -> Nodes0
                    end,
                Formatted = format_nodes(Nodes, PrintWidth),
                verify_nodes(Filename, Nodes, Formatted),
                VerboseWarnings =
                    case proplists:get_bool(verbose, Options) of
                        true -> check_line_lengths(Filename, PrintWidth, Formatted);
                        false -> []
                    end,
                {ok, unicode:characters_to_list(Formatted), Warnings ++ VerboseWarnings};
            {skip, RawString} ->
                {skip, RawString}
        end
    catch
        {error, Error} -> {error, Error}
    end.

which_pragma_node(Node) ->
    {PreComments, _, PostComments} = erlfmt_format:comments_with_pre_dot(Node),
    Comments = PreComments ++ PostComments,
    case lists:any(fun contains_noformat_pragma/1, Comments) of
        true ->
            noformat;
        false ->
            case lists:any(fun contains_format_pragma/1, Comments) of
                true -> format;
                false -> nopragma
            end
    end.

contains_format_pragma({comment, _Loc, Comments}) ->
    string:find(Comments, "@format") =/= nomatch.

contains_noformat_pragma({comment, _Loc, Comments}) ->
    string:find(Comments, "@noformat") =/= nomatch.

%% insert_pragma_nodes only inserts an @format comment,
%% if one has not already been inserted.
insert_pragma_nodes([]) ->
    [];
insert_pragma_nodes([{shebang, _, _} = Node | Nodes]) ->
    case which_pragma_node(Node) of
        format -> [replace_pragma_node(Node) | Nodes];
        noformat -> [Node | Nodes];
        nopragma -> [Node | insert_pragma_nodes(Nodes)]
    end;
insert_pragma_nodes([Node | Nodes]) ->
    case which_pragma_node(Node) of
        format -> [replace_pragma_node(Node) | Nodes];
        noformat -> [Node | Nodes];
        nopragma -> [insert_pragma_node(Node) | Nodes]
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
    case which_pragma_node(Node) of
        format -> [remove_pragma_node(Node) | Nodes];
        noformat -> [Node | Nodes];
        nopragma -> [Node | remove_pragma_nodes(Nodes)]
    end;
remove_pragma_nodes([Node | Nodes]) ->
    case which_pragma_node(Node) of
        format -> [remove_pragma_node(Node) | Nodes];
        _ -> [Node | Nodes]
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
    [{print_width, pos_integer()}]
) ->
    {ok, string(), [error_info()]}
    | {skip, string()}
    | {error, error_info()}.
format_file_range(FileName, StartLocation, EndLocation, Options) ->
    String = read_file_or_stdin(FileName),
    format_string_range(FileName, String, StartLocation, EndLocation, Options).

-spec format_string_range(
    string(),
    erlfmt_scan:location(),
    erlfmt_scan:location(),
    [{print_width, pos_integer()} | {filename, string()}]
) ->
    {ok, string(), [error_info()]}
    | {skip, string()}
    | {error, error_info()}.
format_string_range(Original, StartLocation, EndLocation, Options) ->
    Filename = proplists:get_value(filename, Options, "nofile"),
    format_string_range(Filename, Original, StartLocation, EndLocation, Options).
format_string_range(FileName, Original, StartLocation, EndLocation, Options) ->
    Pragma = proplists:get_value(pragma, Options, ignore),
    case read_nodes_string(FileName, Original, Pragma) of
        {ok, Nodes, Warnings} ->
            {{StartLine, _}, {EndLine, _}, Result} = format_enclosing_range(
                FileName,
                StartLocation,
                EndLocation,
                Options,
                Nodes,
                Warnings
            ),
            case Result of
                {ok, Formatted, Info} ->
                    Whole = inject_range(Original, StartLine, EndLine, Formatted),
                    {ok, Whole, Info};
                Other ->
                    % Error or skip.
                    Other
            end;
        {skip, String} ->
            % Happens with `noformat` pragma.
            {skip, String}
    end.

% Reinject formatted extract into whole string.
inject_range(Original, StartLine, EndLine, Formatted) ->
    AsList = string:split(unicode:characters_to_binary(Original), "\n", all),
    FormattedAsList0 = string:split(Formatted, "\n", all),
    % Remove spurious empty line introduced by last \n separator.
    FormattedAsList =
        case lists:last(FormattedAsList0) of
            <<>> ->
                lists:droplast(FormattedAsList0);
            _ ->
                FormattedAsList0
        end,
    true = EndLine >= StartLine,
    Len = EndLine - StartLine + 1,
    New = replace_slice(AsList, StartLine, Len, FormattedAsList),
    lists:join("\n", New).

format_enclosing_range(FileName, StartLocation, EndLocation, Options, Nodes, Warnings) ->
    case format_range(FileName, StartLocation, EndLocation, Options, Nodes, Warnings) of
        {options, []} ->
            % Nothing to format, due to noformat pragma or range outside of file,
            % return empty change (no warning should be emitted if noformat).
            {StartLocation, EndLocation, {skip, []}};
        {options, PossibleRanges} ->
            % Pick the largest range, so all intersected forms are covered.
            {Starts, Ends} = lists:unzip(PossibleRanges),
            Start = lists:min(Starts),
            End = lists:max(Ends),
            Res = format_range(FileName, Start, End, Options, Nodes, Warnings),
            % Poor man's assert (to avoid including assert.hrl)
            % This time we must have the formatted result.
            {ok, _, _} = Res,
            {Start, End, Res};
        X ->
            % Already ok or error: pass as is.
            {StartLocation, EndLocation, X}
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
    {ok, [erlfmt_parse:abstract_node()], [error_info()]} | {error, error_info()}.
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

% Apply 'Action' to file.
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

% Return file as one big string (with '\n' as line separator).
read_file_or_stdin(stdin) ->
    read_stdin([]);
read_file_or_stdin(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    unicode:characters_to_list(Bin).

read_stdin(Acc) ->
    case io:get_line("") of
        eof ->
            lists:flatten(lists:reverse(Acc, []));
        Line ->
            read_stdin([unicode:characters_to_list(Line) | Acc])
    end.

%% API entry point
-spec read_nodes_string(file:name_all(), string()) ->
    {ok, [erlfmt_parse:abstract_node()], [error_info()]} | {error, error_info()}.
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

read_nodes({ok, Tokens, Comments, Cont}, FileName, PragmaFlag, [], Warnings0, TextAcc) ->
    {Node, Warnings, Ignore} = parse_node(Tokens, Comments, FileName, Cont, Warnings0, false),
    case {which_pragma_node(Node), Node} of
        {_, {shebang, _, _}} ->
            {LastString, _Anno} = erlfmt_scan:last_node_string(Cont),
            read_nodes(
                erlfmt_scan:continue(Cont),
                FileName,
                PragmaFlag,
                [Node],
                Warnings,
                TextAcc ++ LastString
            );
        {nopragma, _} when PragmaFlag =:= require; PragmaFlag =:= delete ->
            skip_nodes(Cont, FileName, TextAcc);
        {noformat, _} ->
            skip_nodes(Cont, FileName, TextAcc);
        _ ->
            read_nodes_loop(erlfmt_scan:continue(Cont), FileName, [Node], Warnings, Ignore)
    end;
read_nodes(
    {ok, Tokens, Comments, Cont},
    FileName,
    PragmaFlag,
    [{shebang, _, _} = ShebangNode],
    Warnings0,
    TextAcc
) ->
    {Node, Warnings, Ignore} = parse_node(Tokens, Comments, FileName, Cont, Warnings0, false),
    case which_pragma_node(Node) of
        nopragma when PragmaFlag =:= require; PragmaFlag =:= delete ->
            skip_nodes(Cont, FileName, TextAcc);
        noformat ->
            skip_nodes(Cont, FileName, TextAcc);
        _ ->
            read_nodes_loop(
                erlfmt_scan:continue(Cont), FileName, [Node, ShebangNode], Warnings, Ignore
            )
    end;
read_nodes(Other, FileName, _PragmaFlag, NodeAcc, Warnings, _TextAcc) ->
    read_nodes_loop(Other, FileName, NodeAcc, Warnings, false).

skip_nodes(Cont, FileName, TextAcc) ->
    {LastString, _Anno} = erlfmt_scan:last_node_string(Cont),
    case erlfmt_scan:read_rest(Cont) of
        {ok, Rest} ->
            {skip, [TextAcc, LastString | Rest]};
        {error, {ErrLoc, Mod, Reason}} ->
            throw({error, {FileName, ErrLoc, Mod, Reason}})
    end.

read_nodes_loop({ok, Tokens, Comments, Cont}, FileName, Acc, Warnings0, Ignore0) ->
    {Node, Warnings, Ignore} = parse_node(Tokens, Comments, FileName, Cont, Warnings0, Ignore0),
    read_nodes_loop(erlfmt_scan:continue(Cont), FileName, [Node | Acc], Warnings, Ignore);
read_nodes_loop({eof, _Loc}, _FileName, Acc, Warnings, _Ignore0) ->
    {ok, lists:reverse(Acc), lists:reverse(Warnings)};
read_nodes_loop({error, {ErrLoc, Mod, Reason}, _Loc}, FileName, _Acc, _Warnings, _Ignore0) ->
    throw({error, {FileName, ErrLoc, Mod, Reason}}).

parse_node([], _Comments, _FileName, Cont, Warnings, Ignore) ->
    {node_string(Cont), Warnings, Ignore};
parse_node([{shebang, Meta, String}], Comments, _FileName, _Cont, Warnings, Ignore) ->
    Node = {shebang, erlfmt_recomment:put_post_comments(Meta, Comments), String},
    {Node, Warnings, Ignore};
parse_node([Token | _] = Tokens, Comments, FileName, Cont, Warnings, Ignore0) ->
    {PreComments, _} = erlfmt_recomment:take_comments(erlfmt_scan:get_line(Token), Comments),
    Ignore = ignore_state_pre(PreComments, FileName, Ignore0),
    NextIgnore = ignore_state_post([], FileName, Ignore),
    case Ignore of
        false ->
            case parse(Tokens) of
                {ok, Node} ->
                    {erlfmt_recomment:recomment(Node, Comments), Warnings, NextIgnore};
                {error, {ErrLoc, Mod, Reason}} ->
                    Warning = {FileName, ErrLoc, Mod, Reason},
                    {node_string(Cont), [Warning | Warnings], NextIgnore}
            end;
        _ ->
            {node_string(Cont), Warnings, NextIgnore}
    end.

parse(Tokens) ->
    case erlfmt_parse:parse_node(Tokens) of
        {ok, Node} ->
            {ok, Node};
        {error, Error} ->
            TokensWithoutMaybe = erlfmt_scan:downgrade_maybe(Tokens),
            case erlfmt_parse:parse_node(TokensWithoutMaybe) of
                {ok, Node} -> {ok, Node};
                _ -> {error, Error}
            end
    end.

ignore_state_pre(PreComments, FileName, Acc) ->
    case ignore_state(PreComments, FileName, Acc) of
        'end' -> false;
        Other -> Other
    end.

ignore_state_post(PostComments, FileName, Acc) ->
    case ignore_state(PostComments, FileName, Acc) of
        ignore -> false;
        'end' -> false;
        Other -> Other
    end.

ignore_state([{comment, Loc, Comments} | Rest], FileName, Acc) ->
    ignore_state(Comments, FileName, Loc, Rest, Acc);
ignore_state([], _FileName, Acc) ->
    Acc.

ignore_state([Line | Lines], FileName, Loc, Rest, Acc0) ->
    Acc =
        case string:trim(Line, both, "% ") of
            "erlfmt-ignore" when Acc0 =:= false -> ignore;
            "erlfmt-ignore" ->
                throw({error, {FileName, Loc, ?MODULE, {invalid_ignore, ignore, Acc0}}});
            "erlfmt-ignore-begin" when Acc0 =:= false -> 'begin';
            "erlfmt-ignore-begin" ->
                throw({error, {FileName, Loc, ?MODULE, {invalid_ignore, 'begin', Acc0}}});
            "erlfmt-ignore-end" when Acc0 =:= 'begin' -> 'end';
            "erlfmt-ignore-end" ->
                throw({error, {FileName, Loc, ?MODULE, {invalid_ignore, 'end', Acc0}}});
            _ ->
                Acc0
        end,
    ignore_state(Lines, FileName, Loc, Rest, Acc);
ignore_state([], FileName, _Loc, Rest, Acc) ->
    ignore_state(Rest, FileName, Acc).

node_string(Cont) ->
    {String, Anno} = erlfmt_scan:last_node_string_trimmed(Cont),
    {raw_string, Anno, String}.

-spec format_nodes([erlfmt_parse:abstract_node()], pos_integer()) -> [unicode:chardata()].
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

-spec format_node(erlfmt_parse:abstract_node(), pos_integer()) -> unicode:chardata().
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
    io_lib:format("line too long (~p > ~p)", [Length, Width]);
format_error({invalid_ignore, ignore, 'begin'}) ->
    "invalid erlfmt-ignore while in erlfmt-ignore-begin section";
format_error({invalid_ignore, Same, Same}) ->
    "duplicate ignore comment";
format_error({invalid_ignore, 'end', false}) ->
    "invalid erlfmt-ignore-end while outside of erlfmt-ignore-begin section";
format_error({invalid_ignore, Given, Previous}) ->
    io_lib:format("invalid ignore specification ~ts while in ~ts state", [Given, Previous]).

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

% Replace the sublist(Target, Start, Len) by Injected.
% E.g. if Injected is empty it will just remove the sublist.
-spec replace_slice(list(), pos_integer(), non_neg_integer(), list()) -> list().
replace_slice(Target, Start, Len, Injected) ->
    Res =
        lists:sublist(Target, Start - 1) ++
            Injected ++
            lists:sublist(Target, Start + Len, length(Target)),
    true = length(Target) - Len + length(Injected) =:= length(Res),
    Res.
