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
    format_range/4,
    read_forms/1,
    read_forms_string/2,
    format_error/1,
    format_error_info/1
]).

-export_type([error_info/0, out/0]).

-type error_info() :: {file:name_all(), erl_anno:location(), module(), Reason :: any()}.

-type out() :: standard_out | {path, file:name_all()} | replace.

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
        {ok, {ArgOpts, _Extra}} ->
            erlfmt_cli:do(ArgOpts, "erlfmt");
        {error, Error} ->
            io:put_chars(standard_error, [getopt:format_error(Opts, Error), "\n\n"]),
            getopt:usage(Opts, "erlfmt")
    end.

%% rebar3 plugin entry point
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_fmt_prv:init(State).

%% API entry point
-spec format_file(file:name_all(), out()) -> {ok, [error_info()]} | {error, error_info()}.
format_file(FileName, Out) ->
    try
        {ok, Forms, Warnings} = file_read_forms(FileName),
        [$\n | Formatted] = format_forms(Forms),
        verify_forms(FileName, Forms, Formatted),
        write_forms(FileName, Formatted, Out),
        {ok, Warnings}
    catch
        {error, Error} -> {error, Error}
    end.

-spec format_range(
          file:name_all(),
          out(),
          erlfmt_scan:location(),
          erlfmt_scan:location()
      ) ->
          {ok, [error_info()]} |
              {error, error_info()} |
              {options, [{erlfmt_scan:location(), erlfmt_scan:location()}]}.
format_range(FileName, Out, StartLocation, EndLocation) ->
    try
        {ok, Forms, Warnings} = file_read_forms(FileName),
        case verify_ranges(Forms, StartLocation, EndLocation) of
            ok ->
                InRange = get_forms_in_range(Forms, StartLocation, EndLocation),
                [$\n | Result] = format_forms(InRange),
                verify_forms(FileName, InRange, Result),
                Prefix = get_prefix(FileName, StartLocation),
                Suffix = get_suffix(FileName, EndLocation),
                write_forms(FileName, [Prefix, Result, Suffix], Out),
                {ok, Warnings};
            {error, Options} ->
                {options, Options}
        end
    catch
        {error, Error} -> {error, Error}
    end.

%% API entry point
-spec read_forms(file:name_all()) ->
          {ok, [erlfmt_parse:abstract_form()], [error_info()]} | {error, error_info()}.
read_forms(FileName) ->
    try file_read_forms(FileName)
    catch
        {error, Error} -> {error, Error}
    end.

get_prefix(FileName, {Lines, Chars}) ->
    read_file(FileName, fun
        (File) ->
            read_lines_and_chars(File, Lines, Chars)
    end).

get_suffix(FileName, {Lines, Chars}) ->
    read_file(FileName, fun
        (File) ->
            skip_lines_and_chars(File, Lines, Chars)
    end).

file_read_forms(FileName) ->
    read_file(FileName, fun
        (File) ->
            read_forms(erlfmt_scan:io_form(File), FileName, [], [])
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
-spec read_forms_string(file:name_all(), string()) ->
          {ok, [erlfmt_parse:abstract_form()], [error_info()]} | {error, error_info()}.
read_forms_string(FileName, String) ->
    try read_forms(erlfmt_scan:string_form(String), FileName, [], [])
    catch
        {error, Error} -> {error, Error}
    end.

read_forms({ok, Tokens, Comments, Cont}, FileName, Acc, Warnings0) ->
    {Form, Warnings} = parse_form(Tokens, Comments, FileName, Cont, Warnings0),
    read_forms(erlfmt_scan:continue(Cont), FileName, [Form | Acc], Warnings);
read_forms({eof, _Loc}, _FileName, Acc, Warnings) ->
    {ok, lists:reverse(Acc), lists:reverse(Warnings)};
read_forms({error, {ErrLoc, Mod, Reason}, _Loc}, FileName, _Acc, _Warnings) ->
    throw({error, {FileName, ErrLoc, Mod, Reason}}).

parse_form([], _Comments, _FileName, Cont, Warnings) ->
    {form_string(Cont), Warnings};
parse_form(Tokens, Comments, FileName, Cont, Warnings) ->
    case erlfmt_parse:parse_form(Tokens) of
        {ok, Form0} ->
            {erlfmt_recomment:recomment(Form0, Comments), Warnings};
        {error, {ErrLoc, Mod, Reason}} ->
            Warning = {FileName, ErrLoc, Mod, Reason},
            {form_string(Cont), [Warning | Warnings]}
    end.

form_string(Cont) ->
    {raw_string, string:trim(erlfmt_scan:last_form_string(Cont), both, "\n")}.

format_forms([{attribute, _, {atom, _, spec}, _} = Attr, {function, _, _} = Fun | Rest]) ->
    [$\n, format_form(Attr), format_form(Fun) | format_forms(Rest)];
format_forms([Form | Rest]) ->
    [$\n, format_form(Form) | format_forms(Rest)];
format_forms([]) ->
    [].

format_form({raw_string, String}) ->
    [String, $\n];
format_form(Form) ->
    Doc = erlfmt_format:form_to_algebra(Form),
    [erlfmt_algebra:document_render(Doc, [{page_width, ?PAGE_WIDTH}]), $\n].

verify_forms(FileName, Forms, Formatted) ->
    case read_forms_string(FileName, unicode:characters_to_list(Formatted)) of
        {ok, Forms2, _} ->
            try equivalent_list(Forms, Forms2)
            catch
                {not_equivalent, Left, Right} ->
                    Location = try_location(Left, Right),
                    throw(
                        {error,
                            {FileName, Location, ?MODULE, {not_equivalent, Left, Right}}}
                    )
            end;
        {error, _} ->
            throw({error, {FileName, 0, ?MODULE, could_not_reparse}})
    end.

equivalent(Element, Element) ->
    true;
equivalent({raw_string, RawL}, {raw_string, RawR}) ->
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

write_forms(_FileName, Formatted, standard_out) ->
    io:put_chars(Formatted);
write_forms(FileName, Formatted, Out) ->
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

verify_ranges(Forms, StartLocation, EndLocation) ->
    case possible_ranges(Forms, StartLocation, EndLocation) of
        [{StartLocation, EndLocation}] ->
            ok;
        Options ->
            {error, {possible_ranges, Options}}
    end.

possible_ranges([Form | Forms], StartLocation, EndLocation) ->
    case get_location_range(Form) of
        % Form is entirely before range
        {_Start, End} when End < StartLocation ->
            possible_ranges(Forms, StartLocation, EndLocation);
        % Form is entirely after range
        {Start, _End} when Start >= EndLocation ->
            [{StartLocation, EndLocation}];
        % Range contains form
        {Start, End} when Start >= StartLocation andalso End < EndLocation ->
            combine([StartLocation], possible_end_locations(Forms, EndLocation));
        % Form is the only form in range but extends outside it
        {Start, End} when End >= EndLocation ->
            [{Start, End}];
        % Form starts outside range and end in range
        {Start, End} ->
            combine([Start, next_position(End)], possible_end_locations(Forms, EndLocation))
    end;
possible_ranges([], StartLocation, EndLocation) ->
    [{StartLocation, EndLocation}].

combine(L1, L2) ->
    [{X1, X2} || X1 <- L1, X2 <- L2].

next_position({X, Y}) ->
    {X, Y + 1}.

possible_end_locations([Form | Forms], EndLocation) ->
    case get_location_range(Form) of
        {Start, _End} when Start >= EndLocation ->
            [EndLocation];
        {_Start, End} when End =< EndLocation ->
            possible_end_locations(Forms, EndLocation);
        {Start, End} ->
            [Start, next_position(End)]
    end;
possible_end_locations([], EndLocation) ->
    [EndLocation].

get_forms_in_range([Form | Forms], StartLocation, EndLocation) ->
    case get_location_range(Form) of
        {Start, End} when Start >= StartLocation andalso End < EndLocation ->
            [Form | get_forms_in_range(Forms, StartLocation, EndLocation)];
        {_Start, End} when End < StartLocation ->
            get_forms_in_range(Forms, StartLocation, EndLocation);
        {Start, _End} when Start >= EndLocation ->
            []
    end;
get_forms_in_range([], _StartLocation, _EndLocation) ->
    [].

get_location_range(Form) ->
    {erlfmt_scan:get_anno(location, Form), erlfmt_scan:get_anno(end_location, Form)}.

read_lines_and_chars(File, Lines, Chars) when Lines > ?FIRST_LINE ->
    case io:get_line(File, "") of
        eof ->
            [];
        {error, Description} ->
            throw({error, Description});
        Data ->
            [Data | read_lines_and_chars(File, Lines - 1, Chars)]
    end;
read_lines_and_chars(File, ?FIRST_LINE, Chars) ->
    case io:get_chars(File, "", Chars - ?FIRST_COLUMN) of
        eof ->
            [];
        {error, Description} ->
            throw({error, Description});
        Data ->
            [Data]
    end.

skip_lines_and_chars(File, Lines, Chars) when Lines > ?FIRST_LINE ->
    case io:get_line(File, "") of
        eof ->
            [];
        {error, Description} ->
            throw({error, Description});
        _Data ->
            skip_lines_and_chars(File, Lines - 1, Chars)
    end;
skip_lines_and_chars(File, ?FIRST_LINE, Chars) ->
    case io:get_chars(File, "", Chars - ?FIRST_COLUMN) of
        eof ->
            [];
        {error, Description} ->
            throw({error, Description});
        _Data ->
            read_rest_of_file(File)
    end.

read_rest_of_file(File) ->
    case io:get_line(File, "") of
        eof ->
            [];
        {error, Description} ->
            throw({error, Description});
        Data ->
            [Data | read_rest_of_file(File)]
    end.

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
