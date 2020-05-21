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
    read_forms/1,
    read_forms_string/2,
    format_error/1,
    format_error_info/1,
    contains_pragma_forms/1
]).

-export_type([error_info/0, out/0, config/0]).

-type error_info() :: {file:name_all(), erl_anno:location(), module(), Reason :: any()}.

-type out() :: standard_out | {path, file:name_all()} | replace.

-type config() :: {RequirePragma :: boolean(), Out :: out()}.

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
-spec format_file(file:name_all(), config()) -> {ok, [error_info()]} | {error, error_info()}.
format_file(FileName, {RequirePragma, Out}) ->
    try
        {ok, Forms, Warnings} = file_read_forms(FileName),
        ShouldFormat = not RequirePragma or contains_pragma_forms(Forms),
        case ShouldFormat of
            true -> [$\n | Formatted] = format_forms(Forms),
                    verify_forms(FileName, Forms, Formatted),
                    write_forms(FileName, Formatted, Out),
                    {ok, Warnings};
            false -> {ok, Warnings}
        end
    catch
        {error, Error} -> {error, Error}
    end.

-spec contains_pragma_forms([erlfmt_parse:abstract_form()]) -> boolean().
contains_pragma_forms(Forms) -> lists:any(fun contains_pragma_form/1, Forms).

contains_pragma_form(Form) ->
    case Form of
        {attribute, Meta, _AfAtom, _AbstractExprs} ->
            {PreComments, PostComments} = erlfmt_format:comments(Meta),
            lists:any(fun contains_pragma_comment/1, PreComments ++ PostComments);
        _ -> false
        end.

contains_pragma_comment({comment, _Loc, Comments}) ->
    lists:any(fun (Comment) ->
        case string:find(Comment, "@format") of
            nomatch -> false;
            _ -> true
        end
    end, Comments);
contains_pragma_comment(_) -> false.

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
        {ok, Forms, Warnings} = file_read_forms(FileName),
        case verify_ranges(Forms, StartLocation, EndLocation) of
            {ok, FormsInRange} ->
                [$\n | Result] = format_forms(FormsInRange),
                verify_forms(FileName, FormsInRange, Result),
                {ok, unicode:characters_to_binary(Result), Warnings};
            {options, Options} ->
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

file_read_forms(FileName) ->
    read_file(FileName, fun (File) ->
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
    {String, Anno} = erlfmt_scan:last_form_string(Cont),
    {raw_string, Anno, string:trim(String, both, "\n")}.

format_forms([{attribute, _, {atom, _, spec}, _} = Attr, {function, _, _} = Fun | Rest]) ->
    [$\n, format_form(Attr), format_form(Fun) | format_forms(Rest)];
format_forms([Form | Rest]) ->
    [$\n, format_form(Form) | format_forms(Rest)];
format_forms([]) ->
    [].

format_form({raw_string, _Anno, String}) ->
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

verify_ranges(Forms, StartLocation, EndLocation) ->
    ApplicableForms = forms_in_range(Forms, StartLocation, EndLocation),
    case possible_ranges(ApplicableForms, StartLocation, EndLocation) of
        [{StartLocation, EndLocation}] ->
            {ok, ApplicableForms};
        Options ->
            {options, Options}
    end.

% Returns ranges which starts with the start of a form and ends with the end of form
possible_ranges(Forms, StartLocation, EndLocation) ->
    case Forms of
        [] ->
            [];
        [OnlyForm] ->
            [get_location_range(OnlyForm)];
        MultipleForms ->
            combine(
                get_possible_locations(MultipleForms, StartLocation, fun get_location/1),
                get_possible_locations(lists:reverse(MultipleForms), EndLocation, fun get_end_location/1))
    end.

forms_in_range(Forms, StartLocation, EndLocation) ->
    [Form || Form <- Forms, form_intersects_range(Form, StartLocation, EndLocation)].

form_intersects_range(Form, StartLocation, EndLocation) ->
    {Start, End} = get_location_range(Form),
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

get_location_range(Form) ->
    {get_location(Form), get_end_location(Form)}.

get_location(Form) ->
    erlfmt_scan:get_anno(location, Form).

get_end_location(Form) ->
    erlfmt_scan:get_anno(end_location, Form).
