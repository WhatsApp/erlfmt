-module(erlfmt).

-oncall("whatsapp_erlang").

-typing([dialyzer]).

%% API exports
-export([main/1, format_file/2, read_forms/1, read_forms_string/2, format_error/1, format_error_info/1]).

-export_type([error_info/0]).

-type error_info() :: {file:filename(), erl_anno:location(), module(), Reason :: any()}.

-type option() :: verbose | {out, file:name_all()}.

-record(state, {
    verbose = false :: boolean(),
    out = replace :: file:name_all() | replace,
    files = [] :: [file:name_all()],
    warnings = [] :: [error_info()],
    errors = [] :: [error_info()]
}).

-define(PAGE_WIDTH, 92).

-define(SCAN_START, {1, 1}).

-define(SCAN_OPTS, [text, return_comments]).

-define(COMMAND_NAME, "erlfmt").

%% escript entry point
-spec main([string()]) -> no_return().
main(Argv) ->
    State = parse_args(Argv, #state{}),
    try run_format(State) of
        #state{errors = [], warnings = []} ->
            erlang:halt(0);
        #state{errors = [], warnings = Warnings} ->
            [print_warning(Warning) || Warning <- Warnings],
            erlang:halt(0);
        #state{errors = Errors, warnings = Warnings} ->
            [print_warning(Warning) || Warning <- Warnings],
            [print_error(Error) || Error <- Errors],
            erlang:halt(1)
    catch
        Class:Error:Stack ->
            io:format(standard_error, "internal error while running ~s:\n", [?COMMAND_NAME]),
            erlang:raise(Class, Error, Stack)
    end.

%% API entry point
-spec format_file(file:name_all(), [option()]) -> {ok, [error_info()]} | {error, [error_info(), ...], [error_info()]}.
format_file(Path, Opts) ->
    State = parse_opts(Opts, #state{files = [Path]}),
    case run_format(State) of
        #state{errors = [], warnings = Warnings} ->
            {ok, Warnings};
        #state{errors = Errors, warnings = Warnings} ->
            {error, Errors, Warnings}
    end.

print_usage() ->
    io:format("~s", [[
"Usage: ", ?COMMAND_NAME, " [-vh] [-o Path] [files...]

", ?COMMAND_NAME, " is a code formatter for Erlang.

Arguments:

  files -- files to format

Options:

  -v -- verbose mode

  -o Path -- output path (default \".\" replacing original files)

  --help (-h) -- print this help message

"
    ]]).

parse_args(["-v" | Rest], State) ->
    parse_args(Rest, State#state{verbose = true});
parse_args(["-o", Path | Rest], State) ->
    parse_args(Rest, State#state{out = Path});
parse_args([Help | _Rest], _State) when Help =:= "-h"; Help =:= "--help" ->
    print_usage(),
    erlang:halt(0);
parse_args([Name | Rest], State0) ->
    State = State0#state{files = [Name | State0#state.files]},
    parse_args(Rest, State);
parse_args([], #state{files = []}) ->
    print_usage(),
    erlang:halt(0);
parse_args([], State) ->
    State#state{files = lists:reverse(State#state.files)}.

parse_opts([verbose | Rest], State) ->
    parse_opts(Rest, State#state{verbose = true});
parse_opts([{out, Path} | Rest], State) ->
    parse_opts(Rest, State#state{out = Path});
parse_opts([], State) ->
    State.

run_format(State) ->
    #state{errors = Errors, warnings = Warnings} =
        lists:foldl(fun run_format_file/2, State, State#state.files),
    #state{errors = lists:reverse(Errors), warnings = lists:reverse(Warnings)}.

run_format_file(FileName, State0) ->
    case State0#state.verbose of
        true -> io:format(standard_error, "Formatting ~s\n", [FileName]);
        false -> ok
    end,
    try
        {Forms, State1} = read_forms(FileName, State0),
        [$\n | Formatted] = format_forms(Forms),
        verify_forms(FileName, Forms, Formatted),
        State2 = write_forms(FileName, Formatted, State1),
        State2
    catch
        {error, Error} ->
            #state{errors = [Error | State0#state.errors]}
    end.

%% API entry point
-spec read_forms(file:name_all()) ->
    {ok, [erlfmt_parse:abstract_form()], [error_info()]} | {error, error_info()}.
read_forms(FileName) ->
    try read_forms(FileName, #state{}) of
        {Forms, #state{errors = [], warnings = Warnings}} ->
            {ok, Forms, Warnings}
    catch
        {error, Error} ->
            {error, Error}
    end.

read_forms(FileName, State) ->
    case file:open(FileName, [read]) of
        {ok, File} ->
            try read_forms(erlfmt_scan:io_form(File), FileName, [], State)
            after file:close(File)
            end;
        {error, Reason} ->
            throw({error, {FileName, 0, file, Reason}})
    end.

%% API entry point
-spec read_forms_string(file:name_all(), string()) ->
    {ok, [erlfmt_parse:abstract_form()], [error_info()]} | {error, error_info()}.
read_forms_string(FileName, String) ->
    try read_forms(erlfmt_scan:string_form(String), FileName, [], #state{}) of
        {Forms, #state{warnings = Warnings}} ->
            {ok, Forms, Warnings}
    catch
        {error, Error} ->
            {error, Error}
    end.

read_forms({ok, Tokens, Comments, Cont}, FileName, Acc, State0) ->
    {Form, State} = parse_form(Tokens, Comments, FileName, Cont, State0),
    read_forms(erlfmt_scan:continue(Cont), FileName, [Form | Acc], State);
read_forms({eof, _Loc}, _FileName, Acc, State) ->
    {lists:reverse(Acc), State};
read_forms({error, {ErrLoc, Mod, Reason}, _Loc}, FileName, _Acc, _State) ->
    throw({error, {FileName, ErrLoc, Mod, Reason}}).

parse_form([], _Comments, _FileName, Cont, State) ->
    {form_string(Cont), State};
parse_form(Tokens, Comments, FileName, Cont, State0) ->
    case erlfmt_parse:parse_form(Tokens) of
        {ok, Form0} ->
            {erlfmt_recomment:recomment(Form0, Comments), State0};
        {error, {ErrLoc, Mod, Reason}} ->
            Warning = {FileName, ErrLoc, Mod, Reason},
            State = State0#state{warnings = [Warning | State0#state.warnings]},
            {form_string(Cont), State}
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
                    throw({error, {FileName, Location, ?MODULE, {not_equivalent, Left, Right}}})
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
equivalent({Type, _, L1, L2, L3}, {Type,_, R1, R2, R3}) ->
    equivalent(L1, R1) andalso equivalent(L2, R2) andalso equivalent(L3, R3);
equivalent({Type, _, L1, L2, L3, L4}, {Type, _, R1, R2, R3, R4}) ->
    equivalent(L1, R1) andalso equivalent(L2, R2) andalso equivalent(L3, R3) andalso equivalent(L4, R4);
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

write_forms(FileName, Formatted, State) ->
    OutFileName = out_file(FileName, State),
    case filelib:ensure_dir(OutFileName) of
        ok -> ok;
        {error, Reason1} -> throw({error, {OutFileName, 0, file, Reason1}})
    end,
    case file:write_file(OutFileName, unicode:characters_to_binary(Formatted)) of
        ok -> State;
        {error, Reason2} -> throw({error, {OutFileName, 0, file, Reason2}})
    end.

out_file(FileName, #state{out = replace}) ->
    FileName;
out_file(FileName, #state{out = Path}) ->
    filename:join(Path, filename:basename(FileName)).

%% Keep warnings and errors separate in case we want to colour them
print_warning(Warning) ->
    io:put_chars(standard_error, [format_error_info(Warning), $\n]).

print_error(Error) ->
    io:put_chars(standard_error, [format_error_info(Error), $\n]).

-spec format_error_info(error_info()) -> unicode:chardata().
format_error_info({FileName, Anno, Mod, Reason}) ->
    io_lib:format("~ts~s: ~ts", [FileName, format_loc(Anno), Mod:format_error(Reason)]).

format_loc(0) -> "";
format_loc({Line, Col}) -> io_lib:format(":~B:~B", [Line, Col]);
format_loc(#{location := {Line, Col}}) -> io_lib:format(":~B:~B", [Line, Col]);
format_loc(Line) when is_integer(Line) -> io_lib:format(":~B", [Line]).

format_error({not_equivalent, Node1, Node2}) ->
    io_lib:format("formatter result not equivalent. Please report this bug.~n~n~p~n~n~p", [Node1, Node2]);
format_error(could_not_reparse) ->
    "formatter result invalid, could not reparse".
