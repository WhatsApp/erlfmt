-module(erlfmt).

-oncall("whatsapp_erlang").

-typing([dialyzer]).

%% API exports
-export([main/1, read_forms/1, read_forms_string/2, format_error_info/1]).

-export_type([error_info/0]).

-type error_info() :: {file:filename(), erl_anno:location(), module(), Reason :: any()}.

-record(state, {
    verbose = false :: boolean(),
    files = [] :: [string()],
    warnings = [] :: [error_info()],
    errors = [] :: [error_info()]
}).

-define(PAGE_WIDTH, 92).

-define(SCAN_START, {1, 1}).

-define(SCAN_OPTS, [text, return_comments]).

%% escript entry point
main(Argv) ->
    try run_command(Argv) of
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
        Class:Error:Stacktrace ->
            Msg = "internal error while running ~s:\n\t~p\nStacktrace:\n~p\n",
            Args = [command_name(), {Class, Error}, Stacktrace],
            io:format(standard_error, Msg, Args),
            erlang:halt(1)
    end.

command_name() ->
    "erlfmt".

print_usage() ->
    io:format("~s", [[
"Usage: ", command_name(), " [-vh] [files...]

", command_name(), " is a code formatter for Erlang.

Arguments:

  files -- files to format

Options:

  -v -- verbose mode

  -h -- print this help message

"
    ]]).

run_command(Argv) ->
    State = parse_args(Argv),
    run_format(State).

parse_args(Argv) -> parse_args(Argv, #state{}).

parse_args(["-v" | Rest], State) ->
    parse_args(Rest, State#state{verbose = true});
parse_args(["-h" | _Rest], _State) ->
    print_usage(),
    erlang:halt(0);
parse_args([Name | Rest], State0) ->
    State = State0#state{files = [Name | State0#state.files]},
    parse_args(Rest, State);
parse_args([], State) ->
    State#state{files = lists:reverse(State#state.files)}.

run_format(State) ->
    #state{errors = Errors, warnings = Warnings} =
        lists:foldl(fun format_file/2, State, State#state.files),
    #state{errors = lists:reverse(Errors), warnings = lists:reverse(Warnings)}.

format_file(FileName, State0) ->
    case State0#state.verbose of
        true -> io:format(standard_error, "Formatting ~s\n", [FileName]);
        false -> ok
    end,
    try
        {Forms, State1} = read_forms(FileName, State0),
        Formatted = lists:join("\n", lists:map(fun format_form/1, Forms)),
        State2 = write_forms(FileName, Formatted, State1),
        State2
    catch
        {error, Error} ->
            #state{errors = [Error | State0#state.errors]}
    end.

%% Entry-point to the parser
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
    {erlfmt_scan:last_form_string(Cont), State};
parse_form(Tokens, Comments, FileName, Cont, State0) ->
    case erlfmt_parse:parse_form(Tokens) of
        {ok, Form0} ->
            {erlfmt_recomment:recomment(Form0, Comments), State0};
        {error, {ErrLoc, Mod, Reason}} ->
            Warning = {FileName, ErrLoc, Mod, Reason},
            State = #state{warnings = [Warning | State0#state.warnings]},
            {erlfmt_scan:last_form_string(Cont), State}
    end.

format_form(String) when is_list(String) ->
    String;
format_form(Form) ->
    Doc = erlfmt_format:form_to_algebra(Form),
    [erlfmt_algebra:document_render(Doc, [{page_width, ?PAGE_WIDTH}]), $\n].

write_forms(FileName, Formatted, State) ->
    case file:write_file(FileName, unicode:characters_to_binary(Formatted)) of
        ok ->
            State;
        {error, Reason} ->
            throw({error, {FileName, 0, file, Reason}})
    end.

%% Keep warnings and errors separate in case we want to colour them
print_warning(Warning) ->
    io:put_chars(standard_error, [format_error_info(Warning), $\n]).

print_error(Error) ->
    io:put_chars(standard_error, [format_error_info(Error), $\n]).

-spec format_error_info(error_info()) -> unicode:chardata().
format_error_info({FileName, {Line, Col}, Mod, Reason}) ->
    io_lib:format("~ts:~B:~B: ~ts", [FileName, Line, Col, Mod:format_error(Reason)]);
format_error_info({FileName, Line, Mod, Reason}) ->
    io_lib:format("~ts:~B: ~ts", [FileName, Line, Mod:format_error(Reason)]).
