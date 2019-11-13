-module(erlfmt).

-oncall("whatsapp_erlang").

-typing([dialyzer]).

%% API exports
-export([main/1]).

-record(args, {
    verbose = false :: boolean(),
    files = [] :: [string()]
}).

%% escript entry point
main(Argv) ->
    try run_command(Argv) of
        ok ->
            erlang:halt(0)
        % Commented out for now to satisfy dialyzer
        % {error, Error} ->
        %     print_error(Error),
        %     erlang:halt(1)
    catch
        Class:Error:Stacktrace ->
            Msg = "internal error while running ~s:\n\t~p\nStacktrace:\n~p\n",
            Args = [command_name(), {Class, Error}, Stacktrace],
            io:format(standard_error, Msg, Args)
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
    Args = parse_args(Argv),
    run_format(Args).

parse_args(Argv) -> parse_args(Argv, #args{}).

parse_args(["-v" | Rest], Args) ->
    parse_args(Rest, Args#args{verbose = true});
parse_args(["-h" | _Rest], _Args) ->
    print_usage(),
    erlang:halt(0);
parse_args([Name | Rest], Args0) ->
    Args = Args0#args{files = [Name | Args0#args.files]},
    parse_args(Rest, Args);
parse_args([], Args) ->
    Args#args{
        files = lists:reverse(Args#args.files)
    }.

run_format(#args{files = FileNames} = Args) ->
    lists:foreach(fun(FileName) -> format_file(FileName, Args) end, FileNames),
    ok.

format_file(FileName, Args) ->
    case Args#args.verbose of
        true -> io:format("Formatting ~s\n", [FileName]);
        false -> ok
    end,
    %% TODO: actual formatting
    ok.
