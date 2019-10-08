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
-export([main/1]).

-record(args, {
    verbose = false :: boolean(),
    files = [] :: [string()]
}).

%% escript entry point
main(Argv) ->
    try run_command(Argv) of
        ok ->
            erlang:halt(0);
        {error, Error} ->
            print_error(Error),
            erlang:halt(1)
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
    case parse_args(Argv) of
        {Args, []} ->
            run_format(Args);
        {_Args, Errs} ->
            Formatted = lists:join("\n", Errs),
            {error, ["invalid files given to ", command_name(), ":\n" | Formatted]}
    end.

parse_args(Argv) -> parse_args(Argv, #args{}, []).

parse_args(["-v" | Rest], Args, Errs) ->
    parse_args(Rest, Args#args{verbose = true}, Errs);
parse_args(["-h" | _Rest], _Args, _Errs) ->
    print_usage(),
    erlang:halt(0);
parse_args([Name | Rest], Args0, Errs) ->
    Args = Args0#args{files = [Name | Args0#args.files]},
    parse_args(Rest, Args, Errs);
parse_args([], Args, Errs) ->
    {
        Args#args{
            files = lists:reverse(Args#args.files)
        },
        lists:reverse(Errs)
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

print_error(Error) ->
    %% TODO: print nicely
    io:format(standard_error, "~p\n", [Error]).
