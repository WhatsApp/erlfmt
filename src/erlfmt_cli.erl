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
-module(erlfmt_cli).

-export([opts/0, do/2]).

-record(config, {
    verbose = false :: boolean(),
    width = undefined :: undefined | pos_integer(),
    pragma = ignore :: erlfmt:pragma(),
    out = standard_out :: erlfmt:out()
}).

-spec opts() -> [getopt:option_spec()].
opts() ->
    [
        {help, $h, "help", undefined, "print this message"},
        {version, $v, "version", undefined, "print version"},
        {write, $w, "write", undefined, "modify formatted files in place"},
        {out, $o, "out", binary, "output directory"},
        {verbose, undefined, "verbose", undefined, "include debug output"},
        {check, $c, "check", undefined,
            "Check if your files are formatted."
            "Get exit code 1, if some files are not formatted."
            "stdin is not supported and --write is not supported."},
        {print_width, undefined, "print-width", integer,
            "The line length that formatter would wrap on"},
        {require_pragma, undefined, "require-pragma", undefined,
            "Require a special comment @format, called a pragma, "
            "to be present in the file's first docblock comment in order for prettier to format it."},
        {insert_pragma, undefined, "insert-pragma", undefined,
            "Insert a @format pragma to the top of formatted files when pragma is absent."
            "Works well when used in tandem with --require-pragma, but"
            "it is not allowed to use require-pragma and insert-pragma at the same time."},
        {files, undefined, undefined, string, "files to format, - for stdin"}
    ].

-spec do(list(), string()) -> ok.
do(Opts, Name) ->
    try
        do_unprotected(Opts, Name)
    catch
        Kind:Reason:Stack ->
            io:format(standard_error, "~s Internal Error~n~s:~p~n~p~n", [Name, Kind, Reason, Stack]),
            erlang:halt(127)
    end.

-spec do_unprotected(list(), string()) -> ok.
do_unprotected(Opts, Name) ->
    case parse_opts(Opts, Name, [], #config{}) of
        {format, Files, Config} ->
            case Config#config.out of
                check -> io:format(standard_error, "Checking formatting...~n", []);
                _ -> ok
            end,
            case parallel(fun(File) -> format_file(File, Config) end, Files) of
                ok ->
                    ok;
                warn ->
                    io:format(
                        standard_error,
                        "[warn] Code style issues found in the above file(s). Forgot to run erlfmt?~n",
                        []
                    ),
                    erlang:halt(1);
                error ->
                    erlang:halt(4)
            end;
        {error, Message} ->
            io:put_chars(standard_error, [Message, "\n\n"]),
            getopt:usage(opts(), Name),
            erlang:halt(2)
    end.

format_file(FileName, Config) ->
    #config{pragma = Pragma, width = Width, verbose = Verbose, out = Out} = Config,
    case Verbose of
        true -> io:format(standard_error, "Formatting ~s\n", [FileName]);
        false -> ok
    end,
    Options = [{pragma, Pragma}] ++ [{width, Width} || Width =/= undefined],
    case erlfmt:format_file(FileName, Out, Options) of
        {ok, Warnings} ->
            [print_error_info(Warning) || Warning <- Warnings],
            ok;
        {check_failed, _, _, Warnings} ->
            [print_error_info(Warning) || Warning <- Warnings],
            io:format(standard_error, "[warn] ~s\n", [FileName]),
            warn;
        skip when Verbose ->
            io:format(standard_error, "Skipping ~s because of missing @format pragma\n", [FileName]),
            skip;
        skip ->
            skip;
        {error, Error} ->
            print_error_info(Error),
            error
    end.

parse_opts([help | _Rest], Name, _Files, _Config) ->
    getopt:usage(opts(), Name),
    erlang:halt(0);
parse_opts([version | _Rest], Name, _Files, _Config) ->
    {ok, Vsn} = application:get_key(erlfmt, vsn),
    io:format("~s version ~s\n", [Name, Vsn]),
    erlang:halt(0);
parse_opts([write | _Rest], _Name, _Files, #config{out = check}) ->
    {error, "--write replace mode can't be combined check mode"};
parse_opts([write | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{out = replace});
parse_opts([{out, Path} | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{out = {path, Path}});
parse_opts([verbose | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{verbose = true});
parse_opts([check | _Rest], _Name, _Files, #config{out = replace}) ->
    {error, "--write replace mode can't be combined check mode"};
parse_opts([check | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{out = check});
parse_opts([{print_width, Value} | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{width = Value});
parse_opts([require_pragma | _Rest], _Name, _Files, #config{pragma = insert}) ->
    {error, "Cannot use both --insert-pragma and --require-pragma options together."};
parse_opts([require_pragma | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{pragma = require});
parse_opts([insert_pragma | _Rest], _Name, _Files, #config{pragma = require}) ->
    {error, "Cannot use both --insert-pragma and --require-pragma options together."};
parse_opts([insert_pragma | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{pragma = insert});
parse_opts([{files, NewFiles} | Rest], Name, Files0, Config) ->
    parse_opts(Rest, Name, expand_files(NewFiles, Files0), Config);
parse_opts([], _Name, [stdin], #config{out = Out}) when Out =/= standard_out ->
    {error, "stdin mode can't be combined with out or write options"};
parse_opts([], _Name, [stdin], #config{out = check}) ->
    {error, "stdin mode can't be combined check mode"};
parse_opts([], _Name, [stdin], Config) ->
    {format, [stdin], Config};
parse_opts([], _Name, [], _Config) ->
    {error, "no files provided to format"};
parse_opts([], _Name, Files, Config) ->
    case lists:member(stdin, Files) of
        true -> {error, "stdin mode can't be combined with other files"};
        false -> {format, lists:reverse(Files), Config}
    end.

expand_files("-", Files) ->
    [stdin | Files];
expand_files(NewFile, Files) when is_integer(hd(NewFile)) ->
    case filelib:is_regular(NewFile) of
        true ->
            [NewFile | Files];
        false ->
            case filelib:wildcard(NewFile) of
                [] ->
                    io:format(standard_error, "no file matching '~s'", [NewFile]),
                    Files;
                NewFiles ->
                    NewFiles ++ Files
            end
    end;
expand_files(NewFiles, Files) when is_list(NewFiles) ->
    lists:foldl(fun expand_files/2, Files, NewFiles).

print_error_info(Info) ->
    io:put_chars(standard_error, [erlfmt:format_error_info(Info), $\n]).

parallel(Fun, List) ->
    N = erlang:system_info(schedulers) * 2,
    parallel_loop(Fun, List, N, [], _ReducedResult = ok).

parallel_loop(_, [], _, [], ReducedResult) ->
    ReducedResult;
parallel_loop(Fun, [Elem | Rest], N, Refs, ReducedResult) when length(Refs) < N ->
    {_, Ref} = erlang:spawn_monitor(fun() -> exit(Fun(Elem)) end),
    parallel_loop(Fun, Rest, N, [Ref | Refs], ReducedResult);
parallel_loop(Fun, List, N, Refs0, ReducedResult0) ->
    receive
        {'DOWN', Ref, process, _, Result} when
            Result =:= error; Result =:= warn; Result =:= ok; Result =:= skip
        ->
            Refs = Refs0 -- [Ref],
            ReducedResult =
                case {Result, ReducedResult0} of
                    {_, error} -> error;
                    {error, _} -> error;
                    {_, warn} -> warn;
                    {warn, _} -> warn;
                    {_, _} -> ok
                end,
            parallel_loop(Fun, List, N, Refs, ReducedResult);
        {'DOWN', _Ref, process, _, Crash} ->
            exit(Crash)
    end.
