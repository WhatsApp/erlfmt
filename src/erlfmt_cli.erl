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
    try do_unprotected(Opts, Name)
    catch
        Kind:Reason:Stack ->
            io:format(standard_error, "~s Internal Error~n~s:~p~n~p~n", [Name, Kind, Reason, Stack]),
            erlang:halt(127)
    end.

-spec do_unprotected(list(), string()) -> ok.
do_unprotected(Opts, Name) ->
    case parse_opts(Opts, Name, [], #config{}) of
        {format, Files, Config} ->
            case parallel(fun (File) -> format_file(File, Config) end, Files) of
                true -> erlang:halt(4);
                false -> ok
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
            false;
        skip when Verbose ->
            io:format(standard_error, "Skipping ~s because of missing @format pragma\n", [FileName]),
            false;
        skip ->
            false;
        {error, Error} ->
            print_error_info(Error),
            true
    end.

parse_opts([help | _Rest], Name, _Files, _Config) ->
    getopt:usage(opts(), Name),
    erlang:halt(0);
parse_opts([version | _Rest], Name, _Files, _Config) ->
    {ok, Vsn} = application:get_key(erlfmt, vsn),
    io:format("~s version ~s\n", [Name, Vsn]),
    erlang:halt(0);
parse_opts([write | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{out = replace});
parse_opts([{out, Path} | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{out = {path, Path}});
parse_opts([verbose | Rest], Name, Files, Config) ->
    parse_opts(Rest, Name, Files, Config#config{verbose = true});
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
    parallel_loop(Fun, List, N, [], _HadErrors = false).

parallel_loop(_, [], _, [], HadErrors) ->
    HadErrors;
parallel_loop(Fun, [Elem | Rest], N, Refs, HadErrors) when length(Refs) < N ->
    {_, Ref} = erlang:spawn_monitor(fun() -> exit(Fun(Elem)) end),
    parallel_loop(Fun, Rest, N, [Ref | Refs], HadErrors);
parallel_loop(Fun, List, N, Refs0, HadErrors) ->
    receive
        {'DOWN', Ref, process, _, Bool} when is_boolean(Bool) ->
            Refs = Refs0 -- [Ref],
            parallel_loop(Fun, List, N, Refs, HadErrors orelse Bool);
        {'DOWN', _Ref, process, _, Crash} ->
            exit(Crash)
    end.
