%% Copyright (c) Meta Platforms, Inc. and affiliates.
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

-export([opts/0, do/2, do/3]).

-type out() ::
    standard_out | {path, file:name_all()} | {path_full, file:name_all()} | replace | check.

-record(config, {
    verbose = false :: boolean(),
    print_width = undefined :: undefined | pos_integer(),
    pragma = ignore :: erlfmt:pragma(),
    out = standard_out :: out(),
    range = undefined :: undefined | {pos_integer(), pos_integer()},
    %% Whether --cache was given: --check and --write then skip files known formatted.
    cache = false :: boolean(),
    %% --cache-location: where the cache is kept, instead of rebar3's global cache.
    cache_location = undefined :: undefined | file:filename(),
    %% The files and exclusions as given, before expansion: with the current
    %% directory, they name the run's cached entries.
    run_args = [] :: [{files | exclude_files, term()}]
}).

-type parsed() :: {format, list(), list(), #config{}} | help | version | {error, string()}.

%% What --cache remembers of a file: what formatting it found, with the
%% diagnostics it printed, so a later run reports it the same without
%% formatting it.
-type verdict() ::
    %% It needs no change.
    {formatted, Warnings :: [erlfmt:error_info()]}
    %% --require-pragma leaves it alone, so --verbose still says so.
    | skipped
    %% It needs formatting: --check reports it, --write must format it anyway.
    | {unformatted, Warnings :: [erlfmt:error_info()]}
    %% It can't be formatted.
    | {failed, erlfmt:error_info()}.

%% A run with --cache: its cache, and the key its verdicts are kept under.
-record(cached_run, {
    cache :: erlfmt_cache:cache(),
    %% Where the cache is kept.
    dir :: file:filename(),
    %% Everything a verdict depends on besides the file's path and content:
    %% erlfmt's code and version, the OTP release, and the options that
    %% change what is printed.
    key :: erlfmt_cache:digest()
}).

%% A file of a cached run, after its stat was looked up.
-type lookup() ::
    {done, ok | warn | error}
    | {stale, file:filename(), {erlfmt_cache:path(), erlfmt_cache:stat()} | uncacheable}.

%% The cache's on-disk format: another version's directory is never read.
-define(CACHE_FORMAT, "v1").
%% Run shapes kept, by most recent write.
-define(MAX_RUNS, 256).

opts() ->
    [
        {help, $h, "help", undefined, "print this message"},
        {version, $v, "version", undefined, "print version"},
        {write, $w, "write", undefined, "modify formatted files in place"},
        {out, $o, "out", binary, "output directory"},
        {out_full, $O, "out-full", binary, "output directory using full paths"},
        {verbose, undefined, "verbose", undefined, "include debug output"},
        {check, $c, "check", undefined,
            "Check if your files are formatted. "
            "Get exit code 1, if some files are not formatted. "
            "--write is not supported."},
        {print_width, undefined, "print-width", integer,
            "The line length that formatter would wrap on"},
        {require_pragma, undefined, "require-pragma", undefined,
            "Require a special comment @format, called a pragma, "
            "to be present in the file's first docblock comment in order for erlfmt to format it."},
        {insert_pragma, $i, "insert-pragma", undefined,
            "Insert a @format pragma to the top of formatted files when pragma is absent. "
            "Works well when used in tandem with --require-pragma, "
            "but it is not allowed to use require-pragma and insert-pragma at the same time."},
        {delete_pragma, undefined, "delete-pragma", undefined,
            "Deletes the @format pragma at the top of formatted files. "
            "It will also reformat the file, but is only applied to files with a pragma, see --require-pragma."},
        % The getopt module doesn't support tuple of integers as a type,
        % So we accept a string, and do an ad-hoc parsing manually.
        {range, undefined, "range", string,
            "Range to be formatted in start-end format (both inclusive). "
            "Warning! A bigger range might end up being formatted, as for now the format granularity is top-level form."},
        {exclude_files, undefined, "exclude-files", string,
            "files not to format. "
            "This overrides the files specified to format"},
        {cache, undefined, "cache", undefined,
            "With --check or --write, only read and format files that changed since the last run. "
            "The output is the same as without it."},
        {cache_location, undefined, "cache-location", string,
            "Where --cache keeps what it knows. "
            "Defaults to erlfmt/ in rebar3's global cache directory."},
        {files, undefined, undefined, string,
            "files to format, - for stdin. "
            "If no files are provided and one option of [-w, -o, -c] are provided then "
            "{src,include,test}/*.{hrl,erl,app.src} and rebar.config "
            "are specified as the default."}
    ].

-spec do(string(), list()) -> ok.
do(Name, Opts) ->
    do(Name, Opts, []).

-spec do(string(), list(), list()) -> ok.
do(Name, PreferOpts, DefaultOpts) ->
    PreferParsed = parse_opts(PreferOpts),
    DefaultParsed = parse_opts(DefaultOpts),
    SpecifiedFiles = specified_files(PreferOpts) ++ specified_files(DefaultOpts),
    Parsed =
        case {PreferParsed, DefaultParsed, SpecifiedFiles} of
            {
                {format, _, _, #config{out = standard_out}},
                {format, _, _, #config{out = standard_out}},
                _
            } ->
                %% Do not provide default files if we are writing to stdout
                resolve_parsed(PreferParsed, DefaultParsed);
            {{format, [], _, _}, {format, [], _, _}, _} when SpecifiedFiles =/= [] ->
                io:format(standard_error, "no files matching ~p~n", [SpecifiedFiles]),
                help;
            {_, _, []} ->
                %% no files means we should provide default files
                DefaultFiles = parse_opts([
                    {files, ["{src,include,test}/*.{hrl,erl,app.src}", "rebar.config"]}
                ]),
                resolve_parsed(PreferParsed, resolve_parsed(DefaultParsed, DefaultFiles));
            _ ->
                resolve_parsed(PreferParsed, DefaultParsed)
        end,
    with_parsed(Name, with_run_args(Parsed, run_args(PreferOpts ++ DefaultOpts))).

%% The files and exclusions given, as given: a later run with the same ones in
%% the same directory is of the same shape, and keeps this one's entries.
-spec run_args(list()) -> [{files | exclude_files, term()}].
run_args(Opts) ->
    [Opt || {Name, _} = Opt <- Opts, Name =:= files orelse Name =:= exclude_files].

-spec with_run_args(parsed(), [{files | exclude_files, term()}]) -> parsed().
with_run_args({format, Files, Excludes, Config}, RunArgs) ->
    {format, Files, Excludes, Config#config{run_args = RunArgs}};
with_run_args(Parsed, _RunArgs) ->
    Parsed.

-spec with_parsed(string(), parsed()) -> ok.
with_parsed(Name, Config) ->
    try
        unprotected_with_config(Name, Config)
    catch
        Kind:Reason:Stack ->
            io:format(standard_error, "~s Internal Error~n~s:~p~n~p~n", [Name, Kind, Reason, Stack]),
            erlang:halt(127)
    end.

-spec set_difference([file:name_all()], [file:name_all()]) -> [file:name_all()].
set_difference(Files, []) ->
    Files;
set_difference(Files, Excludes) ->
    {ok, Cwd} = file:get_cwd(),
    AbsoluteFiles = maps:from_list([{resolve_path(Cwd, F), F} || F <- Files]),
    AbsoluteExcludes = [resolve_path(Cwd, E) || E <- Excludes],
    maps:values(maps:without(AbsoluteExcludes, AbsoluteFiles)).

resolve_path(Dir, Filename) ->
    resolve_path2(filename:absname(Filename, Dir)).

resolve_path2(AbsPath) ->
    [Volume | Components] = filename:split(AbsPath),
    filename:join(resolve_path2(Components, [Volume])).

resolve_path2([".." | T1], [Volume]) -> resolve_path2(T1, [Volume]);
resolve_path2([<<"..">> | T1], [Volume]) -> resolve_path2(T1, [Volume]);
resolve_path2([".." | T1], [_H2 | T2]) -> resolve_path2(T1, T2);
resolve_path2([<<"..">> | T1], [_H2 | T2]) -> resolve_path2(T1, T2);
resolve_path2([H1 | T1], Components) -> resolve_path2(T1, [H1 | Components]);
resolve_path2([], Components) -> lists:reverse(Components).

%% needed because of getopt
-dialyzer({nowarn_function, [unprotected_with_config/2]}).

-spec unprotected_with_config(string(), parsed()) -> ok.
unprotected_with_config(Name, ParsedConfig) ->
    case ParsedConfig of
        {format, Files0, Excludes, Config} ->
            case set_difference(Files0, Excludes) of
                [] ->
                    io:put_chars(standard_error, ["no files provided to format\n\n"]),
                    getopt:usage(opts(), Name),
                    erlang:halt(2);
                Files ->
                    case Config#config.out of
                        check -> io:format(standard_error, "Checking formatting...~n", []);
                        _ -> ok
                    end,
                    case format_files(Files, Config) of
                        ok ->
                            case Config#config.out of
                                check ->
                                    io:format(
                                        standard_error,
                                        "All matched files use erlfmt code style!~n",
                                        []
                                    );
                                _ ->
                                    ok
                            end;
                        warn ->
                            io:format(
                                standard_error,
                                "[warn] Code style issues found in the above file(s). Forgot to run erlfmt?~n",
                                []
                            ),
                            erlang:halt(1);
                        error ->
                            erlang:halt(4)
                    end
            end;
        {error, Message} ->
            io:put_chars(standard_error, [Message, "\n\n"]),
            getopt:usage(opts(), Name),
            erlang:halt(2);
        help ->
            getopt:usage(opts(), Name),
            erlang:halt(0);
        version ->
            {ok, Vsn} = application:get_key(erlfmt, vsn),
            io:format("~s version ~s\n", [Name, Vsn]),
            erlang:halt(0)
    end.

-spec format_files([file:filename() | stdin], #config{}) -> ok | warn | error.
format_files(Files, Config) ->
    case open_cache(Config) of
        {ok, Run} -> format_files_cached(Files, Config, Run);
        uncached -> parallel(fun(File) -> format_file(File, Config) end, Files)
    end.

format_file(FileName, Config) ->
    announce(FileName, Config),
    report(FileName, format_result(FileName, Config), Config).

announce(FileName, #config{verbose = true}) ->
    io:format(standard_error, "Formatting ~s\n", [FileName]);
announce(_FileName, #config{verbose = false}) ->
    ok.

format_result(FileName, Config) ->
    format_result(FileName, Config, unread).

%% Formats `FileName' as `Config' says. `Original' is its contents, if they
%% were already read: --check compares the formatted text with them instead
%% of reading the file again.
format_result(FileName, Config, Original) ->
    #config{pragma = Pragma, print_width = PrintWidth, verbose = Verbose, out = Out, range = Range} =
        Config,
    Options =
        [{pragma, Pragma}] ++
            [{print_width, PrintWidth} || PrintWidth =/= undefined] ++
            [verbose || Verbose] ++
            [{range, Range} || Range =/= undefined],
    Result =
        case {Range, Out, FileName} of
            {undefined, check, stdin} ->
                check_stdin(Options);
            {undefined, check, _} ->
                check_file(FileName, Options, Original);
            {{_, _}, check, _} ->
                {error, "Checking of range not supported."};
            {{_, _}, replace, _} ->
                {error, "In place formatting of range not supported yet."};
            _ ->
                erlfmt:format_file(FileName, Options)
        end,
    Result.

report(FileName, Result, #config{out = Out} = Config) ->
    case Result of
        {skip, _} -> report_skipping(FileName, Config);
        _ -> ok
    end,
    case Result of
        {ok, FormattedText, Warnings} ->
            print_warnings(Warnings),
            write_formatted(FileName, FormattedText, Out);
        {warn, Warnings} ->
            report_unformatted(FileName, Warnings);
        {skip, RawString} ->
            write_formatted(FileName, RawString, Out);
        {error, Error} ->
            print_error_info(Error),
            error
    end.

print_warnings(Warnings) ->
    [print_error_info(Warning) || Warning <- Warnings],
    ok.

report_unformatted(FileName, Warnings) ->
    print_warnings(Warnings),
    io:format(standard_error, "[warn] ~s\n", [FileName]),
    warn.

report_skipping(FileName, #config{verbose = true}) ->
    io:format(standard_error, "Skipping ~s because of missing @format pragma\n", [FileName]);
report_skipping(_FileName, #config{verbose = false}) ->
    ok.

%%--------------------------------------------------------------------
%% --cache
%%
%% Invariants:
%% - A run with --cache prints, exits and writes what the same run without it
%%   would, up to the order files are reported in, which is the order
%%   parallel formatting finishes in either way.
%% - A file is reported without formatting it only under a key covering
%%   erlfmt's build and version, the OTP release, the options and the file's
%%   path and content (or its stat, outside the racy window).
%% - What a file's formatting printed is remembered with its verdict, under
%%   the same key, so a replayed diagnostic is the one formatting would print.
%%   An error opening a file is about the file system, not the content: it
%%   is never remembered. A verdict is only remembered if the file's stat
%%   after formatting is the one before reading it.
%% - The cache never changes an answer by failing: anything unreadable is a
%%   miss, anything unwritable is skipped.

%% Opens the cache for a run of `Config', if it is cached: with --cache, in
%% --check or --write mode, on whole files.
-spec open_cache(#config{}) -> {ok, #cached_run{}} | uncached.
open_cache(#config{cache = true, range = undefined, out = Out} = Config) when
    Out =:= check; Out =:= replace
->
    case erlfmt_cache:available() of
        true ->
            open_available_cache(Config);
        false ->
            io:format(
                standard_error, "[warn] crypto is unavailable: running without --cache~n", []
            ),
            uncached
    end;
open_cache(_Config) ->
    uncached.

-spec open_available_cache(#config{}) -> {ok, #cached_run{}} | uncached.
open_available_cache(Config) ->
    case {cache_dir(Config), build()} of
        {{ok, Dir}, {ok, Build}} ->
            Key = verdict_key(Build, Config),
            Cache = erlfmt_cache:open(Dir, run_name(Key, Config), #{}),
            {ok, #cached_run{cache = Cache, dir = Dir, key = Key}};
        _ ->
            uncached
    end.

%% Where the cache is kept: --cache-location, or erlfmt/ in rebar3's global
%% cache directory (REBAR_CACHE_DIR, or ~/.cache/rebar3), as `rebar3 fmt'
%% and the escript share it.
-spec cache_dir(#config{}) -> {ok, file:filename()} | error.
cache_dir(#config{cache_location = undefined}) ->
    case rebar_cache_dir() of
        {ok, RebarCache} -> {ok, filename:join([RebarCache, "erlfmt", ?CACHE_FORMAT])};
        error -> error
    end;
cache_dir(#config{cache_location = Location}) ->
    {ok, filename:join(filename:absname(Location), ?CACHE_FORMAT)}.

-spec rebar_cache_dir() -> {ok, file:filename()} | error.
rebar_cache_dir() ->
    case os:getenv("REBAR_CACHE_DIR") of
        false ->
            case init:get_argument(home) of
                {ok, [[Home]]} -> {ok, filename:join([Home, ".cache", "rebar3"])};
                _ -> error
            end;
        Dir ->
            {ok, Dir}
    end.

%% The running erlfmt: its version, and the code of its modules and of the
%% OTP scanner it wraps. Unknown if the erlfmt application can't be loaded
%% (rebar3 may not have loaded its plugin's): then the run is uncached.
-spec build() -> {ok, erlfmt_cache:digest()} | error.
build() ->
    _ = application:load(erlfmt),
    case {application:get_key(erlfmt, vsn), application:get_key(erlfmt, modules)} of
        {{ok, Vsn}, {ok, Modules}} ->
            Code = erlfmt_cache:build_id([erl_scan | Modules]),
            {ok, erlfmt_cache:digest_term({Vsn, erlang:system_info(otp_release), Code})};
        _ ->
            error
    end.

%% Everything a verdict depends on besides the file: the build, and the
%% options that change what formatting prints. The output mode doesn't: a
%% file's verdict is the same in --check and --write.
-spec verdict_key(erlfmt_cache:digest(), #config{}) -> erlfmt_cache:digest().
verdict_key(Build, #config{print_width = PrintWidth, pragma = Pragma, verbose = Verbose}) ->
    erlfmt_cache:digest_term({verdict, Build, PrintWidth, Pragma, Verbose}).

%% The file a run shape's entries are kept in: runs in the same directory,
%% with the same files, exclusions and key, share it.
-spec run_name(erlfmt_cache:digest(), #config{}) -> file:filename().
run_name(Key, #config{run_args = RunArgs}) ->
    {ok, Cwd} = file:get_cwd(),
    Shape = erlfmt_cache:digest_term({Cwd, Key, RunArgs}),
    filename:join("runs", binary_to_list(binary:encode_hex(Shape))).

%% Formats `Files' as format_file/2 does, but for those the cache knows the
%% verdict of. Every file is looked up by its stat first; the store, for
%% lookups by content, is only read if some file's stat changed.
-spec format_files_cached([file:filename()], #config{}, #cached_run{}) -> ok | warn | error.
format_files_cached(Files, Config, #cached_run{dir = Dir} = Run) ->
    Lookups = parallel_map(fun(File) -> lookup_by_stat(File, Config, Run) end, Files),
    Known = [Result || {done, Result} <- Lookups],
    Stale = [{File, Lookup} || {stale, File, Lookup} <- Lookups],
    {Checked, Cache} = check_stale(Stale, Config, Run),
    erlfmt_cache:save(Cache),
    erlfmt_cache:prune(Dir, "runs", ?MAX_RUNS),
    reduce_results(Checked ++ Known).

%% Checks the files whose stat changed, by content or by formatting them.
%% Returns their results, and the cache with the store loaded if it was needed.
-spec check_stale([{file:filename(), term()}], #config{}, #cached_run{}) ->
    {[ok | warn | error | skip], erlfmt_cache:cache()}.
check_stale([], _Config, #cached_run{cache = Cache}) ->
    {[], Cache};
check_stale(Stale, Config, #cached_run{cache = Cache} = Run0) ->
    Run = Run0#cached_run{cache = erlfmt_cache:load_store(Cache)},
    Checked = parallel_map(
        fun({File, Lookup}) -> check_by_content(File, Lookup, Config, Run) end, Stale
    ),
    {Checked, Run#cached_run.cache}.

%% Announces `File', and reports it if its stat says what its verdict is.
-spec lookup_by_stat(file:filename(), #config{}, #cached_run{}) -> lookup().
lookup_by_stat(File, Config, #cached_run{cache = Cache, key = Key}) ->
    announce(File, Config),
    case unicode:characters_to_binary(File) of
        Path when is_binary(Path) ->
            case erlfmt_cache:stat(File) of
                {ok, Stat} ->
                    Verdict = erlfmt_cache:get_by_stat(Cache, Path, Key, Stat),
                    case replayable(Verdict, Config) of
                        true -> {done, replay(File, Verdict, Config)};
                        false -> {stale, File, {Path, Stat}}
                    end;
                error ->
                    {stale, File, uncacheable}
            end;
        _NotUnicode ->
            {stale, File, uncacheable}
    end.

%% Reports `File', whose stat changed: from its content if the cache knows
%% it, or by formatting it, remembering what formatting found.
check_by_content(File, uncacheable, Config, _Run) ->
    report(File, format_result(File, Config), Config);
check_by_content(File, {Path, Stat}, Config, #cached_run{cache = Cache, key = Key}) ->
    case read_file(File) of
        {ok, Original} ->
            Content = erlfmt_cache:digest(Original),
            Known = erlfmt_cache:get_by_content(Cache, Path, Key, Stat, Content),
            case replayable(Known, Config) of
                true ->
                    replay(File, Known, Config);
                false ->
                    Result = format_result(File, Config, Original),
                    remember(File, verdict(Result, Original), {Path, Stat, Content}, Cache, Key),
                    report(File, Result, Config)
            end;
        {error, _} ->
            report(File, format_result(File, Config), Config)
    end.

%% Remembers `Verdict' for `File', read as `Content' with `Stat', unless the
%% file changed while it was formatted: then the verdict may be for other
%% contents than those hashed.
remember(_File, none, _Read, _Cache, _Key) ->
    ok;
remember(File, Verdict, {Path, Stat, Content}, Cache, Key) ->
    case erlfmt_cache:stat(File) of
        {ok, Stat} -> erlfmt_cache:put(Cache, Path, Key, Stat, Content, Verdict);
        _Changed -> ok
    end.

%% What formatting `Original' found, to remember. An error opening the file
%% is about the file system, not its contents: it isn't remembered.
-spec verdict(term(), binary()) -> verdict() | none.
verdict({ok, Formatted, Warnings}, Original) ->
    case same_bytes(Formatted, Original) of
        true -> {formatted, Warnings};
        false -> {unformatted, Warnings}
    end;
verdict({warn, Warnings}, _Original) ->
    {unformatted, Warnings};
verdict({skip, Unformatted}, Original) ->
    case same_bytes(Unformatted, Original) of
        true -> skipped;
        false -> none
    end;
verdict({error, {_File, _Location, Module, _Reason} = Error}, _Original) when Module =/= file ->
    {failed, Error};
verdict(_Result, _Original) ->
    none.

-spec same_bytes(unicode:chardata(), binary()) -> boolean().
same_bytes(Text, Original) when is_binary(Text) ->
    Text =:= Original;
same_bytes(Text, Original) ->
    unicode:characters_to_binary(Text) =:= Original.

%% Whether a lookup's verdict can be reported without formatting the file:
%% --write must format a file that needs it.
-spec replayable({ok, verdict()} | miss, #config{}) -> boolean().
replayable({ok, {unformatted, _Warnings}}, #config{out = replace}) -> false;
replayable({ok, _Verdict}, _Config) -> true;
replayable(miss, _Config) -> false.

%% Reports a file whose verdict is known, as formatting it would.
-spec replay(file:filename(), {ok, verdict()}, #config{}) -> ok | warn | error.
replay(_File, {ok, {formatted, Warnings}}, _Config) ->
    print_warnings(Warnings);
replay(File, {ok, skipped}, Config) ->
    report_skipping(File, Config);
replay(File, {ok, {unformatted, Warnings}}, #config{out = check}) ->
    report_unformatted(File, Warnings);
replay(_File, {ok, {failed, Error}}, _Config) ->
    print_error_info(Error),
    error.

write_formatted(_FileName, _Formatted, check) ->
    ok;
write_formatted(_FileName, Formatted, standard_out) ->
    io:put_chars(Formatted);
write_formatted(FileName, Formatted, Out) ->
    {ok, OriginalBin} = read_file(FileName),
    case unicode:characters_to_binary(Formatted) of
        OriginalBin ->
            ok;
        FormattedBin ->
            case out_file(FileName, Out) of
                error ->
                    error;
                OutFileName ->
                    case filelib:ensure_dir(OutFileName) of
                        ok ->
                            write_file(OutFileName, FormattedBin);
                        {error, Reason1} ->
                            print_error_info({OutFileName, 0, file, Reason1}),
                            error
                    end
            end
    end.

write_file(OutFileName, FormattedBin) ->
    case file:write_file(OutFileName, unicode:characters_to_binary(FormattedBin), [raw]) of
        ok ->
            ok;
        {error, Reason2} ->
            print_error_info({OutFileName, 0, file, Reason2}),
            error
    end.

out_file(FileName, replace) ->
    FileName;
out_file(FileName, {path, Path}) ->
    filename:join(Path, filename:basename(FileName));
out_file(FileName, {path_full, Path}) ->
    case filename:pathtype(FileName) of
        relative ->
            filename:join(Path, FileName);
        _ ->
            print_error_info(
                io_lib:format(
                    "only relative paths are supported for the out-full option, got: ~p", [Path]
                )
            ),
            error
    end.

check_file(FileName, Options, Original) ->
    case erlfmt:format_file(FileName, Options) of
        {ok, Formatted, FormatWarnings} ->
            OriginalBin = read_original(FileName, Original),
            FormattedBin = unicode:characters_to_binary(Formatted),
            case FormattedBin of
                OriginalBin -> {ok, FormattedBin, FormatWarnings};
                _ -> {warn, FormatWarnings}
            end;
        Other ->
            Other
    end.

read_original(FileName, unread) ->
    {ok, OriginalBin} = read_file(FileName),
    OriginalBin;
read_original(_FileName, OriginalBin) ->
    OriginalBin.

check_stdin(Options) ->
    CurrentOpts = io:getopts(standard_io),
    io:setopts(standard_io, [binary, {encoding, unicode}]),
    Result =
        case read_stdin([]) of
            {ok, OriginalBin} ->
                Original = unicode:characters_to_list(OriginalBin),
                case erlfmt:format_string(Original, Options) of
                    {ok, Formatted, FormatWarnings} ->
                        case Formatted of
                            Original -> {ok, Formatted, FormatWarnings};
                            _ -> {warn, FormatWarnings}
                        end;
                    Other ->
                        Other
                end;
            {error, Reason} ->
                {error, Reason}
        end,
    io:setopts(standard_io, CurrentOpts),
    Result.

-dialyzer({no_improper_lists, [read_stdin/1]}).

read_stdin(Data) ->
    case io:get_chars(standard_io, "", 4096) of
        MoreData when is_binary(MoreData) -> read_stdin([Data | MoreData]);
        eof -> {ok, Data};
        {error, Reason} -> {error, Reason}
    end.

-spec parse_opts(list()) -> parsed().
parse_opts(Args) ->
    parse_opts(Args, [], [], #config{}).

parse_opts([help | _Rest], _Files, _Exclude, _Config) ->
    help;
parse_opts([version | _Rest], _Files, _Exclude, _Config) ->
    version;
parse_opts([write | _Rest], _Files, _Exclude, #config{out = Out}) when Out =/= standard_out ->
    {error, "--write or replace mode can't be combined check mode"};
parse_opts([write | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{out = replace});
parse_opts([{out, _Path} | _Rest], _Files, _Exclude, #config{out = Out}) when
    Out =/= standard_out
->
    {error, "out or replace mode can't be combined check mode"};
parse_opts([{out, Path} | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{out = {path, Path}});
parse_opts([{out_full, _Path} | _Rest], _Files, _Exclude, #config{out = Out}) when
    Out =/= standard_out
->
    {error, "out-full mode can't be combined with other output modes"};
parse_opts([{out_full, Path} | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{out = {path_full, Path}});
parse_opts([verbose | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{verbose = true});
parse_opts([check | _Rest], _Files, _Exclude, #config{out = Out}) when Out =/= standard_out ->
    {error, "--check mode can't be combined write or replace mode"};
parse_opts([check | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{out = check});
parse_opts([cache | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{cache = true});
parse_opts([{cache_location, Location} | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{cache_location = Location});
parse_opts([{print_width, Value} | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{print_width = Value});
parse_opts([require_pragma | _Rest], _Files, _Exclude, #config{pragma = insert}) ->
    {error, "Cannot use both --insert-pragma and --require-pragma options together."};
parse_opts([require_pragma | _Rest], _Files, _Exclude, #config{pragma = delete}) ->
    {error, "Cannot use both --delete-pragma and --require-pragma options together."};
parse_opts([require_pragma | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{pragma = require});
parse_opts([insert_pragma | _Rest], _Files, _Exclude, #config{pragma = require}) ->
    {error, "Cannot use both --insert-pragma and --require-pragma options together."};
parse_opts([insert_pragma | _Rest], _Files, _Exclude, #config{pragma = delete}) ->
    {error, "Cannot use both --insert-pragma and --delete-pragma options together."};
parse_opts([insert_pragma | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{pragma = insert});
parse_opts([delete_pragma | _Rest], _Files, _Exclude, #config{pragma = insert}) ->
    {error, "Cannot use both --insert-pragma and --delete-pragma options together."};
parse_opts([delete_pragma | _Rest], _Files, _Exclude, #config{pragma = require}) ->
    {error, "Cannot use both --require-pragma and --delete-pragma options together."};
parse_opts([delete_pragma | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, Exclude, Config#config{pragma = delete});
parse_opts([{range, String} | Rest], Files, Exclude, Config) ->
    % Ad-hoc parsing. Mitigation for the absence of "couple of integers" direct support.
    Range1 = string:split(String, ",", all),
    Range2 = [list_to_integer(X) || X <- Range1],
    Range3 =
        case Range2 of
            [X] ->
                % Single line. Set end=start
                [X, X];
            [X, Y] ->
                [X, Y];
            _ ->
                {error, "Range: Expected 1 argument (single line) or 2 (start, end)."}
        end,
    case Range3 of
        [Start, End] when Start > End ->
            {error, "Range: End must be greater or equal than Start."};
        [Start, End] ->
            parse_opts(Rest, Files, Exclude, Config#config{range = {Start, End}});
        Error ->
            Error
    end;
parse_opts([{files, NewFiles} | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, expand_files(NewFiles, Files), Exclude, Config);
parse_opts([{exclude_files, NewExcludes} | Rest], Files, Exclude, Config) ->
    parse_opts(Rest, Files, expand_files(NewExcludes, Exclude), Config);
parse_opts([], [stdin], _Exclude, #config{out = Out}) when Out =/= standard_out, Out =/= check ->
    {error, "stdin mode can't be combined with write options"};
parse_opts([], [stdin], [], Config) ->
    {format, [stdin], [], Config};
parse_opts([], [stdin], [_Exclude | _], _Config) ->
    {error, "stdin mode can't be combined with excluded files"};
parse_opts([], Files, Exclude, Config) ->
    case lists:member(stdin, Files) of
        true -> {error, "stdin mode can't be combined with other files"};
        false -> {format, lists:reverse(Files), lists:reverse(Exclude), Config}
    end;
parse_opts([Unknown | _], _Files, _Exclude, _Config) ->
    {error, io_lib:format("unknown option: ~p", [Unknown])}.

-spec resolve_parsed(parsed(), parsed()) -> parsed().
resolve_parsed(PreferParsed, DefaultParsed) ->
    case {PreferParsed, DefaultParsed} of
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error;
        {help, _} ->
            help;
        {_, help} ->
            help;
        {version, _} ->
            version;
        {_, version} ->
            version;
        {
            {format, PreferFiles, PreferExclude, PreferConfig},
            {format, DefaultFiles, DefaultExclude, DefaultConfig}
        } ->
            {format, resolve_files(PreferFiles, DefaultFiles),
                resolve_files(PreferExclude, DefaultExclude),
                resolve_config(PreferConfig, DefaultConfig)}
    end.

resolve_files([], DefaultFiles) -> DefaultFiles;
resolve_files(PreferFiles, _DefaultFiles) -> PreferFiles.

resolve_config(
    #config{
        verbose = PreferVerbose,
        print_width = PreferWidth,
        pragma = PreferPragma,
        out = PreferOut,
        range = PreferRange,
        cache = PreferCache,
        cache_location = PreferLocation
    },
    #config{
        verbose = DefaultVerbose,
        print_width = DefaultWidth,
        pragma = DefaultPragma,
        out = DefaultOut,
        range = DefaultRange,
        cache = DefaultCache,
        cache_location = DefaultLocation
    }
) ->
    #config{
        verbose = PreferVerbose orelse DefaultVerbose,
        print_width = resolve_width(PreferWidth, DefaultWidth),
        pragma = resolve_pragma(PreferPragma, DefaultPragma),
        out = resolve_out(PreferOut, DefaultOut),
        range = resolve_range(PreferRange, DefaultRange),
        cache = PreferCache orelse DefaultCache,
        cache_location = resolve_cache_location(PreferLocation, DefaultLocation)
    }.

resolve_width(undefined, W) -> W;
resolve_width(W, _) -> W.

resolve_pragma(ignore, P) -> P;
resolve_pragma(P, _) -> P.

resolve_out(standard_out, O) -> O;
resolve_out(O, _) -> O.

resolve_range(undefined, R) -> R;
resolve_range(R, _) -> R.

resolve_cache_location(undefined, L) -> L;
resolve_cache_location(L, _) -> L.

specified_files(List) ->
    lists:filter(
        fun
            ({files, _}) -> true;
            (stdin) -> true;
            (_) -> false
        end,
        List
    ).

expand_files("-", Files) ->
    [stdin | Files];
expand_files(NewFile, Files) when is_integer(hd(NewFile)) ->
    case filelib:is_regular(NewFile, prim_file) of
        true ->
            [NewFile | Files];
        false ->
            case filelib:wildcard(NewFile, ".", prim_file) of
                [] ->
                    Files;
                NewFiles ->
                    NewFiles ++ Files
            end
    end;
expand_files(NewFiles, Files) when is_list(NewFiles) ->
    lists:foldl(fun expand_files/2, Files, NewFiles).

print_error_info(Info) when is_tuple(Info) ->
    io:put_chars(standard_error, [erlfmt:format_error_info(Info), $\n]);
print_error_info(Info) when is_list(Info) ->
    io:put_chars(standard_error, [Info, $\n]).

%% Runs `Fun' on every element of `List' in parallel, and reduces the results:
%% `error' if any errored, else `warn' if any warned, else `ok'.
parallel(Fun, List) ->
    reduce_results(parallel_map(Fun, List)).

-spec reduce_results([ok | warn | error | skip]) -> ok | warn | error.
reduce_results(Results) ->
    lists:foldl(fun reduce_result/2, ok, Results).

reduce_result(_, error) -> error;
reduce_result(error, _) -> error;
reduce_result(_, warn) -> warn;
reduce_result(warn, _) -> warn;
reduce_result(_, _) -> ok.

%% Returns `Fun' of every element of `List', in the order of `List', running
%% at most twice as many at once as there are schedulers. A crash in `Fun'
%% exits the caller.
-spec parallel_map(fun((A) -> B), [A]) -> [B].
parallel_map(Fun, List) ->
    N = erlang:system_info(schedulers) * 2,
    Indexed = lists:zip(lists:seq(1, length(List)), List),
    Results = parallel_map_loop(Fun, Indexed, N, #{}, #{}),
    [maps:get(Index, Results) || {Index, _Elem} <- Indexed].

parallel_map_loop(_Fun, [], _N, Running, Results) when map_size(Running) =:= 0 ->
    Results;
parallel_map_loop(Fun, [{Index, Elem} | Rest], N, Running, Results) when map_size(Running) < N ->
    {_, Ref} = erlang:spawn_monitor(fun() -> exit({?MODULE, result, Fun(Elem)}) end),
    parallel_map_loop(Fun, Rest, N, Running#{Ref => Index}, Results);
parallel_map_loop(Fun, List, N, Running, Results) ->
    receive
        {'DOWN', Ref, process, _, {?MODULE, result, Result}} when is_map_key(Ref, Running) ->
            {Index, StillRunning} = maps:take(Ref, Running),
            parallel_map_loop(Fun, List, N, StillRunning, Results#{Index => Result});
        {'DOWN', Ref, process, _, Crash} when is_map_key(Ref, Running) ->
            exit(Crash)
    end.

-if(?OTP_RELEASE >= 27).
read_file(FileName) -> file:read_file(FileName, [raw]).
-else.
read_file(FileName) -> file:read_file(FileName).
-endif.
