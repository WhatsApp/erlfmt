-module(erlfmt_escript_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0]).

%% Test cases
-export([hello/1, hello_sh/1]).

%% Test server callbacks
all() -> [hello, hello_sh].

%% Test cases
hello(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    File = "hello",
    {ok, []} = erlfmt:format_file(filename:join(DataDir, File), {ignore, {path, PrivDir}}),
    {ok, Expected} = file:read_file(filename:join(DataDir, File ++ ".formatted")),
    {ok, Expected} = file:read_file(filename:join(PrivDir, File)).

hello_sh(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    File = "hello_sh",
    Path = filename:join(DataDir, File),
    {ok, [Warning]} = erlfmt:format_file(Path, {ignore, {path, PrivDir}}),
    {Path, #{end_location := {1, 2}, location := {1, 1}},
        erlfmt_parse, ["syntax error before: ", "'#'"]} = Warning,
    {ok, Expected} = file:read_file(filename:join(DataDir, File ++ ".formatted")),
    {ok, Expected} = file:read_file(filename:join(PrivDir, File)).
