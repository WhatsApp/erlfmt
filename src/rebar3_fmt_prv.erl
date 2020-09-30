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
-module(rebar3_fmt_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, fmt).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 fmt --write"},
            {opts, erlfmt_cli:opts()},
            {short_desc, "Erlang code formatter"},
            {desc, "Erlang code formatter"}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    ConfigOpts = rebar_state:get(State, erlfmt, []),
    case rebar_state:command_parsed_args(State) of
        {ArgOpts, []} ->
            erlfmt_cli:do("rebar3 fmt", ArgOpts, ConfigOpts);
        {ArgOpts, ExtraFiles} ->
            erlfmt_cli:do("rebar3 fmt", ArgOpts, ConfigOpts ++ [{files, ExtraFiles}])
    end,
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
