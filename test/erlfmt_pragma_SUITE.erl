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
-module(erlfmt_pragma_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    contains_pragma/1
]).

suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {pragma, [parallel], [
            contains_pragma
        ]}
    ].

all() ->
    [{group, pragma}].

contains_pragma(Config) when is_list(Config) ->
    ?assertEqual(true, contains_pragma_string("file.erl", 
    "% @format\n"
    "\n"
    "-module(pragma).\n"
    "\n"
    "-export([f/3]).\n"
    "\n"
    "f(_Arg1,_Arg2,   _Arg3) ->\n"
    "ok.\n")),
    ?assertEqual(true, contains_pragma_string("file.erl",
    "\n"
    "\n"
    "% @format\n"
    "\n"
    "-module(pragma).\n")),
    ?assertEqual(false, contains_pragma_string("file.erl",
    "-module(pragma).\n"
    "-export([f/3]).\n"
    "\n"
    "f(_Arg1,_Arg2,   _Arg3) ->\n"
    "ok.\n")),
    ?assertEqual(false, contains_pragma_string("file.erl", 
    "% LICENSE\n"
    "% LICENSE\n"
    "% LICENSE\n"
    "% LICENSE\n"
    "\n"
    "-module(pragma)\n."
    "\n"
    "-export([f/3]).\n"
    "\n"
    "f(_Arg1,_Arg2,   _Arg3) ->\n"
    "ok.\n")),
    ?assertEqual(true, contains_pragma_string("file.erl", 
    "% LICENSE\n"
    "% LICENSE\n"
    "% LICENSE\n"
    "% LICENSE\n"
    "\n"
    "% @format\n"
    "\n"
    "-module(pragma).\n"
    "\n"
    "-export([f/3]).\n"
    "\n"
    "f(_Arg1,_Arg2,   _Arg3) ->\n"
    "ok.\n")),
    ?assertEqual(true, contains_pragma_string("file.erl",
    "% @format\n"
    "-module(pragma).\n")).

-spec contains_pragma_string(file:name_all(), string()) -> ok.
contains_pragma_string(Filename, Content) ->
    {ok, Forms, _ } = erlfmt:read_forms_string(Filename, Content),
    erlfmt:contains_pragma_forms(Forms).
