%%%-------------------------------------------------------------------
%%% @author Michał Muskała <micmus@whatsapp.com>
%%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%%% @doc
%%%     Tests erlfmt
%%% @end
%%% @see http://erlang.org/doc/man/common_test.html
%%% -------------------------------------------------------------------

-module(erlfmt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-oncall("whatsapp_erlang_team").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    group/1,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).

%% Test cases
-export([
    records/1,
    attributes/1,
    specs/1,
    macro_call_exprs/1,
    macro_call_pats/1,
    macro_call_types/1,
    macro_definitions/1,
    functions_and_funs/1,
    operators/1,
    lists/1,
    binaries/1,
    clauses/1,
    types/1,
    annos/1,
    smoke_test_cli/1,
    smoke_test_parser_ac2d/1,
    smoke_test_parser_admdb/1,
    smoke_test_parser_asyncwd/1,
    smoke_test_parser_autotest/1,
    smoke_test_parser_bcastd/1,
    smoke_test_parser_bcld/1,
    smoke_test_parser_bizd/1,
    smoke_test_parser_bizdb/1,
    smoke_test_parser_bl2d/1,
    smoke_test_parser_calld/1,
    smoke_test_parser_camd/1,
    smoke_test_parser_cfgd/1,
    smoke_test_parser_chatd/1,
    smoke_test_parser_clust/1,
    smoke_test_parser_ctsd/1,
    smoke_test_parser_deld/1,
    smoke_test_parser_deviced/1,
    smoke_test_parser_dispatcher/1,
    smoke_test_parser_elgd/1,
    smoke_test_parser_erl2/1,
    smoke_test_parser_erlbuild/1,
    smoke_test_parser_erlclient/1,
    smoke_test_parser_erlfmt/1,
    smoke_test_parser_fbid/1,
    smoke_test_parser_fbmd/1,
    smoke_test_parser_fgtd/1,
    smoke_test_parser_filed/1,
    smoke_test_parser_forgets/1,
    smoke_test_parser_gcalld/1,
    smoke_test_parser_grpd/1,
    smoke_test_parser_ipsd/1,
    smoke_test_parser_llcd/1,
    smoke_test_parser_loader/1,
    smoke_test_parser_loadtestd/1,
    smoke_test_parser_locd/1,
    smoke_test_parser_mmsd/1,
    smoke_test_parser_mnfd/1,
    smoke_test_parser_monitor/1,
    smoke_test_parser_msgd/1,
    smoke_test_parser_ntsd/1,
    smoke_test_parser_offd/1,
    smoke_test_parser_orphand/1,
    smoke_test_parser_payd/1,
    smoke_test_parser_pgwd/1,
    smoke_test_parser_pmtd/1,
    smoke_test_parser_pmtdb/1,
    smoke_test_parser_presd/1,
    smoke_test_parser_ps2d/1,
    smoke_test_parser_pshd/1,
    smoke_test_parser_ptokend/1,
    smoke_test_parser_queryd/1,
    smoke_test_parser_rebar3_plugins/1,
    smoke_test_parser_reg2d/1,
    smoke_test_parser_regd/1,
    smoke_test_parser_regfe/1,
    smoke_test_parser_reld/1,
    smoke_test_parser_riddb/1,
    smoke_test_parser_rptd/1,
    smoke_test_parser_schd/1,
    smoke_test_parser_sdld/1,
    smoke_test_parser_sgwd/1,
    smoke_test_parser_shardtestdb/1,
    smoke_test_parser_sigd/1,
    smoke_test_parser_smc_observer/1,
    smoke_test_parser_ssnd/1,
    smoke_test_parser_stsd/1,
    smoke_test_parser_syncd/1,
    smoke_test_parser_systemd/1,
    smoke_test_parser_tapd/1,
    smoke_test_parser_tftd/1,
    smoke_test_parser_thrift/1,
    smoke_test_parser_thriftrpc/1,
    smoke_test_parser_trcd/1,
    smoke_test_parser_ugwd/1,
    smoke_test_parser_usrd/1,
    smoke_test_parser_util/1,
    smoke_test_parser_vestad/1,
    smoke_test_parser_void/1,
    smoke_test_parser_void_client/1,
    smoke_test_parser_wa/1,
    smoke_test_parser_wa_admintool/1,
    smoke_test_parser_wa_asyncd_lib/1,
    smoke_test_parser_wa_auth/1,
    smoke_test_parser_wa_biz/1,
    smoke_test_parser_wa_chat/1,
    smoke_test_parser_wa_crypto/1,
    smoke_test_parser_wa_data/1,
    smoke_test_parser_wa_db/1,
    smoke_test_parser_wa_experimental/1,
    smoke_test_parser_wa_integrity/1,
    smoke_test_parser_wa_listd/1,
    smoke_test_parser_wa_nodeapi/1,
    smoke_test_parser_wa_proto/1,
    smoke_test_parser_wa_raft/1,
    smoke_test_parser_wa_reg/1,
    smoke_test_parser_wa_service/1,
    smoke_test_parser_wa_web/1,
    smoke_test_parser_wamd/1,
    smoke_test_parser_wamd_api/1,
    smoke_test_parser_wamid/1,
    smoke_test_parser_webd/1,
    smoke_test_parser_zcrawld/1,
    snapshot_simple_comments/1,
    snapshot_comments/1,
    snapshot_broken/1
]).

suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(smoke_test_parser, Config) ->
    case os:getenv("PURPOSE") of
        "diff" -> {skip, "skipping on diffs - see D18138121"};
        _ -> Config
    end;
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
        {parser, [parallel], [
            records,
            attributes,
            specs,
            macro_call_exprs,
            macro_call_pats,
            macro_call_types,
            macro_definitions,
            functions_and_funs,
            operators,
            lists,
            binaries,
            clauses,
            types,
            annos
        ]},
        {smoke_tests, [parallel], [
            {group, smoke_test_parser},
            {group, snapshot_tests},
            smoke_test_cli
        ]},
        {smoke_test_parser, [parallel], [
            smoke_test_parser_wa, %% first since it's the slowest
            smoke_test_parser_ac2d,
            smoke_test_parser_admdb,
            smoke_test_parser_asyncwd,
            smoke_test_parser_autotest,
            smoke_test_parser_bcastd,
            smoke_test_parser_bcld,
            smoke_test_parser_bizd,
            smoke_test_parser_bizdb,
            smoke_test_parser_bl2d,
            smoke_test_parser_calld,
            smoke_test_parser_camd,
            smoke_test_parser_cfgd,
            smoke_test_parser_chatd,
            smoke_test_parser_clust,
            smoke_test_parser_ctsd,
            smoke_test_parser_deld,
            smoke_test_parser_deviced,
            smoke_test_parser_dispatcher,
            smoke_test_parser_elgd,
            smoke_test_parser_erl2,
            smoke_test_parser_erlbuild,
            smoke_test_parser_erlclient,
            smoke_test_parser_erlfmt,
            smoke_test_parser_fbid,
            smoke_test_parser_fbmd,
            smoke_test_parser_fgtd,
            smoke_test_parser_filed,
            smoke_test_parser_forgets,
            smoke_test_parser_gcalld,
            smoke_test_parser_grpd,
            smoke_test_parser_ipsd,
            smoke_test_parser_llcd,
            smoke_test_parser_loader,
            smoke_test_parser_loadtestd,
            smoke_test_parser_locd,
            smoke_test_parser_mmsd,
            smoke_test_parser_mnfd,
            smoke_test_parser_monitor,
            smoke_test_parser_msgd,
            smoke_test_parser_ntsd,
            smoke_test_parser_offd,
            smoke_test_parser_orphand,
            smoke_test_parser_payd,
            smoke_test_parser_pgwd,
            smoke_test_parser_pmtd,
            smoke_test_parser_pmtdb,
            smoke_test_parser_presd,
            smoke_test_parser_ps2d,
            smoke_test_parser_pshd,
            smoke_test_parser_ptokend,
            smoke_test_parser_queryd,
            smoke_test_parser_rebar3_plugins,
            smoke_test_parser_reg2d,
            smoke_test_parser_regd,
            smoke_test_parser_regfe,
            smoke_test_parser_reld,
            smoke_test_parser_riddb,
            smoke_test_parser_rptd,
            smoke_test_parser_schd,
            smoke_test_parser_sdld,
            smoke_test_parser_sgwd,
            smoke_test_parser_shardtestdb,
            smoke_test_parser_sigd,
            smoke_test_parser_smc_observer,
            smoke_test_parser_ssnd,
            smoke_test_parser_stsd,
            smoke_test_parser_syncd,
            smoke_test_parser_systemd,
            smoke_test_parser_tapd,
            smoke_test_parser_tftd,
            smoke_test_parser_thrift,
            smoke_test_parser_thriftrpc,
            smoke_test_parser_trcd,
            smoke_test_parser_ugwd,
            smoke_test_parser_usrd,
            smoke_test_parser_util,
            smoke_test_parser_vestad,
            smoke_test_parser_void,
            smoke_test_parser_void_client,
            smoke_test_parser_wa_admintool,
            smoke_test_parser_wa_asyncd_lib,
            smoke_test_parser_wa_auth,
            smoke_test_parser_wa_biz,
            smoke_test_parser_wa_chat,
            smoke_test_parser_wa_crypto,
            smoke_test_parser_wa_data,
            smoke_test_parser_wa_db,
            smoke_test_parser_wa_experimental,
            smoke_test_parser_wa_integrity,
            smoke_test_parser_wa_listd,
            smoke_test_parser_wa_nodeapi,
            smoke_test_parser_wa_proto,
            smoke_test_parser_wa_raft,
            smoke_test_parser_wa_reg,
            smoke_test_parser_wa_service,
            smoke_test_parser_wa_web,
            smoke_test_parser_wamd,
            smoke_test_parser_wamd_api,
            smoke_test_parser_wamid,
            smoke_test_parser_webd,
            smoke_test_parser_zcrawld
        ]},
        {snapshot_tests, [parallel], [
            snapshot_simple_comments,
            snapshot_comments,
            snapshot_broken
        ]}
    ].

group(smoke_test_parser) -> [{timetrap, {minutes, 1}}];
group(_) -> [].

all() ->
    [{group, smoke_tests}, {group, parser}].

%%--------------------------------------------------------------------
%% TEST CASES

records(Config) when is_list(Config) ->
    ?assertMatch(
        {record, _, {atom, _, foo}, []},
        parse_expr("#foo{}")
    ),
    ?assertMatch(
        {record_index, _, {atom, _, foo}, {atom, _, bar}},
        parse_expr("#foo.bar")
    ),
    ?assertMatch(
        {record_field, _, {var, _, 'X'}, {atom, _, foo}, {atom, _, bar}},
        parse_expr("X#foo.bar")
    ),
    ?assertMatch(
        {record, _, {var, _, 'X'}, {atom, _, foo}, []},
        parse_expr("X#foo{}")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, record}, [{atom, _, foo}, {tuple, _, [
            {op, _, '::', {record_field, _, {atom, _, a}, {integer, _, 1}}, {call, _, {atom, _, integer}, []}},
            {op, _, '::', {record_field, _, {atom, _, b}}, {call, _, {atom, _, float}, []}},
            {record_field, _, {atom, _, c}, {integer, _, 2}},
            {record_field, _, {atom, _, d}}
        ]}]},
        parse_form("-record(foo, {a = 1 :: integer(), b :: float(), c  = 2, d}).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, type}, [{op, _, '::',
            {call, _, {atom, _, foo}, []},
            {record, _, {atom, _, foo}, []}
        }]},
        parse_form("-type foo() :: #foo{}.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, opaque}, [{op, _, '::',
            {call, _, {atom, _, foo}, []},
            {record, _, {atom, _, foo}, []}
        }]},
        parse_form("-opaque foo() :: #foo{}.")
    ).

attributes(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, {atom, _, foo}, [{atom, _, bar}]},
        parse_form("-foo(bar).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, ifdef}, [{atom, _, foo}]},
        parse_form("-ifdef(foo).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, 'if'}, [{op, _, '==', {macro_call, _, {atom, _, foo}, none}, {integer, _, 2}}]},
        parse_form("-if(?foo == 2).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, else}, []},
        parse_form("-else.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, endif}, []},
        parse_form("-endif.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, endif}, [{macro_call, _, {var, _, 'BAR'}, none}]},
        parse_form("-endif(?BAR).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, export}, [{list, _, [
            {op, _, '/', {macro_call, _, {var, _, 'FOO'}, none}, {integer, _, 1}},
            {op, _, '/', {atom, _, foo}, {integer, _, 2}}
        ]}]},
        parse_form("-export([?FOO/1, foo/2]).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, import}, [
            {atom, _, foo},
            {list, _, [{op, _, '/', {atom, _, bar}, {integer, _, 1}}]}
        ]},
        parse_form("-import(foo, [bar/1]).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, something_else}, [
            {tuple, _, [{atom, _, foo}, {op, _, '/', {atom, _, bar}, {integer, _, 1}}]}
        ]},
        parse_form("-something_else({foo, bar/1}).")
    ).

specs(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, {atom, _, spec}, [{spec, _,
            {remote, _, {atom, _, foo}, {atom, _, bar}},
            [{clause, _, spec, [], empty, [{atom, _, ok}]}]
        }]},
        parse_form("-spec foo:bar() -> ok.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, spec}, [{spec, _, {atom, _, foo}, [
            {clause, _, spec, [{call, _, {atom, _, integer}, []}], empty, [{atom, _,integer}]},
            {clause, _, spec, [{call, _, {atom, _, atom}, []}], empty, [{atom, _, atom}]}
        ]}]},
        parse_form("-spec foo(integer()) -> integer; (atom()) -> atom.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, callback}, [{spec, _, {atom, _, foo}, [
            {clause, _, spec, [{var, _, 'X'}], {guard_or, _, [{guard_and, _, [
                    {op, _, '::', {var, _, 'X'}, {call, _, {atom, _, integer}, []}},
                    {op, _, '::', {var, _, 'Y'}, {call, _, {atom, _, atom}, []}}
                ]}]},
                [{var, _, 'Y'}]}
        ]}]},
        parse_form("-callback foo(X) -> Y when X :: integer(), Y :: atom().")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, spec}, [{spec, _,
            {macro_call, _, {atom, _, foo}, none},
            [{clause, _, spec, [], empty, [{atom, _, ok}]}]
        }]},
        parse_form("-spec ?foo() -> ok.")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, callback}, [{spec, _,
            {macro_call, _, {var, _, 'FOO'}, none},
            [{clause, _, spec, [], empty, [{atom, _, ok}]}]
        }]},
        parse_form("-callback ?FOO() -> ok.")
    ).


macro_call_exprs(Config) when is_list(Config) ->
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, none},
        parse_expr("?FOO")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, []},
        parse_expr("?FOO()")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, [{integer, _, 1}]},
        parse_expr("?FOO(1)")
    ),
    ?assertMatch(
        {macro_string, _, {var, _, 'FOO'}},
        parse_expr("??FOO")
    ),
    ?assertMatch(
        {macro_call, _, {atom, _, foo}, [{op, _, 'when', {atom, _, x}, {atom, _, true}}]},
        parse_expr("?foo(x when true)")
    ),
    ?assertMatch(
        {concat, _, [{macro_call, _, {atom, _, foo}, none}, {string, _, "suffix"}]},
        parse_expr("?foo \"suffix\"")
    ),
    ?assertMatch(
        {concat, _, [{macro_string, _, {atom, _, foo}}, {string, _, "suffix"}]},
        parse_expr("??foo \"suffix\"")
    ),
    ?assertMatch(
        {concat, _, [{var, _, 'Var'}, {string, _, "suffix"}]},
        parse_expr("Var \"suffix\"")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {string, _, "suffix"}]},
        parse_expr("\"prefix\" \"suffix\"")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {macro_call, _, {atom, _, foo}, none}]},
        parse_expr("\"prefix\" ?foo")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {macro_string, _, {atom, _, foo}}]},
        parse_expr("\"prefix\" ??foo")
    ),
    ?assertMatch(
        {concat, _, [{string, _, "prefix"}, {var, _, 'Var'}]},
        parse_expr("\"prefix\" Var")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_expr("?foo{}")
    ),
    ?assertMatch(
        {record_index, _, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_expr("?foo.bar")
    ),
    ?assertMatch(
        {record, _, {var, _, 'S'}, {macro_call, _, {atom, _, foo}, none}, []},
        parse_expr("S?foo{}")
    ),
    ?assertMatch(
        {record_field, _, {var, _, 'S'}, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_expr("S?foo.bar")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_expr("#?foo{}")
    ),
    ?assertMatch(
        {record_index, _, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_expr("#?foo.bar")
    ),
    ?assertMatch(
        {record, _, {var, _, 'S'}, {macro_call, _, {atom, _, foo}, none}, []},
        parse_expr("S#?foo{}")
    ),
    ?assertMatch(
        {record_field, _, {var, _, 'S'}, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_expr("S#?foo.bar")
    ),
    ?assertMatch(
        {record_index, _, {atom, _, foo}, {var, _, 'Bar'}},
        parse_expr("#foo.Bar")
    ),
    ?assertMatch(
        {record_field, _, {var, _, 'S'}, {atom, _, foo}, {var, _, 'Bar'}},
        parse_expr("S#foo.Bar")
    ).

macro_call_pats(Config) when is_list(Config) ->
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, none},
        parse_pat("?FOO")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, []},
        parse_pat("?FOO()")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, [{integer, _, 1}]},
        parse_pat("?FOO(1)")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_pat("?foo{}")
    ),
    ?assertMatch(
        {record_index, _, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_pat("?foo.bar")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_pat("#?foo{}")
    ),
    ?assertMatch(
        {record_index, _, {macro_call, _, {atom, _, foo}, none}, {atom, _, bar}},
        parse_pat("#?foo.bar")
    ),
    ?assertMatch(
        {record_index, _, {atom, _, foo}, {var, _, 'Bar'}},
        parse_pat("#foo.Bar")
    ).

macro_call_types(Config) when is_list(Config) ->
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, none},
        parse_type("?FOO")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, []},
        parse_type("?FOO()")
    ),
    ?assertMatch(
        {macro_call, _, {var, _, 'FOO'}, [{op, _, '|', {integer, _, 1}, {integer, _, 2}}]},
        parse_type("?FOO(1 | 2)")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_type("?foo{}")
    ),
    ?assertMatch(
        {record, _, {macro_call, _, {atom, _, foo}, none}, []},
        parse_type("#?foo{}")
    ).

macro_definitions(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{var, _, 'FOO'}, {atom, _, foo}]},
        parse_form("-define(FOO, foo).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{call, _, {var, _, 'FOO'}, []}, {atom, _, foo}]},
        parse_form("-define(FOO(), foo).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{call, _, {var, _, 'FOO'}, [{var, _, 'X'}]}, {atom, _, foo}]},
        parse_form("-define(FOO(X), foo).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {call, _, {atom, _, is_nice}, [{var, _, 'X'}]},
            {guard_or, _, [{guard_and, _, [
                {call, _, {atom, _, is_tuple}, [{var, _, 'X'}]},
                {op, _, '=:=', {call, _, {atom, _,element}, [{integer, _, 1},{var, _, 'X'}]}, {atom, _, nice}}
            ]}]}
        ]},
        parse_form("-define(is_nice(X), is_tuple(X), element(1, X) =:= nice).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{atom, _, foo}, {record_name, _, {atom, _,bar}}]},
        parse_form("-define(foo, #bar).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{atom, _, foo}, empty]},
        parse_form("-define(foo,).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {call, _, {atom, _, pass}, [{var, _, 'Name'}]},
            {'fun', _, {function, {var, _, 'Name'}, {integer, _, 2}}}
        ]},
        parse_form("-define(pass(Name), fun Name/2).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{atom, _, foo}, {clause, _, {atom, _, foo}, [], empty, [{atom, _, ok}]}]},
        parse_form("-define(foo, foo() -> ok).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [
            {call, _, {var, _, 'FOO'}, [{var, _, 'Name'}]},
            {clause, _, {var, _, 'Name'}, [], empty, [{atom, _, ok}]}
        ]},
        parse_form("-define(FOO(Name), Name() -> ok).")
    ),
    ?assertMatch(
        {attribute, _, {atom, _, define}, [{var, _, 'HASH_FUN'}, {remote, _, {atom, _, erlang}, {atom, _, phash}}]},
        parse_form("-define(HASH_FUN, erlang:phash).")
    ).

functions_and_funs(Config) when is_list(Config) ->
    ?assertMatch(
        {'fun', _, {function, {atom, _, foo}, {integer, _, 1}}},
        parse_expr("fun foo/1")
    ),
    ?assertMatch(
        {'fun', _, {function, {macro_call, _, {atom, _, foo}, none}, {integer, _, 1}}},
        parse_expr("fun ?foo/1")
    ),
    ?assertMatch(
        {'fun', _, {function, {atom, _, foo}, {macro_call, _, {atom, _, foo}, none}}},
        parse_expr("fun foo/?foo")
    ),
    ?assertMatch(
        {'fun', _, {function, {var, _, 'Mod'}, {atom, _, foo}, {integer, _, 1}}},
        parse_expr("fun Mod:foo/1")
    ),
    ?assertMatch(
        {'fun', _, {function, {macro_call, _, {atom, _, 'foo'}, none}, {atom, _, foo}, {integer, _, 1}}},
        parse_expr("fun ?foo:foo/1")
    ),
    ?assertMatch(
        {'fun', _, {function, {atom, _, foo}, {macro_call, _, {atom, _, foo}, none}, {integer, _, 1}}},
        parse_expr("fun foo:?foo/1")
    ),
    ?assertMatch(
        {'fun', _, {function, {atom, _, foo}, {atom, _, foo}, {macro_call, _, {atom, _, foo}, none}}},
        parse_expr("fun foo:foo/?foo")
    ),
    ?assertMatch(
        {'fun', _, {clauses, [{clause, _, 'fun', [], empty, [{atom, _, ok}]}]}},
        parse_expr("fun () -> ok end")
    ),
    ?assertMatch(
        {'fun', _, {clauses, [{clause, _, {var, _, 'Foo'}, [], empty, [{atom, _, ok}]}]}},
        parse_expr("fun Foo() -> ok end")
    ),
    ?assertMatch(
        {function, _, [{clause, _, {atom, _, foo}, [], empty, [{atom, _, ok}]}]},
        parse_form("foo() -> ok.")
    ),
    ?assertMatch(
        {function, _, [{clause, _, {macro_call, _, {var, _, 'FOO'}, none}, [], empty, [{atom, _, ok}]}]},
        parse_form("?FOO() -> ok.")
    ),
    ?assertMatch(
        {function, _, [
            {clause, _, {atom, _, foo}, [], empty, [{atom, _, ok}]},
            {macro_call, _, {atom, _, bar}, []}
        ]},
        parse_form("foo() -> ok; ?bar().")
    ),
    ?assertMatch(
        {function, _, [{macro_call, _, {var, _,'TESTS_WITH_SETUP'}, [
            {atom, _, all_tests_},
            {'fun', _, {clauses, [{clause, _, 'fun', [], empty, [{atom, _, ok}]}]}}
        ]}]},
        parse_form("?TESTS_WITH_SETUP(all_tests_, fun() -> ok end).")
    ).

operators(Config) when is_list(Config) ->
    ?assertMatch(
        {op, _, '=', {integer, _, 1}, {integer, _, 2}},
        parse_expr("1 = 2")
    ),
    ?assertMatch(
        {op, _, '=', {integer, _, 1}, {integer, _, 2}},
        parse_pat("1 = 2")
    ),
    ?assertMatch(
        {op, _, 'catch', {integer, _, 1}},
        parse_expr("catch 1")
    ).

lists(Config) when is_list(Config) ->
    ?assertMatch(
        {list, _, []},
        parse_expr("[]")
    ),
    ?assertMatch(
        {list, _, [{integer, _, 1}, {integer, _, 2}]},
        parse_expr("[1,2]")
    ),
    ?assertMatch(
        {list, _, [{cons, _, {integer, _, 1}, {integer, _, 2}}]},
        parse_expr("[1 | 2]")
    ),
    ?assertMatch(
        {list, _, [{integer, _, 1}, {cons, _, {integer, _, 2}, {integer, _, 3}}]},
        parse_expr("[1, 2 | 3]")
    ),
    ?assertMatch(
        {list, _, [{cons, _,
            {op, _, 'catch', {integer, _, 1}},
            {op, _, '!', {integer, _, 2}, {integer, _,3}}
        }]},
        parse_expr("[catch 1 | 2 ! 3]")
    ).

binaries(Config) when is_list(Config) ->
    ?assertMatch(
        {bin, _, [{bin_element, _, {integer, _, 1}, {integer, _, 4}, default}]},
        parse_expr("<<1:4>>")
    ),
    ?assertMatch(
        {bin, _, [{bin_element, _, {integer, _, 1}, default, [{remote, _, {atom, _, unit}, {integer, _, 8}}]}]},
        parse_expr("<<1/unit:8>>")
    ).

clauses(Config) when is_list(Config) ->
    ?assertMatch(
        {'if', _, [{clause, _, 'if', [], {guard_or, _, [{guard_and, _, [{atom, _, true}]}]}, [{atom, _, ok}]}]},
        parse_expr("if true -> ok end")
    ),
    ?assertMatch(
        {'case', _, {var, _, 'X'}, [
            {clause, _, 'case', [{atom, _, true}], empty, [{atom, _, ok}]}
        ]},
        parse_expr("case X of true -> ok end")
    ),
    ?assertMatch(
        {'receive', _, [{clause, _, 'case', [{var, _, '_'}], empty, [{atom, _, true}]}]},
        parse_expr("receive _ -> true end")
    ),
    ?assertMatch(
        {'try', _, [{atom, _, ok}],
            [{clause, _, 'case', [{var, _,'_'}], empty, [{atom, _, ok}]}],
            [
                {clause, _, 'catch', [{var, _, '_'}], empty, [{atom, _, ok}]},
                {clause, _, 'catch', [{var, _, '_'}, {var, _, '_'}], empty, [{atom, _, ok}]},
                {clause, _, 'catch', [{var, _, '_'}, {var, _, '_'}, {var, _, '_'}], empty, [{atom, _, ok}]}
            ],
            []
        },
        parse_expr("try ok of _ -> ok catch _ -> ok; _:_ -> ok; _:_:_ -> ok end")
    ).

types(Config) when is_list(Config) ->
    ?assertMatch(
        {op, _, '::', {var, _, 'Foo'}, {atom, _, foo}},
        parse_type("Foo :: foo")
    ),
    ?assertMatch(
        {op, _, '|', {var, _, 'Foo'}, {var, _, 'Bar'}},
        parse_type("Foo | Bar")
    ),
    ?assertMatch(
        {op, _, '..', {integer, _, 1}, {var, _, 'Bar'}},
        parse_type("1..Bar")
    ),
    ?assertMatch(
        {op, _, '+', {op, _, '-', {integer, _, 1}}, {op, _, '*', {integer, _, 2}, {integer, _, 3}}},
        parse_type("- 1 + 2 * 3")
    ),
    ?assertMatch(
        {tuple, _, [
            {call, _, {atom, _, foo}, []},
            {call, _, {remote, _, {atom, _, foo}, {atom, _, bar}}, []}
        ]},
        parse_type("{foo(), foo:bar()}")
    ),
    ?assertMatch(
        {list, _, [{list, _, [{list, _, []}]}, {'...', _}]},
        parse_type("[[[]], ...]")
    ),
    ?assertMatch(
        {map, _, [
            {map_field_exact, _, {map, _, []}, {integer, _, 1}},
            {map_field_assoc, _, {tuple, _, []}, {integer, _, 2}}
        ]},
        parse_type("#{#{} := 1, {} => 2}")
    ),
    ?assertMatch(
        {tuple, _, [
            {bin, _, []},
            {bin, _, [{bin_element, _, {var, _, '_'}, {integer, _, 8}, default}]},
            {bin, _, [
                {bin_element, _, {var, _, '_'}, {bin_size, _, {var, _, '_'}, {integer, _, 8}}, default}
            ]},
            {bin, _, [
                {bin_element, _, {var, _, '_'}, {integer, _, 8}, default},
                {bin_element, _, {var, _, '_'}, {bin_size, _, {var, _, '_'}, {integer, _, 4}}, default}
            ]}
        ]},
        parse_type("{<<>>, <<_:8>>, <<_:_*8>>, <<_:8, _:_*4>>}")
    ),
    ?assertMatch(
        {'fun', _, type},
        parse_type("fun()")
    ),
    ?assertMatch(
        {'fun', _, {type, [{'...', _}], {call, _, {atom, _, integer}, []}}},
        parse_type("fun((...) -> integer())")
    ),
    ?assertMatch(
        {'fun', _, {type, [], {atom, _, ok}}},
        parse_type("fun(() -> ok)")
    ),
    ?assertMatch(
        {'fun', _, {type, [{call, _, {atom, _, integer}, []}], {call, _, {atom, _,integer}, []}}},
        parse_type("fun((integer()) -> integer())")
    ).

annos(Config) when is_list(Config) ->
    %% We parse with leading \n to avoid accounting for extra stuff parse_expr does
    ?assertMatch(
        {integer, #{location := {2, 1}, end_location := {2, 4}, text := "100"}, 100},
        parse_expr("\n100")
    ),
    ?assertMatch(
        {float, #{location := {2, 1}, end_location := {2, 5}, text := "10.0"}, 10.0},
        parse_expr("\n10.0")
    ),
    ?assertMatch(
        {char, #{location := {2, 1}, end_location := {2, 4}, text := "$\\s"}, $\s},
        parse_expr("\n$\\s")
    ),
    ?assertMatch(
        {atom, #{location := {2, 1}, end_location := {2, 4}, text := "foo"}, foo},
        parse_expr("\nfoo")
    ),
    ?assertMatch(
        {string, #{location := {2, 1}, end_location := {2, 6}, text := "\"foo\""}, "foo"},
        parse_expr("\n\"foo\"")
    ),
    ?assertMatch(
        {var, #{location := {2, 1}, end_location := {2, 4}, text := "Foo"}, 'Foo'},
        parse_expr("\nFoo")
    ),
    ?assertMatch(
        {atom, #{pre_comments := [{comment, #{location := {2, 1}, end_location := {3, 6}}, ["%foo", "%bar"]}]}, ok},
        parse_expr(
            "\n%foo\n"
            " %bar\n"
            "ok"
        )
    ),
    ?assertMatch(
        {op, #{location := {2, 1}, end_location := {2, 6}}, '+', _, _},
        parse_expr("\n1 + 2")
    ),
    ?assertMatch(
        {list, #{location := {2, 1}, end_location := {3, 4}}, [
            {call, #{location := {2, 2}, end_location := {2, 16}}, _, [
                {tuple, #{location := {2, 6}, end_location := {2, 12}}, [_, _]},
                _
            ]},
            {map, #{location := {2, 18}, end_location := {3, 3}}, [
                {map_field_assoc, #{location := {2, 20}, end_location := {3, 2}}, _, _}
            ]}
        ]},
        parse_expr("\n[foo({1, 2}, 3), #{4 =>\n5}]")
    ),
    ?assertMatch(
        {attribute, #{location := {1, 1}, end_location := {1, 11}},
            {atom, #{location := {1, 2}, end_location := {1, 7}, text := "'foo'"}, foo},
            [_]
        },
        parse_form("-'foo'(1).")
    ),
    ?assertMatch(
        {function, #{location := {1, 1}, end_location := {2, 14}}, [
            {clause, #{location := {1, 1}, end_location := {1, 12}}, _, [_], empty, [_]},
            {clause, #{location := {2, 1}, end_location := {2, 13}}, _, [_], empty, [_]}
        ]},
        parse_form(
            "foo(a) -> 1;\n"
            "foo(b) -> 12."
        )
    ).

parse_expr(String) ->
    {function, _, [{clause, _, _, [], empty, [Expr]}]} =
        parse_form("f() -> " ++ String ++ "."),
    Expr.

parse_pat(String) ->
    {function, _, [{clause, _, _, [Pat], empty, [_]}]} =
        parse_form("f(" ++ String ++ ") -> ok."),
    Pat.

parse_type(String) ->
    {attribute, _, {atom, _, type}, [{op, _, '::', _, Type}]} =
        parse_form("-type foo() :: " ++ String ++ "."),
    Type.

parse_form(String) ->
    case erlfmt:read_forms_string("nofile", String) of
        {ok, [Form], []} ->
            Form;
        {error, {_, Mod, Reason}} ->
            ct:fail("Expected successful parse:\n~ts\ngot: ~ts", [String, Mod:format_error(Reason)])
    end.

smoke_test_cli(Config) when is_list(Config) ->
    %% this relies on the _build structure rebar3 uses
    Escript = filename:join(code:lib_dir(erlfmt), "../../bin/erlfmt"),
    ?assertMatch("Usage: erlfmt " ++ _, os:cmd(Escript ++ " -h")).

smoke_test_parser_ac2d(Config) -> smoke_test_parser(ac2d, Config).
smoke_test_parser_admdb(Config) -> smoke_test_parser(admdb, Config).
smoke_test_parser_asyncwd(Config) -> smoke_test_parser(asyncwd, Config).
smoke_test_parser_bcastd(Config) -> smoke_test_parser(bcastd, Config).
smoke_test_parser_bcld(Config) -> smoke_test_parser(bcld, Config).
smoke_test_parser_bizd(Config) -> smoke_test_parser(bizd, Config).
smoke_test_parser_bizdb(Config) -> smoke_test_parser(bizdb, Config).
smoke_test_parser_bl2d(Config) -> smoke_test_parser(bl2d, Config).
smoke_test_parser_calld(Config) -> smoke_test_parser(calld, Config).
smoke_test_parser_camd(Config) -> smoke_test_parser(camd, Config).
smoke_test_parser_cfgd(Config) -> smoke_test_parser(cfgd, Config).
smoke_test_parser_chatd(Config) -> smoke_test_parser(chatd, Config).
smoke_test_parser_ctsd(Config) -> smoke_test_parser(ctsd, Config).
smoke_test_parser_deld(Config) -> smoke_test_parser(deld, Config).
smoke_test_parser_deviced(Config) -> smoke_test_parser(deviced, Config).
smoke_test_parser_dispatcher(Config) -> smoke_test_parser(dispatcher, Config).
smoke_test_parser_elgd(Config) -> smoke_test_parser(elgd, Config).
smoke_test_parser_erl2(Config) -> smoke_test_parser(erl2, Config).
smoke_test_parser_erlbuild(Config) -> smoke_test_parser(erlbuild, Config).
smoke_test_parser_erlclient(Config) -> smoke_test_parser(erlclient, Config).
smoke_test_parser_erlfmt(Config) -> smoke_test_parser(erlfmt, Config).
smoke_test_parser_fbid(Config) -> smoke_test_parser(fbid, Config).
smoke_test_parser_fbmd(Config) -> smoke_test_parser(fbmd, Config).
smoke_test_parser_fgtd(Config) -> smoke_test_parser(fgtd, Config).
smoke_test_parser_filed(Config) -> smoke_test_parser(filed, Config).
smoke_test_parser_forgets(Config) -> smoke_test_parser(forgets, Config).
smoke_test_parser_gcalld(Config) -> smoke_test_parser(gcalld, Config).
smoke_test_parser_grpd(Config) -> smoke_test_parser(grpd, Config).
smoke_test_parser_ipsd(Config) -> smoke_test_parser(ipsd, Config).
smoke_test_parser_llcd(Config) -> smoke_test_parser(llcd, Config).
smoke_test_parser_loader(Config) -> smoke_test_parser(loader, Config).
smoke_test_parser_loadtestd(Config) -> smoke_test_parser(loadtestd, Config).
smoke_test_parser_locd(Config) -> smoke_test_parser(locd, Config).
smoke_test_parser_mmsd(Config) -> smoke_test_parser(mmsd, Config).
smoke_test_parser_mnfd(Config) -> smoke_test_parser(mnfd, Config).
smoke_test_parser_monitor(Config) -> smoke_test_parser(monitor, Config).
smoke_test_parser_msgd(Config) -> smoke_test_parser(msgd, Config).
smoke_test_parser_ntsd(Config) -> smoke_test_parser(ntsd, Config).
smoke_test_parser_offd(Config) -> smoke_test_parser(offd, Config).
smoke_test_parser_orphand(Config) -> smoke_test_parser(orphand, Config).
smoke_test_parser_payd(Config) -> smoke_test_parser(payd, Config).
smoke_test_parser_pgwd(Config) -> smoke_test_parser(pgwd, Config).
smoke_test_parser_pmtd(Config) -> smoke_test_parser(pmtd, Config).
smoke_test_parser_pmtdb(Config) -> smoke_test_parser(pmtdb, Config).
smoke_test_parser_presd(Config) -> smoke_test_parser(presd, Config).
smoke_test_parser_ps2d(Config) -> smoke_test_parser(ps2d, Config).
smoke_test_parser_pshd(Config) -> smoke_test_parser(pshd, Config).
smoke_test_parser_ptokend(Config) -> smoke_test_parser(ptokend, Config).
smoke_test_parser_queryd(Config) -> smoke_test_parser(queryd, Config).
smoke_test_parser_reg2d(Config) -> smoke_test_parser(reg2d, Config).
smoke_test_parser_regd(Config) -> smoke_test_parser(regd, Config).
smoke_test_parser_regfe(Config) -> smoke_test_parser(regfe, Config).
smoke_test_parser_reld(Config) -> smoke_test_parser(reld, Config).
smoke_test_parser_riddb(Config) -> smoke_test_parser(riddb, Config).
smoke_test_parser_rptd(Config) -> smoke_test_parser(rptd, Config).
smoke_test_parser_schd(Config) -> smoke_test_parser(schd, Config).
smoke_test_parser_sdld(Config) -> smoke_test_parser(sdld, Config).
smoke_test_parser_sgwd(Config) -> smoke_test_parser(sgwd, Config).
smoke_test_parser_shardtestdb(Config) -> smoke_test_parser(shardtestdb, Config).
smoke_test_parser_sigd(Config) -> smoke_test_parser(sigd, Config).
smoke_test_parser_smc_observer(Config) -> smoke_test_parser(smc_observer, Config).
smoke_test_parser_ssnd(Config) -> smoke_test_parser(ssnd, Config).
smoke_test_parser_stsd(Config) -> smoke_test_parser(stsd, Config).
smoke_test_parser_syncd(Config) -> smoke_test_parser(syncd, Config).
smoke_test_parser_systemd(Config) -> smoke_test_parser(systemd, Config).
smoke_test_parser_tapd(Config) -> smoke_test_parser(tapd, Config).
smoke_test_parser_tftd(Config) -> smoke_test_parser(tftd, Config).
smoke_test_parser_thrift(Config) -> smoke_test_parser(thrift, Config).
smoke_test_parser_thriftrpc(Config) -> smoke_test_parser(thriftrpc, Config).
smoke_test_parser_trcd(Config) -> smoke_test_parser(trcd, Config).
smoke_test_parser_ugwd(Config) -> smoke_test_parser(ugwd, Config).
smoke_test_parser_usrd(Config) -> smoke_test_parser(usrd, Config).
smoke_test_parser_vestad(Config) -> smoke_test_parser(vestad, Config).
smoke_test_parser_void(Config) -> smoke_test_parser(void, Config).
smoke_test_parser_void_client(Config) -> smoke_test_parser(void_client, Config).
smoke_test_parser_wa(Config) -> smoke_test_parser(wa, Config).
smoke_test_parser_wa_admintool(Config) -> smoke_test_parser(wa_admintool, Config).
smoke_test_parser_wa_asyncd_lib(Config) -> smoke_test_parser(wa_asyncd_lib, Config).
smoke_test_parser_wa_auth(Config) -> smoke_test_parser(wa_auth, Config).
smoke_test_parser_wa_biz(Config) -> smoke_test_parser(wa_biz, Config).
smoke_test_parser_wa_chat(Config) -> smoke_test_parser(wa_chat, Config).
smoke_test_parser_wa_crypto(Config) -> smoke_test_parser(wa_crypto, Config).
smoke_test_parser_wa_data(Config) -> smoke_test_parser(wa_data, Config).
smoke_test_parser_wa_db(Config) -> smoke_test_parser(wa_db, Config).
smoke_test_parser_wa_experimental(Config) -> smoke_test_parser(wa_experimental, Config).
smoke_test_parser_wa_integrity(Config) -> smoke_test_parser(wa_integrity, Config).
smoke_test_parser_wa_listd(Config) -> smoke_test_parser(wa_listd, Config).
smoke_test_parser_wa_nodeapi(Config) -> smoke_test_parser(wa_nodeapi, Config).
smoke_test_parser_wa_proto(Config) -> smoke_test_parser(wa_proto, Config).
smoke_test_parser_wa_raft(Config) -> smoke_test_parser(wa_raft, Config).
smoke_test_parser_wa_reg(Config) -> smoke_test_parser(wa_reg, Config).
smoke_test_parser_wa_service(Config) -> smoke_test_parser(wa_service, Config).
smoke_test_parser_wa_web(Config) -> smoke_test_parser(wa_web, Config).
smoke_test_parser_wamd(Config) -> smoke_test_parser(wamd, Config).
smoke_test_parser_wamd_api(Config) -> smoke_test_parser(wamd_api, Config).
smoke_test_parser_wamid(Config) -> smoke_test_parser(wamid, Config).
smoke_test_parser_webd(Config) -> smoke_test_parser(webd, Config).
smoke_test_parser_zcrawld(Config) -> smoke_test_parser(zcrawld, Config).

smoke_test_parser_autotest(Config) ->
    smoke_test_parser("autotest/prototype/autotest", Config),
    smoke_test_parser("autotest/prototype/autotest_models", Config),
    smoke_test_parser("autotest/prototype/cluster", Config),
    smoke_test_parser("autotest/prototype/pyrlclient", Config).

smoke_test_parser_clust(Config) ->
    smoke_test_parser("clust", Config),
    smoke_test_parser("clust/clust_cli", Config),
    smoke_test_parser("clust/clust_lib", Config),
    smoke_test_parser("clust/clust_service", Config).

smoke_test_parser_rebar3_plugins(Config) ->
    smoke_test_parser("rebar3_plugins/analyze", Config),
    smoke_test_parser("rebar3_plugins/ct_test_helpers", Config),
    smoke_test_parser("rebar3_plugins/rebar3_fbpkg", Config),
    smoke_test_parser("rebar3_plugins/relaxed_dialyzer", Config),
    smoke_test_parser("rebar3_plugins/thrift_build", Config),
    smoke_test_parser("rebar3_plugins/wa_release", Config).

smoke_test_parser_util(Config) ->
    smoke_test_parser("util/erlint", Config).

smoke_test_parser(Path, Config) ->
    Root = filename:join([code:lib_dir(erlfmt), "../../../..", Path]),
    case filelib:is_dir(Root) of
        true -> ok;
        false -> ct:fail("~ts is not a directory", [Root])
    end,
    Dirs = [filename:join(Root, SubDir) || SubDir <- ["src", "include", "test"]],
    [smoke_test_parser_dir(Dir, Config) || Dir <- Dirs, filelib:is_dir(Dir)],
    ok.

-define(EXCLUDE_FILES, [
    "wa/src/gen_factory_sample.erl",
    "wa/src/wa_table_formatter.erl"
]).

smoke_test_parser_dir(Dir, Config) ->
    Files = filelib:wildcard(filename:join(Dir, "*.{erl,hrl}")),
    [smoke_test_parser_file(File, Config) || File <- Files, not excluded(File)],
    ok.

smoke_test_parser_file(FileName, _Config) ->
    case erlfmt:read_forms(FileName) of
        {ok, _, []} ->
            ok;
        {ok, _, Warnings} ->
            ct:fail([unicode:characters_to_binary(erlfmt:format_error_info(Info)) || Info <- Warnings]);
        {error, Error} ->
            ct:fail(unicode:characters_to_binary(erlfmt:format_error_info(Error)))
    end.

excluded(File) ->
    lists:any(
        fun(Pattern) -> string:find(File, Pattern, trailing) =:= Pattern end,
        ?EXCLUDE_FILES
    ).

snapshot_simple_comments(Config) -> snapshot_same("simple_comments.erl", Config).
snapshot_comments(Config) -> snapshot_formatted("comments.erl", Config).
snapshot_broken(Config) -> snapshot_formatted("broken.erl", Config).

snapshot_same(Module, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    {ok, _} = erlfmt:format_file(filename:join(DataDir, Module), [{out, PrivDir}]),
    {ok, Original} = file:read_file(filename:join(DataDir, Module)),
    {ok, Formatted} = file:read_file(filename:join(PrivDir, Module)),
    ?assertEqual(Original, Formatted).

snapshot_formatted(Module, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    {ok, Expected} = file:read_file(filename:join([DataDir, Module ++ ".formatted"])),
    {ok, _} = erlfmt:format_file(filename:join([DataDir, Module]), [{out, PrivDir}]),
    {ok, Formatted} = file:read_file(filename:join([PrivDir, Module])),
    ?assertEqual(Expected, Formatted),
    {ok, _} = erlfmt:format_file(filename:join([DataDir, Module ++ ".formatted"]), [{out, PrivDir}]),
    {ok, FormattedFormatted} = file:read_file(filename:join([PrivDir, Module ++ ".formatted"])),
    ?assertEqual(Expected, FormattedFormatted).
