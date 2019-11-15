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
    macro_call_exprs/1,
    macro_call_pats/1,
    macro_call_types/1,
    macro_definitions/1,
    functions_and_funs/1,
    smoke_test_cli/1,
    smoke_test_parser_ac2d/1,
    smoke_test_parser_adsd/1,
    smoke_test_parser_agtd/1,
    smoke_test_parser_asyncd/1,
    smoke_test_parser_asyncwd/1,
    smoke_test_parser_autotest/1,
    smoke_test_parser_bcld/1,
    smoke_test_parser_bizd/1,
    smoke_test_parser_bl2d/1,
    smoke_test_parser_camd/1,
    smoke_test_parser_cfgd/1,
    smoke_test_parser_chatd/1,
    smoke_test_parser_ctsd/1,
    smoke_test_parser_deld/1,
    smoke_test_parser_deviced/1,
    smoke_test_parser_elgd/1,
    smoke_test_parser_erl2/1,
    smoke_test_parser_erlfmt/1,
    smoke_test_parser_fbmd/1,
    smoke_test_parser_fgtd/1,
    smoke_test_parser_forgets/1,
    smoke_test_parser_ftest/1,
    smoke_test_parser_gcalld/1,
    smoke_test_parser_grpd/1,
    smoke_test_parser_ipsd/1,
    smoke_test_parser_keyd/1,
    smoke_test_parser_llcd/1,
    smoke_test_parser_loader/1,
    smoke_test_parser_locd/1,
    smoke_test_parser_mmsd/1,
    smoke_test_parser_mnfd/1,
    smoke_test_parser_monitor/1,
    smoke_test_parser_ndpd/1,
    smoke_test_parser_ntsd/1,
    smoke_test_parser_offd/1,
    smoke_test_parser_payd/1,
    smoke_test_parser_pgwd/1,
    smoke_test_parser_plhd/1,
    smoke_test_parser_pmtd/1,
    smoke_test_parser_presd/1,
    smoke_test_parser_ps2d/1,
    smoke_test_parser_pshd/1,
    smoke_test_parser_ptokend/1,
    smoke_test_parser_queryd/1,
    smoke_test_parser_reg2d/1,
    smoke_test_parser_regd/1,
    smoke_test_parser_regfe/1,
    smoke_test_parser_reld/1,
    smoke_test_parser_rptd/1,
    smoke_test_parser_schd/1,
    smoke_test_parser_sdld/1,
    smoke_test_parser_sgwd/1,
    smoke_test_parser_sigd/1,
    smoke_test_parser_smc_observer/1,
    smoke_test_parser_smppd/1,
    smoke_test_parser_ssnd/1,
    smoke_test_parser_stsd/1,
    smoke_test_parser_syncd/1,
    smoke_test_parser_tapd/1,
    smoke_test_parser_tftd/1,
    smoke_test_parser_thrift/1,
    smoke_test_parser_thrift_gen/1,
    smoke_test_parser_thrift_sync/1,
    smoke_test_parser_trcd/1,
    smoke_test_parser_ugwd/1,
    smoke_test_parser_usrd/1,
    smoke_test_parser_void/1,
    smoke_test_parser_wa/1,
    smoke_test_parser_wa_asyncd_lib/1,
    smoke_test_parser_wa_chat/1,
    smoke_test_parser_wa_crypto/1,
    smoke_test_parser_wa_db/1,
    smoke_test_parser_wa_experimental/1,
    smoke_test_parser_wa_http/1,
    smoke_test_parser_wa_listd/1,
    smoke_test_parser_wamd/1,
    smoke_test_parser_wamid/1,
    smoke_test_parser_wa_pool_mon/1,
    smoke_test_parser_wa_reg/1,
    smoke_test_parser_wa_service/1,
    smoke_test_parser_wa_web/1,
    smoke_test_parser_webd/1,
    smoke_test_parser_zcrawld/1
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
            macro_call_exprs,
            macro_call_pats,
            macro_call_types,
            macro_definitions,
            functions_and_funs
        ]},
        {smoke_tests, [parallel], [
            {group, smoke_test_parser},
            smoke_test_cli
        ]},
        {smoke_test_parser, [parallel], [
            smoke_test_parser_wa, %% first since it's the slowest
            smoke_test_parser_ac2d,
            smoke_test_parser_adsd,
            smoke_test_parser_agtd,
            smoke_test_parser_asyncd,
            smoke_test_parser_asyncwd,
            smoke_test_parser_autotest,
            smoke_test_parser_bcld,
            smoke_test_parser_bizd,
            smoke_test_parser_bl2d,
            smoke_test_parser_camd,
            smoke_test_parser_cfgd,
            smoke_test_parser_chatd,
            smoke_test_parser_ctsd,
            smoke_test_parser_deld,
            smoke_test_parser_deviced,
            smoke_test_parser_elgd,
            smoke_test_parser_erl2,
            smoke_test_parser_erlfmt,
            smoke_test_parser_fbmd,
            smoke_test_parser_fgtd,
            smoke_test_parser_forgets,
            smoke_test_parser_ftest,
            smoke_test_parser_gcalld,
            smoke_test_parser_grpd,
            smoke_test_parser_ipsd,
            smoke_test_parser_keyd,
            smoke_test_parser_llcd,
            smoke_test_parser_loader,
            smoke_test_parser_locd,
            smoke_test_parser_mmsd,
            smoke_test_parser_mnfd,
            smoke_test_parser_monitor,
            smoke_test_parser_ndpd,
            smoke_test_parser_ntsd,
            smoke_test_parser_offd,
            smoke_test_parser_payd,
            smoke_test_parser_pgwd,
            smoke_test_parser_plhd,
            smoke_test_parser_pmtd,
            smoke_test_parser_presd,
            smoke_test_parser_ps2d,
            smoke_test_parser_pshd,
            smoke_test_parser_ptokend,
            smoke_test_parser_queryd,
            smoke_test_parser_reg2d,
            smoke_test_parser_regd,
            smoke_test_parser_regfe,
            smoke_test_parser_reld,
            smoke_test_parser_rptd,
            smoke_test_parser_schd,
            smoke_test_parser_sdld,
            smoke_test_parser_sgwd,
            smoke_test_parser_sigd,
            smoke_test_parser_smc_observer,
            smoke_test_parser_smppd,
            smoke_test_parser_ssnd,
            smoke_test_parser_stsd,
            smoke_test_parser_syncd,
            smoke_test_parser_tapd,
            smoke_test_parser_tftd,
            smoke_test_parser_thrift,
            smoke_test_parser_thrift_gen,
            smoke_test_parser_thrift_sync,
            smoke_test_parser_trcd,
            smoke_test_parser_ugwd,
            smoke_test_parser_usrd,
            smoke_test_parser_void,
            smoke_test_parser_wa_asyncd_lib,
            smoke_test_parser_wa_chat,
            smoke_test_parser_wa_crypto,
            smoke_test_parser_wa_db,
            smoke_test_parser_wa_experimental,
            smoke_test_parser_wa_http,
            smoke_test_parser_wa_listd,
            smoke_test_parser_wamd,
            smoke_test_parser_wamid,
            smoke_test_parser_wa_pool_mon,
            smoke_test_parser_wa_reg,
            smoke_test_parser_wa_service,
            smoke_test_parser_wa_web,
            smoke_test_parser_webd,
            smoke_test_parser_zcrawld
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
        {attribute, _, record, {{atom, _, foo}, []}},
        parse_form("-record(foo, {}).")
    ),
    ?assertMatch(
        {attribute, _, type, {foo, {type, _, record, [{atom, _, foo}]}, []}},
        parse_form("-type foo() :: #foo{}.")
    ).

attributes(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, foo, {atom, _, bar}},
        parse_form("-foo(bar).")
    ),
    ?assertMatch(
        {attribute, _, ifdef, {atom, _, foo}},
        parse_form("-ifdef(foo).")
    ),
    ?assertMatch(
        {attribute, _, 'if', {op, _, '==', {macro_call, _, {atom, _, foo}, none}, {integer, _, 2}}},
        parse_form("-if(?foo == 2).")
    ),
    ?assertMatch(
        {attribute, _, else, undefined},
        parse_form("-else.")
    ),
    ?assertMatch(
        {attribute, _, endif, undefined},
        parse_form("-endif.")
    ),
    ?assertMatch(
        {attribute, _, endif, {macro_call, _, {var, _, 'BAR'}, none}},
        parse_form("-endif(?BAR).")
    ),
    ?assertMatch(
        {attribute, _, spec, {{{macro_call, _, {atom, _, 'foo'}, none}, 0}, [
            {type, _, 'fun', [{type, _, product, []}, {atom, _, ok}]}
        ]}},
        parse_form("-spec ?foo() -> ok.")
    ),
    ?assertMatch(
        {attribute, _, callback, {{{macro_call, _, {var, _, 'FOO'}, none}, 0}, [
            {type, _, 'fun', [{type, _, product, []}, {atom, _, ok}]}
        ]}},
        parse_form("-callback ?FOO() -> ok.")
    ),
    ?assertMatch(
        {attribute, _, export,
            {cons, _, {op, _, '/', {macro_call, _, {var, _, 'FOO'}, none}, {integer, _, 1}},
                {cons, _, {op, _, '/', {atom, _, foo}, {integer, _, 2}},
                    {nil, _}}}},
        parse_form("-export([?FOO/1, foo/2]).")
    ),
    ?assertMatch(
        {attribute, _, import, {
            {atom, _, foo},
            {cons, _, {op, _, '/', {atom, _, bar}, {integer, _, 1}}, {nil, _}}
        }},
        parse_form("-import(foo, [bar/1]).")
    ),
    ?assertMatch(
        {attribute, _, something_else,
            {tuple, _, [{atom, _, foo}, {op, _, '/', {atom, _, bar}, {integer, _, 1}}]}
        },
        parse_form("-something_else({foo, bar/1}).")
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
        {macro_call, _, {atom, _, foo}, [{guard, _, {atom, _, x}, {atom, _, true}}]},
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
        {macro_call, _, {var, _, 'FOO'}, [{type, _, union, [{integer, _, 1}, {integer, _, 2}]}]},
        parse_type("?FOO(1 | 2)")
    ),
    ?assertMatch(
        {type, _, record, [{macro_call, _, {atom, _, foo}, none}]},
        parse_type("?foo{}")
    ),
    ?assertMatch(
        {type, _, record, [{macro_call, _, {atom, _, foo}, none}]},
        parse_type("#?foo{}")
    ).

macro_definitions(Config) when is_list(Config) ->
    ?assertMatch(
        {attribute, _, define, {expr, {var, _, 'FOO'}, none, [[{atom, _, foo}]]}},
        parse_form("-define(FOO, foo).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {var, _, 'FOO'}, [], [[{atom, _, foo}]]}},
        parse_form("-define(FOO(), foo).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {var, _, 'FOO'}, [{var, _, 'X'}], [[{atom, _, foo}]]}},
        parse_form("-define(FOO(X), foo).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {atom, _, is_nice}, [{var, _, 'X'}], [[
            {call, _, {atom, _, is_tuple}, [{var, _, 'X'}]},
            {op, _, '=:=', {call, _, {atom, _,element}, [{integer, _, 1},{var, _, 'X'}]}, {atom, _, nice}}
        ]]}},
        parse_form("-define(is_nice(X), is_tuple(X), element(1, X) =:= nice).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {atom, _, foo}, none, {record_name, _, {atom, _,bar}}}},
        parse_form("-define(foo, #bar).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {atom, _, foo}, none, empty}},
        parse_form("-define(foo,).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {atom, _, pass}, [{var, _, 'Name'}],
            [[{'fun', _, {function, {var, _, 'Name'}, {integer, _, 2}}}]]}
        },
        parse_form("-define(pass(Name), fun Name/2).")
    ),
    ?assertMatch(
        {attribute, _, define, {clause, {atom, _, foo}, none, {clause, _, {atom, _, foo}, [], [], [{atom, _, ok}]}}},
        parse_form("-define(foo, foo() -> ok).")
    ),
    ?assertMatch(
        {attribute, _, define, {clause, {var, _, 'FOO'}, [{var, _, 'Name'}],
            {clause, _, {var, _, 'Name'}, [], [], [{atom, _, ok}]}}
        },
        parse_form("-define(FOO(Name), Name() -> ok).")
    ),
    ?assertMatch(
        {attribute, _, define, {expr, {var, _, 'HASH_FUN'}, none, [[{remote, _, {atom, _, erlang}, {atom, _, phash}}]]}},
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
        {'fun', _, {clauses, [{clause, _, 'fun', [], [], [{atom, _, ok}]}]}},
        parse_expr("fun () -> ok end")
    ),
    ?assertMatch(
        {'fun', _, {clauses, [{clause, _, {var, _, 'Foo'}, [], [], [{atom, _, ok}]}]}},
        parse_expr("fun Foo() -> ok end")
    ),
    ?assertMatch(
        {function, _, [{clause, _, {atom, _, foo}, [], [], [{atom, _, ok}]}]},
        parse_form("foo() -> ok.")
    ),
    ?assertMatch(
        {function, _, [{clause, _, {macro_call, _, {var, _, 'FOO'}, none}, [], [], [{atom, _, ok}]}]},
        parse_form("?FOO() -> ok.")
    ),
    ?assertMatch(
        {function, _, [
            {clause, _, {atom, _, foo}, [], [], [{atom, _, ok}]},
            {macro_call, _, {atom, _, bar}, []}
        ]},
        parse_form("foo() -> ok; ?bar().")
    ),
    ?assertMatch(
        {function, _, [{macro_call, _, {var, _,'TESTS_WITH_SETUP'}, [
            {atom, _, all_tests_},
            {'fun', _, {clauses, [{clause, _, 'fun', [], [], [{atom, _, ok}]}]}}
        ]}]},
        parse_form("?TESTS_WITH_SETUP(all_tests_, fun() -> ok end).")
    ).

parse_expr(String) ->
    {function, _, [{clause, _, _, [], [], [Expr]}]} =
        parse_form("f() -> " ++ String ++ "."),
    Expr.

parse_pat(String) ->
    {function, _, [{clause, _, _, [Pat], [], [_]}]} =
        parse_form("f(" ++ String ++ ") -> ok."),
    Pat.

parse_type(String) ->
    {attribute, _, type, {_, Type, []}} =
        parse_form("-type foo() :: " ++ String ++ "."),
    Type.

parse_form(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    case erlfmt_parse:parse_form(Tokens) of
        {ok, Form} ->
            Form;
        {error, {_, Mod, Reason}} ->
            ct:fail("Expected successful parse:\n~ts\ngot: ~ts", [String, Mod:format_error(Reason)])
    end.

smoke_test_cli(Config) when is_list(Config) ->
    %% this relies on the _build structure rebar3 uses
    Escript = filename:join(code:lib_dir(erlfmt), "../../bin/erlfmt"),
    ?assertMatch("Usage: erlfmt " ++ _, os:cmd(Escript ++ " -h")).

smoke_test_parser_ac2d(Config) -> smoke_test_parser(ac2d, Config).
smoke_test_parser_adsd(Config) -> smoke_test_parser(adsd, Config).
smoke_test_parser_agtd(Config) -> smoke_test_parser(agtd, Config).
smoke_test_parser_asyncd(Config) -> smoke_test_parser(asyncd, Config).
smoke_test_parser_asyncwd(Config) -> smoke_test_parser(asyncwd, Config).
smoke_test_parser_bcld(Config) -> smoke_test_parser(bcld, Config).
smoke_test_parser_bizd(Config) -> smoke_test_parser(bizd, Config).
smoke_test_parser_bl2d(Config) -> smoke_test_parser(bl2d, Config).
smoke_test_parser_camd(Config) -> smoke_test_parser(camd, Config).
smoke_test_parser_cfgd(Config) -> smoke_test_parser(cfgd, Config).
smoke_test_parser_chatd(Config) -> smoke_test_parser(chatd, Config).
smoke_test_parser_ctsd(Config) -> smoke_test_parser(ctsd, Config).
smoke_test_parser_deld(Config) -> smoke_test_parser(deld, Config).
smoke_test_parser_deviced(Config) -> smoke_test_parser(deviced, Config).
smoke_test_parser_elgd(Config) -> smoke_test_parser(elgd, Config).
smoke_test_parser_erl2(Config) -> smoke_test_parser(erl2, Config).
smoke_test_parser_erlfmt(Config) -> smoke_test_parser(erlfmt, Config).
smoke_test_parser_fbmd(Config) -> smoke_test_parser(fbmd, Config).
smoke_test_parser_fgtd(Config) -> smoke_test_parser(fgtd, Config).
smoke_test_parser_forgets(Config) -> smoke_test_parser(forgets, Config).
smoke_test_parser_ftest(Config) -> smoke_test_parser(ftest, Config).
smoke_test_parser_gcalld(Config) -> smoke_test_parser(gcalld, Config).
smoke_test_parser_grpd(Config) -> smoke_test_parser(grpd, Config).
smoke_test_parser_ipsd(Config) -> smoke_test_parser(ipsd, Config).
smoke_test_parser_keyd(Config) -> smoke_test_parser(keyd, Config).
smoke_test_parser_llcd(Config) -> smoke_test_parser(llcd, Config).
smoke_test_parser_loader(Config) -> smoke_test_parser(loader, Config).
smoke_test_parser_locd(Config) -> smoke_test_parser(locd, Config).
smoke_test_parser_mmsd(Config) -> smoke_test_parser(mmsd, Config).
smoke_test_parser_mnfd(Config) -> smoke_test_parser(mnfd, Config).
smoke_test_parser_monitor(Config) -> smoke_test_parser(monitor, Config).
smoke_test_parser_ndpd(Config) -> smoke_test_parser(ndpd, Config).
smoke_test_parser_ntsd(Config) -> smoke_test_parser(ntsd, Config).
smoke_test_parser_offd(Config) -> smoke_test_parser(offd, Config).
smoke_test_parser_payd(Config) -> smoke_test_parser(payd, Config).
smoke_test_parser_pgwd(Config) -> smoke_test_parser(pgwd, Config).
smoke_test_parser_plhd(Config) -> smoke_test_parser(plhd, Config).
smoke_test_parser_pmtd(Config) -> smoke_test_parser(pmtd, Config).
smoke_test_parser_presd(Config) -> smoke_test_parser(presd, Config).
smoke_test_parser_ps2d(Config) -> smoke_test_parser(ps2d, Config).
smoke_test_parser_pshd(Config) -> smoke_test_parser(pshd, Config).
smoke_test_parser_ptokend(Config) -> smoke_test_parser(ptokend, Config).
smoke_test_parser_queryd(Config) -> smoke_test_parser(queryd, Config).
smoke_test_parser_reg2d(Config) -> smoke_test_parser(reg2d, Config).
smoke_test_parser_regd(Config) -> smoke_test_parser(regd, Config).
smoke_test_parser_regfe(Config) -> smoke_test_parser(regfe, Config).
smoke_test_parser_reld(Config) -> smoke_test_parser(reld, Config).
smoke_test_parser_rptd(Config) -> smoke_test_parser(rptd, Config).
smoke_test_parser_schd(Config) -> smoke_test_parser(schd, Config).
smoke_test_parser_sdld(Config) -> smoke_test_parser(sdld, Config).
smoke_test_parser_sgwd(Config) -> smoke_test_parser(sgwd, Config).
smoke_test_parser_sigd(Config) -> smoke_test_parser(sigd, Config).
smoke_test_parser_smc_observer(Config) -> smoke_test_parser(smc_observer, Config).
smoke_test_parser_smppd(Config) -> smoke_test_parser(smppd, Config).
smoke_test_parser_ssnd(Config) -> smoke_test_parser(ssnd, Config).
smoke_test_parser_stsd(Config) -> smoke_test_parser(stsd, Config).
smoke_test_parser_syncd(Config) -> smoke_test_parser(synd, Config).
smoke_test_parser_tapd(Config) -> smoke_test_parser(tapd, Config).
smoke_test_parser_tftd(Config) -> smoke_test_parser(tftd, Config).
smoke_test_parser_thrift(Config) -> smoke_test_parser(thrift, Config).
smoke_test_parser_thrift_gen(Config) -> smoke_test_parser(thrift_gen, Config).
smoke_test_parser_thrift_sync(Config) -> smoke_test_parser(thrift_sync, Config).
smoke_test_parser_trcd(Config) -> smoke_test_parser(trcd, Config).
smoke_test_parser_ugwd(Config) -> smoke_test_parser(ugwd, Config).
smoke_test_parser_usrd(Config) -> smoke_test_parser(usrd, Config).
smoke_test_parser_void(Config) -> smoke_test_parser(void, Config).
smoke_test_parser_wa(Config) -> smoke_test_parser(wa, Config).
smoke_test_parser_wa_asyncd_lib(Config) -> smoke_test_parser(wa_asyncd_lib, Config).
smoke_test_parser_wa_chat(Config) -> smoke_test_parser(wa_chat, Config).
smoke_test_parser_wa_crypto(Config) -> smoke_test_parser(crypto, Config).
smoke_test_parser_wa_db(Config) -> smoke_test_parser(wa_db, Config).
smoke_test_parser_wa_experimental(Config) -> smoke_test_parser(wa_experimental, Config).
smoke_test_parser_wa_http(Config) -> smoke_test_parser(wa_http, Config).
smoke_test_parser_wa_listd(Config) -> smoke_test_parser(wa_listd, Config).
smoke_test_parser_wamd(Config) -> smoke_test_parser(wamd, Config).
smoke_test_parser_wamid(Config) -> smoke_test_parser(wamid, Config).
smoke_test_parser_wa_pool_mon(Config) -> smoke_test_parser(wa_pool_mon, Config).
smoke_test_parser_wa_reg(Config) -> smoke_test_parser(wa_reg, Config).
smoke_test_parser_wa_service(Config) -> smoke_test_parser(wa_service, Config).
smoke_test_parser_wa_web(Config) -> smoke_test_parser(wa_web, Config).
smoke_test_parser_webd(Config) -> smoke_test_parser(webd, Config).
smoke_test_parser_zcrawld(Config) -> smoke_test_parser(zcrawld, Config).

smoke_test_parser_autotest(Config) ->
    smoke_test_parser("autotest/prototype/autotest", Config),
    smoke_test_parser("autotest/prototype/autotest_models", Config),
    smoke_test_parser("autotest/prototype/cluster", Config),
    smoke_test_parser("autotest/prototype/pyrlclient", Config).

smoke_test_parser(App, Config) ->
    Root = filename:join([code:lib_dir(erlfmt), "../../lib", App]),
    Dirs = [filename:join(Root, SubDir) || SubDir <- ["src", "include", "test"]],
    [smoke_test_parser_dir(Dir, Config) || Dir <- Dirs, filelib:is_dir(Dir)],
    ok.

-define(EXCLUDE_FILES, [
    "wa/src/gen_factory_sample.erl",
    "wa/src/wa_table_formatter.erl",
    "wa/include/wa_remote_fun.hrl"
]).

smoke_test_parser_dir(Dir, Config) ->
    Files = filelib:wildcard(filename:join(Dir, "*.{erl,hrl}")),
    [smoke_test_parser_file(File, Config) || File <- Files, not excluded(File)],
    ok.

smoke_test_parser_file(FileName, Config) ->
    {ok, File} = file:open(FileName, [read]),
    try smoke_test_parser_loop(File, FileName, 1, Config)
    after file:close(File)
    end.

excluded(File) ->
    lists:any(
        fun(Pattern) -> string:find(File, Pattern, trailing) =:= Pattern end,
        ?EXCLUDE_FILES
    ).

smoke_test_parser_loop(File, FileName, Loc0, Config) ->
    case io:scan_erl_form(File, "", Loc0, [text]) of
        {ok, Tokens, Loc} ->
            case erlfmt_parse:parse_form(Tokens) of
                {ok, _} -> ok;
                {error, {Loc1, Mod, Reason}} ->
                    ct:fail("~s:~B: ~s", [FileName, Loc1, Mod:format_error(Reason)])
            end,
            smoke_test_parser_loop(File, FileName, Loc, Config);
        {eof, _Loc} ->
            ok;
        {error, Reason} ->
            ct:fail("~s:~B: ~s", [FileName, Loc0, file:format_error(Reason)]);
        {error, {_, Mod, Reason}, Loc} ->
            ct:fail("~s:~B: ~s", [FileName, Loc, Mod:format_error(Reason)])
    end.
