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
-module(erlfmt_format).

-include("erlfmt.hrl").

-export([to_algebra/1, comments/1]).

-import(erlfmt_algebra, [
    force_breaks/0,
    group/1,
    format/2,
    string/1,
    empty/0,
    break/0,
    break/1,
    break/2,
    break/3,
    flex_break/1,
    flex_break/0,
    flex_break/2,
    flex_break/3,
    space/2,
    nest/2,
    nest/3,
    line/0,
    line/1,
    line/2,
    concat/1,
    concat/2,
    concat/3,
    next_break_fits/1,
    next_break_fits/2,
    fold_doc/2
]).

-define(INDENT, 4).
-define(NEXT_BREAK_FITS, [map, list, record, block, 'fun', lc, bc]).
-define(NEXT_BREAK_FITS_OPS, ['=', '::']).

-spec to_algebra(erlfmt_parse:abstract_form()) -> erlfmt_algebra:doc().
to_algebra({function, Meta, Clauses}) ->
    Doc = concat(clauses_to_algebra(Clauses), string(".")),
    combine_comments(Meta, Doc);
to_algebra({attribute, Meta, Name, []}) ->
    Doc = concat(<<"-">>, expr_to_algebra(Name), <<".">>),
    combine_comments(Meta, Doc);
to_algebra({attribute, Meta, {atom, _, define}, [Define | Body]}) ->
    HeadD = concat(<<"-define(">>, expr_to_algebra(Define), <<",">>),
    NextBreakFits = length(Body) =:= 1 andalso is_next_break_fits(hd(Body)),
    with_next_break_fits(NextBreakFits, block_to_algebra(Meta, Body), fun (BodyD) ->
        Doc = surround(HeadD, <<" ">>, BodyD, <<"">>, <<").">>),
        combine_comments(Meta, Doc)
    end);
to_algebra({attribute, Meta, {atom, _, RawName} = Name, [Value]}) when
    RawName =:= type; RawName =:= opaque; RawName =:= spec; RawName =:= callback
->
    ValueD = next_break_fits(expr_to_algebra(Value), enabled),
    Doc = concat([<<"-">>, expr_to_algebra(Name), <<" ">>, ValueD, <<".">>]),
    combine_comments(Meta, Doc);
to_algebra({attribute, Meta, {atom, _, record}, [Name, {tuple, TMeta, Values} = Tuple]}) ->
    HeadD = concat(<<"-record(">>, expr_to_algebra(Name), <<",">>),
    case has_no_comments_or_parens(TMeta) of
        true ->
            Doc = space(HeadD, container(TMeta, Values, <<"{">>, <<"}).">>)),
            combine_comments(Meta, Doc);
        false ->
            Doc = surround(HeadD, <<"">>, expr_to_algebra(Tuple), <<"">>, <<").">>),
            combine_comments(Meta, Doc)
    end;
to_algebra({attribute, Meta, Name, Values}) ->
    Doc = concat(<<"-">>, expr_to_algebra(Name), call(Meta, Values, <<"(">>, <<").">>)),
    combine_comments(Meta, Doc);
to_algebra(Expr) ->
    Meta = element(2, Expr),
    Doc = do_expr_to_algebra(Expr),
    case maps:get(dot, Meta, false) of
        true ->
            combine_comments(Meta, concat(Doc, <<".">>));
        false ->
            combine_comments(Meta, Doc)
    end.

-spec expr_to_algebra(erlfmt_parse:abstract_expr()) -> erlfmt_algebra:doc().
expr_to_algebra(Expr) when is_tuple(Expr) ->
    Meta = element(2, Expr),
    Doc = do_expr_to_algebra(Expr),
    combine_comments(Meta, maybe_wrap_in_parens(Meta, Doc));
expr_to_algebra(Other) ->
    do_expr_to_algebra(Other).

do_expr_to_algebra({string, Meta, _Value}) ->
    string_to_algebra(erlfmt_scan:get_anno(text, Meta));
do_expr_to_algebra({char, #{text := "$ "}, $\s}) ->
    <<"$\\s">>;
do_expr_to_algebra({Atomic, Meta, _Value}) when ?IS_ATOMIC(Atomic) ->
    string(erlfmt_scan:get_anno(text, Meta));
do_expr_to_algebra({concat, _Meta, Values}) ->
    concat_to_algebra(Values);
do_expr_to_algebra({op, _Meta, Op, Expr}) ->
    unary_op_to_algebra(Op, Expr);
do_expr_to_algebra({op, Meta, Op, Left, Right}) ->
    binary_op_to_algebra(Op, Meta, Left, Right);
do_expr_to_algebra({tuple, Meta, Values}) ->
    flex_container(Meta, Values, <<"{">>, <<"}">>);
do_expr_to_algebra({list, Meta, Values}) ->
    container(Meta, Values, <<"[">>, <<"]">>);
do_expr_to_algebra({cons, _, Head, Tail}) ->
    cons_to_algebra(Head, Tail);
do_expr_to_algebra({bin, Meta, Values}) ->
    flex_container(Meta, Values, <<"<<">>, <<">>">>);
do_expr_to_algebra({bin_element, _Meta, Expr, Size, Types}) ->
    bin_element_to_algebra(Expr, Size, Types);
do_expr_to_algebra({map, Meta, Values}) ->
    container(Meta, Values, <<"#{">>, <<"}">>);
do_expr_to_algebra({map, Meta, Expr, Values}) ->
    concat(expr_to_algebra(Expr), container(Meta, Values, <<"#{">>, <<"}">>));
do_expr_to_algebra({map_field_assoc, _Meta, Key, Value}) ->
    field_to_algebra(<<" =>">>, Key, Value);
do_expr_to_algebra({map_field_exact, _Meta, Key, Value}) ->
    field_to_algebra(<<" :=">>, Key, Value);
do_expr_to_algebra({record, Meta, Name, Values}) ->
    concat(record_name_to_algebra(Meta, Name), container(Meta, Values, <<"{">>, <<"}">>));
do_expr_to_algebra({record, Meta, Expr, Name, Values}) ->
    concat(
        expr_to_algebra(Expr),
        record_name_to_algebra(Meta, Name),
        container(Meta, Values, <<"{">>, <<"}">>)
    );
do_expr_to_algebra({record_field, _Meta, Key, Value}) ->
    field_to_algebra(<<" =">>, Key, Value);
do_expr_to_algebra({record_index, Meta, Name, Key}) ->
    record_access_to_algebra(Meta, Name, Key);
do_expr_to_algebra({record_field, _Meta, Name}) ->
    expr_to_algebra(Name);
do_expr_to_algebra({record_field, Meta, Expr, Name, Key}) ->
    concat(expr_to_algebra(Expr), record_access_to_algebra(Meta, Name, Key));
do_expr_to_algebra({lc, _Meta, Expr, LcExprs}) ->
    comprehension_to_algebra(Expr, LcExprs, <<"[">>, <<"]">>);
do_expr_to_algebra({bc, _Meta, Expr, LcExprs}) ->
    comprehension_to_algebra(Expr, LcExprs, <<"<<">>, <<">>">>);
do_expr_to_algebra({generate, _Meta, Left, Right}) ->
    field_to_algebra(<<" <-">>, Left, Right);
do_expr_to_algebra({b_generate, _Meta, Left, Right}) ->
    field_to_algebra(<<" <=">>, Left, Right);
do_expr_to_algebra({call, Meta, Name, Args}) ->
    concat(expr_to_algebra(Name), call(Meta, Args, <<"(">>, <<")">>));
do_expr_to_algebra({macro_call, _Meta, Name, none}) ->
    concat(<<"?">>, expr_to_algebra(Name));
do_expr_to_algebra({macro_call, Meta, Name, Args}) ->
    concat(<<"?">>, expr_to_algebra(Name), call(Meta, Args, <<"(">>, <<")">>));
do_expr_to_algebra({macro_string, _Meta, Name}) ->
    concat(<<"??">>, expr_to_algebra(Name));
do_expr_to_algebra({remote, _Meta, Left, Right}) ->
    concat(expr_to_algebra(Left), <<":">>, expr_to_algebra(Right));
do_expr_to_algebra({block, Meta, Exprs}) ->
    surround_block(<<"begin">>, block_to_algebra(Meta, Exprs), <<"end">>);
do_expr_to_algebra({'fun', _Meta, Expr}) ->
    fun_to_algebra(Expr);
do_expr_to_algebra({args, Meta, Values}) ->
    container(Meta, Values, <<"(">>, <<")">>);
do_expr_to_algebra({'case', _Meta, Expr, Clauses}) ->
    Prefix = surround(<<"case">>, <<" ">>, expr_to_algebra(Expr), <<" ">>, <<"of">>),
    surround_block(Prefix, clauses_to_algebra(Clauses), <<"end">>);
do_expr_to_algebra({'receive', _Meta, Clauses}) ->
    surround_block(<<"receive">>, clauses_to_algebra(Clauses), <<"end">>);
do_expr_to_algebra({'receive', _Meta, [], AfterExpr, AfterBody}) ->
    AfterD = receive_after_to_algebra(AfterExpr, AfterBody),
    concat(force_breaks(), (line(<<"receive">>, line(AfterD, <<"end">>))));
do_expr_to_algebra({'receive', _Meta, Clauses, AfterExpr, AfterBody}) ->
    AfterD = receive_after_to_algebra(AfterExpr, AfterBody),
    surround_block(<<"receive">>, clauses_to_algebra(Clauses), line(AfterD, <<"end">>));
do_expr_to_algebra({'try', _Meta, Exprs, OfClauses, CatchClauses, After}) ->
    try_to_algebra(Exprs, OfClauses, CatchClauses, After);
do_expr_to_algebra({'catch', _Meta, Exprs}) ->
    ExprsD = lists:map(fun expr_to_algebra/1, Exprs),
    fold_doc(fun (Doc, Acc) -> concat(Doc, <<":">>, Acc) end, ExprsD);
do_expr_to_algebra({'if', _Meta, Clauses}) ->
    surround_block(<<"if">>, clauses_to_algebra(Clauses), <<"end">>);
do_expr_to_algebra({spec, _Meta, Name, [SingleClause]}) ->
    single_clause_spec_to_algebra(Name, SingleClause);
do_expr_to_algebra({spec, _Meta, Name, Clauses}) ->
    group(nest(line(expr_to_algebra(Name), clauses_to_algebra(Clauses)), ?INDENT));
do_expr_to_algebra({Clause, _, _, _, _} = Expr) when
    Clause =:= clause; Clause =:= spec_clause
->
    clause_to_algebra(Expr);
do_expr_to_algebra({'...', _Meta}) ->
    <<"...">>;
do_expr_to_algebra({bin_size, _Meta, Left, Right}) ->
    concat(expr_to_algebra(Left), <<"*">>, expr_to_algebra(Right));
do_expr_to_algebra({guard_or, _Meta, Guards}) ->
    guard_to_algebra(Guards, <<";">>);
do_expr_to_algebra({guard_and, _Meta, Guards}) ->
    guard_to_algebra(Guards, <<",">>);
do_expr_to_algebra(Other) ->
    error(unsupported, [Other]).

surround(Left, LeftSpace, Doc, RightSpace, Right) ->
    group(
        break(
            concat(Left, nest(concat(break(LeftSpace), Doc), ?INDENT, break)),
            RightSpace,
            Right
        )
    ).

surround_block(Left, Doc, Right) ->
    concat(
        force_breaks(),
        (group(line(concat(Left, nest(concat(line(), Doc), ?INDENT)), Right)))
    ).

string_to_algebra(Text) ->
    case string:split(Text, "\n", all) of
        [Line] ->
            string(Line);
        [First, "\""] ->
            string([First | "\\n\""]);
        [First | Lines] ->
            FirstD = string([First | "\\n\""]),
            LinesD = string_lines_to_algebra(Lines),
            line(FirstD, LinesD)
    end.

string_lines_to_algebra([LastLine]) ->
    string(["\"" | LastLine]);
string_lines_to_algebra([Line, "\""]) ->
    string(["\"", Line | "\\n\""]);
string_lines_to_algebra([Line | Lines]) ->
    LineD = string(["\"", Line | "\\n\""]),
    line(LineD, string_lines_to_algebra(Lines)).

%% there's always at least two elements in concat
concat_to_algebra([Value1, Value2 | _] = Values) ->
    ValuesD = lists:map(fun expr_to_algebra/1, Values),
    case has_break_between(Value1, Value2) of
        true ->
            concat(force_breaks(), group(fold_doc(fun erlfmt_algebra:line/2, ValuesD)));
        false ->
            group(fold_doc(fun erlfmt_algebra:break/2, ValuesD))
    end.

unary_op_to_algebra(Op, Expr) ->
    OpD = string(atom_to_binary(Op, utf8)),
    ExprD = expr_to_algebra(Expr),
    NeedsSpace = unary_needs_space(Expr, Op),
    if
        NeedsSpace ->
            space(OpD, ExprD);
        true ->
            concat(OpD, ExprD)
    end.

unary_needs_space(_, Op) when Op =:= 'not'; Op =:= 'bnot'; Op =:= 'catch' ->
    true;
unary_needs_space({op, Meta, _, _}, _) ->
    not erlfmt_scan:get_anno(parens, Meta, false);
unary_needs_space(_, _) ->
    false.

binary_op_to_algebra('..', _Meta, Left, Right) ->
    %% .. is special - non-assoc, no spaces around and never breaks
    concat(expr_to_algebra(Left), <<"..">>, expr_to_algebra(Right));
binary_op_to_algebra('/', _Meta, {atom, _, _} = Left, {integer, _, _} = Right) ->
    %% special-case the foo/N syntax in attributes, division with a literal atom
    %% doesn't make sense in normal code, so it's safe to apply it everywhere
    concat(expr_to_algebra(Left), <<"/">>, expr_to_algebra(Right));
binary_op_to_algebra('|' = Op, Meta0, Left, Right) ->
    %% | does not increase indentation
    %% don't print parens and comments twice - expr_to_algebra took care of it already
    Meta = erlfmt_scan:delete_annos([parens, pre_comments, post_comments], Meta0),
    binary_op_to_algebra(Op, Meta, Left, Right, 0);
binary_op_to_algebra(Op, Meta0, Left, Right) ->
    %% don't print parens and comments twice - expr_to_algebra took care of it already
    Meta = erlfmt_scan:delete_annos([parens, pre_comments, post_comments], Meta0),
    binary_op_to_algebra(Op, Meta, Left, Right, ?INDENT).

binary_op_to_algebra(Op, Meta, Left, Right, Indent) ->
    OpD = string(atom_to_binary(Op, utf8)),
    LeftD = binary_operand_to_algebra(Op, Left, Indent),
    RightD = binary_operand_to_algebra(Op, Right, 0),
    HasBreak = has_break_between(Left, Right),
    IsNextBreakFits = is_next_break_fits_op(Op, Right) andalso not HasBreak,

    Doc =
        with_next_break_fits(IsNextBreakFits, RightD, fun (R) ->
            RightOpD = nest(break(concat(<<" ">>, OpD), group(R)), Indent, break),
            concat(group(LeftD), group(concat(maybe_force_breaks(HasBreak), RightOpD)))
        end),

    combine_comments(Meta, maybe_wrap_in_parens(Meta, Doc)).

maybe_force_breaks(true) -> force_breaks();
maybe_force_breaks(false) -> empty().

with_next_break_fits(true, Doc, Fun) ->
    next_break_fits(Fun(next_break_fits(Doc, enabled)), disabled);
with_next_break_fits(false, Doc, Fun) ->
    Fun(Doc).

binary_operand_to_algebra(Op, {op, Meta, Op, Left, Right}, Indent) ->
    %% Same operator, no parens, means correct side and no repeated nesting
    case erlfmt_scan:get_anno(parens, Meta, false) of
        false -> binary_op_to_algebra(Op, Meta, Left, Right, Indent);
        _ -> binary_op_to_algebra(Op, Meta, Left, Right, ?INDENT)
    end;
binary_operand_to_algebra(_ParentOp, Expr, _Indent) ->
    expr_to_algebra(Expr).

container(Meta, Values, Left, Right) ->
    container_common(Meta, Values, Left, Right, break, last_normal).

flex_container(Meta, Values, Left, Right) ->
    container_common(Meta, Values, Left, Right, flex_break, last_fits).

call(Meta, Values, Left, Right) ->
    container_common(Meta, Values, Left, Right, break, last_fits).

container_common(_Meta, [], Left, Right, _Combine, _Last) ->
    concat(Left, Right);
container_common(Meta, [Value | _] = Values, Left, Right, Combine, Last) ->
    {HasTrailingComments, WasLastFits, ValuesD} =
        container_exprs_to_algebra(Values, Last, []),
    case HasTrailingComments orelse has_inner_break(Meta, Value) of
        true ->
            Doc = fold_doc(fun (D, Acc) -> line(concat(D, <<",">>), Acc) end, ValuesD),
            surround(Left, <<"">>, concat(force_breaks(), Doc), <<"">>, Right);
        false when Combine =:= break ->
            Doc = fold_doc(fun (D, Acc) -> break(concat(D, <<",">>), Acc) end, ValuesD),
            Wrapped = surround(Left, <<"">>, Doc, <<"">>, Right),
            case WasLastFits of
                true -> next_break_fits(Wrapped, disabled);
                false -> Wrapped
            end;
        false when Combine =:= flex_break ->
            Doc = fold_doc(
                fun (D, Acc) -> flex_break(concat(D, <<",">>), Acc) end,
                ValuesD
            ),
            Wrapped = group(concat(nest(concat(Left, Doc), ?INDENT, break), Right)),
            case WasLastFits of
                true -> next_break_fits(Wrapped, disabled);
                false -> Wrapped
            end
    end.

container_exprs_to_algebra([{comment, _, _} | _] = Comments, _Last, Acc) ->
    {true, false,
        lists:reverse(Acc, [concat(force_breaks(), comments_to_algebra(Comments))])};
container_exprs_to_algebra([Value | [{comment, _, _} | _] = Comments], _Last, Acc) ->
    {true, false,
        lists:reverse(Acc, [
            concat(
                force_breaks(),
                line(expr_to_algebra(Value), comments_to_algebra(Comments))
            )
        ])};
container_exprs_to_algebra([Value], last_fits, Acc) ->
    IsNextBreakFits = is_next_break_fits(Value),
    ValueD =
        case IsNextBreakFits of
            true -> next_break_fits(expr_to_algebra(Value), enabled);
            false -> expr_to_algebra(Value)
        end,
    {false, IsNextBreakFits, lists:reverse(Acc, [ValueD])};
container_exprs_to_algebra([Value], last_normal, Acc) ->
    {false, false, lists:reverse(Acc, [expr_to_algebra(Value)])};
container_exprs_to_algebra([Value | Values], Last, Acc) ->
    container_exprs_to_algebra(Values, Last, [expr_to_algebra(Value) | Acc]).

cons_to_algebra(Head, Tail) ->
    HeadD = expr_to_algebra(Head),
    TailD = concat(<<"| ">>, expr_to_algebra(Tail)),
    break(HeadD, TailD).

bin_element_to_algebra(Expr, Size, Types) ->
    Docs =
        [expr_to_algebra(Expr)] ++
            [bin_size_to_algebra(Size) || Size =/= default] ++
            [bin_types_to_algebra(Types) || Types =/= default],
    concat(Docs).

bin_size_to_algebra(Expr) ->
    concat(<<":">>, expr_to_algebra(Expr)).

bin_types_to_algebra(Types) ->
    TypesD = lists:map(fun expr_to_algebra/1, Types),
    concat(<<"/">>, fold_doc(fun (Doc, Acc) -> concat([Doc, <<"-">>, Acc]) end, TypesD)).

record_access_to_algebra(Meta, Name, Key) ->
    concat(record_name_to_algebra(Meta, Name), <<".">>, expr_to_algebra(Key)).

record_name_to_algebra(Meta, Name) ->
    %% Differentiate between #?FOO{} and ?FOO{}
    case erlfmt_scan:get_anno(macro_record, Meta, false) of
        true -> expr_to_algebra(Name);
        false -> concat(<<"#">>, expr_to_algebra(Name))
    end.

field_to_algebra(Op, Key, Value) ->
    KeyD = expr_to_algebra(Key),
    ValueD = expr_to_algebra(Value),

    with_next_break_fits(is_next_break_fits(Value, [call, macro_call]), ValueD, fun (V) ->
        concat(group(KeyD), group(nest(break(Op, group(V)), ?INDENT, break)))
    end).

comprehension_to_algebra(Expr, LcExprs, Left, Right) ->
    ExprD = expr_to_algebra(Expr),
    {_HasTrailingComment, _WasLastFits, LcExprsD} =
        container_exprs_to_algebra(LcExprs, last_normal, []),
    LcExprD = fold_doc(fun (D, Acc) -> break(concat(D, <<",">>), Acc) end, LcExprsD),
    Doc = concat([ExprD, break(<<" ">>), <<"||">>, <<" ">>, nest(group(LcExprD), 3)]),
    surround(Left, <<"">>, Doc, <<"">>, Right).

block_to_algebra([Expr]) ->
    expr_to_algebra(Expr);
block_to_algebra(Exprs) ->
    concat(force_breaks(), block_to_algebra_each(Exprs)).

block_to_algebra(Meta, [Expr]) ->
    concat(maybe_force_breaks(has_inner_break(Meta, Expr)), expr_to_algebra(Expr));
block_to_algebra(_Meta, Exprs) ->
    concat(force_breaks(), block_to_algebra_each(Exprs)).

%% standalone comments are always trailing other expressions
block_to_algebra_each([Expr | [{comment, _, _} | _] = Comments]) ->
    concat(force_breaks(), line(expr_to_algebra(Expr), comments_to_algebra(Comments)));
block_to_algebra_each([Expr]) ->
    expr_to_algebra(Expr);
block_to_algebra_each([Expr | [Next | _] = Rest]) ->
    ExprD = expr_to_algebra(Expr),
    RestD = block_to_algebra_each(Rest),
    case has_empty_line_between(Expr, Next) of
        true -> concat([ExprD, <<",">>, line(2), RestD]);
        false -> concat([ExprD, <<",">>, line(), RestD])
    end.

fun_to_algebra({function, _Anno, Name, Arity}) ->
    concat([
        <<"fun ">>,
        expr_to_algebra(Name),
        <<"/">>,
        expr_to_algebra(Arity)
    ]);
fun_to_algebra({function, _Anno, Mod, Name, Arity}) ->
    concat([
        <<"fun ">>,
        expr_to_algebra(Mod),
        <<":">>,
        expr_to_algebra(Name),
        <<"/">>,
        expr_to_algebra(Arity)
    ]);
fun_to_algebra({clauses, _Anno, [Clause]}) ->
    ClauseD = concat(maybe_force_breaks(clause_has_break(Clause)), expr_to_algebra(Clause)),
    group(break(space(<<"fun">>, ClauseD), <<"end">>));
fun_to_algebra({clauses, _Anno, Clauses}) ->
    ClausesD = clauses_to_algebra(Clauses),
    surround_block(<<"fun">>, ClausesD, <<"end">>);
fun_to_algebra(type) ->
    <<"fun()">>;
fun_to_algebra({type, Meta, Args, Result}) ->
    ArgsD = group(call(Meta, Args, <<"fun((">>, <<") ->">>)),
    with_next_break_fits(is_next_break_fits(Result), expr_to_algebra(Result), fun (ResultD) ->
        Body = concat(ArgsD, group(nest(concat(break(), ResultD), ?INDENT, break))),
        group(break(nest(Body, ?INDENT, break), <<"">>, <<")">>))
    end).

clauses_to_algebra([Clause | _] = Clauses) ->
    ClausesD = fold_clauses_to_algebra(Clauses),
    HasBreak = clause_has_break(Clause),
    group(concat(maybe_force_breaks(HasBreak), ClausesD)).

fold_clauses_to_algebra([Clause | [{comment, _, _} | _] = Comments]) ->
    line(expr_to_algebra(Clause), comments_to_algebra(Comments));
fold_clauses_to_algebra([Clause]) ->
    expr_to_algebra(Clause);
fold_clauses_to_algebra([Clause | Clauses]) ->
    line(concat(expr_to_algebra(Clause), <<";">>), fold_clauses_to_algebra(Clauses)).

clause_has_break({clause, _Meta, empty, Guards, [Body | _]}) ->
    has_break_between(Guards, Body);
clause_has_break({clause, _Meta, Head, _Guards, [Body | _]}) ->
    has_break_between(Head, Body);
clause_has_break({spec_clause, _Meta, Head, [Body], _Guards}) ->
    has_break_between(Head, Body);
clause_has_break({macro_call, _Meta, _Name, _Args}) ->
    false.

clause_to_algebra({clause, _Meta, Head, empty, Body}) ->
    HeadD = expr_to_algebra(Head),
    BodyD = block_to_algebra(Body),
    space(HeadD, nest(break(<<"->">>, BodyD), ?INDENT));
clause_to_algebra({clause, _Meta, empty, Guards, Body}) ->
    GuardsD = expr_to_algebra(Guards),
    BodyD = block_to_algebra(Body),
    space(GuardsD, nest(break(<<"->">>, BodyD), ?INDENT));
clause_to_algebra({clause, Meta, Head, Guards, Body}) ->
    HeadD = expr_to_algebra(Head),
    GuardsD = expr_to_algebra(Guards),
    BodyD = block_to_algebra(Meta, Body),

    Nested = fun (Doc) -> nest(concat(break(<<" ">>), Doc), ?INDENT) end,
    concat(
        space(HeadD, <<"when">>),
        group(concat(Nested(GuardsD), break(<<" ">>), <<"->">>)),
        Nested(BodyD)
    );
clause_to_algebra({spec_clause, Meta, Head, Body, empty}) ->
    clause_to_algebra({clause, Meta, Head, empty, Body});
clause_to_algebra({spec_clause, _Meta, Head, [Body], Guards}) ->
    HeadD = expr_to_algebra(Head),
    GuardsD = expr_to_algebra(Guards),
    BodyD = expr_to_algebra(Body),

    Nested = fun (Doc) -> nest(concat(break(<<" ">>), Doc), ?INDENT) end,
    concat(
        space(HeadD, <<"->">>),
        group(concat(Nested(BodyD), break(<<" ">>), <<"when">>)),
        Nested(GuardsD)
    ).

guard_to_algebra(Guards, Separator) ->
    GuardsD = lists:map(fun expr_to_algebra/1, Guards),
    Doc = fold_doc(fun (GuardD, Acc) -> break(concat(GuardD, Separator), Acc) end, GuardsD),
    group(Doc).

% %% Because the spec syntax is different from regular function syntax,
% %% in the general case we have to indent them differently, but with just
% %% one clause we can indent them like functions, which seems more natural.
single_clause_spec_to_algebra(Name, {spec_clause, CMeta, Head, Body, Guards}) ->
    {args, AMeta, Args} = Head,
    clauses_to_algebra([{spec_clause, CMeta, {call, AMeta, Name, Args}, Body, Guards}]).

receive_after_to_algebra(Expr, Body) ->
    ExprD = do_expr_to_algebra(Expr),
    BodyD = block_to_algebra(Body),

    HeadD = concat([<<"after ">>, ExprD, <<" ->">>]),
    Doc = group(nest(break(HeadD, BodyD), ?INDENT)),
    combine_comments(element(2, Expr), Doc).

try_to_algebra(Exprs, OfClauses, CatchClauses, After) ->
    Clauses =
        [try_of_block(Exprs, OfClauses)] ++
            [try_catch_to_algebra(CatchClauses) || CatchClauses =/= []] ++
            [try_after_to_algebra(After) || After =/= []] ++
            [<<"end">>],
    concat(force_breaks(), (group(fold_doc(fun erlfmt_algebra:line/2, Clauses)))).

try_catch_to_algebra(Clauses) ->
    group(nest(line(<<"catch">>, clauses_to_algebra(Clauses)), ?INDENT)).

try_after_to_algebra(Exprs) ->
    ExprsD = block_to_algebra(Exprs),
    group(nest(break(<<"after">>, ExprsD), ?INDENT)).

try_of_block(Exprs, OfClauses) ->
    ExprsD = block_to_algebra(Exprs),

    case OfClauses of
        [] ->
            group(nest(break(<<"try">>, ExprsD), ?INDENT));
        _ ->
            concat(
                surround(<<"try">>, <<" ">>, ExprsD, <<" ">>, <<"of">>),
                nest(concat(line(), clauses_to_algebra(OfClauses)), ?INDENT)
            )
    end.

has_break_between(Left, Right) ->
    erlfmt_scan:get_end_line(Left) < erlfmt_scan:get_line(Right).

has_empty_line_between(Left, Right) ->
    erlfmt_scan:get_end_line(Left) + 1 < erlfmt_scan:get_line(Right).

has_inner_break(Outer, Inner) ->
    erlfmt_scan:get_inner_line(Outer) < erlfmt_scan:get_line(Inner).

is_next_break_fits_op(Op, Expr) ->
    lists:member(Op, ?NEXT_BREAK_FITS_OPS) andalso
        is_next_break_fits(Expr, [call, macro_call]).

is_next_break_fits(Expr) -> is_next_break_fits(Expr, []).

is_next_break_fits(Expr, Extra) ->
    lists:member(element(1, Expr), Extra ++ ?NEXT_BREAK_FITS) andalso
        has_no_comments_or_parens(Expr).

has_no_comments_or_parens(Meta) ->
    {Pre, Post} = comments(Meta),
    Pre =:= [] andalso Post =:= [] andalso not erlfmt_scan:get_anno(parens, Meta, false).

maybe_wrap_in_parens(Meta, Doc) ->
    case erlfmt_scan:get_anno(parens, Meta, false) of
        true -> concat(<<"(">>, Doc, <<")">>);
        false -> Doc
    end.

combine_comments(Meta, Doc) ->
    {Pre, Post} = comments(Meta),
    combine_post_comments(Post, combine_pre_comments(Pre, Meta, Doc)).

combine_pre_comments([], _Meta, Doc) ->
    Doc;
combine_pre_comments(Comments, Meta, Doc) ->
    case
        erlfmt_scan:get_end_line(lists:last(Comments)) + 1 <
            erlfmt_scan:get_inner_line(Meta)
    of
        true -> concat([force_breaks(), comments_to_algebra(Comments), line(2), Doc]);
        false -> concat([force_breaks(), comments_to_algebra(Comments), line(), Doc])
    end.

combine_post_comments([], Doc) ->
    Doc;
combine_post_comments(Comments, Doc) ->
    concat(force_breaks(), line(Doc, comments_to_algebra(Comments))).

comments_to_algebra(Comments) ->
    CommentsD = lists:map(fun comment_to_algebra/1, Comments),
    fold_doc(fun (C, Acc) -> concat(C, line(2), Acc) end, CommentsD).

comment_to_algebra({comment, _Meta, Lines}) ->
    LinesD = lists:map(fun erlfmt_algebra:string/1, Lines),
    fold_doc(fun erlfmt_algebra:line/2, LinesD).

comments(Meta) ->
    {erlfmt_scan:get_anno(pre_comments, Meta, []),
        erlfmt_scan:get_anno(post_comments, Meta, [])}.
