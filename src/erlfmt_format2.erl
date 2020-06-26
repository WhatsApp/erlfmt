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
-module(erlfmt_format2).

-include("erlfmt.hrl").

-export([to_algebra/1, comments/1]).

-import(erlfmt_algebra2, [
    group/1,
    group/2,
    format/2,
    string/1,
    empty/0,
    break/0,
    break/1,
    flex_break/1,
    flex_break/0,
    glue/2,
    glue/3,
    flex_glue/2,
    flex_glue/3,
    space/2,
    nest/2,
    nest/3,
    line/0,
    line/2,
    concat/1,
    concat/2,
    collapse_lines/1,
    force_unfit/1,
    next_break_fits/1,
    next_break_fits/2,
    container_doc/3,
    container_doc/4,
    fold_doc/2,
    concat_to_last_group/2
]).

-define(INDENT, 4).

-define(NEXT_BREAK_FITS, [map, list, record, block, 'fun']).

-define(NEXT_BREAK_FITS_OPS, ['=', '::']).

-spec to_algebra(erlfmt_parse:abstract_form()) -> erlfmt_algebra2:doc().
% to_algebra({function, Meta, Clauses}) ->
%     Doc = concat(clauses_to_algebra(Clauses), string(".")),
%     combine_comments(Meta, Doc);
% to_algebra({attribute, Meta, Name, []}) ->
%     Doc = wrap(string("-"), expr_to_algebra(Name), string(".")),
%     combine_comments(Meta, Doc);
% to_algebra({attribute, Meta, {atom, _, define}, [Define | Body]}) ->
%     HeadD = concat(string("-define("), expr_to_algebra(Define)),
%     BodyD = block_to_algebra(Body),
%     EndD = string(")."),
%     case next_break_fits(hd(Body)) of
%         true ->
%             Doc = prepend_comma_space(HeadD, concat(BodyD, EndD)),
%             combine_comments(Meta, Doc);
%         false ->
%             SingleBody = concat(document_single_line(BodyD), EndD),
%             Doc = document_choice(
%                 comma_space(HeadD, SingleBody),
%                 wrap_nested(concat(HeadD, string(",")), BodyD, EndD)
%             ),
%             combine_comments(Meta, Doc)
%     end;
% to_algebra({attribute, Meta, {atom, _, RawName} = Name, [Value]})
%         when RawName =:= type; RawName =:= opaque; RawName =:= spec; RawName =:= callback ->
%     NameD = wrap(string("-"), expr_to_algebra(Name), string(" ")),
%     ValueD = expr_to_algebra(Value),
%     Doc = wrap_prepend(NameD, ValueD, string(".")),
%     combine_comments(Meta, Doc);
% to_algebra({attribute, Meta, {atom, _, record}, [Name, {tuple, TMeta, Values}]}) ->
%     HeadD = wrap(string("-record("), expr_to_algebra(Name), string(",")),
%     case no_comments_or_parens(TMeta) of
%         true ->
%             Prefix = concat(HeadD, string(" {")),
%             Doc = container_to_algebra(TMeta, Values, Prefix, string("}).")),
%             combine_comments(Meta, Doc);
%         false ->
%             Doc = line(
%                 combine_nested(HeadD, expr_to_algebra({tuple, TMeta, Values})),
%                 string(").")
%             ),
%             combine_comments(Meta, Doc)
%     end;
% to_algebra({attribute, Meta, Name, Values}) ->
%     Prefix = wrap(string("-"), expr_to_algebra(Name), string("(")),
%     Doc = container_to_algebra(Meta, Values, Prefix, string(").")),
%     combine_comments(Meta, Doc);
to_algebra(Expr) ->
    Meta = element(2, Expr),
    Doc = do_expr_to_algebra(Expr),
    case maps:get(dot, Meta, false) of
        true ->
            combine_comments(Meta, concat(Doc, <<".">>));
        false ->
            combine_comments(Meta, Doc)
    end.

-spec expr_to_algebra(erlfmt_parse:abstract_expr()) -> erlfmt_algebra2:doc().
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
% do_expr_to_algebra({tuple, Meta, Values}) ->
%     fill_container_to_algebra(Meta, Values, string("{"), string("}"));
do_expr_to_algebra({list, Meta, Values}) ->
    container(Meta, Values, <<"[">>, <<"]">>);
do_expr_to_algebra({cons, _, Head, Tail}) ->
    cons_to_algebra(Head, Tail);
% do_expr_to_algebra({bin, Meta, Values}) ->
%     fill_container_to_algebra(Meta, Values, string("<<"), string(">>"));
% do_expr_to_algebra({bin_element, _Meta, Expr, Size, Types}) ->
%     bin_element_to_algebra(Expr, Size, Types);
% do_expr_to_algebra({map, Meta, Values}) ->
%     container_to_algebra(Meta, Values, string("#{"), string("}"));
% do_expr_to_algebra({map, Meta, Expr, Values}) ->
%     Prefix = concat(expr_to_algebra(Expr), string("#{")),
%     container_to_algebra(Meta, Values, Prefix, string("}"));
% do_expr_to_algebra({map_field_assoc, _Meta, Key, Value}) ->
%     field_to_algebra("=>", Key, Value);
% do_expr_to_algebra({map_field_exact, _Meta, Key, Value}) ->
%     field_to_algebra(":=", Key, Value);
% do_expr_to_algebra({record, Meta, Name, Values}) ->
%     Prefix = concat(record_name_to_algebra(Meta, Name), string("{")),
%     container_to_algebra(Meta, Values, Prefix, string("}"));
% do_expr_to_algebra({record, Meta, Expr, Name, Values}) ->
%     PrefixName = concat(record_name_to_algebra(Meta, Name), string("{")),
%     Prefix = concat(expr_to_algebra(Expr), PrefixName),
%     container_to_algebra(Meta, Values, Prefix, string("}"));
% do_expr_to_algebra({record_field, _Meta, Key, Value}) ->
%     field_to_algebra("=", Key, Value);
% do_expr_to_algebra({record_index, Meta, Name, Key}) ->
%     record_access_to_algebra(Meta, Name, Key);
% do_expr_to_algebra({record_field, _Meta, Name}) ->
%     expr_to_algebra(Name);
% do_expr_to_algebra({record_field, Meta, Expr, Name, Key}) ->
%     Access = record_access_to_algebra(Meta, Name, Key),
%     concat(expr_to_algebra(Expr), Access);
% do_expr_to_algebra({lc, _Meta, Expr, LcExprs}) ->
%     ExprD = expr_to_algebra(Expr),
%     comprehension_to_algebra(ExprD, LcExprs, string("["), string("]"));
% do_expr_to_algebra({bc, _Meta, Expr, LcExprs}) ->
%     ExprD = expr_to_algebra(Expr),
%     comprehension_to_algebra(ExprD, LcExprs, string("<<"), string(">>"));
% do_expr_to_algebra({generate, _Meta, Left, Right}) ->
%     field_to_algebra("<-", Left, Right);
% do_expr_to_algebra({b_generate, _Meta, Left, Right}) ->
%     field_to_algebra("<=", Left, Right);
% do_expr_to_algebra({call, Meta, Name, Args}) ->
%     Prefix = concat(expr_to_algebra(Name), string("(")),
%     container_to_algebra(Meta, Args, Prefix, string(")"));
% do_expr_to_algebra({macro_call, _Meta, Name, none}) ->
%     concat(string("?"), expr_to_algebra(Name));
% do_expr_to_algebra({macro_call, Meta, Name, Args}) ->
%     Prefix = wrap(string("?"), expr_to_algebra(Name), string("(")),
%     container_to_algebra(Meta, Args, Prefix, string(")"));
% do_expr_to_algebra({macro_string, _Meta, Name}) ->
%     concat(string("??"), expr_to_algebra(Name));
% do_expr_to_algebra({remote, _Meta, Left, Right}) ->
%     combine_sep(expr_to_algebra(Left), ":", expr_to_algebra(Right));
% do_expr_to_algebra({block, _Meta, Exprs}) ->
%     wrap_nested(string("begin"), block_to_algebra(Exprs), string("end"));
% do_expr_to_algebra({'fun', _Meta, Expr}) ->
%     fun_to_algebra(Expr);
% do_expr_to_algebra({'case', _Meta, Expr, Clauses}) ->
%     Prefix = wrap(string("case "), expr_to_algebra(Expr), string(" of")),
%     wrap_nested(Prefix, clauses_to_algebra(Clauses), string("end"));
% do_expr_to_algebra({'receive', _Meta, Clauses}) ->
%     wrap_nested(
%         string("receive"),
%         clauses_to_algebra(Clauses),
%         string("end")
%     );
% do_expr_to_algebra({'receive', _Meta, [], AfterExpr, AfterBody}) ->
%     AfterD = receive_after_to_algebra(AfterExpr, AfterBody),
%     line(
%         string("receive"),
%         line(AfterD, string("end"))
%     );
% do_expr_to_algebra({'receive', _Meta, Clauses, AfterExpr, AfterBody}) ->
%     AfterD = receive_after_to_algebra(AfterExpr, AfterBody),
%     Suffix = line(AfterD, string("end")),
%     wrap_nested(string("receive"), clauses_to_algebra(Clauses), Suffix);
% do_expr_to_algebra({'try', _Meta, Exprs, OfClauses, CatchClauses, After}) ->
%     try_to_algebra(Exprs, OfClauses, CatchClauses, After);
% do_expr_to_algebra({'if', _Meta, Clauses}) ->
%     wrap_nested(string("if"), clauses_to_algebra(Clauses), string("end"));
% do_expr_to_algebra({spec, _Meta, Name, [SingleClause]}) ->
%     single_clause_spec_to_algebra(Name, SingleClause);
% do_expr_to_algebra({spec, _Meta, Name, Clauses}) ->
%     combine_nested(expr_to_algebra(Name), clauses_to_algebra(Clauses));
% do_expr_to_algebra({'...', _Meta}) ->
%     string("...");
% do_expr_to_algebra({bin_size, _Meta, Left, Right}) ->
%     wrap(expr_to_algebra(Left), string("*"), expr_to_algebra(Right));
% do_expr_to_algebra({guard_or, _Meta, Guards}) ->
%     element(2, do_guard_or_to_algebra_pair(Guards, "")).
do_expr_to_algebra(Other) ->
    error(unsupported, [Other]).

comma_space(D1, D2) -> concat([D1, <<", ">>, D2]).

dash(D1, D2) -> concat([D1, <<"-">>, D2]).

semi_space(D1, D2) -> concat([D1, <<"; ">>, D2]).

colon(D1, D2) -> concat([D1, <<":">>, D2]).

comma_newline(D1, D2) ->
    line(concat(D1, <<",">>), D2).

% combine_comma_double_newline(D1, D2) ->
%     Left = document_flush(document_flush(concat(D1, string(",")))),
%     concat(Left, D2).

% combine_semi_newline(D1, D2) ->
%     concat(document_flush(concat(D1, string(";"))), D2).

% combine_nested(Head, Doc) -> combine_nested(Head, Doc, ?INDENT).

% combine_nested(Head, Doc, Indent) ->
%     line(Head, concat(document_spaces(Indent), Doc)).

% prepend_space(D1, D2) ->
%     wrap_prepend(D1, string(" "), D2).

% prepend_comma_space(D1, D2) ->
%     wrap_prepend(D1, string(", "), D2).

wrap(Left, Doc, Right) ->
    concat(Left, concat(Doc, Right)).

% wrap_prepend(Left, Doc, Right) ->
%     document_prepend(Left, document_prepend(Doc, Right)).

wrap_in_parens(Doc) ->
    concat([<<"(">>, Doc, <<")">>]).

% wrap_nested(Left, Doc, Right) ->
%     Nested = concat(document_spaces(?INDENT), Doc),
%     line(Left, line(Nested, Right)).

surround(Left, Doc, Right) ->
    group(glue(nest(glue(Left, <<"">>, Doc), ?INDENT, break), <<"">>, Right)).

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
            group(fold_doc(fun erlfmt_algebra2:line/2, ValuesD));
        false ->
            group(fold_doc(fun erlfmt_algebra2:glue/2, ValuesD))
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
    concat([expr_to_algebra(Left), <<"..">>, expr_to_algebra(Right)]);
binary_op_to_algebra('/', _Meta, {atom, _, _} = Left, {integer, _, _} = Right) ->
    %% special-case the foo/N syntax in attributes, division with a literal atom
    %% doesn't make sense in normal code, so it's safe to apply it everywhere
    concat([expr_to_algebra(Left), <<"/">>, expr_to_algebra(Right)]);
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

    Doc = with_next_break_fits(IsNextBreakFits, RightD, fun(R) ->
        RightOpD = nest(glue(concat(<<" ">>, OpD), group(R)), Indent, break),
        concat(group(LeftD),group(maybe_force_unfit(HasBreak, RightOpD)))
    end),

    combine_comments(Meta, maybe_wrap_in_parens(Meta, Doc)).

maybe_force_unfit(true, Doc) -> force_unfit(Doc);
maybe_force_unfit(false, Doc) -> Doc.

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

% fill_container_to_algebra(_Meta, [], Left, Right) ->
%     concat(Left, Right);
% fill_container_to_algebra(#{newline := true}, Values, Left, Right) ->
%     VerticalD = container_vertical_values_to_algebra(Values),
%     wrap_nested(Left, VerticalD, Right);
% fill_container_to_algebra(_Meta, [Value], Left, Right) ->
%     wrap_prepend(Left, expr_to_algebra(Value), Right);
% fill_container_to_algebra(_Meta, [Value | Values], Left, Right) ->
%     ValueD = concat(expr_to_algebra(Value), string(", ")),
%     Doc = fill_container_values_to_algebra(ValueD, Values),
%     wrap_prepend(Left, Doc, Right).

% fill_container_values_to_algebra(Acc0, [Expr]) ->
%     ExprD = expr_to_algebra(Expr),
%     Doc = document_choice(
%         concat(Acc0, document_single_line(ExprD)),
%         combine_nested(Acc0, ExprD)
%     ),
%     case next_break_fits(Expr) of
%         true ->
%             document_choice(document_prepend(document_single_line(Acc0), ExprD), Doc);
%         false ->
%             Doc
%     end;
% fill_container_values_to_algebra(Acc0, [Expr | Rest]) ->
%     ExprD = concat(expr_to_algebra(Expr), string(", ")),
%     SingleExprD = document_single_line(ExprD),
%     Acc = document_choice(
%         concat(Acc0, SingleExprD),
%         document_choice(
%             combine_nested(Acc0, SingleExprD),
%             combine_nested(Acc0, document_flush(ExprD))
%         )
%     ),
%     fill_container_values_to_algebra(Acc, Rest).

container(_Meta, [], Left, Right) ->
    concat(Left, Right);
container(Meta, [Value | _] = Values, Left, Right) ->
    ValuesD = container_exprs_to_algebra(Values, no_fits),
    case has_inner_break(Meta, Value) of
        true -> 
            Doc = fold_doc(fun (D, Acc) -> line(concat_to_last_group(D, <<",">>), Acc) end, ValuesD),
            surround(Left, force_unfit(Doc), Right);
        false ->
            Doc = fold_doc(fun (D, Acc) -> glue(concat_to_last_group(D, <<",">>), Acc) end, ValuesD),
            surround(Left, Doc, Right)
    end.

container_exprs_to_algebra([{comment, _, _} | _] = Comments, _LastBreak) ->
    [comments_to_algebra(Comments)];
container_exprs_to_algebra([Value | [{comment, _, _} | _] = Comments], _LastBreak) ->
    [line(expr_to_algebra(Value), comments_to_algebra(Comments))];
container_exprs_to_algebra([Value], fits) ->
    case is_next_break_fits(Value) of
        true -> [next_break_fits(expr_to_algebra(Value), enabled)];
        false -> [expr_to_algebra(Value)]
    end;
container_exprs_to_algebra([Value], no_fits) ->
    [expr_to_algebra(Value)];
container_exprs_to_algebra([Value | Values], LastBreak) ->
    [expr_to_algebra(Value) | container_exprs_to_algebra(Values, LastBreak)].

% container_to_algebra(_Meta, [], Left, Right) ->
%     concat(Left, Right);
% container_to_algebra(Meta, Values, Left, Right) ->
%     {Single, Multi} = container_to_algebra_pair(Meta, Values, Left, Right),
%     document_choice(Single, Multi).

% container_to_algebra_pair(_, [], Left, Right) ->
%     Doc = concat(Left, Right),
%     {Doc, Doc};
% container_to_algebra_pair(#{newline := true}, Values, Left, Right) ->
%     VerticalD = container_vertical_values_to_algebra(Values),
%     {document_fail(), wrap_nested(Left, VerticalD, Right)};
% container_to_algebra_pair(_, Values, Left, Right) ->
%     {Horizontal, LastFits, Vertical} = container_values_to_algebra_pair(Values),
%     HorizontalD = wrap(Left, Horizontal, Right),
%     VerticalD = document_choice(
%         wrap_prepend(Left, LastFits, Right),
%         wrap_nested(Left, Vertical, Right)
%     ),
%     {HorizontalD, VerticalD}.

% %% standalone comments are always trailing, but can appear as only expressions
% container_values_to_algebra_pair([{comment, _, _} | _] = Comments) ->
%     {document_fail(), document_fail(), comments_to_algebra(Comments)};
% container_values_to_algebra_pair([Expr | [{comment, _, _} | _] = Comments]) ->
%     CommentsD = comments_to_algebra(Comments),
%     ExprD = expr_to_algebra(Expr),
%     {document_fail(), document_fail(), line(ExprD, CommentsD)};
% container_values_to_algebra_pair([Expr]) ->
%     ExprD = expr_to_algebra(Expr),
%     case next_break_fits(Expr) of
%         true -> {document_single_line(ExprD), ExprD, ExprD};
%         false -> {document_single_line(ExprD), document_fail(), ExprD}
%     end;
% container_values_to_algebra_pair([Expr | Rest]) ->
%     ExprD = expr_to_algebra(Expr),
%     SingleExprD = document_single_line(ExprD),
%     {RestSingle, RestLastFits, RestMulti} = container_values_to_algebra_pair(Rest),
%     Single = comma_space(SingleExprD, RestSingle),
%     LastFits = prepend_comma_space(SingleExprD, RestLastFits),
%     Multi = comma_newline(ExprD, RestMulti),
%     {Single, LastFits, Multi}.

% container_vertical_values_to_algebra([{comment, _, _} | _] = Comments) ->
%     comments_to_algebra(Comments);
% container_vertical_values_to_algebra([Expr | [{comment, _, _} | _] = Comments]) ->
%     line(expr_to_algebra(Expr), comments_to_algebra(Comments));
% container_vertical_values_to_algebra([Expr]) ->
%     expr_to_algebra(Expr);
% container_vertical_values_to_algebra([Expr | Rest]) ->
%     comma_newline(
%         expr_to_algebra(Expr),
%         container_vertical_values_to_algebra(Rest)
%     ).

cons_to_algebra(Head, Tail) ->
    HeadD = expr_to_algebra(Head),
    TailD = concat(string("| "), expr_to_algebra(Tail)),
    glue(HeadD, TailD).

% bin_element_to_algebra(Expr, Size, Types) ->
%     Docs =
%         [expr_to_algebra(Expr)] ++
%             [bin_size_to_algebra(Size) || Size =/= default] ++
%             [bin_types_to_algebra(Types) || Types =/= default],
%     document_reduce(fun erlfmt_algebra:concat/2, Docs).

% bin_size_to_algebra(Expr) ->
%     concat(string(":"), expr_to_algebra(Expr)).

% bin_types_to_algebra(Types) ->
%     TypesD = lists:map(fun expr_to_algebra/1, Types),
%     concat(string("/"), document_reduce(fun dash/2, TypesD)).

% record_access_to_algebra(Meta, Name, Key) ->
%     NameD = record_name_to_algebra(Meta, Name),
%     KeyD = expr_to_algebra(Key),
%     DotD = string("."),
%     concat(NameD, concat(DotD, KeyD)).

% record_name_to_algebra(Meta, Name) ->
%     %% Differentiate between #?FOO{} and ?FOO{}
%     case erlfmt_scan:get_anno(macro_record, Meta, false) of
%         true -> expr_to_algebra(Name);
%         false -> concat(string("#"), expr_to_algebra(Name))
%     end.

% field_to_algebra(Op, Key, Value) ->
%     KeyD = expr_to_algebra(Key),
%     ValueD = expr_to_algebra(Value),
%     KeyOpD = space(KeyD, string(Op)),

%     case next_break_fits(Value, [call, macro_call]) of
%         true ->
%             document_choice(
%                 prepend_space(KeyOpD, ValueD),
%                 combine_nested(KeyOpD, ValueD)
%             );
%         false ->
%             document_choice(
%                 space(KeyOpD, document_single_line(ValueD)),
%                 combine_nested(KeyOpD, ValueD)
%             )
%     end.

% comprehension_to_algebra(ExprD, LcExprs, Left, Right) ->
%     PipesD = string("|| "),
%     {LcExprsSingleD, _LcExprsLastFitsD, LcExprsMultiD} =
%         container_values_to_algebra_pair(LcExprs),
%     LcExprsD = document_choice(LcExprsSingleD, LcExprsMultiD),

%     SingleLine = space(
%         document_single_line(ExprD),
%         concat(PipesD, LcExprsSingleD)
%     ),
%     Multiline = document_choice(
%         space(ExprD, concat(PipesD, LcExprsSingleD)),
%         line(ExprD, concat(PipesD, LcExprsD))
%     ),

%     document_choice(
%         wrap(Left, SingleLine, Right),
%         wrap_nested(Left, Multiline, Right)
%     ).

% %% standalone comments are always trailing other expressions
% block_to_algebra([Expr | [{comment, _, _} | _] = Comments]) ->
%     line(expr_to_algebra(Expr), comments_to_algebra(Comments));
% block_to_algebra([Expr]) ->
%     expr_to_algebra(Expr);
% block_to_algebra([Expr | [Next | _] = Rest]) ->
%     ExprD = expr_to_algebra(Expr),
%     RestD = block_to_algebra(Rest),
%     case erlfmt_scan:get_end_line(Expr) + 1 < erlfmt_scan:get_line(Next) of
%         true -> combine_comma_double_newline(ExprD, RestD);
%         false -> comma_newline(ExprD, RestD)
%     end.

% fun_to_algebra({function, _Anno, Name, Arity}) ->
%     concat([
%         string("fun "),
%         expr_to_algebra(Name),
%         string("/"),
%         expr_to_algebra(Arity)
%     ]);
% fun_to_algebra({function, _Anno, Mod, Name, Arity}) ->
%     concat([
%         string("fun "),
%         expr_to_algebra(Mod),
%         string(":"),
%         expr_to_algebra(Name),
%         string("/"),
%         expr_to_algebra(Arity)
%     ]);
% fun_to_algebra({clauses, _Anno, [Clause]}) ->
%     FunD = string("fun "),
%     {Single, Multi} = clause_to_algebra_pair(Clause),
%     MultiD = line(document_prepend(FunD, Multi), string("end")),
%     case erlfmt_scan:get_anno(newline, Clause, false) of
%         true -> MultiD;
%         false -> document_choice(wrap(FunD, Single, string(" end")), MultiD)
%     end;
% fun_to_algebra({clauses, _Anno, Clauses}) ->
%     ClausesD = clauses_to_algebra(Clauses),
%     document_choice(
%         wrap(string("fun "), document_single_line(ClausesD), string(" end")),
%         wrap_nested(string("fun"), ClausesD, string("end"))
%     );
% fun_to_algebra(type) ->
%     string("fun()");
% fun_to_algebra({type, Anno, Args, Result}) ->
%     ResultD = expr_to_algebra(Result),
%     ArgsD = container_to_algebra(Anno, Args, string("("), string(") ->")),
%     SingleLine =
%         case next_break_fits(Result) of
%             true -> prepend_space(ArgsD, ResultD);
%             false -> space(ArgsD, document_single_line(ResultD))
%         end,
%     Body = document_choice(SingleLine, combine_nested(ArgsD, ResultD)),
%     wrap(string("fun("), Body, string(")")).

% clauses_to_algebra([{_, #{newline := true}, _, _, _} | _] = Clauses) ->
%     {_Single, Multi} = clauses_to_algebra_pair(Clauses),
%     Multi;
% clauses_to_algebra(Clauses) ->
%     {Single, Multi} = clauses_to_algebra_pair(Clauses),
%     document_choice(Single, Multi).

% %% standalone comments are always trailing other expressions
% clauses_to_algebra_pair([Clause | [{comment, _, _} | _] = Comments]) ->
%     CommentsD = comments_to_algebra(Comments),
%     {SingleClauseD, MultiClauseD} = clause_to_algebra_pair(Clause),
%     {line(SingleClauseD, CommentsD), line(MultiClauseD, CommentsD)};
% clauses_to_algebra_pair([Clause]) ->
%     clause_to_algebra_pair(Clause);
% clauses_to_algebra_pair([Clause | Rest]) ->
%     {SingleClauseD, MultiClauseD} = clause_to_algebra_pair(Clause),
%     {RestSingle, RestMulti} = clauses_to_algebra_pair(Rest),
%     Single = combine_semi_newline(SingleClauseD, RestSingle),
%     Multi = combine_semi_newline(MultiClauseD, RestMulti),
%     {Single, Multi}.

% clause_to_algebra_pair({Clause, Meta, _, _, _} = Node)
%         when Clause =:= clause; Clause =:= spec_clause ->
%     %% clause nodes only have precomments
%     {Pre, []} = comments(Meta),
%     {Single, Multi} = do_clause_to_algebra_pair(Node),
%     {combine_pre_comments(Pre, Single), combine_pre_comments(Pre, Multi)};
% clause_to_algebra_pair({macro_call, _, _, _} = Expr) ->
%     %% It's possible the entire clause is defined inside of a macro call
%     ExprD = expr_to_algebra(Expr),
%     {document_single_line(ExprD), ExprD}.

% do_clause_to_algebra_pair({clause, _Meta, empty, Guards, Body}) ->
%     BodyD = block_to_algebra(Body),
%     SingleBodyD = document_single_line(BodyD),
%     {SingleGuardsD, _InlineGuardsD, GuardsD} = guard_or_to_algebra_pair(Guards, ""),

%     SingleD = wrap(SingleGuardsD, string(" -> "), SingleBodyD),
%     MultiD = combine_nested(concat(GuardsD, string(" ->")), BodyD),

%     {SingleD, MultiD};
% %% If there are no guards, spec is the same as regular clauses
% do_clause_to_algebra_pair({spec_clause, Meta, Head, Body, empty}) ->
%     do_clause_to_algebra_pair({clause, Meta, Head, empty, Body});
% do_clause_to_algebra_pair({spec_clause, _Meta, Head, [Body], Guards}) ->
%     {SingleHeadD, HeadD} = clause_head_to_algebra(Head),
%     {SingleGuardsD, _InlineGuardsD, GuardsD} = guard_or_to_algebra_pair(Guards, "when "),
%     BodyD = expr_to_algebra(Body),
%     SingleBodyD = document_single_line(BodyD),

%     SingleD =
%         wrap(SingleHeadD, string(" -> "), space(SingleBodyD, SingleGuardsD)),
%     MultiPrefix = document_choice(
%         wrap(SingleHeadD, string(" -> "), SingleBodyD),
%         document_choice(
%             combine_nested(concat(SingleHeadD, string(" ->")), BodyD),
%             wrap(HeadD, string(" -> "), BodyD)
%         )
%     ),
%     MultiD = combine_nested(MultiPrefix, GuardsD),

%     {SingleD, MultiD};
% do_clause_to_algebra_pair({clause, _Meta, Head, empty, Body}) ->
%     {SingleHeadD, HeadD} = clause_head_to_algebra(Head),
%     BodyD = block_to_algebra(Body),
%     SingleBodyD = document_single_line(BodyD),

%     SingleD = wrap(SingleHeadD, string(" -> "), SingleBodyD),
%     MultiPrefix =
%         concat(document_choice(SingleHeadD, HeadD), string(" ->")),
%     MultiD = combine_nested(MultiPrefix, BodyD),

%     {SingleD, MultiD};
% do_clause_to_algebra_pair({clause, _Meta, Head, Guards, Body}) ->
%     {SingleHeadD, HeadD} = clause_head_to_algebra(Head),
%     {SingleGuardsD, InlineGuardsD, GuardsD} = guard_or_to_algebra_pair(Guards, "when "),
%     BodyD = block_to_algebra(Body),
%     SingleBodyD = document_single_line(BodyD),

%     SingleD =
%         space(SingleHeadD, wrap(SingleGuardsD, string(" -> "), SingleBodyD)),
%     MultiPrefix = document_choice(
%         space(document_choice(SingleHeadD, HeadD), InlineGuardsD),
%         combine_nested(SingleHeadD, GuardsD, ?INDENT * 2)
%     ),
%     MultiD = combine_nested(concat(MultiPrefix, string(" ->")), BodyD),

%     {SingleD, MultiD}.

% clause_head_to_algebra({args, Meta, Args}) ->
%     container_to_algebra_pair(Meta, Args, string("("), string(")"));
% clause_head_to_algebra({'catch', _, Args}) ->
%     ArgsD = lists:map(fun expr_to_algebra/1, Args),
%     Doc = document_reduce(fun colon/2, ArgsD),
%     {document_single_line(Doc), Doc};
% clause_head_to_algebra({call, Meta, Name, Args}) ->
%     Prefix = concat(expr_to_algebra(Name), string("(")),
%     container_to_algebra_pair(Meta, Args, Prefix, string(")"));
% clause_head_to_algebra(Expr) ->
%     ExprD = expr_to_algebra(Expr),
%     {document_single_line(ExprD), ExprD}.

% guard_or_to_algebra_pair({guard_or, Meta, Guards}, When) ->
%     case comments(Meta) of
%         {[], []} ->
%             {Single, Multi} = do_guard_or_to_algebra_pair(Guards, When),
%             {Single, Multi, Multi};
%         {Pre, Post} ->
%             {_Single, Multi} = do_guard_or_to_algebra_pair(Guards, When),
%             Doc = combine_pre_comments(Pre, combine_post_comments(Post, Multi)),
%             {document_fail(), document_fail(), Doc}
%     end.

% do_guard_or_to_algebra_pair(Guards, When) ->
%     WhenD = string(When),
%     {SingleLine, MultiLine} =
%         lists:unzip(lists:map(fun guard_and_to_algebra_pair/1, Guards)),
%     SingleLineD = document_reduce(fun semi_space/2, SingleLine),
%     MultiLineD = document_reduce(fun combine_semi_newline/2, MultiLine),
%     {
%         concat(WhenD, SingleLineD),
%         concat(WhenD, document_choice(SingleLineD, MultiLineD))
%     }.

% guard_and_to_algebra_pair({guard_and, Meta, Exprs}) ->
%     ExprsD = lists:map(fun expr_to_algebra/1, Exprs),
%     case comments(Meta) of
%         {[], []} ->
%             SingleLine = lists:map(fun erlfmt_algebra:document_single_line/1, ExprsD),

%             SingleLineD = document_reduce(fun comma_space/2, SingleLine),
%             MultiLineD = document_reduce(fun comma_newline/2, ExprsD),
%             {SingleLineD, document_choice(SingleLineD, MultiLineD)};
%         {Pre, Post} ->
%             Doc = document_reduce(fun comma_newline/2, ExprsD),
%             {document_fail(), combine_pre_comments(Pre, combine_post_comments(Post, Doc))}
%     end.

% %% Because the spec syntax is different from regular function syntax,
% %% in the general case we have to indent them differently, but with just
% %% one clause we can indent them like functions, which seems more natural.
% single_clause_spec_to_algebra(Name, {spec_clause, CMeta, Head, Body, Guards}) ->
%     {args, AMeta, Args} = Head,
%     clauses_to_algebra([{spec_clause, CMeta, {call, AMeta, Name, Args}, Body, Guards}]).

% receive_after_to_algebra(Expr, Body) ->
%     {Pre, []} = comments(element(2, Expr)),
%     ExprD = do_expr_to_algebra(Expr),
%     BodyD = block_to_algebra(Body),

%     HeadD = wrap(string("after "), ExprD, string(" ->")),
%     CommentHeadD = combine_pre_comments(Pre, HeadD),
%     document_choice(
%         space(CommentHeadD, document_single_line(BodyD)),
%         combine_nested(CommentHeadD, BodyD)
%     ).

% try_to_algebra(Exprs, OfClauses, CatchClauses, After) ->
%     Clauses =
%         [try_of_block(Exprs, OfClauses)] ++
%             [try_catch_to_algebra(CatchClauses) || CatchClauses =/= []] ++
%             [try_after_to_algebra(After) || After =/= []] ++
%             [string("end")],

%     document_reduce(fun line/2, Clauses).

% try_catch_to_algebra(Clauses) ->
%     combine_nested(string("catch"), clauses_to_algebra(Clauses)).

% try_after_to_algebra(Exprs) ->
%     AfterD = string("after"),
%     ExprsD = block_to_algebra(Exprs),
%     document_choice(
%         space(AfterD, document_single_line(ExprsD)),
%         combine_nested(AfterD, ExprsD)
%     ).

% try_of_block(Exprs, OfClauses) ->
%     TryD = string("try"),
%     OfD = string("of"),
%     ExprsD = block_to_algebra(Exprs),

%     TrySingle = space(TryD, document_single_line(ExprsD)),
%     TryMulti = combine_nested(TryD, ExprsD),

%     case OfClauses of
%         [] ->
%             document_choice(TrySingle, TryMulti);
%         _ ->
%             combine_nested(
%                 document_choice(
%                     space(TrySingle, OfD),
%                     line(TryMulti, OfD)
%                 ),
%                 clauses_to_algebra(OfClauses)
%             )
%     end.

has_break_between(Left, Right) ->
    erlfmt_scan:get_end_line(Left) < erlfmt_scan:get_line(Right).

has_inner_break(Outer, Inner) ->
    erlfmt_scan:get_line(Outer) < erlfmt_scan:get_line(Inner).

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
        true -> wrap_in_parens(Doc);
        false -> Doc
    end.

combine_comments(Meta, Doc) ->
    {Pre, Post} = comments(Meta),
    combine_post_comments(Post, combine_pre_comments(Pre, Doc)).

combine_pre_comments([], Doc) -> Doc;
combine_pre_comments(Comments, Doc) -> line(comments_to_algebra(Comments), Doc).

combine_post_comments([], Doc) -> Doc;
combine_post_comments(Comments, Doc) -> line(Doc, comments_to_algebra(Comments)).

comments_to_algebra(Comments) ->
    CommentsD = lists:map(fun comment_to_algebra/1, Comments),
    Doc = fold_doc(fun (C, Acc) -> concat([C, line(), line(), Acc]) end, CommentsD),
    force_unfit(Doc).

comment_to_algebra({comment, _Meta, Lines}) ->
    LinesD = lists:map(fun erlfmt_algebra2:string/1, Lines),
    fold_doc(fun erlfmt_algebra2:line/2, LinesD).

comments(Meta) ->
    {erlfmt_scan:get_anno(pre_comments, Meta, []),
        erlfmt_scan:get_anno(post_comments, Meta, [])}.
