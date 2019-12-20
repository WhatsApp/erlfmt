-module(erlfmt_format).

-oncall("whatsapp_erlang").

-typing([dialyzer]).

-export([expr_to_algebra/1, form_to_algebra/1]).

-import(erlfmt_algebra, [
    document_text/1,
    document_spaces/1,
    document_combine/2,
    document_flush/1,
    document_choice/2,
    document_single_line/1,
    document_reduce/2
]).

-import(erl_anno, [text/1]).

-define(IN_RANGE(Value, Low, High), (Value) >= (Low) andalso (Value) =< (High)).
-define(IS_OCT_DIGIT(C), ?IN_RANGE(C, $0, $7)).

-define(INDENT, 4).

%% Thses operators when mixed, force parens on nested operators
-define(MIXED_REQUIRE_PARENS_OPS, ['or', 'and', 'andalso', 'orelse']).

%% These operators always force parens on nested operators
-define(REQUIRE_PARENS_OPS, ['bor', 'band', 'bxor', 'bsl', 'bsr', '++', '--']).

-define(PARENLESS_ATTRIBUTE, [type, opaque, spec, callback]).

-spec form_to_algebra(erlfmt_parse:abstract_form()) -> erlfmt_algebra:document().
form_to_algebra({function, Meta, Clauses}) ->
    Doc = document_combine(clauses_to_algebra(Clauses), document_text(".")),
    combine_comments(Meta, Doc);
form_to_algebra({attribute, Meta, Name, []}) ->
    NameD = document_text(format_atom(text(Meta), Name)),
    Doc = wrap(document_text("-"), NameD, document_text(".")),
    combine_comments(Meta, Doc);
form_to_algebra({attribute, Meta, Name, Values}) ->
    DashD = document_text("-"),
    NameD = document_text(format_atom(text(Meta), Name)),
    case lists:member(Name, ?PARENLESS_ATTRIBUTE) of
        true ->
            [Value] = Values,
            ValueD = expr_to_algebra(Value),
            Doc = document_combine(DashD, combine_space(NameD, document_combine(ValueD, document_text(".")))),
            combine_comments(Meta, Doc);
        false ->
            Prefix = wrap(DashD, NameD, document_text("(")),
            Doc = container_to_algebra(Values, Prefix, document_text(").")),
            combine_comments(Meta, Doc)
    end.

-spec expr_to_algebra(erlfmt_parse:abstract_expr()) -> erlfmt_algebra:document().
expr_to_algebra(Expr) when is_tuple(Expr) ->
    Meta = element(2, Expr),
    combine_comments(Meta, do_expr_to_algebra(Expr));
expr_to_algebra(Other) ->
    do_expr_to_algebra(Other).

parens_expr_to_algebra(Expr) ->
    Meta = element(2, Expr),
    combine_comments(Meta, wrap_in_parens(do_expr_to_algebra(Expr))).

do_expr_to_algebra({integer, Meta, _Value}) ->
    document_text(format_integer(text(Meta)));
do_expr_to_algebra({float, Meta, _Value}) ->
    document_text(format_float(text(Meta)));
do_expr_to_algebra({char, Meta, Value}) ->
    document_text(format_char(text(Meta), Value));
do_expr_to_algebra({atom, Meta, Value}) ->
    document_text(format_atom(text(Meta), Value));
do_expr_to_algebra({string, Meta, Value}) ->
    document_text(format_string(text(Meta), Value));
do_expr_to_algebra({var, Meta, _Value}) ->
    document_text(text(Meta));
do_expr_to_algebra({concat, _Meta, Values0}) ->
    Values = lists:map(fun expr_to_algebra/1, Values0),
    Horizontal = document_reduce(fun combine_space/2, Values),
    Vertical = document_reduce(fun combine_newline/2, Values),
    document_choice(Horizontal, Vertical);
do_expr_to_algebra({op, _Meta, Op, Expr}) ->
    unary_op_to_algebra(Op, Expr);
do_expr_to_algebra({op, _Meta, Op, Left, Right}) ->
    binary_op_to_algebra(Op, Left, Right);
do_expr_to_algebra({typed, _Meta, Left, Right}) ->
    binary_op_to_algebra('::', Left, Right);
do_expr_to_algebra({tuple, _Meta, Values}) ->
    container_to_algebra(Values, document_text("{"), document_text("}"));
do_expr_to_algebra({list, _Meta, Values}) ->
    container_to_algebra(Values, document_text("["), document_text("]"));
do_expr_to_algebra({cons, _, Head, Tail}) ->
    cons_to_algebra(Head, Tail);
do_expr_to_algebra({bin, _Meta, Values}) ->
    container_to_algebra(Values, document_text("<<"), document_text(">>"));
do_expr_to_algebra({bin_element, _Meta, Expr, Size, Types}) ->
    bin_element_to_algebra(Expr, Size, Types);
do_expr_to_algebra({map, _Meta, Values}) ->
    container_to_algebra(Values, document_text("#{"), document_text("}"));
do_expr_to_algebra({map, _Meta, Expr, Values}) ->
    Prefix = document_combine(map_expr_to_algebra(Expr), document_text("#{")),
    container_to_algebra(Values, Prefix, document_text("}"));
do_expr_to_algebra({map_field_assoc, _Meta, Key, Value}) ->
    field_to_algebra("=>", Key, Value);
do_expr_to_algebra({map_field_exact, _Meta, Key, Value}) ->
    field_to_algebra(":=", Key, Value);
do_expr_to_algebra({record, Meta, Name, Values}) ->
    Prefix = document_combine(record_name_to_algebra(Meta, Name), document_text("{")),
    container_to_algebra(Values, Prefix, document_text("}"));
do_expr_to_algebra({record, Meta, Expr, Name, Values}) ->
    PrefixName = document_combine(record_name_to_algebra(Meta, Name), document_text("{")),
    Prefix = document_combine(record_expr_to_algebra(Expr), PrefixName),
    container_to_algebra(Values, Prefix, document_text("}"));
do_expr_to_algebra({record_field, _Meta, Key, Value}) ->
    field_to_algebra("=", Key, Value);
do_expr_to_algebra({record_index, Meta, Name, Key}) ->
    record_access_to_algebra(Meta, Name, Key);
do_expr_to_algebra({record_field, _Meta, Name}) ->
    expr_to_algebra(Name);
do_expr_to_algebra({record_field, Meta, Expr, Name, Key}) ->
    Access = record_access_to_algebra(Meta, Name, Key),
    document_combine(record_expr_to_algebra(Expr), Access);
do_expr_to_algebra({lc, _Meta, Expr, LcExprs}) ->
    ExprD = expr_to_algebra(Expr),
    comprehension_to_algebra(ExprD, LcExprs, document_text("["), document_text("]"));
do_expr_to_algebra({bc, _Meta, Expr, LcExprs}) ->
    ExprD = expr_max_to_algebra(Expr),
    comprehension_to_algebra(ExprD, LcExprs, document_text("<<"), document_text(">>"));
do_expr_to_algebra({generate, _Meta, Left, Right}) ->
    field_to_algebra("<-", Left, Right);
do_expr_to_algebra({b_generate, _Meta, Left, Right}) ->
    field_to_algebra("<=", Left, Right);
do_expr_to_algebra({call, _Meta, Name, Args}) ->
    Prefix = document_combine(expr_max_to_algebra(Name), document_text("(")),
    container_to_algebra(Args, Prefix, document_text(")"));
do_expr_to_algebra({macro_call, Meta, Name, none}) ->
    macro_name_to_algebra(Meta, Name);
do_expr_to_algebra({macro_call, Meta, Name, Args}) ->
    Prefix = document_combine(macro_name_to_algebra(Meta, Name), document_text("(")),
    container_to_algebra(Args, Prefix, document_text(")"));
do_expr_to_algebra({macro_string, _Meta, Name}) ->
    document_combine(document_text("??"), expr_to_algebra(Name));
do_expr_to_algebra({remote, _Meta, Mod, Name}) ->
    wrap(expr_max_to_algebra(Mod), document_text(":"), expr_max_to_algebra(Name));
do_expr_to_algebra({block, _Meta, Exprs}) ->
    wrap_nested(document_text("begin"), block_to_algebra(Exprs), document_text("end"));
do_expr_to_algebra({'fun', _Meta, Expr}) ->
    fun_to_algebra(Expr);
do_expr_to_algebra({'case', _Meta, Expr, Clauses}) ->
    Prefix = wrap(document_text("case "), expr_to_algebra(Expr), document_text(" of")),
    wrap_nested(Prefix, clauses_to_algebra(Clauses), document_text("end"));
do_expr_to_algebra({'receive', _Meta, Clauses}) ->
    wrap_nested(document_text("receive"), clauses_to_algebra(Clauses), document_text("end"));
do_expr_to_algebra({'receive', _Meta, [], AfterExpr, AfterBody}) ->
    AfterD = receive_after_to_algebra(AfterExpr, AfterBody),
    combine_newline(document_text("receive"), combine_newline(AfterD, document_text("end")));
do_expr_to_algebra({'receive', _Meta, Clauses, AfterExpr, AfterBody}) ->
    AfterD = receive_after_to_algebra(AfterExpr, AfterBody),
    Suffix = combine_newline(AfterD, document_text("end")),
    wrap_nested(document_text("receive"), clauses_to_algebra(Clauses), Suffix);
do_expr_to_algebra({'try', _Meta, Exprs, OfClauses, CatchClauses, After}) ->
    try_to_algebra(Exprs, OfClauses, CatchClauses, After);
do_expr_to_algebra({'if', _Meta, Clauses}) ->
    wrap_nested(document_text("if"), clauses_to_algebra(Clauses), document_text("end"));
do_expr_to_algebra({guard, _Meta, Expr, Guard}) ->
    guard_to_algebra(Expr, Guard);
do_expr_to_algebra({spec, _Meta, Name, Clauses}) ->
    document_combine(expr_to_algebra(Name), clauses_to_algebra(Clauses));
do_expr_to_algebra({'...', Meta}) ->
    document_text(text(Meta));
do_expr_to_algebra({bin_size, _Meta, Left, Right}) ->
    wrap(expr_to_algebra(Left), document_text("*"), expr_to_algebra(Right));
do_expr_to_algebra(GuardList) when is_list(GuardList) ->
    element(2, guards_to_algebra_pair(GuardList)).

combine_space(D1, D2) -> combine_sep(D1, " ", D2).

combine_comma_space(D1, D2) -> combine_sep(D1, ", ", D2).

combine_dash(D1, D2) -> combine_sep(D1, "-", D2).

combine_semi_space(D1, D2) -> combine_sep(D1, "; ", D2).

combine_colon(D1, D2) -> combine_sep(D1, ":", D2).

combine_sep(D1, Sep, D2) ->
    document_combine(D1, document_combine(document_text(Sep), D2)).

combine_newline(D1, D2) ->
    document_combine(document_flush(D1), D2).

combine_comma_newline(D1, D2) ->
    document_combine(document_flush(document_combine(D1, document_text(","))), D2).

combine_semi_newline(D1, D2) ->
    document_combine(document_flush(document_combine(D1, document_text(";"))), D2).

combine_all(Docs) ->
    document_reduce(fun erlfmt_algebra:document_combine/2, Docs).

combine_nested(Head, Doc) ->
    combine_newline(Head, document_combine(document_spaces(?INDENT), Doc)).

wrap(Left, Doc, Right) ->
    document_combine(Left, document_combine(Doc, Right)).

wrap_in_parens(Doc) -> wrap(document_text("("), Doc, document_text(")")).

wrap_nested(Left, Doc, Right) ->
    Nested = document_combine(document_spaces(?INDENT), Doc),
    combine_newline(Left, combine_newline(Nested, Right)).

%% TODO: handle underscores once on OTP 23
format_integer([B1, B2, $# | Digits]) -> [B1, B2, $# | string:uppercase(Digits)];
format_integer(Other) -> Other.

%% TODO: handle underscores in int part on OTP 23
format_float(FloatText) ->
    [IntPart, DecimalPart] = string:split(FloatText, "."),
    [IntPart, "." | string:lowercase(DecimalPart)].

format_char("$ ", $\s) -> "$\\s";
format_char("$\\s", $\s) -> "$\\s";
format_char([$$ | String], Value) ->
    [$$ | escape_string_loop(String, [Value], -1)].

format_atom(Text, Atom) ->
    RawString = atom_to_list(Atom),
    case erl_scan:reserved_word(Atom) orelse atom_needs_quotes(RawString) of
        true -> escape_string(Text, RawString, $');
        false -> RawString
    end.

format_string(String, Original) ->
    escape_string(String, Original, $").

unary_op_to_algebra(Op, Expr) ->
    OpD = document_text(atom_to_binary(Op, utf8)),
    ExprD = unary_operand_to_algebra(Op, Expr),
    if
        Op =:= 'not'; Op =:= 'bnot'; Op =:= 'catch' ->
            combine_space(OpD, ExprD);
        true ->
            document_combine(OpD, ExprD)
    end.

binary_op_to_algebra(Op, Left, Right) ->
    binary_op_to_algebra(Op, Left, Right, ?INDENT, inop_prec(Op)).

binary_op_to_algebra(Op, Left, Right, Indent, {PrecL, _, PrecR}) ->
    OpD = document_text(atom_to_binary(Op, utf8)),
    %% X...Y is layed out without spaces, other operators always have spaces around
    Combine =
        if
            Op =:= '...' -> fun erlfmt_algebra:document_combine/2;
            true -> fun combine_space/2
        end,
    %% Propagate indent for left-associative operators,
    %% for right-associative ones document algebra does it for us
    LeftD = binary_operand_to_algebra(Op, Left, Indent, PrecL),
    RightD = binary_operand_to_algebra(Op, Right, 0, PrecR),
    LeftOpD = Combine(LeftD, OpD),
    document_choice(
        Combine(LeftOpD, document_single_line(RightD)),
        combine_newline(LeftOpD, document_combine(document_spaces(Indent), RightD))
    ).

%% not and bnot are nestable without parens, others are not
unary_operand_to_algebra(Op, {op, _, Op, _} = Expr) when Op =:= 'not'; Op =:= 'bnot' ->
    expr_to_algebra(Expr);
unary_operand_to_algebra(_, {op, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
unary_operand_to_algebra(_, {op, _, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
unary_operand_to_algebra(_, Expr) ->
    expr_to_algebra(Expr).

binary_operand_to_algebra(_ParentOp, {op, _, 'catch', _} = Expr, _Indent, _Prec) ->
    parens_expr_to_algebra(Expr);
binary_operand_to_algebra(_ParentOp, {op, _, _, _} = Expr, _Indent, _Prec) ->
    expr_to_algebra(Expr);
binary_operand_to_algebra(ParentOp, {op, Meta, ParentOp, Left, Right}, Indent, Prec) ->
    %% Same operator on correct side - no parens and no repeated nesting
    case inop_prec(ParentOp) of
        {Prec, Prec, _} = Precs ->
            Doc = binary_op_to_algebra(ParentOp, Left, Right, Indent, Precs),
            combine_comments(Meta, Doc);
        {_, Prec, Prec} = Precs ->
            Doc = binary_op_to_algebra(ParentOp, Left, Right, Indent, Precs),
            combine_comments(Meta, Doc);
        Precs ->
            Doc = wrap_in_parens(binary_op_to_algebra(ParentOp, Left, Right, ?INDENT, Precs)),
            combine_comments(Meta, Doc)
    end;
binary_operand_to_algebra(ParentOp, {op, Meta, Op, Left, Right}, _Indent, Prec) ->
    {_, NestedPrec, _} = Precs = inop_prec(Op),
    NeedsParens =
        lists:member(ParentOp, ?REQUIRE_PARENS_OPS) orelse
        (lists:member(ParentOp, ?MIXED_REQUIRE_PARENS_OPS) andalso
            lists:member(Op, ?MIXED_REQUIRE_PARENS_OPS)) orelse
        NestedPrec < Prec,

    case NeedsParens of
        true ->
            Doc = wrap_in_parens(binary_op_to_algebra(Op, Left, Right, ?INDENT, Precs)),
            combine_comments(Meta, Doc);
        false ->
            Doc = binary_op_to_algebra(Op, Left, Right, ?INDENT, Precs),
            combine_comments(Meta, Doc)
    end;
binary_operand_to_algebra(_ParentOp, Expr, _Indent, _Prec) ->
    expr_to_algebra(Expr).

container_to_algebra(Values, Left, Right) ->
    {Single, Multi} = container_to_algebra_pair(Values, Left, Right),
    document_choice(Single, Multi).

container_to_algebra_pair([], Left, Right) ->
    Doc = document_combine(Left, Right),
    {Doc, Doc};
container_to_algebra_pair(Values0, Left, Right) ->
    Values = lists:map(fun expr_to_algebra/1, Values0),
    SingleLine = lists:map(fun erlfmt_algebra:document_single_line/1, Values),

    Horizontal = document_reduce(fun combine_comma_space/2, SingleLine),
    Vertical = document_reduce(fun combine_comma_newline/2, Values),

    Single = wrap(Left, Horizontal, Right),
    Multi = wrap_nested(Left, Vertical, Right),
    {Single, Multi}.

cons_to_algebra(Head, Tail) ->
    HeadD = expr_to_algebra(Head),
    TailD = document_combine(document_text("| "), expr_to_algebra(Tail)),

    document_choice(
        combine_space(document_single_line(HeadD), document_single_line(TailD)),
        combine_newline(HeadD, TailD)
    ).

bin_element_to_algebra(Expr, Size, Types) ->
    Docs =
        [bin_expr_to_algebra(Expr)] ++
        [bin_size_to_algebra(Size) || Size =/= default] ++
        [bin_types_to_algebra(Types) || Types =/= default],
    document_reduce(fun erlfmt_algebra:document_combine/2, Docs).

bin_expr_to_algebra({op, _, Op, _} = Expr) when Op =/= 'catch' -> expr_to_algebra(Expr);
bin_expr_to_algebra(Expr) -> expr_max_to_algebra(Expr).

bin_size_to_algebra(Expr) ->
    document_combine(document_text(":"), expr_max_to_algebra(Expr)).

bin_types_to_algebra(Types) ->
    TypesD = lists:map(fun bin_type_to_algebra/1, Types),
    document_combine(document_text("/"), document_reduce(fun combine_dash/2, TypesD)).

bin_type_to_algebra({Type, Size}) ->
    combine_sep(expr_to_algebra(Type), ":", expr_to_algebra(Size));
bin_type_to_algebra(Type) ->
    expr_to_algebra(Type).

expr_max_to_algebra({op, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra({op, _, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra({map, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra({map, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra({record, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra({record, _, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra({record_field, _, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra({record_index, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra({call, _, _, _} = Expr) ->
    parens_expr_to_algebra(Expr);
expr_max_to_algebra(Expr) ->
    expr_to_algebra(Expr).

map_expr_to_algebra({map, _, _} = Expr) ->
    expr_to_algebra(Expr);
map_expr_to_algebra({map, _, _, _} = Expr) ->
    expr_to_algebra(Expr);
map_expr_to_algebra(Expr) ->
    expr_max_to_algebra(Expr).

record_expr_to_algebra({record, _, _, _} = Expr) ->
    expr_to_algebra(Expr);
record_expr_to_algebra({record, _, _, _, _} = Expr) ->
    expr_to_algebra(Expr);
record_expr_to_algebra({record_field, _, _, _, _} = Expr) ->
    expr_to_algebra(Expr);
record_expr_to_algebra(Expr) ->
    expr_max_to_algebra(Expr).

record_access_to_algebra(Meta, Name, Key) ->
    NameD = record_name_to_algebra(Meta, Name),
    KeyD = expr_to_algebra(Key),
    DotD = document_text("."),
    document_combine(NameD, document_combine(DotD, KeyD)).

record_name_to_algebra(Meta, Name) ->
    %% Differentiate between #?FOO{} and ?FOO{}
    case text(Meta) of
        "#" -> document_combine(document_text("#"), expr_to_algebra(Name));
        "?" -> expr_to_algebra(Name)
    end.

macro_name_to_algebra(Meta, Name) ->
    %% Differentiate between macro calls and definitions
    case text(Meta) of
        "?" -> document_combine(document_text("?"), expr_to_algebra(Name));
        _ -> expr_to_algebra(Name)
    end.

field_to_algebra(Op, Key, Value) ->
    KeyD = expr_to_algebra(Key),
    ValueD = expr_to_algebra(Value),
    KeyOpD = combine_space(KeyD, document_text(Op)),

    document_choice(
        combine_space(KeyOpD, document_single_line(ValueD)),
        combine_nested(KeyOpD, ValueD)
    ).

comprehension_to_algebra(ExprD, LcExprs, Left, Right) ->
    PipesD = document_text("|| "),
    {LcExprsSingleD, LcExprsMultiD} = comprehension_exprs_to_algebra_pair(LcExprs),
    LcExprsD = document_choice(LcExprsSingleD, LcExprsMultiD),

    SingleLine =
        combine_space(document_single_line(ExprD), document_combine(PipesD, LcExprsSingleD)),
    Multiline =
        document_choice(
            combine_space(ExprD, document_combine(PipesD, LcExprsSingleD)),
            combine_newline(ExprD, document_combine(PipesD, LcExprsD))
        ),

    document_choice(
        wrap(Left, SingleLine, Right),
        wrap_nested(Left, Multiline, Right)
    ).

comprehension_exprs_to_algebra_pair(LcExprs0) ->
    LcExprs = lists:map(fun expr_to_algebra/1, LcExprs0),
    SingleLine = lists:map(fun erlfmt_algebra:document_single_line/1, LcExprs),

    Horizontal = document_reduce(fun combine_comma_space/2, SingleLine),
    Vertical = document_reduce(fun combine_comma_newline/2, LcExprs),

    {Horizontal, Vertical}.

%% TODO: insert extra newlines between expressions to preserve grouping.
block_to_algebra(Exprs) ->
    ExprsD = lists:map(fun expr_to_algebra/1, Exprs),
    document_reduce(fun combine_comma_newline/2, ExprsD).

fun_to_algebra({function, Name, Arity}) ->
    combine_all([
        document_text("fun "),
        expr_to_algebra(Name),
        document_text("/"),
        expr_to_algebra(Arity)
    ]);
fun_to_algebra({function, Mod, Name, Arity}) ->
    combine_all([
        document_text("fun "),
        expr_to_algebra(Mod),
        document_text(":"),
        expr_to_algebra(Name),
        document_text("/"),
        expr_to_algebra(Arity)
    ]);
fun_to_algebra({clauses, Clauses}) ->
    ClausesD = clauses_to_algebra(Clauses),
    document_choice(
        wrap(document_text("fun "), document_single_line(ClausesD), document_text(" end")),
        wrap_nested(document_text("fun"), ClausesD, document_text("end"))
    );
fun_to_algebra(type) ->
    document_text("fun()");
fun_to_algebra({type, Args, Result}) ->
    ResultD = document_combine(document_text(") -> "), expr_to_algebra(Result)),
    document_combine(container_to_algebra(Args, document_text("fun(("), ResultD), document_text(")")).

clauses_to_algebra(Clauses) ->
    {SingleClausesD, ClausesD} = lists:unzip(lists:map(fun clause_to_algebra_pair/1, Clauses)),
    document_choice(
        document_reduce(fun combine_semi_newline/2, SingleClausesD),
        document_reduce(fun combine_semi_newline/2, ClausesD)
    ).

clause_to_algebra_pair({clause, Meta, _, _, _, _} = Clause) ->
    %% clause nodes only have precomments
    {Pre, []} = comments(Meta),
    {Single, Multi} = do_clause_to_algebra_pair(Clause),
    {combine_pre_comments(Pre, Single), combine_pre_comments(Pre, Multi)};
clause_to_algebra_pair(Other) ->
    do_clause_to_algebra_pair(Other).


do_clause_to_algebra_pair({clause, _Meta, 'if', [], Guards, Body}) ->
    BodyD = block_to_algebra(Body),
    SingleBodyD = document_single_line(BodyD),
    {SingleGuardsD, GuardsD} = guards_to_algebra_pair(Guards),

    SingleD = wrap(SingleGuardsD, document_text(" -> "), SingleBodyD),
    MultiD = combine_nested(document_combine(GuardsD, document_text(" ->")), BodyD),

    {SingleD, MultiD};
%% If there are no guards, spec is the same as regular clauses
do_clause_to_algebra_pair({clause, _Meta, spec, Args, Guards, [Body]}) when Guards =/= [] ->
    {SingleHeadD, HeadD} = clause_head_to_algebra(spec, Args),
    {SingleGuardsD, GuardsD} = guards_to_algebra_pair(Guards),
    BodyD = expr_to_algebra(Body),
    SingleBodyD = document_single_line(BodyD),

    SingleD = wrap(SingleHeadD, document_text(" -> "), wrap(SingleBodyD, document_text(" when "), SingleGuardsD)),
    MultiPrefix =
        document_choice(
            wrap(SingleHeadD, document_text(" -> "), SingleBodyD),
            document_choice(
                combine_nested(document_combine(SingleHeadD, document_text(" ->")), BodyD),
                wrap(HeadD, document_text(" -> "), BodyD)
            )
        ),
    MultiD = combine_newline(MultiPrefix, document_combine(document_text("when "), GuardsD)),

    {SingleD, MultiD};
do_clause_to_algebra_pair({clause, _Meta, Name, Args, [], Body}) ->
    {SingleHeadD, HeadD} = clause_head_to_algebra(Name, Args),
    BodyD = block_to_algebra(Body),
    SingleBodyD = document_single_line(BodyD),

    SingleD = wrap(SingleHeadD, document_text(" -> "), SingleBodyD),
    MultiPrefix = document_combine(document_choice(SingleHeadD, HeadD), document_text(" ->")),
    MultiD = combine_nested(MultiPrefix, BodyD),

    {SingleD, MultiD};
do_clause_to_algebra_pair({clause, _Meta, Name, Args, Guards, Body}) ->
    {SingleHeadD, HeadD} = clause_head_to_algebra(Name, Args),
    {SingleGuardsD, GuardsD} = guards_to_algebra_pair(Guards),
    BodyD = block_to_algebra(Body),
    SingleBodyD = document_single_line(BodyD),

    SingleD = wrap(SingleHeadD, document_text(" when "), wrap(SingleGuardsD, document_text(" -> "), SingleBodyD)),
    MultiPrefix =
        document_choice(
            wrap(SingleHeadD, document_text(" when "), SingleGuardsD),
            document_choice(
                combine_newline(SingleHeadD, document_combine(document_text("when "), GuardsD)),
                wrap(HeadD, document_text(" when "), GuardsD)
            )
        ),
    MultiD = combine_nested(document_combine(MultiPrefix, document_text(" ->")), BodyD),

    {SingleD, MultiD};
do_clause_to_algebra_pair({macro_call, _, _, _} = Expr) ->
    %% It's possible the entire clause is defined inside of a macro call
    Doc = expr_to_algebra(Expr),
    {document_single_line(Doc), Doc}.

clause_head_to_algebra(FunOrSpec, Args) when FunOrSpec =:= 'fun'; FunOrSpec =:= spec ->
    container_to_algebra_pair(Args, document_text("("), document_text(")"));
clause_head_to_algebra('case', [Arg]) ->
    Doc = expr_to_algebra(Arg),
    {document_single_line(Doc), Doc};
clause_head_to_algebra('catch', Args) ->
    ArgsD = lists:map(fun expr_to_algebra/1, Args),
    Doc = document_reduce(fun combine_colon/2, ArgsD),
    {document_single_line(Doc), Doc};
clause_head_to_algebra(Name, Args) ->
    Prefix = document_combine(expr_to_algebra(Name), document_text("(")),
    container_to_algebra_pair(Args, Prefix, document_text(")")).

guards_to_algebra_pair(Guards) ->
    {SingleLine, MultiLine} = lists:unzip(lists:map(fun guard_alt_to_algebra_pair/1, Guards)),
    SingleLineD = document_reduce(fun combine_semi_space/2, SingleLine),
    MultiLineD = document_reduce(fun combine_semi_newline/2, MultiLine),
    {SingleLineD, document_choice(SingleLineD, MultiLineD)}.

guard_alt_to_algebra_pair(Alt) ->
    AltD = lists:map(fun expr_to_algebra/1, Alt),
    SingleLine = lists:map(fun erlfmt_algebra:document_single_line/1, AltD),

    SingleLineD = document_reduce(fun combine_comma_space/2, SingleLine),
    MultiLineD = document_reduce(fun combine_comma_newline/2, AltD),
    {SingleLineD, document_choice(SingleLineD, MultiLineD)}.

receive_after_to_algebra(Expr, Body) ->
    ExprD = expr_to_algebra(Expr),
    BodyD = block_to_algebra(Body),

    Head = wrap(document_text("after "), ExprD, document_text(" ->")),
    document_choice(
        combine_space(Head, document_single_line(BodyD)),
        combine_nested(Head, BodyD)
    ).

try_to_algebra(Exprs, OfClauses, CatchClauses, After) ->
    Clauses =
        [try_of_block(Exprs, OfClauses)] ++
        [try_catch_to_algebra(CatchClauses) || CatchClauses =/= []] ++
        [try_after_to_algebra(After) || After =/= []] ++
        [document_text("end")],

    document_reduce(fun combine_newline/2, Clauses).

try_catch_to_algebra(Clauses) ->
    combine_nested(document_text("catch"), clauses_to_algebra(Clauses)).

try_after_to_algebra(Exprs) ->
    AfterD = document_text("after"),
    ExprsD = block_to_algebra(Exprs),
    document_choice(
        combine_space(AfterD, document_single_line(ExprsD)),
        combine_nested(AfterD, ExprsD)
    ).

try_of_block(Exprs, OfClauses) ->
    TryD = document_text("try"),
    OfD = document_text("of"),
    ExprsD = block_to_algebra(Exprs),

    TrySingle = combine_space(TryD, document_single_line(ExprsD)),
    TryMulti = combine_nested(TryD, ExprsD),

    case OfClauses of
        [] ->
            document_choice(TrySingle, TryMulti);
        _ ->
            combine_nested(
                document_choice(combine_space(TrySingle, OfD), combine_newline(TryMulti, OfD)),
                clauses_to_algebra(OfClauses)
            )
    end.

guard_to_algebra(Expr, Guard) ->
    WhenD = document_text("when"),
    ExprD = expr_to_algebra(Expr),
    GuardD = expr_to_algebra(Guard),

    document_choice(
        combine_space(document_single_line(ExprD), combine_space(WhenD, document_single_line(GuardD))),
        combine_newline(ExprD, combine_space(WhenD, GuardD))
    ).

combine_comments(Meta, Doc) ->
    {Pre, Post} = comments(Meta),
    combine_post_comments(Post, combine_pre_comments(Pre, Doc)).

combine_pre_comments([], Doc) -> Doc;
combine_pre_comments(Comments, Doc) ->
    combine_newline(comments_to_algebra(Comments), Doc).

combine_post_comments([], Doc) -> Doc;
combine_post_comments(Comments, Doc) ->
    document_flush(combine_newline(Doc, comments_to_algebra(Comments))).

comments_to_algebra(Comments) ->
    %% TODO: should we add spaces in between?
    CommentsD = lists:map(fun comment_to_algebra/1, Comments),
    document_reduce(fun combine_newline/2, CommentsD).

comment_to_algebra({comment, _Meta, Lines}) ->
    LinesD = lists:map(fun erlfmt_algebra:document_text/1, Lines),
    document_reduce(fun combine_newline/2, LinesD).

comments(Meta0) ->
    Meta = erl_anno:to_term(Meta0),
    {proplists:get_value(pre_comments, Meta, []), proplists:get_value(post_comments, Meta, [])}.

atom_needs_quotes([C0 | Cs]) when C0 >= $a, C0 =< $z ->
    lists:any(fun
        (C) when ?IN_RANGE(C, $a, $z); ?IN_RANGE(C, $A, $Z); ?IN_RANGE(C, $0, $9); C =:= $_; C=:= $@ -> false;
        (_) -> true
    end, Cs);
atom_needs_quotes(_) -> true.

escape_string([Quote | Rest], Original, Quote) ->
    [Quote | escape_string_loop(Rest, Original, Quote)].

%% Remove unneeded escapes, upcase hex escapes
escape_string_loop(Tail, [], _Quote) -> Tail;
escape_string_loop([$\\, $x | EscapeAndRest], [_Escaped | Original], Quote) ->
    {Escape, Rest} = escape_hex(EscapeAndRest),
    [$\\, $x, Escape | escape_string_loop(Rest, Original, Quote)];
escape_string_loop([$\\, Escape | Rest], [Value | Original], Quote) ->
    if
        ?IS_OCT_DIGIT(Escape) ->
            case Rest of
                [D2, D3 | Rest1] when ?IS_OCT_DIGIT(D2), ?IS_OCT_DIGIT(D3) ->
                    [$\\, Escape, D2, D3 | escape_string_loop(Rest1, Original, Quote)];
                [D2 | Rest1] when ?IS_OCT_DIGIT(D2) ->
                    [$\\, Escape, D2 | escape_string_loop(Rest1, Original, Quote)];
                _ ->
                    [$\\, Escape | escape_string_loop(Rest, Original, Quote)]
            end;
        Escape =:= $s ->
            [Value | escape_string_loop(Rest, Original, Quote)];
        Escape =:= Quote; Escape =:= $\\; Escape =/= Value ->
            [$\\, Escape | escape_string_loop(Rest, Original, Quote)];
        true ->
            [Escape | escape_string_loop(Rest, Original, Quote)]
    end;
escape_string_loop([C | Rest], [C | Original], Quote) ->
    [C | escape_string_loop(Rest, Original, Quote)].

escape_hex([${ | Rest0]) ->
    [Escape, Rest] = string:split(Rest0, "}"),
    {[${, string:uppercase(Escape), $}], Rest};
escape_hex([X1, X2 | Rest]) ->
    {string:uppercase([X1, X2]), Rest}.

inop_prec('::') -> {100,40,100};
inop_prec('=') -> {140,100,100};
inop_prec('!') -> {140,100,100};
inop_prec('orelse') -> {160,150,150};
inop_prec('andalso') -> {170,160,160};
inop_prec('|') -> {180,170,170};
inop_prec('..') -> {300,200,300};
inop_prec('==') -> {300,200,300};
inop_prec('/=') -> {300,200,300};
inop_prec('=<') -> {300,200,300};
inop_prec('<') -> {300,200,300};
inop_prec('>=') -> {300,200,300};
inop_prec('>') -> {300,200,300};
inop_prec('=:=') -> {300,200,300};
inop_prec('=/=') -> {300,200,300};
inop_prec('++') -> {400,300,300};
inop_prec('--') -> {400,300,300};
inop_prec('+') -> {400,400,500};
inop_prec('-') -> {400,400,500};
inop_prec('bor') -> {400,400,500};
inop_prec('bxor') -> {400,400,500};
inop_prec('bsl') -> {400,400,500};
inop_prec('bsr') -> {400,400,500};
inop_prec('or') -> {400,400,500};
inop_prec('xor') -> {400,400,500};
inop_prec('*') -> {500,500,600};
inop_prec('/') -> {500,500,600};
inop_prec('div') -> {500,500,600};
inop_prec('rem') -> {500,500,600};
inop_prec('band') -> {500,500,600};
inop_prec('and') -> {500,500,600};
inop_prec('#') -> {800,700,800};
inop_prec(':') -> {900,800,900};
inop_prec('.') -> {900,900,1000}.
