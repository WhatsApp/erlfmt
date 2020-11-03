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
-module(erlfmt_recomment).

-include("erlfmt_scan.hrl").

-export([recomment/2, put_pre_comments/2, put_post_comments/2, take_comments/2]).

-define(IS_COLLECTION(Kind),
    Kind =:= map orelse
        Kind =:= list orelse
        Kind =:= tuple orelse
        Kind =:= bin orelse
        Kind =:= block
).

-spec recomment(erlfmt_parse:abstract_node(), [erlfmt_scan:comment()]) ->
    erlfmt_parse:abstract_node().
recomment(Node, Comments) ->
    insert_node(Node, Comments).

insert_node(Node, []) ->
    Node;
insert_node({function, Meta0, Clauses0}, Comments0) ->
    {PreComments, Comments} = split_pre_comments(Meta0, Comments0),
    {Clauses, PostComments} = insert_expr_list(Clauses0, Comments),
    Meta1 = put_pre_comments(Meta0, PreComments),
    Meta = put_post_comments(Meta1, PostComments),
    {function, Meta, Clauses};
insert_node({attribute, Meta0, {atom, _, RawName} = Name, Values0}, Comments) when
    RawName =:= spec; RawName =:= callback; RawName =:= type; RawName =:= opaque
->
    {PreComments, InnerComments, PostCommennts} = split_comments(Meta0, Comments),
    {Values, RestComments} = insert_expr_list(Values0, InnerComments),
    Meta1 = put_pre_comments(Meta0, PreComments),
    Meta2 = put_post_comments(Meta1, PostCommennts),
    Meta = put_pre_dot_comments(Meta2, RestComments),
    {attribute, Meta, Name, Values};
insert_node({attribute, Meta0, Name, Values0}, Comments) ->
    {PreComments, InnerComments, PostCommennts} = split_comments(Meta0, Comments),
    Values = insert_expr_container(Values0, InnerComments),
    Meta1 = put_pre_comments(Meta0, PreComments),
    Meta = put_post_comments(Meta1, PostCommennts),
    {attribute, Meta, Name, Values};
insert_node({exprs, Meta0, Exprs0}, Comments0) ->
    {PreComments, InnerComments, PostCommennts} = split_comments(Meta0, Comments0),
    {Exprs, RestComments} = insert_expr_list(Exprs0, InnerComments),
    Meta1 = put_pre_comments(Meta0, PreComments),
    Meta2 = put_post_comments(Meta1, PostCommennts),
    Meta = put_pre_dot_comments(Meta2, RestComments),
    {exprs, Meta, Exprs};
insert_node(Expr0, Comments) ->
    {PreComments, InnerComments, PostComments} = split_comments(
        element(2, Expr0),
        Comments
    ),
    {Expr1, RestComments} = insert_expr(Expr0, InnerComments),
    Expr = put_pre_comments(Expr1, PreComments),
    put_post_comments(Expr, RestComments ++ PostComments).

insert_expr(Node0, Comments) ->
    {PreComments, InnerComments, RestComments} =
        split_comments(element(2, Node0), Comments),
    {Node1, []} = insert_nested(Node0, InnerComments),
    Node = put_pre_comments(Node1, PreComments),
    {Node, RestComments}.

insert_expr_list(Exprs, Comments) -> insert_expr_list(Exprs, Comments, []).

insert_expr_list(Exprs, [], Acc) ->
    {lists:reverse(Acc, Exprs), []};
insert_expr_list([Expr0 | Exprs], Comments0, Acc) when is_tuple(Expr0) ->
    {Expr, Comments} = insert_expr(Expr0, Comments0),
    insert_expr_list(Exprs, Comments, [Expr | Acc]);
insert_expr_list([], Comments, Acc) ->
    {lists:reverse(Acc), Comments}.

insert_expr_container(Exprs, []) ->
    Exprs;
insert_expr_container([Expr0], Comments0) when is_tuple(Expr0) ->
    {Expr, Comments} = insert_expr(Expr0, Comments0),
    [put_post_comments(Expr, Comments)];
insert_expr_container([Expr0 | Exprs], Comments0) when is_tuple(Expr0) ->
    {Expr, Comments} = insert_expr(Expr0, Comments0),
    [Expr | insert_expr_container(Exprs, Comments)];
%% final comments in containers become standalone comment elements
insert_expr_container([], Comments) ->
    Comments.

split_pre_comments(#{location := {SLine, _}}, Comments) ->
    take_comments(SLine, Comments).

split_comments(#{location := {SLine, _}, end_location := {ELine, _}}, Comments0) ->
    {PreComments, Comments1} = take_comments(SLine, Comments0),
    {InnerComments, PostComments} = take_comments(ELine, Comments1),
    {PreComments, InnerComments, PostComments}.

take_comments(Line, Comments) ->
    F = fun({_, #{end_location := {CLine, _}}, _}) -> CLine =< Line end,
    lists:splitwith(F, Comments).

insert_nested(Node, []) ->
    {Node, []};
insert_nested({Atomic, _, _} = Node, Comments) when ?IS_ATOMIC(Atomic) ->
    {Node, Comments};
insert_nested({concat, Meta, Strings0}, Comments0) ->
    {Strings, Comments} = insert_expr_list(Strings0, Comments0),
    {{concat, Meta, Strings}, Comments};
insert_nested({op, Meta, Op, Expr0}, Comments0) ->
    {Expr, Comments} = insert_expr(Expr0, Comments0),
    {{op, Meta, Op, Expr}, Comments};
insert_nested({op, Meta, Op, Left0, Right0}, Comments0) ->
    {Left, Comments1} = insert_expr(Left0, Comments0),
    {Right, Comments} = insert_expr(Right0, Comments1),
    {{op, Meta, Op, Left, Right}, Comments};
insert_nested({cons, Meta, Head0, Tail0}, Comments0) ->
    {Head, Comments1} = insert_expr(Head0, Comments0),
    {Tail, Comments} = insert_expr(Tail0, Comments1),
    {{cons, Meta, Head, Tail}, Comments};
insert_nested({spec_clause, Meta, Head0, Body0, empty}, Comments0) ->
    {Head, Comments1} = insert_expr(Head0, Comments0),
    {Body, Comments} = insert_expr_list(Body0, Comments1),
    {{spec_clause, Meta, Head, Body, empty}, Comments};
insert_nested({spec_clause, Meta, Head0, Body0, Guards0}, Comments0) ->
    {Head, Comments1} = insert_expr(Head0, Comments0),
    {Body, Comments2} = insert_expr_list(Body0, Comments1),
    {Guards, Comments} = insert_expr(Guards0, Comments2),
    {{spec_clause, Meta, Head, Body, Guards}, Comments};
insert_nested({clause, Meta, Head0, empty, Body0}, Comments0) ->
    {Head, Comments1} = insert_expr(Head0, Comments0),
    {Body, Comments} = insert_expr_list(Body0, Comments1),
    {{clause, Meta, Head, empty, Body}, Comments};
insert_nested({clause, Meta, empty, Guards0, Body0}, Comments0) ->
    {Guards, Comments1} = insert_expr(Guards0, Comments0),
    {Body, Comments} = insert_expr_list(Body0, Comments1),
    {{clause, Meta, empty, Guards, Body}, Comments};
insert_nested({clause, Meta, Head0, Guards0, Body0}, Comments0) ->
    {Head, Comments1} = insert_expr(Head0, Comments0),
    {Guards, Comments2} = insert_expr(Guards0, Comments1),
    {Body, Comments} = insert_expr_list(Body0, Comments2),
    {{clause, Meta, Head, Guards, Body}, Comments};
insert_nested({Guard, Meta, Guards0}, Comments0) when Guard =:= guard_or; Guard =:= guard_and ->
    {Guards, Comments} = insert_expr_list(Guards0, Comments0),
    {{Guard, Meta, Guards}, Comments};
insert_nested({Collection, Meta, Values0}, Comments0) when ?IS_COLLECTION(Collection) ->
    Values = insert_expr_container(Values0, Comments0),
    {{Collection, Meta, Values}, []};
insert_nested({map, Meta, Expr0, Values0}, Comments0) ->
    {Expr, Comments1} = insert_expr(Expr0, Comments0),
    Values = insert_expr_container(Values0, Comments1),
    {{map, Meta, Expr, Values}, []};
insert_nested({record, Meta, Name, Values0}, Comments0) ->
    Values = insert_expr_container(Values0, Comments0),
    {{record, Meta, Name, Values}, []};
insert_nested({record, Meta, Expr0, Name, Values0}, Comments0) ->
    {Expr, Comments1} = insert_expr(Expr0, Comments0),
    Values = insert_expr_container(Values0, Comments1),
    {{record, Meta, Expr, Name, Values}, []};
insert_nested({Comprehension, Meta, Expr0, LcExprs0}, Comments0) when
    Comprehension =:= lc; Comprehension =:= bc
->
    {Expr, Comments1} = insert_expr(Expr0, Comments0),
    LcExprs = insert_expr_container(LcExprs0, Comments1),
    {{Comprehension, Meta, Expr, LcExprs}, []};
insert_nested({Field, Meta, Key0, Value0}, Comments0) when
    Field =:= map_field_assoc;
    Field =:= map_field_exact;
    Field =:= record_field;
    Field =:= generate;
    Field =:= b_generate
->
    {Key, Comments1} = insert_expr(Key0, Comments0),
    {Value, Comments} = insert_expr(Value0, Comments1),
    {{Field, Meta, Key, Value}, Comments};
%% no internal comments in record_indexm record_field, or bin_element
insert_nested({record_index, _, _, _} = Node, Comments) ->
    {put_pre_comments(Node, Comments), []};
insert_nested({record_field, _, _} = Node, Comments) ->
    {put_pre_comments(Node, Comments), []};
insert_nested({record_field, Meta0, Expr0, Name, Key}, Comments0) ->
    {Expr, Comments1} = insert_expr(Expr0, Comments0),
    Meta = put_pre_comments(Meta0, Comments1),
    {{record_field, Meta, Expr, Name, Key}, []};
insert_nested({bin_element, Meta0, Expr0, Size, Types}, Comments0) ->
    {Expr, Comments1} = insert_expr(Expr0, Comments0),
    Meta = put_pre_comments(Meta0, Comments1),
    {{bin_element, Meta, Expr, Size, Types}, []};
insert_nested({call, Meta, Name0, Args0}, Comments0) ->
    {Name, Comments1} = insert_expr(Name0, Comments0),
    Args = insert_expr_container(Args0, Comments1),
    {{call, Meta, Name, Args}, []};
insert_nested({macro_call, Meta0, Name, none}, Comments0) ->
    Meta = put_pre_comments(Meta0, Comments0),
    {{macro_call, Meta, Name, none}, []};
insert_nested({macro_call, Meta, Name, Args0}, Comments0) ->
    Args = insert_expr_container(Args0, Comments0),
    {{macro_call, Meta, Name, Args}, []};
insert_nested({macro_string, Meta0, Name}, Comments0) ->
    Meta = put_pre_comments(Meta0, Comments0),
    {{macro_string, Meta, Name}, []};
insert_nested({remote, Meta, Mod0, Name0}, Comments0) ->
    {Mod, Comments1} = insert_expr(Mod0, Comments0),
    {Name, Comments} = insert_expr(Name0, Comments1),
    {{remote, Meta, Mod, Name}, Comments};
insert_nested({'case', Meta, Expr0, Clauses0}, Comments0) ->
    {Expr, Comments1} = insert_expr(Expr0, Comments0),
    Clauses = insert_expr_container(Clauses0, Comments1),
    {{'case', Meta, Expr, Clauses}, []};
insert_nested({'receive', Meta, Clauses0}, Comments0) ->
    Clauses = insert_expr_container(Clauses0, Comments0),
    {{'receive', Meta, Clauses}, []};
insert_nested({'receive', Meta, Clauses0, AfterExpr0, AfterBody0}, Comments0) ->
    {Clauses, Comments1} = insert_expr_list(Clauses0, Comments0),
    {AfterExpr, Comments2} = insert_expr(AfterExpr0, Comments1),
    AfterBody = insert_expr_container(AfterBody0, Comments2),
    {{'receive', Meta, Clauses, AfterExpr, AfterBody}, []};
insert_nested({'if', Meta, Clauses0}, Comments0) ->
    Clauses = insert_expr_container(Clauses0, Comments0),
    {{'if', Meta, Clauses}, []};
insert_nested({'try', Meta, Exprs0, OfClauses0, CatchClauses0, []}, Comments0) ->
    {Exprs, Comments1} = insert_expr_list(Exprs0, Comments0),
    {OfClauses, Comments2} = insert_expr_list(OfClauses0, Comments1),
    CatchClauses = insert_expr_container(CatchClauses0, Comments2),
    {{'try', Meta, Exprs, OfClauses, CatchClauses, []}, []};
insert_nested({'try', Meta, Exprs0, OfClauses0, CatchClauses0, After0}, Comments0) ->
    {Exprs, Comments1} = insert_expr_list(Exprs0, Comments0),
    {OfClauses, Comments2} = insert_expr_list(OfClauses0, Comments1),
    {CatchClauses, Comments3} = insert_expr_list(CatchClauses0, Comments2),
    After = insert_expr_container(After0, Comments3),
    {{'try', Meta, Exprs, OfClauses, CatchClauses, After}, []};
insert_nested({spec, Meta, Name, Clauses0}, Comments0) ->
    Clauses = insert_expr_container(Clauses0, Comments0),
    {{spec, Meta, Name, Clauses}, []};
insert_nested({'fun', _, Fun} = Node, Comments0) when
    element(1, Fun) =:= 'function'; Fun =:= type
->
    {put_pre_comments(Node, Comments0), []};
insert_nested({'fun', Meta, {type, InnerMeta, Args0, Res0}}, Comments0) ->
    {Args, Comments1} = insert_expr(Args0, Comments0),
    {Res, Comments} = insert_expr(Res0, Comments1),
    {{'fun', Meta, {type, InnerMeta, Args, Res}}, Comments};
insert_nested({'fun', Meta, {clauses, InnerMeta, Clauses0}}, Comments0) ->
    Clauses = insert_expr_container(Clauses0, Comments0),
    {{'fun', Meta, {clauses, InnerMeta, Clauses}}, []};
insert_nested({'catch', Meta, Args0}, Comments0) ->
    {Args, Comments} = insert_expr_list(Args0, Comments0),
    {{'catch', Meta, Args}, Comments};
insert_nested({args, Meta, Args0}, Comments0) ->
    Args = insert_expr_container(Args0, Comments0),
    {{args, Meta, Args}, []};
insert_nested({Name, Meta}, Comments) ->
    {{Name, Meta}, Comments}.

put_pre_dot_comments(NodeOrMeta, []) ->
    NodeOrMeta;
put_pre_dot_comments(NodeOrMeta, Comments) ->
    Existing = erlfmt_scan:get_anno(pre_dot_comments, NodeOrMeta, []),
    erlfmt_scan:merge_anno(
        #{
            pre_dot_comments => Existing ++ Comments
        },
        NodeOrMeta
    ).

put_post_comments(NodeOrMeta, []) ->
    NodeOrMeta;
put_post_comments(NodeOrMeta, Comments) ->
    Loc = erlfmt_scan:get_anno(end_location, NodeOrMeta),
    CommentLoc = erlfmt_scan:get_anno(end_location, lists:last(Comments)),
    Existing = erlfmt_scan:get_anno(post_comments, NodeOrMeta, []),
    erlfmt_scan:merge_anno(
        #{
            end_location => max(Loc, CommentLoc),
            inner_end_location => Loc,
            post_comments => Existing ++ Comments
        },
        NodeOrMeta
    ).

put_pre_comments(NodeOrMeta, []) ->
    NodeOrMeta;
put_pre_comments(NodeOrMeta, Comments) ->
    Loc = erlfmt_scan:get_anno(location, NodeOrMeta),
    CommentLoc = erlfmt_scan:get_anno(location, hd(Comments)),
    Existing = erlfmt_scan:get_anno(pre_comments, NodeOrMeta, []),
    erlfmt_scan:merge_anno(
        #{
            location => min(Loc, CommentLoc),
            inner_location => Loc,
            pre_comments => Comments ++ Existing
        },
        NodeOrMeta
    ).
