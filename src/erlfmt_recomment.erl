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

-export([recomment/2]).

-spec recomment(erlfmt_parse:abstract_form(), [erlfmt_scan:comment()]) -> erlfmt_parse:abstract_form().
recomment(Form, Comments) ->
    insert_form(Form, Comments).

insert_form(Form, []) ->
    Form;
insert_form({function, Meta, Clauses0}, Comments0) ->
    case lists:mapfoldl(fun insert/2, Comments0, Clauses0) of
        {Clauses, []} ->
            {function, Meta, Clauses};
        {Clauses, PostCommennts} ->
            {function, erlfmt_scan:put_anno(post_comments, PostCommennts, Meta), Clauses}
    end;
insert_form({attribute, Meta0, Name, Values0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    case lists:mapfoldl(fun insert/2, Comments1, Values0) of
        {Values, []} ->
            {attribute, Meta, Name, Values};
        {Values, PostComments} ->
            {attribute, erlfmt_scan:put_anno(post_comments, PostComments, Meta), Name, Values}
    end.

insert({Atomic, Meta0, Value}, Comments0)
when Atomic =:= integer; Atomic =:= float; Atomic =:= char; Atomic =:= atom; Atomic =:= string; Atomic =:= var ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0),
    {{Atomic, Meta, Value}, Comments};
insert({concat, Meta0, Values}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0, Values),
    {{concat, Meta, Values}, Comments};
insert({op, Meta0, Op, Expr0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Expr, Comments} = insert(Expr0, Comments1),
    {{op, Meta, Op, Expr}, Comments};
insert({op, Meta0, Op, Left0, Right0}, Comments0) ->
    {Left, Comments1} = insert(Left0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1),
    {Right, Comments} = insert(Right0, Comments2),
    {{op, Meta, Op, Left, Right}, Comments};
insert({typed, Meta0, Left0, Right0}, Comments0) ->
    {Left, Comments1} = insert(Left0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1),
    {Right, Comments} = insert(Right0, Comments2),
    {{typed, Meta, Left, Right}, Comments};
insert({Collection, Meta0, Values0}, Comments0)
when Collection =:= map; Collection =:= list; Collection =:= tuple; Collection =:= bin; Collection =:= block ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Values, Comments} = lists:mapfoldl(fun insert/2, Comments1, Values0),
    {{Collection, Meta, Values}, Comments};
insert({cons, Meta0, Head0, Tail0}, Comments0) ->
    {Head, Comments1} = insert(Head0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1),
    {Tail, Comments} = insert(Tail0, Comments2),
    {{cons, Meta, Head, Tail}, Comments};
insert({bin_element, Meta0, Expr, Size, Types}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0, [Expr, Size, Types]),
    {{bin_element, Meta, Expr, Size, Types}, Comments};
insert({map, Meta0, Expr0, Values0}, Comments0) ->
    {Expr, Comments1} = insert(Expr0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1),
    {Values, Comments} = lists:mapfoldl(fun insert/2, Comments2, Values0),
    {{map, Meta, Expr, Values}, Comments};
insert({Field, Meta0, Key0, Value0}, Comments0)
when Field =:= map_field_assoc; Field =:= map_field_exact; Field =:= record_field; Field =:= generate; Field =:= b_generate ->
    {Key, Comments1} = insert(Key0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1),
    {Value, Comments} = insert(Value0, Comments2),
    {{Field, Meta, Key, Value}, Comments};
insert({record, Meta0, Name, Values0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0, [Name]),
    {Values, Comments} = lists:mapfoldl(fun insert/2, Comments1, Values0),
    {{record, Meta, Name, Values}, Comments};
insert({record, Meta0, Expr0, Name, Values0}, Comments0) ->
    {Expr, Comments1} = insert(Expr0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1, [Name]),
    {Values, Comments} = lists:mapfoldl(fun insert/2, Comments2, Values0),
    {{record, Meta, Expr, Name, Values}, Comments};
insert({record_index, Meta0, Name, Key}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0, [Name, Key]),
    {{record_index, Meta, Name, Key}, Comments};
insert({record_field, Meta0, Name}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0, [Name]),
    {{record_field, Meta, Name}, Comments};
insert({record_field, Meta0, Expr0, Name, Key}, Comments0) ->
    {Expr, Comments1} = insert(Expr0, Comments0),
    {Meta, Comments} = put_pre_comments(Meta0, Comments1, [Name, Key]),
    {{record_field, Meta, Expr, Name, Key}, Comments};
insert({Comprehension, Meta0, Expr0, LcExprs0}, Comments0)
when Comprehension =:= lc; Comprehension =:= bc ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Expr, Comments2} = insert(Expr0, Comments1),
    {LcExprs, Comments} = lists:mapfoldl(fun insert/2, Comments2, LcExprs0),
    {{Comprehension, Meta, Expr, LcExprs}, Comments};
insert({call, Meta0, Name0, Args0}, Comments0) ->
    {Name, Comments1} = insert(Name0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1),
    {Args, Comments} = lists:mapfoldl(fun insert/2, Comments2, Args0),
    {{call, Meta, Name, Args}, Comments};
insert({macro_call, Meta0, Name, none}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0, [Name]),
    {{macro_call, Meta, Name, none}, Comments};
insert({macro_call, Meta0, Name, Args0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0, [Name]),
    {Args, Comments} = lists:mapfoldl(fun insert/2, Comments1, Args0),
    {{macro_call, Meta, Name, Args}, Comments};
insert({macro_string, Meta0, Name}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0, [Name]),
    {{macro_string, Meta, Name}, Comments};
insert({remote, Meta0, Mod0, Name0}, Comments0) ->
    {Mod, Comments1} = insert(Mod0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1),
    {Name, Comments} = insert(Name0, Comments2),
    {{remote, Meta, Mod, Name}, Comments};
insert({'fun', Meta0, {function, Name, Arity}}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0, [Name, Arity]),
    {{'fun', Meta, {function, Name, Arity}}, Comments};
insert({'fun', Meta0, {function, Mod, Name, Arity}}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0, [Mod, Name, Arity]),
    {{'fun', Meta, {function, Mod, Name, Arity}}, Comments};
insert({'fun', Meta0, {clauses, Clauses0}}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Clauses, Comments} = lists:mapfoldl(fun insert/2, Comments1, Clauses0),
    {{'fun', Meta, {clauses, Clauses}}, Comments};
insert({'fun', Meta0, type}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0),
    {{'fun', Meta, type}, Comments};
insert({'fun', Meta0, {type, Args0, Res0}}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Args, Comments2} = insert(Args0, Comments1),
    {Res, Comments} = insert(Res0, Comments2),
    {{'fun', Meta, {type, Args, Res}}, Comments};
insert({'case', Meta0, Expr0, Clauses0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Expr, Comments2} = insert(Expr0, Comments1),
    {Clauses, Comments} = lists:mapfoldl(fun insert/2, Comments2, Clauses0),
    {{'case', Meta, Expr, Clauses}, Comments};
insert({'receive', Meta0, Clauses0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Clauses, Comments} = lists:mapfoldl(fun insert/2, Comments1, Clauses0),
    {{'receive', Meta, Clauses}, Comments};
insert({'receive', Meta0, Clauses0, AfterExpr0, AfterBody0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Clauses, Comments2} = lists:mapfoldl(fun insert/2, Comments1, Clauses0),
    {AfterExpr, Comments3} = insert(AfterExpr0, Comments2),
    {AfterBody, Comments} = lists:mapfoldl(fun insert/2, Comments3, AfterBody0),
    {{'receive', Meta, Clauses, AfterExpr, AfterBody}, Comments};
insert({'try', Meta0, Exprs0, OfClauses0, CatchClauses0, After0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Exprs, Comments2} = lists:mapfoldl(fun insert/2, Comments1, Exprs0),
    {OfClauses, Comments3} = lists:mapfoldl(fun insert/2, Comments2, OfClauses0),
    {CatchClauses, Comments4} = lists:mapfoldl(fun insert/2, Comments3, CatchClauses0),
    {After, Comments} = lists:mapfoldl(fun insert/2, Comments4, After0),
    {{'try', Meta, Exprs, OfClauses, CatchClauses, After}, Comments};
insert({'if', Meta0, Clauses0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0),
    {Clauses, Comments} = lists:mapfoldl(fun insert/2, Comments1, Clauses0),
    {{'if', Meta, Clauses}, Comments};
insert({guard, Meta0, Expr0, Guard0}, Comments0) ->
    {Expr, Comments1} = insert(Expr0, Comments0),
    {Meta, Comments2} = put_pre_comments(Meta0, Comments1),
    {Guard, Comments} = insert(Guard0, Comments2),
    {{guard, Meta, Expr, Guard}, Comments};
insert({spec, Meta0, Name, Clauses0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0, [Name]),
    {Clauses, Comments} = lists:mapfoldl(fun insert/2, Comments1, Clauses0),
    {{spec, Meta, Name, Clauses}, Comments};
insert({'...', Meta0}, Comments0) ->
    {Meta, Comments} = put_pre_comments(Meta0, Comments0),
    {{'...', Meta}, Comments};
insert({clause, Meta0, Name, Args0, Guards0, Body0}, Comments0) ->
    {Meta, Comments1} = put_pre_comments(Meta0, Comments0, [Name]),
    {Args, Comments2} = insert(Args0, Comments1),
    {Guards, Comments3} = insert(Guards0, Comments2),
    {Body, Comments} = insert(Body0, Comments3),
    {{clause, Meta, Name, Args, Guards, Body}, Comments};
insert(List, Comments) when is_list(List) ->
    lists:mapfoldl(fun insert/2, Comments, List).

put_pre_comments(Meta, Comments) ->
    {Line, _} = erlfmt_scan:get_anno(location, Meta),
    case take_comments(Comments, Line, []) of
        {[], Rest} -> {Meta, Rest};
        {PreComments, Rest} ->
            {erlfmt_scan:put_anno(pre_comments, PreComments, Meta), Rest}
    end.

put_pre_comments(Meta, Comments, _Other) ->
    %% TODO: compute max line using "Other"
    put_pre_comments(Meta, Comments).

take_comments([Comment | Rest], Line, Acc) ->
    case erlfmt_scan:get_anno(location, Comment) of
        {CommentLine, _} when CommentLine =< Line ->
            take_comments(Rest, Line, [Comment | Acc]);
        _ ->
            {lists:reverse(Acc), [Comment | Rest]}
    end;
take_comments([], _Line, Acc) ->
    {lists:reverse(Acc), []}.
