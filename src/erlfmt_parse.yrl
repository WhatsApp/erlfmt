%% -*- erlang -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%

%% Definition of the Erlang grammar.

Nonterminals
form
attribute attr_val
function function_clauses function_clause
clause_args clause_guard clause_body
expr expr_max expr_max_remote
pat_expr pat_expr_max map_pat_expr record_pat_expr
pat_argument_list pat_exprs
list list_exprs
list_comprehension lc_expr lc_exprs
binary_comprehension
tuple
record_expr record_name record_field_name record_tuple record_field record_fields
map_expr map_tuple map_field map_fields map_key
if_expr if_clause if_clauses case_expr cr_clause cr_clauses receive_expr
fun_expr fun_clause fun_clauses
atom_or_var atom_or_var_or_macro integer_or_var_or_macro
try_expr try_catch try_clause try_clauses try_opt_stacktrace
function_call argument_list
exprs guard vars
atomic concatable concatables macro_record_or_concatable
prefix_op mult_op add_op list_op comp_op
binary bin_elements bin_element bit_expr
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type
type types type_argument_list
type_sig type_sigs fun_type binary_type bin_element_type type_map_fields type_map_field
type_spec spec_fun
macro_call_expr macro_string macro_call_pat macro_call_type macro_call_none
macro_expr macro_exprs
macro_name macro_def_expr macro_def_expr_body macro_def_clause.

Terminals
char integer float atom string var

'(' ')' ',' '->' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.'
'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
'andalso' 'orelse'
'bnot' 'not'
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'++' '--'
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<=' '=>' ':='
'<<' '>>'
'?' '!' '=' '::' '..' '...'
spec callback define_expr define_clause % helper
dot.

%% Conflict comes from optional parens on macro calls.
Expect 1.

Rootsymbol form.

%% Expressions

Unary 10 'catch'.
Right 100 '=' '!'.
Right 150 'orelse'.
Right 160 'andalso'.
Nonassoc 200 comp_op.
Right 300 list_op.
Left 400 add_op.
Left 500 mult_op.
Unary 600 prefix_op.
Nonassoc 700 '#'.
Nonassoc 800 ':'.

%% Types

Right 40 '::'.
Right 170 '|'.
Nonassoc 200 '..'.
Nonassoc 200 '*'. % for binary expressions

form -> attribute dot : '$1'.
form -> function dot : '$1'.

attribute -> '-' 'if' attr_val               : build_attribute({atom,?anno('$2'),'if'}, '$3').
attribute -> '-' atom                        : build_attribute('$2', []).
attribute -> '-' atom attr_val               : build_attribute('$2', '$3').
attribute -> '-' spec type_spec              : build_attribute('$2', ['$3']).
attribute -> '-' callback type_spec          : build_attribute('$2', ['$3']).
attribute -> '-' define_expr macro_def_expr  : build_macro_def('$1', '$3').
attribute -> '-' define_clause macro_def_clause : build_macro_def('$1', '$3').

type_spec -> spec_fun type_sigs : {spec, ?anno('$1'), '$1', '$2'}.
type_spec -> '(' spec_fun type_sigs ')' : {spec, ?anno('$2'), '$2', '$3'}.

spec_fun -> atom_or_var_or_macro : '$1'.
spec_fun -> atom ':' atom : {remote, ?anno('$2'), '$1', '$3'}.

type_sigs -> type_sig : ['$1'].
type_sigs -> type_sig ';' type_sigs : ['$1'|'$3'].

type_sig -> type_argument_list '->' type :
    {clause, element(2, '$1'), spec, element(1, '$1'), [], ['$3']}.
type_sig -> type_argument_list '->' type 'when' types :
    {clause, element(2, '$1'), spec, element(1, '$1'), ['$5'], ['$3']}.

type -> type '::' type : {typed, ?anno('$2'), '$1', '$3'}.
type -> type '|' type : ?mkop2('$1', '$2', '$3').
type -> type '..' type : ?mkop2('$1', '$2', '$3').
type -> macro_call_type : '$1'.
type -> type add_op type : ?mkop2('$1', '$2', '$3').
type -> type mult_op type : ?mkop2('$1', '$2', '$3').
type -> prefix_op type : ?mkop1('$1', '$2').
type -> '(' type ')' : set_parens('$2').
type -> var : '$1'.
type -> atom : '$1'.
type -> atom type_argument_list : {call, ?anno('$1'), '$1', element(1, '$2')}.
type -> atom ':' atom type_argument_list : {call, ?anno('$1'), {remote, ?anno('$2'), '$1', '$3'}, element(1, '$4')}.
type -> '[' ']' : {list, ?anno('$1'), []}.
type -> '[' type ']' : {list, ?anno('$1'), ['$2']}.
type -> '[' type ',' '...' ']' : {list, ?anno('$1'), ['$2', '$4']}.
type -> '#' '{' '}' : {map, ?anno('$1'), []}.
type -> '#' '{' type_map_fields '}' : {map, ?anno('$1'), '$3'}.
type -> '{' '}' : {tuple, ?anno('$1'), []}.
type -> '{' types '}' : {tuple, ?anno('$1'), '$2'}.
type -> '#' record_name '{' '}' : {record, ?anno('$1'), '$2', []}.
type -> '#' record_name '{' types '}' : {record, ?anno('$1'), '$2', '$4'}.
type -> macro_call_none '{' '}' : {record, ?anno('$1'), '$1', []}.
type -> macro_call_none '{' types '}' : {record, ?anno('$1'), '$1', '$3'}.
type -> binary_type : '$1'.
type -> integer : '$1'.
type -> char : '$1'.
type -> fun_type : '$1'.

fun_type -> 'fun' '(' ')' :
    {'fun', ?anno('$1'), type}.
fun_type -> 'fun' '(' '(' '...' ')' '->' type ')' :
    {'fun', ?anno('$1'), {type, ['$4'], '$7'}}.
fun_type -> 'fun' '(' type_argument_list '->' type ')' :
    {'fun', ?anno('$1'), {type, element(1, '$3'), '$5'}}.

type_map_fields -> type_map_field : ['$1'].
type_map_fields -> type_map_field ',' type_map_fields : ['$1' | '$3'].

type_map_field -> type '=>' type :
    {map_field_assoc, ?anno('$2'), '$1', '$3'}.
type_map_field -> type ':=' type :
    {map_field_exact, ?anno('$2'), '$1', '$3'}.

binary_type -> '<<' '>>' : {bin, ?anno('$1'), []}.
binary_type -> '<<' bin_element_type '>>' : {bin, ?anno('$1'), ['$2']}.
binary_type -> '<<' bin_element_type ',' bin_element_type '>>' : {bin, ?anno('$1'), ['$2', '$4']}.

bin_element_type -> var ':' type :
    {bin_element, ?anno('$1'), '$1', '$3', default}.
bin_element_type -> var ':' var '*' type :
    %% We use a different node instead of regular operator
    %% since the precedence rules are different.
    {bin_element, ?anno('$1'), '$1', {bin_size, ?anno('$4'), '$3', '$5'}, default}.

attr_val -> expr                     : [delete_parens('$1')].
attr_val -> expr ',' exprs           : ['$1' | '$3'].
attr_val -> '(' expr ',' exprs ')'   : ['$2' | '$4'].

function -> function_clauses :
    {function,?anno(hd('$1')),'$1'}.

function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom_or_var clause_args clause_guard clause_body :
    {clause,?anno('$1'),'$1','$2','$3','$4'}.
function_clause -> macro_call_expr clause_guard clause_body :
    {macro_call,A,Name,Args} = '$1',
    {clause,A,{macro_call,A,Name,none},Args,'$2','$3'}.
function_clause -> macro_call_expr :
    '$1'.

clause_args -> pat_argument_list : element(1, '$1').

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' exprs: '$2'.

expr -> 'catch' expr : ?mkop1('$1', '$2').
expr -> expr '=' expr : ?mkop2('$1', '$2', '$3').
expr -> expr '!' expr : ?mkop2('$1', '$2', '$3').
expr -> expr 'orelse' expr : ?mkop2('$1', '$2', '$3').
expr -> expr 'andalso' expr : ?mkop2('$1', '$2', '$3').
expr -> expr comp_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr list_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr add_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr mult_op expr : ?mkop2('$1', '$2', '$3').
expr -> prefix_op expr : ?mkop1('$1', '$2').
expr -> expr '::' type : {typed, ?anno('$2'), '$1', '$3'}.
expr -> map_expr : '$1'.
expr -> function_call : '$1'.
expr -> record_expr : '$1'.
expr -> expr_max_remote : '$1'.

expr_max_remote -> expr_max ':' expr_max : {remote,?anno('$2'),'$1','$3'}.
expr_max_remote -> expr_max : '$1'.

expr_max -> macro_call_expr : '$1'.
expr_max -> macro_record_or_concatable : '$1'.
expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
expr_max -> '(' expr ')' : set_parens('$2').
expr_max -> 'begin' exprs 'end' : {block,?anno('$1'),'$2'}.
expr_max -> if_expr : '$1'.
expr_max -> case_expr : '$1'.
expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
expr_max -> try_expr : '$1'.

pat_expr -> pat_expr '=' pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr comp_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr list_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr add_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr mult_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> prefix_op pat_expr : ?mkop1('$1', '$2').
pat_expr -> map_pat_expr : '$1'.
pat_expr -> record_pat_expr : '$1'.
pat_expr -> pat_expr_max : '$1'.

pat_expr_max -> macro_call_pat : '$1'.
pat_expr_max -> var : '$1'.
pat_expr_max -> atomic : '$1'.
pat_expr_max -> list : '$1'.
pat_expr_max -> binary : '$1'.
pat_expr_max -> tuple : '$1'.
pat_expr_max -> '(' pat_expr ')' : set_parens('$2').

map_pat_expr -> '#' map_tuple :
        {map, ?anno('$1'),'$2'}.
map_pat_expr -> pat_expr_max '#' map_tuple :
        {map, ?anno('$2'),'$1','$3'}.
map_pat_expr -> map_pat_expr '#' map_tuple :
        {map, ?anno('$2'),'$1','$3'}.

record_pat_expr -> '#' record_name '.' record_field_name :
        {record_index,?anno('$1'),'$2','$4'}.
record_pat_expr -> '#' record_name record_tuple :
        {record,?anno('$1'),'$2','$3'}.

list -> '[' ']' : {list, ?anno('$1'), []}.
list -> '[' list_exprs ']' : {list, ?anno('$1'), '$2'}.

list_exprs -> expr : ['$1'].
list_exprs -> expr '|' expr : [{cons, ?anno('$2'), '$1', '$3'}].
list_exprs -> expr ',' list_exprs : ['$1' | '$3'].

binary -> '<<' '>>' : {bin,?anno('$1'),[]}.
binary -> '<<' bin_elements '>>' : {bin,?anno('$1'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
    Anno = erlfmt_recomment:delete_anno(parens, ?anno('$1')),
    {bin_element,Anno,'$1','$2','$3'}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> macro_call_none  : '$1'.
bit_type -> atom             : '$1'.
bit_type -> atom ':' integer : {'$1', '$3'}.

bit_size_expr -> expr_max : '$1'.

list_comprehension -> '[' expr '||' lc_exprs ']' :
        {lc,?anno('$1'),'$2','$4'}.
binary_comprehension -> '<<' expr_max '||' lc_exprs '>>' :
        {bc,?anno('$1'),'$2','$4'}.
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate,?anno('$2'),'$1','$3'}.
lc_expr -> binary '<=' expr : {b_generate,?anno('$2'),'$1','$3'}.

tuple -> '{' '}' : {tuple,?anno('$1'),[]}.
tuple -> '{' exprs '}' : {tuple,?anno('$1'),'$2'}.

map_expr -> '#' map_tuple :
    {map, ?anno('$1'),'$2'}.
map_expr -> expr_max '#' map_tuple :
    {map, ?anno('$2'),'$1','$3'}.
map_expr -> map_expr '#' map_tuple :
    {map, ?anno('$2'),'$1','$3'}.

map_tuple -> '{' '}' : [].
map_tuple -> '{' map_fields '}' : '$2'.

map_fields -> map_field : ['$1'].
map_fields -> map_field ',' map_fields : ['$1' | '$3'].

map_field -> map_key '=>' expr :
    {map_field_assoc, ?anno('$2'), '$1', '$3'}.
map_field -> map_key ':=' expr :
    {map_field_exact, ?anno('$2'), '$1', '$3'}.

map_key -> expr : '$1'.

%% N.B. This is called from expr.
%% N.B. Field names are returned as the complete object, even if they are
%% always atoms for the moment, this might change in the future.

record_expr -> '#' record_name '.' record_field_name :
    {record_index,?anno('$1'),'$2','$4'}.
record_expr -> '#' record_name record_tuple :
    {record,?anno('$1'),'$2','$3'}.
record_expr -> expr_max '#' record_name '.' record_field_name :
    {record_field,?anno('$2'),'$1','$3','$5'}.
record_expr -> expr_max '#' record_name record_tuple :
    {record,?anno('$2'),'$1','$3','$4'}.
record_expr -> record_expr '#' record_name '.' record_field_name :
    {record_field,?anno('$2'),'$1','$3','$5'}.
record_expr -> record_expr '#' record_name record_tuple :
    {record,?anno('$2'),'$1','$3','$4'}.

macro_record_or_concatable -> var macro_call_none '.' record_field_name :
    {record_field,?anno('$2'),'$1','$2','$4'}.
macro_record_or_concatable -> var macro_call_none record_tuple :
    {record,?anno('$2'),'$1','$2','$3'}.
macro_record_or_concatable -> var concatables :
    {concat, ?anno('$1'), ['$1' | '$2']}.

record_tuple -> '{' '}' : [].
record_tuple -> '{' record_fields '}' : '$2'.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> var '=' expr : {record_field,?anno('$1'),'$1','$3'}.
record_field -> atom '=' expr : {record_field,?anno('$1'),'$1','$3'}.

record_name -> atom : '$1'.
record_name -> macro_call_none : '$1'.

record_field_name -> atom : '$1'.
record_field_name -> var : '$1'.
record_field_name -> macro_call_none : '$1'.

function_call -> expr_max_remote argument_list :
    {call,?anno('$1'),'$1',element(1, '$2')}.

if_expr -> 'if' if_clauses 'end' : {'if',?anno('$1'),'$2'}.

if_clauses -> if_clause : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

if_clause -> guard clause_body :
        {clause,?anno(hd(hd('$1'))),'if',[],'$1','$2'}.

case_expr -> 'case' expr 'of' cr_clauses 'end' :
        {'case',?anno('$1'),'$2','$4'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

%% FIXME: merl in syntax_tools depends on patterns in a 'case' being
%% full expressions. Therefore, we can't use pat_expr here. There
%% should be a better way.

cr_clause -> expr clause_guard clause_body :
        {clause,?anno('$1'),'case',['$1'],'$2','$3'}.

receive_expr -> 'receive' cr_clauses 'end' :
        {'receive',?anno('$1'),'$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
        {'receive',?anno('$1'),[],'$3','$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
        {'receive',?anno('$1'),'$2','$4','$5'}.

fun_expr -> 'fun' atom_or_var_or_macro '/' integer_or_var_or_macro :
    {'fun',?anno('$1'),{function,'$2','$4'}}.
fun_expr -> 'fun' atom_or_var_or_macro ':' atom_or_var_or_macro '/' integer_or_var_or_macro :
    {'fun',?anno('$1'),{function,'$2','$4','$6'}}.
fun_expr -> 'fun' fun_clauses 'end' :
    {'fun',?anno('$1'),{clauses,'$2'}}.

atom_or_var -> atom : '$1'.
atom_or_var -> var : '$1'.

atom_or_var_or_macro -> atom_or_var : '$1'.
atom_or_var_or_macro -> macro_call_none : '$1'.

integer_or_var_or_macro -> integer : '$1'.
integer_or_var_or_macro -> var : '$1'.
integer_or_var_or_macro -> macro_call_none : '$1'.

fun_clauses -> fun_clause : ['$1'].
fun_clauses -> fun_clause ';' fun_clauses : ['$1' | '$3'].

fun_clause -> pat_argument_list clause_guard clause_body :
    {Args,Anno} = '$1',
    {clause,Anno,'fun',Args,'$2','$3'}.
fun_clause -> var pat_argument_list clause_guard clause_body :
    {clause,?anno('$1'),'$1',element(1, '$2'),'$3','$4'}.

try_expr -> 'try' exprs 'of' cr_clauses try_catch :
        build_try(?anno('$1'),'$2','$4','$5').
try_expr -> 'try' exprs try_catch :
        build_try(?anno('$1'),'$2',[],'$3').

try_catch -> 'catch' try_clauses 'end' :
        {'$2',[]}.
try_catch -> 'catch' try_clauses 'after' exprs 'end' :
        {'$2','$4'}.
try_catch -> 'after' exprs 'end' :
        {[],'$2'}.

try_clauses -> try_clause : ['$1'].
try_clauses -> try_clause ';' try_clauses : ['$1' | '$3'].

try_clause -> pat_expr clause_guard clause_body :
        {clause,?anno('$1'),'catch',['$1'],'$2','$3'}.
try_clause -> atom_or_var ':' pat_expr try_opt_stacktrace clause_guard clause_body :
        {clause,?anno('$1'),'catch',['$1','$3'|'$4'],'$5','$6'}.

try_opt_stacktrace -> ':' var : ['$2'].
try_opt_stacktrace -> '$empty' : [].

macro_def_expr -> '(' macro_name ',' macro_def_expr_body ')' : {'$2', '$4'}.

macro_def_clause -> '(' macro_name ',' function_clause ')' : {'$2', '$4'}.

macro_def_expr_body -> '$empty' : empty.
macro_def_expr_body -> '#' atom : {record_name, ?anno('$1'), '$2'}.
macro_def_expr_body -> guard : '$1'.

macro_name -> atom_or_var : {macro_call, ?anno('$1'), '$1', none}.
macro_name -> atom_or_var '(' ')' : {macro_call, ?anno('$1'), '$1', []}.
macro_name -> atom_or_var '(' vars ')' : {macro_call, ?anno('$1'), '$1', '$3'}.

macro_call_expr -> macro_string :
    '$1'.
macro_call_expr -> macro_call_none :
    '$1'.
macro_call_expr -> '?' atom_or_var '(' ')' :
    {macro_call, ?anno('$1'), '$2', []}.
macro_call_expr -> '?' atom_or_var '(' macro_exprs ')' :
    {macro_call, ?anno('$1'), '$2', '$4'}.
macro_call_expr -> '?' atom_or_var record_tuple :
    {record, ?anno('$1'), {macro_call, ?anno('$1'), '$2', none}, '$3'}.
macro_call_expr -> '?' atom_or_var '.' atom :
    {record_index, ?anno('$1'), {macro_call, ?anno('$1'), '$2', none} ,'$4'}.

macro_call_pat -> macro_call_none :
    '$1'.
macro_call_pat -> '?' atom_or_var '(' ')' :
    {macro_call, ?anno('$1'), '$2', []}.
macro_call_pat -> '?' atom_or_var '(' pat_exprs ')' :
    {macro_call, ?anno('$1'), '$2', '$4'}.
macro_call_pat -> '?' atom_or_var record_tuple :
    {record, ?anno('$1'), {macro_call, ?anno('$1'), '$2', none}, '$3'}.
macro_call_pat -> '?' atom_or_var '.' atom :
    {record_index, ?anno('$1'), {macro_call, ?anno('$1'), '$2', none} ,'$4'}.

macro_call_type -> macro_call_none :
    '$1'.
macro_call_type -> '?' atom_or_var '(' ')' :
    {macro_call, ?anno('$1'), '$2', []}.
macro_call_type -> '?' atom_or_var '(' types ')' :
    {macro_call, ?anno('$1'), '$2', '$4'}.

macro_call_none -> '?' atom_or_var :
    {macro_call, ?anno('$1'), '$2', none}.

macro_string -> '?' '?' atom_or_var :
    {macro_string, ?anno('$1'), '$3'}.

macro_expr -> expr : '$1'.
macro_expr -> expr 'when' expr : {guard,?anno('$2'),'$1','$3'}.

argument_list -> '(' ')' : {[],?anno('$1')}.
argument_list -> '(' exprs ')' : {'$2',?anno('$1')}.

pat_argument_list -> '(' ')' : {[],?anno('$1')}.
pat_argument_list -> '(' pat_exprs ')' : {'$2',?anno('$1')}.

type_argument_list -> '(' ')' : {[], ?anno('$1')}.
type_argument_list -> '(' types ')' : {'$2', ?anno('$1')}.

exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

pat_exprs -> pat_expr : ['$1'].
pat_exprs -> pat_expr ',' pat_exprs : ['$1' | '$3'].

types -> type : ['$1'].
types -> type ',' types : ['$1' | '$3'].

macro_exprs -> macro_expr : ['$1'].
macro_exprs -> macro_expr ',' macro_exprs : ['$1' | '$3'].

vars -> var : ['$1'].
vars -> var ',' vars : ['$1' | '$3'].

guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1'|'$3'].

atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> string : '$1'.
atomic -> string concatables : {concat, ?anno('$1'), ['$1' | '$2']}.
atomic -> macro_call_none concatables : {concat, ?anno('$1'), ['$1' | '$2']}.
atomic -> macro_string concatables : {concat, ?anno('$1'), ['$1' | '$2']}.

concatables -> concatable : ['$1'].
concatables -> concatable concatables : ['$1' | '$2'].

concatable -> string : '$1'.
concatable -> var : '$1'.
concatable -> macro_call_none : '$1'.
concatable -> macro_string : '$1'.

prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not' : '$1'.

mult_op -> '/' : '$1'.
mult_op -> '*' : '$1'.
mult_op -> 'div' : '$1'.
mult_op -> 'rem' : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and' : '$1'.

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> 'bor' : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl' : '$1'.
add_op -> 'bsr' : '$1'.
add_op -> 'or' : '$1'.
add_op -> 'xor' : '$1'.

list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

comp_op -> '==' : '$1'.
comp_op -> '/=' : '$1'.
comp_op -> '=<' : '$1'.
comp_op -> '<' : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '>' : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.

Header
"%% This file was automatically generated from the file \"erlfmt_parse.yrl\"."
"%%"
"%% Copyright Ericsson AB 1996-2015. All Rights Reserved."
"%%"
"%% Licensed under the Apache License, Version 2.0 (the \"License\"); you may"
"%% not use this file except in compliance with the License. You may obtain"
"%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>"
"%%"
"%% Unless required by applicable law or agreed to in writing, software"
"%% distributed under the License is distributed on an \"AS IS\" BASIS,"
"%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
"%% See the License for the specific language governing permissions and"
"%% limitations under the License."
"".

Erlang code.

-oncall("whatsapp_erlang").

-typing([dialyzer]).

-export([parse_form/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-export_type([abstract_clause/0, abstract_expr/0, abstract_form/0,
              abstract_type/0, form_info/0, error_info/0]).

%% Start of Abstract Format

-type anno() :: erl_anno:anno().

-type abstract_form() ::
    af_function_decl() | af_attribute().

-type af_attribute() ::
    af_function_spec() |
    af_record_decl() |
    af_type_decl() |
    {attribute, anno(), atom(), [abstract_expr()]}.

-type af_function_spec() ::
    {attribute, anno(), spec | callback, {af_atom(), af_function_type_list()}} |
    {attribute, anno(), spec, {af_remote_function(af_atom()), af_function_type_list()}}.

-type af_record_decl() ::
    {attribute, anno(), record, {af_record_name(), [af_field_decl()]}}.

-type af_type_decl() ::
    {attribute, anno(), type | opaque, {af_local_call(), abstract_type()}}.

-type af_field_decl() :: af_typed_field() | af_field().

-type af_typed_field() :: {'typed_record_field', af_field(), abstract_type()}.

-type af_field() :: {'record_field', anno(), af_field_name()}
                  | {'record_field', anno(), af_field_name(), abstract_expr()}.

-type af_function_decl() :: {function, anno(), af_clause_seq()}.

-type abstract_expr() :: af_literal()
                       | af_variable()
                       | af_tuple(abstract_expr())
                       | af_list(abstract_expr())
                       | af_bin(abstract_expr())
                       | af_binary_op(abstract_expr())
                       | af_unary_op(abstract_expr())
                       | af_record_creation(abstract_expr())
                       | af_record_update(abstract_expr())
                       | af_record_index()
                       | af_record_field_access(abstract_expr())
                       | af_map_creation(abstract_expr())
                       | af_map_update(abstract_expr())
                       | af_local_call()
                       | af_remote_call()
                       | af_list_comprehension()
                       | af_binary_comprehension()
                       | af_block()
                       | af_if()
                       | af_case()
                       | af_try()
                       | af_receive()
                       | af_fun()
                       | af_macro_call().

-type af_record_update(T) :: {'record',
                              anno(),
                              abstract_expr(),
                              af_record_name(),
                              [af_record_field(T)]}.

-type af_local_call() :: {'call', anno(), af_local_function(), af_args()}.

-type af_remote_call() :: {'call', anno(), af_remote_function(abstract_expr()), af_args()}.

-type af_macro_call() ::
    {'macro_call', anno(), af_atom() | af_variable(), [abstract_expr()]}.

-type af_args() :: [abstract_expr()].

-type af_local_function() :: abstract_expr().

-type af_remote_function(Expr) :: {'remote', anno(), Expr, Expr}.

-type af_list_comprehension() ::
        {'lc', anno(), af_template(), af_qualifier_seq()}.

-type af_binary_comprehension() ::
        {'bc', anno(), af_template(), af_qualifier_seq()}.

-type af_template() :: abstract_expr().

-type af_qualifier_seq() :: [af_qualifier()].

-type af_qualifier() :: af_generator() | af_filter().

-type af_generator() :: {'generate', anno(), af_pattern(), abstract_expr()}
                      | {'b_generate', anno(), af_pattern(), abstract_expr()}.

-type af_filter() :: abstract_expr().

-type af_block() :: {'block', anno(), af_body()}.

-type af_if() :: {'if', anno(), af_clause_seq()}.

-type af_case() :: {'case', anno(), abstract_expr(), af_clause_seq()}.

-type af_try() :: {'try',
                   anno(),
                   af_body() | [],
                   af_clause_seq() | [],
                   af_clause_seq() | [],
                   af_body() | []}.

-type af_clause_seq() :: [af_clause(), ...].

-type af_receive() ::
        {'receive', anno(), af_clause_seq()}
      | {'receive', anno(), af_clause_seq(), abstract_expr(), af_body()}.

-type af_fun() ::
    {'fun', anno(), {'clauses', af_clause_seq()}} |
    {'fun', anno(), {function, abstract_expr(), abstract_expr()}} |
    {'fun', anno(), {function, abstract_expr(), abstract_expr(), abstract_expr()}}.

-type abstract_clause() :: af_clause().

-type af_clause() ::
        {'clause', anno(), af_clause_name(), [af_pattern()], af_guard_seq(), af_body()} |
        af_macro_call().

-type af_clause_name() :: af_atom() | af_variable() | af_macro_call() | 'fun' | 'case' | 'catch'.

-type af_body() :: [abstract_expr(), ...].

-type af_guard_seq() :: [af_guard()].

-type af_guard() :: [af_guard_test(), ...].

-type af_guard_test() :: af_literal()
                       | af_variable()
                       | af_tuple(af_guard_test())
                       | af_list(af_guard_test())
                       | af_bin(af_guard_test())
                       | af_binary_op(af_guard_test())
                       | af_unary_op(af_guard_test())
                       | af_record_creation(af_guard_test())
                       | af_record_index()
                       | af_record_field_access(af_guard_test())
                       | af_map_creation(abstract_expr())
                       | af_map_update(abstract_expr())
                       | af_guard_call()
                       | af_remote_guard_call().

-type af_record_field_access(T) ::
        {'record_field', anno(), T, af_record_name(), af_field_name()}.

-type af_map_creation(T) :: {'map', anno(), [af_assoc(T)]}.

-type af_map_update(T) :: {'map', anno(), T, [af_assoc(T)]}.

-type af_assoc(T) :: {'map_field_assoc', anno(), T, T}
                   | af_assoc_exact(T).

-type af_assoc_exact(T) :: {'map_field_exact', anno(), T, T}.

-type af_guard_call() :: {'call', anno(), function_name(), [af_guard_test()]}.

-type af_remote_guard_call() ::
        {'call', anno(),
         {'remote', anno(), af_lit_atom('erlang'), af_atom()},
         [af_guard_test()]}.

-type af_pattern() :: af_literal()
                    | af_variable()
                    | af_tuple(af_pattern())
                    | af_list(af_pattern())
                    | af_bin(af_pattern())
                    | af_binary_op(af_pattern())
                    | af_unary_op(af_pattern())
                    | af_record_creation(af_pattern())
                    | af_record_index()
                    | af_map_pattern().

-type af_record_index() ::
        {'record_index', anno(), af_record_name(), af_field_name()}.

-type af_record_creation(T) ::
        {'record', anno(), af_record_name(), [af_record_field(T)]}.

-type af_record_field(T) :: {'record_field', anno(), af_field_name(), T}.

-type af_map_pattern() ::
        {'map', anno(), [af_assoc_exact(abstract_expr())]}.

-type abstract_type() :: af_annotated_type()
                       | af_atom()
                       | af_bitstring_type()
                       | af_empty_list_type()
                       | af_fun_type()
                       | af_integer_range_type()
                       | af_map_type()
                       | af_predefined_type()
                       | af_record_type()
                       | af_remote_type()
                       | af_singleton_integer_type()
                       | af_tuple_type()
                       | af_type_union()
                       | af_type_variable()
                       | af_user_defined_type().

-type af_annotated_type() ::
        {'ann_type', anno(), [af_anno() | abstract_type()]}. % [Var, Type]

-type af_anno() :: af_variable().

-type af_bitstring_type() ::
        {'type', anno(), 'binary', [af_singleton_integer_type()]}.

-type af_empty_list_type() :: {'type', anno(), 'nil', []}.

-type af_fun_type() :: {'type', anno(), 'fun', []}
                     | {'type', anno(), 'fun', [{'type', anno(), 'any'} |
                                                abstract_type()]}
                     | af_function_type().

-type af_integer_range_type() ::
        {'type', anno(), 'range', [af_singleton_integer_type()]}.

-type af_map_type() :: {'type', anno(), 'map', 'any'}
                     | {'type', anno(), 'map', [af_assoc_type()]}.

-type af_assoc_type() ::
        {'type', anno(), 'map_field_assoc', [abstract_type()]}
      | {'type', anno(), 'map_field_exact', [abstract_type()]}.

-type af_predefined_type() ::
        {'type', anno(), type_name(),  [abstract_type()]}.

-type af_record_type() ::
        {'type', anno(), 'record', [(Name :: af_atom()) % [Name, T1, ... Tk]
                                    | af_record_field_type()]}.

-type af_record_field_type() ::
        {'type', anno(), 'field_type', [(Name :: af_atom()) |
                                        abstract_type()]}. % [Name, Type]

-type af_remote_type() ::
        {'remote_type', anno(), [(Module :: af_atom()) |
                                 (TypeName :: af_atom()) |
                                 [abstract_type()]]}. % [Module, Name, [T]]

-type af_tuple_type() :: {'type', anno(), 'tuple', 'any'}
                       | {'type', anno(), 'tuple', [abstract_type()]}.

-type af_type_union() :: {'type', anno(), 'union', [abstract_type()]}.

-type af_type_variable() :: {'var', anno(), atom()}. % except '_'

-type af_user_defined_type() ::
        {'user_type', anno(), type_name(),  [abstract_type()]}.

-type af_function_type_list() :: [af_constrained_function_type() |
                                  af_function_type()].

-type af_constrained_function_type() ::
        {'type', anno(), 'bounded_fun', [af_function_type() | % [Ft, Fc]
                                         af_function_constraint()]}.

-type af_function_type() ::
        {'type', anno(), 'fun',
         [{'type', anno(), 'product', [abstract_type()]} | abstract_type()]}.

-type af_function_constraint() :: [af_constraint()].

-type af_constraint() :: {'type', anno(), 'constraint',
                          [af_lit_atom('is_subtype') |
                           [af_type_variable() | abstract_type()]]}. % [IsSubtype, [V, T]]

-type af_singleton_integer_type() :: af_integer()
                                   | af_character()
                                   | af_unary_op(af_singleton_integer_type())
                                   | af_binary_op(af_singleton_integer_type()).

-type af_literal() :: af_atom()
                    | af_character()
                    | af_float()
                    | af_integer()
                    | af_string().

-type af_atom() :: af_lit_atom(atom()).

-type af_lit_atom(A) :: {'atom', anno(), A}.

-type af_character() :: {'char', anno(), char()}.

-type af_float() :: {'float', anno(), float()}.

-type af_integer() :: {'integer', anno(), non_neg_integer()}.

-type af_string() :: {'string', anno(), string()}.

-type af_variable() :: {'var', anno(), atom()}.

-type af_tuple(T) :: {'tuple', anno(), [T]}.

-type af_list(T) :: {'list', anno(), [T | {cons, anno(), T, T}]}.

-type af_bin(T) :: {'bin', anno(), [af_binelement(T)]}.

-type af_binelement(T) :: {'bin_element',
                           anno(),
                           T,
                           af_binelement_size(),
                           type_specifier_list()}.

-type af_binelement_size() :: 'default' | abstract_expr().

-type af_binary_op(T) :: {'op', anno(), binary_op(), T, T}.

-type binary_op() :: '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-'
                   | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++'
                   | '--' | '==' | '/=' | '=<' | '<'  | '>=' | '>' | '=:='
                   | '=/=' | '='.

-type af_unary_op(T) :: {'op', anno(), unary_op(), T}.

-type unary_op() :: '+' | '-' | 'bnot' | 'not' | 'catch'.

%% See also lib/stdlib/{src/erl_bits.erl,include/erl_bits.hrl}.
-type type_specifier_list() :: 'default' | [type_specifier(), ...].

-type type_specifier() :: type()
                        | signedness()
                        | endianness()
                        | unit().

-type type() :: 'integer'
              | 'float'
              | 'binary'
              | 'bytes'
              | 'bitstring'
              | 'bits'
              | 'utf8'
              | 'utf16'
              | 'utf32'.

-type signedness() :: 'signed' | 'unsigned'.

-type endianness() :: 'big' | 'little' | 'native'.

-type unit() :: {'unit', 1..256}.

-type af_record_name() :: af_local_record_name() | af_remote_record_name().

-type af_local_record_name() :: af_atom().

-type af_remote_record_name() :: {remote, anno(), af_atom(), af_atom()}.

-type af_field_name() :: af_atom().

-type function_name() :: atom().

-type type_name() :: atom().

-type form_info() :: {'eof', erl_anno:line()}
                   | {'error', erl_scan:error_info() | error_info()}
                   | {'warning', erl_scan:error_info() | error_info()}.

%% End of Abstract Format

%% XXX. To be refined.
-type error_description() :: term().
-type error_info() :: {erl_anno:line(), module(), error_description()}.
-type token() :: erl_scan:token().

%% mkop(Op, Arg) -> {op,Anno,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Anno,Op,Left,Right}.

-define(mkop2(L, OpAnno, R),
        begin
            {Op,Anno} = OpAnno,
            {op,Anno,Op,L,R}
        end).

-define(mkop1(OpAnno, A),
        begin
            {Op,Anno} = OpAnno,
            {op,Anno,Op,A}
        end).

%% keep track of annotation info in tokens
-define(anno(Tup), element(2, Tup)).

%% Entry points compatible to old erl_parse.

-spec parse_form(Tokens) -> {ok, AbsForm} | {error, ErrorInfo} when
      Tokens :: [token()],
      AbsForm :: abstract_form(),
      ErrorInfo :: error_info().
parse_form([{'-',A1},{atom,A2,spec}|Tokens]) ->
    NewTokens = [{'-',A1},{'spec',A2}|Tokens],
    parse(NewTokens);
parse_form([{'-',A1},{atom,A2,callback}|Tokens]) ->
    NewTokens = [{'-',A1},{'callback',A2}|Tokens],
    parse(NewTokens);
parse_form([{'-',A1},{atom,A2,define}|Tokens]) ->
    NewTokens1 = [{'-',A1},{define_expr,A2}|Tokens],
    case parse(NewTokens1) of
        {ok, _} = Res ->
            Res;
        _ ->
            NewTokens3 = [{'-',A1},{define_clause,A2}|Tokens],
            parse(NewTokens3)
    end;
parse_form(Tokens) ->
    parse(Tokens).

build_macro_def({_, Anno}, {Name, Body}) ->
    {attribute, Anno, define, [Name, Body]}.

build_attribute({atom, Anno, record}, [Name, Tuple]) ->
    {attribute, Anno, record, [Name, record_tuple(Tuple)]};
build_attribute({atom, Anno, Name}, Values) ->
    {attribute, Anno, Name, Values};
build_attribute({Name, Anno}, Values) ->
    {attribute, Anno, Name, Values}.

record_tuple({tuple,At,Fields}) ->
    {tuple,At,record_fields(Fields)};
record_tuple(Other) ->
    ret_err(?anno(Other), "bad record declaration").

record_fields([{atom,Aa,A}|Fields]) ->
    [{record_field,Aa,{atom,Aa,A}}|record_fields(Fields)];
record_fields([{op,_Am,'=',{atom,Aa,A},Expr}|Fields]) ->
    [{record_field,Aa,{atom,Aa,A},Expr}|record_fields(Fields)];
record_fields([{typed,Am,Expr,TypeInfo}|Fields]) ->
    [Field] = record_fields([Expr]),
    [{typed,Am,Field,TypeInfo}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    ret_err(?anno(Other), "bad record field");
record_fields([]) -> [].

build_try(A,Es,Scs,{Ccs,As}) ->
    {'try',A,Es,Scs,Ccs,As}.

-spec ret_err(_, _) -> no_return().
ret_err(Anno, S) ->
    return_error(erl_anno:location(Anno), S).

%% TODO: figure out a better way to manage annos
set_parens(Expr) ->
    Anno = erlfmt_recomment:put_anno(parens, true, ?anno(Expr)),
    setelement(2, Expr, Anno).

delete_parens(Expr) ->
    Anno = erlfmt_recomment:delete_anno(parens, ?anno(Expr)),
    setelement(2, Expr, Anno).
