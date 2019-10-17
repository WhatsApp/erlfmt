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
expr expr_max
pat_expr pat_expr_max map_pat_expr record_pat_expr
pat_argument_list pat_exprs
list tail
list_comprehension lc_expr lc_exprs
binary_comprehension
tuple
record_expr record_name record_tuple record_field record_fields
map_expr map_tuple map_field map_field_assoc map_field_exact map_fields map_key
if_expr if_clause if_clauses case_expr cr_clause cr_clauses receive_expr
fun_expr fun_clause fun_clauses
atom_or_var atom_or_var_or_macro integer_or_var_or_macro
try_expr try_catch try_clause try_clauses try_opt_stacktrace
function_call argument_list
exprs guard vars
atomic concatable concatables
prefix_op mult_op add_op list_op comp_op
binary bin_elements bin_element bit_expr
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type
top_type top_types type typed_expr typed_attr_val
type_sig type_sigs type_guard type_guards fun_type fun_type_anon binary_type
type_spec spec_fun typed_exprs typed_record_fields field_types field_type
map_pair_types map_pair_type
bin_base_type bin_unit_type
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

Unary 0 'catch'.
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

Right 0 '::'.
Left 100 '|'.
Nonassoc 200 '..'.
Nonassoc 200 '*'. % for binary expressions

form -> attribute dot : '$1'.
form -> function dot : '$1'.

attribute -> '-' 'if' attr_val               : build_attribute({atom,?anno('$2'),'if'}, '$3').
attribute -> '-' atom                        : build_attribute('$2', []).
attribute -> '-' atom attr_val               : build_attribute('$2', '$3').
attribute -> '-' atom typed_attr_val         : build_typed_attribute('$2','$3').
attribute -> '-' atom '(' typed_attr_val ')' : build_typed_attribute('$2','$4').
attribute -> '-' spec type_spec              : build_type_spec('$2', '$3').
attribute -> '-' callback type_spec          : build_type_spec('$2', '$3').
attribute -> '-' define_expr macro_def_expr  : build_macro_def(expr, '$1', '$3').
attribute -> '-' define_clause macro_def_clause : build_macro_def(clause, '$1', '$3').

type_spec -> spec_fun type_sigs : {'$1', '$2'}.
type_spec -> '(' spec_fun type_sigs ')' : {'$2', '$3'}.

spec_fun ->                           atom : '$1'.
spec_fun ->                  atom ':' atom : {'$1', '$3'}.

typed_attr_val -> expr ',' typed_record_fields : {typed_record, '$1', '$3'}.
typed_attr_val -> expr '::' top_type           : {type_def, '$1', '$3'}.

typed_record_fields -> '{' typed_exprs '}' : {tuple, ?anno('$1'), '$2'}.

typed_exprs -> typed_expr                 : ['$1'].
typed_exprs -> typed_expr ',' typed_exprs : ['$1'|'$3'].
typed_exprs -> expr ',' typed_exprs       : ['$1'|'$3'].
typed_exprs -> typed_expr ',' exprs       : ['$1'|'$3'].

typed_expr -> expr '::' top_type          : {typed,'$1','$3'}.

type_sigs -> type_sig                     : ['$1'].
type_sigs -> type_sig ';' type_sigs       : ['$1'|'$3'].

type_sig -> fun_type                      : '$1'.
type_sig -> fun_type 'when' type_guards   : {type, ?anno('$1'), bounded_fun,
                                             ['$1','$3']}.

type_guards -> type_guard                 : ['$1'].
type_guards -> type_guard ',' type_guards : ['$1'|'$3'].

type_guard -> atom '(' top_types ')'   : build_compat_constraint('$1', '$3').
type_guard -> var '::' top_type        : build_constraint('$1', '$3').

top_types -> top_type                     : ['$1'].
top_types -> top_type ',' top_types       : ['$1'|'$3'].

top_type -> var '::' top_type             : {ann_type, ?anno('$1'), ['$1','$3']}.
top_type -> type '|' top_type             : lift_unions('$1','$3').
top_type -> type                          : '$1'.

type -> type '..' type                    : {type, ?anno('$1'), range,
                                             ['$1', '$3']}.
type -> macro_call_type                   : '$1'.
type -> type add_op type                  : ?mkop2('$1', '$2', '$3').
type -> type mult_op type                 : ?mkop2('$1', '$2', '$3').
type -> prefix_op type                    : ?mkop1('$1', '$2').
type -> '(' top_type ')'                  : '$2'.
type -> var                               : '$1'.
type -> atom                              : '$1'.
type -> atom '(' ')'                      : build_gen_type('$1').
type -> atom '(' top_types ')'            : build_type('$1', '$3').
type -> atom ':' atom '(' ')'             : {remote_type, ?anno('$1'),
                                             ['$1', '$3', []]}.
type -> atom ':' atom '(' top_types ')'   : {remote_type, ?anno('$1'),
                                             ['$1', '$3', '$5']}.
type -> '[' ']'                           : {type, ?anno('$1'), nil, []}.
type -> '[' top_type ']'                  : {type, ?anno('$1'), list, ['$2']}.
type -> '[' top_type ',' '...' ']'        : {type, ?anno('$1'),
                                             nonempty_list, ['$2']}.
type -> '#' '{' '}'                       : {type, ?anno('$1'), map, []}.
type -> '#' '{' map_pair_types '}'        : {type, ?anno('$1'), map, '$3'}.
type -> '{' '}'                           : {type, ?anno('$1'), tuple, []}.
type -> '{' top_types '}'                 : {type, ?anno('$1'), tuple, '$2'}.
type -> '#' record_name '{' '}'           : {type, ?anno('$1'), record, ['$2']}.
type -> '#' record_name '{' field_types '}' : {type, ?anno('$1'), record, ['$2'|'$4']}.
type -> binary_type                       : '$1'.
type -> integer                           : '$1'.
type -> char                              : '$1'.
type -> 'fun' '(' ')'                     : {type, ?anno('$1'), 'fun', []}.
type -> 'fun' '(' fun_type_anon ')'       : '$3'.
type -> 'fun' '(' fun_type ')'            : '$3'.

fun_type_anon -> '(' '...' ')' '->' top_type
                                          : {type, ?anno('$1'), 'fun',
                                             [{type, ?anno('$1'), any}, '$5']}.

fun_type -> '(' ')' '->' top_type  : {type, ?anno('$1'), 'fun',
                                      [{type, ?anno('$1'), product, []}, '$4']}.
fun_type -> '(' top_types ')' '->' top_type
                                   : {type, ?anno('$1'), 'fun',
                                      [{type, ?anno('$1'), product, '$2'},'$5']}.

map_pair_types -> map_pair_type                    : ['$1'].
map_pair_types -> map_pair_type ',' map_pair_types : ['$1'|'$3'].

map_pair_type  -> top_type '=>' top_type  : {type, ?anno('$2'),
                                             map_field_assoc,['$1','$3']}.
map_pair_type  -> top_type ':=' top_type  : {type, ?anno('$2'),
                                             map_field_exact,['$1','$3']}.

field_types -> field_type                 : ['$1'].
field_types -> field_type ',' field_types : ['$1'|'$3'].

field_type -> atom '::' top_type          : {type, ?anno('$1'), field_type,
                                             ['$1', '$3']}.

binary_type -> '<<' '>>' : {type,?anno('$1'),binary,[{integer,?anno('$1'),0},{integer,?anno('$1'),0}]}.
binary_type -> '<<' bin_base_type '>>' : {type,?anno('$1'),binary,['$2',{integer,?anno('$1'),0}]}.
binary_type -> '<<' bin_unit_type '>>' : {type,?anno('$1'),binary,[{integer,?anno('$1'),0},'$2']}.
binary_type -> '<<' bin_base_type ',' bin_unit_type '>>' : {type,?anno('$1'),binary,['$2','$4']}.

bin_base_type -> var ':' type          : build_bin_type(['$1'], '$3').

bin_unit_type -> var ':' var '*' type  : build_bin_type(['$1', '$3'], '$5').

attr_val -> expr                     : ['$1'].
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

expr -> 'catch' expr : {'catch',?anno('$1'),'$2'}.
expr -> expr '=' expr : {match,?anno('$2'),'$1','$3'}.
expr -> expr '!' expr : ?mkop2('$1', '$2', '$3').
expr -> expr 'orelse' expr : ?mkop2('$1', '$2', '$3').
expr -> expr 'andalso' expr : ?mkop2('$1', '$2', '$3').
expr -> expr comp_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr list_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr add_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr mult_op expr : ?mkop2('$1', '$2', '$3').
expr -> prefix_op expr : ?mkop1('$1', '$2').
expr -> map_expr : '$1'.
expr -> function_call : '$1'.
expr -> record_expr : '$1'.
expr -> expr_max : '$1'.

expr_max -> expr_max ':' expr_max : {remote,?anno('$2'),'$1','$3'}.
expr_max -> macro_call_expr : '$1'.
expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
expr_max -> '(' expr ')' : '$2'.
expr_max -> 'begin' exprs 'end' : {block,?anno('$1'),'$2'}.
expr_max -> if_expr : '$1'.
expr_max -> case_expr : '$1'.
expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
expr_max -> try_expr : '$1'.

pat_expr -> pat_expr '=' pat_expr : {match,?anno('$2'),'$1','$3'}.
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
pat_expr_max -> '(' pat_expr ')' : '$2'.

map_pat_expr -> '#' map_tuple :
        {map, ?anno('$1'),'$2'}.
map_pat_expr -> pat_expr_max '#' map_tuple :
        {map, ?anno('$2'),'$1','$3'}.
map_pat_expr -> map_pat_expr '#' map_tuple :
        {map, ?anno('$2'),'$1','$3'}.

record_pat_expr -> '#' record_name '.' atom :
        {record_index,?anno('$1'),'$2','$4'}.
record_pat_expr -> '#' record_name record_tuple :
        {record,?anno('$1'),'$2','$3'}.

list -> '[' ']' : {nil,?anno('$1')}.
list -> '[' expr tail : {cons,?anno('$1'),'$2','$3'}.

tail -> ']' : {nil,?anno('$1')}.
tail -> '|' expr ']' : '$2'.
tail -> ',' expr tail : {cons,?anno('$2'),'$2','$3'}.


binary -> '<<' '>>' : {bin,?anno('$1'),[]}.
binary -> '<<' bin_elements '>>' : {bin,?anno('$1'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
        {bin_element,?anno('$1'),'$1','$2','$3'}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> atom             : element(3,'$1').
bit_type -> atom ':' integer : { element(3,'$1'), element(3,'$3') }.

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

map_field -> map_field_assoc : '$1'.
map_field -> map_field_exact : '$1'.

map_field_assoc -> map_key '=>' expr :
        {map_field_assoc,?anno('$1'),'$1','$3'}.

map_field_exact -> map_key ':=' expr :
        {map_field_exact,?anno('$1'),'$1','$3'}.

map_key -> expr : '$1'.

%% N.B. This is called from expr.
%% N.B. Field names are returned as the complete object, even if they are
%% always atoms for the moment, this might change in the future.

record_expr -> '#' record_name '.' atom :
    {record_index,?anno('$1'),'$2','$4'}.
record_expr -> '#' record_name record_tuple :
    {record,?anno('$1'),'$2','$3'}.
record_expr -> expr_max '#' record_name '.' atom :
    {record_field,?anno('$2'),'$1','$3','$5'}.
record_expr -> expr_max '#' record_name record_tuple :
    {record,?anno('$2'),'$1','$3','$4'}.
record_expr -> record_expr '#' record_name '.' atom :
    {record_field,?anno('$2'),'$1','$3','$5'}.
record_expr -> record_expr '#' record_name record_tuple :
    {record,?anno('$2'),'$1','$3','$4'}.

record_tuple -> '{' '}' : [].
record_tuple -> '{' record_fields '}' : '$2'.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> var '=' expr : {record_field,?anno('$1'),'$1','$3'}.
record_field -> atom '=' expr : {record_field,?anno('$1'),'$1','$3'}.

record_name -> atom : '$1'.

%% N.B. This is called from expr.
function_call -> expr_max argument_list :
    {call,?anno('$1'),'$1',element(1, '$2')}.

if_expr -> 'if' if_clauses 'end' : {'if',?anno('$1'),'$2'}.

if_clauses -> if_clause : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

if_clause -> guard clause_body :
        {clause,?anno(hd(hd('$1'))),[],'$1','$2'}.


case_expr -> 'case' expr 'of' cr_clauses 'end' :
        {'case',?anno('$1'),'$2','$4'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

%% FIXME: merl in syntax_tools depends on patterns in a 'case' being
%% full expressions. Therefore, we can't use pat_expr here. There
%% should be a better way.

cr_clause -> expr clause_guard clause_body :
        {clause,?anno('$1'),['$1'],'$2','$3'}.

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
        A = ?anno('$1'),
        {clause,A,[{tuple,A,[{atom,A,throw},'$1',{var,A,'_'}]}],'$2','$3'}.
try_clause -> atom ':' pat_expr try_opt_stacktrace clause_guard clause_body :
        A = ?anno('$1'),
        {clause,A,[{tuple,A,['$1','$3',{var,A,'$4'}]}],'$5','$6'}.
try_clause -> var ':' pat_expr try_opt_stacktrace clause_guard clause_body :
        A = ?anno('$1'),
        {clause,A,[{tuple,A,['$1','$3',{var,A,'$4'}]}],'$5','$6'}.

%% TODO: don't drop annos
try_opt_stacktrace -> ':' var : element(3, '$2').
try_opt_stacktrace -> '$empty' : '_'.

macro_def_expr -> '(' macro_name ',' macro_def_expr_body ')' : {'$2', '$4'}.

macro_def_clause -> '(' macro_name ',' function_clause ')' : {'$2', '$4'}.

macro_def_expr_body -> '$empty' : empty.
macro_def_expr_body -> '#' atom : {record_name, ?anno('$1'), '$2'}.
macro_def_expr_body -> guard : '$1'.

macro_name -> atom_or_var : {'$1', none}.
macro_name -> atom_or_var '(' ')' : {'$1', []}.
macro_name -> atom_or_var '(' vars ')' : {'$1', '$3'}.

macro_call_expr -> macro_string :
    '$1'.
macro_call_expr -> macro_call_none :
    '$1'.
macro_call_expr -> '?' atom_or_var '(' ')' :
    {macro_call, ?anno('$1'), '$2', []}.
macro_call_expr -> '?' atom_or_var '(' macro_exprs ')' :
    {macro_call, ?anno('$1'), '$2', '$4'}.

macro_call_pat -> macro_call_none :
    '$1'.
macro_call_pat -> '?' atom_or_var '(' ')' :
    {macro_call, ?anno('$1'), '$2', []}.
macro_call_pat -> '?' atom_or_var '(' pat_exprs ')' :
    {macro_call, ?anno('$1'), '$2', '$4'}.

macro_call_type -> macro_call_none :
    '$1'.
macro_call_type -> '?' atom_or_var '(' ')' :
    {macro_call, ?anno('$1'), '$2', []}.
macro_call_type -> '?' atom_or_var '(' top_types ')' :
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

exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

pat_exprs -> pat_expr : ['$1'].
pat_exprs -> pat_expr ',' pat_exprs : ['$1' | '$3'].

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
atomic -> concatable concatables : {concat, ?anno('$1'), ['$1' | '$2']}.

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

-export([parse_form/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-export_type([abstract_clause/0, abstract_expr/0, abstract_form/0,
              abstract_type/0, form_info/0, error_info/0]).

%% Start of Abstract Format

-type anno() :: erl_anno:anno().

-type abstract_form() :: af_module()
                       | af_behavior()
                       | af_behaviour()
                       | af_export()
                       | af_import()
                       | af_export_type()
                       | af_compile()
                       | af_file()
                       | af_record_decl()
                       | af_type_decl()
                       | af_function_spec()
                       | af_wild_attribute()
                       | af_function_decl().

-type af_module() :: {'attribute', anno(), 'module', module()}.

-type af_behavior() :: {'attribute', anno(), 'behavior', behaviour()}.

-type af_behaviour() :: {'attribute', anno(), 'behaviour', behaviour()}.

-type behaviour() :: atom().

-type af_export() :: {'attribute', anno(), 'export', af_fa_list()}.

-type af_import() :: {'attribute', anno(), 'import', af_fa_list()}.

-type af_fa_list() :: [{function_name(), arity()}].

-type af_export_type() :: {'attribute', anno(), 'export_type', af_ta_list()}.

-type af_ta_list() :: [{type_name(), arity()}].

-type af_compile() :: {'attribute', anno(), 'compile', any()}.

-type af_file() :: {'attribute', anno(), 'file', {string(), anno()}}.

-type af_record_decl() ::
        {'attribute', anno(), 'record', {af_record_name(), [af_field_decl()]}}.

-type af_field_decl() :: af_typed_field() | af_field().

-type af_typed_field() ::
        {'typed_record_field', af_field(), abstract_type()}.

-type af_field() :: {'record_field', anno(), af_field_name()}
                  | {'record_field', anno(), af_field_name(), abstract_expr()}.

-type af_type_decl() :: {'attribute', anno(), type_attr(),
                         {type_name(), abstract_type(), [af_variable()]}}.

-type type_attr() :: 'opaque' | 'type'.

-type af_function_spec() :: {'attribute', anno(), spec_attr(),
                             {{function_name(), arity()},
                              af_function_type_list()}}
                          | {'attribute', anno(), 'spec',
                             {{module(), function_name(), arity()},
                              af_function_type_list()}}.

-type spec_attr() :: 'callback' | 'spec'.

-type af_wild_attribute() :: {'attribute', anno(), atom(), any()}.

-type af_function_decl() ::
        {'function', anno(), function_name(), arity(), af_clause_seq()}.

-type abstract_expr() :: af_literal()
                       | af_match(abstract_expr())
                       | af_variable()
                       | af_tuple(abstract_expr())
                       | af_nil()
                       | af_cons(abstract_expr())
                       | af_bin(abstract_expr())
                       | af_binary_op(abstract_expr())
                       | af_unary_op(abstract_expr())
                       | af_record_creation(abstract_expr())
                       | af_record_update(abstract_expr())
                       | af_record_index()
                       | af_record_field_access(abstract_expr())
                       | af_map_creation(abstract_expr())
                       | af_map_update(abstract_expr())
                       | af_catch()
                       | af_local_call()
                       | af_remote_call()
                       | af_list_comprehension()
                       | af_binary_comprehension()
                       | af_block()
                       | af_if()
                       | af_case()
                       | af_try()
                       | af_receive()
                       | af_local_fun()
                       | af_remote_fun()
                       | af_fun()
                       | af_named_fun().

-type af_record_update(T) :: {'record',
                              anno(),
                              abstract_expr(),
                              af_record_name(),
                              [af_record_field(T)]}.

-type af_catch() :: {'catch', anno(), abstract_expr()}.

-type af_local_call() :: {'call', anno(), af_local_function(), af_args()}.

-type af_remote_call() :: {'call', anno(), af_remote_function(), af_args()}.

-type af_args() :: [abstract_expr()].

-type af_local_function() :: abstract_expr().

-type af_remote_function() ::
        {'remote', anno(), abstract_expr(), abstract_expr()}.

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

-type af_local_fun() ::
        {'fun', anno(), {'function', function_name(), arity()}}.

-type af_remote_fun() ::
        {'fun', anno(), {'function', module(), function_name(), arity()}}
      | {'fun', anno(), {'function', af_atom(), af_atom(), af_integer()}}.

-type af_fun() :: {'fun', anno(), {'clauses', af_clause_seq()}}.

-type af_named_fun() :: {'named_fun', anno(), fun_name(), af_clause_seq()}.

-type fun_name() :: atom().

-type abstract_clause() :: af_clause().

-type af_clause() ::
        {'clause', anno(), [af_pattern()], af_guard_seq(), af_body()}.

-type af_body() :: [abstract_expr(), ...].

-type af_guard_seq() :: [af_guard()].

-type af_guard() :: [af_guard_test(), ...].

-type af_guard_test() :: af_literal()
                       | af_variable()
                       | af_tuple(af_guard_test())
                       | af_nil()
                       | af_cons(af_guard_test())
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
                    | af_match(af_pattern())
                    | af_variable()
                    | af_tuple(af_pattern())
                    | af_nil()
                    | af_cons(af_pattern())
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

-type af_match(T) :: {'match', anno(), af_pattern(), T}.

-type af_variable() :: {'var', anno(), atom()}.

-type af_tuple(T) :: {'tuple', anno(), [T]}.

-type af_nil() :: {'nil', anno()}.

-type af_cons(T) :: {'cons', anno(), T, T}.

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
                   | '=/='.

-type af_unary_op(T) :: {'op', anno(), unary_op(), T}.

-type unary_op() :: '+' | '-' | 'bnot' | 'not'.

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

build_macro_def(Type, {_,A}, {{Name, Args}, Body}) ->
    {attribute,A,define,{Type,Name,Args,Body}}.

-type attributes() :: 'export' | 'file' | 'import' | 'module'
                    | 'opaque' | 'record' | 'type'.

build_typed_attribute({atom,Aa,record},{typed_record,Name,RecTuple}) ->
    {attribute,Aa,record,{record_name(Name),record_tuple(RecTuple)}};
build_typed_attribute({atom,Aa,Attr},{type_def,{call,_,{atom,_,TypeName},Args}, Type})
  when Attr =:= 'type' ; Attr =:= 'opaque' ->
    lists:foreach(fun({var, A, '_'}) -> ret_err(A, "bad type variable");
                     (_)             -> ok
                  end, Args),
    case lists:all(fun({var, _, _}) -> true;
                      (_)           -> false
                   end, Args) of
        true -> {attribute,Aa,Attr,{TypeName,Type,Args}};
        false -> error_bad_decl(Aa, Attr)
    end;
build_typed_attribute({atom,Aa,Attr},_) ->
    case Attr of
        record -> error_bad_decl(Aa, record);
        type   -> error_bad_decl(Aa, type);
        opaque -> error_bad_decl(Aa, opaque);
        _      -> ret_err(Aa, "bad attribute")
    end.

build_type_spec({Kind,Aa}, {SpecFun, TypeSpecs})
  when Kind =:= spec ; Kind =:= callback ->
    NewSpecFun =
        case SpecFun of
            {atom, _, Fun} ->
                {Fun, find_arity_from_specs(TypeSpecs)};
            {{atom, _, Mod}, {atom, _, Fun}} ->
                {Mod, Fun, find_arity_from_specs(TypeSpecs)}
        end,
    {attribute,Aa,Kind,{NewSpecFun, TypeSpecs}}.

find_arity_from_specs([Spec|_]) ->
    %% Use the first spec to find the arity. If all are not the same,
    %% erl_lint will find this.
    Fun = case Spec of
              {type, _, bounded_fun, [F, _]} -> F;
              {type, _, 'fun', _} = F -> F
          end,
    {type, _, 'fun', [{type, _, product, Args},_]} = Fun,
    length(Args).

%% The 'is_subtype(V, T)' syntax is not supported as of Erlang/OTP
%% 19.0, but is kept for backward compatibility.
build_compat_constraint({atom, _, is_subtype}, [{var, _, _}=LHS, Type]) ->
    build_constraint(LHS, Type);
build_compat_constraint({atom, _, is_subtype}, [LHS, _Type]) ->
    ret_err(?anno(LHS), "bad type variable");
build_compat_constraint({atom, A, Atom}, _Types) ->
    ret_err(A, io_lib:format("unsupported constraint ~tw", [Atom])).

build_constraint({atom, _, is_subtype}, [{var, _, _}=LHS, Type]) ->
    build_constraint(LHS, Type);
build_constraint({atom, A, Atom}, _Foo) ->
    ret_err(A, io_lib:format("unsupported constraint ~tw", [Atom]));
build_constraint({var, A, '_'}, _Types) ->
    ret_err(A, "bad type variable");
build_constraint(LHS, Type) ->
    IsSubType = {atom, ?anno(LHS), is_subtype},
    {type, ?anno(LHS), constraint, [IsSubType, [LHS, Type]]}.

lift_unions(T1, {type, _Aa, union, List}) ->
    {type, ?anno(T1), union, [T1|List]};
lift_unions(T1, T2) ->
    {type, ?anno(T1), union, [T1, T2]}.

build_gen_type({atom, Aa, tuple}) ->
    {type, Aa, tuple, any};
build_gen_type({atom, Aa, map}) ->
    {type, Aa, map, any};
build_gen_type({atom, Aa, Name}) ->
    Tag = type_tag(Name, 0),
    {Tag, Aa, Name, []}.

build_bin_type([{var, _, '_'}|Left], Int) ->
    build_bin_type(Left, Int);
build_bin_type([], Int) ->
    Int;
build_bin_type([{var, Aa, _}|_], _) ->
    ret_err(Aa, "Bad binary type").

build_type({atom, A, Name}, Types) ->
    Tag = type_tag(Name, length(Types)),
    {Tag, A, Name, Types}.

type_tag(TypeName, NumberOfTypeVariables) ->
    case erl_internal:is_type(TypeName, NumberOfTypeVariables) of
        true -> type;
        false -> user_type
    end.

%% build_attribute(AttrName, AttrValue) ->
%%	{attribute,Anno,module,Module}
%%	{attribute,Anno,export,Exports}
%%	{attribute,Anno,import,Imports}
%%	{attribute,Anno,record,{Name,Inits}}
%%	{attribute,Anno,file,{Name,Line}}
%%	{attribute,Anno,Name,Val}

build_attribute({atom,Aa,module}, Val) ->
    case Val of
        [{atom,_Am,Module}] ->
            {attribute,Aa,module,Module};
        [{atom,_Am,Module},ExpList] ->
            {attribute,Aa,module,{Module,var_list(ExpList)}};
        _Other ->
            error_bad_decl(Aa, module)
    end;
build_attribute({atom,Aa,export}, Val) ->
    case Val of
        [ExpList] ->
            {attribute,Aa,export,farity_list(ExpList)};
        _Other -> error_bad_decl(Aa, export)
    end;
build_attribute({atom,Aa,import}, Val) ->
    case Val of
        [{atom,_Am,Mod},ImpList] ->
            {attribute,Aa,import,{Mod,farity_list(ImpList)}};
        _Other -> error_bad_decl(Aa, import)
    end;
build_attribute({atom,Aa,record}, Val) ->
    case Val of
        [Name,RecTuple] ->
            {attribute,Aa,record,{record_name(Name),record_tuple(RecTuple)}};
        _Other -> error_bad_decl(Aa, record)
    end;
build_attribute({atom,Aa,file}, Val) ->
    case Val of
        [{string,_An,Name},{integer,_Al,Line}] ->
            {attribute,Aa,file,{Name,Line}};
        _Other -> error_bad_decl(Aa, file)
    end;
build_attribute({atom,Aa,Epp}, Val) when Epp =:= else; Epp =:= endif ->
    case Val of
        [] -> {attribute,Aa,Epp,undefined};
        [Value] -> {attribute,Aa,Epp,Value};
        _Other -> ret_err(Aa, "bad attribute")
    end;
build_attribute({atom,Aa,Attr}, Val) ->
    case Val of
        [Expr0] ->
            Expr = attribute_farity(Expr0),
            {attribute,Aa,Attr,Expr};
        _Other -> ret_err(Aa, "bad attribute")
    end.

var_list({cons,_Ac,{var,_,V},Tail}) ->
    [V|var_list(Tail)];
var_list({nil,_An}) -> [];
var_list(Other) ->
    ret_err(?anno(Other), "bad variable list").

attribute_farity({cons,A,H,T}) ->
    {cons,A,attribute_farity(H),attribute_farity(T)};
attribute_farity({tuple,A,Args0}) ->
    Args = attribute_farity_list(Args0),
    {tuple,A,Args};
attribute_farity({map,A,Args0}) ->
    Args = attribute_farity_map(Args0),
    {map,A,Args};
attribute_farity({op,A,'/',{atom,_,_}=Name,{integer,_,_}=Arity}) ->
    {tuple,A,[Name,Arity]};
attribute_farity(Other) -> Other.

attribute_farity_list(Args) ->
    [attribute_farity(A) || A <- Args].

%% It is not meaningful to have farity keys.
attribute_farity_map(Args) ->
    [{Op,A,K,attribute_farity(V)} || {Op,A,K,V} <- Args].

-spec error_bad_decl(erl_anno:anno(), attributes()) -> no_return().

error_bad_decl(Anno, S) ->
    ret_err(Anno, io_lib:format("bad ~tw declaration", [S])).

farity_list({cons,_Ac,{op,_Ao,'/',{atom,_Aa,A},{integer,_Ai,I}},Tail}) ->
    [{A,I}|farity_list(Tail)];
farity_list({nil,_An}) -> [];
farity_list(Other) ->
    ret_err(?anno(Other), "bad function arity").

record_tuple({tuple,_At,Fields}) ->
    record_fields(Fields);
record_tuple(Other) ->
    ret_err(?anno(Other), "bad record declaration").

record_name({atom, _, _} = Name) -> Name;
record_name(Other) -> ret_err(?anno(Other), "bad record declaration").

record_fields([{atom,Aa,A}|Fields]) ->
    [{record_field,Aa,{atom,Aa,A}}|record_fields(Fields)];
record_fields([{match,_Am,{atom,Aa,A},Expr}|Fields]) ->
    [{record_field,Aa,{atom,Aa,A},Expr}|record_fields(Fields)];
record_fields([{typed,Expr,TypeInfo}|Fields]) ->
    [Field] = record_fields([Expr]),
    [{typed_record_field,Field,TypeInfo}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    ret_err(?anno(Other), "bad record field");
record_fields([]) -> [].

build_try(A,Es,Scs,{Ccs,As}) ->
    {'try',A,Es,Scs,Ccs,As}.

-spec ret_err(_, _) -> no_return().
ret_err(Anno, S) ->
    return_error(location(Anno), S).

location(Anno) ->
    erl_anno:location(Anno).
