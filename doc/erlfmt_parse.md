# `erlfmt_parse`

## Differences to `erl_parse`

In `erlfmt_parse` the following AST nodes have different definitions:

* The record name is always represented as either a full `atom` or `macro_call`
  node instead of a raw atom. This affects the following AST nodes:
  * `{record, Anno, Name, Fields}`
  * `{record, Anno, Expr, Name, Updates}`
  * `{record_index, Anno, Name, Field}`
  * `{record_field, Anno, Expr, Name, Field}`
  * `{attribute, Anno, record, [Name, FieldsTuple]}`

* The value of an attribute node is always a list of abstract term formats
  instead of concrete terms. The name is always represented as a full `atom` node.

* The `clause` node has a different AST representation:
  `{clause, Anno, Head, Guards, Body}`, where the `Guards` element is either
  an atom `empty` or a `guard_or` node, and `Head` element is one of:
  * regular `call` node for functions and named funs;
  * atom `empty` for `if` expressions;
  * `{args, Anno, Args}` node for bare argument clauses inside anonymous funs;
  * `{catch, Anno, Args}` node for clauses in `catch` clauses, where
    2 to 3 arguments represent the various `:` separated syntaxes;
  * other expression for `case`, `receive`, "of" part of `try` expression
    and simple `catch` clauses without `:`.

* New `{spec_clause, Anno, Head, Body, Guards}` node for clauses inside
  `spec` and `callback` attributes, similar to the `clause` node above.
  It reflects the fact that in specs guards come after body. The `Head`
  element is always an `args` node.

* New `{guard_or, Anno, GuardAndList}` and `{guard_and, Anno, Exprs}` nodes
  are introduced to support annotating guard sequences, insted of a plain
  nested list of lists structure.

* The `function` node has a different AST representation:
  `{function, Anno, Clauses}`, where `Clauses` is a list of `clause` nodes
  or `macro_call` nodes. Additionally it is less strict - it does not enforce
  all clauses have the same name and arity.

* The `fun` node has a different AST representation:
  `{'fun', Anno, Value}`, where `Value` is one of:
  * `{function, Anno, Name, Arity}`, where `Name` and `Arity` are an `atom` and
    `integer` node respectively or `var` or `macro_call` nodes.
  * `{function, Anno, Module, Name, Arity}`, where `Module`, `Name`, and `Arity`
    are `atom`, `atom`, and `integer` nodes respectively or a `var` or `macro_call` node.
  * `{clauses, Anno, Clauses}`, where `Clauses` is a list of `clause` nodes.
    Additionally it is less strict - the clauses aren't checked for the same
    name or arity.
  * `type` for the anonymous function type `fun()`.
  * `{type, Anno, Args, Res}` for the anonymous function type `fun((...Args) -> Res)`.

* The `named_fun` node is not used.

* A new node `{macro_call, Anno, Name, Args}` is introduced, where `Name` is
  either an `atom` or a `var` node and `Args` is a list of expressions, types,
  or special `guard` nodes.

* A new operator node `{op, Anno, 'when', Expr, Guard}` is introduced, used only as
  argument for a macro. It represents "free-standing" `Expr when Guard` expressions
  as used, for example, in the `assertMatch` macro.

* A new node `{macro_string, Anno, Name}` is introduced, where `Name` is either
  an `atom` or a `var` node. It represents `??Name` macro syntax.

* A new node `{concat, Anno, Concatables}`, where `Concatables` is a list of
  `atom`, `var`, and `macro_call` nodes. This is used to represent implicit
  string concatenation, for example `"foo" "bar"`.

* Attributes are not processed to convert the `fun/arity` syntax into tuples,
  they are left as the `op` nodes with the `/` operator. Additionally, the
  `import` and `export` attributes are not processed to convert the `cons` node
  chains into lists.

* Bit type definitions inside binaries are represented as full nodes instead
  of raw atoms and integers. The unit notation `unit:Int` is represented with
  a `{remote, Anno, {atom, Anno, unit}, Int}` node.

* The special `match` node is encoded as regular binary operator node.

* The special `catch` node is encoded as regular unary operator node.

* Lists are represented as a `list` node instead of a chain of `cons` and `nil` nodes,
  similar to the `tuple` node. The last elemenent of the list can be a `cons` node
  representing explicit consing syntax.

* Representation for types is in general the same as for corresponding values.
  The `type` node is not used at all. This means new binary operators inside types
  are defined: `|`, `::`, and `..`.
