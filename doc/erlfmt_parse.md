# `erlfmt_parse`

## Differences to `erl_parse`

In `erlfmt_parse` the following AST nodes have different definitions:

* The record name is always represented as either a full `atom` or `macro_call`
  node instead of a raw atom. This affects the following AST nodes:
  * `{record, Anno, Name, Fields}`
  * `{record, Anno, Expr, Name, Updates}`
  * `{record_index, Anno, Name, Field}`
  * `{record_field, Anno, Expr, Name, Field}`
  * `{type, Anno, record, [Name | Fields]}`
  * `{attribute, Anno, record, {Name, Fields}}`

* The attribute values are not "normalized" - they are represented in the
  abstract term format instead of as concrete terms. An additional value of
  `undefined` is allowed as a value of an attribute, indicating the attribute
  was defined without arguments, for example `-else.`.

* A new attribute with a special value is recognised:
  `{attribute, Anno, define, {Type, Name, Args, Body}}`, where
  * `Type` is `expr` or `clause`,
  * `Name` is either an `atom` node or a `var` node,
  * `Args` is either a list of vars or an atom `none`,
  * `Body` is:
    * for `expr`, it is a list of lists of expressions (equivalent to guards in clauses),
      an atom `empty` or a `{record_name, Anno, Name}` node,
    * for `clause`, it is a `clause` node as used in functions.

* The `clause` node as used in functions or funs has a different AST representation:
  `{clause, Anno, Name, Args, Guards, Body}`, where the newly added `Name` field
  is an `atom`, `var`, or `macro_call` node or an atom `'fun'` for anonymous funs.

* The `function` node has a different AST representation:
  `{function, Anno, Clauses}`, where `Clauses` is a list of `clause` nodes
  or `macro_call` nodes. Additionally it is less strict - it does not enforce
  all clauses have the same name and arity.

* The `fun` node has a different AST representation:
  `{'fun', Anno, Value}`, where `Value` is one of:
  * `{function, Name, Arity}`, where `Name` and `Arity` are an `atom` and
    `integer` node respectively or `var` or `macro_call` nodes.
  * `{function, Module, Name, Arity}`, where `Module`, `Name`, and `Arity`
    are `atom`, `atom`, and `integer` nodes respectively or a `var` or `macro_call` node.
  * `{clauses, Clauses}`, where `Clauses` is a list of `clause` nodes.
    Additionally it is less strict - the clauses aren't checked for the same
    name or arity.

* The `named_fun` node is not used.

* A new node `{macro_call, Anno, Name, Args}` is introduced, where `Name` is
  either an `atom` or a `var` node and `Args` is a list of expressions, types,
  or special `guard` nodes.

* A new node `{guard, Anno, Expr, Guard}` is introduced, used only as argument
  for a macro. It represents "free-standing" `Expr when Guard` expressions as used,
  for example, in the `assertMatch` macro.

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
  of raw atoms and integers.

* The special `match` node is encoded as regular binary operator node.

* The special `catch` node is encoded as regular unary operator node.

* Lists are represented as a `list` node instead of a chain of `cons` and `nil` nodes,
  similar to the `tuple` node. The last elemenent of the list can be a `cons` node
  representing explicit consing syntax.
