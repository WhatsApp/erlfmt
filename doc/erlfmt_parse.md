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
  abstract term format instead of as concrete terms. Additionally, the value of
  attributes is always a list of expressions, except for couple "binary" attributes,
  where the value is a two-tuple and which usually have some special syntax.
  Those include `type`, `opaque`, `spec`, `callback`, `record`, and `define`.

  * The value of the `type` and `opaque` attributes is a `call` node, and a `type` node.
  * The value of the `spec` and `callback` attributes is an `atom` or `remote` node,
    and a list of `type` nodes.
  * The value of the `record` attribute is a name as described above and a list of
    `typed_record_field` or `record_field` nodes.
  * The value of the the `define` attribute is a `macro_call` node, and either a
    `clause` node or a list of lists of expressions (similar to guards in clauses).

* The `clause` node has a different AST representation:
  `{clause, Anno, Name, Args, Guards, Body}`, where the newly added `Name` field
  is an `atom`, `var`, or `macro_call` node or an atom `'fun'` for anonymous funs,
  `'case'` for case, receive and "of" part of try expressions,
  `'if'` for if expressions and `catch` for "catch" part of try expressions.

* The `clause` nodes tagged with `catch` have 1 to 3 arguments representing
  the various syntaxes of catch clauses. This replaces a fixed 3-tuple as a single
  argument of the clause.

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