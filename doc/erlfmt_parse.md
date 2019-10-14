# `erlfmt_parse`

## Differences to `erl_parse`

In `erlfmt_parse` the following AST nodes have different definitions:

* The record name is always represented as a full `{atom, Anno, Name}` node,
  instead of the raw atom `Name`. This affects the following AST nodes:
  * `{record, Anno, Name, Fields}`
  * `{record, Anno, Expr, Name, Updates}`
  * `{record_index, Anno, Name, Field}`
  * `{record_field, Anno, Expr, Name, Field}`
  * `{type, Anno, record, [Name | Fields]}`
  * `{attribute, Anno, record, {Name, Fields}}`

* The attribute values are not "normalized" - they are represented in the
  abstract term format instead of as concrete terms.
