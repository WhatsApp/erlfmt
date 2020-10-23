# Formatting Decision: List Comprehensions

This is a document explaining our reasoning behind the formatting decision for list comprehensions.

This document conforms to the decisions already made for [lists](./FormattingDecisionLists.md)
and does not rehash decisions already taken in that document.

This document focuses on list comprehensions that are broken up over multiple lines, because of:
  - a long function name,
  - long parameters,
  - long generators
  - long filters
  - etc.

We will choose a format that best conforms to our goals:

  - Create/keep differentiation between the generator, body and filters.
  - Consistent with formatting of [Lists](./FormattingDecisionLists.md)
  - Handles multiline expressions consistently.

Here you can see all the other candidates we evaluated against our goals.

## Expanded

  - ✅ Differentiating
  - ✅ Consistent Lists
  - ✅ Multiline expressions

This is how `erlfmt` formats a multiline list comprehension today:

```erlang formatted list_comp
[
    function_with_long_name(A)
    || {A, B} <- Cs,
       filter(B)
]
```

`erlfmt` does allow you to move some parts onto the same line, if they fit `erlfmt` will keep them there:

```erlang formatted list_comp_2
[
    function_with_long_name(A)
    || {A, B} <- Cs, filter(B)
]
```

Here is an example with a multiline expression, that shows what a long argument in the function would look like:

```erlang formatted list_comp_3
[
    function_with_long_name(
        A,
        ALongArgument
    )
    || {A, B} <- Cs,
       filter(B)
]
```

## Expanded with consistent 4 spacing

  - ✅ Differentiating
  - ✅ Consistent with lists
  - ✅ Handles multiline expression consistently

The one negative in the previous example is inconsistent indentation, where `filter(B)` was indented by 3 spaces.
This alternative tries to correct that, but now we have a misalignment between `{A, B}` and `filter(B)` by a single space.

```erlang
[
    function_with_long_name(A)
    || {A, B} <- Cs,
        filter(B)
]
```

## Dedent double pipes

  - ✅ Differentiating
  - ❌ Consistent with lists
  - ✅ Handles multiline expression consistently

We also considered dedenting the double pipes to the left.

```erlang
[
    function_with_long_name(A)
|| {A, B} <- Cs,
    filter(B)
]
```

This doesn't seem very consistent with how lists are formatted.

```erlang
[
    function_with_long_name(A)
|| {A, B} <- Cs, filter(B)
]
```

## Double pipes on their own line

  - ✅ Differentiating
  - ❌ Consistent with lists
  - ✅ Handles multiline expression consistently

We also considered giving the double pipes their own line.

```erlang
[
    function_with_long_name(A)
||
    {A, B} <- Cs,
    filter(B)
]
```

```erlang
[
    function_with_long_name(A)
||
    {A, B} <- Cs, filter(B)
]
```

## Compressed

  - ✅ Differentiating
  - ❌ Consistent with lists
  - ❌ Handles multiline expression consistently

The compressed option is inconsistent with how `erlfmt` formats lists,
but does make differentiation clear.

```erlang
[function_with_long_name(A)
 || {A, B} <- Cs,
    filter(B)]
```

There is also a problem with how to indent multiline expressions consistently:

```erlang
[function_with_long_name(
    A,
    AlongArgument
 )
 || {A, B} <- Cs,
    filter(B)]
```

## Compressed with 4 spaces

  - ✅ Differentiating
  - ❌ Consistent with lists
  - ❌ Handles multiline expression consistently

We could also consider an option where indentations are made with four spaces.

```erlang
[function_with_long_name(A)
    || A <- a_generator(),
        some_check(A)]
```

Unfortunately since the first function is technically already indented by one space,
this still results in inconsistent handling of multiline expressions:

```erlang
[function_with_long_name(
    A,
    AlongArgument
 )
    || A <- a_generator(),
        some_check(A)]
```
