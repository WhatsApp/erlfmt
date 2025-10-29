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
- Consistent indenting by 4, to aid writability

Here you can see all the other candidates we evaluated against our goals.

## Expanded

- ✅ Differentiating
- ✅ Consistent Lists
- ✅ Multiline expressions
- ❌ Indenting by 4

We considered indenting the filter expressions by 3.

```erlang
[
    function_with_long_name(A)
    || {A, B} <- Cs,
       filter(B)
]
```

This does allow you to move some parts onto the same line, if they fit they will be kept there:

```erlang
[
    function_with_long_name(A)
    || {A, B} <- Cs, filter(B)
]
```

Here is an example with a multiline expression, that shows what a long argument in the function would look like:

```erlang
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
- ❌ Indenting by 4

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
- ✅ Indenting by 4

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

## Dedent double pipes, just a little

- ✅ Differentiating
- ✅ Consistent with lists
- ✅ Handles multiline expression consistently
- ✅ Indenting by 4

This is the formatting `erlfmt` chose.
Another alternative is dedenting the double pipes to the left, but just a little:

```erlang formatted list_comp_1
[
    function_with_long_name(A)
 || {A, B} <- Cs, filter(B)
]
```

All parts of the list comprehension stay aligned if they are broken apart at a consistent 4 spaces:

```erlang formatted list_comp
[
    function_with_long_name(A)
 || {A, B} <- Cs,
    filter(B)
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

## Two spaces after double pipes

- ✅ Differentiating
- ✅ Consistent with lists
- ✅ Handles multiline expression consistently
- ✅ Indenting by 4

`erlfmt` chose the above format over this one, purely because this alternative had much larger complexity and seems to require inventing a new operator in the algebra.
We can also considered dedenting the pipes, with two spaces after, to keep expressions aligned:

```erlang
[
    function_with_long_name(A)
||  {A, B} <- Cs, filter(B)
]
```

All parts of the list comprehension stay aligned if they are broken apart at a consistent 4 spaces:

```erlang
[
    function_with_long_name(A)
||  {A, B} <- Cs,
    filter(B)
]
```

Here is an example with a multiline expression, that shows what a long argument in the function would look like:

```erlang
[
    function_with_long_name(
        A,
        ALongArgument
    )
||  {A, B} <- Cs,
    filter(B)
]
```

## Double pipes on their own line

- ✅ Differentiating
- ❌ Consistent with lists
- ✅ Handles multiline expression consistently
- ✅ Indenting by 4

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
- ✅ Indenting by 4

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
- ✅ Indenting by 4

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
