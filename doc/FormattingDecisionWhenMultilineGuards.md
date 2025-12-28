# Formatting Decision: When with Multiline Guards

This is a document explaining our reasoning behind the formatting decision for `when` with `multiline guards`.

We chose to format `when` with `multiline guards` by
using the `when` and `->` as parenthesis and placing the [arrow on a newline](#arrow-on-newline)
as this most closely matched our goals.

Goals:

- Create/keep differentiation between the guards and the body of the function.
- Minimize diff when changing a single line, impacts surrounding context.
- Minimize indentation, as this causes more lines not to fit and more line breaks.
- Consistent indenting by 4, to aid writability

Here you can see all the other candidates we evaluated against our goals.

## Newline and double indentation

The first idea was for `when` to be placed on a new line and doubly-indented.
This worked well, except for the case of extremely small functions and multiline guards (for example, because of a comment):

- ✅ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ❌ Minimize indentation
- ❌ Indenting by 4

```erlang
bar(X)
        when is_tuple(X);
             % some comment
             is_list(X) ->
    body.
```

```erlang
insert_nested({Field, Meta, Key0, Value0}, Comments0)
        when Field =:= map_field_assoc;
             Field =:= map_field_exact;
             Field =:= record_field;
             Field =:= generate;
             Field =:= b_generate ->
    body.
```

## Newline and single indentation

We tried to indent `when` with one full indentation.
This is good in for multiline guards, but fails the guard-body distinction check when guard is on a separate line, but single-line

- ❌ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ✅ Minimize indentation
- ✅ Indenting by 4

```erlang
bar(Some, Very, Long, Arguments)
    when is_tuple(X); is_list(X) ->
    body.
```

## Newline and half indentation

Another idea was to indent `when` by half the indentation.
This is preserves the differentiation between guards and body.
We find this rather awkward as we don't use half-indentation anywhere else in the formatter.

- ✅ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ✅ Minimize indentation
- ❌ Indenting by 4

```erlang
bar(X)
  when is_tuple(X);
       % some comment
       is_list(X) ->
    body.
```

```erlang
insert_nested({Field, Meta, Key0, Value0}, Comments0)
  when Field =:= map_field_assoc;
       Field =:= map_field_exact;
       Field =:= record_field;
       Field =:= generate;
       Field =:= b_generate ->
    body.
```

## Newline and no indentation

We tried not to indent `when`.
This creates relatively little differentiation between guards and body of the function.

- ❌ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ✅ Minimize indentation
- ❌ Indenting by 4

```erlang
bar(X)
when is_tuple(X);
     % some comment
     is_list(X) ->
    body.
```

## Floating guards

Small functions and multiline guards look great with "floating" guards.

- ✅ Differentiation between guards and body
- ❌ Minimize diff when changing a single line
- ❌ Minimize indentation
- ❌ Indenting by 4

```erlang
bar(X) when is_tuple(X);
            % some comment
            is_list(X) ->
    body.
```

But larger functions with floating guards are debatable.

```erlang unformatted insert_nested
insert_nested({Field, Meta, Key0, Value0}, Comments0) when Field =:= map_field_assoc;
                                                           Field =:= map_field_exact;
                                                           Field =:= record_field;
                                                           Field =:= generate;
                                                           Field =:= b_generate ->
    body.
```

Changing the way we pattern match and argument names is not too uncommon and will then result in a diff of several lines.
For example a 5 line diff, instead of a 4 line diff in this case:

```erlang
insert_nested(KeyValue, Comments0) when Field =:= map_field_assoc;
                                        Field =:= map_field_exact;
                                        Field =:= record_field;
                                        Field =:= generate;
                                        Field =:= b_generate ->
    body.
```

## Arrow on newline

We tried to treat `when` and `->` as a sort-of parenthesis.
This preserves clarity, by creating differentiation between guards and body of the function, but has relatively low current usage.

- ✅ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ✅ Minimize indentation
- ✅ Indenting by 4

```erlang formatted newlinewhen
bar(X) when
    is_tuple(X);
    % some comment
    is_list(X)
->
    body.
```

It also works great for longer function declarations.

```erlang formatted insert_nested
insert_nested({Field, Meta, Key0, Value0}, Comments0) when
    Field =:= map_field_assoc;
    Field =:= map_field_exact;
    Field =:= record_field;
    Field =:= generate;
    Field =:= b_generate
->
    body.
```

## Newline after when

We tried to bring back the `->` onto the same line for a more compact representation, compared to `Arrow on newline`,
but this loses the differentiation between guards and body.

- ❌ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ✅ Minimize indentation
- ✅ Indenting by 4

```erlang unformatted newlinewhen
bar(X) when
    is_tuple(X);
    % some comment
    is_list(X) ->
    body.
```

## Body next to arrow

We tried to bring back the body onto the same line for a more compact representation, compared to `Arrow on newline`,
but this also loses the differentiation between guards and body.

- ❌ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ✅ Minimize indentation
- ✅ Indenting by 4

```erlang
bar(X) when
    is_tuple(X);
    % some comment
    is_list(X)
-> body.
```

## When on same line as guards

We can move `when` to the same line as guards and still keep the `->` on a separate line.
This creates an even greater differentiation between guards and body, than when only the `->` is on a separate line.
We find this rather awkward to type when we have multiline guards:

- ✅ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ❌ Minimize indentation
- ❌ Indenting by 4

```erlang
post({Generate, _L, _Pattern, _Expr}, St, expr)
    when Generate =:= generate; Generate =:= b_generate
->
    body.
```

```erlang
bar(X)
    when is_tuple(X);
         % some comment
         is_list(X)
->
    body.
```

```erlang
insert_nested({Field, Meta, Key0, Value0}, Comments0)
    when Field =:= map_field_assoc;
         Field =:= map_field_exact;
         Field =:= record_field;
         Field =:= generate;
         Field =:= b_generate
->
    body.
```

## When indented by 3

We can indent `when` by 3 to cause constant indentation by 4 for the other multiline guards.
This creates an even greater differentiation between guards and body, than when only the `->` is on a separate line.

- ✅ Differentiation between guards and body
- ✅ Minimize diff when changing a single line
- ✅ Minimize indentation
- ✅ Indenting by 4

```erlang
insert_nested({Field, Meta, Key0, Value0}, Comments0)
   when Field =:= map_field_assoc;
        Field =:= map_field_exact;
        Field =:= record_field;
        Field =:= generate;
        Field =:= b_generate
->
    body.
```

❌ We don't have a measurable goal that disqualifies this option, but it tends to look rather awkward, in the case for single line guards.

```erlang
post({Generate, _L, _Pattern, _Expr}, St, expr)
   when Generate =:= generate; Generate =:= b_generate
->
    body.
```

```erlang
bar(X)
   when is_tuple(X);
        % some comment
        is_list(X)
->
    body.
```

## References

- Some of the discussion started on [github issue 20](https://github.com/WhatsApp/erlfmt/issues/20).
- This [insert_nested](https://github.com/WhatsApp/erlfmt/blob/487d2ab216ddbadd5b42c8d9eb7ad7a8cfe0e504/src/erlfmt_recomment.erl#L142) function was found in erlfmt.
