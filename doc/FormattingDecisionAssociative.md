# Formatting Decision: Left Associative Equal and Dolon Operators

We have decided to keep `=` and `::` operators left associative even while other binary operators are rewritten to be right associative.
We realize this solution is not perfect and in this document we hope to show why this problem is too hard and we had to compromise.

## Explanation

Binary operators which are parsed as right [associative](https://erlang.org/doc/reference_manual/expressions.html#operator-precedence)
for example: `A ++ B ++ C` is parsed as `A ++ (B ++ C)`
The layout algorithm used for `erlfmt` is greedy, which means that when this expressions needs to break it will break as:

```erlang
A ++
    B ++ C
```

We avoid this by rewriting the parsed AST as left associative `(A ++ B) ++ C` which results in the preferred format:

```erlang formatted plusplus
A ++ B ++
    C
```

This rewriting is only internal to `erlfmt` and does not affect the semantics of the code.
`erlfmt` does not place any new parenthesis and the formatted code will be parsed as right associative again.

This works for all binary operators, except `=` and `::` which are the only two operators than can be chained and also have next break fits behaviour if the container on the right is also next break fits, for example a list. Here is an example of how equal is formatted with next break fits with a list of the right side, given we had to break the line:

```erlang formatted nextbreakfits
A = [
    1,
    2
]
```

If this was not formatted with next break fits, we would have to break the line earlier, resulting in an extra line and an extra indentation:

```erlang formatted notnextbreakfits
A =
    [
        1,
        2
    ]
```

The problem with rewriting equal to left associativity is that when we need to break it could result in the following formatting, where the context of the indentation is totally lost and we dedent too much:

```erlang
A =
    B = c(
    D,
    E
).
```

We have also tried adding extra detection for binary operator chains, implementation details in the appendix.  This is more complicated and does not offer a clearly better formatting than the current state, so we opted for simplicity.  See below several examples that we explored, where `assoc` is rewriting to left associativity and `opchain` is our operator chain detection algorithm.

### ends with case (not next break fits)

✅ want

```erlang
A = {B, C} =
    case X of
        true -> f();
        _ -> g()
    end.
```

⚠️ current

```erlang formatted case
A =
    {B, C} =
    case X of
        true -> f();
        _ -> g()
    end.
```

✅ assoc

```erlang
A = {B, C} =
    case X of
        true -> f();
        _ -> g()
    end.
```

✅ opchain

```erlang
A = {B, C} =
    case X of
        true -> f();
        _ -> g()
    end.
```

### ends with case with break after first `=`

⚠️ want

```erlang
A =
    {B, C} =
    case X of
        true -> f();
        _ -> g()
    end.
```

⚠️ current

```erlang formatted case_with_new_line
A =
    {B, C} =
    case X of
        true -> f();
        _ -> g()
    end.
```

⚠️ assoc

```erlang
A =
    {B, C} =
    case X of
        true -> f();
        _ -> g()
    end.
```

⚠️ opchain

```erlang
A =
    {B, C} =
    case X of
        true -> f();
        _ -> g()
    end.
```

### ends with call (next break fits)

✅ want

```erlang
A = B = c(
    D,
    E
).
```

⚠️ current

```erlang formatted call
A =
    B = c(
        D,
        E
    ).
```

✅ assoc

```erlang
A = B = c(
    D,
    E
).
```

⚠️ opchain

```erlang
A =
    B =
    c(
        D,
        E
    ).
```

### ends with call (next break fits) with break after first `=`

✅ want

```erlang
A =
    B = c(
        D,
        E
    ).
```

✅ current

```erlang formatted call_with_new_line
A =
    B = c(
        D,
        E
    ).
```

❌ assoc

```erlang
A =
    B = c(
    D,
    E
).
```

⚠️ opchain

```erlang
A =
    B =
    c(
        D,
        E
    ).
```

### ends with list (next break fits)

✅ want

```erlang
A = B = [
    a,
    b
].
```

⚠️ current

```erlang formatted list
A =
    B = [
        a,
        b
    ].
```

✅  assoc

```erlang
A = B = [
    a,
    b
].
```

⚠️ opchain

```erlang
A = B =
    [
        a,
        b
    ].
```

### ends with list (next break fits) with break after first `=`

✅ want

```erlang
A =
    B = [
        a,
        b
    ].
```

✅ current

```erlang formatted list_with_new_line
A =
    B = [
        a,
        b
    ].
```

❌ assoc

```erlang
A =
    B = [
    a,
    b
].
```

⚠️ opchain

```erlang
A =
    B =
    [
        a,
        b
    ].
```

## Appendix

### Next break fits vs not next break fits for equal in general

```erlang
B = c(
    D,
    E
).

B =
    c(
        D,
        E
    ).

ThisVar = my_function_call(
    Param1,
    param2
).

ThisVar =
    my_function_call(
        Param1,
        param2
    ).

ThisVar = [
    Param1,
    param2
].

ThisVar =
    [
        Param1,
        param2
    ].
```

### Diff for assoc

Diff for enabling rewriting right to left associativity for `=` and `::` too.

```
diff --git a/src/erlfmt_format.erl b/src/erlfmt_format.erl
index 66e32b4..698189e 100644
--- a/src/erlfmt_format.erl
+++ b/src/erlfmt_format.erl
@@ -312,10 +312,10 @@ binary_op_to_algebra(Op, Meta0, Left0, Right0) ->
     {op, _Meta, Op, Left, Right} = rewrite_assoc(Op, Meta, Left0, Right0),
     binary_op_to_algebra(Op, Meta, Left, Right, ?INDENT).

-rewrite_assoc('=' = Op, Meta, Left, Right) ->
-    {op, Meta, Op, Left, Right};
-rewrite_assoc('::' = Op, Meta, Left, Right) ->
-    {op, Meta, Op, Left, Right};
+% rewrite_assoc('=' = Op, Meta, Left, Right) ->
+%     {op, Meta, Op, Left, Right};
+% rewrite_assoc('::' = Op, Meta, Left, Right) ->
+%     {op, Meta, Op, Left, Right};
 rewrite_assoc(Op, MetaABC0, A, {op, MetaBC0, Op, B0, C} = BC) ->
     case erlfmt_scan:get_anno(parens, MetaBC0, false) of
         true ->
```

### Diff for opchain

The diff for binary operator detection requires the diff of assoc to also be applied.

```
diff --git a/src/erlfmt_format.erl b/src/erlfmt_format.erl
index 66e32b4..bc95541 100644
--- a/src/erlfmt_format.erl
+++ b/src/erlfmt_format.erl
@@ -349,11 +349,15 @@ binary_operand_to_algebra(_ParentOp, Expr, _Indent) ->
 binary_op_to_algebra(Op, Meta, Left, Right, Indent) ->
     LeftD = binary_operand_to_algebra(Op, Left, Indent),
     RightD = binary_operand_to_algebra(Op, Right, 0),
+    OpChain = case Left of
+        {op, _, _, _, _} -> true;
+        _ -> false
+    end,
     Doc =
         case Op of
-            '::' ->
+            '::' when OpChain =:= false ->
                 field_to_algebra(<<"::">>, Left, Right, LeftD, RightD, Indent);
-            '=' ->
+            '=' when OpChain =:= false ->
                 field_to_algebra(<<"=">>, Left, Right, LeftD, RightD, Indent);
             %% when a pattern is in a clause and it breaks we want to prevent issue #211
             {clause_op, '='} ->
```
