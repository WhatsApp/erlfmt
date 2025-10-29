# Formatting Decision: Ignore

This is a document explaining our reasoning behind the formatting decision to take direction from an `erlfmt-ignore` comment to ignore formatting the next top-level expression.

One goal of `erlfmt` is to keep the number of options to a minimum and standardize a way to format erlang code.
We found that mostly it is possible to format erlang code in an at least somewhat acceptable way, but exceptions do occur.
We have introduced the `erlfmt-ignore` comment, which when placed before a top-level expression, will indicate to `erlfmt` to skip over that expression, leave it as is and move on to the next expression. Adding these comments is discouraged, but we understand that there are exceptions, here are some we have found.

## Example 1: Aligned Matrix

Here is a matrix that is defined and well aligned, so that is easy to see the element at index `3,3` is `-16`.

```erlang unformatted matrix
-define(DELTA_MATRIX, [
    [0,   0,   0,   0,   0,   0,   0,   0,   0, -17,   0,   0,   0,   0,   0,   0, -16,   0,   0,   0],
    [0,   0,   0, -15,   0,   0, -17,   0,   0,   0,   0,   0, -16,   0,   0,   0,   0,   0, -15,   0],
    [0,   0,   0, -17,   0,   0,   0,   0, -16,   0,   0,   0,   0, -15,   0,   0,   0,   0,   0,   0],
    [-17, 0,   0, -16, -16,   0,   0,   0, -15,   0,   0,   0,   0,   0,   0,   0,   0, -17,   0,   0],
    [-16, 0,   0, -15,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -17, -33, -16, -31, -15,   0],
    [0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -18, -17, -16, -15, -14,   0,   0,   0,   0,   0],
    [0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0],
    [0,   0,  14,  15,  16,  17,  18,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  15,  31],
    [16,  33, 17,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  15,   0,   0,  16,   0,   0,  17],
    [0,   0,   0,   0,   0,   0,   0,   0,  15,   0,   0,   0,  16,   0,   0,   0,  17,   0,   0,   0],
    [0,   0,   0,  15,   0,   0,   0,   0,  16,   0,   0,   0,   0,  17,   0,   0,   0,   0,  15,   0],
    [0,   0,   0,   0,  16,   0,   0,   0,   0,   0,  17,   0,   0,  15,   0,   0,   0,   0,   0,   0],
    [16,  0,   0,   0,   0,   0,   0,  17,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0]
]).
```

If we format it, we loose all of the alignment.
Now `-16` from index `3,3` seems to be aligned with column `5` in the top row.
TODO: Don't ignore line length limit.

```erlang formatted matrix
-define(DELTA_MATRIX, [
    [0, 0, 0, 0, 0, 0, 0, 0, 0, -17, 0, 0, 0, 0, 0, 0, -16, 0, 0, 0],
    [0, 0, 0, -15, 0, 0, -17, 0, 0, 0, 0, 0, -16, 0, 0, 0, 0, 0, -15, 0],
    [0, 0, 0, -17, 0, 0, 0, 0, -16, 0, 0, 0, 0, -15, 0, 0, 0, 0, 0, 0],
    [-17, 0, 0, -16, -16, 0, 0, 0, -15, 0, 0, 0, 0, 0, 0, 0, 0, -17, 0, 0],
    [-16, 0, 0, -15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -17, -33, -16, -31, -15, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -18, -17, -16, -15, -14, 0, 0, 0, 0, 0],
    [0, -1, -1, -1, -1, -1, -1, -1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
    [0, 0, 14, 15, 16, 17, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 31],
    [16, 33, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 16, 0, 0, 17],
    [0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 16, 0, 0, 0, 17, 0, 0, 0],
    [0, 0, 0, 15, 0, 0, 0, 0, 16, 0, 0, 0, 0, 17, 0, 0, 0, 0, 15, 0],
    [0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 17, 0, 0, 15, 0, 0, 0, 0, 0, 0],
    [16, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
]).
```

We can't think of a work around for this case.

## Example 2: DSLish Lists

`erlfmt` formats lists that don’t fit on a single line, by giving each element its own line:

```erlang formatted list
[
    a_long_expression,
    b_long_expression,
    c_long_expression
]
```

Here is an example of an unformatted DSLish list:

```erlang unformatted gen_part
gen_part({constructed,bif},TypeName,{_Name,parts,Tag,_Type}) ->
    emit(["  case Data of",nl,
          "    L when is_list(L) ->",nl,
          "      'dec_",TypeName,"'(lists:map(fun(X) -> element(1, ",
          {call,ber,ber_decode_erlang,["X"]},") end, L),",{asis,Tag},");",nl,
          "    _ ->",nl,
          "      [Res] = 'dec_",TypeName,"'([Data],",{asis,Tag},"),",nl,
          "      Res",nl,
          "  end"]).
```

This is what the DSLish list looks like if formatted by `erlfmt`:

```erlang formatted gen_part
gen_part({constructed, bif}, TypeName, {_Name, parts, Tag, _Type}) ->
    emit([
        "  case Data of",
        nl,
        "    L when is_list(L) ->",
        nl,
        "      'dec_",
        TypeName,
        "'(lists:map(fun(X) -> element(1, ",
        {call, ber, ber_decode_erlang, ["X"]},
        ") end, L),",
        {asis, Tag},
        ");",
        nl,
        "    _ ->",
        nl,
        "      [Res] = 'dec_",
        TypeName,
        "'([Data],",
        {asis, Tag},
        "),",
        nl,
        "      Res",
        nl,
        "  end"
    ]).
```

The problem is that this DSLish list, is now less readable than it was before.

Technically there is a work around, so that you don't need the `erlfmt-ignore` comment.
You can place each line, in its own list, so that each line is seen as a single element of the bigger list:

```erlang formatted gen_part2
gen_part({constructed, bif}, TypeName, {_Name, parts, Tag, _Type}) ->
    emit([
        ["  case Data of", nl],
        ["    L when is_list(L) ->", nl],
        ["      'dec_", TypeName, "'(lists:map(fun(X) -> element(1, "],
        [{call, ber, ber_decode_erlang, ["X"]}, ") end, L),", {asis, Tag}, ");", nl],
        ["    _ ->", nl],
        ["      [Res] = 'dec_", TypeName, "'([Data],", {asis, Tag}, "),", nl],
        ["      Res", nl],
        ["  end"]
    ]).
```

## Example 3: Aligned Binary Strings

Binary strings with comments, can also require alignment for readability.
This is another good use case with using the `erlfmt-ignore` comment.

```erlang formatted binary
%% erlfmt-ignore
<<
% Phase 0     Phase 1     Phase 2     Phase 3     Phase 4     Phase 5     Phase 6
% A B C D     A B C D     A B C D     A B C D     A B C D     A B C D     A B C D
2#01001000, 2#10100000, 2#00010000, 2#00010000, 2#00010011, 2#00000000, 2#00000000,  % LUT0 - Black
2#01001000, 2#10100000, 2#10000000, 2#00000000, 2#00000011, 2#00000000, 2#00000000,  % LUTT1 - White
>>
```

## How to specify not to format

From our examples it seems not formatting is useful for code where alignment makes code more readable.
Given we are now motivated to make it possible to exclude some things from the formatter, we needed a way to do that.

Goals:

- Compatible: Usable inside all erlang files, including for example `rebar.config`
- Future Proof: We cannot foresee all exceptions.
- Localized: Encourages formatting most of the file

### 1. Comment Off/On

Use a comment that can turn formatting off and then later it back on.

Prior Art:

- [Python Black](https://github.com/psf/black#the-black-code-style) comments around expression
- [YaPF](https://github.com/google/yapf#why-does-yapf-destroy-my-awesome-formatting) comments around or after expression

- ✅ Compatible: Comments can be applied inside rebar.config files, etc.
- ✅ Future Proof: Could be “abused” for not formatting other things
- ❌ Localized: It is very easy to forget to turn the formatter on again and makes it easier to not format large pieces of code.

### 2. Comment Off

Use a comment that can turn formatting off for the next top-level expression to which the comment is attached to.
Prior Art includes: Javascript's [Prettier](https://prettier.io/docs/en/ignore.html) comments before expression.

- ✅ Compatible: Comments can be applied inside rebar.config files, etc.
- ✅ Future Proof: Could be “abused” for not formatting other things
- ✅ Localized: Less Comments than Off/On Alternative

This is the chosen option.

### 3. Pragma

We already have `@format` pragma comment to opt in and out of formatting per file.
We could reuse this and disable formatting for the entire file.
Prior Art: This is what elixir formatter suggests to do in cases like this.

- ✅ Compatible: Comments can be applied inside rebar.config files, etc.
- ✅ Future Proof: Could be “abused” for not formatting other things
- ❌ Localized: A whole file is not formatted

### 4. No Line Breakings

Strictly follow the user-provided line breaks inside lists, don’t remove them when re-formatting.
Prior Art: gofmt, has very little opinion about new lines in general.

- ✅ Compatible: Comments can be applied inside rebar.config files, etc.
- ❌ Future Proof: This only covers a specific case.
- ❌ Localized: This affects all lists everywhere, which gives rise to more formatting discussion between users, which exactly contradicts the project goal.

### 5. Limited no line breaking

A fusion between option 1 and 4 - strictly follow user-provided line breaks inside lists only in regions delimited by a special comment.

- ✅ Compatible: Comments can be applied inside rebar.config files, etc.
- ❌ Future Proof: For example this doesn't work with the DELTA_MATRIX
- ✅ Localized: This only affects a specific list.

### 6. Marco or function call

A macro could also change the indentation.

- ❌ Compatible: This will require an include file and can not use macro inside rebar.config and other files
- ✅ Future Proof: It can wrap any expression.
- ✅ Localized: Easy to target an exact expression.

### 7. macro with magic name

This doesn’t need to be imported, but is redefine in the same file.

```erlang
-define(IDERLFMT(ID), ID).

gen_part({constructed, bif}, TypeName, {_Name, parts, Tag, _Type}) ->
    ?IDERLFMT(emit(["  case Data of",nl,
                    "    L when is_list(L) ->",nl,
                    "      'dec_",TypeName,"'(lists:map(fun(X) -> element(1, ",
                    {call,ber,ber_decode_erlang,["X"]},") end, L),",{asis,Tag},");",nl,
                    "    _ ->",nl,
                    "      [Res] = 'dec_",TypeName,"'([Data],",{asis,Tag},"),",nl,
                    "      Res",nl,
                    "  end"])).
```

- ❌ Compatible: A macro inside rebar.config and other files
- ✅ Future Proof: It can wrap any expression.
- ✅ Localized: Easy to target an exact expression.

## References

- The discussion started in [issue 19](https://github.com/WhatsApp/erlfmt/issues/19), formatting of DSLish lists.
- Example of work around for DSLish lists and more examples like matrices, were discussed in [pull request 93](https://github.com/WhatsApp/erlfmt/pull/93)
