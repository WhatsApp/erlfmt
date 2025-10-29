# Formatting Decision: Pipes

This is a document explaining our reasoning behind the formatting decision for pipes.

The most polarizing issue about pipes is whether you prefer to have `|` (pipes) as a suffix of the expression on each line:

```erlang
-spec foo() ->
    term() |
    [term()] |
    error.
```

Or is if you prefer to have a `|` character as a prefix of the expression on each line:

```erlang
-spec foo() ->
    term()
    | [term()]
    | error.
```

We have decided to try and settle this issue using an analysis of what is more popular in the practice.

## Analysis

Our analysis is very naive and we hope it is good enough, but we are open to contributions for a more sophisticated analysis.
We have decided to analyse the OTP and WhatsApp code base, since this was too big code bases we had access to.

We `grep` `.hrl` and `.erl` files recursively for lines that end or start with a `|` character, disregarding:

- whitespace `\s*`
- lines that contain more than one `|` on the same line: `grep -v -E '\|.*\|'`
- lines that contain lists or comments `grep -v -E '(\]|\[|%)'`

Here follows our naive analysis script for reproducibility:

```sh
# count lines that end with a pipe (suffix)
grep -r -E --include "*\.hrl" --include "*\.erl" '\w+\s*\|\s*$' . | grep -v -E '\|.*\|' | grep -v -E '(\]|\[|%)' | wc -l
# count lines that start with a pipe (prefix)
grep -r -E --include "*\.hrl" --include "*\.erl" '^\s+\|\s*\w+' . | grep -v -E '\|.*\|' | grep -v -E '(\]|\[|%)' | wc -l
```

We found that both OTP and WhatsApp prefer pipes as a prefix.

OTP:

- Pipes as a Suffix: 531
- Pipes as a Prefix: 734

WhatsApp

- Pipes as a Suffix: x
- Pipes as a Prefix: 50x

## History

Previously `erlfmt` formatted pipes as a suffix, because this is consistent with other binary operators.
We proposed and updated this implementation and move pipes to be a prefix.
