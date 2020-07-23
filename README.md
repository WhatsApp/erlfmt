# erlfmt

erlfmt is an opinionated Erlang code formatter. By automating the process of
formatting the code, it frees your team up to focus on what the code does,
instead of how it looks like.

## Before

Remember reading code before erlfmt and having arguments with co workers :(

```erl
what_is(Erlang) ->
case Erlang of movie->[hello(mike,joe,robert),credits]; language->formatting_arguments end
.
```

## After

Now, with the new erlfmt, code is readable and you get along with your co workers :D

```erl formatted demo2
what_is(Erlang) ->
    case Erlang of
        movie -> [hello(mike, joe, robert), credits];
        language -> no_more_formatting_arguments
    end.
```

*Disclaimer: erlfmt is just a code formatter, not a solution to all life's problems.*

## Line length

erlfmt enforces a consistent style by parsing your code and re-printing it,
while enforcing a selected maximum line-length.

For example, this line that exceeds the length limit:

```erl unformatted scenario
scenario(dial_phone_number(),  ring(), hello(mike),hello(joe), system_working(), seems_to_be())
```

will be re-printed automatically in a vertical style:

```erl formatted scenario
scenario(
    dial_phone_number(),
    ring(),
    hello(mike),
    hello(joe),
    system_working(),
    seems_to_be()
)
```

But this snippet:

```erl formatted hello
hello(mike, joe, robert)
```

will be kept as-is, since it fits in a single line.

## Usage

### Rebar3

The easiest way to use erlfmt is as a rebar plugin, by adding to your
`rebar.config`:

```erl formatted rebarconfig1
{plugins, [erlfmt]}.
```

This will provide a new `rebar3 fmt` task. All erlfmt command-line options
can be configured with defaults in your `rebar.config`, for example:

```erl formatted rebarconfig2
{erlfmt, [
    write,
    {files, "{src,include,test}/*.{hrl,erl}"}
]}.
```

Now you can format all the files in your project by running:

```
$ rebar3 fmt
```

### Escript

Alternatively, you can build a standalone and portable escript and use erlfmt
without rebar (it still requires Erlang to be installed on the target system).

```sh
$ rebar3 as release escriptize
$ _build/release/bin/erlfmt -h
```

You can then run it from the command line:

```sh
$ erlfmt -w './otp/lib/*/{src,include}/*.{erl,hrl}'
```

## Requirements

erlfmt requires Erlang/OTP 21+ and works on all platforms.

## Design principles

The formatter was designed with these main principles in mind:

First, the formatter never changes the semantics or structure of the code. This
means the input AST and the output AST are equivalent. The formatter does not
try to arbitrarily "improve" the code. For the most part it limits its
behaviour to shifting whitespace around - it won't rewrite literals, add
parentheses, reorder exports, etc.

The second principle is to provide as little configuration as possible. This
removes contention points and makes it easier to achieve the goal of consistent
code. Instead of providing configuration, the formatter respects a limited set
of choices made in the original code to preserve intent and make it easier to
achieve beautiful code based on contextual hints.

Furthermore, the formatter avoids special cases as much as possible. For
example, there is no hard-coded behaviour specific to some function - all
functions are laid out the same. There are some clear layout rules and general
structures that are re-used as much as possible between different constructs.
For example, the general layout of lists, functions, maps, records, and
similar, all follow the same "container" rules.

Finally, the formatter should be idempotent. Formatting the code once should
produce the same output as formatting it multiple times.

### Manual interventions

In some cases, the formatter rules might lead to code that looks decent, but
not perfect. Therefore some manual intervention to help the formatter out might
be needed. For example, given the following code:

```erl unformatted split_tokens
split_tokens([{Type, Meta, Value} | Rest], Acc, CAcc) ->
    split_tokens(Rest, [{Type, token_anno(erl_anno:to_term(Meta), #{}), Value} | Acc], CAcc).
```

Because the line-length is exceeded, the formatter will produce the following:

```erl formatted split_tokens
split_tokens([{Type, Meta, Value} | Rest], Acc, CAcc) ->
    split_tokens(
        Rest,
        [{Type, token_anno(erl_anno:to_term(Meta), #{}), Value} | Acc],
        CAcc
    ).
```

It might be more desirable, though, to extract a variable and allow the call to
still be rendered in a single line, for example:

```erl formatted split_tokens2
split_tokens([{Type, Meta, Value} | Rest], Acc, CAcc) ->
    Token = {Type, token_anno(erl_anno:to_term(Meta), #{}), Value},
    split_tokens(Rest, [Token | Acc], CAcc).
```

A similar situation could happen with long patterns in function heads,
for example let's look at this function:

```erl
my_function(
    #user{name: Name, age: Age, ...},
    Arg2,
    Arg3
) ->
    ...
```

Even though the code is perfectly valid, you might prefer not to split the
arguments across multiple lines and move the pattern extraction into the
function body instead:

```erl
my_function(User, Arg2, Arg3) ->
    #user{name: Name, age: Age, ...} = User,
    ...
```

Such transformations cannot be automated since the formatter is not allowed to
change the AST of your program. After running the formatter, especially if
running it for the first time on a sizeable codebase, it's recommended to
inspect the code manually to correct similar sub-par layouts.

### Respecting original format

The formatter keeps the original decisions in two key places

  * when choosing between a "collapsed" and an "expanded" layout for containers
  * when choosing between single-line and multi-line clauses.

#### In containers

For containers like lists, tuples, maps, records, function calls, macro calls,
etc, there are two possible layouts - "collapsed" where the entire collection
is printed in a single line; and "expanded" where each element is printed on a
separate line. The formatter respects this choice, if possible. If there is a
newline between the opening bracket/brace/parenthesis and the first element,
the collection will be always printed "expanded", for example:

```erl formatted foobar
[
    Foo,
    Bar
]
```

will be preserved, even though it could fit on a single line. The fact that
this is controlled by a single newline, allows you to easily convert between
those layouts, for example, merely deleting the first newline from the above
sequence to have:

```erl unformatted foobar1
[    Foo,
    Bar
]
```

and re-running the formatter, will produce:

```erl formatted foobar1
[Foo, Bar]
```

Similarly, adding the single newline back:

```erl unformatted foobar
[
Foo, Bar]
```

and re-running the formatter, will produce the initial format again.

#### In clauses

A similar approach is followed, when laying our clause sequences in functions,
case expressions, receives, etc. The main choice there is simple - should
the clause body be printed directly after `->` or on a new line indented.
The formatter imposes one constraint - either all clauses are printed on
a single line, or in all clauses the body is printed on a new line.
This is controlled by the layout of the first clause, again allowing to change
the layout of the entire sequence with just one character, for example:

```erl formatted is_beautiful
case is_beautiful(Code) of
    true ->
        ring_the_bell();
    false ->
        dig_a_hole()
end
```

Even though, the expressions could all fit on a single line, because there is a
newline in the first clause after `->`, this layout is preserved. If we'd like
to "collapse" it, we can do that by removing the first newline:

```erl unformatted is_beautiful2
case is_beautiful(Code) of
    true ->        ring_the_bell();
    false ->
        dig_a_hole()
end
```

and re-running the formatter will produce:

```erl formatted is_beautiful2
case is_beautiful(Code) of
    true -> ring_the_bell();
    false -> dig_a_hole()
end
```

To go back to the original layout, we can insert the newline back again:

```erl unformatted is_beautiful
case is_beautiful(Code) of
    true ->
ring_the_bell();
    false -> dig_a_hole()
end
```

which after re-formatting will result in the original layout again.

## Integrations

 - Visual Studio Code's [Erlang Formatter](https://marketplace.visualstudio.com/items?itemName=szTheory.erlang-formatter) extension.
 - How to integrate with [doom emacs](https://github.com/WhatsApp/erlfmt/issues/46#issuecomment-655996639)

Add your integration here, by making a pull request.

## Internal Documentation

To learn more about erlfmt internals, please explore the `doc/` directory

## Test

```sh
$ rebar3 ct
$ rebar3 dialyzer
```

## Local use

To format erlfmt itself:

```sh
$ rebar3 as release escriptize
$ _build/release/bin/erlfmt -w "{src,include,test}/*.{hrl,erl}" "rebar.config"
```

## Join the erlfmt community
See the [CONTRIBUTING](.github/CONTRIBUTING.md) file for how to help out.

## License
erlfmt is Apache 2.0 licensed, as found in the LICENSE file.
