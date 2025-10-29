# erlfmt

erlfmt is an opinionated Erlang code formatter. By automating the process of
formatting the code, it frees your team up to focus on what the code does,
instead of what it looks like.

erlfmt is feature complete and released as version 1.0
This means only backwards compatible changes and bug fixes can be adopted without very serious consideration.
We do not want to put users in the position where they need to reformat code without a very good reason.

## Before

Remember reading code before erlfmt and having arguments with co workers :(

```erl
what_is(Erlang) ->
case Erlang of movie->[hello(mike,joe,robert),credits]; language->formatting_arguments end
.
```

## After

Now, with the new erlfmt, code is readable and you get along with your co workers :D

```erlang formatted demo2
what_is(Erlang) ->
    case Erlang of
        movie -> [hello(mike, joe, robert), credits];
        language -> no_more_formatting_arguments
    end.
```

*Disclaimer: erlfmt is just a code formatter, not a solution to all life's problems.*

## Table of Contents

- [Comparison with other Erlang formatters](#comparison-with-other-erlang-formatters)
- [Usage](#usage)
- [Line length](#line-length)
- [Design principles](#design-principles)
- [Manual interventions](#manual-interventions)
- [Respecting original format](#respecting-original-format)
- [Ignoring Formatting](#ignoring-formatting)
- [Join the erlfmt community](#join-the-erlfmt-community)

## Comparison with other Erlang formatters

|                               |erlfmt                                                           |rebar3_format                |steamroller                                    |erl_tidy |
|---                            |---                                                              |---                          |---                                            |--- |
|File Types                     |.erl, .hrl, .app, .app.src, .config, .script, .escript           |.erl, .hrl	                |.erl, .hrl, .app, .app.src, .config, .script	|.erl |
|Macros                         |No crashes formatting OTP                                        |Skips entire files sometimes	|Skips entire files sometimes	                |Crashes sometimes |
|Comments                       |Preserves and moves to line before                               |Preserves but Floating       |Crashes sometimes and Reorders	                |Crashes sometimes and Floating |
|Configurable vs Opinionated    |Opinionated                                                      |Configurable                 |Opinionated                                    |Configurable |
|Preserving Representation      |Yes                                                              |Some                         |Some                                             |No |
|Line Break Hints               |Yes                                                              |No                           |No                                             |No |
|Opt In/Out                     |per file, per top level expression                               |per file                     |No                                             |No |
|Speed                          |OTP lib in 7s                                                    |N/A                          |N/A                                            |N/A |

See the [comparison with other erlang formatters document](./doc/ErlangFormatterComparison.md) for more details.

## Usage

### Rebar3

The easiest way to use erlfmt is as a rebar plugin, by adding to your
`rebar.config`:

```erlang formatted rebarconfig1
{project_plugins, [erlfmt]}.
```

This will provide a new `rebar3 fmt` task. All erlfmt command-line options
can be configured with defaults in your `rebar.config`, for example:

```erlang formatted rebarconfig2
{erlfmt, [write]}.
```

Now you can format all the files in your project by running:

```sh
rebar3 fmt
```

And you can add the following command in your CI to ensure your Erlang is formatted:

```sh
rebar3 fmt --check
```

For more usage instructions, see [RebarUsage](./doc/RebarUsage.md)

### Escript

Alternatively, you can build a standalone and portable escript and use erlfmt
without rebar (it still requires Erlang to be installed on the target system).

```sh
rebar3 as release escriptize
_build/release/bin/erlfmt -h
```

You can then run it from the command line:

```sh
erlfmt -w './otp/lib/*/{src,include}/*.{erl,hrl}'
```

### Requirements

erlfmt requires Erlang/OTP 21+ and works on all platforms.

### Integrations

- Visual Studio Code's [Erlang Formatter](https://marketplace.visualstudio.com/items?itemName=szTheory.erlang-formatter) extension.
- How to integrate with [doom emacs](https://github.com/WhatsApp/erlfmt/issues/46#issuecomment-655996639)
- Use `erlfmt` through [`rebar3_format`](https://github.com/AdRoll/rebar3_format/blob/master/README.md#erlfmt)

Add your integration here, by making a pull request.

## Line length

erlfmt enforces a consistent style by parsing your code and re-printing it,
while enforcing a selected maximum line-length.

For example, this line that exceeds the length limit:

```erlang unformatted scenario
scenario(dial_phone_number(),  ring(), hello(mike),hello(joe), hello(robert),   system_working(), seems_to_be())
```

will be re-printed automatically in a vertical style:

```erlang formatted scenario
scenario(
    dial_phone_number(),
    ring(),
    hello(mike),
    hello(joe),
    hello(robert),
    system_working(),
    seems_to_be()
)
```

But this snippet:

```erlang formatted hello
hello(mike, joe, robert)
```

will be kept as-is, since it fits in a single line.

Note: The enforcing of line-length is best effort and will sometimes overrun the selected line length, because the algorithm is greedy.

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

## Manual interventions

In some cases, the formatter rules might lead to code that looks decent, but
not perfect. Therefore some manual intervention to help the formatter out might
be needed. For example, given the following code:

```erlang unformatted split_tokens
split_tokens([{TokenType, Meta, TokenValue} | Rest], TokenAcc, CommentAcc) ->
    split_tokens(Rest, [{TokenType, token_anno(erl_anno:to_term(Meta), #{}), TokenValue} | TokenAcc], CommentAcc).
```

Because the line-length is exceeded, the formatter will produce the following:

```erlang formatted split_tokens
split_tokens([{TokenType, Meta, TokenValue} | Rest], TokenAcc, CommentAcc) ->
    split_tokens(
        Rest,
        [{TokenType, token_anno(erl_anno:to_term(Meta), #{}), TokenValue} | TokenAcc],
        CommentAcc
    ).
```

It might be more desirable, though, to extract a variable and allow the call to
still be rendered in a single line, for example:

```erlang formatted split_tokens2
split_tokens([{TokenType, Meta, TokenValue} | Rest], TokenAcc, CommentAcc) ->
    Token = {TokenType, token_anno(erl_anno:to_term(Meta), #{}), TokenValue},
    split_tokens(Rest, [Token | TokenAcc], CommentAcc).
```

A similar situation could happen with long patterns in function heads,
for example let's look at this function:

```erlang
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

## Respecting original format

The formatter keeps the original decisions in two key places

- when choosing between a "collapsed", "semi-expanded", and an "expanded" layout for containers
- when choosing between single-line and multi-line clauses.

### In containers

For containers like lists, tuples, maps, records, function calls, etc,
there are three possible layouts - "collapsed" where the entire collection
is printed in a single line; "semi-expanded" where the enclosing
brackets/breaces/parentheses are printed on a line of their own, but all elements
are printed in a single line; and "expanded" where each element is printed on a
separate line. The formatter respects this choice, if possible. If there is a
newline between the opening bracket/brace/parenthesis and the first element,
the collection will be always printed "semi-expanded", for example:

```erlang formatted semi-expanded
[
    Foo, Bar
]
```

will be preserved, even though it could fit on a single line.

Similarly, if there's a break between any elements, the container will be printed
in the "expanded" format:

```erlang formatted expanded
[
    Foo,
    Bar
]
```

This is controlled by the newlines in the original version.
For example, merely deleting the newlines from the above sequence:

```erlang unformatted collapsed
[    Foo, Bar]
```

and re-running the formatter, will produce:

```erlang formatted collapsed
[Foo, Bar]
```

Similarly, adding the single initial newline back:

```erlang unformatted semi-expanded
[
Foo, Bar]
```

and re-running the formatter, will produce the "semi-expanded" format again.

While adding a newline in the middle:

```erlang unformatted expanded
[Foo,
Bar]
```

and re-running the formatter, will produce the "expanded" format again.

### In clauses

A similar approach is followed, when laying our clause sequences in functions,
case expressions, receives, etc. The main choice there is simple - should
the clause body be printed directly after `->` or on a new line indented.
The formatter imposes one constraint - either all clauses are printed on
a single line, or in all clauses the body is printed on a new line.
This is controlled by the layout of the first clause, again allowing to change
the layout of the entire sequence with just one character, for example:

```erlang formatted is_beautiful
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

```erlang unformatted is_beautiful2
case is_beautiful(Code) of
    true ->        ring_the_bell();
    false ->
        dig_a_hole()
end
```

and re-running the formatter will produce:

```erlang formatted is_beautiful2
case is_beautiful(Code) of
    true -> ring_the_bell();
    false -> dig_a_hole()
end
```

To go back to the original layout, we can insert the newline back again:

```erlang unformatted is_beautiful
case is_beautiful(Code) of
    true ->
ring_the_bell();
    false -> dig_a_hole()
end
```

which after re-formatting will result in the original layout again.

## Ignoring Formatting

We found that mostly it is possible to format erlang code in an at least somewhat acceptable way, but exceptions do occur.
We have introduced the `erlfmt:ignore` comment, which when placed before a top-level expression, will indicate to `erlfmt` to skip over that expression, leave it as is and move on to the next expression. For documentation purposes, a reason for not formatting can be given..

```erlang formatted matrix
%% erlfmt:ignore I like it more this way
-define(DELTA_MATRIX, [
    [0,   0,   0,   0,   0,   0],
    [0, -16,   0,   0,   0,   0],
    [0,   0,  15,   0,   0,   0],
    [0,   0,   0,   6,   0,   0],
    [0, -16,   0,   0, -14,   0],
    [0,   0,  15,   0,   0,   0]
]).
```

You can also encose multiple top-level forms in a `erlfmt:ignore-begin`, `erlfmt:ignore-end` section.
```erlang formatted ignore-many
%% erlfmt:ignore-begin
-define(DELTA_MATRIX1, [
    [0,   0,   0,   0,   0,   0]
]).
-define(DELTA_MATRIX2, [
    [0,   0,   0,   0,   0,   0]
]).
%% erlfmt:ignore-end

-define(THIS_IS_FORMATTED, ok).
```

**Only top-level expression are supported.**
Nested expressions are not supported, for example expressions inside functions.

You can also add a comment to `%%% % @noformat` at the top of your file to opt that file out of formatting.
It is also possible to use `%%% % @format` comments at the top of your files with the `--require-pragma` flag to only format opted in files.

## Join the erlfmt community

To learn more about erlfmt internals, please explore the `doc/` directory

See the [CONTRIBUTING](.github/CONTRIBUTING.md) file for how to help out.

### Test

```sh
$ rebar3 ct
$ rebar3 dialyzer
# or
$ make check
```

### Local use

To format erlfmt itself:

```sh
make fmt
```

### Release Process

The [release process](./RELEASE.md) requires a few steps, updating the [CHANGELOG.md](./CHANGELOG.md), releasing to [hex](https://hex.pm/packages/erlfmt) and more.

### Decision Documents

[Formatting Decisions](https://github.com/WhatsApp/erlfmt/blob/main/doc/Readme.md) documents are intended to explain our reasoning for making certain formatting decisions.

### License

erlfmt is Apache 2.0 licensed, as found in the [LICENSE](./LICENSE) file.
