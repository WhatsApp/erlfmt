# Erlang Formatter Comparison

|                                                           |erlfmt                                                           |rebar3_format                |steamroller                                    |erl_tidy |
|---                            |---                                                              |---                          |---                                            |--- |
|[File Types](#file-types)                                  |.erl, .hrl, .app, .app.src, .config, .script, .escript           |.erl, .hrl	                |.erl, .hrl, .app, .app.src, .config, .script	|.erl |
|[Macros](#macros)                                          |No crashes formatting OTP                                        |Skips entire files sometimes	|Skips entire files sometimes	                |Crashes sometimes |
|[Comments](#comments)                                      |Preserves and moves to line before                               |Preserves but Floating       |Crashes sometimes and Reorders	                |Crashes sometimes and Floating |
|[Configurable vs Opinionated](#configurable-vs-opinionated)|Opinionated                                                      |Configurable                 |Opinionated                                    |Configurable |
|[Preserving Representation](#preserving-representation)    |Yes                                                              |Some                         |Some                                           |No |
|[Line Break Hints](#line-break-hints)                      |Yes                                                              |No                           |No                                             |No |
|[Opt In/Out](#opt-inout)                                   |per file, per top level expression                               |per file                     |No                                             |No |
|[Speed](#speed)                                            |OTP lib in 7s                                                    |N/A                          |N/A                                            |N/A |

In practice, `rebar3_format` is configurable, so you could embed a formatter of your choosing,
but in this document, when we mention `rebar3_format` we are referring to the rebar3 `default_formatter`.

## File Types

`erlfmt` and `streamroller` support all file types `.erl`, `.hrl`, `.app`, `.app.src`, `.config`, `.script` that are common in the Erlang ecosystem, but `erlfmt` can also handle `.escript` files which include a top `#!/usr/bin/env escript` directive, where most other formatters only support `.erl` and `.hrl` files.

## Macros

One of the biggest lacking features with current Erlang formatters is the handling of macros.  For example:

* `erl_tidy`
    * Crashes on some macros (`argument`, `?NAME`, and `compute`)
    * On some, prints the whole containing function in a huge, single, ugly line (`match`)
* `streamroller`
    * Requires that all macros are defined
    * Skips the whole file, if it contains a marco it can’t handle.
* `rebar3_format`
    * Cannot handle most macros in the example (functions `argument`, `match`, `?NAME` and `compute`)
    * Skips the whole file in these cases
    * Syntax error on double question mark
    * Loses parenthesis in `IMPORTANT_PARENS`, which changes the answer of `?IMPORTANT_PARENS(2 + 1)` from 9 to 5.

`erlfmt` forked the erlang parser to make sure that it can handle macros and can handle all of the following macros and format them.

```erlang
-module(macros).

-include_lib("stdlib/include/assert.hrl").

-define(MACRO(), object).

argument(?MACRO()) -> ok.

match() ->
    ?assertMatch({ok, Num} when is_integer(Num), get_number()).

-define(IMPORTANT_PARENS(Expr), (Expr) * (Expr)).
-define(RECORD_NAME1, #foo).
-define(RECORD_NAME2, foo).

bar() ->
    ?IMPORTANT_PARENS(2 + 1),
    ?RECORD_NAME1{a = 1},
    #?RECORD_NAME2{a = 1}.

-define(NAME, name).

?NAME() -> ok.

-define(FALLBACK_CLAUSE(Name), Name(_) -> default_action()).

compute(X) when is_integer(X) -> ok;
?FALLBACK_CLAUSE(compute).
```

Sometimes there are still edge cases, which `erlfmt` cannot format, but it can at least preserve the exact string, without crashing, while formatting all the other functions/attributes/expressions in the file.

```erlang
-define(line, put(line, ?LINE),).

foo() ->
    ?line compute().
```

If you can find macros where `erlfmt` breaks, please report it, so we can look into it and fix this bug.

## Comments

Another place where some Erlang formatters had problems was with comments. `erlfmt` forked the erlang parser to make sure that it can preserve the positioning of comments.
`rebar3_format` preserves the content of all the comments we tested, including unicode in comments, but given the following input:

```erlang
-type foo() ::
    %% comment 1
    fun(
        (
            %% comment 2
            ...
        ) ->
            %% comment 3
            bar()
    ).

```

`rebar3_format` sometimes moves comments further out of their original scope to the point where they are floating:

```erlang
-type foo() :: fun((...) -> bar()).    %% comment 1

            %% comment 2

            %% comment 3
```

`steamroller` crashes on the following:

```erlang
'try'() ->
    try Expr
    after
        'after'
        %% comment
    end.
```

Given the following input:

```erlang
comprehension() ->
    [
        [
            %% comment 1
            X
        ]
        || %% comment 2
           X <-
               %% comment 3
               [
                   %% comment 4
               ]
           %% comment 5
    ].
```

`steamroller` moves comments out of order:

```erlang
comprehension() ->
  [
    %% comment 2
    %% comment 3
    [
      %% comment 1
      X
    ] || X <- [
      %% comment 4
    ]
    %% comment 5
  ].
```

`erl_tidy` crashes on unicode and when it crashes removes the file, it also results in floating comments, same as `rebar3_format`’s example.

`erlfmt` moves trailing comments above the line, given the following input:

```erlang
%% Constants
-define(VERSION_CHECK_INTERVAL_MILLIS_DEFAULT, 10000). % Minimum interval between health checks
-define(MAX_WRITE_FAILURES, 3).
```

The trailing comment is moved to the line above and the above comment is moved to make space.

```erlang formatted comments
%% Constants

% Minimum interval between health checks
-define(VERSION_CHECK_INTERVAL_MILLIS_DEFAULT, 10000).
-define(MAX_WRITE_FAILURES, 3).
```

`erlfmt` does not reorder comments and does not crash.

## Configurable vs Opinionated

Other Erlang formatters have several configurable options, which can adjust the format.

`erlfmt` is very opinionated and only a single option that will change the format, `print-width` the maximum line length, which has a default of 100.
See the reasoning behind this decision [here](https://github.com/WhatsApp/erlfmt/blob/master/doc/FormattingDecisionDefaultWidth.md).

## Preserving Representation

During parsing with the default Erlang parser, some information is lost, but since `erlfmt` forked the parser, it can preserve the exact representation of strings, atoms and integers.
For example:

```erlang
1_000_000,

"\x61\x62\x63",

"\x61\x62\x63" "\x41\x42\x43",

[1 | [2 | [3 | []]]],

[1 | [2 | [3 | improper_list]]],

{undefined, 'undefined'}.
```

`rebar3_format` simplifies lists, atoms and strings.

```erlang
1_000_000,

"\x61\x62\x63",

"abcABC",

[1, 2, 3],

[1, 2, 3 | improper_list],

{undefined, undefined}.
```

`steamroller` preserves lists, but simplifies strings, atoms and numbers.

```erlang
1000000,
"abc",
"abc" "ABC",
[1 | [2 | [3 | []]]],
[1 | [2 | [3 | improper_list]]],
{undefined, undefined}.
```

`erl_tidy` simplifies all the examples.

```erlang
1000000,

"abc",

"abcABC",

[1, 2, 3],

[1, 2, 3 | improper_list],

{undefined, undefined}.
```

## Line Break Hints

`erlfmt` tries to fit as much as it can on a single line, but will respect user introduced line breaks hints and empty lines in certain places and preserve them.
For example in lists, where the whole list is broken up, if it contains a line break.

```erlang formatted linehints
-export([
    %% public functions
    foo/1, foo/2,
    bar/1,

    %% testing helpers
    baz/5
]).

x() ->
    Foo = [
        short,
        list
    ].
```

`rebar3_format` loses all new lines and moves the comments out of the list, since the list can fit on a single line, completely loosing the information about grouping.

```erlang
-export([foo/1, foo/2, bar/1, baz/5]).    %% public functions

    %% testing helpers

x() ->
    Foo = [short, list].
```

`steamroller` preserves the newlines in the export grouping, since there were comments that forced newlines to exist inside the list, but does not preserve newlines in the short list.

```erlang
-export(
  [
    %% public functions
    foo/1,
    foo/2,
    bar/1,
    %% testing helpers
    baz/5
  ]
).

x() -> Foo = [short, list].
```

`erl_tidy` kept some new lines inside the export statement, we assume because of it originally containing comments.

```erlang
-export([bar/1,
         baz/5,
         foo/1,
         foo/2]).    %% public functions

    %% testing helpers

x() -> Foo = [short, list].
```

If we reformat with `erl_tidy` again these newlines are also removed, because as expected the comments were no longer inside the export statement.

```erlang
-export([bar/1, baz/5, foo/1, foo/2]).

                   %% public functions

    %% testing helpers

x() -> Foo = [short, list].
```

## Opt In/Out

`rebar3-format` allows you to opt out per file by adding `-format ignore.` to the top of your file.

`erlfmt` allows you to opt-in per file and opt-out per top-level expression.
Adding a comment to the top of the file `%% @format` and running `erlfmt` with the `--require-pragma` option, will result in only files that have this comment will be formatted.

Adding a comment `%% erlfmt-ignore` above a top level expression, will skip over this single expression and continue to format the rest of the file.
[Here](https://github.com/WhatsApp/erlfmt/blob/master/doc/FormattingDecisionIgnore.md) you can see the reasoning behind including this option.

## Speed

It was really tough to come up with a way to compare speed, as all other formatters crashes on a large enough test bed.
We are sorry to say that this comparison is not fair.
We ran `erlfmt` on the OTP library, using a MacBook Pro (15-inch, 2019) with a 2.4 GHz 8-Core Intel Core i9 CPU.

```sh
$ time erlfmt -w otp/lib/*/{src,include}/*.{e,h}rl
________________________________________________________
Executed in    6.86 secs   fish           external
   usr time   59.20 secs    1.17 millis   59.20 secs
   sys time    7.85 secs    0.93 millis    7.85 secs

$ cat otp/lib/*/{src,include}/*.{e,h}rl | wc -l
1361202 (1.36M)
```

