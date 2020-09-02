## Formatting Decision: Comments

This is a document explaining our reasoning behind the formatting decision for comments.

## Conventions in Documentation

### Erlang.se

[Programming Rules and Conventions, Section 8.5 Comments](http://www.erlang.se/doc/programming_rules.shtml#HDR36)

> 1. Comments within Erlang code shall start with one percent character (%). If a line only contains a comment, it shall be indented as Erlang code. This kind of comment shall be placed above the statement it refers to. If the comment can be placed at the same line as the statement, this is preferred.
> 2. Comments about a function shall be without indentation and start with two percent characters (%%)
> 3. Comments about the module shall be without indentation and start with three percent characters (%%%)

### Erlang in Action (Book)
Page 43, Section COMMENTS

> Style-wise, comments that follow code on the same line are usually written with only a single % character, whereas comments that are on lines of their own are typically written starting with two % characters ...
>
> (Some people even like to start with three % characters on comment lines that describe things on a whole-file level, such as comments at the top of the source file.)
>
> One good reason to stick to these conventions is that syntax-aware editors such as Emacs and erlIDE can be made to know about them, so that they will indent comments automatically according to how many % characters they begin with.
>
> Now that you have a source file that defines a module, you need to compile it.

### Conventions in Practice

 - Emacs formats single line comments to align.
 - In some places of OTP and RabbitMQ double percentage comments are used instead of single percentage comments:
   * https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_discasm.hrl#L32
   * https://github.com/erlang/otp/blob/master/lib/stdlib/src/gen_server.erl#L562
   * https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_tar.hrl#L32-L39
   * https://github.com/rabbitmq/rabbitmq-server/blob/master/src/rabbit_memory_monitor.erl#L26-L32
   * https://github.com/rabbitmq/rabbitmq-server/blob/master/src/rabbit_mirror_queue_misc.erl#L492
 - Elixir, in the erlang code, single percentage comments are used as double percentage comments:
   * https://github.com/elixir-lang/elixir/blob/master/lib/elixir/src/elixir.erl#L309-L323

## Goals

  - Minimize diff when changing a single line, impacts surrounding context.

## Aligning single percentage comments

Sometimes single percentage comments are aligned.
We have decided not to support this, as it violates one of our goals.

 - ❌ Minimize diff when changing a single line

```erlang
#{
    A => a,  % comment
    B => bb, % another comment
    C => ccc % that comment
}
```

If we add a new field that is longer than the others,
then all the previous lines with comments will also need to change.

```erlang
#{
    A => a,     % comment
    B => bb,    % another comment
    C => ccc,   % that comment
    Longer => d % this comment
}
```

Another example:

```erlang
X =
    A |   % comment
    BB |  % another comment
    CCC.  % that comment
```

```erlang
X =
    A |      % comment
    BB |     % another comment
    CCC |    % that comment
    Longer.  % this comment
```

We have also [seen](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_lint.erl#L107) users try to prevent having to change a lot of lines, by changing where the comment is placed

```erlang
-record(x, {a=a :: 'a' | 'aa',
    b='',         % b comment
    c=cc :: 'cc'  % c comment
    }).
```

```erlang
-record(x, {a=a :: 'a' | 'aa',
    b='',         % b comment
    c=cc :: 'cc', % c comment
    d=dd          % d comment
        :: 'd' | 'dd'
    }).
```

## Erlang mode for Emacs

- ✅ Minimize diff when changing a single line

The standard erlang mode for emacs puts the single quoted comments by default at position 48 on the line.
This means that comments are not realigned based on line lengths, but always aligned at the same fixed column.

```erlang
#{
    A => a,                                    % comment
    B => bb,                                   % another comment
    C => ccc                                   % that comment
}
```

If we add a new field that is longer than the others,
then all the previous lines with comments will not need to change.

```erlang
#{
    A => a,                                    % comment
    B => bb,                                   % another comment
    C => ccc,                                  % that comment
    NotTooLong => d                            % this comment
}
```

We have also [seen](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_lint.erl#L107) users try to prevent having to change a lot of lines,
by changing where the comment is placed, but when the comments are so far aligned to the right, this is less likely to happen.

```erlang
-record(x, {a=a :: 'a' | 'aa',
    b='',                                      % b comment
    c=cc :: 'cc'                               % c comment
    }).
```

```erlang
-record(x, {
    a=a :: 'a' | 'aa',
    b='',                                     % b comment
    c=cc :: 'cc',                             % c comment
    d=dd :: 'd' | 'dd'                        % d comment
}).
```

The downside is when the line is indented a bit and ends up being longer than the 48 characters.
We now have another design decision.

Option 1: Do we simply leave the violating comment at the end up of the line?

```erlang
f() ->
    g(
        #{
            A => a,                           % comment
            B => bb,                          % another comment
            CThisLongerField => with_this_longer_function(), % that comment
            NotTooLong => d                   % this comment
        },
        H,
    ).
```

Option 2: Do we move the violating comment above the line?

```erlang
f() ->
    g(
        #{
            A => a,                           % comment
            B => bb,                          % another comment
            % that comment
            CThisLongerField => with_this_longer_function(),
            NotTooLong => d                   % this comment
        },
        H,
    ).
```

Option 3: Move all comments, when one column is violated?

```erlang
f() ->
    g(
        #{
            % comment
            A => a,
            % another comment
            B => bb,
            % that comment
            CThisLongerField => with_this_longer_function(),
            % this comment
            NotTooLong => d
        },
        H,
    ).
```

This would be extremely complex to implement.
Requiring us to invent another operator in the formatting algebra.

## All comments always on a newline

- ✅ Minimize diff when changing a single line

```erlang
#{
    % comment
    A => a,
    % another comment
    B => bb,
    % that comment
    C => ccc
}
```

Only the added lines are changed when a new longer field is added.

```erlang
#{
    % comment
    A => a,
    % another comment
    B => bb,
    % that comment
    C => ccc,
    % this comment
    Longer => d
}
```

Here is another example

```erlang
X =
    % comment
    A |
    % another comment
    BB |
    % that comment
    CCC.
```

```erlang
X =
    % comment
    A |
    % another comment
    BB |
    % that comment
    CCC |
    % this comment
    Longer.
```

## Single comments on the same line

- ✅ Minimize diff when changing a single line

```erlang
#{
    A => a, % comment
    B => bb, % another comment
    C => ccc % that comment
}
```

Only the added lines are changed when a new longer field is added.

```erlang
#{
    A => a, % comment
    B => bb, % another comment
    C => ccc, % that comment
    Longer => d % this comment
}
```

Here is another example:

```erlang
X =
    A | % comment
    BB | % another comment
    CCC. % that comment
```

```erlang
X =
    A | % comment
    BB | % another comment
    SignificantlyLonger | % this comment
    CCC. % that comment
```
