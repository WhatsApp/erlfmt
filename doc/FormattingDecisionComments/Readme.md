# Analysis of comments

We have conducted a deeper analysis of comments in several code bases.

We looked at how popular the number of percentages, standalone, aligned and directly following comments are.
You can see the analysis [here](./types.md).

standalone example:
```erlang
   %% My standalone comment
   case IAmOnTheNextLine of
   ...
```

aligned example:
```erlang
#{
    A => a,                   % aligned comment
    B => bb,                  % another aligned comment
    C => ccc                  % and yet another aligned comment
}
```

directly following example:
```erlang
#{
    A => a, % directly following comment
    B => bbbbb, % also directly following comment
    C => cccccc% also directly following if there is no space
}
```

In OTP we saw a clear discipline to use:
  - two percentages for standalone comments and
  - single percentages for directly following and aligned comments
This pattern is also seen in kazoo, ejabberd and MongooseIM, although it is not as disciplined.
Inaka mix use of single and two percentage, so much so that there doesn't seem to be a pattern.
WhatsApp seems to have a preference for single percentages over two percentages for standalone and directly following comments.

WhatsApp, OTP, kazoo, MongooseIM and Inaka have a preference for standalone comments over directly following comments.
Just Inaka slightly prefers directly following comments over standalone comments.
All codebases seem to prefer directly following comments over aligned comments.
It is clear that standalone comments is the most popular, but directly following comments is also very popular.

```
standalone > directly following > aligned
```

Aligned comments are less popular, but we still did an analysis, to see if we could find a specific number at which comments are preferred to be aligned.
There is a pattern in the OTP code base of 17% of comments being aligned at column 48
and 28% of comments aligned at column 68 in the kazoo code base, but otherwise no other patterns jump out.
You can find the analysis [here](./columns.html).
You can download the file and view it in a web browser, sorry for the inconvenience.

## Reproduce Analysis of Column where Comments are aligned

To reproduce the analysis clone the following repos:

 - OTP
 - WhatsApp
 - [Inaka repos](../clone_inaka.sh)
 - [ejabberd](https://github.com/processone/ejabberd)
 - [MongooseIM](https://github.com/esl/MongooseIM)
 - [Kazoo](https://github.com/2600hz/kazoo)

Next edit the paths in the `data` variable in `columns.py` and `types.py` to point to the folders where you have cloned these repos.

And finally run:

```sh
$ make
```

This will result in generating the following files:
  - [columns.html](./columns.html).
  - [types.md](./types.md).
