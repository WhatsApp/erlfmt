## Formatting Decision: Commas in Lists

This is a document explaining our reasoning behind the formatting decision for commas in lists.

The most polarizing issue about lists is whether you prefer to have `,` (commas) as a suffix for the expression on each line:

```erlang
[
  x,
  y,
  z
]
```

Or is if you prefer to have a `,` character as a prefix of the expression on each line:

```erlang
[ x
, y
, z
]
```

We have decided to try and settle this issue using an analysis of what is more popular in the practice.

## Analysis

Our analysis is very naive and we hope it is good enough, but we are open to contributions for a more sophisticated analysis.
We have decided to analyse the OTP and WhatsApp code base, since this was too big code bases we had access to.

We `grep` `.hrl` and `.erl` files recursively.
This requires grepping for two patterns:
1. lists with commas at the end of the line (suffixes)
2. lists with commas at the start of the line (prefixes)

### Suffix Pattern

Here are examples of the suffix pattern.

One that starts with a newline after the opening bracket:
```erlang
[
  a,
...
```

One that has no newline after the opening bracket:
```erlang
[ a/1,
...
```

The Suffix pattern:
  - starts with open bracket `(\[)`, followed by
  - optional white space `(\s)*`, followed by
  - optional an new line `(\n)?`, followed by
  - optional white space `(\s)*`, again followed by
  - characters that are not a comma, close bracket, new line or opening call or opening record or tuple `([^,\n\]\{\(])*`, followed by
  - a comma and some optional white space before the new line `,(\s*)\n`

So we end up with the following `pcregrep` pattern, where:
  - `-M` allows us to match over multiple lines.
  - `grep "\["` allows us to only count once per list

```sh
pcregrep --include=".*\.erl" --include=".*\.hrl" -rM "(\[)(\s)*(\n)?(\s)*([^,\n\]\{\(])*,(\s*)\n" . | grep "\[" | wc -l
```

### Prefix Pattern

One that ends with a newline before the closing bracket:
```erlang
...
  , a
]
```

One that has no newline before the closing bracket:
```erlang
...
, a/1 ]
```

The Prefix Pattern:
- starts with a newline, followed by some white space and a comma. `(\n(\s)*,)`, followed by
- characters that are not a comma, closing bracket, new line or opening bracket `([^,\n\]\[])*`, followed by
- an optional newline `(\n)?`, followed by
- some white space `(\s)*`
- and ending with a closing bracket `(\])`

So we end up with the following `pcregrep` pattern, where:
  - `-M` allows us to match over multiple lines.
  - `grep "\]"` allows us to only count once per list

```sh
pcregrep --include=".*\.erl" --include=".*\.hrl" -rM "(\n(\s)*,)([^,\n\]\[])*(\n)?(\s)*(\])" . | grep "\]" | wc -l
```

### Result

We found that both OTP and WhatsApp prefer commas as a suffix.

OTP:
  - Commas as a Suffix: 4533
  - Commas as a Prefix: 20

WhatsApp:
  - Commas as a Suffix: 50x
  - Commas as a Prefix: x

[Kazoo](https://github.com/2600hz/kazoo):
  - Commas as a Suffix: 124
  - Commas as a Prefix: 3116

[MongooseIM](https://github.com/esl/MongooseIM):
  - Commas as a Suffix: 852
  - Commas as a Prefix: 1

[ejabberd](https://github.com/processone/ejabberd):
  - Commas as a Suffix: 173
  - Commas as a Prefix: 0

[inaka repos](#inaka):
  - Commas as a Suffix: 244
  - Commas as a Prefix: 442

We tried some other code bases too: [luerl](https://github.com/rvirding/luerl) and [circuitbreak](https://github.com/klarna/circuit_breaker), but these data sets where too small in comparison.

## Welcoming to new comers

Another consideration is possibly alienating new comers.
We would love to attract more talent to the erlang community.

The following style guides from other languages use ending commas exclusively:
  - [Guido van Rossum's style guide for Python](https://www.python.org/dev/peps/pep-0008/#multiline-if-statements) (consistent ending)
  - [Google's Java style guide](https://google.github.io/styleguide/javaguide.html#s4.8.3.1-array-initializers) (consistent or interspersed endings permitted, but not leading)
  - [Mozilla's JavaScript style guide](https://firefox-source-docs.mozilla.org/code-quality/coding-style/coding_style_js.html) (interspersed ending; old JS did not allow trailing commas)

## Decision

The issue seems to be divisive per erlang code base, but still the numbers show that commas at the end of lines are more popular.
`erlfmt` will try to enforce this as the consistent style.

## Appendix

### Inaka

Reproducing the `inaka` data, can be done by cloning a bunch of the `inaka` repos, using the following script:

```sh
#!/bin/sh
git clone https://github.com/inaka/elvis_core
git clone https://github.com/inaka/elvis
git clone https://github.com/inaka/apns4erl
git clone https://github.com/inaka/sheldon
git clone https://github.com/inaka/shotgun
git clone https://github.com/inaka/cowboy_swagger
git clone https://github.com/inaka/worker_pool
git clone https://github.com/inaka/katana-test
git clone https://github.com/inaka/erlang-github
git clone https://github.com/inaka/cowboy-trails
git clone https://github.com/inaka/katana-code
git clone https://github.com/inaka/tirerl
git clone https://github.com/inaka/gold_fever
git clone https://github.com/inaka/xref_runner
git clone https://github.com/inaka/lasse
git clone https://github.com/inaka/zipper
git clone https://github.com/inaka/canillita
git clone https://github.com/inaka/sumo_db_mysql
git clone https://github.com/inaka/rpsls
git clone https://github.com/inaka/sumo_db_pgsql
git clone https://github.com/inaka/sumo_db
git clone https://github.com/inaka/sumo_rest
git clone https://github.com/inaka/sumo_db_elasticsearch
git clone https://github.com/inaka/spellingci
git clone https://github.com/inaka/beam_olympics-extended
git clone https://github.com/inaka/beam_olympics
git clone https://github.com/inaka/sumo_db_riak
git clone https://github.com/inaka/fiar
git clone https://github.com/inaka/serpents
git clone https://github.com/inaka/sumo_db_mongo
git clone https://github.com/inaka/lsl
git clone https://github.com/inaka/niffy
git clone https://github.com/inaka/toy_kv
```
