# Formatting Decision: Lists

This is a document explaining our reasoning behind the formatting decision for lists.

This does not discuss prefix or suffix style for commas, which is discussed in [another document](./FormattingDecisionCommas.md).
Here we assume that the decision has been taken for commas as suffixes.

This document also does not discuss the number of spaces used for indentation, which is discussed in [another document](./FormattingDecisionSpaces.md).  Here we assume that the decision has been taken for 4 spaces to be used for indentation.

We chose to format multi line lists with [each element on their own line](each-element-on-their-own-line), because this most closely matched our goals.

Goals:

  - Minimize the diff when changing a single line.
  - [Welcoming to new comers](#welcoming-to-new-comers), because we want erlang to be growing community.
  - [Popular with current users](#analysis), it would be great if the style we pick is already a popular style

Readability would also be a goal, but it seems depending on who we ask each style can be seen as more readable than the other,
which makes it seem as though this is subjective and would require a study to resolve.
Readability studies we found, were for very small sets of people and did not cover this subject.
If you do find one that shows statistically significant results for this subject, please let us know!

Here you follows all the candidates we evaluated against our goals.

## Each Element On Their Own Line

  - ✅ Welcoming to new comers
  - ✅ Minimize the diff when changing a single line
  - ✅ Popular with current users

`erlfmt` formats multiline lists with each element on a separate line:

```erlang formatted list
[
    the_first_and,
    second_element_fits_on_one_line,
    but_the_third_cannot_also_fit
]
```

This style seems to be relatively popular among the erlang community, see our [analysis](#analysis) below.
It also seems to be [popular among other bigger communities](#welcoming-to-new-comers), where we hope to attract talent from, to grow the erlang community.

Finally, given a change to the variable to which this list is assigned to, this format will result in only a single line change.

```erlang formatted change_list
My_Variable = [
    the_first_and,
    second_element_fits_on_one_line,
    but_the_third_cannot_also_fit
]
```

If we change the assigned variable name, only the line with the variable changes:

```erlang formatted change_list2
I_Decided_To_Change_My_Variable_Name = [
    the_first_and,
    second_element_fits_on_one_line,
    but_the_third_cannot_also_fit
]
```

### IO Format Tilde P

  - ✅ Popular with current users
  - ❌ Minimize the diff when changing a single line
  - ❌ Welcoming to new comers

`IO Format Tilde P` represents the format of lists printed by `io:format("~p", [MyList])`.

`io:format ~p` formats lists in a very compact way:

```erlang
[the_first_and,second_element_fits_on_one_line,
 but_the_third_cannot_also_fit]
```

This style is very popular with the erlang community, see our [analysis](#analysis) below.
Unfortunately this style is [not very popular with other bigger communities](#welcoming-to-new-comers),
where we hope to attract new talent from in future.

Finally, given a change to the variable to which this list is assigned, the diff can make it tough to retrace our steps.

```erlang
My_Variable = [the_first_and,second_element_fits_on_one_line,
               but_the_third_cannot_also_fit]
```

If we change the assigned variable name, every line of the list has to change to realign the elements.

```erlang
I_Decided_To_Change_My_Variable_Name = [the_first_and,second_element_fits_on_one_line,
                                        but_the_third_cannot_also_fit]
```

## Analysis

We did an analysis to try and find which style is more popular.

```sh
# open braces followed by new line
$ pcregrep -r --include=".*\.erl" --include=".*\.hrl" "\[(\s)*$" . | grep -v "%" | wc -l
# multi line lists with io:format ~p style
$ pcregrep -r --include=".*\.erl" --include=".*\.hrl" "\[(.*),(\s)*$" . | grep -v "\]" | grep -v "%" | wc -l
```

The results were inconclusive, they show that both styles `io:format ~p` and having each element on their own line are both relatively popular, with no clear winner.

WhatsApp:
  - tildep: x
  - newline: 5x

OTP:
  - tildep: 18095
  - newline: 3415

[Inaka repos](#inaka):
  - tildep: 115
  - newline: 408

[Kazoo](https://github.com/2600hz/kazoo):
  - tildep: 229
  - newline: 36

[MongooseIM](https://github.com/esl/MongooseIM):
  - tildep: 1991
  - newline: 700

[ejabberd](https://github.com/processone/ejabberd):
  - tildep: 1599
  - newline: 32

## Welcoming to new comers

We would love to attract more talent to the erlang community.
Making our format familiar to programmers from other languages, this could help to decrease the barrier to entry.

The following style guides from other languages exclusively places each element on their own line:

  - [Guido van Rossum's style guide for Python](https://www.python.org/dev/peps/pep-0008/#multiline-if-statements)
  - [Google's Java style guide](https://google.github.io/styleguide/javaguide.html#s4.8.3.1-array-initializers)
  - [Javascript's Prettier](https://prettier.io/docs/en/rationale.html#multi-line-objects)
  - [Elixir Style Guide](https://github.com/christopheradams/elixir_style_guide)

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