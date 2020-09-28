## Formatting Decision: Number of Spaces

This is a document explaining our reasoning behind the formatting decision for the default number of spaces, representing an indentation.
This document assumes that indentation is not aligned and that indentation requires a fixed number of spaces.
The document discusses why we chose 4 spaces instead of 2.

## Analysis

Our analysis is very naive and we hope it is good enough, but we are open to contributions for a more sophisticated analysis.
We have decided to analyse the OTP, WhatsApp code base and some others, since this was too big code bases we had access to.

We decided to grep for a start of a function implementation `->` followed by a newline and 2 or 4 spaces `\s`.

```sh
# count 4 spaces
$ pcregrep -Mr --include=".*\.erl" --include=".*\.hrl" "\->\n\s\s\s\s\w" . | grep "\->" | wc -l
# count 2 spaces
$ pcregrep -Mr --include=".*\.erl" --include=".*\.hrl" "\->\n\s\s\w" . | grep "\->" | wc -l
```

## Results

OTP:
  - 2 spaces: 16366
  - 4 spaces: 113481

WhatsApp:
  - 2 spaces: x
  - 4 spaces: 20x

[Kazoo](https://github.com/2600hz/kazoo):
  - 2 spaces: 0
  - 4 spaces: 37355

[MongooseIM](https://github.com/esl/MongooseIM):
  - 2 spaces: 30
  - 4 spaces: 13740

[ejabberd](https://github.com/processone/ejabberd):
  - 2 spaces: 349
  - 4 spaces: 9010

[Inaka repos](#inaka):
  - 2 spaces: 2800
  - 4 spaces: 1349

## Decision

The issue seems to be divisive per erlang code base, but still the numbers show that 4 spaces are more popular.
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