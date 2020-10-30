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

[Inaka repos](./clone_inaka.sh):
  - 2 spaces: 2800
  - 4 spaces: 1349

## Decision

The issue seems to be divisive per erlang code base, but still the numbers show that 4 spaces are more popular.
`erlfmt` will try to enforce this as the consistent style.
