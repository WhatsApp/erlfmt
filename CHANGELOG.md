# Changelog

## v0.3.0 (09.07.2020)

### Enhancements
  - Allow reading from stdin with command `$ erlfmt -` #46
  - Support Erlang version 23.0

### Bug Fixes
  - Remove trailing spaces from comments #48
  - Make position of return type in specs consistent #47
  - Fix some parser failures in OTP #39
  - Concat converted from a string also forces breaks #43

## v0.2.0 (03.06.2020)

### New formatting algebra
Switched out formatting algorithm.
This is now based on Elixir's greedy algorithm, instead of a lazy algorithm.
This fixes performance issues with laying out larger tuples.

### Bug Fixes
  - Preserve empty lines between comments and expressions
  - Stop adding unnecessary empty lines between attributes
  - Stop indenting list and binary comprehensions unnecessarily
  - Fix formatting multiple files from command line
  - Ensure all parsable OTP files format cleanly

### Enhancements
  - Support formatting escripts and "consult" files like rebar.config
  - Add `--require-pragma` flag to only format files annotated with `@format`

## v0.1.0 (06.04.2020)

Initial release
