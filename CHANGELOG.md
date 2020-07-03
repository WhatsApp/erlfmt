# Changelog

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
