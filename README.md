# erlfmt

An Erlang code formatter

## Build

    $ rebar3 compile

## Run

    $ rebar3 as release escriptize
    $ _build/release/bin/erlfmt -h

## Run as a rebar3 plugin

Add `{plugins, [erlfmt]}` to your `rebar.config`, then run the `rebar3 fmt` task to
run the formatter. For all options available on the command line, you can provide them
in `rebar.config` under `{erlfmt, [...]}`.

## Internal Documentation
See documentation in the `doc/` directory.

## Test

    $ rebar3 ct
    $ rebar3 dialyzer

## Local use

You can use erlfmt as a rebar plugin on itself thanks to the symlink in `_checkouts` and
recursive plugin override in `rebar.config`.

## Join the erlfmt community
* Website:
* Facebook page:
* Mailing list
* irc:

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## License
erlfmt is Apache 2.0 licensed, as found in the LICENSE file.
