# erlfmt

An Erlang code formatter

## Build

    $ rebar3 compile

## Run

    $ rebar3 escriptize
    $ _build/default/bin/erlfmt -h

## Run as a rebar3 plugin

Add `{plugins, [erlfmt]}` to your `rebar.config`, then run the `rebar3 fmt` task to
run the formatter. For all options available on the command line, you can provide them
in `rebar.config` under `{erlfmt, [...]}`.

## Internal Documentation
See documentation in the `doc/` directory.

## Test

    $ rebar3 ct
    $ rebar3 dialyzer

## Join the erlfmt community
* Website:
* Facebook page:
* Mailing list
* irc:

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## License
erlfmt is Apache 2.0 licensed, as found in the LICENSE file.
