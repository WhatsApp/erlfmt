## Using erlfmt with Rebar

The easiest way to use erlfmt is as a rebar plugin, by adding to your
`rebar.config`:

```erlang formatted rebarconfig1
{project_plugins, [erlfmt]}.
```

This will provide a new `rebar3 fmt` task.
All erlfmt command-line options can be configured with defaults in your `rebar.config`, for example:

```erlang formatted rebarconfig2
{erlfmt, [
    write,
    {files, "{src,include,test}/*.{hrl,erl}"}
]}.
```

Now you can format all the files in your project by running:

```sh
$ rebar3 fmt
```

And you can add the following command in your CI to ensure your Erlang is formatted:

```sh
$ rebar3 fmt --check
```

This means that `--check` overwrites `--write`.

The options specified in the `rebar.config` file can all be overwritten using command line arguments.

For example, if you want to format in place as specified in the `rebar.config` with the `write` option,
but only format a single file, then you can overwrite the file list:

```sh
$ rebar3 fmt ./src/myfile.erl
```

You could also setup your rebar3 config to:
  - only format files that include a `%% @format` comment, using `require_pragma`,
  - only check files and not format them, using `check` instead of `write`,
  - output which files are being checked, using `verbose` and
  - set the default width, using `{print_width, 100}`

```erlang formatted rebarconfig3
{erlfmt, [
    check,
    require_pragma,
    verbose,
    {print_width, 100},
    {files, ["{src,include,test}/*.{hrl,erl,app.src}", "rebar.config"]}
]}.
```

See the command line help for a complete list of options:
```sh
$ rebar3 --help
```
Simply convert dashes to underscores as appropriate,
for example `--insert-pragma`, becomes `insert_pragma`.

