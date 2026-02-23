# erlfmt

Opinionated Erlang code formatter. Built with rebar3, runs on OTP 24+.

## Build & Test

```bash
rebar3 compile              # Compile
rebar3 ct                   # Run all tests (Common Test)
rebar3 ct --suite=test/erlfmt_SUITE --case=<test_name>  # Run a single test
rebar3 dialyzer             # Static type analysis
make check                  # Full check: compile, test, dialyzer, format check
```

The `getopt` dependency is only available under the `test` and `release` profiles. Running `rebar3 ct` automatically triggers `escriptize` as a pre-hook.

## Formatting

```bash
make fmt                    # Self-format the project (builds release escript first)
make checkfmt               # Check formatting without writing
```

`src/erlfmt_parse.erl` is auto-generated from `erlfmt_parse.yrl` and excluded from formatting.

## Project Structure

### Source (`src/`)

| File | Role |
|------|------|
| `erlfmt.erl` | Main module: escript entry (`main/1`), rebar3 plugin entry (`init/1`), API (`format_file/2`, `format_string/2`, `read_nodes/1`). Orchestrates the scan-parse-format pipeline. Also handles pragma logic and skip path for `--require-pragma`. |
| `erlfmt_scan.erl` | Scanner wrapping `erl_scan`. Detects shebangs, collects comments, enriches token annotations. |
| `erlfmt_parse.yrl` | Yecc grammar producing the custom AST (compiled to `erlfmt_parse.erl`). |
| `erlfmt_recomment.erl` | Re-inserts comments into the AST as `pre_comments`/`post_comments`/`pre_dot_comments`. |
| `erlfmt_format.erl` | Converts AST nodes to algebra documents via `to_algebra/1`. All layout rules live here. |
| `erlfmt_algebra.erl` | Document algebra engine |
| `erlfmt_cli.erl` | CLI option parsing, file glob expansion, parallel formatting execution. |
| `rebar3_fmt_prv.erl` | Rebar3 provider plugin for `rebar3 fmt`. |

### Formatting pipeline

```
Input -> Scanner (erlfmt_scan) -> Parser (erlfmt_parse)
      -> Recommenter (erlfmt_recomment) -> Formatter (erlfmt_format)
      -> Algebra renderer (erlfmt_algebra) -> Output
```

### Tests (`test/`)

Uses Erlang Common Test. Main suite is `erlfmt_SUITE.erl` with test data in `erlfmt_SUITE_data/`.

**Test patterns:**
- **Parser tests**: parse strings and `?assertMatch` on AST structure
- **Snapshot tests**: `snapshot_same/2` verifies idempotent formatting; `snapshot_formatted/2` compares `.erl` against `.erl.formatted`
- **Pragma tests**: test `--require-pragma`, `--insert-pragma`, and skip behavior
- **Error tests**: verify error handling for invalid directives
- **Range tests**: test formatting of file subsets
