# Style Guide

> A programmer does not primarily write code; rather, he primarily writes to
> another programmer about his problem solution. The understanding of this
> fact is the final step in his maturation as technician.
>
> — [What a Programmer Does, 1967][1]

This style guide represents guidelines for writing good Erlang code according
to the authors of erlfmt. It consists of two sections:
  * Linting - with guidelines about code content that are considered good
    practice and need to be applied manually
    (or using [`rebar3_lint`](https://hex.pm/packages/rebar3_lint) / [`elvis`](https://github.com/inaka/elvis))
  * Formatting - with guidelines about code layout that can be applied
    automatically by using `erlfmt`

## Linting

### Naming
* Use `CamelCase` for variables (acronyms are fine both ways)
```erlang
%% Bad
File_name
File_Name
_file
Stored_id
```
```erlang
%% Good
FileName
_File
StoredID
StoredId
```
[Related `elvis` configuration](https://github.com/inaka/elvis_core/wiki/Rules#variable-naming-convention):
  ```erlang
  {elvis_style, variable_naming_convention, #{regex => "^([A-Z][0-9a-zA-Z]*)$"}}
  ```

* Use `snake_case` for functions, attributes, and atoms
```erlang
%% Bad
'Error'
badReturn
read_File
```
```erlang
%% Good
error
bad_return
read_file
```
[Related `elvis` configuration](https://github.com/inaka/elvis_core/wiki/Rules#module-naming-convention):
  ```erlang
  {elvis_style, module_naming_convention, #{regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$"}},
  {elvis_style, function_naming_convention, #{regex => "^([a-z]*_?)*$"}}
  ```

* Use `SCREAMING_SNAKE_CASE` for macro definitions
```erlang
%% Bad
-define(some_value, 100).
-define(someMacro(), hidden:call()).
-define(AnotherMacro, "another macro").
```
```erlang
%% Good
-define(SOME_VALUE, 100).
-define(SOME_MACRO(), hidden:call()).
-define(ANOTHER_MACRO, "another macro").
```
[Related `elvis` configuration](https://github.com/inaka/elvis_core/wiki/Rules#macro-names):
  ```erlang
  {elvis_style, macro_names}
  ```

* Omit `()` in macro names only if they represent constants
```erlang
%% Bad
-define(NOT_CONSTANT, application:get_env(myapp, key)).
-define(CONSTANT(), 100).
```
```erlang
%% Good
-define(NOT_CONSTANT(), application:get_env(myapp, key)).
-define(CONSTANT, 100).
```

* Start names of predicate functions (functions that return a boolean)
  with `is_` or `has_` prefix
```erlang
%% Bad
evenp(Num) -> ...
```
```erlang
%% Good
is_even(Num) -> ...
```

* Avoid using one-letter variable names and `do_` prefixes for functions,
  prefer descriptive names
```erlang
%% Bad
validate([X | Xs]) ->
    do_validate(X),
    validate(Xs).
```
```erlang
%% Good
validate_all([Key | Keys]) ->
    validate(Key),
    validate_all(Keys).
```
[Related `elvis` configuration](https://github.com/inaka/elvis_core/wiki/Rules#variable-naming-convention):
  ```erlang
  {elvis_style, variable_naming_convention, #{regex => "^[A-Z][a-zA-Z]([0-9a-zA-Z]+)$"}}
  ```

* Use `snake_case` for naming applications and modules
```
%% Good
myapp/src/task_server.erl
```
[Related `elvis` configuration](https://github.com/inaka/elvis_core/wiki/Rules#module-naming-convention):
  ```erlang
  {elvis_style, module_naming_convention, #{regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$"}}
  ```

### Booleans

* avoid `and` and `or` operators, prefer` andalso` and `orelse`, or
  in guards, where possible, combine expressions with `,` and `;`
```erlang
%% Bad
is_atom(Name) and Name =/= undefined.
is_binary(Task) or is_atom(Task).
```

```erlang
%% Good
is_atom(Name) andalso Name =/= undefined.
is_binary(Task) orelse is_atom(Task).
```

```erlang
%% Also good
when is_atom(Name), Name =/= undefined.
when is_binary(Task); is_atom(Task).
```

## Formatting

The erlfmt formatter enforces the following rules automatically.

### Whitespace

* avoid trailing spaces

* end each file with a newline

* use single space after comma
```erlang unformatted comma-space
%% Bad
application:get_env(kernel,start_pg2)
```
```erlang formatted comma-space
%% Good
application:get_env(kernel, start_pg2)
```

* use single space before and after binary operator (for example:  `=` , `-`, `|`, `||`, `!`, `->`, or `=>`)
```erlang unformatted binary-space
%% Bad
Values=[
    #{key=>value},
    #rec{key=value,key2=value2},
    120-90,
    Server!Message
]
```
```erlang formatted binary-space
%% Good
Values = [
    #{key => value},
    #rec{key = value, key2 = value2},
    120 - 90,
    Server ! Message
]
```

* do not use space after symbolic unary operators (for example: `+`, `-`)
```erlang unformatted unary-space
%% Bad
Angle = - 45,
WithParens = - ( + 45 ).
```
```erlang formatted unary-space
%% Good
Angle = -45,
WithParens = -(+45).
```

* do not put spaces before or after `( )`, `{ }`, or `[]`  for function/record/maps/lists (declarations, definitions, calls)
```erlang unformatted paren-spaces
%% Bad
function_definition( { ok, Argument } ) ->
    mod:function_call(Argument).

sort( [ 1,2,3 ] ).
```
```erlang formatted paren-spaces
%% Good
function_definition({ok, Argument}) ->
    mod:function_call(Argument).

sort([1, 2, 3]).
```

* do not put spaces around segment definitions in binary patterns
```erlang unformatted binary-pattern
%% Bad
<<102 : 8 / unsigned-big-integer, Rest / binary>>,
<<255/unsigned - big - integer, Rest/binary>>.
```
```erlang formatted binary-pattern
%% Good
<<102:8/unsigned-big-integer, Rest/binary>>,
<<255/unsigned-big-integer, Rest/binary>>.
```

* avoid aligning expression groups
```erlang unformatted groups
%% Bad
Module = Env#env.module,
Arity  = length(Args).

inspect(false) -> "false";
inspect(true)  -> "true";
inspect(ok)    -> "ok".
```
```erlang formatted groups
%% Good
Module = Env#env.module,
Arity = length(Args).

inspect(false) -> "false";
inspect(true) -> "true";
inspect(ok) -> "ok".
```

### Indentation

* use 4 spaces to indent, no hard tabs
* inside of `case`, `if`, `try`, `receive`, and functions write all clauses on the same line or all clauses on separate lines, don't mix two clause styles.

```erlang unformatted clauses
%% Bad
lookup(1) -> one;
lookup(2) ->
    two.

reverse_lookup(one) ->
    1;
reverse_lookup(two) -> 2.
```
```erlang formatted clauses
%% Good
lookup(1) -> one;
lookup(2) -> two.

reverse_lookup(one) ->
    1;
reverse_lookup(two) ->
    2.
```

* always break comma-separated expressions across multiple lines:
```erlang unformatted comma
%% Bad
run() -> compile(), execute().

case application:get_env(kernel, start_pg2) of
    {ok, true} -> pg2:start();
    undefined -> ?LOG_NOTICE("skipping pg2 start"), ok
end.

try Processed = process(Val), info(Processed)
catch
    throw:error -> error
end.
```
```erlang formatted comma
%% Good
run() ->
    compile(),
    execute().

case application:get_env(kernel, start_pg2) of
    {ok, true} ->
        pg2:start();
    undefined ->
        ?LOG_NOTICE("skipping pg2 start"),
        ok
end.

try
    Processed = process(Val),
    info(Processed)
catch
    throw:error -> error
end.
```

* Indent the right-hand side of a binary operator if it is on a different line
```erlang unformatted binary-op
%% Bad
f(Op, Atom) ->
    is_atom(Op) andalso
    Atom =/= '!' andalso
    Atom =/= '='.
```
```erlang formatted binary-op
%% Good
f(Op, Atom) ->
    is_atom(Op) andalso
        Atom =/= '!' andalso
        Atom =/= '='.
```

* When assigning the result of a multi-line expression, begin the expression on a new line
```erlang unformatted newline-assign
%% Bad
Prefix = case Base of
             binary -> "2#";
             octal -> "8#";
             hex -> "16#"
         end.
```
```erlang formatted newline-assign
%% Good
Prefix =
    case Base of
        binary -> "2#";
        octal -> "8#";
        hex -> "16#"
    end.
```

### Exports

Every exported function occupies exactly 1 line, functions with same name, but
different arity can be put on the same line, following the definition:

```erlang formatted export
%% Good
-export([
    api_function/1,
    api_another/2, api_another/3,
    api_third/4
]).
```

### Strings

* do not use multilne strings
```erlang unformatted string-to-concat
%% Bad
Message = "erlfmt is a code formatter for Erlang.

Arguments:

  files -- files to format
..."
```
```erlang formatted string-to-concat
%% Good
Message =
    "erlfmt is a code formatter for Erlang.\n"
    "\n"
    "Arguments:\n"
    "\n"
    "  files -- files to format\n"
    "..."
```
```erlang formatted string-to-concat2
%% Also good
Message =
    "erlfmt is a code formatter for Erlang.\n\n"
    "Arguments:\n\n"
    "  files -- files to format\n"
    "..."
```

## Line length

Line length must not exceed **92** characters.

## Licence

This style guide was created by the maintainers of erlfmt and is licenced under
[the CC by 4.0 license][2] and is partially based on [the Elixir Style Guide][3].

[1]: http://archive.computerhistory.org/resources/text/Knuth_Don_X4100/PDF_index/k-9-pdf/k-9-u2769-1-Baker-What-Programmer-Does.pdf
[2]: https://creativecommons.org/licenses/by/4.0/
[3]: https://github.com/lexmag/elixir-style-guide
