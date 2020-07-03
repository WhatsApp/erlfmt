-module(attributes).

-inlude("foo.hrl").
-inlude("bar.hrl").

-export([foo/0]).
-export([bar/0]).

% bla
-define(FOO, "foo").
-define(BAR, "bar").
-define(BAZ, [
    "a"
]).

-define(XXX, "xxx").

-record(abc, {
    a,
    b
}).

-record(def, {
    a,
    b
}).

-type ghi() ::
    fun() |
    binary().

-type klm() :: fun() | binary().

foo() -> ?FOO.

bar() -> ?BAR.
