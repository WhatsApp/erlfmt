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

% doc_string represents Literal text, which is simply printed as is.
-record(doc_string, {
    string :: unicode:chardata()
}).

-record(doc_line, {
    count :: integer()
}).

-type ghi() ::
    fun()
    | binary().

-type klm() :: fun() | binary().

foo() -> ?FOO.

bar() -> ?BAR.

% no empty lines around "if"-like attributes
-ifndef(TEST).
-define(THISISPROD, true).
-define(OTHER, foo).
-else.
-define(THISISPROD, false).
-endif.

% with empty lines around "if"-like attributes
-ifndef(TEST).

-define(THISISPROD, true).
-define(OTHER, foo).

-else.

-define(THISISPROD, false).

-endif.
