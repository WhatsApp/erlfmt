-module(broken).

-export([foo/0, bar/0]).
-define(PARENS, ()).

foo?PARENS %% comment
    ->ok.

bar()->
    ok. %% comment
