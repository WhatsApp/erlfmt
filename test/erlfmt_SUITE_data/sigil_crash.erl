-module(sigil).
-export([f/0]).
% erlfmt:ignore
f() -> ~"foo", ~"foo"a.

% erlfmt:ignore
f() -> ~B"foo", ~B"foo"a.

%erlfmt:ignore
f() -> ~|foo|, ~|foo|a.
