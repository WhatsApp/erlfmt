#! /usr/bin/env escript

-module(t).

-mode(compile).

-export([main/1]).

main(_) -> io:fwrite("hello world\n").
