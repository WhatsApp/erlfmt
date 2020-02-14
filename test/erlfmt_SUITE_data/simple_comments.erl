-module(simple_comments).

-export([foo/0]).

-export_type([foo/0]).

-record(
    foo,
    {
        %% comment
        a
    }
).

-define(CONST, const).

-define(
    STR(X),
    %% comment 1
    ??X
    %% comment 2
).

-define(FUN(M, F), M:F).

-type bar() ::
          %% comment
          fun().

-type foo() ::
          %% comment 1
          fun((
                  %% comment 2
                  ...
              ) -> %% comment 3
                   bar()).

%% TODO: fix indentation after |
-spec foo() ->
             %% comment
             term() |
                 %% other comment
                 [term()].
foo() ->
    [
        [bar(1), baz(2), foobar(), map(#{}), record(#foo{}), binary(), comprehension()],
        [call(), 'case'(), 'receive'(), 'if'(), 'try'(), 'fun'()]
    ].

bar(X)
when %% comment
     is_list(X) ->
    ok.

baz(Y)
when %% comment
     is_list(Y);
     %% other comment
     is_binary(Y) ->
    ok.

foobar() ->
    %% comment 1
    [
        %% comment 2
        1,
        %% comment 3
        2
        %% comment 4
    ].

map(M) ->
    #{
        %% comment 1
        foo =>
            %% comment 2
            M#{
                foo :=
                    %% comment 3
                    baz
            }
        %% comment 4
    }.

record(R) ->
    #foo{
        %% comment 1
        a =
            %% comment 2
            R#foo{
                a =
                    %% comment 3
                    {
                        %% comment 4
                        R#foo.a,
                        %% comment 5
                        #foo.a
                    }
            }
        %% comment 4
    }.

binary() ->
    <<
        %% comment 1
        (1 +
             %% comment 2
             2)/binary
        %% comment 3
    >>.

comprehension() ->
    [
        [
            %% comment 1
            X
        ]
        || %% comment 2
           X <-
               [
                   %% comment 3
               ]
           %% comment 4
    ].

call() ->
    %% comment 1
    ?FUN(
        %% comment 2
        ?CONST,
        b
        %% comment 3
    )(
        %% comment 4
        (list_to_atom(
             %% comment 5
             "foo"
         )):bar()
        %% comment 6
    ).

'case'() ->
    %% comment 1
    case {
             %% comment 2
             value
         } of
        %% comment 3
        {_} ->
            %% comment 4
            ok
        %% comment 5
    end.

'receive'() ->
    %% comment 1
    receive
        %% comment 2
        ok ->
            %% comment 3
            ok
        %% comment 4
    end,
    receive
        ok -> ok
    %% comment 5
    after 1 ->
        %% comment 6
        ok
        %% comment 7
    end.

'if'() ->
    %% comment 1
    if
        %% comment 2
        true ->
            %% comment 3
            ok
        %% comment 4
    end.

'try'() ->
    %% comment 1
    try ok of
        %% comment 2
        ok ->
            %% comment 3
            ok
    catch
        %% comment 4
        _ ->
            %% comment 5
            error
    after
        %% comment 6
        'after'
        %% comment 7
    end.

'fun'() ->
    %% comment 1
    fun 'fun'/0,
    %% comment 2
    _ = fun ?MODULE:'fun'/0,
    %% comment 3
    fun
        () ->
            %% comment 4
            ok
        %% comment 5
    end.
