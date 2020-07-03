-module(comments).

-export([
    has_fanciness/1
]).

-export([
    bar/1, baz/1
]).

%% Constants
-define(VERSION_CHECK_INTERVAL_MILLIS_DEFAULT, 10000). % Minimum interval between health checks
-define(MAX_WRITE_FAILURES, 3).

%% Cp stands for "codepoint"
has_fanciness([Cp | Rest]) ->
    case wa_unicode:bidi_class(Cp) of
        Class when Class =:= 'L'
                 ; Class =:= 'NSM' %% TODO maybe invalidate if there is too many NSM?
                 ->
            has_fanciness(Rest);
        _ ->
            {true, Cp}
    end;
has_fanciness([]) ->
    false.

bar(X) when %% comment
            is_list(X) ->
    ok;
bar(X) when is_atom(X) -> %% comment
    ok.

baz(Y)
when %% comment
     is_list(Y);
     %% other comment
     is_binary(Y) -> ok.
