-module(ignore_format_many_old).

%%% erlfmt-ignore-begin
-define(DELTA_MATRIX, [
    [0,   0,   0,   0,   0,   0],
    [0, -16,   0,   0,   0,   0],
    [0,   0,  15,   0,   0,   0],
    [0,   0,   0,   6,   0,   0],
    [0, -16,   0,   0, -14,   0],
    [0,   0,  15,   0,   0,   0]
]).
-define(ALSO_IGNORED, [  1,  2,  3]).

%%% erlfmt-ignore-end
-define(DELTA_MATRIX_FORMATTED, [
    [0,   0,   0,   0,   0,   0],
    [0, -16,   0,   0,   0,   0],
    [0,   0,  15,   0,   0,   0],
    [0,   0,   0,   6,   0,   0],
    [0, -16,   0,   0, -14,   0],
    [0,   0,  15,   0,   0,   0]
]).

%% some comment
%%erlfmt-ignore %
%% another comment
gen_part_decode_funcs({constructed,bif},TypeName,
              {_Name,parts,Tag,_Type}) ->
    emit(["  case Data of",nl,
          "    L when is_list(L) ->",nl,
          "      'dec_",TypeName,"'(lists:map(fun(X) -> element(1, ",
          {call,ber,ber_decode_erlang,["X"]},") end, L),",{asis,Tag},");",nl,
          "    _ ->",nl,
          "      [Res] = 'dec_",TypeName,"'([Data],",{asis,Tag},"),",nl,
          "      Res",nl,
          "  end"]).

% erlfmt-ignore-begin I like the comments next to the statement
f() -> ok. % this is ok
g() -> ok. % this is also ok
% erlfmt-ignore-end I'm done with this style

h() -> ok. % blah

%% TODO write emit
emit(S) ->   ok.
