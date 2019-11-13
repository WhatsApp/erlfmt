-module(erlfmt_format).

-oncall("whatsapp_erlang").

-typing([dialyzer]).

-export([expr_to_algebra/1]).

-import(erlfmt_algebra, [
    document_text/1,
    document_combine/2,
    document_flush/1,
    document_choice/2
]).

-spec expr_to_algebra(erlfmt_parse:abstract_form()) -> erlfmt_algebra:document().
expr_to_algebra({integer, Meta, _Value}) ->
    {text, Text} = proplists:lookup(text, Meta),
    document_text(format_integer(Text)).

%% TODO: handle underscores once on OTP 23
format_integer([B1, B2, $# | Digits]) -> [B1, B2, $# | string:uppercase(Digits)];
format_integer(Other) -> Other.
