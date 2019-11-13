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
    document_text(format_integer(Text));
expr_to_algebra({float, Meta, _Value}) ->
    {text, Text} = proplists:lookup(text, Meta),
    document_text(format_float(Text));
expr_to_algebra({char, Meta, Value}) ->
    {text, Text} = proplists:lookup(text, Meta),
    document_text(format_char(Text, Value)).

%% TODO: handle underscores once on OTP 23
format_integer([B1, B2, $# | Digits]) -> [B1, B2, $# | string:uppercase(Digits)];
format_integer(Other) -> Other.

%% TODO: handle underscores in int part on OTP 23
format_float(FloatText) ->
    [IntPart, DecimalPart] = string:split(FloatText, "."),
    [IntPart, "." | string:lowercase(DecimalPart)].

%% Keep \\x escapes as they were, but upcased, special-case space in chars vs strings
format_char("$\\x" ++ Escape, _)  -> ["$\\x" | string:uppercase(Escape)];
format_char(_, $\s) -> "$\\s";
format_char(_, Value) -> [$$ | escape_char(Value)].

escape_char(Char) when Char > $\s, Char =< $~; Char > 16#9F -> [Char];
escape_char($\0) -> "\\0";
escape_char($\n) -> "\\n";
escape_char($\r) -> "\\r";
escape_char($\t) -> "\\t";
escape_char($\v) -> "\\v";
escape_char($\b) -> "\\b";
escape_char($\f) -> "\\f";
escape_char($\e) -> "\\e";
escape_char($\d) -> "\\d";
escape_char(Char) when Char =< 16#9F ->
    ["\\x" | string:pad(integer_to_binary(Char, 16), 2, leading, $0)].
