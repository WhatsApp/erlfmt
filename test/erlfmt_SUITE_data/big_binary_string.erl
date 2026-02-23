-module(big_binary_string).

-export([big_binary_string/0]).

big_binary_string() ->
    <<
        "{\"key\": \"value\", \"nested\": {\"items\": [\"item1\", \"item2\", \"item3\"]}\n"
        "\"description\": \"A somewhat long description that goes on and on and is very verbose\"\n"
    >>.
