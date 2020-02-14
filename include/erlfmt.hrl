-oncall("whatsapp_erlang").

-define(IS_ATOMIC(Kind),
    Kind =:= integer orelse
    Kind =:= float orelse
    Kind =:= char orelse
    Kind =:= atom orelse
    Kind =:= string orelse
    Kind =:= var
).

-define(IS_COLLECTION(Kind),
    Kind =:= map orelse
    Kind =:= list orelse
    Kind =:= tuple orelse
    Kind =:= bin orelse
    Kind =:= block
).
