-ifndef(UTIL_H).

-define(UTIL_H, true).

-define(UNIXTIME,           util:unixtime()).
-define(UNIXTIME(Datetime), util:unixtime(Datetime)).
-define(LONG_UNIXTIME,      util:longunixtime()).
-define(INTEGER(V),         util:to_integer(V)).
-define(FLOAT(V),           util:to_float(V)).
-define(NUM(V),             util:to_num(V)).
-define(ATOM(V),            util:to_atom(V)).
-define(LIST(V),            util:to_list(V)).
-define(TERM(V),            util:to_term(V)).
-define(BINARY(V),          util:to_binary(V)).

-ifndef(DEBUG).
-define(DEBUG(Format), error_logger:info_msg(Format)).
-define(DEBUG(Format, Data), error_logger:info_msg(Format, Data)).
-endif.

-ifndef(INFO).
-define(INFO(Format), error_logger:info_msg(Format)).
-define(INFO(Format, Data), error_logger:info_msg(Format, Data)).
-endif.

-ifndef(ERROR).
-define(ERROR(Format), error_logger:error_msg(Format)).
-define(ERROR(Format, Data), error_logger:error_msg(Format, Data)).
-endif.

-endif.