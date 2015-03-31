-ifndef(RAMP_HRL).
-define(RAMP_HRL, true).

-record(kv, {item, value}).
-record(ramp_kv, {ts, item, value, md}).

-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(KEY(Item), {<<"ramp">>, crypto:hash(md5, Item)}).
-define(TIMEOUT, 5000).

-endif.
