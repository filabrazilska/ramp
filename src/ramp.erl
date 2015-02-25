-module(ramp).

-export([start/0, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:start(sasl),
    application:start(mnesia),
    application:start(ramp).

stop() ->
    application:stop(ramp),
    application:stop(mnesia),
    application:stop(sasl).
