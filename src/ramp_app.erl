-module(ramp_app).
-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================
%%% Callbacks
%%%===================================================================
start(_Type, _Args) ->
    ok = mnesia:start(),
    ramp_sup:start_link().

stop(_State) ->
    ok.
