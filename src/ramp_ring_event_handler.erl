-module(ramp_ring_event_handler).
-behaviour(gen_event).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {}).

%%%===================================================================
%%% Callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_event({ring_update, _Ring}, State) ->
    {ok, State}.

handle_call(_Msg, State) ->
    {ok, ok, State}.

handle_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
