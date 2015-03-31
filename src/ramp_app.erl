-module(ramp_app).
-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================
%%% Callbacks
%%%===================================================================
start(_Type, _Args) ->
%    ok = mnesia:start(),
    case ramp_sup:start_link() of
        {ok, _Pid} = R ->
            ok = riak_core:register([{vnode_module, ramp_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(ramp_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(ramp_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(ramp, self()),
            R;
        {error, _Reason} = E ->
            E
    end.

stop(_State) ->
    ok.
