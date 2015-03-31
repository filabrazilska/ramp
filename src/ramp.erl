-module(ramp).

-export([start/0, stop/0]).
-export([ping/0]).

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

% @doc Ping a random node
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    [{IndexNode, _Type}] = riak_core_apl:get_primary_apl(DocIdx, 1, ramp),
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, ramp_vnode_master).
