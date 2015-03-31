-module(ramp_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Callbacks
%%%===================================================================
init([]) ->
    Storage = {ramp_storage, {ramp_storage, start_link, []},
               permanent, 1000, worker, [ramp_storage]},
    OpSup = {ramp_op_fsm_sup, {ramp_op_fsm_sup, start_link, []},
             permanent, infinity, supervisor, [ramp_op_fsm_sup]},
    VMaster = {ramp_vnode_master, {riak_core_vnode_master, start_link, [ramp_core_vnode]},
               permanent, 5000, worker, [ramp_vnode_master]},
    {ok, {{one_for_one, 1, 5}, [Storage, OpSup, VMaster]}}.
