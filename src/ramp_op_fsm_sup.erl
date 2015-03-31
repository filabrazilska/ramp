-module(ramp_op_fsm_sup).
-behaviour(supervisor).

-export([start_write_fsm/1,
         start_link/0]).
-export([init/1]).

start_write_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WriteFsm = {undefined,
                {ramp_op_fsm, start_link, []},
                temporary, 5000, worker, [ramp_op_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [WriteFsm]}}.
