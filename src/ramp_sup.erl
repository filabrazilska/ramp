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
    Children = [{ramp_storage, {ramp_storage, start_link, []},
                 permanent, 1000, worker, [ramp_storage]}],
    {ok, {{one_for_one, 1, 5}, Children}}.
