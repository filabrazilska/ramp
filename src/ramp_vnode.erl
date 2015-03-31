-module(ramp_vnode).
-behaviour(riak_core_vnode).
-include("include/ramp.hrl").

-export([start_vnode/1,
         init/1,
         delete/1,
         is_empty/1,
         handle_command/3,
         handle_coverage/4,
         handle_exit/3,
         handle_handoff_command/3,
         handle_handoff_data/2,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         encode_handoff_item/2,
         terminate/2
        ]).

-record(state, {partition}).

%%%===================================================================
%%% Callbacks
%%%===================================================================
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition}}.

delete(State) ->
    {ok, State}.

is_empty(State) ->
    {true, State}.

handle_command({ReqId, {prepare, I}}, _From, State) ->
    ok = ramp_storage:prepare(I),
    {reply, {ReqId, ok}, State};
%handle_command({ReqId, {commit, Ts}}, _From, State) ->
handle_command({commit, Ts}, _From, State) ->
    ok = ramp_storage:commit(Ts),
    {reply, ok, State};
%    {reply, {ReqId, ok}, State};
handle_command({ReqId, {get, Key}}, _From, State) ->
    {ok, Item} = ramp_storage:get(Key),
    {reply, {ReqId, {ok, Item}}, State};
handle_command({ReqId, {get, Key, Ts}}, _From, State) ->
    {ok, Item} = ramp_storage:get(Key, Ts),
    {reply, {ReqId, {ok, Item}}, State};
handle_command(Msg, _From, State) ->
    ?PRINT({unhandled_cmd, Msg}),
    {noreply, State}.

handle_coverage(_Req, _KeySpaces, _From, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

handle_handoff_command(_Msg, _From, State) ->
    {noreply, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

handoff_starting(_To, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_To, State) ->
    {ok, State}.

encode_handoff_item(_Name, _Val) ->
    <<>>.

terminate(_Reason, _State) ->
    ok.
