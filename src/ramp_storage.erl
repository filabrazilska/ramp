-module(ramp_storage).
-behaviour(gen_server).

-export([start_link/0, prepare/1, commit/1, get/1, get/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {}).
-record(latest, {item, ts}).

-include("include/ramp.hrl").

-type ramp_kv() :: #ramp_kv{}.
-export_type([ramp_kv/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok,pid()} | ignore | {error,term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec prepare(ramp_kv()) -> ok.
prepare(Item) ->
    gen_server:call(?MODULE, {prepare, Item}).

-spec commit(term()) -> ok.
commit(Ts) ->
    gen_server:call(?MODULE, {commit, Ts}).

-spec get(term()) -> {ok, ramp_kv()} | not_found | multiple_items_call_doctor.
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

get(Key, Timestamp) ->
    gen_server:call(?MODULE, {get, Key, Timestamp}).

%%%===================================================================
%%% Callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}, 0}.

handle_call({prepare, Item}, _From, State) ->
    mnesia:transaction(fun () ->
        mnesia:write(vals, Item, write)
    end),
    {reply, ok, State};

handle_call({commit, Ts}, _From, State) ->
    {atomic, ok} = mnesia:transaction(fun () ->
        Items = mnesia:read(vals, Ts),
        lists:foreach(fun (#ramp_kv{item = I, ts = T}) ->
                case mnesia:read(latest, I) of
                    [] ->
                        mnesia:write(latest, #latest{item = I, ts = T}, write);
                    [#latest{ts = SavedT}] when SavedT < T ->
                        mnesia:write(latest, #latest{item = I, ts = T}, write);
                    _Newer ->
                        ok
                end
            end, Items)
    end),
    {reply, ok, State};

handle_call({get, Key}, From, State) ->
    case mnesia:transaction(fun () ->
                case mnesia:read(latest, Key) of
                    [] -> not_found;
                    [#latest{ts = SavedT}] -> SavedT
                end
            end) of
        {atomic, not_found} -> {reply, not_found, State};
        {atomic, Ts} ->
            handle_call({get, Key, Ts}, From, State)
    end;

handle_call({get, Key, Ts}, _From, State) ->
    case mnesia:transaction(fun () ->
                case mnesia:read(vals, Ts) of
                    [] -> not_found;
                    Vals -> Vals
                end
            end) of
        {atomic, not_found} -> {reply, not_found, State};
        {atomic, Items} ->
            case [I || I <- Items, I#ramp_kv.item =:= Key] of
                [#ramp_kv{} = V] ->
                    {reply, {ok, V}, State};
                [] ->
                    {reply, not_found, State};
                _ ->
                    {reply, multiple_items_call_doctor, State}
            end
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {atomic, ok} = mnesia:create_table(vals,
                                       [{attributes, record_info(fields, ramp_kv)},
                                        {type, bag},
                                        {record_name, ramp_kv},
                                        {ram_copies, [node()]}]),
    {atomic, ok} = mnesia:create_table(latest,
                                       [{attributes, record_info(fields, latest)},
                                        {ram_copies, [node()]}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Unit tests
%%%===================================================================

-ifdef(TEST).

prepare_test_() ->
    {setup,
     fun() ->
        meck:new(mnesia, [unstick]),
        meck:expect(mnesia, transaction, fun (Fun) -> {atomic, Fun()} end),
        meck:expect(mnesia, write, fun(vals, #ramp_kv{}, write) -> ok end)
     end,
     fun(_) ->
        meck:unload(mnesia)
     end,
     [
      ?_assertMatch({reply, ok, {}},
                    handle_call({prepare,
                                 #ramp_kv{ts={{1,1,1},from}, item=key1, value=value1, md=[]}},
                                from, {})),
      ?_assertError(function_clause,
                    handle_call({prepare, value1}, from, {}))
     ]}.

commit_test_() ->
    {setup,
     fun() ->
        meck:new(mnesia, [unstick]),
        meck:expect(mnesia, transaction, fun (Fun) -> {atomic, Fun()} end),
        meck:expect(mnesia, read,
                    fun(vals, {{1,1,1}, from}) -> [];
                       (vals, {{1,1,2}, from}) -> [#ramp_kv{ts={{1,1,2},from}, item=key1, value=value1, md=[]}];
                       (vals, {{1,1,3}, from}) -> [#ramp_kv{ts={{1,1,3},from}, item=key2, value=value2, md=[]}];
                       (vals, {{6,6,6}, from}) -> [#ramp_kv{ts={{6,6,6},from}, item=key3, value=value3, md=[]}];
                       (latest, key1) -> [];
                       (latest, key2) -> [#latest{ts={{1,1,3}, from}, item=key2}];
                       (latest, key3) -> [#latest{ts={{0,0,0}, from}, item=key2}]
                    end),
       meck:expect(mnesia, write,
                   fun(latest, #latest{ts={{1,1,2}, from}, item = key1}, write) -> ok;
                      (latest, #latest{ts={{6,6,6}, from}, item = key3}, write) -> ok
                   end)
     end,
     fun(_) ->
        meck:unload(mnesia)
     end,
     [
      ?_assertEqual({reply, ok, {}},
                    handle_call({commit, {{1, 1, 1}, from}}, from, {})),
      ?_assertEqual({reply, ok, {}},
                    handle_call({commit, {{1, 1, 2}, from}}, from, {})),
      ?_assertEqual({reply, ok, {}},
                    handle_call({commit, {{6, 6, 6}, from}}, from, {}))

     ]}.

get_test_() ->
    {setup,
     fun() ->
        meck:new(mnesia, [unstick]),
        meck:expect(mnesia, transaction, fun (Fun) -> {atomic, Fun()} end),
        meck:expect(mnesia, read,
                    fun(latest, key1) -> [];
                       (latest, key2) -> [#latest{ts={{1,1,1},from}}];
                       (vals, {{1,1,1},from}) -> [#ramp_kv{ts={{1,1,1},from}, item=key2, value=value2, md=[]}];
                       (vals, {{1,1,2},from}) -> [];
                       (vals, {{1,1,3},from}) -> [#ramp_kv{ts={{1,1,3},from}, item=other, value=value666, md=[]},
                                                  #ramp_kv{ts={{1,1,3},from}, item=key4, value=value4, md=[]}];
                       (vals, {{1,1,4},from}) -> [#ramp_kv{ts={{1,1,4},from}, item=key5, value=value6, md=[]},
                                                  #ramp_kv{ts={{1,1,4},from}, item=key5, value=value5, md=[]}]
                    end)
     end,
     fun(_) ->
        meck:unload(mnesia)
     end,
     [
      ?_assertEqual({reply, not_found, {}},
                    handle_call({get, key1}, from, {})),
      ?_assertEqual({reply, {ok, #ramp_kv{ts={{1,1,1},from},
                                          item=key2,
                                          value=value2,
                                          md=[]}}, {}},
                    handle_call({get, key2}, from, {})),
      ?_assertEqual({reply, not_found, {}},
                    handle_call({get, key3, {{1,1,2},from}}, from, {})),
      ?_assertEqual({reply, not_found, {}},
                    handle_call({get, key3, {{1,1,3},from}}, from, {})),
      ?_assertEqual({reply, {ok, #ramp_kv{ts={{1,1,3},from},
                                          item=key4,
                                          value=value4,
                                          md=[]}}, {}},
                    handle_call({get, key4, {{1,1,3},from}}, from, {})),
      ?_assertEqual({reply, multiple_items_call_doctor, {}},
                    handle_call({get, key5, {{1,1,4},from}}, from, {}))
     ]}.

-endif.
