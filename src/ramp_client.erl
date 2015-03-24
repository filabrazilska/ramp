-module(ramp_client).

-export([put_all/1,
         get_all/1
        ]).

-include("include/ramp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-spec put_all([#kv{}]) -> ok.
put_all(Data) ->
    Ts = {now(), node()},
    Keys = [X#kv.item || X <- Data],
    %TODO: make it a parallel-for
    lists:foreach(fun (#kv{item = Item, value = Value}) ->
                          Md = lists:delete(Item, Keys),
                          I = #ramp_kv{item = Item,
                                       value = Value,
                                       ts = Ts,
                                       md = Md},
                          ramp_storage:prepare(I)
                  end, Data),
    ramp_storage:commit(Ts).

%%%procedure GET_ALL (I : set of items)
%%% ret <- {}
%%% parallel-for i in I
%%%    ret[i] <- GET (i, 0)
-spec get_all([term()]) -> [term()].
get_all(Keys) ->
    %TODO: make it parallel
    Ret = lists:foldl(fun (Key, Acc) ->
                            {ok, Item} = ramp_storage:get(Key),
                            dict:append(Item#ramp_kv.item, Item, Acc)
                        end, dict:new(), Keys),
    Vlatest = find_latest_timestamps(Ret),
    get_latest_versions(Ret, Vlatest, Keys).

%%%===================================================================
%%% Private functions
%%%===================================================================

%%% vlatest <- {} (default value: -1)
%%% for response r in ret do
%%%     for itx in r.md do
%%%         vlatest[itx] <- max(vlatest[itx], r.tsv )
-spec find_latest_timestamps(dict:dict()) -> dict:dict().
find_latest_timestamps(Ret) ->
    dict:fold(fun (_Item, [#ramp_kv{ts = Ts, md = MD}], Acc) ->
                        lists:foldl(fun (Item, InnerAcc) ->
                                            case dict:find(Item, InnerAcc) of
                                                error -> % not present
                                                    dict:append(Item, Ts, InnerAcc);
                                                {ok, [ItemTs]} when ItemTs < Ts ->
                                                    dict:append(Item, Ts, dict:erase(Item, InnerAcc));
                                                _ ->
                                                    InnerAcc
                                            end
                                    end, Acc, MD)
                end, dict:new(), Ret).

%%% parallel-for item i in I
%%%     if vlatest[i] > ret[i].tsv then
%%%         ret[i] <- GET(i, vlatest[i])
%%%
%%% return ret
-spec get_latest_versions(dict:dict(), dict:dict(), [term()]) -> [term()].
get_latest_versions(Ret, Vlatest, Keys) ->
    %TODO: make it parallel
    lists:reverse(
      lists:foldl(
        fun (Key, Acc) ->
                VTs = case dict:find(Key, Vlatest) of
                          error -> old; % item not present in any metadata -> do not fetch the latest version
                          {ok, [Latest]} -> Latest
                end,
                [RTs] = dict:fetch(Key, Ret),
                case VTs of
                    VTs when VTs > RTs#ramp_kv.ts ->
                        {ok, I} = ramp_storage:get(Key, VTs),
                        [I#ramp_kv.value | Acc];
                    VTs when VTs =< RTs#ramp_kv.ts ->
                        [RTs#ramp_kv.value | Acc]
                end
        end, [], Keys)).

%%%===================================================================
%%% Unit tests
%%%===================================================================

-ifdef(TEST).

find_latest_timestamps_test() ->
    ?assertEqual(dict:new(), find_latest_timestamps(dict:new())),
    Single = dict:append(1, #ramp_kv{ts={{1,1,1},node()},item=1,value=a,md=[]}, dict:new()),
    One = dict:append(4, #ramp_kv{ts={{1,1,1},node()},item=4,value=d,md=[1]}, Single),
    Newer = dict:append(2, #ramp_kv{ts={{1,1,2},node()},item=2,value=b,md=[1]}, One),
    ?assertEqual([],dict:to_list(find_latest_timestamps(Single))),
    ?assertEqual([{1,[{{1,1,2},node()}]}],dict:to_list(find_latest_timestamps(Newer))),
    %TODO: dict:fold/3 is in undefined order so it may not test the catch-all branch
    Older = dict:append(3, #ramp_kv{ts={{1,1,0},node()},item=3,value=c,md=[1]}, One),
    Multiple = dict:append(5, #ramp_kv{ts={{1,1,5},node()},item=5,value=e,md=[2,4]},Older),
    ?assertEqual([{1,[{{1,1,1},node()}]}],dict:to_list(find_latest_timestamps(Older))),
    ?assertEqual([{1,[{{1,1,1},node()}]},
                  {2,[{{1,1,5},nonode@nohost}]},
                  {4,[{{1,1,5},nonode@nohost}]}],
                 lists:keysort(1,dict:to_list(find_latest_timestamps(Multiple)))).

-endif.
