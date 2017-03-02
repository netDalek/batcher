-module(batcher_tests).
-include_lib("eunit/include/eunit.hrl").

-define(MOD, batcher).

all_test_() ->
    [
     fun batch_insert/0,
     fun batch_increment_with_aggregation/0
    ].

batch_insert() ->
    InitFun = fun() ->
                      ets:new(?MODULE, [named_table])
              end,
    ProcessFun = fun(Conn, Queries) ->
                         ets:insert(Conn, Queries)
                 end,
    {ok, Pid} = ?MOD:start_link(InitFun, ProcessFun, 100000),

    [?MOD:q(Pid, {I, 1}) || I <- lists:seq(1, 1000)],
    ?MOD:sync(Pid, 100000),
    ?assertEqual(1000, ets:info(?MODULE, size)),

    unlink(Pid),
    exit(Pid, kill).

batch_increment_with_aggregation() ->
    InitFun = fun() ->
                      ets:new(?MODULE, [named_table]),
                      ets:insert(?MODULE, {a, 0}),
                      ?MODULE
              end,
    ProcessFun = fun(Conn, Queries) ->
                         ets:update_counter(Conn, a, length(Queries))
                 end,
    {ok, Pid} = ?MOD:start_link(InitFun, ProcessFun, 100000),

    [?MOD:q(Pid, i) || I <- lists:seq(1, 1000)],
    ?MOD:sync(Pid, 100000),
    ?assertEqual(1000, ets:lookup_element(?MODULE, a, 2)),

    unlink(Pid),
    exit(Pid, kill).
