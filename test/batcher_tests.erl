-module(batcher_tests).
-include_lib("eunit/include/eunit.hrl").

-define(MOD, batcher).

all_test_() ->
    [
     {timeout, 50, [{"Integration test",                                  fun highload/0}]},
     {timeout, 50, [{"Integration test",                                  fun highload2/0}]}
    ].

highload() ->
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

highload2() ->
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
