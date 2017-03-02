# Batcher

Allow to aggregate commands and send it at once. Usefull for sending smth through the network (redis, influx)

###Test with eredis_sync

```erlang
    {ok, Redis} = eredis:start_link(),

    InitFun = fun() ->
                      {ok, Conn} = eredis_sync:connect_db("127.0.0.1", 6379, 0),
                      Conn
              end,
    ProcessFun = fun(Conn, Queries) ->
                         eredis_sync:qp(Conn, Queries)
                 end,
    {ok, Pid} = batcher:start_link(InitFun, ProcessFun, 100000),

    eredis:q(Redis, [<<"DEL">>, <<"f">>]),
    [?MOD:q(Pid, [<<"INCR">>, <<"f">>]) || _ <- lists:seq(1, 1000)],
    ?MOD:sync(Pid, 100000),
    ?assertEqual({ok, <<"1000">>}, eredis:q(Redis, [<<"GET">>, <<"f">>])),

    unlink(Pid),
    exit(Pid, kill).
```
