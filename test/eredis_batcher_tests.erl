-module(eredis_batcher_tests).
-include_lib("eunit/include/eunit.hrl").

-define(MOD, eredis_batcher).

all_test_() ->
    [
     {timeout, 50, [{"Integration test",                                  fun highload/0}]}
    ].

highload() ->
    {ok, Redis} = eredis:start_link(),

    RedisOpts = ["127.0.0.1", 6379, 0],
    {ok, Pid} = ?MOD:start_link(RedisOpts, 100000),

    eredis:q(Redis, [<<"DEL">>, <<"f">>]),
    [?MOD:q(Pid, [<<"INCR">>, <<"f">>]) || _ <- lists:seq(1, 1000)],
    ?MOD:sync(Pid, 100000),
    ?assertEqual({ok, <<"1000">>}, eredis:q(Redis, [<<"GET">>, <<"f">>])),

    eredis:q(Redis, [<<"DEL">>, <<"f">>]),
    [?MOD:q(Pid, [<<"INCR">>, <<"f">>]) || _ <- lists:seq(1, 1000)],
    ?MOD:sync(Pid, 100000),
    ?assertEqual({ok, <<"1000">>}, eredis:q(Redis, [<<"GET">>, <<"f">>])),

    unlink(Pid),
    exit(Pid, kill).
