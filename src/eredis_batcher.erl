-module(eredis_batcher).
-behaviour(gen_server).

-export([
         start_link/2,
         start_link/3,
         q/2,
         sync/2
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(st, {
          queries = [],
          count = 0,
          dropped = 0,
          limit,
          redis
         }).

start_link(RedisOpts, Limit) ->
    gen_server:start_link(?MODULE, [RedisOpts, Limit], []).

start_link(Name, RedisOpts, Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, [RedisOpts, Limit], []).

q(Pid, Query) ->
    gen_server:cast(Pid, {q, Query}).

sync(Pid, Timeout) ->
    gen_server:call(Pid, sync, Timeout).

%% ===================================================================
%% GEN SERVER
%% ===================================================================

init([RedisOpts, Limit]) ->
    {ok, Conn} = apply(eredis_sync, connect_db, RedisOpts),
    St = #st{limit = Limit, redis = Conn},
    {ok, St}.

terminate(_Reason, _St) -> ok.

handle_cast({q, _Query}, #st{count = Limit, limit = Limit, dropped = Dropped} = St) ->
    {noreply, St#st{dropped = Dropped+1}};
handle_cast({q, Query}, #st{queries = []} = St) ->
    self() ! flush,
    {noreply, St#st{queries = [Query], count = 1}};
handle_cast({q, Query}, #st{queries = Queries, count = Count} = St) ->
    {noreply, St#st{queries = [Query|Queries], count = Count+1}}.

handle_call(sync, From, St) ->
    do_sync(From, St, erlang:process_info(self(), message_queue_len)).

handle_info(flush, St) ->
    {noreply, do_flush(St)};
handle_info(_Info, St) -> {noreply, St}.
code_change(_OldVsn, St, _Extra) -> {ok, St}.

do_sync(_From, #st{count = 0} = St, {message_queue_len, 0}) ->
    {reply, ok, St};
do_sync(From, St, {message_queue_len, _}) ->
    self() ! {'$gen_call', From, sync},
    {noreply, St}.

do_flush(#st{redis = Conn, queries = Queries, dropped = 0} = St) ->
    case eredis_sync:qp(Conn, Queries) of
        {error, Error} ->
            lager:error("batch failed with error ~p", [Error]),
            self() ! flush,
            St;
        _Results ->
            St#st{queries = [], count = 0}
    end;
do_flush(#st{dropped = Dropped} = St) ->
    lager:error("dropped ~p", [Dropped]),
    do_flush(St#st{dropped = 0}).

