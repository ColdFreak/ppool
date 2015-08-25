%% ユーザーのためのAPI
-module(ppool).



start_link() ->
    ppool_supersup:start_link().

stop() ->
    ppool_supersup:stop().

start_pool(Name, Limit, {M,F,A}) ->
    ppool_supersup:start_pool(Name, Limit, {M, F, A}).

stop_pool(Name) ->
    ppool_supersup:stop_pool(Name).


%% ワーカーを起動したいときに直接
%% ppool_servサーバーを呼び出している
%% ことに注目
run(Name, Args) ->
    ppool_serv:run(Name, Args).

sync_queue(Name, Args) ->
    ppool_serv:sync_queue(Name, Args).

async_queue(Name, Args) ->
    ppool_serv:async_queue(Name, Args).
