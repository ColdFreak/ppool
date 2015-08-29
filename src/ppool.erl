%% @author Wang Zhijun
%% @doc ユーザーのためのAPI
-module(ppool).
-export([start_link/0, stop/0, start_pool/3,
         run/2, sync_queue/2, async_queue/2, stop_pool/1]).

%% @private 
%% @doc private notationで指定するとedocに{private, true}を指定しない限りドキュメントからなくなる
start_link() ->
    ppool_supersup:start_link().

stop() ->
    ppool_supersup:stop().

%% @doc プールを起動する
%% プールを起動するときにLimitを指定すれば済む
%% run/2呼び出すときLimitは必要はない
start_pool(Name, Limit, {M,F,A}) ->
    ppool_supersup:start_pool(Name, Limit, {M, F, A}).

%% @spec stop_pool(Name::atom()) -> any()
%% @doc 指定したプールを削除する
stop_pool(Name) ->
    ppool_supersup:stop_pool(Name).


%% @doc ワーカーを起動したいときに直接
%% ppool_servサーバーを呼び出している
%% ことに注目
run(Name, Args) ->
    ppool_serv:run(Name, Args).

%% @todo 実装をチェック
sync_queue(Name, Args) ->
    ppool_serv:sync_queue(Name, Args).

%% @todo 実装をチェック
async_queue(Name, Args) ->
    ppool_serv:async_queue(Name, Args).
