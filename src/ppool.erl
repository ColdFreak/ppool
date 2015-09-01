%% @author Wang Zhijun
%% @doc ユーザーのためのAPI
-module(ppool).
-behaviour(application).
% -export([start_link/0, stop/0, start_pool/3,
%          run/2, sync_queue/2, async_queue/2, stop_pool/1]).

-export([start/2, stop/1, start_pool/3,
         run/2, sync_queue/2, async_queue/2, stop_pool/1]).

%% @private 
%% @doc private notationで指定するとedocに{private, true}を指定しない限りドキュメントからなくなる
% start_link() ->
%     ppool_supersup:start_link().

%% 
%% 1> application:start(ppool).
%% ok
%% 4> ppool:start_pool(nag, 2, {ppool_nagger, start_link, []}).
%% {ok,<0.43.0>}
%% 5> ppool:run(nag, [make_ref(), 500, 10, self()]).
%% {ok,<0.47.0>}
%% 6> received down message
%% 6> ppool:run(nag, [make_ref(), 500, 10, self()]).
%% {ok,<0.49.0>}
%% 7> received down message
%% 8> self().
%% <0.41.0>
%% 9> ppool:run(nag, [make_ref(), 500, 10, self()]).
%% {ok,<0.53.0>}
%% 10> ppool:run(nag, [make_ref(), 500, 10, self()]).
%% {ok,<0.55.0>}
%% 11> ppool:run(nag, [make_ref(), 500, 10, self()]).
%% noalloc
%% 16> application:which_applications().
%% [{ppool,[],"1.0.1"},
%%  {stdlib,"ERTS  CXC 138 10","2.4"},
%%  {kernel,"ERTS  CXC 138 10","3.2"}]

%% 17> application:stop(ppool).
%% ok
%% 18>
%% =INFO REPORT==== 2-Sep-2015::08:32:44 ===
%%     application: ppool
%%     exited: stopped
%%     type: temporary
start(normal, _Args) ->
    ppool_supersup:start_link().

% stop() ->
%     ppool_supersup:stop().

stop(_State) ->
    ok.

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
%% 下のような立ち上げ方しています
%% 1> ppool:start_link().
%% {ok,<0.34.0>}
%% 2> ppool:start_pool(nagger, 2, {ppool_nagger, start_link, []}).
%% {ok,<0.36.0>}
%% 3> ppool:run(nagger, ["finish the chapter!", 5000, 2, self()]).
%% {ok,<0.40.0>}
run(Name, Args) ->
    ppool_serv:run(Name, Args).

%% @todo 実装をチェック
sync_queue(Name, Args) ->
    ppool_serv:sync_queue(Name, Args).

%% @todo 実装をチェック
async_queue(Name, Args) ->
    ppool_serv:async_queue(Name, Args).
