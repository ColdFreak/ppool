-module(ppool_suppersup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    %% start_link(SupName, Module, Args)
    %% `Module` is the name of the callback module
    %% `Args` is the arbitrary term which is passed 
    %% as the argument to `Module:init/1`
    supervisor:start_link({local, ppool}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Module:init(Args) -> Result
    %% Result = {ok, {SupFlags, [ChildSpec]}} | ignore
    MaxRestart = 6,
    MaxTime = 3600,
    %% 最上位のスーパーバイザーの唯一のタスクは、メモリ上ニプールを保持
    %% して、それを監視することで、この場合には
    %% 子プロセスがいないスーパバイザになる。
    {ok, {
            {one_for_one, MaxRestart, MaxTime},
            []
         }
    }.
    % {ok, { {one_for_one, 5, 10}, []} }.

stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ ->
            ok
    end.


