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

start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
                 {ppool_sup, start_link,[Name, Limit, MFA]},
                 permanent,
                 105000,
                 supervisor,
                 [ppool_sup]
                },
    %% start_child(SupRef, ChildSepc) -> startchild_ret()
    %% Dynamically adds a child specification to the supervisor `SupRef`
    %% which starts the corresponding child process.
    supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
    %% If the chile is temporary , the child  specification is
    %% deleted as soon as the process terminates. This means that 
    %% `delete_child/2` has no meaning, and `restart_child/2` can not 
    %% be used for these children
    supervisor:terminate_child(ppool, Name),
    %% `delete_child/2` tells the supervisor to delete the child
    %% specification idenfified by `Name`. The corresponding child
    %% process must not be running. use `terminate_child/2` to terminate it.
    %% If successful, the function returns `ok`, If the child process is running
    %% or about to be restarted, the function returns `{error, running}` or 
    %% `{error, restarting}` respectively.
    supervisor:delete_child(ppool, Name).

