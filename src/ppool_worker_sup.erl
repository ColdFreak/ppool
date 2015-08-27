-module(ppool_worker_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).

start_link(MFA={_, _, _}) ->
    supervisor:start_link(?MODULE, MFA).

%% we use an `{M, F, A}` tuple to start the worker
%% we can use any kind of OTP behaviour there
%% すべてのワーカーは一時的なもので、ワーカーの起動に{M, F, A}タプルを使っている
init({M, F, A}) ->
    MaxRestart  = 5,
    MaxTime     = 3600,
    ChildSpec = [{ppool_worker, %% 子のID
                  {M, F, A},
                  temporary, 
                  5000,
                  worker,
                  [M]
                 }],

    %% simple_one_for_oneを使うと高速で子を追加できる
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},ChildSpec}}.
