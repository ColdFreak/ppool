-module(ppool_sup).
-export([start_link/3, init/1]).


%% ユーザー直接呼び出す必要がないから、名前をつけたくない
start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

%% ppool_servはワーカとして働く
%% `Name` is passed to the server,
%% along with `self()`, the supervisor's own pid.
%% This will let the server call for the spawning of the 
%% worker supervisor; the `MFA` variable will be used in that call
%% to let the `simple_one_for_one` supervisor know what kind of worker to run
%% スーパースーパーバイザー渡ってきた`Name`は直接使わずにppool_serv`に使ってもらう
init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    ChildSpec = [
                 {serv,
                  {ppool_serv, start_link, [Name, Limit, self(), MFA]},
                  permanent,
                  5000,
                  worker,
                  [ppool_serv]
                 }
                ],
    {ok, {{one_for_all, MaxRestart, MaxTime}, ChildSpec }}.
