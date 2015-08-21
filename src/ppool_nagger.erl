-module(ppool_nagger).
-behavior(gen_server).
-export([start_link/4, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, code_change/3, terminate/2]).

start_link(Task, Delay, Max, SendTo) ->
    gen_server:start(?MODULE, {Task, Delay, Max, SendTo}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({Taks, Delay, Max, SendTo}) ->
    %% `Delay`はMillisecond単位で
    %% `Delay`時間以内にメッセージがないとtimeoutになって、
    %% handle_infoが`timeout`メッセージを処理する
    {ok, {Taks, Delay, Max, SendTo}, Delay}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, {Task, Delay, Max, SendTo}) ->
    SendTo ! {self(), Task},
    if 
        Max =:= infinity ->
            {noreply, {Task, Delay, Max, SendTo}, Delay};
        Max =< 1 ->
            {stop, normal, {Task, Delay, 0, SendTo}};
        Max > 1 ->
            %% if the function returns `{stop, Reason, Reply, }`
            {noreply, {Task, Delay, Max-1, SendTo}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.


