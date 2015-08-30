%% あとでppool_servのテストかならずやる
%% ppool_servプロセスはworker_supプロセスに連絡が取れるべき
%% 同じスーパバイザーによって同時に起動されるようにしたいと思っている場合は
%% supervisor:whichl_children/1を使って策略を練るも可能だけど、使わない
-module(ppool_serv).
-behaviour(gen_server).
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-record(state, {limit=0,
                sup, %% ワーカースーパーバイザーをppool_supの下に起動するようにppool_supのPidをもらってくる
                refs, %% gb_sets:empty()が最初に入る
                queue=queue:new()}).

-define(SPEC(MFA), 
         {worker_sup,
          {ppool_worker_sup, start_link, [MFA]},
          temporary,
          10000,
          supervisor,
          [ppool_worker_sup]}).
          

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

%% ppool_supから起動されると、ppool_servがワーカースーパバイザーを起動する
%% `ppool_sup`から渡ってきたNameで自身に名前をつける
start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

init({Limit, MFA, Sup}) ->
    self() ! {start_worker_supervisor, Sup, MFA},
    %% ppool_servがワーカー達のステータス持っていることに注目
    %% ワーカースーパバイザーもっているのではなく
    %% `refs`はいまどんあワーカー起動しているかがgb_setsで管理している
    {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

run(Name, Args) ->
    %% call(ServerRef, Request) -> Reply
    %% `call`の二個目の引数はrequestである
    gen_server:call(Name, {run, Args}).


sync_queue(Name, Args) -> 
    %% `infinity`指定している意味は`reply`を無限時間に待つ
    %% syncとasyncの区別は大いにここにある
    %% 他の動作はほぼ一緒、まつか待たないかの区別
    gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{limit=_L, sup=_Sup, refs=Refs}) ->
    io:format("received down message~n"),
    case gb_sets:is_element(Ref, Refs) of
        true ->
            handle_down_worker(Ref, S);
        false -> %% 私たちに関係のないプロセス
            {noreply, S}
    end;
% `start_worker_supervisor`は一番最初に届くメッセージ
handle_info({start_worker_supervisor, Sup, MFA}, S=#state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    %% `ppool_serv`からワーカースーパバイザーを起動した後にlinkしないといけない
    link(Pid),
    {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

%% 直接ワーカースーパバイザーがワーカーを起動するのではなく、
%% `ppool_serv`からワーカースーパバイザーの下にワーカーを起動している状況
%% 下の`Args`は`{Task, Delay, Max, SendTo}`のようなメッセージ
%% `run`の意味はリソースがないと起動失敗させる
handle_call({run, Args}, _From, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    %% 起動されたワーカーのRefを取得して、gb_setsに入れる
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}};

%% N個以上のプロセスが起動できないようにしている
handle_call({run, Args}, _From, S=#state{limit=N}) when N =< 0 ->
    {reply, noalloc, S};

%% @doc `sync`の意味はワーカーが起動されるまで待つ
%% 1> ppool:start_link().
%% {ok,<0.34.0>}
%% 2> ppool:start_pool(nagger, 2, {ppool_nagger, start_link, []}).
%% {ok,<0.36.0>}
%%
%% 一つのメッセージを処理したらdown messageがくる
%% 3> ppool:sync_queue(nagger, ["Pet a dog", 3000, 1, self()]).
%% {ok,<0.40.0>}
%% received down message
%%
%% 二つのメッセージを処理した後にdown messageがくる
%% 4> ppool:sync_queue(nagger, ["Pet a dog", 3000, 2, self()]). 
%% {ok,<0.42.0>}
%% received down message
handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    %% An alternative to links are monitors. 
    %% A process Pid1 can create a monitor for Pid2 by calling the BIF
    %% erlang:monitor(process, Pid2). The function returns a reference Ref.
    %% If Pid2 terminates with exit reason Reason, a 'DOWN' message is sent to Pid1:
    %% {'DOWN', Ref, process, Pid2, Reason}
    %% minotorするとワーカー終了するときにdown messageが帰ってきて、監視できる
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}};
handle_call({sync, Args}, From, S = #state{queue=Q}) ->
    %% ワーカ死ぬときにメッセージを`ppool_serv`に送っている
    %% そして、`ppool_serv`はそのときにqueueにあるワーカーを起動する
    {noreply, S#state{queue=queue:in({From, Args}, Q)}};

handle_call(stop, _From, State) ->
    %% stopの場合は`{stop,Reason,Reply,NewState}`の戻り値になっている
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.


%% またワーカー起動できる( N > 0)の場合
handle_cast({async, Args}, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid}   = supervisor:start_child(Sup, Args),
    Ref         = erlang:monitor(process, Pid),
    %% linkがないことに注目
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}};

handle_cast({async, Args}, S = #state{limit=N, queue=Q}) when N =< 0 ->
    {noreply, S#state{queue=queue:in(Args, Q)}};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% 別のワーカーが死んで、キューに詰め込まれたワーカーを取り出す
handle_down_worker(Ref, S = #state{limit=L, sup=Sup, refs=Refs}) ->
    case queue:out(S#state.queue) of
        {{value, {From, Args}}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            %% 新しく作成したプロセスにRefをつける
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),

            %% reply(Client, Reply) -> Result
            %% This function can be used by a gen_server 
            %% to explicitly send a reply to a client that called `call/2,3` or
            %% `multi_call/2,3,4`, when the reply 
            %% cannot be defined in the return value of Module:handle_call/3.
            %% 確かに`{ok, Pid}`は別のhandle_callから返している
            gen_server:reply(From, {ok, Pid}),
            {noreply, S#state{refs=NewRefs, queue=Q}};
        {{value, Args}, Q} ->
            {ok, Pid}   = supervisor:start_child(Sup, Args),
            NewRef      = erlang:monitor(process, Pid),
            NewRefs     = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            %% ここにgen_server:replyが呼ばれていない、asyncだから
            {noreply, S#state{refs=NewRefs, queue=Q}};
        {empty, _} ->
            %% なんで `L+1`？そもそもlimitは何？
            {noreply, S#state{limit=L+1, refs=gb_sets:delete(Ref, Refs)}}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
    
