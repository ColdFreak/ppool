-module(ppool_serv).
-behaviour(gen_server).
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
-export([init/1]).

-record(state, {limit=0,
                sup,
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
    gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).

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

%% `sync`の意味はワーカーが起動されるまで待つ
handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
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


