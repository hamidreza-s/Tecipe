-module(tecipe_monitor).
-behaviour(gen_server).

-export([start_link/2, monitor_worker/3, get_workers/1, get_stats/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("tecipe.hrl").

-record(state, {ref, listener_rec, workers}).

monitor_worker(MonitorName, TecipeSock, WorkerPID) ->
    gen_server:cast(MonitorName, {monitor_worker, TecipeSock, WorkerPID}).

get_workers(MonitorName) ->
    WorkersDict = gen_server:call(MonitorName, get_workers),
    {ok, dict:to_list(WorkersDict)}.

get_stats(MonitorName) ->
    Stats = gen_server:call(MonitorName, get_stats),
    {ok, dict:to_list(Stats)}.

start_link(Ref, ListenerRec) ->
    Name = ListenerRec#tecipe_listener.monitor_name,
    gen_server:start_link({local, Name}, ?MODULE, [Ref, ListenerRec], []).

init([Ref, ListenerRec]) ->
    Workers = dict:new(),
    {ok, #state{ref = Ref, listener_rec = ListenerRec, workers = Workers}}.

handle_call(get_workers, _From, #state{workers = Workers} = State) ->
    {reply, Workers, State};

handle_call(get_stats, _From, #state{workers = Workers} = State) ->
    ListenerRec = State#state.listener_rec,
    Transport = ListenerRec#tecipe_listener.transport,
    Stats = dict:map(fun(_MonitorRef, {TecipeSock, WorkerPID}) ->

			     {ok, {RemoteIP, RemotePort}} = Transport:peername(TecipeSock),
			     {ok, {LocalIP, LocalPort}} = Transport:sockname(TecipeSock),
			     {ok, SockStats} = Transport:getstat(TecipeSock),

			     #tecipe_socket_stats{
				worker_pid = WorkerPID,
				socket_port = TecipeSock#tecipe_socket.inet_socket,
				remote_ip = RemoteIP,
				remote_port = RemotePort,
				local_ip = LocalIP,
				local_port = LocalPort,
				proxy = has_proxy(TecipeSock),
				recv_cnt = proplists:get_value(recv_cnt, SockStats),
				recv_max = proplists:get_value(recv_max, SockStats),
				recv_avg = proplists:get_value(recv_avg, SockStats),
				recv_oct = proplists:get_value(recv_oct, SockStats),
				recv_dvi = proplists:get_value(recv_dvi, SockStats),
				send_cnt = proplists:get_value(send_cnt, SockStats),
				send_max = proplists:get_value(send_max, SockStats),
				send_avg = proplists:get_value(send_avg, SockStats),
				send_oct = proplists:get_value(send_oct, SockStats),
				send_pend = proplists:get_value(send_pend, SockStats)}
		     end,
		     Workers),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({monitor_worker, TecipeSock, WorkerPID}, #state{workers = Workers} = State) ->
    MonitorRef = monitor(process, WorkerPID),
    {noreply, State#state{workers = dict:store(MonitorRef, {TecipeSock, WorkerPID}, Workers)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, WorkerPID, Reason}, #state{workers = Workers} = State) ->
    NewWorkers = dict:erase(MonitorRef, Workers),
    error_logger:info_msg("tecipe worker ~p is down: ~p~n", [WorkerPID, Reason]),
    {noreply, State#state{workers = NewWorkers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% === private functions

has_proxy(#tecipe_socket{proxy = undefined}) ->
    false;
has_proxy(#tecipe_socket{proxy = Proxy}) ->
    Proxy#tecipe_proxy.proxy_version.
