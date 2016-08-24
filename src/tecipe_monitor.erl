-module(tecipe_monitor).
-behaviour(gen_server).

-export([start_link/2, monitor_worker/3, get_workers/1, get_stats/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("tecipe.hrl").

-record(state, {workers}).

monitor_worker(MonitorName, Sock, WorkerPID) ->
    gen_server:cast(MonitorName, {monitor_worker, Sock, WorkerPID}).

get_workers(ListenerRef) ->
    {ok, MonitorName} = tecipe:make_monitor_name(ListenerRef),
    WorkersDict = gen_server:call(MonitorName, get_workers),
    {ok, dict:to_list(WorkersDict)}.

get_stats(ListenerRef) ->
    {ok, MonitorName} = tecipe:make_monitor_name(ListenerRef),
    Stats = gen_server:call(MonitorName, get_stats),
    {ok, dict:to_list(Stats)}.

start_link(Ref, ListenerRec) ->
    Name = ListenerRec#tecipe_listener.monitor_name,
    gen_server:start_link({local, Name}, ?MODULE, [Ref], []).

init([_Ref]) ->
    Workers = dict:new(),
    {ok, #state{workers = Workers}}.

handle_call(get_workers, _From, #state{workers = Workers} = State) ->
    {reply, Workers, State};

handle_call(get_stats, _From, #state{workers = Workers} = State) ->
    Stats = dict:map(fun(_MonitorRef, {Sock, _WorkerPID}) ->
			     {ok, {RemoteIP, RemotePort}} = inet:peername(Sock),
			     {ok, {LocalIP, LocalPort}} = inet:sockname(Sock),
			     #tecipe_socket_stats{
				remote_ip = RemoteIP,
				remote_port = RemotePort,
				local_ip = LocalIP,
				local_port = LocalPort}
		     end,
		     Workers),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({monitor_worker, Sock, WorkerPID}, #state{workers = Workers} = State) ->
    MonitorRef = monitor(process, WorkerPID),
    {noreply, State#state{workers = dict:store(MonitorRef, {Sock, WorkerPID}, Workers)}};

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
