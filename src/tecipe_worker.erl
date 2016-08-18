-module(tecipe_worker).
-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

start_link(Name, ListeningSock, ListenerOpts) ->
    PoolCount = proplists:get_value(pool_count, ListenerOpts),
    Transport = proplists:get_value(transport, ListenerOpts),
    gen_server:start_link(?MODULE, [Name, ListeningSock, ListenerOpts], []).

add_worker(WorkerSup, Transport, ListeningSock) ->
    Pid = spawn_link(fun() -> worker_loop(WorkerSup, Transport, ListeningSock) end),
    {ok, Pid}.

worker_loop(WorkerSup, Transport, ListeningSock) ->
    {ok, Sock} = Transport:accept(ListeningSock),
    add_worker(WorkerSup, Transport, ListeningSock),
    handle(Transport, Sock).

init([_Name, _ListeningSock, _ListeningOpts]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    %% todo: spawn a new worker
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle(Transport, Sock) ->
    receive
	{tcp, Sock, Data} ->
	    Transport:send(Sock, Data),
	    handle(Transport, Sock);
	{tcp_error, Sock, _Reason} ->
	    Transport:close(Sock);
	{tcp_close, Sock} ->
	    Transport:close(Sock);
	_ ->
	    Transport:close(Sock)
    end.

%% start_acceptor(ListeningSock) ->
%%     Sup = self(),
%%     io:format("listener: ~p~n", [self()]),
%%     AcceptorFun = fun() ->
%% 			  {ok, AcceptedSock} = gen_tcp:accept(ListeningSock),
%% 			  add_listener(),
%% 			  io:format("a new listener was added!~n", []),
%% 			  unlink(Sup),
%% 			  handle(AcceptedSock)
%% 		  end,
%%     AcceptorPID = spawn_link(AcceptorFun),
%%     {ok, AcceptorPID}.
