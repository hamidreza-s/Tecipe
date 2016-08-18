-module(tecipe_acceptor_worker).
-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {name, transport, listening_sock}).

start_link(Name, ListeningSock, ListenerOpts) ->
    Pool = proplists:get_value(pool, ListenerOpts),
    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, AcceptorWorker} = gen_server:start_link(?MODULE, [Name, Transport, ListeningSock], []),
    [{ok, _} = add_worker(AcceptorWorker) || _ <- lists:seq(1, Pool)],
    {ok, AcceptorWorker}.


add_worker(AcceptorWorker) ->
    gen_server:call(AcceptorWorker, add_worker).

worker_loop(AcceptorWorker, Transport, ListeningSock) ->
    {ok, Sock} = Transport:accept(ListeningSock),
    {ok, _} = add_worker(AcceptorWorker),
    unlink(AcceptorWorker),
    handle(Transport, Sock).

init([Name, Transport, ListeningSock]) ->
    process_flag(trap_exit, true),
    {ok, #state{name = Name, transport = Transport, listening_sock = ListeningSock}}.

handle_call(add_worker, _From, State) ->
    AcceptorWorker = self(),
    Transport = State#state.transport,
    ListeningSock = State#state.listening_sock,
    Pid = spawn_link(fun() -> worker_loop(AcceptorWorker, Transport, ListeningSock) end),
    Reply = {ok, Pid},
    {reply, Reply, State};

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
