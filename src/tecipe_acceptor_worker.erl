-module(tecipe_acceptor_worker).
-behaviour(gen_server).

-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {name, handler, transport, listening_sock}).

start_link(Name, Handler, ListeningSock, ListenerOpts) ->
    Pool = proplists:get_value(pool, ListenerOpts),
    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, AcceptorWorker} = gen_server:start_link(?MODULE,
						 [Name, Handler, Transport, ListeningSock], []),
    [{ok, _} = add_worker(AcceptorWorker) || _ <- lists:seq(1, Pool)],
    {ok, AcceptorWorker}.


add_worker(AcceptorWorker) ->
    gen_server:call(AcceptorWorker, add_worker).

worker_loop(AcceptorWorker, {Module, Function, Args} = _Handler, Transport, ListeningSock) ->
    {ok, Sock} = Transport:accept(ListeningSock),
    {ok, _} = add_worker(AcceptorWorker),
    unlink(AcceptorWorker),
    Module:Function(Transport, Sock, Args).


init([Name, Handler, Transport, ListeningSock]) ->
    process_flag(trap_exit, true),
    {ok, #state{name = Name, handler = Handler,
		transport = Transport, listening_sock = ListeningSock}}.

handle_call(add_worker, _From, State) ->
    AcceptorWorker = self(),
    Handler = State#state.handler,
    Transport = State#state.transport,
    ListeningSock = State#state.listening_sock,
    Pid = spawn_link(fun() ->
			     worker_loop(AcceptorWorker, Handler, Transport, ListeningSock)
		     end),
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
