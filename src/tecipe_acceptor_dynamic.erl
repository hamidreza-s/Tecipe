-module(tecipe_acceptor_dynamic).
-behaviour(gen_server).

-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("tecipe.hrl").

-record(state, {name, handler, transport, listening_sock, listener_rec}).

start_link(Ref, Handler, ListeningSock, ListenerRec) ->
    Pool = ListenerRec#tecipe_listener.acceptor_pool,
    Transport = ListenerRec#tecipe_listener.transport,
    Name = ListenerRec#tecipe_listener.acceptor_name,
    {ok, AcceptorWorker} = gen_server:start_link({local, Name}, ?MODULE,
						 [Ref, Handler, Transport, ListeningSock, ListenerRec], []),
    [ok = add_worker(AcceptorWorker) || _ <- lists:seq(1, Pool)],
    {ok, AcceptorWorker}.

add_worker(AcceptorWorker) ->
    gen_server:cast(AcceptorWorker, add_worker).

worker_loop(AcceptorWorker, Handler, Transport, ListeningSock, ListenerRec) ->
    {ok, Sock} = Transport:accept(ListeningSock),
    ok = add_worker(AcceptorWorker),
    unlink(AcceptorWorker),

    WorkerPID = self(),
    case ListenerRec#tecipe_listener.monitor of
	true ->
	    tecipe_monitor:monitor_worker(ListenerRec#tecipe_listener.monitor_name, Sock, WorkerPID);
	_ ->
	    ok
    end,

    case Handler of
	{Module, Function, Args} ->
	    apply(Module, Function, [Transport, Sock, Args]);
	Function ->
	    apply(Function, [Transport, Sock])
    end.

init([Ref, Handler, Transport, ListeningSock, ListenerRec]) ->
    process_flag(trap_exit, true),
    {ok, #state{name = Ref, handler = Handler,
		transport = Transport, listening_sock = ListeningSock,
		listener_rec = ListenerRec}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(add_worker, State) ->
    AcceptorWorker = self(),
    Handler = State#state.handler,
    Transport = State#state.transport,
    ListeningSock = State#state.listening_sock,
    ListenerRec = State#state.listener_rec,
    proc_lib:spawn_link(fun() ->
				worker_loop(AcceptorWorker, Handler, Transport, ListeningSock, ListenerRec)
			end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    AcceptorWorker = self(),
    ok = add_worker(AcceptorWorker),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
