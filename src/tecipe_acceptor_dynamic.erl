-module(tecipe_acceptor_dynamic).
-behaviour(gen_server).

-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {name, handler, transport, listening_sock}).

start_link(SName, Handler, ListeningSock, ListenerOpts) ->
    Pool = proplists:get_value(pool, ListenerOpts),
    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, LName} = tecipe:make_acceptor_lname(SName),
    {ok, AcceptorWorker} = gen_server:start_link({local, LName}, ?MODULE,
						 [SName, Handler, Transport, ListeningSock], []),
    [ok = add_worker(AcceptorWorker) || _ <- lists:seq(1, Pool)],
    {ok, AcceptorWorker}.


add_worker(AcceptorWorker) ->
    gen_server:cast(AcceptorWorker, add_worker).

worker_loop(AcceptorWorker, Handler, Transport, ListeningSock) ->
    {ok, Sock} = Transport:accept(ListeningSock),
    ok = add_worker(AcceptorWorker),
    unlink(AcceptorWorker),

    case Handler of
	{Module, Function, Args} ->
	    apply(Module, Function, [Transport, Sock, Args]);
	Function ->
	    apply(Function, [Transport, Sock])
    end.

init([SName, Handler, Transport, ListeningSock]) ->
    process_flag(trap_exit, true),
    {ok, #state{name = SName, handler = Handler,
		transport = Transport, listening_sock = ListeningSock}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(add_worker, State) ->
    AcceptorWorker = self(),
    Handler = State#state.handler,
    Transport = State#state.transport,
    ListeningSock = State#state.listening_sock,
    spawn_link(fun() -> worker_loop(AcceptorWorker, Handler, Transport, ListeningSock) end),
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
