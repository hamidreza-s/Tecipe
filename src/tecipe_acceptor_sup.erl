-module(tecipe_acceptor_sup).
-behaviour(supervisor).

-export([start_link/4, start_acceptor/3]).

-export([init/1]).

start_link(Name, Handler, ListeningSock, ListenerOpts) ->
    Pool = proplists:get_value(pool, ListenerOpts),
    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, AcceptorSup} = supervisor:start_link(?MODULE,
					      [Name, Handler, Transport, ListeningSock]),
    [{ok, _} = add_acceptor(AcceptorSup) || _ <- lists:seq(1, Pool)],
    {ok, AcceptorSup}.

add_acceptor(Pid) ->
    supervisor:start_child(Pid, []).

init([Name, Handler, Transport, ListeningSock]) ->
    Acceptor = {{tecipe_acceptor_loop, Name},
		{?MODULE, start_acceptor, [Handler, Transport, ListeningSock]},
		permanent,
		3000,
		worker,
		[?MODULE]},

    {ok, {{simple_one_for_one, 10, 1}, [Acceptor]}}.

start_acceptor(Handler, Transport, ListeningSock) ->
    Pid = spawn_link(fun() -> acceptor_loop(Handler, Transport, ListeningSock) end),
    {ok, Pid}.

acceptor_loop({Module, Function, Args} = Handler, Transport, ListeningSock) ->
    {ok, Sock} = Transport:accept(ListeningSock),
    Pid = spawn(fun() -> Module:Function(Transport, Sock, Args) end),
    gen_tcp:controlling_process(Sock, Pid),
    acceptor_loop(Handler, Transport, ListeningSock).
