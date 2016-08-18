-module(tecipe_acceptor).
-behaviour(supervisor).

-export([start_link/3, start_acceptor/2]).

-export([init/1]).

start_link(Name, ListeningSock, ListenerOpts) ->
    PoolCount = proplists:get_value(pool_count, ListenerOpts),
    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, AcceptorSup} = supervisor:start_link(?MODULE, [Name, Transport, ListeningSock]),
    [{ok, _} = add_acceptor(AcceptorSup) || _ <- lists:seq(1, PoolCount)],
    {ok, AcceptorSup}.

add_acceptor(Pid) ->
    supervisor:start_child(Pid, []).

init([Name, Transport, ListeningSock]) ->
    Acceptor = {
      {tecipe_acceptor_loop, Name},
      {?MODULE, start_acceptor, [Transport, ListeningSock]},
      permanent,
      3000,
      worker,
      [?MODULE]},

    {ok, {{simple_one_for_one, 10, 1}, [Acceptor]}}.

start_acceptor(Transport, ListeningSock) ->
    Pid = spawn_link(fun() -> acceptor_loop(Transport, ListeningSock) end),
    {ok, Pid}.

acceptor_loop(Transport, ListeningSock) ->
    {ok, Sock} = Transport:accept(ListeningSock),
    Pid = spawn(fun() -> handle(Transport, Sock) end),
    gen_tcp:controlling_process(Sock, Pid),
    acceptor_loop(Transport, ListeningSock).

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
