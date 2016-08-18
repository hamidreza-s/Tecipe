-module(tecipe_listener_sup).
-behaviour(supervisor).

-export([start_link/4]).

-export([init/1]).

start_link(Name, Port, ListenerOpts, TransportOpts) ->
    supervisor:start_link({local, make_name(Name)}, ?MODULE, [Name, Port, ListenerOpts, TransportOpts]).

init([Name, Port, ListenerOpts, TransportOpts]) ->

    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, ListeningSock} = Transport:listen(Port, TransportOpts),

    PoolType = proplists:get_value(pool_type, ListenerOpts),

    case PoolType of

	acceptor ->

	    AcceptorManager = {
	      tecipe_acceptor_manager,
	      {tecipe_acceptor_manager, start_link, []},
	      permanent,
	      3000,
	      worker,
	      [tecipe_acceptor_manager]},

	    Acceptor = {
	      {tecipe_acceptor, Name},
	      {tecipe_acceptor, start_link, [Name, ListeningSock, ListenerOpts]},
	      permanent,
	      3000,
	      supervisor,
	      [tecipe_acceptor]},

	    {ok, {{one_for_one, 10, 1}, [AcceptorManager, Acceptor]}};

	worker ->


	    WorkerManager = {
	      tecipe_worker_manager,
	      {tecipe_worker_manager, start_link, []},
	      permanent,
	      3000,
	      worker,
	      [tecipe_worker_manager]},

	    Worker = {
	      {tecipe_worker, Name},
	      {tecipe_worker, start_link, [Name, ListeningSock, ListenerOpts]},
	      permanent,
	      3000,
	      worker,
	      [tecipe_worker]},

	    {ok, {{one_for_one, 10, 1}, [WorkerManager, Worker]}}

    end.

make_name(Name)
  when is_atom(Name) ->
    list_to_atom("tecipe_"
		 ++ atom_to_list(Name)
		 ++ "_listener").
