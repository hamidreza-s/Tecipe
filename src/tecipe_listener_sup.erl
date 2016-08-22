-module(tecipe_listener_sup).
-behaviour(supervisor).

-export([start_link/5]).

-export([init/1]).

start_link(Name, Port, Handler, ListenerOpts, TransportOpts) ->
    supervisor:start_link({local, make_name(Name)}, ?MODULE,
			  [Name, Port, Handler, ListenerOpts, TransportOpts]).

init([Name, Port, Handler, ListenerOpts, TransportOpts]) ->

    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, ListeningSock} = Transport:listen(Port, TransportOpts),

    AcceptorChild =
	case proplists:get_value(acceptor, ListenerOpts) of
	    supervisor ->
		{{tecipe_acceptor_sup, Name},
		 {tecipe_acceptor_sup, start_link,
		  [Name, Handler, ListeningSock, ListenerOpts]},
		 permanent,
		 3000,
		 supervisor,
		 [tecipe_acceptor_sup]};
	    worker ->
		{{tecipe_acceptor_worker, Name},
		 {tecipe_acceptor_worker, start_link,
		  [Name, Handler, ListeningSock, ListenerOpts]},
		 permanent,
		 3000,
		 worker,
		 [tecipe_acceptor_worker]}
	end,

    AcceptorManagerChild = {{tecipe_acceptor_manager, Name},
			    {tecipe_acceptor_manager, start_link, [Name]},
			    permanent,
			    3000,
			    worker,
			    [tecipe_acceptor_manager]},

    {ok, {{one_for_one, 10, 1}, [AcceptorManagerChild, AcceptorChild]}}.

make_name(Name)
  when is_atom(Name) ->
    list_to_atom("tecipe_listener_" ++ atom_to_list(Name)).
