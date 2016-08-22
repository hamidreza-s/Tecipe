-module(tecipe_listener_sup).
-behaviour(supervisor).

-export([start_link/5]).

-export([init/1]).

start_link(Name, Port, Handler, ListenerOpts, TransportOpts) ->
    {ok, ListenerName} = tecipe:make_listener_name(Name),
    supervisor:start_link({local, ListenerName}, ?MODULE,
			  [Name, Port, Handler, ListenerOpts, TransportOpts]).

init([Name, Port, Handler, ListenerOpts, TransportOpts]) ->

    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, ListeningSock} = Transport:listen(Port, TransportOpts),

    AcceptorChild =
	case proplists:get_value(acceptor, ListenerOpts) of
	    static ->
		{{tecipe_acceptor_static, Name},
		 {tecipe_acceptor_static, start_link,
		  [Name, Handler, ListeningSock, ListenerOpts]},
		 permanent,
		 3000,
		 supervisor,
		 [tecipe_acceptor_static]};
	    dynamic ->
		{{tecipe_acceptor_dynamic, Name},
		 {tecipe_acceptor_dynamic, start_link,
		  [Name, Handler, ListeningSock, ListenerOpts]},
		 permanent,
		 3000,
		 worker,
		 [tecipe_acceptor_dynamic]}
	end,

    CollectorChild = {{tecipe_collector, Name},
		      {tecipe_collector, start_link, [Name]},
		      permanent,
		      3000,
		      worker,
		      [tecipe_collector]},

    {ok, {{one_for_one, 10, 1}, [CollectorChild, AcceptorChild]}}.
