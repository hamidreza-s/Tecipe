-module(tecipe_listener_sup).
-behaviour(supervisor).

-export([start_link/5]).

-export([init/1]).

start_link(SName, Port, Handler, ListenerOpts, TransportOpts) ->
    {ok, LName} = tecipe:make_listener_lname(SName),
    supervisor:start_link({local, LName}, ?MODULE,
			  [SName, Port, Handler, ListenerOpts, TransportOpts]).

init([SName, Port, Handler, ListenerOpts, TransportOpts]) ->

    Transport = proplists:get_value(transport, ListenerOpts),
    {ok, ListeningSock} = Transport:listen(Port, TransportOpts),

    AcceptorChild =
	case proplists:get_value(acceptor, ListenerOpts) of
	    static ->
		{{tecipe_acceptor_static, SName},
		 {tecipe_acceptor_static, start_link,
		  [SName, Handler, ListeningSock, ListenerOpts]},
		 permanent,
		 3000,
		 supervisor,
		 [tecipe_acceptor_static]};
	    dynamic ->
		{{tecipe_acceptor_dynamic, SName},
		 {tecipe_acceptor_dynamic, start_link,
		  [SName, Handler, ListeningSock, ListenerOpts]},
		 permanent,
		 3000,
		 worker,
		 [tecipe_acceptor_dynamic]}
	end,

    CollectorChild = {{tecipe_collector, SName},
		      {tecipe_collector, start_link, [SName]},
		      permanent,
		      3000,
		      worker,
		      [tecipe_collector]},

    {ok, {{one_for_all, 10, 1}, [CollectorChild, AcceptorChild]}}.
