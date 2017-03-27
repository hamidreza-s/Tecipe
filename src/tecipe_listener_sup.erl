-module(tecipe_listener_sup).
-behaviour(supervisor).

-export([start_link/4]).

-export([init/1]).

-include("tecipe.hrl").

start_link(Ref, Port, Handler, ListenerRec) ->
    Name = ListenerRec#tecipe_listener.listener_name,
    supervisor:start_link({local, Name}, ?MODULE, [Ref, Port, Handler, ListenerRec]).

init([Ref, Port, Handler, ListenerRec]) ->

    Transport = ListenerRec#tecipe_listener.transport,
    TransportInitOpts = ListenerRec#tecipe_listener.transport_init_opts,
    TransportUserOpts = ListenerRec#tecipe_listener.transport_user_opts,
    {ok, ListeningSock} = Transport:listen(Port, TransportInitOpts ++ TransportUserOpts),

    AcceptorChild =
	case ListenerRec#tecipe_listener.acceptor_type of
	    static ->
		{{tecipe_acceptor_static, Ref},
		 {tecipe_acceptor_static, start_link,
		  [Ref, Handler, ListeningSock, ListenerRec]},
		 permanent,
		 3000,
		 supervisor,
		 [tecipe_acceptor_static]};
	    dynamic ->
		{{tecipe_acceptor_dynamic, Ref},
		 {tecipe_acceptor_dynamic, start_link,
		  [Ref, Handler, ListeningSock, ListenerRec]},
		 permanent,
		 3000,
		 worker,
		 [tecipe_acceptor_dynamic]}
	end,

    MonitorChild = {{tecipe_monitor, Ref},
		    {tecipe_monitor, start_link, [Ref, ListenerRec]},
		    permanent,
		    3000,
		    worker,
		    [tecipe_monitor]},

    {ok, {{one_for_all, 10, 1}, [MonitorChild, AcceptorChild]}}.
