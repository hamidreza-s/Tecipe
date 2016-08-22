-module(tecipe).

-export([start/0, start_listener/3, start_listener/4, start_listener/5]).


-type tecipe_listener_name() :: atom().

-type tecipe_listener_port() :: integer().

-type tecipe_listener_handler_mfa() :: {module(), atom(), list()}.

-type tecipe_listener_handler_fun() :: fun((tecipe_listener_transport(),
					    inet:socket()) -> no_return()).

-type tecipe_listener_handler() :: tecipe_listener_handler_mfa() |
				   tecipe_listener_handler_fun().

-type tecipe_listener_transport() :: tecipe_tcp | tecipe_ssl.

-type tecipe_listener_opts() :: list(tecipe_listener_opt()).

-type tecipe_listener_opt() :: {transport, tecipe_listener_transport()} |
			       {acceptor, static | dynamic} | {pool, integer()}.

-type tecipe_transport_opts() :: gen_tcp:option() | ssl:options() | sctp:option().

start() ->
    application:start(?MODULE).

-spec start_listener(tecipe_listener_name(),
		     tecipe_listener_port(),
		     tecipe_listener_handler()) -> {ok, pid()}.

start_listener(Name, Port, Handler) ->
    start_listener(Name, Port, Handler, [], []).

-spec start_listener(tecipe_listener_name(),
		     tecipe_listener_port(),
		     tecipe_listener_handler(),
		     tecipe_listener_opts()) -> {ok, pid()}.

start_listener(Name, Port, Handler, ListenerOpts) ->
    start_listener(Name, Port, Handler, ListenerOpts, []).

-spec start_listener(tecipe_listener_name(),
		     tecipe_listener_port(),
		     tecipe_listener_handler(),
		     tecipe_listener_opts(),
		     tecipe_transport_opts()) -> {ok, pid()}.

start_listener(Name, Port, Handler, ListenerOpts, TransportOpts) ->
    Acceptor = proplists:get_value(acceptor, ListenerOpts, default_listener_acceptor()),
    Pool = proplists:get_value(pool, ListenerOpts, default_listener_pool_count()),
    Transport = proplists:get_value(transport, ListenerOpts, default_listener_transport()),
    NewListenerOpts = [{acceptor, Acceptor}, {pool, Pool}, {transport, Transport}],

    ListenerSup = {{tecipe_listener_sup, Name},
		   {tecipe_listener_sup, start_link,
		    [Name, Port, Handler, NewListenerOpts, TransportOpts]},
		   permanent,
		   3000,
		   supervisor,
		   [tecipe_listener_sup]},

    supervisor:start_child(tecipe_sup, ListenerSup).

default_listener_acceptor() ->
    static.

default_listener_pool_count() ->
    5.

default_listener_transport() ->
    tecipe_tcp.
