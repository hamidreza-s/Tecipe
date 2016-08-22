-module(tecipe).

-export([start/0, start_listener/3, start_listener/4, start_listener/5,
	 make_listener_name/1, make_acceptor_name/1, make_collector_name/1]).

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

-type tecipe_listener_pid() :: pid().

-type tecipe_listener_names() :: {atom(), atom(), atom()}.

start() ->
    application:start(?MODULE).

-spec start_listener(tecipe_listener_name(),
		     tecipe_listener_port(),
		     tecipe_listener_handler()) -> {ok,
						    tecipe_listener_pid(),
						    tecipe_listener_names()}.

start_listener(Name, Port, Handler) ->
    start_listener(Name, Port, Handler, [], []).

-spec start_listener(tecipe_listener_name(),
		     tecipe_listener_port(),
		     tecipe_listener_handler(),
		     tecipe_listener_opts()) -> {ok,
						 tecipe_listener_pid(),
						 tecipe_listener_names()}.

start_listener(Name, Port, Handler, ListenerOpts) ->
    start_listener(Name, Port, Handler, ListenerOpts, []).

-spec start_listener(tecipe_listener_name(),
		     tecipe_listener_port(),
		     tecipe_listener_handler(),
		     tecipe_listener_opts(),
		     tecipe_transport_opts()) -> {ok,
						  tecipe_listener_pid(),
						  tecipe_listener_names()}.

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

    {ok, ListenerPID} = supervisor:start_child(tecipe_sup, ListenerSup),
    {ok, ListenerName} = make_listener_name(Name),
    {ok, AcceptorName} = make_acceptor_name(Name),
    {ok, CollectorName} = make_collector_name(Name),
    {ok, ListenerPID, {ListenerName, AcceptorName, CollectorName}}.

-spec make_listener_name(atom()) -> {ok, atom()}.
make_listener_name(Name)
  when is_atom(Name) ->
    {ok, list_to_atom("tecipe_listener_" ++ atom_to_list(Name))}.

-spec make_acceptor_name(atom()) -> {ok, atom()}.
make_acceptor_name(Name)
  when is_atom(Name) ->
    {ok, list_to_atom("tecipe_acceptor_" ++ atom_to_list(Name))}.

-spec make_collector_name(atom()) -> {ok, atom()}.
make_collector_name(Name)
  when is_atom(Name) ->
    {ok, list_to_atom("tecipe_collector_" ++ atom_to_list(Name))}.

default_listener_acceptor() ->
    static.

default_listener_pool_count() ->
    5.

default_listener_transport() ->
    tecipe_tcp.
