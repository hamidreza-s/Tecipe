-module(tecipe).

-export([start/0, start_listener/3, start_listener/4, start_listener/5,
	 make_listener_lname/1, make_acceptor_lname/1, make_collector_lname/1,
	 stop_listener/1]).

-include("tecipe.hrl").

-type tecipe_listener_sname() :: atom().

-type tecipe_listener_lname() :: atom().

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

-type tecipe_listener_lnames() :: {tecipe_listener_lname(),
				   tecipe_listener_lname(),
				   tecipe_listener_lname()}.

start() ->
    application:start(?MODULE).

-spec start_listener(tecipe_listener_sname(),
		     tecipe_listener_port(),
		     tecipe_listener_handler()) -> {ok,
						    tecipe_listener_pid(),
						    tecipe_listener_lnames()}.

start_listener(SName, Port, Handler) ->
    start_listener(SName, Port, Handler, [], []).

-spec start_listener(tecipe_listener_sname(),
		     tecipe_listener_port(),
		     tecipe_listener_handler(),
		     tecipe_listener_opts()) -> {ok,
						 tecipe_listener_pid(),
						 tecipe_listener_lnames()}.

start_listener(SName, Port, Handler, ListenerOpts) ->
    start_listener(SName, Port, Handler, ListenerOpts, []).

-spec start_listener(tecipe_listener_sname(),
		     tecipe_listener_port(),
		     tecipe_listener_handler(),
		     tecipe_listener_opts(),
		     tecipe_transport_opts()) -> {ok,
						  tecipe_listener_pid(),
						  tecipe_listener_lnames()}.

start_listener(SName, Port, Handler, ListenerOpts, TransportOpts) ->
    Acceptor = proplists:get_value(acceptor, ListenerOpts, default_listener_acceptor()),
    Pool = proplists:get_value(pool, ListenerOpts, default_listener_pool_count()),
    Transport = proplists:get_value(transport, ListenerOpts, default_listener_transport()),
    NewListenerOpts = [{acceptor, Acceptor}, {pool, Pool}, {transport, Transport}],

    ListenerSup = {{tecipe_listener_sup, SName},
		   {tecipe_listener_sup, start_link,
		    [SName, Port, Handler, NewListenerOpts, TransportOpts]},
		   permanent,
		   3000,
		   supervisor,
		   [tecipe_listener_sup]},

    {ok, ListenerPID} = supervisor:start_child(tecipe_sup, ListenerSup),
    {ok, ListenerLName} = make_listener_lname(SName),
    {ok, AcceptorLName} = make_acceptor_lname(SName),
    {ok, CollectorLName} = make_collector_lname(SName),

    true = ets:insert(?LISTENER_TAB, {SName,
				      #tecipe_listener{name = SName,
						       listener_name = ListenerLName,
						       listener_pid = ListenerPID,
						       acceptor_name = AcceptorLName,
						       acceptor_pid = whereis(AcceptorLName),
						       collector_name = CollectorLName,
						       collector_pid = whereis(CollectorLName),
						       acceptor_type = Acceptor,
						       acceptor_pool = Pool,
						       transport = Transport,
						       handler = Handler}}),

    {ok, ListenerPID, {ListenerLName, AcceptorLName, CollectorLName}}.

-spec stop_listener(tecipe_listener_sname()) -> ok.
stop_listener(SName) ->
    supervisor:terminate_child(tecipe_sup, {tecipe_listener_sup, SName}).

-spec make_listener_lname(atom()) -> {ok, atom()}.
make_listener_lname(SName)
  when is_atom(SName) ->
    {ok, list_to_atom("tecipe_listener_" ++ atom_to_list(SName))}.

-spec make_acceptor_lname(atom()) -> {ok, atom()}.
make_acceptor_lname(SName)
  when is_atom(SName) ->
    {ok, list_to_atom("tecipe_acceptor_" ++ atom_to_list(SName))}.

-spec make_collector_lname(atom()) -> {ok, atom()}.
make_collector_lname(SName)
  when is_atom(SName) ->
    {ok, list_to_atom("tecipe_collector_" ++ atom_to_list(SName))}.

default_listener_acceptor() ->
    static.

default_listener_pool_count() ->
    5.

default_listener_transport() ->
    tecipe_tcp.
