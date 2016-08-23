-module(tecipe).

-export([start/0, start_listener/3, start_listener/4, start_listener/5,
	 make_listener_lname/1, make_acceptor_lname/1, make_monitor_lname/1,
	 get_listener/1, stop_listener/1]).

-include("tecipe.hrl").

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
    {ok, MonitorLName} = make_monitor_lname(SName),

    true = ets:insert(?LISTENER_TAB, {SName,
				      #tecipe_listener{name = SName,
						       listener_name = ListenerLName,
						       listener_pid = ListenerPID,
						       acceptor_name = AcceptorLName,
						       acceptor_pid = whereis(AcceptorLName),
						       monitor_name = MonitorLName,
						       monitor_pid = whereis(MonitorLName),
						       acceptor_type = Acceptor,
						       acceptor_pool = Pool,
						       transport = Transport,
						       handler = Handler}}),

    {ok, ListenerPID}.

-spec get_listener(tecipe_listener_sname()) -> {ok, tecipe_listener()} | {error, not_found}.
get_listener(SName) ->
    case ets:lookup(?LISTENER_TAB, SName) of
	[{SName, Listener}] ->
	    {ok, Listener};
	_ ->
	    {error, not_found}
    end.


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

-spec make_monitor_lname(atom()) -> {ok, atom()}.
make_monitor_lname(SName)
  when is_atom(SName) ->
    {ok, list_to_atom("tecipe_monitor_" ++ atom_to_list(SName))}.

default_listener_acceptor() ->
    static.

default_listener_pool_count() ->
    5.

default_listener_transport() ->
    tecipe_tcp.
