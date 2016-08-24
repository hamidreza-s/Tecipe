-module(tecipe).

-export([start/0, start_listener/3, start_listener/4, start_listener/5,
	 make_listener_name/1, make_acceptor_name/1, make_monitor_name/1,
	 get_listener/1, stop_listener/1]).

-include("tecipe.hrl").

start() ->
    application:start(?MODULE).

-spec start_listener(tecipe_listener_ref(),
		     tecipe_listener_port(),
		     tecipe_listener_handler()) -> {ok, tecipe_listener_pid()}.

start_listener(Ref, Port, Handler) ->
    start_listener(Ref, Port, Handler, [], []).

-spec start_listener(tecipe_listener_ref(),
		     tecipe_listener_port(),
		     tecipe_listener_handler(),
		     tecipe_listener_opts()) -> {ok, tecipe_listener_pid()}.

start_listener(Ref, Port, Handler, ListenerOpts) ->
    start_listener(Ref, Port, Handler, ListenerOpts, []).

-spec start_listener(tecipe_listener_ref(),
		     tecipe_listener_port(),
		     tecipe_listener_handler(),
		     tecipe_listener_opts(),
		     tecipe_transport_opts()) -> {ok, tecipe_listener_pid()}.

start_listener(Ref, Port, Handler, ListenerOpts, TransportOpts) ->
    Acceptor = proplists:get_value(acceptor, ListenerOpts, default_listener_acceptor()),
    Pool = proplists:get_value(pool, ListenerOpts, default_listener_pool_count()),
    Transport = proplists:get_value(transport, ListenerOpts, default_listener_transport()),
    Monitor = proplists:get_value(monitor, ListenerOpts, default_listener_monitor()),

    {ok, ListenerName} = make_listener_name(Ref),
    {ok, AcceptorName} = make_acceptor_name(Ref),
    {ok, MonitorName} = make_monitor_name(Ref),

    ListenerRec = #tecipe_listener{ref = Ref,
				   listener_name = ListenerName,
				   acceptor_name = AcceptorName,
				   monitor_name = MonitorName,
				   acceptor_type = Acceptor,
				   acceptor_pool = Pool,
				   transport = Transport,
				   handler = Handler,
				   monitor = Monitor},

    ListenerSup = {{tecipe_listener_sup, Ref},
		   {tecipe_listener_sup, start_link,
		    [Ref, Port, Handler, ListenerRec, TransportOpts]},
		   permanent,
		   3000,
		   supervisor,
		   [tecipe_listener_sup]},

    {ok, ListenerPID} = supervisor:start_child(tecipe_sup, ListenerSup),

    true = ets:insert(?LISTENER_TAB, {Ref, ListenerRec#tecipe_listener{
					     listener_pid = ListenerPID,
					     acceptor_pid = whereis(AcceptorName),
					     monitor_pid = whereis(MonitorName)}}),

    {ok, ListenerPID}.

-spec get_listener(tecipe_listener_ref()) -> {ok, tecipe_listener()} | {error, not_found}.
get_listener(Ref) ->
    case ets:lookup(?LISTENER_TAB, Ref) of
	[{Ref, Listener}] ->
	    {ok, Listener};
	_ ->
	    {error, not_found}
    end.

-spec stop_listener(tecipe_listener_ref()) -> ok.
stop_listener(Ref) ->
    supervisor:terminate_child(tecipe_sup, {tecipe_listener_sup, Ref}).

-spec make_listener_name(tecipe_listener_ref()) -> {ok, tecipe_listener_name()}.
make_listener_name(Ref)
  when is_atom(Ref) ->
    {ok, list_to_atom("tecipe_listener_" ++ atom_to_list(Ref))}.

-spec make_acceptor_name(tecipe_listener_ref()) -> {ok, tecipe_acceptor_name()}.
make_acceptor_name(Ref)
  when is_atom(Ref) ->
    {ok, list_to_atom("tecipe_acceptor_" ++ atom_to_list(Ref))}.

-spec make_monitor_name(tecipe_listener_ref()) -> {ok, tecipe_monitor_name()}.
make_monitor_name(Ref)
  when is_atom(Ref) ->
    {ok, list_to_atom("tecipe_monitor_" ++ atom_to_list(Ref))}.

default_listener_acceptor() ->
    static.

default_listener_pool_count() ->
    5.

default_listener_transport() ->
    tecipe_tcp.

default_listener_monitor() ->
    false.
