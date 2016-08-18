-module(tecipe).

-export([start/0, start_listener/2, start_listener/3, start_listener/4]).

-type tecipe_listener_opts() :: list(tecipe_listener_opt()).

-type tecipe_listener_transport() :: tecipe_tcp | tecipe_ssl.

-type tecipe_listener_opt() :: {name, atom()} | {port, integer()} | {pool_type, worker | acceptor} |
			       {pool_actors, integer()} | {transport, tecipe_listener_transport()}.

-type tecipe_transport_opts() :: gen_tcp:option() | ssl:options() | sctp:option().

start() ->
    application:start(?MODULE).

-spec start_listener(atom(), integer()) -> {ok, pid()}.
start_listener(Name, Port) ->
    start_listener(Name, Port, [], []).

-spec start_listener(atom(), integer(), tecipe_listener_opts()) -> {ok, pid()}.
start_listener(Name, Port, ListenerOpts) ->
    start_listener(Name, Port, ListenerOpts, []).

-spec start_listener(atom(), integer(), tecipe_listener_opts(), tecipe_transport_opts()) -> {ok, pid()}.
start_listener(Name, Port, ListenerOpts, TransportOpts) ->
    PoolType = proplists:get_value(pool_type, ListenerOpts, default_listener_pool_type()),
    PoolCount = proplists:get_value(pool_count, ListenerOpts, default_listener_pool_count()),
    Transport = proplists:get_value(transport, ListenerOpts, default_listener_transport()),

    NewListenerOpts = [{pool_type, PoolType}, {pool_count, PoolCount}, {transport, Transport}],

    ListenerSup = {{tecipe_listener_sup, Name},
		   {tecipe_listener_sup, start_link, [Name, Port, NewListenerOpts, TransportOpts]},
		   permanent,
		   3000,
		   supervisor,
		   [tecipe_listener_sup]},

    supervisor:start_child(tecipe_sup, ListenerSup).

default_listener_pool_type() ->
    acceptor.

default_listener_pool_count() ->
    2.

default_listener_transport() ->
    tecipe_tcp.
