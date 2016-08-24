-define(LISTENER_TAB, tecipe_listeners).

-record(tecipe_listener, {ref,
			  listener_name, listener_pid,
			  acceptor_name, acceptor_pid,
			  monitor_name, monitor_pid,
			  acceptor_type, acceptor_pool,
			  transport, handler, monitor}).

-type tecipe_listener() :: #tecipe_listener{}.

-type tecipe_listener_ref() :: atom().

-type tecipe_listener_name() :: atom().

-type tecipe_acceptor_name() :: atom().

-type tecipe_monitor_name() :: atom().

-type tecipe_listener_port() :: integer().

-type tecipe_listener_handler_mfa() :: {module(), atom(), list()}.

-type tecipe_listener_handler_fun() :: fun((tecipe_listener_transport(),
					    inet:socket()) -> no_return()).

-type tecipe_listener_handler() :: tecipe_listener_handler_mfa() |
				   tecipe_listener_handler_fun().

-type tecipe_listener_transport() :: tecipe_tcp | tecipe_ssl.

-type tecipe_listener_opts() :: list(tecipe_listener_opt()).

-type tecipe_listener_opt() :: {transport, tecipe_listener_transport()} | {monitor, boolean()} |
			       {acceptor, static | dynamic} | {pool, integer()}.

-type tecipe_transport_opts() :: gen_tcp:option() | ssl:options() | sctp:option().

-type tecipe_listener_pid() :: pid().
