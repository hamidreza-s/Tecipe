-define(LISTENER_TAB, tecipe_listeners).

-record(tecipe_listener, {name,
			  listener_name, listener_pid,
			  acceptor_name, acceptor_pid,
			  monitor_name, monitor_pid,
			  acceptor_type, acceptor_pool,
			  transport, handler}).

-type tecipe_listener() :: #tecipe_listener{}.

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
