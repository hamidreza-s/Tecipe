-define(LISTENER_TAB, tecipe_listeners).


-record(tecipe_socket, {inet_socket :: tecipe_inet_socket(),
			proxy :: tecipe_proxy()}).

-record(tecipe_proxy, {inet_version :: ipv4 | ipv6,
		       proxy_version :: tecipe_proxy_version(),
		       source_address :: inet:ip_address(),
		       dest_address :: inet:ip_address(),
		       source_port :: inet:port_number(),
		       dest_port :: inet:port_number()}).

-record(tecipe_listener, {ref :: atom(),
			  listener_name :: atom(),
			  listener_pid :: pid(),
			  acceptor_name :: atom(),
			  acceptor_pid :: pid(),
			  monitor_name :: atom(),
			  monitor_pid :: pid(),
			  acceptor_type :: tecipe_acceptor_type(),
			  acceptor_pool :: integer(),
			  transport :: tecipe_listener_transport(),
			  handler :: tecipe_listener_handler(),
			  monitor :: boolean(),
			  proxy :: false | tecipe_proxy_version()}).

-record(tecipe_socket_stats, {worker_pid :: pid(),
			      socket_port :: inet:port_number(),
			      local_ip :: inet:ip_address(),
			      local_port :: inet:ip_address(),
			      remote_ip :: inet:ip_address(),
			      remote_port :: inet:ip_address(),
			      recv_cnt :: integer(),
			      recv_max :: integer(),
			      recv_avg :: integer(),
			      recv_oct :: integer(),
			      recv_dvi :: integer(),
			      send_cnt :: integer(),
			      send_max :: integer(),
			      send_avg :: integer(),
			      send_oct :: integer(),
			      send_pend :: integer()}).

-type tecipe_acceptor_type() :: dynamic | static.

-type tecipe_proxy() :: #tecipe_proxy{}.

-type tecipe_proxy_version() :: v1 | v2.

-type tecipe_inet_socket() ::  inet:socket() | ssl:sslsocket().

-type tecipe_listener() :: #tecipe_listener{}.

-type tecipe_socket() :: #tecipe_socket{}.

-type tecipe_listener_ref() :: atom().

-type tecipe_listener_name() :: atom().

-type tecipe_acceptor_name() :: atom().

-type tecipe_monitor_name() :: atom().

-type tecipe_listener_port() :: integer().

-type tecipe_listener_handler_mfa() :: {module(), atom(), list()}.

-type tecipe_listener_handler_fun() :: fun((tecipe_listener_transport(),
					    tecipe_socket()) -> no_return()).

-type tecipe_listener_handler() :: tecipe_listener_handler_mfa() |
				   tecipe_listener_handler_fun().

-type tecipe_listener_transport() :: tecipe_tcp | tecipe_ssl.

-type tecipe_listener_opts() :: list(tecipe_listener_opt()).

-type tecipe_listener_opt() :: {transport, tecipe_listener_transport()} | {monitor, boolean()} |
			       {acceptor, tecipe_acceptor_type()} | {pool, integer()} |
			       {proxy, false | tecipe_proxy_version()}.

-type tecipe_transport_opts() :: gen_tcp:option() | ssl:options().

-type tecipe_listener_pid() :: pid().

-type tecipe_workers() :: list(any()).

-type tecipe_stats() :: list(any()).
