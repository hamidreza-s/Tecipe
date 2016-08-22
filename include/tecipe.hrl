-define(LISTENER_TAB, tecipe_listeners).

-record(tecipe_listener, {name,
			  listener_name, listener_pid,
			  acceptor_name, acceptor_pid,
			  collector_name, collector_pid,
			  acceptor_type, acceptor_pool,
			  transport, handler}).
