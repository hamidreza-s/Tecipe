-module(tecipe_utils).

-export([echo_handler/3, test/1]).

-include("tecipe.hrl").

echo_handler(Transport, #tecipe_socket{inet_socket = Sock} = TecipeSock, Args) ->
    error_logger:info_msg("echo handler socket: ~p~n", [TecipeSock]),
    receive
	{tcp, Sock, Data} ->
	    error_logger:info_msg("echo handler got: ~p~n", [Data]),
	    Transport:send(TecipeSock, Data),
	    error_logger:info_msg("echo handler sent: ~p~n", [Data]),
	    echo_handler(Transport, TecipeSock, Args);
	{tcp_error, Sock, _Reason} ->
	    error_logger:info_msg("echo handler got error and closed!~n"),
	    Transport:close(TecipeSock);
	{tcp_closed, Sock} ->
	    error_logger:info_msg("echo handler closed.~n"),
	    Transport:close(TecipeSock);
	Unexpected ->
	    error_logger:info_msg("echo handler got unexpected data and closed: ~p~n", [Unexpected]),
	    Transport:close(TecipeSock)
    end.

test(Port) ->
    tecipe:start_listener(test, Port, {tecipe_utils, echo_handler, []},
			  [{acceptor, dynamic}, {pool, 10},
			   {transport, tecipe_tcp}, {monitor, true}, {proxy, v2}],
			  [{reuseaddr, true}]).
