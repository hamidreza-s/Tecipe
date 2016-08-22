-module(tecipe_utils).

-export([echo_handler/3]).

echo_handler(Transport, Sock, Args) ->
    receive
	{tcp, Sock, Data} ->
	    Transport:send(Sock, Data),
	    echo_handler(Transport, Sock, Args);
	{tcp_error, Sock, _Reason} ->
	    Transport:close(Sock);
	{tcp_close, Sock} ->
	    Transport:close(Sock);
	_ ->
	    Transport:close(Sock)
    end.
