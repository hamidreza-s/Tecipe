-module(echo_handler).

-export([echo/3]).

echo(Transport, Sock, Args) ->
    receive
	{tcp, Sock, Data} ->
	    Transport:send(Sock, Data),
	    echo(Transport, Sock, Args);
	{tcp_error, Sock, _Reason} ->
	    Transport:close(Sock);
	{tcp_close, Sock} ->
	    Transport:close(Sock);
	_ ->
	    Transport:close(Sock)
    end.
