-module(tecipe_utils).

-export([echo_handler/3]).

-include("tecipe.hrl").

echo_handler(Transport, #tecipe_socket{inet_socket = Sock} = TecipeSock, Args) ->
    receive
	{tcp, Sock, Data} ->
	    Transport:send(TecipeSock, Data),
	    echo_handler(Transport, TecipeSock, Args);
	{tcp_error, Sock, _Reason} ->
	    Transport:close(TecipeSock);
	{tcp_close, Sock} ->
	    Transport:close(TecipeSock);
	_ ->
	    Transport:close(TecipeSock)
    end.
