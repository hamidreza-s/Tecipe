-module(tecipe_ssl).

-export([listen/2, accept/1, accept/2, send/2, recv/2, close/1]).

listen(Port, Opts) ->
    ssl:listen(Port, Opts).

accept(ListeningSock) ->
    accept(ListeningSock, infinity).

accept(ListeningSock, Timeout) ->
    case ssl:transport_accept(ListeningSock, Timeout) of
        {ok, Sock} ->
            ssl_accept(Sock, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

ssl_accept(Sock, Timeout) ->
    case ssl:ssl_accept(Sock, Timeout) of
        ok ->
            {ok, Sock};
        {error, Reason} ->
            {error, Reason}
    end.

send(Sock, Data) ->
    ssl:send(Sock, Data).

recv(Sock, Length) ->
    ssl:recv(Sock, Length).

close(Sock) ->
    ssl:close(Sock).
