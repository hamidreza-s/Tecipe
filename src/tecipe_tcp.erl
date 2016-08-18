-module(tecipe_tcp).

-export([listen/2, accept/1, send/2, recv/2, close/1]).

listen(Port, Opts) ->
    gen_tcp:listen(Port, Opts).

accept(ListeningSock) ->
    gen_tcp:accept(ListeningSock).

send(Sock, Data) ->
    gen_tcp:send(Sock, Data).

recv(Sock, Length) ->
    gen_tcp:recv(Sock, Length).

close(Sock) ->
    gen_tcp:close(Sock).
