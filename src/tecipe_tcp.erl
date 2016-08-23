-module(tecipe_tcp).

-export([listen/2, accept/1, setopts/2, send/2, recv/2,
	 controlling_process/2, close/1]).

listen(Port, Opts) ->
    gen_tcp:listen(Port, Opts).

accept(ListeningSock) ->
    gen_tcp:accept(ListeningSock).

setopts(Sock, Opts) ->
    inet:setopts(Sock, Opts).

send(Sock, Data) ->
    gen_tcp:send(Sock, Data).

recv(Sock, Length) ->
    gen_tcp:recv(Sock, Length).

controlling_process(Sock, Pid) ->
    gen_tcp:controlling_process(Sock, Pid).

close(Sock) ->
    gen_tcp:close(Sock).
