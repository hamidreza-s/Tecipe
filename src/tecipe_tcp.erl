-module(tecipe_tcp).

-export([listen/2, accept/1, setopts/2, send/2, recv/2,
	 controlling_process/2, peername/1, sockname/1, getstat/1, close/1]).

-include("tecipe.hrl").

listen(Port, Opts) ->
    gen_tcp:listen(Port, Opts).

accept(ListeningSock) ->
    gen_tcp:accept(ListeningSock).

setopts(#tecipe_socket{inet_socket = Sock}, Opts) ->
    inet:setopts(Sock, Opts).

send(#tecipe_socket{inet_socket = Sock}, Data) ->
    gen_tcp:send(Sock, Data).

recv(#tecipe_socket{inet_socket = Sock}, Length) ->
    gen_tcp:recv(Sock, Length).

controlling_process(#tecipe_socket{inet_socket = Sock}, Pid) ->
    gen_tcp:controlling_process(Sock, Pid).

peername(#tecipe_socket{inet_socket = Sock}) ->
    inet:peername(Sock).

sockname(#tecipe_socket{inet_socket = Sock}) ->
    inet:sockname(Sock).

getstat(#tecipe_socket{inet_socket = Sock}) ->
    inet:getstat(Sock).

close(#tecipe_socket{inet_socket = Sock}) ->
    gen_tcp:close(Sock).
