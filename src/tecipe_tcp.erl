-module(tecipe_tcp).

-export([listen/2, accept/1, send/2, recv/2,
	 controlling_process/2, peername/1, sockname/1,
	 setopts/2, getopts/2, getstat/1, close/1]).

-include("tecipe.hrl").

listen(Port, Opts) ->
    gen_tcp:listen(Port, Opts).

accept(ListeningSock) ->
    gen_tcp:accept(ListeningSock).

send(#tecipe_socket{inet_socket = Sock}, Data) ->
    gen_tcp:send(Sock, Data);
send(Sock, Data) ->
    gen_tcp:send(Sock, Data).

recv(#tecipe_socket{inet_socket = Sock}, Length) ->
    gen_tcp:recv(Sock, Length);
recv(Sock, Length) ->
    gen_tcp:recv(Sock, Length).

controlling_process(#tecipe_socket{inet_socket = Sock}, Pid) ->
    gen_tcp:controlling_process(Sock, Pid);
controlling_process(Sock, Pid) ->
    gen_tcp:controlling_process(Sock, Pid).

peername(#tecipe_socket{inet_socket = Sock, proxy = false}) ->
    inet:peername(Sock);
peername(#tecipe_socket{proxy = #tecipe_proxy{source_address = Address, source_port = Port}}) ->
    {ok, {Address, Port}};
peername(Sock) ->
    inet:peername(Sock).

sockname(#tecipe_socket{inet_socket = Sock, proxy = false}) ->
    inet:sockname(Sock);
sockname(#tecipe_socket{proxy = #tecipe_proxy{dest_address = Address, dest_port = Port}}) ->
    {ok, {Address, Port}};
sockname(Sock) ->
    inet:sockname(Sock).

setopts(#tecipe_socket{inet_socket = Sock}, Opts) ->
    inet:setopts(Sock, Opts);
setopts(Sock, Opts) ->
    inet:setopts(Sock, Opts).

getopts(#tecipe_socket{inet_socket = Sock}, Opts) ->
    inet:getopts(Sock, Opts);
getopts(Sock, Opts) ->
    inet:getopts(Sock, Opts).

getstat(#tecipe_socket{inet_socket = Sock}) ->
    inet:getstat(Sock);
getstat(Sock) ->
    inet:getstat(Sock).

close(#tecipe_socket{inet_socket = Sock}) ->
    gen_tcp:close(Sock);
close(Sock) ->
    gen_tcp:close(Sock).
