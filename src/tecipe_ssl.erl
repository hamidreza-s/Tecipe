-module(tecipe_ssl).

-export([listen/2, accept/1, accept/2, send/2, recv/2,
	 controlling_process/2, peername/1, sockname/1,
	 setopts/2, getopts/2, getstat/1, close/1]).

-include("tecipe.hrl").

listen(Port, Opts) ->
    ssl:listen(Port, Opts).

accept(ListeningSock) ->
    accept(ListeningSock, infinity).

accept(ListeningSock, Timeout) ->
    case ssl:transport_accept(ListeningSock, Timeout) of
        {ok, Sock} ->
            do_accept(Sock, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

do_accept(Sock, Timeout) ->
    case ssl:ssl_accept(Sock, Timeout) of
        ok ->
            {ok, Sock};
        {error, Reason} ->
            {error, Reason}
    end.

send(#tecipe_socket{inet_socket = Sock}, Data) ->
    ssl:send(Sock, Data);
send(Sock, Data) ->
    ssl:send(Sock, Data).

recv(#tecipe_socket{inet_socket = Sock}, Length) ->
    ssl:recv(Sock, Length);
recv(Sock, Length) ->
    ssl:recv(Sock, Length).

controlling_process(#tecipe_socket{inet_socket = Sock}, Pid) ->
    ssl:controlling_process(Sock, Pid);
controlling_process(Sock, Pid) ->
    ssl:controlling_process(Sock, Pid).

peername(#tecipe_socket{inet_socket = Sock}) ->
    ssl:peername(Sock);
peername(Sock) ->
    ssl:peername(Sock).

sockname(#tecipe_socket{inet_socket = Sock}) ->
    ssl:sockname(Sock);
sockname(Sock) ->
    ssl:sockname(Sock).

getopts(#tecipe_socket{inet_socket = Sock}, Opts) ->
    ssl:getopts(Sock, Opts);
getopts(Sock, Opts) ->
    ssl:getopts(Sock, Opts).

setopts(#tecipe_socket{inet_socket = Sock}, Opts) ->
    ssl:setopts(Sock, Opts);
setopts(Sock, Opts) ->
    ssl:setopts(Sock, Opts).

getstat(#tecipe_socket{inet_socket = Sock}) ->
    ssl:getstat(Sock);
getstat(Sock) ->
    ssl:getstat(Sock).

close(#tecipe_socket{inet_socket = Sock}) ->
    ssl:close(Sock);
close(Sock) ->
    ssl:close(Sock).
