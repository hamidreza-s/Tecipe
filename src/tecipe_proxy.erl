-module(tecipe_proxy).

-export([check/3]).

-include("tecipe.hrl").

-define(V2_SIGNATURE, 13,10,13,10,0,13,10,81,85,73,84,10). % 12 bytes
-define(V2_BYTE_SIZE_HEADER, 16).
-define(V2_VERSION, 2).
-define(V2_COMMAND_LOCAL, 0).
-define(V2_COMMAND_PROXY, 1).
-define(V2_FAMILY_AF_UNSPEC, 0).
-define(V2_FAMILY_AF_INET, 1).
-define(V2_FAMILY_AF_INET6, 2).
-define(V2_FAMILY_AF_UNIX, 3).
-define(V2_TRASPORT_UNSPEC, 0).
-define(V2_TRANSPORT_STREAM, 1).
-define(V2_TRANSPORT_DGRAM, 2).

-spec check(tecipe_socket(),
	    tecipe_listener_transport(),
	    tecipe_listener()) -> tecipe_socket().

check(_Sock, _Transport, #tecipe_listener{proxy = v1}) ->
    %% @TODO: implement it.
    #tecipe_proxy{};

check(Sock, Transport, #tecipe_listener{proxy = v2}) ->
    {ok, Data} = Transport:recv(Sock, ?V2_BYTE_SIZE_HEADER),

    case Data of
	<<?V2_SIGNATURE, Info:2/binary, Len:2/binary>> ->
	    {ok, Body} = Transport:recv(Sock, binary:decode_unsigned(Len)),
	    Sock#tecipe_socket{proxy = parse_v2(Info, Body)};

	BadFrame ->
	    %% @TODO: return and handle error
	    exit({bad_frame, BadFrame})
    end;

check(Sock, _, _) ->
    Sock.

%% === private function

-spec parse_v2(binary(), binary()) -> tecipe_proxy().
parse_v2(<<?V2_VERSION:4, Command:4, Family:4, Transport:4>>, Body) ->
    error_logger:info_msg("proxy info: ~p~n", [{Command, Transport, Family}]),
    error_logger:info_msg("proxy body: ~p~n", [Body]),
    #tecipe_proxy{proxy_version = v2,
		  proxy_command = parse_v2_command(Command),
		  proxy_family = parse_v2_family(Family),
		  proxy_transport = parse_v2_transport(Transport),
		  source_address = todo,
		  dest_address = todo,
		  source_port = todo,
		  dest_port = todo}.

-spec parse_v2_command(integer()) -> atom().
parse_v2_command(?V2_COMMAND_LOCAL) -> local;
parse_v2_command(?V2_COMMAND_PROXY) -> proxy.

-spec parse_v2_family(integer()) -> atom().
parse_v2_family(?V2_FAMILY_AF_UNSPEC) -> unspec;
parse_v2_family(?V2_FAMILY_AF_INET) -> inet4;
parse_v2_family(?V2_FAMILY_AF_INET6) -> inet6;
parse_v2_family(?V2_FAMILY_AF_UNIX) -> unix.

-spec parse_v2_transport(integer()) -> atom().
parse_v2_transport(?V2_TRASPORT_UNSPEC) -> unspec;
parse_v2_transport(?V2_TRANSPORT_STREAM) -> stream;
parse_v2_transport(?V2_TRANSPORT_DGRAM) -> dgram.
