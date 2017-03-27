-module(tecipe_proxy).

-export([check/3]).

-include("tecipe.hrl").

-define(V1_SIGNATURE, "PROXY ").
-define(V1_FAMILY_INET, "TCP4 ").
-define(V1_FAMILY_INET6, "TCP6 ").

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

check(Sock, Transport, #tecipe_listener{proxy = v1}) ->
    Transport:setopts(Sock, [{active, once}, {packet, line}]),
    InetSock = Sock#tecipe_socket.inet_socket,
    receive
	{tcp, InetSock, <<?V1_SIGNATURE, Body/binary>>} ->
	    Sock#tecipe_socket{proxy = parse_v1(Body)};

	BadFrame ->
	    exit({proxy_v1_parsing_error, BadFrame})
    end;

check(Sock, Transport, #tecipe_listener{proxy = v2}) ->
    {ok, Data} = Transport:recv(Sock, ?V2_BYTE_SIZE_HEADER),

    case Data of
	<<?V2_SIGNATURE, Info:2/binary, Len:2/binary>> ->
	    {ok, Body} = Transport:recv(Sock, binary:decode_unsigned(Len)),
	    Sock#tecipe_socket{proxy = parse_v2(Info, Body)};

	BadFrame ->
	    exit({proxy_v2_parsing_error, BadFrame})
    end;

check(Sock, _, _) ->
    Sock.

%% === private function

-spec parse_v1(binary()) -> tecipe_proxy().
parse_v1(<<Family:5/binary, Rest/binary>>) ->
    do_parse_v1(#tecipe_proxy{proxy_version = v1,
			      proxy_family = parse_v1_family(Family)},
		binary:replace(Rest, <<"\r\n">>, <<>>)).

do_parse_v1(#tecipe_proxy{proxy_family = inet4} = Proxy, RestBody) ->
    [SourceAddress, DestAddress, SourcePort, DestPort] = binary:split(RestBody, <<" ">>, [global]),
    Proxy#tecipe_proxy{source_address = parse_binary_ip4(SourceAddress),
		       dest_address = parse_binary_ip4(DestAddress),
		       source_port = binary_to_integer(SourcePort),
		       dest_port = binary_to_integer(DestPort)};

do_parse_v1(#tecipe_proxy{proxy_family = inet6} = Proxy, RestBody) ->
    [SourceAddress, DestAddress, SourcePort, DestPort] = binary:split(RestBody, <<" ">>, [global]),
    Proxy#tecipe_proxy{source_address = parse_binary_ip6(SourceAddress),
		       dest_address = parse_binary_ip6(DestAddress),
		       source_port = binary_to_integer(SourcePort),
		       dest_port = binary_to_integer(DestPort)}.

-spec parse_v2(binary(), binary()) -> tecipe_proxy().
parse_v2(<<?V2_VERSION:4, Command:4, Family:4, Transport:4>>, Body) ->
    do_parse_v2(#tecipe_proxy{proxy_version = v2,
			      proxy_command = parse_v2_command(Command),
			      proxy_family = parse_v2_family(Family),
			      proxy_transport = parse_v2_transport(Transport)},
		Body).

do_parse_v2(#tecipe_proxy{proxy_family = inet4} = Proxy,
	    <<SA1:8, SA2:8, SA3:8, SA4:8, DA1:8, DA2:8, DA3:8, DA4:8,
	      SourcePort:16, DestPort:16, _/binary>>) ->
    Proxy#tecipe_proxy{
      source_address = {SA1, SA2, SA3, SA4},
      dest_address = {DA1, DA2, DA3, DA4},
      source_port = SourcePort,
      dest_port = DestPort};

do_parse_v2(#tecipe_proxy{proxy_family = inet6} = Proxy,
	    <<SA1:16, SA2:16, SA3:16, SA4:16, SA5:16, SA6:16, SA7:16, SA8:16,
	      DA1:16, DA2:16, DA3:16, DA4:16, DA5:16, DA6:16, DA7:16, DA8:16,
	      SrcPort:16,
	      DestPort:16, _/binary>>) ->
    Proxy#tecipe_proxy{
      source_address = SrcPort,
      dest_address = DestPort,
      source_port = {SA1, SA2, SA3, SA4, SA5, SA6, SA7, SA8},
      dest_port = {DA1, DA2, DA3, DA4, DA5, DA6, DA7, DA8}};

do_parse_v2(Proxy, _) ->
    %% @TODO: implement unix sockets
    Proxy#tecipe_proxy{
      source_address = unsupported,
      dest_address = unsupported,
      source_port = unsupported,
      dest_port = unsupported}.


-spec parse_v1_family(binary()) -> atom().
parse_v1_family(<<?V1_FAMILY_INET>>) -> inet4;
parse_v1_family(<<?V1_FAMILY_INET6>>) -> inet6.

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

-spec parse_binary_ip4(binary()) -> inet:ip4_address().
parse_binary_ip4(BinaryIP) ->
    parse_binary_ip(BinaryIP).

-spec parse_binary_ip6(binary()) -> inet:ip6_address().
parse_binary_ip6(BinaryIP) ->
    parse_binary_ip(BinaryIP).

-spec parse_binary_ip(binary()) -> inet:ip_address().
parse_binary_ip(BinaryIP) ->
    {ok, TupleIP} = inet_parse:address(binary_to_list(BinaryIP)),
    TupleIP.
