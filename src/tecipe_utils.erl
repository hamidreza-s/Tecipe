-module(tecipe_utils).

-export([ipv4_ascii_to_bin/1, ipv4_bin_to_ascii/1]).
-export([ipv6_ascii_to_bin/1, ipv6_bin_to_ascii/1]).

-spec ipv4_ascii_to_bin(string()) -> inet:ip4_address().
ipv4_ascii_to_bin(IPv4Ascii) ->
    {ok, IPv4Tuple} = inet_parse:ipv4strict_address(IPv4Ascii),
    IPv4Tuple.

-spec ipv4_bin_to_ascii(inet:ip4_address()) -> string().
ipv4_bin_to_ascii(IPv4Tuple) ->
    IPv4Ascii = inet_parse:ntoa(IPv4Tuple),
    IPv4Ascii.

-spec ipv6_ascii_to_bin(string()) -> inet:ip6_address().
ipv6_ascii_to_bin(IPv6Ascii) ->
    {ok, IPv6Tuple} = inet_parse:ipv6strict_address(IPv6Ascii),
    IPv6Tuple.

-spec ipv6_bin_to_ascii(inet:ip6_address()) -> string().
ipv6_bin_to_ascii(IPv6Tuple) ->
    IPv6Ascii = inet_parse:ntoa(IPv6Tuple),
    IPv6Ascii.
