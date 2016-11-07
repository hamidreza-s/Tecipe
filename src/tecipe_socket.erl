-module(tecipe_socket).

-export([upgrade/2]).

-include("tecipe.hrl").

-spec upgrade(tecipe_inet_socket(), tecipe_listener()) -> tecipe_socket().
upgrade(Sock, ListenerRec) ->
    Proxy = ListenerRec#tecipe_listener.proxy,
    #tecipe_socket{inet_socket = Sock, proxy = proxy(Sock, Proxy)}.


%% === private functions

proxy(_, false) ->
    #tecipe_proxy{};
proxy(_Sock, v1) ->
    %% @TODO: implement it.
    #tecipe_proxy{};
proxy(_Sock, v2) ->
    %% @TODO: implement it.
    #tecipe_proxy{}.
