-module(calc_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    application:ensure_started(tecipe),
    tecipe:start_listener(calc_server, 9090, {calc_handler, start, []},
			  [{acceptor, dynamic}, {pool, 10}, {transport, tecipe_tcp}]),

    calc_server_sup:start_link().

stop(_State) ->
    ok.
