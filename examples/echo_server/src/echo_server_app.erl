-module(echo_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    application:ensure_started(tecipe),
    tecipe:start_listener(echo_server, 8080, {echo_handler, echo, []},
			  [{monitor, true}],
			  [{reuseaddr, true}]),

    echo_server_sup:start_link().

stop(_State) ->
    ok.
