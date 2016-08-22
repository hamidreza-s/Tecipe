-module(tecipe_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("tecipe.hrl").

start(_StartType, _StartArgs) ->
    ets:new(?LISTENER_TAB, [public, named_table]),
    tecipe_sup:start_link().

stop(_State) ->
    ok.
