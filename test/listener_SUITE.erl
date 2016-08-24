-module(listener_SUITE).
-compile(export_all).

-include("tecipe.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
    ok = application:ensure_started(tecipe),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [static_acceptor, dynamic_acceptor].

static_acceptor(_Config) ->
    Ref = foo,
    Port = 9999,
    Handler = {tecipe_utils, echo_handler, []},
    Acceptor = static,
    Transport = tecipe_tcp,
    Pool = 10,
    ListenerOpts = [{acceptor, Acceptor}, {pool, Pool}, {transport, Transport}],

    {ok, ListenerPID} = tecipe:start_listener(Ref, Port, Handler, ListenerOpts),
    {ok, #tecipe_listener{ref = Ref,
			  listener_name = ListenerName,
			  listener_pid = ListenerPID,
			  acceptor_name = AcceptorName,
			  acceptor_pid = AcceptorPID,
			  monitor_name = MonitorName,
			  monitor_pid = MonitorPID}} = tecipe:get_listener(Ref),

    ?assertEqual(ListenerPID, whereis(ListenerName)),
    ?assertEqual(AcceptorPID, whereis(AcceptorName)),
    ?assertEqual(MonitorPID, whereis(MonitorName)),

    ?assertEqual(true, is_process_alive(ListenerPID)),
    ?assertEqual(true, is_process_alive(AcceptorPID)),
    ?assertEqual(true, is_process_alive(MonitorPID)),

    {links, AcceptorLinks} = process_info(whereis(AcceptorName), links),
    ?assertEqual(Pool+1, length(AcceptorLinks)),

    ok.

dynamic_acceptor(_Config) ->
    Ref = bar,
    Port = 8888,
    Acceptor = dynamic,
    Transport = tecipe_tcp,
    Pool = 10,
    Handler = {tecipe_utils, echo_handler, []},
    ListenerOpts = [{acceptor, Acceptor}, {pool, Pool}, {transport, Transport}],

    {ok, ListenerPID} = tecipe:start_listener(Ref, Port, Handler, ListenerOpts),
    {ok, #tecipe_listener{ref = Ref,
			  listener_name = ListenerName,
			  listener_pid = ListenerPID,
			  acceptor_name = AcceptorName,
			  acceptor_pid = AcceptorPID,
			  monitor_name = MonitorName,
			  monitor_pid = MonitorPID}} = tecipe:get_listener(Ref),

    ?assertEqual(ListenerPID, whereis(ListenerName)),
    ?assertEqual(AcceptorPID, whereis(AcceptorName)),
    ?assertEqual(MonitorPID, whereis(MonitorName)),

    ?assertEqual(true, is_process_alive(ListenerPID)),
    ?assertEqual(true, is_process_alive(AcceptorPID)),
    ?assertEqual(true, is_process_alive(MonitorPID)),

    {links, AcceptorLinks} = process_info(whereis(AcceptorName), links),
    ?assertEqual(Pool+1, length(AcceptorLinks)),

    ok.
