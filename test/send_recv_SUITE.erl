-module(send_recv_SUITE).
-compile(export_all).

-include("tecipe.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    ok = application:ensure_started(tecipe),

    AcceptorPort1 = 9993,
    AcceptorPort2 = 9994,

    {ok, _} = tecipe:start_listener(send_recv_test_1, AcceptorPort1,
				    {tecipe_utils, echo_handler, []},
				    [{acceptor, static}, {pool, 10}, {transport, tecipe_tcp}]),

    {ok, _} = tecipe:start_listener(send_recv_test_2, AcceptorPort2,
				    {tecipe_utils, echo_handler, []},
				    [{acceptor, dynamic}, {pool, 10}, {transport, tecipe_tcp}]),


    [{port_1, AcceptorPort1}, {port_2, AcceptorPort2} | Config].

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
    [send_recv_1, send_recv_2].

send_recv_1(Config) ->
    Port = ?config(port_1, Config),
    do_send_recv(Port).

send_recv_2(Config) ->
    Port = ?config(port_1, Config),
    do_send_recv(Port).

%% === private functions

do_send_recv(Port) ->
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary]),
    lists:foreach(fun(I) ->
			  Data = <<"ping", I>>,
			  ok = gen_tcp:send(Socket, Data),
			  receive {tcp, Socket, Data} -> ok end
		  end,
		  lists:seq(1, 9)),
    ok.
