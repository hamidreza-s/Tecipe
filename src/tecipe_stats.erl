-module(tecipe_stats).

-export([format/1]).

-include("tecipe.hrl").

format(Stats) ->
    io:format(
      "~-15s ~-20s ~-20s ~-20s ~-20s ~-5s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-9s ~n",
      ["worker-pid", "socket-port", "monitor-ref", "local-address", "remote-address", "proxy",
       "recv-cnt", "recv-max", "recv-avg", "recv-oct", "recv-dvi",
       "send-cnt", "send-max", "send-avg", "send-oct", "send-pend"]),
    io:format(
      "~-15s ~-20s ~-20s ~-20s ~-20s ~-5s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-9s ~n",
      ["---------------", "-------------------", "-------------------",
       "-------------------", "-------------------", "-----",
       "--------", "--------", "--------", "--------", "--------",
       "--------", "--------", "--------", "--------", "---------"]),
    do_format(Stats).
do_format([{MonitorRef, #tecipe_socket_stats{
			   worker_pid = WorkerPID,
			   socket_port = SocketPort,
			   remote_ip = RemoteIP,
			   remote_port = RemotePort,
			   local_ip = LocalIP,
			   local_port = LocalPort,
			   proxy = Proxy,
			   recv_cnt = RecvCnt,
			   recv_max = RecvMax,
			   recv_avg = RecvAvg,
			   recv_oct = RecvOct,
			   recv_dvi = RecvDiv,
			   send_cnt = SendCnt,
			   send_max = SendMax,
			   send_avg = SendAvg,
			   send_oct = SendOct,
			   send_pend = SendPend}} | Stats]) ->
    io:format(
      "~-15s ~-20s ~-20s ~-20s ~-20s ~-5s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-8s ~-9s ~n",
      [erlang:pid_to_list(WorkerPID), erlang:port_to_list(SocketPort), erlang:ref_to_list(MonitorRef),
       inet_parse:ntoa(LocalIP) ++ ":" ++ integer_to_list(LocalPort),
       inet_parse:ntoa(RemoteIP) ++  ":" ++ integer_to_list(RemotePort),
       atom_to_list(Proxy),
       integer_to_list(RecvCnt), integer_to_list(RecvMax), integer_to_list(RecvAvg),
       integer_to_list(RecvOct), integer_to_list(RecvDiv), integer_to_list(SendCnt),
       integer_to_list(SendMax), integer_to_list(SendAvg), integer_to_list(SendOct),
       integer_to_list(SendPend)]),

    do_format(Stats);
do_format([]) ->
    ok.
