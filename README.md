Tecipe
=====

Tecipe (pronounce it like recipe) is a lightweight and flexible TCP socket acceptor pool for Erlang.

Quick Start
-----

Include `tecipe` in the `rebar.conf` file of your project.

```erlang
{deps, [
        %% ...
        {tecipe, ".*", {git, "git://github.com/bisphone/tecipe.git", {branch, "master"}}}
       ]}.
```

Then start its application.

```erlang
ok = application:start(tecipe).
```

Now you can start a TCP listener as follows:

```erlang
Ref = foo, Port = 8080,
Handler = fun(Transport, Sock) ->
              receive
                  {tcp, Data} ->
                      Transport:send(Sock, Data);
                   _ ->
                      Transport:close(Sock)
              end
          end,

ListenerOpts = [{acceptor, static}, {pool, 50}, {transport, tecipe_tcp}, {monitor, true}],
{ok, ListenerPID} = tecipe:start_listener(Ref, Name, Handler, ListenerOpts),
```

This way a static pool with 50 acceptors is started and echoes back every data it receives.

Tecipe Concepts
----

* **Listener**: An actor which is listening on a port and waits for new connections by its acceptors.
* **Acceptor**: An actor which accepts new connections and spawns workers to handler the connection.
* **Worker**: An actor which owns an accepted socket and do the logic.
* **Monitor**: An actor which is attached to a listener and monitors the workers and collects their stats.
* **Pool**: The number of acceptors which a listeners created.
* **Transport**: An interface which must implement some required API to work with sockets.
* **Handler**: A function or MFA which is responsible to implement the logic of workers.

Acceptor Types
-----

* **Static**:

A listener with static acceptor pool accepts incoming connections inside the acceptors and then passes the socket to
a newly spawned actor which is called a worker. This way the acceptors remain intact and do not do anything except accepting
new connections.
```
                       +--------------+                           +--------------+
                    +--+    Monitor   |                        +--+    Monitor   +--+
                    |  +--------------+                        |  +--------------+  |
1) A listener with  |                      2) After coming     |                    |
   no connections   |  +--------------+       new connections  |  +--------------+  |  +--------------+
                    +--+ Acceptor # 1 |                        +--+ Acceptor # 1 |  +--+  Worker # 1  |
+----------------+  |  +--------------+    +----------------+  |  +--------------+  |  +--------------+
|    Listener    +--+                      |    Listener    +--+                    |
+----------------+  |  +--------------+    +----------------+  |  +--------------+  |  +--------------+
                    +--+ Acceptor # 2 |                        +--+ Acceptor # 2 |  +--+  Worker # 2  |
                    |  +--------------+                        |  +--------------+  |  +--------------+
                    |                                          |                    |
                    |  +--------------+                        |  +--------------+  |  +--------------+
                    +--+ Acceptor # 3 |                        +--+ Acceptor # 3 |  +--+  Worker # 3  |
                       +--------------+                           +--------------+     +--------------+
```

* **Dynamic**:

A listener with dynamic acceptor pool accepts incoming connections inside the acceptors and make it a worker, and then
a new acceptor is spawned. This way the acceptors are changed to workers and new acceptors are replaced with.
```
                       +--------------+                           +--------------+
                    +--+    Monitor   |                        +--+    Monitor   +--+
                    |  +--------------+                        |  +--------------+  |
1) A listener with  |                      2) After coming     |                    |
   no connections   |  +--------------+       new connections  |  +--------------+  |  +--------------+
                    +--+ Acceptor # 1 |                        +--+ Acceptor # 4 |  +--+  Worker # 1  |
+----------------+  |  +--------------+    +----------------+  |  +--------------+  |  +--------------+
|    Listener    +--+                      |    Listener    +--+                    |
+----------------+  |  +--------------+    +----------------+  |  +--------------+  |  +--------------+
                    +--+ Acceptor # 2 |                        +--+ Acceptor # 5 |  +--+  Worker # 2  |
                    |  +--------------+                        |  +--------------+  |  +--------------+
                    |                                          |                    |
                    |  +--------------+                        |  +--------------+  |  +--------------+
                    +--+ Acceptor # 3 |                        +--+ Acceptor # 6 |  +--+  Worker # 3  |
                       +--------------+                           +--------------+     +--------------+
```


**Note**: *Both approaches could have good results in different scenarios. I have plan to write some benchmarks to show the differences.*

Monitoring Workers
-----

When the `monitor` option of a listener is set to `true`, a monitor actor becomes responsible for collecting the statistics and giving the report.
Using `get_stats/1` you can get the statistics as return value and format it as you want. However there is a `print_stats/1` function which prints
all the available statistics as follows.

```
worker-pid      socket-port          monitor-ref          local-address        remote-address       recv-cnt recv-max recv-avg recv-oct recv-dvi send-cnt send-max send-avg send-oct send-pend
--------------- -------------------  -------------------  -------------------  -------------------  -------- -------- -------- -------- -------- -------- -------- -------- -------- ---------
<0.227.0>       #Port<0.40397>       #Ref<0.0.1.1156>     127.0.0.1:9999       127.0.0.1:52876      1631     42       20       23112    0        1112     40       20       23112    0
<0.219.0>       #Port<0.40389>       #Ref<0.0.1.1140>     127.0.0.1:9999       127.0.0.1:52868      6341     60       20       23311    0        1621     40       20       23311    0
<0.211.0>       #Port<0.40381>       #Ref<0.0.1.1124>     127.0.0.1:9999       127.0.0.1:52860      6112     31       21       23641    0        6322     39       21       23641    0
<0.224.0>       #Port<0.40394>       #Ref<0.0.1.1150>     127.0.0.1:9999       127.0.0.1:52873      1624     60       21       23732    0        6412     40       21       23732    0
<0.216.0>       #Port<0.40386>       #Ref<0.0.1.1134>     127.0.0.1:9999       127.0.0.1:52865      5123     10       21       23373    0        1523     40       21       23373    0
<0.208.0>       #Port<0.40378>       #Ref<0.0.1.1118>     127.0.0.1:9999       127.0.0.1:52857      1112     35       20       22523    0        1644     39       20       22523    0
<0.221.0>       #Port<0.40391>       #Ref<0.0.1.1144>     127.0.0.1:9999       127.0.0.1:52870      1632     70       21       23394    0        1114     40       21       23394    0
<0.213.0>       #Port<0.40383>       #Ref<0.0.1.1128>     127.0.0.1:9999       127.0.0.1:52862      2312     49       20       23326    0        6322     40       20       23326    0

```

For more information about the fields please refer to [inet documentation page](http://erlang.org/doc/man/inet.html#getstat-1) of Erlang/OTP.

Tecipe API
-----

* `start_listener/3,4,5` starts a new listener.
* `stop_listener/1` stops a previously started listener.
* `get_listener/1` gets information about a running listener.
* `get_workers/1` gets a list of a listener's workers.
* `get_stats/1` gets a detailed statistics about listener's workers.
* `print_stats/1` print the statistics of listener's workers.

Also there is a complete list of type specifications in Tecipe main [header file](https://github.com/bisphone/Tecipe/blob/master/include/tecipe.hrl).

Listener Options
-----

* `acceptor` defines the type of acceptor which could be `dynamic` or `static`. Default is `static`.
* `pool` defines the number of acceptor actors of a listener acceptor pool. Default is `5`.
* `monitor` defines whether the listener monitors their workers or not. Default is `false`.
* `transport` defines the using interface of tecipe which could be `tecipe_tcp` or `tecipe_ssl`. Default is `tecipe_tcp`.

Contribution
-----

Comments, contributions and patches are greatly appreciated.

License
-----
The MIT License (MIT).
