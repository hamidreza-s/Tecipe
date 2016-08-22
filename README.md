Tecipe
=====

Tecipe (prounce it like recipe) is a lightweight and flexible TCP socket acceptor pool for Erlang.

Usage
-----

Include `tecipe` in the `rebar.conf` file of your project.

```erlang
{deps, [
        %% ...
        {tecipe, ".*", {git, "git://github.com/bisphone/tecipe.git", {branch, "master"}}}
       ]}.
```

Now you can start it as well as a listener as follow:

```erlang
Name = foo,
Port = 8080,
Handler = {your_handler, your_function, []},
ListenerOpts = [{acceptor, static}, {pool, 50}, {transport, tecipe_tcp}],
TransportOpts = [],
{ok, ListenerPID, ListenerRef} = tecipe:start_listener(Name, Port, Handler,
                                                       ListenerOpts, TransportOpts),
```

This way a listener with 50 acceptors is started.

Contribution
-----

Comments, contributions and patches are greatly appreciated.

License
-----
The MIT License (MIT).
