%% @doc This module tests the functions in the client module.
-module(client_tests).
-export([test_start_stop/1]).

%% @doc Tests starting and stopping a node on a host that is different
%% from erlang:self(). If you use the same Host as self, it can't work.
test_start_stop(Host) ->
    {ok, Platform} = agent:ring_topology(10),
    Pid = client:start(Host, Platform),
    client:ping(Pid),
    client:stop(Pid),
    client:ping(Pid).


