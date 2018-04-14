%% Agent to be spawn on the nodes

-module(agent).
-export([init/1, loop/1]).
-export([ring_topology/1, ping/1, join/1, new_node/1, 
         destroy/1, kill/2, init_network/1]).

-include_lib("state.hrl").


init (NextPid) -> 
    io:format ("Agent ~p was succesfully started~n", [erlang:self()]),
    loop (#state{nextPid=NextPid}).


loop (S) ->
    receive
        {cmd, Cmd, _} ->
            loop (handlers:handle_cmd (Cmd, S));

        {pack, Msg, Id} ->
            loop (handlers:handle_msg (Msg, Id, S));

        {data, Binary, Hash} ->
            loop (transfer:handle_data ({Binary, Hash}, S));

        {ping, Id} ->
            loop (handlers:handle_ping (Id, S))
    end.



% -------- Trying a ring topology --------


last_init () ->
    io:format("Initiating first agent\n", []),
    receive
        {Pid, pid} ->
            init(Pid)
    end.


ring_topology (N) ->
    io:format("Initiating a ring network with ~p nodes~n", [N]),
    FirstPid = erlang:spawn(agent, last_init, []),
    Pid = ring_topology (FirstPid, N-1),
    erlang:send(FirstPid, {Pid, pid}).


ring_topology (NextPid, 0) -> NextPid;
ring_topology (NextPid, N) ->
    io:format("Initiating agent ~p\n", [N+1]),
    Pid = erlang:spawn(agent, init, [NextPid]),
    ring_topology (Pid, N-1).



% ------- Distributed ping -----------

ping (Pid) ->
   erlang:send (Pid, {ping, utils:hash()}).

% -------- Join the topology ----------

join (Pid) ->
    com:ask_pid(Pid),
    receive
        {pid, NextPid, _} -> 
            io:format ("Received the NextPid ~p for new node~n", [NextPid]),
            com:change_pid (Pid, erlang:self()),
            io:format ("Sent changePid instruction to ~p~n", [Pid]),
            init (NextPid)
    end.

new_node (Pid) ->
    P = erlang:spawn (agent, join, [Pid]),
    io:format("Initiated new agent with Pid ~p~n", [P]).

% ------- Destroy all nodes -------

destroy (Dest) ->
    erlang:send (Dest, com:cmd (destroy)).


% -------- Kill a node --------

kill(Dest, Kill) ->
    erlang:send(Dest, com:msg({kill, Kill})). 


% -------- Init a network for tests ------

init_network(N) ->
    {Pid, _} = ring_topology(N),
    io:format ("~nPassing a ping on the network~n", []),
    ping(Pid),
    Pid.


