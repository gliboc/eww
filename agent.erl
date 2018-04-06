-module(agent).
-compile(export_all).


loop () ->
    receive 
        {Sender, Ref} ->
            erlang:send(Sender, {Ref, ok}),
            loop ()
    end.

ring_agent (NextPid) -> 
    io:format ("Agent ~p was succesfully started~n", [erlang:self()]),
    ring_agent (NextPid, []).

ring_agent (NextPid, L) ->
    receive
        {cmd, Cmd, _} ->
            case Cmd of
                {sendPid, Pid} -> erlang:send(Pid, pid (NextPid)),
                                  ring_agent (NextPid, L);

                {kill, KillPid} when KillPid =/= NextPid ->
                    erlang:send (NextPid, cmd ({kill, KillPid})),
                    ring_agent(NextPid, L);

                {kill, KillPid} when KillPid =:= NextPid ->
                    erlang:send (NextPid, cmd ({die, erlang:self()})),
                    ring_agent (NextPid, L);
               
                destroy ->
                    erlang:send (NextPid, cmd (destroy)),
                    erlang:exit ("Rcvd destroy signal~n");

                {changeNextPid, NewPid} ->
                    ring_agent (NewPid, L);

                {die, Murderer} ->
                    io:format ("I'm ~p and I die now. Pls remember~n", [erlang:self()]),
                    change_pid (Murderer, NextPid),
                    erlang:exit("Rcvd kill signal~n")
            end;

        {pack, Msg, Id} ->
            io:format("Agent ~p received msg ~p~n", [erlang:self(), Msg]),
            case (lists:member (Id, L)) of
                true -> ring_agent (NextPid, L);
                false -> io:format("Agent ~p passing around message ~p to ~p~n", [erlang:self(), Msg, NextPid]),
                         erlang:send(NextPid, {pack, Msg, Id}),
                         ring_agent (NextPid, L ++ [Id])
            end;

        {ping, Id} ->
            case (lists:member (Id, L)) of 
                true -> ring_agent (NextPid, L);
                false -> io:format ("Agent ~p was pinged~n", [erlang:self()]),
                         erlang:send(NextPid, {ping, Id}),
                         ring_agent (NextPid, L ++ [Id])
            end
    end.

last_ring_agent () ->
    io:format("Initiating first agent\n", []),
    receive
        {Pid, pid} ->
            ring_agent(Pid)
    end.

% -------- Trying a ring topology --------

ring_topology (N) ->
    io:format("Initiating a ring network with ~p nodes~n", [N]),
    FirstPid = erlang:spawn(agent, last_ring_agent, []),
    Pid = ring_topology (FirstPid, N-1),
    erlang:send(FirstPid, {Pid, pid}).

ring_topology (NextPid, 0) -> NextPid;
ring_topology (NextPid, N) ->
    io:format("Initiating agent ~p\n", [N+1]),
    Pid = erlang:spawn(agent, ring_agent, [NextPid]),
    ring_topology (Pid, N-1).

% ------- Utility ----------

hash() -> erlang:phash2({erlang:self(), now()}).

msg(Msg) -> {pack, Msg, hash()}.

cmd(Cmd) -> {cmd, Cmd, hash()}.

pid (Pid) -> {pid, Pid, hash()}.

change_pid (Pid, NewPid) -> erlang:send(Pid, cmd ({changeNextPid, NewPid})).

% ------- Distributed ping -----------

ping (Pid) ->
    erlang:send (Pid, {ping, hash()}).

% -------- Join the topology ----------

join (Pid) ->
    erlang:send (Pid, cmd ({sendPid, erlang:self()})),
    receive
        {pid, NextPid, _} -> 
            io:format ("Received the NextPid ~p for new node~n", [NextPid]),
            change_pid (Pid, erlang:self()),
            io:format ("Sent changePid instruction to ~p~n", [Pid]),
            ring_agent (NextPid)
    end.

new_node (Pid) ->
    P = erlang:spawn (agent, join, [Pid]),
    io:format("Initiated new agent with Pid ~p~n", [P]).

% ------- Destroy all nodes -------

destroy (Dest) ->
    erlang:send (Dest, cmd (destroy)).


% -------- Kill a node --------

kill(Dest, Kill) ->
    erlang:send(Dest, msg({kill, Kill})). 


% -------- Message passing ---------

broadcast(Pid, Msg) ->
    erlang:send (Pid, msg(Msg)).

% -------- Init a network for tests ------

init(N) ->
    {Pid, _} = ring_topology(N),
    io:format ("~nPassing a ping on the network~n", []),
    ping(Pid),
    Pid.


