-module(agent).
-compile(export_all).


loop () ->
    receive 
        {Sender, Ref} ->
            erlang:send(Sender, {Ref, ok}),
            loop ()
    end.

ring_agent (NextPid) -> ring_agent (NextPid, []).

ring_agent (NextPid, L) -> 
    receive
        {cmd, Cmd, Id} ->
            case Cmd of
                {sendPid, Pid} -> erlang:send(Pid, pid (NextPid)),
                                  ring_agent (NextPid, L);

                {kill, KillPid} when KillPid != NextPid ->
                    erlang:send (NextPid, cmd ({kill,KillPid})),
                    ring_agent(NextPid, L);

                {kill, KillPid} ->
                    erlang:send (NextPid, cmd ({die, erlang:self()})),
                    ring_agent (NextPid, L);

                {changeNext, NewPid} ->
                    ring_agent (NewPid, L)
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

hash(Pid) -> erlang:phash2({Pid, now()}).

msg(Msg) -> {pack, Msg, hash(erlang:self())}.

cmd(Cmd) -> {cmd, Cmd, hash(erlang:self())}.

pid (Pid) -> {pid, Pid, hash(erlang:self())}.

                        



% ------- Distributed ping -----------

ping (Pid) ->
    erlang:send (Pid, {ping, hash()}).

% -------- Join the topology ----------

join (Pid) ->
    erlang:send (Pid, cmd (sendPid)),
    receive
        {NextPid, pid} -> 
            ring_agent (NextPid)
    end,
    erlang:send (Pid, cmd({changePid, erlang:self()})).

new_node (Pid) ->
    P = erlang:spawn (agent, join, [Pid]),
    io:format("Initiated new agent with Pid ~p~n", [P]).

% -------- Kill a node --------

kill(Dest, Kill) ->
    erlang:send(Dest, msg({kill, Kill})). 


% -------- Init a network for tests ------

init() ->
    {Pid, _} = ring_topology(4),
    ping(Pid),
    Pid.


