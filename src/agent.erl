%% Agent to be spawn on the nodes

-module(agent).
-export([init/1, loop/1, terminate/2]).
-export([ring_topology/1, last_init/0, ping/1, join/1, new_node/1, 
         destroy/1, kill/2, init_network/1, start_elect/1]).

-include_lib("state.hrl").


init(NextPid) -> 
    io:format("Agent ~p was succesfully started~n", [erlang:self()]),
    Proc = uuid:uuid4(),
    filelib:ensure_dir("data/"),
    filelib:ensure_dir("data_rcv/"),
    loop(#state{nextpid=NextPid, 
                proc=Proc,
                refs=[],
                keys=[],
                elect=sleep,
                min_cand=Proc}).


loop(S) ->
    receive
        {cmd, Cmd, _} ->
            loop(handlers:handle_cmd(Cmd, S));

        {pack, Msg, Ref} ->
            loop(handlers:handle_msg(Msg, Ref, S));

        {data, Data} ->
            loop(transfer:handle_data(Data, S));
        _ ->
            io:format("Received wrong message type~n",[]),
            loop(S)
        after 1000 ->
            if S#state.elect == leader ->
                   io:format("~p performing sanity check~n", [erlang:self()]),
                   loop(S);
            true ->
                   loop(S)
            end
    end.

terminate(KillerPid, S) ->
    io:format("I'm ~p and I die now. Pls remember~n", [erlang:self()]),
    transfer:send_legacy(S),
    com:change_pid(KillerPid, S#state.nextpid),
    erlang:exit("Received kill signal~").



% -------- Trying a ring topology --------


last_init() ->
    io:format("Initiating first agent\n", []),
    receive
        {pid, Pid} ->
            init(Pid)
    end.


ring_topology(N) ->
    io:format("Initiating a ring network with ~p nodes~n", [N]),
    FirstPid = erlang:spawn(agent, last_init, []),
    Pid = ring_topology(FirstPid, N-1),
    erlang:send(FirstPid, {pid, Pid}),
    {ok, FirstPid}.

ring_topology(NextPid, 0) ->
    NextPid;
ring_topology(NextPid, N) ->
    io:format("Initiating agent ~p\n", [N+1]),
    Pid = erlang:spawn(agent, init, [NextPid]),
    ring_topology(Pid, N-1).



% ------- Distributed ping -----------

ping(Pid) ->
   erlang:send(Pid, {ping, utils:hash()}).

% -------- Join the topology ----------

join(Pid) ->
    com:ask_pid(Pid),
    receive
        {pid, NextPid, _} -> 
            io:format("Received the NextPid ~p for new node~n", [NextPid]),
            com:change_pid(Pid, erlang:self()),
            io:format("Sent changePid instruction to ~p~n", [Pid]),
            init(NextPid)
    end.

new_node(Pid) ->
    P = erlang:spawn(agent, join, [Pid]),
    io:format("Initiated new agent with Pid ~p~n", [P]).

% ------- Destroy all nodes -------

destroy(Platform) ->
    erlang:send(Platform, com:cmd(destroy)).


% -------- Kill a node --------

kill(Platform, Kill) ->
    erlang:send(Platform, com:cmd({kill, Kill})). 


% -------- Start an election --------

start_elect (Pid) ->
    com:send_msg(Pid, start_election).


% -------- Init a network for tests ------

init_network(N) ->
    {Pid, _} = ring_topology(N),
    io:format("~nPassing a ping on the network~n", []),
    ping(Pid),
    Pid.


