%% Agent to be spawn on the nodes

%% @doc Module containing the functions to create and manage agents, as well as creating a platform.
-module(agent).
-export([init/1, last_init/0, loop/1, terminate/2]).
-export([ring_topology/1, join/1, new_node/1, 
         destroy/1, kill/2, start_elect/1]).

-include_lib("state.hrl").

-spec init(NextPid :: pid()) -> #state{}.
%% @doc Initializes an agent.
init(NextPid) -> 
    io:format("Agent ~p was succesfully started~n", [erlang:self()]),

    filelib:ensure_dir("data/"),
    filelib:ensure_dir("data_rcv/"),

    Proc = uuid:uuid4(),
    loop (#state{nextpid=NextPid, 
                proc=Proc,
                elect=sleep,
                min_cand=Proc}).

-spec loop(S::#state{}) -> no_return().
%% @doc Event-handling loop for the agent.
loop(S) ->
    receive
        {cmd, Cmd, _} ->
            loop(handlers:handle_cmd(Cmd, S));

        {pack, Msg, Ref} ->
            loop(handlers:handle_msg(Msg, Ref, S));

        {data, Data} ->
            loop(transfer:handle_data(Data, S));

        _ ->
            io:format("wrong message type~n",[]),
            loop(S)
    
        after 1000 ->

            if S#state.elect == leader ->
                   io:format("~p performing sanity check~n", [erlang:self()]),
                   loop(S);
            true ->
                   loop(S)
            end
    end.

-spec terminate(KillerPid :: pid(), S :: #state{}) -> no_return().
%% @doc Quietly terminates the agent, transferring its data to its peer.
terminate(KillerPid, S) ->
    io:format("I'm ~p and I die now. Pls remember~n", [erlang:self()]),
    io:format("Sending data and keys to ~p~n", [S#state.nextpid]), 

    transfer:send_legacy(S),
    io:format("Legacy was delivered~n"),
    com:change_pid(KillerPid, S#state.nextpid),
    erlang:exit(received_kill_signal).



% -------- Trying a ring topology --------

last_init() ->
    io:format("Initiating first agent\n", []),
    receive
        {pid, Pid} ->
            init(Pid)
    end.

-spec ring_topology (N :: integer()) -> {atom(), pid()}.
%% @doc Creates a ring-shaped topology of agents of length N.
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



% -------- Join the topology ----------

-spec join(Platform :: pid()) -> no_return().
%% @doc Join a topology as an agent.
join(Platform) ->
    com:ask_pid(Platform),
    receive
        {pid, NextPid, _} -> 
            io:format("Received the NextPid ~p for new node~n", [NextPid]),
            com:change_pid(Platform, erlang:self()),
            io:format("Sent changePlatform instruction to ~p~n", [Platform]),
            init(NextPid)
    end.

-spec new_node (Platform :: pid()) -> {atom(), pid()}.
%% @doc Spawn an agent and have it join a topology.
new_node(Platform) ->
    NewAgentPid = erlang:spawn(agent, join, [Platform]),
    {ok, NewAgentPid}.

% ------- Destroy all nodes -------

-spec destroy (Platform :: pid()) -> no_return().
%% @doc Asks a platform to self-destruct.
destroy(Platform) ->
    erlang:send(Platform, com:cmd(destroy)).


% -------- Kill a node --------

-spec kill (Platform :: pid(), Target :: pid()) -> no_return().
%% @doc Asks a platform to kill a specific target agent.
kill(Platform, Target) ->
    erlang:send(Platform, com:cmd({kill,Target, erlang:self()})). 


% -------- Start an election --------

-spec start_elect (Pid :: pid()) -> no_return().
%% @doc Asks an agent to start a leader election.
start_elect (Pid) ->
    com:send_msg(Pid, start_election).
