-module(transfer_tests).
-export([pull_test/0, terminate_test/0, release_test/0]).
-include_lib("stdlib/include/assert.hrl").

%% @doc This program tests the ability to store data on a remote node, 
%% @doc and then retrieve it using the UUID the node provided.
pull_test () ->
    {_, Pid} = agent:ring_topology(4),
    transfer:read_and_send("3648.mp4", Pid),
    Key = receive 
        {pack, {key, K}, _} -> K;
        _ -> error
    end,
    client:pull("test-file", Key, Pid),
    
    io:format("File was written in 'data_rcv/test-file', please
    check that it is identical to '3648.mp4'~n", []).


%% @doc This program tests the ability of a node killed
%% @doc remotely to pass its data and keys to the next node. Data should
%% @doc still be accessible by querying the network with the same key.
terminate_test () ->
    {_, Pid} = agent:ring_topology(6),
    transfer:read_and_send("3648.mp4", Pid),
    Key = receive 
        {pack, {key, K}, _} -> K;
        _ -> error
    end,
    com:ask_pid(Pid),
    NPid = receive
        {pid, NextPid, _} ->
            NextPid
    end,
    agent:kill(NPid, Pid),
    Success = receive
        ok -> ok;
        _ -> not_ok
    end,
    case Success of
        ok -> client:pull("term-test", Key, NPid);
        not_ok -> {error, kill_op_failed}
    end.
   
%% @doc Tests the release command.
release_test () ->

    {_, Pid} = agent:ring_topology(6),
    transfer:read_and_send("3648.mp4", Pid),
    Key = receive
         {pack, {key, K}, _} -> K;
         _ -> error
    end,

    client:release(Key, Pid),
    ?assert({error, key_not_found} == client:pull("shouldnwork", Key, Pid)).
