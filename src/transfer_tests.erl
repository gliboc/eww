-module(transfer_tests).
-export([pull_test/0, terminate_test/0]).


pull_test () ->
    io:format("This program tests the ability to store data
    on a remote node, and then retrieve it using the UUID the
    node provided~n~n", []),
    
    {_, Pid} = agent:ring_topology(4),
    transfer:read_and_send("3648.mp4", Pid),
    Key = receive 
        {pack, {key, K}, _} -> K;
        _ -> error
    end,
    client:pull("test-file", Key, Pid),
    
    io:format("File was written in 'data_rcv/test-file', please
    check that it is identical to '3648.mp4'~n", []).


terminate_test () ->
    io:format("This program tests the ability of a node killed
    remotely to pass its data and keys to the next node. Data should
    still be accessible by querying the network with the same key.~n",[]),
    
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
    
