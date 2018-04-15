%%% Handlers for messages received by the agents
-module(handlers).
-export([handle_cmd/2, handle_msg/3]).

-include_lib("state.hrl").
-include_lib("ping_info.hrl").


handle_cmd({sendPid, Pid}, S) -> 
    erlang:send(Pid, com:pid(S#state.nextpid)),
    S;

handle_cmd({kill, KillPid}, S) when KillPid =/= S#state.nextpid ->
    erlang:send(S#state.nextpid, com:cmd({kill, KillPid})),
    S;

handle_cmd({kill, KillPid}, S) when KillPid =:= S#state.nextpid ->
    erlang:send(S#state.nextpid, com:cmd({die, erlang:self()})),
    S;

handle_cmd(destroy, S) ->
    erlang:send(S#state.nextpid, com:cmd(destroy)),
    erlang:exit("Rcvd destroy signal~n"),
    S;

handle_cmd({changeNextPid, NewPid}, S) ->
    S#state{nextpid=NewPid};

handle_cmd({die, Killer}, S) ->
    io:format("I'm ~p and I die now. Pls remember~n", [erlang:self()]),
    com:change_pid(Killer, S#state.nextpid),
    agent:terminate(Killer, S),
    S.

ref_check(Ref, S) ->
    lists:member(Ref, S#state.hashes).

handle_msg(Msg, Ref, S) ->
    io:format("Agent ~p received msg ~p~n", [erlang:self(), Msg]),
    case ref_check(Ref, S) of
        true -> 
            terminate_msg(Msg, S);
        false -> 
            process_msg(Msg, S)
    end.


process_msg({start_election}, S) ->
    com:send_msg(S#state.nextpid, com:msg({elect, S#state.proc})),
    S#state{elect=cand};

process_msg({elect, Proc}, S) ->
    
    if S#state.elect == cand ->
          
        case Proc =:= S#state.proc of
            true -> 
                case S#state.proc =:= S#state.min_cand of
                    true -> 
                        S#state{elect=leader};
                    false ->
                        S#state{elect=lost}
                end;

            false ->
                com:send_msg(S#state.nextpid, com:msg({elect, Proc})),

                case Proc < S#state.min_cand of 
                    true ->
                        S#state{min_cand=Proc};
                    false ->
                        S
                end
        end;

    true ->
        com:send_msg(S#state.nextpid, com:msg({elect, Proc})),
        
        if S#state.elect == sleep ->
            S#state{elect=lost};
        true ->
            S
        end
    end;

process_msg({req, Key, ClientPid}, S) ->
    transfer:retrieve_data({Key, ClientPid}, S);

process_msg({del, Key}, S) ->
    transfer:delete_data(Key, S);

process_msg({ping, Ping}, S) ->
    io:format("Agent ~p was pinged~n", [erlang:self()]),
    Nodes = Ping#ping_info.nb_nodes + 1,
    Hashes = Ping#ping_info.nb_hashes + lists:lenght(S#state.hashes),
    Keys = Ping#ping_info.nb_keys + lists:length(S#state.keys),
    com:send_ping(S#state.nextpid, 
                  Ping#ping_info{nb_nodes=Nodes,
                                 nb_hashes=Hashes,
                                 nb_keys=Keys}),
    S.


terminate_msg({req, _, ClientPid}, S) ->
    erlang:send(ClientPid, {error, "Key was not found in the network"}),
    S;

terminate_msg({ping, Ping}, S) ->
    erlang:send(Ping#ping_info.clientpid, Ping),
    S.
