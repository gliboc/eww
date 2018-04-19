%%% Handlers for messages received by the agents
-module(handlers).
-export([handle_cmd/2, handle_msg/3]).

-include_lib("state.hrl").
-include_lib("ping_info.hrl").


handle_cmd({sendpid, Pid}, S) -> 
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

handle_cmd({changenextpid, NewPid}, S) ->
    S#state{nextpid=NewPid};

handle_cmd({die, Killer}, S) ->
    io:format("I'm ~p and I die now. Pls remember~n", [erlang:self()]),
    com:change_pid(Killer, S#state.nextpid),
    agent:terminate(Killer, S),
    S.



handle_msg(Msg, Ref, S) ->
    io:format("Agent ~p received msg ~p with ref ~p~n", [erlang:self(), Msg, Ref]),
    io:format("Agent ~p has refs ~p~n", [erlang:self(), S#state.refs]),
    case (utils:ref_check(Ref, S)) of
        true -> 
            terminate_msg(Msg, S);
        false ->
            process_msg(Msg, Ref, S#state{refs=S#state.refs++[Ref]})
    end.


process_msg({start_election}, _, S) ->
    com:send_msg(S#state.nextpid, com:msg({elect, S#state.proc})),
    S#state{elect=cand};

process_msg({elect, Proc}, _, S) ->
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

process_msg({req, Key, ClientPid}, _, S) ->
    transfer:retrieve_data({Key, ClientPid}, S);

process_msg({del, Key}, _, S) ->
    transfer:delete_data(Key, S);

process_msg({ping, Ping}, Ref, S) ->
    io:format("Agent ~p was pinged~n", [erlang:self()]),
    Nodes = Ping#ping_info.nb_nodes + 1,
    Refs = Ping#ping_info.nb_refs + erlang:length(S#state.refs),
    Keys = Ping#ping_info.nb_keys + erlang:length(S#state.keys),
    com:send_ping(S#state.nextpid, Ping#ping_info{nb_nodes=Nodes,
                                    nb_refs=Refs,
                                    nb_keys=Keys}, Ref),
    S.


terminate_msg({req, _, ClientPid}, S) ->
    erlang:send(ClientPid, {error, "Key was not found in the network"}),
    S;

terminate_msg({ping, Ping}, S) ->
    io:format("Terminate ping, sending it to ~p~n", [Ping#ping_info.clientpid]),
    C = erlang:send(Ping#ping_info.clientpid, Ping),
    io:format("Sent message ~p~n", [C]),
    S.
