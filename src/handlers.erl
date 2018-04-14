%%% Handlers for messages received by the agents
-module(handlers).
-export([handle_cmd/2, handle_msg/3, handle_ping/2]).

-include_lib("state.hrl").

handle_cmd ({sendPid, Pid}, S) -> 
    erlang:send(Pid, com:pid (S#state.nextpid)),
    S;

handle_cmd ({kill, KillPid}, S) when KillPid =/= S#state.nextpid ->
    erlang:send (S#state.nextpid, com:cmd ({kill, KillPid})),
    S;

handle_cmd ({kill, KillPid}, S) when KillPid =:= S#state.nextpid ->
    erlang:send (S#state.nextpid, com:cmd ({die, erlang:self()})),
    S;

handle_cmd (destroy, S) ->
    erlang:send (S#state.nextpid, com:cmd (destroy)),
    erlang:exit ("Rcvd destroy signal~n"),
    S;

handle_cmd ({changeNextPid, NewPid}, S) ->
    S#state{nextpid=NewPid};

handle_cmd ({die, Killer}, S) ->
    io:format ("I'm ~p and I die now. Pls remember~n", [erlang:self()]),
    com:change_pid (Killer, S#state.nextpid),
    erlang:exit("Reiceved kill signal~n"),
    S.


handle_msg (Msg, Id, S) ->
    io:format("Agent ~p received msg ~p~n", [erlang:self(), Msg]),
    case (lists:member (Id, S#state.hashes)) of
        true -> S;
        false -> 
            io:format("Agent ~p passing around message ~p to ~p~n", [erlang:self(), Msg, S#state.nextpid]),
            erlang:send(S#state.nextpid, {pack, Msg, Id}),
            S#state{hashes=S#state.hashes ++ [Id]}
    end.


handle_ping (Id, S) ->
    case (lists:member (Id, S#state.hashes)) of
        true -> 
            S;
        false -> 
            io:format ("Agent ~p was pinged~n", [erlang:self()]),
            erlang:send(S#state.nextpid, {ping, Id}),
            S#state{hashes=S#state.hashes ++ [Id]}
    end.

