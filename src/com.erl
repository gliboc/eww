%%% Defines messages shapes and communication functions

-module(com).
-export([msg/1, cmd/1, pid/1, request/2]).
-export([ask_pid/1, change_pid/2, send_msg/2, broadcast/2]).


% ------ Message constructors ----------

msg(Msg) -> {pack, Msg, utils:hash()}.

cmd(Cmd) -> {cmd, Cmd, utils:hash()}.

pid (Pid) -> {pid, Pid, utils:hash()}.

request (Key, Pid) -> {key, Key, Pid, utils:hash()}.

% ------ Communication functions -------

ask_pid(Pid) -> 
    erlang:send(Pid, cmd({sendPid, erlang:self()})).

change_pid (Pid, NewPid) -> 
    erlang:send(Pid, cmd ({changeNextPid, NewPid})).

send_msg (Pid, Msg) ->
    erlang:send(Pid, msg(Msg)).

broadcast (Pid, Msg) ->
    erlang:send (Pid, agent:msg(Msg)).


    
