%%% Defines messages shapes and communication functions

-module(com).
-export([msg/1, cmd/1, pid/1, ping/1, data_request/2, del_request/1]).
-export([ask_pid/1, change_pid/2, send_msg/2, send_ping/2, broadcast/2]).


% ------ Message constructors ----------

msg(Msg) -> {pack, Msg, utils:hash()}.
cmd(Cmd) -> {cmd, Cmd, utils:hash()}.
ping (Ping) -> {ping, Ping, utils:hash()}.
pid (Pid) -> {pid, Pid, utils:hash()}.

data_request (Key, ClientPid) -> msg ({req, Key, ClientPid}).

del_request (Key) -> msg ({del, Key}).

% ------ Communication functions -------

ask_pid(Pid) -> 
    erlang:send(Pid, cmd({sendpid, erlang:self()})).

change_pid (Pid, NewPid) -> 
    erlang:send(Pid, cmd ({changenextpid, NewPid})).

send_msg (Pid, Msg) ->
    erlang:send(Pid, msg(Msg)).

send_ping (Pid, Ping) ->
    erlang:send(Pid, ping(Ping)).

broadcast (Pid, Msg) ->
    erlang:send (Pid, agent:msg(Msg)).


    
