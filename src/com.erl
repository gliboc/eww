%%% Defines messages shapes and communication functions

-module(com).
-export([msg/1, cmd/1, pid/1, data/1, data_request/2, del_request/2]).
-export([ask_pid/1, change_pid/2, send_msg/2, send_ping/3, broadcast/2, send_msg/3,
         send_cmd/2, send_cmd/3, send_pid/2, send_data/3, send_data/4]).


% ------ Message constructors ----------

msg(Msg) -> {pack, Msg, utils:hash()}.
cmd(Cmd) -> {cmd, Cmd, utils:hash()}.
pid (Pid) -> {pid, Pid, utils:hash()}.
data(Data) -> {data, Data}.

data_request (Key, ClientPid) -> msg ({req, Key, ClientPid}).
del_request (Key, Ref) -> {pack, {del, Key}, Ref}.

% ------ Communication functions -------

ask_pid(Pid) -> 
    erlang:send(Pid, cmd({sendpid, erlang:self()})).

change_pid (Pid, NewPid) -> 
    erlang:send(Pid, cmd ({changenextpid, NewPid})).

send_cmd(Pid, Cmd) ->
    erlang:send(Pid, cmd(Cmd)).

send_cmd(Pid, Cmd, Ref) ->
    erlang:send(Pid, {cmd, Cmd, Ref}).

send_data(Pid, Data, Size) ->
    com:send_msg(Pid, {store_request, Size, erlang:self()}),
    receive
        ok -> erlang:send(Pid, {data, Data});
        {not_enough_memory, NewPid, Ref} ->
            send_data(NewPid, Data, Size, Ref)
    end.

send_data(Pid, Data, Size, Ref) ->
    com:send_msg(Pid, {store_request, Size, erlang:self()}, Ref),
    receive 
        ok ->
            erlang:send(Pid, {data, Data});
        {not_enough_memory, NewPid, Ref} ->
            send_data(NewPid, Data, Size, Ref)
    end.

send_msg (Pid, Msg) ->
    erlang:send(Pid, msg(Msg)).

send_msg(Pid, Msg, Ref) ->
    erlang:send(Pid, {pack, Msg, Ref}).

send_pid(Target, Pid) ->
    erlang:send(Target, com:pid(Pid)).

send_ping (Pid, Ping_Infos, Ref) ->
    erlang:send(Pid, {pack, {ping, Ping_Infos}, Ref}).

broadcast (Pid, Msg) ->
    erlang:send (Pid, msg({bcast, Msg})).
