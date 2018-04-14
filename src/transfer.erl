% 
% This module implements data transfer protocols,
%

-module(transfer).
-compile(export_all).

-include_lib("state.hrl").

simple_send (Pid, Binary) -> 
    erlang:send (Pid, {nosig, Binary, erlang:phash2(Binary), erlang:self()}).

signed_send (Pid, Binary, Key) ->
    erlang:send (Pid, {sig, Binary, erlang:phash2(Binary), Key, erlang:self()}). 

handle_data ({nosig, Binary, Hash, Pid}, S) ->
    case erlang:phash2(Binary) =:= Hash of
        true -> 
            io:format ("Writing file~n"),
            Key = uuid:to_string(uuid:uuid4()),
            file:write_file (Key ++ ".dat", Binary),
            io:format ("Done writing~n"),
            com:send_msg(Pid, {key, Key}),
            S#state{keys=S#state.keys ++ [Key]};
        false -> 
            io:format ("Wrong hash, ignoring file send", []),
            S
    end;

handle_data ({sig, Binary, Hash, Key, _}, S) ->
    case erlang:phash2(Binary) =:= Hash of
        true ->
            file:write_file (Key ++ ".dat", Binary),
            S#state{keys=S#state.keys ++ [Key]};
        false ->
            S
    end.


retrieve_data ({Key, Pid}, S) ->
    case lists:mem(Key, S#state.keys) of
        true ->
            read_and_send (Key ++ ".dat", Pid),
            S;
        false ->
            com:send_msg (S#state.nextpid, com:data_request(Key, Pid)),
            S
    end.

delete_data (Key, S) ->
    com:send_msg (S#state.nextpid, com:del_request(Key)),
    case lists:mem (Key, S#state.keys) of
        true ->
            Filename = Key ++ ".dat",
            file:delete(Filename),
            S#state{keys=lists:delete(Key, S#state.keys)};
        false ->
            S
    end.


read_and_send (Filename, Pid) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            simple_send(Pid, Binary);
        {error, Reason} ->
            exit(Reason)
    end.

read_and_send (Filename, Pid, Key) ->
    case file:read_file (Filename) of
        {ok, Binary} ->
            signed_send (Pid, Binary, Key);
        {error, Reason} ->
            exit (Reason)
    end.

% ---------------- Data preservation --------------------
% When a node is killed, it transfers its data to its peer

send_data_list ([], _) -> ok;
send_data_list ([Key | T], Pid) ->
    read_and_send (Key ++ ".dat", Pid, Key),
    send_data_list (T, Pid).

send_legacy (S) ->
    send_data_list (S#state.keys, S#state.nextpid).
