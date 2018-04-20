% 
% This module implements data transfer protocols,
%

-module(transfer).
-export([simple_send/2, signed_send/3]).
-export([handle_data/2, retrieve_data/3, delete_data/2, send_legacy/1,
        receive_data/0]).
-export([read_and_send/2, read_and_send/3]).

-include_lib("state.hrl").

receive_data () ->
    receive 
        {data, {nosig, Binary, Hash, _}} -> {ok, {Binary, Hash}};
        Thing -> {error, {wrong_type_data, Thing}} 
    end.

simple_send (Pid, Binary) -> 
    erlang:send (Pid, com:data({nosig, Binary, erlang:phash2(Binary), erlang:self()})).

signed_send (Pid, Binary, Key) ->
    io:format("Sending signed data to ~p~n", [Pid]),

    erlang:send (Pid, com:data({sig, Binary, erlang:phash2(Binary), Key, erlang:self()})). 


handle_data ({nosig, Binary, Hash, Pid}, S) ->
    case erlang:phash2(Binary) =:= Hash of
        true -> 
            io:format ("Writing file~n"),
            Key = uuid:to_string(uuid:uuid4()),
            file:write_file ("data/" ++ Key ++ ".dat", Binary),
            io:format ("Done writing~n"),
            com:send_msg(Pid, {key, Key}),
            S#state{keys=S#state.keys ++ [Key]};
        false -> 
            io:format ("Wrong hash, ignoring file send", []),
            S
    end;

handle_data ({sig, Binary, Hash, Key, _}, S) ->
    io:format("~p received signed data~n", [erlang:self()]),

    case erlang:phash2(Binary) =:= Hash of
        true ->
            file:write_file ("data/" ++ Key ++ ".dat", Binary),

            io:format("wrote file to memory~n",[]),
            io:format("now adding key ~p to ~p set of keys~n", [Key, erlang:self()]),
            S#state{keys=S#state.keys ++ [Key]};

        false ->
            S
    end.


retrieve_data ({Key, Pid}, Ref, S) ->
    case lists:member(Key, S#state.keys) of
        true ->
            read_and_send ("data/" ++ Key ++ ".dat", Pid),
            S;
        false ->
            erlang:send(S#state.nextpid, {pack, {req, Key, Pid}, Ref}),
            S
    end.

delete_data (Key, S) ->
    com:send_msg (S#state.nextpid, com:del_request(Key)),
    case lists:member(Key, S#state.keys) of
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
    io:format("reading file...~n", []),
    case file:read_file (Filename) of
        {ok, Binary} ->
            io:format("Succesful read~n",[]),
            signed_send (Pid, Binary, Key);
        {error, Reason} ->
            {error, Reason}
    end.

% ---------------- Data preservation --------------------
% When a node is killed, it transfers its data to its peer

send_data_list ([], _) -> ok;
send_data_list ([Key | T], Pid) ->
    io:format("Sending file with key ~p to ~p~n", [Key, Pid]),

    read_and_send ("data/" ++ Key ++ ".dat", Pid, Key),
    send_data_list (T, Pid).


send_legacy (S) ->
    send_data_list (S#state.keys, S#state.nextpid).
