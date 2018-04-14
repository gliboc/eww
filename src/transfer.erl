% 
% This module implements data transfer protocols,
%

-module(transfer).
-compile(export_all).

-include_lib("state.hrl").

simple_send (Pid, Binary) -> 
    erlang:send (Pid, {Binary, erlang:phash2(Binary), erlang:self()}).

handle_data ({Binary, Hash, Pid}, S) ->
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
    end.

retrieve_data ({Key, Pid}, S) ->
    case lists:mem(Key, S#state.keys) of
        true ->
            read_and_send (Key ++ ".dat", Pid),
            S#state{keys=lists:delete(Key,S#state.keys)};
        false ->
            com:send_msg (S#state.nextpid, com:request(Key, Pid)),
            S
    end.


read_and_send (Filename, Pid) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            simple_send(Pid, Binary);
        {error, Reason} ->
            exit(Reason)
    end.




