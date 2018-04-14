% 
% This module implements data transfer protocols,
%

-module(transfer).
-compile(export_all).

-include_lib("state.hrl").

simple_send (Pid, Binary) -> 
    erlang:send (Pid, {Binary, erlang:phash2(Binary), erlang:self()}).

handle_data ({Binary, Hash}, S) ->
    case erlang:phash2(Binary) =:= Hash of
        true -> 
            io:format ("Writing file~n"),
            file:write_file ("test-" ++ integer_to_list(Hash) ++ ".dat", Binary),
            io:format ("Done writing~n"),
            S#state{keys=S#state.keys ++ [Hash]};
        false -> 
            io:format ("Wrong hash, ignoring file send", []),
            S
    end.

read_and_send (Filename, Pid) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            simple_send(Pid, Binary);
        {error, Reason} ->
            exit(Reason)
    end.




