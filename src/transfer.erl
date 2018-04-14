% 
% This module implements data transfer protocols,
% such as the window sliding protocol.
%

-module(transfer).
-compile(export_all).

simple_send (Pid, Binary) -> 
    erlang:send (Pid, {Binary, erlang:phash2(Binary), erlang:self()}).

simple_receive () ->
    receive
        {Binary, Hash, Pid} ->
            case erlang:phash2(Binary) =:= Hash of
                true -> 
                    io:format ("Writing file~n"),
                    file:write_file ("test-" ++ integer_to_list(Hash) ++ ".dat", Binary),
                    io:format ("Done writing~n");
                false -> 
                    io:format ("Wrong hash, ignoring file send", [])
            end
    end,
    io:format("Re-launching receive~n"),
    simple_receive ().

read_and_send (Filename, Pid) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            simple_send(Pid, Binary);
        {error, Reason} ->
            exit(Reason)
    end.



