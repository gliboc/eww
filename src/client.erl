-module(client).
-export([start/2, stop/1, push/2, pull/3, ping/1]).
-export([release/2, deploy/1]).
-export([give_status/1]).

-include_lib("ping_info.hrl").


start (Host, Platform) ->
    erlang:spawn (Host, agent, join, [Platform]).


stop (Node) ->
    erlang:send (Node, {die, erlang:self()}),
    receive
        {cmd, {_, Pid}, _} ->
            Pid
    end.


push (Filename, Platform) ->
    {ok, Binary} = file:read_file(Filename),
    transfer:simple_send(Platform, Binary),
    receive 
        {pack, {key, UUID}, _} ->
            UUID
    end.


pull (Name, UUID, Platform) ->
    com:send_msg(Platform, {req, UUID, erlang:self()}),
    {Status, Data} = transfer:receive_data (),

    case {Status, Data} of
        {error, _} -> {error, Data};

        {ok, {Binary, Hash}} -> 
            Filename = "data_rcv/" ++ Name,
            io:format("Writing file ~p~n", [Filename]),
            case erlang:phash2(Binary) =:= Hash of
                true ->
                    file:write_file(Filename, Binary);
                false ->
                    {error, wrong_or_corrupted_file}
            end;

        {ok, _} -> {error, received_wrong_data_type}
    end.


release (UUID, Platform) ->
    erlang:send(Platform, {pack, {del, UUID}, utils:hash()}).


init_ping () ->
    #ping_info{clientpid=erlang:self(),
               nb_nodes=0,
               nb_refs=0,
               nb_keys=0,
               keys=[]}.


ping (Platform) ->
    Ping = init_ping(),
    com:send_ping(Platform, Ping, utils:hash()),
    receive
        FinalPing ->
            io:format("Ping made it through the ring~n",[]),
            pprint_ping(FinalPing),
            FinalPing
    after 1000 ->
        {error, no_response, erlang:self()}
    end.


pprint_ping({ping_info, CPid, NbNodes, NRefs, Keys, NKeys, _, _}) ->
    io:format
    ("=======================================================
== Agent ~p asked for a report on the network ===~n=======================================================
    Keys stored: ~p
    Number of keys stored: ~p
    Number of nodes: ~p
    Number of messages exchanged: ~p~n",
    [CPid, Keys, NKeys, NbNodes, NRefs]).
                


deploy ([H | T]) ->
    io:format("Deploying a platform on nodes TODO~n"),
    FirstPid = erlang:spawn(H, ?MODULE, last_init, []),
    Pid = deploy(FirstPid, T),
    erlang:send(FirstPid, {pid, Pid}).

deploy (NextPid, []) -> NextPid;
deploy (NextPid, [H | T]) ->
    io:format("Starting an agent on ~p~n", [H]),
    Pid = erlang:spawn(H, agent, init, [NextPid]),
    deploy (Pid, T).




% ------- Misc functions; for testing purposes --------

give_status (Platform) ->
    com:send_msg(Platform, give_status).
