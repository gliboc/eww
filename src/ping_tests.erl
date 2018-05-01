-module(ping_tests).
-export([ping_test/1, single_ping_test/1]).

-spec ping_test(N :: integer()) -> atom().
%% @doc Tests the global time for a ping to go through the whole network.
%% The networks used are of size 10*i, for all i lower than N.
ping_test(0) -> ok;
ping_test(N) -> 
    L = ping_test(N, []),
    io:format("~nNodes vs. Ping time(ms)~n"),
    pprint_ping_test(L).

ping_test(0, L) -> L;
ping_test(N, L) ->
    Nb = 10*N,
    {ok, Pid} = agent:ring_topology(Nb),
    T1 = os:timestamp(),
    client:ping(Pid),
    T2 = os:timestamp(),
    T = timer:now_diff(T2,T1),
    agent:destroy(Pid),
    ping_test(N-1, [{Nb, T}] ++ L).

-spec pprint_ping_test (Ping :: [any()]) -> atom().
%% @doc Pretty-prints the statistics in a ping.
pprint_ping_test([]) -> ok;
pprint_ping_test([{Nb, Time} | T]) ->
    io:format("~w  --  ~w~n", [Nb, Time/1000]),
    pprint_ping_test(T).

-spec single_ping_test (Nb :: integer()) -> integer().
%% @doc Tests the ping time of a topology of size N.
single_ping_test(Nb) ->
    {ok, Pid} = agent:ring_topology(Nb),
    client:ping(Pid),
    T1 = os:timestamp(),
    client:ping(Pid),
    T2 = os:timestamp(),
    T = timer:now_diff(T2,T1),
    agent:destroy(Pid),
    T/1000.

