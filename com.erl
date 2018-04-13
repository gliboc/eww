-module(com).

broadcast (Pid, Msg) ->
    erlang:send (Pid, agent:msg(Msg)).


    
