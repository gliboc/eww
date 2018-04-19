% ------- Utility ----------
-module(utils).
-export([hash/0, ref_check/2]).

-include_lib("state.hrl").

hash() -> erlang:phash2({erlang:self(), erlang:system_time()}).

ref_check(Ref, S) ->
    lists:member(Ref, S#state.refs).
