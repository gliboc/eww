% ------- Utility ----------
-module(utils).
-export([hash/0]).

hash() -> erlang:phash2({erlang:self(), now()}).


