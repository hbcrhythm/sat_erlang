-module(sat_erlang_response).
-author('labihbc@gmail.com').

-include("sat_erlang.hrl").

-export([init/0, clear/1]).

init() ->
	?'#response'.

clear(R) ->
	R#{aInB => true, bIna := true, overlap := 1 bsl 32}.

