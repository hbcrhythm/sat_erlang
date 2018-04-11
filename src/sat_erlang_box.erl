-module(sat_erlang_box).
-author('labihbc@gmail.com').

-include("sat_erlang.hrl").

init(Pos, W, H) ->
	?'#box'#{pos := Pos, w := W, h := H}.

toPolygon() ->
	