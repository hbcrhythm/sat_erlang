-module(sat_erlang_box).
-author('labihbc@gmail.com').

-include("sat_erlang.hrl").

-export([init/3, toPolygon/1]).

init(Pos, W, H) ->
	?'#box'#{pos := Pos, w := W, h := H}.

toPolygon(#{pos := #{x := X, y := Y}, w := W, h := H}) ->
	Vector = sat_erlang_vector:init(X, Y),
	sat_erlang_polygon:init(Vector, [ sat_erlang_vector:init(0,0), sat_erlang_vector:init(W, 0), sat_erlang_vector:init(W, H), sat_erlang_vector:init(0, H)]).