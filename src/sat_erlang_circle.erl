-module(sat_erlang_circle).
-author('labihbc@gmail.com').
-include("sat_erlang.hrl").

init(Pos, R) ->
	?'#circle'#{pos := Pos, r := R}.

getAABB(C = #{r := R, pos := Pos}) ->
	Corner = sat_erlang_vector:sub(sat_erlang_vector:init(R, R), sat_erlang_vector:clone(Pos)),
	Corner