-module(sat_erlang_circle).
-author('labihbc@gmail.com').
-include("sat_erlang.hrl").

-export([init/2, getAABB/1]).

init(Pos, R) ->
	?'#circle'#{pos := Pos, r := R}.

getAABB(#{r := R, pos := Pos}) ->
	Corner = sat_erlang_vector:sub(sat_erlang_vector:init(R, R), sat_erlang_vector:clone(Pos)),
	sat_erlang_box:toPolygon(sat_erlang_box:init(Corner, R * 2, R * 2)).