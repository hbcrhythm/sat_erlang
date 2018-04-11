-module(sat_erlang_vector).

-include("sat_erlang.hrl").

-export([copy/2, clone/1, perp/1, rotate/2]).

copy(#{x := X, y := Y}, V) ->
	V#{x := X, y := Y}.

clone(#{x := X, y := Y}) ->
	?'#vector'#{x := X, y := Y}.

perp(V = #{x := X, y := Y}) ->
	V#{x := Y, y := -X}.

rotate(Angle, V = #{x := X, y := Y}) ->
	V#{x := (X * math:cos(Angle) - Y * math:sin(Angle)), y := (X * math:sin(Angle) + Y * math:cos(Angle) ) }.

