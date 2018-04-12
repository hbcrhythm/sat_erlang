-module(sat_erlang_vector).
-author('labihbc@gmail.com').
-include("sat_erlang.hrl").

-export([init/2, copy/2, clone/1, perp/1, rotate/2, reverse/1, normalize/1, add/2, sub/2, scale/3, project/2, projectN/2, reflect/2, reflectN/2, dot/2, len2/1, len/1]).

init(X, Y) ->
	?'#vector'#{x := X, y := Y}.

copy(#{x := X, y := Y}, V) ->
	V#{x := X, y := Y}.

clone(#{x := X, y := Y}) ->
	?'#vector'#{x := X, y := Y}.

perp(V = #{x := X, y := Y}) ->
	V#{x := Y, y := -X}.

rotate(Angle, V = #{x := X, y := Y}) ->
	V#{x := (X * math:cos(Angle) - Y * math:sin(Angle)), y := (X * math:sin(Angle) + Y * math:cos(Angle) ) }.

reverse(V = #{x := X, y := Y}) ->
	V#{x := -X, y := -Y}.

normalize(V = #{x := X, y := Y}) ->
	D = len(V),
	{NewX, NewY} = case D > 0 of
		true ->
			{X / D , Y / D};
		false ->
			{X, Y}
	end,
	V#{x := NewX, y := NewY}.

add(#{x := AX, y := AY} , V = #{x := BX, y := BY}) ->
	V#{ x := AX + BX, y := AY + BY}.

sub(#{x := AX, y := AY} , V = #{x := BX, y := BY}) ->
	V#{ x := BX - AX, y := BY - AY}.

scale(X, Y, VB = #{x := BX, y := BY}) ->
	Y2 = case Y == 0 of
		true -> X;
		false -> Y
	end,
	VB#{x := X * BX , y := BY * Y2}.

project(VA = #{x := AX, y := AY}, VB) ->
	Amt = dot(VA, VB) / len2(VA),
	VB#{x := Amt * AX, y := Amt * AY}.

projectN(VA = #{x := AX, y := AY}, VB) ->
	Amt = dot(VA, VB),
	VB#{x := Amt * AX, y := Amt * AY}.

reflect(Axis, VB = #{x := X, y := Y}) ->
	VB2 = #{x := X2, y := Y2} = scale(2, 0, project(Axis, VB)),
	VB2#{x := X2 - X, y := Y2 - Y}.

reflectN(Axis, VB = #{x := X, y := Y}) ->
	VB2 = #{x := X2, y := Y2} = scale(2, 0, project(Axis, VB)),
	VB2#{x := X2 - X, y := Y2 - Y}.


dot(#{x := AX, y := AY} , #{x := BX, y := BY}) ->
	AX * BX + AY * BY.

len2(V) ->
	dot(V, V).	

len(V) ->
	math:sqrt(len2(V)).

