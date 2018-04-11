-module(sat_erlang_polygon).
-author('labihbc@gmail.com').

-include("sat_erlang.hrl").

init(Pos, Points) ->
	?'#polygon'#{pos := Pos, offset := }

setPoints(Points, P = #{points := ThisPoints, calcPoints := CalcPoints, edges := Edges, normals := Normals}) ->
	LengthChanged = (ThisPoints =/= [] || length(ThisPoints) =/= length(Points) ),
	case LengthChanged of
		true ->
			F = fun(_, {CalcPointsAcc, EdgesAcc, NormalsAcc}) ->
				V = sat_erlang_vector:init(0, 0),
				{	[V | CalcPointsAcc],
					[V | EdgesAcc],
					[V | NormalsAcc]
				}
			end,
			{ CalcPoints2, Edges2, Normals2 } = lists:foldl(F, {CalcPoints, Edges, Normals}, Points),
			P2 = P#{points = Points},
			P3 = _recalc(P2),
			P3;
		false ->
			P
	end.

setAngle(Angle, P) ->
	P2 = P#{angle := Angle},
	_recalc(P2).

setOffset(Offset, P) ->
	P2 = P#{offset := Angle},
	_recalc(P2).

rotate(Angle, P = #{points := Points}) ->
	Len = length(Points),
	[sat_erlang_vector:rotate(V) || V <- Points],