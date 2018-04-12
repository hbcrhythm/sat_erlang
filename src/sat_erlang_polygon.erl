-module(sat_erlang_polygon).
-author('labihbc@gmail.com').

-include("sat_erlang.hrl").

-export([init/2, setPoints/2, setAngle/2, setOffset/2, rotate/2, translate/3, recalc/1, getAABB/1, getCentroid/1]).

init(Pos, Points) ->
	P = ?'#polygon'#{pos := Pos},
	setPoints(Points, P).

setPoints(Points, P = #{points := ThisPoints, calcPoints := CalcPoints, edges := Edges, normals := Normals}) ->
	LengthChanged = (ThisPoints =/= [] orelse length(ThisPoints) =/= length(Points) ),
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
			P2 = P#{points := Points, calcPoints := CalcPoints2, edges := Edges2, normals := Normals2},
			P3 = recalc(P2),
			P3;
		false ->
			P
	end.

setAngle(Angle, P) ->
	P2 = P#{angle := Angle},
	recalc(P2).

setOffset(Offset, P) ->
	P2 = P#{offset := Offset},
	recalc(P2).

rotate(Angle, P = #{points := Points}) ->
	Points2 = [sat_erlang_vector:rotate(Angle, V) || V <- Points],
	P2 = P#{points := Points2},
	recalc(P2).

translate(X, Y, P = #{points := Points}) ->
	Points2 = [Point#{x := PX + X, y := PY + Y} || Point =  #{x := PX, y := PY} <- Points],
	P2 = P#{points := Points2},
	recalc(P2).


recalc(P = #{
	calcPoints := CalcPoints
	% ,edges := Edges 
	% ,normals := Normals 
	,points := Points
	,offset := Offset
	,angle := Angle }) ->
	
	Len = length(Points),

	CalcPoints2 = recalc_calc_point(Points, CalcPoints, Angle, Offset),

	F = fun(I, {EdgesAcc, NormalsAcc}) ->
		P2 = case I =:= Len of
			true ->
				lists:nth(1, CalcPoints2);
			false ->
				lists:nth(I + 1, CalcPoints2)
		end,
		P1 = lists:nth(I, CalcPoints2),
		Edge = sat_erlang_vector:sub(P1, P2),
		% EdgesAcc2 = lists:keystore(I, 1, EdgesAcc, Edge),
		Normal = sat_erlang_vector:normalize(sat_erlang_vector:perp(Edge)),
		% NormalsAcc2 = lists:keystore(I, 1, NormalsAcc, Normal),
		{[Edge | EdgesAcc], [Normal | NormalsAcc]}
	end,
	{Edges2, Normals2} = lists:foldl(F, {[], []}, lists:seq(1, Len)),
	P#{calcPoints := CalcPoints2, edges := lists:reverse(Edges2), normals := lists:reverse(Normals2)}.
	
recalc_calc_point(Points, CalcPoints, Angle, Offset) ->
	recalc_calc_point(Points, CalcPoints, Angle, Offset, []).

recalc_calc_point([Point | T], [CalcPoint | CT], Angle, Offset = #{x := OffsetX, y := OffsetY}, Acc) ->
	CalcPoint2 = #{x := X, y := Y} = sat_erlang_vector:copy(Point, CalcPoint),
	CalcPoint3 = CalcPoint2#{x := X + OffsetX, y := Y + OffsetY},
	CalcPoint4 = case Angle =/= 0 of
		true ->
			sat_erlang_vector:rotate(Angle, CalcPoint3);
		false ->
			CalcPoint3
	end,
	recalc_calc_point(T, CT, Angle, Offset, [CalcPoint4 | Acc]);

recalc_calc_point([], [], _, _, Acc) -> lists:reverse(Acc).


% recalc_edges_normals([CalcPoint, CalcPoint2 | T], FirstCalcPoint, [Edge | EdgeT], [Normal | NormalT], EdgesAcc, NormalsAcc) ->
% 	E = sat_erlang_vector:sub(CalcPoint, sat_erlang_vector:copy(CalcPoint2, Edge)),
% 	Normal2 = sat_erlang_vector:normalize(sat_erlang_vector:perp( sat_erlang_vector:copy(E, Normal)) ),

% 	recalc_edges_normals(T, FirstCalcPoint, EdgeT, NormalT, [E | EdgesAcc], [Normal2 | NormalsAcc]);

% recalc_edges_normals([CalcPoint | T], FirstCalcPoint, [Edge | EdgeT], [Normal | NormalT], EdgesAcc, NormalsAcc) ->
% 	E = sat_erlang_vector:sub(CalcPoint, sat_erlang_vector:copy(FirstCalcPoint, Edge)),
% 	Normal2 = sat_erlang_vector:normalize(sat_erlang_vector:perp( sat_erlang_vector:copy(E, Normal)) ),
% 	recalc_edges_normals(T, FirstCalcPoint, EdgeT, NormalT, [E | EdgesAcc], [Normal2 | NormalsAcc]);

% recalc_edges_normals([], _, _, _, EdgesAcc, NormalsAcc) ->
% 	{lists:reverse(EdgesAcc), lists:reverse(NormalsAcc) }.

getAABB(#{pos := Pos, calcPoints := CalcPoints}) ->
	F = fun(#{x := X, y := Y}, {0,0,0,0}) ->
				{X, Y, X, Y};
			(#{x := X, y := Y}, {XMin, YMin, XMax, YMax}) ->
				XMin2 = min(X, XMin),
				XMax2 = max(X, XMax),
				YMin2 = min(Y, YMin),
				YMax2 = max(Y, YMax),
				{XMin2, YMin2, XMax2, YMax2}
	end,
	{XMin3, YMin3, XMax3, YMax3} = lists:foldl(F, {0, 0, 0, 0}, CalcPoints),
	sat_erlang_box:toPolygon( sat_erlang_box:init(sat_erlang_vector:add(sat_erlang_vector:init(XMin3, YMin3), sat_erlang_vector:clone(Pos) ), XMax3 - XMin3, YMax3 - YMin3) ).

getCentroid(#{calcPoints := CalcPoints = [FirstCalcPoint | _]}) ->
	getCentroid(CalcPoints, FirstCalcPoint, {0, 0, 0}).

getCentroid([ #{x := CPX, y := CPY}, #{x := CP2X, y := CP2Y} | T], FirstCalcPoint, {Ar, Cx, Cy}) ->
	A = CPX * CP2Y - CP2X * CPY,
	Cx2 = (CPX + CP2X) * A + Cx,
	Cy2 = (CPY + CP2Y) * A + Cy,
	Ar2 = Ar + A,
	getCentroid(T, FirstCalcPoint, {Ar2, Cx2, Cy2});

getCentroid([ #{x := CPX, y := CPY}], #{x := CP2X, y := CP2Y}, {Ar, Cx, Cy}) ->
	A = CPX * CP2Y - CP2X * CPY,
	Cx2 = (CPX + CP2X) * A + Cx,
	Cy2 = (CPY + CP2Y) * A + Cy,
	Ar2 = Ar + A,

	Ar3 = Ar2* 3,
	Cx3 = Cx2 / Ar3,
	Cy3 = Cy2 / Ar3,
	sat_erlang_vector:init(Cx3, Cy3).
	