-module(sat_erlang).
-author('labihbc@gmail.com').

-include("sat_erlang.hrl").

%% API exports
-export([testCircleCircle/2, testPolygonCircle/2, testCirclePolygon/2, testPolygonPolygon/2]).

%%====================================================================
%% API functions
%%====================================================================

testCircleCircle(A = #{pos := APos, r := AR}, B = #{pos := #{x := BX, y := BY}, r := BR}) ->
	DifferenceV = sat_erlang_vector:sub(APos, sat_erlang_vector:init(BX, BY)),
	TotalRadius = AR + BR,
	TotalRadiusSq = TotalRadius * TotalRadius,
	DistanceSq = sat_erlang_vector:len2(DifferenceV),
	Responese = ?'#response',

	case DistanceSq > TotalRadiusSq of
		true ->
			Responese#{isCollide := false};
		false ->
			Dist = math:sqrt(DistanceSq),
			OverlapN = sat_erlang_vector:normalize(DifferenceV),
			Responese#{a := A, b := B, overlap := TotalRadius - Dist, 
			overlapN := OverlapN, overlapV := sat_erlang_vector:scale(TotalRadius - Dist, 0, OverlapN),
			aInB := (AR =< BR ) andalso Dist =< (BR - AR),
			bInA := (BR =< AR ) andalso Dist =< (AR - BR),
			isCollide := true}
	end.

testPolygonCircle(Polygon = #{pos := PPos, calcPoints := Points, edges := Edges }, Circle = #{pos := #{x := CX, y := CY}, r := Radius}) ->
	CirclePos = sat_erlang_vector:sub(PPos, sat_erlang_vector:init(CX, CY)),
	Radius2 = Radius * Radius,
	Len = length(Points),
	
	F = fun(_, {Responese, false}) ->
				{Responese, false};
			(I, {Responese, _}) ->
				Next = iF(I == Len, 1, I + 1),
				Prev = iF(I == 1, Len, I - 1),
				Overlap = 0,
				OverlapN = undefined,
				Edge = lists:nth(I, Edges),
				Point = sat_erlang_vector:sub(lists:nth(I, Points), CirclePos),

				Responese2 = case sat_erlang_vector:len2(Point) > Radius2 of
					true ->
						Responese#{aInB := false};
					false ->
						Responese
				end,

				Region = voronoiRegion(Edge, Point),
				{Responese3, OverlapN2, Overlap2, Tag} = if
					Region =:= ?LEFT_VORONOI_REGION ->
						
						Edge2 = lists:nth(Prev, Edges),
						Point2 = sat_erlang_vector:sub(lists:nth(Prev, Points), CirclePos),
						Region2 = voronoiRegion(Edge2, Point2),

						if
							Region2 =:= ?RIGHT_VORONOI_REGION  ->
								Dist = sat_erlang_vector:len(Point),
								case Dist > Radius of
									true ->
										{Responese2#{isCollide := false}, OverlapN, Overlap, false};
									false ->
										{Responese2#{bInA := false, isCollide := true}, sat_erlang_vector:normalize(Point), Radius - Dist, true}
								end;
							true ->
								{Responese2, OverlapN, Overlap, true}
						end;
					Region =:= ?RIGHT_VORONOI_REGION ->
						Edge2 = lists:nth(Next, Edges),
						Point2 = sat_erlang_vector:sub(lists:nth(Next, Points), CirclePos),
						Region2 = voronoiRegion(Edge2, Point2),

						if
							Region2 =:= ?LEFT_VORONOI_REGION ->
								Dist = sat_erlang_vector:len(Point2),
								case Dist > Radius of
									true ->
										{Responese2#{isCollide := false}, OverlapN, Overlap, false};
									false ->
										{Responese2#{bInA := false, isCollide := true}, sat_erlang_vector:normalize(Point2), Radius - Dist, true}
								end;
							true ->
								{Responese2, OverlapN, Overlap, true}
						end;

					Region =:= ?MIDDLE_VORONOI_REGION ->
						Normal = sat_erlang_vector:normalize(sat_erlang_vector:perp(Edge)),
						Dist = sat_erlang_vector:dot(Normal, Point),
						DistAbs = abs(Dist),

						case Dist > 0 andalso DistAbs > Radius of
							true ->
								{Responese2#{isCollide := false}, OverlapN, Overlap, false};	
							false ->
								Responese2_ = case Dist >= 0 orelse Overlap < 2 * Radius of
									true ->
										Responese2#{bInA := false};
									false ->
										Responese2#{bInA := true}
								end,
								{Responese2_#{isCollide := true}, Normal, Radius - Dist, true}
						end
				end,

				case Tag =/= false andalso OverlapN2 =/= undefined andalso abs(Overlap2) < abs( maps:get(overlap, Responese3)) of
					true ->
						{Responese3#{overlap := Overlap2, overlapN := OverlapN2}, Tag};
					false ->
						{Responese3, Tag}
				end
	end,
	{Responese4 = #{overlapN := OverlapN3, overlap := Overlap3}, _} = lists:foldl(F, {?'#response', true}, lists:seq(1, Len)),
	Responese4#{a := Polygon, b := Circle, overlapV := sat_erlang_vector:scale(Overlap3, 0, OverlapN3)}.

testCirclePolygon(Circle, Polygon) ->
	Responese = #{a := A, aInB := AInB, overlapN := OverlapN, overlapV := OverlapV, b := B, bInA := BInA} = testPolygonCircle(Polygon, Circle),
	Responese#{overlapN := sat_erlang_vector:reverse(OverlapN), overlapV := sat_erlang_vector:reverse(OverlapV), a := B, b := A, aInB := BInA, bInA := AInB}.


testPolygonPolygon(A = #{calcPoints := ACalcPoints, pos := APos, normals := ANormals}, B = #{calcPoints := BCalcPoints, pos := BPos, normals := BNormals}) ->
	
	ALen = length(ACalcPoints),
	BLen = length(BCalcPoints),

	FA = fun(_, {Responese, Normals, false}) ->
			{Responese, Normals, false};
		(I, {Responese, Normals, true}) ->
			case isSeparatingAxis(APos, BPos, ACalcPoints, BCalcPoints, lists:nth(I, Normals), Responese) of
				{true, Responese2} ->
					{Responese2, Normals, false};
				{false, Responese2} ->
					{Responese2, Normals, true}
			end
	end,
	{Responese2, _, Tag} = lists:foldl(FA, {?'#response', ANormals, true}, lists:seq(1, ALen)),
	case Tag of
		false ->
			Responese2#{isCollide := false};
		true ->
			{Responese3 = #{overlap := Overlap, overlapN := OverlapN}, _, Tag2} = lists:foldl(FA, {Responese2, BNormals, true}, lists:seq(1, BLen)),
			case Tag2 of
				false ->
					Responese3#{isCollide := false};
				true ->
					Responese3#{a := A, b := B, overlapV := sat_erlang_vector:scale(Overlap, 0, OverlapN), isCollide := true}
			end
	end.



isSeparatingAxis(APos, BPos, APoints, BPoints, Axis, Responese) ->
	OffsetV = sat_erlang_vector:sub(APos, BPos),
	ProjectedOffset = sat_erlang_vector:dot(Axis, OffsetV),

	{RangeAOne, RangeATwo} = flattenPointsOn(APoints, Axis),
	{RangeBOne, RangeBTwo}  = flattenPointsOn(BPoints, Axis),

	RangeBOne2 = RangeBOne + ProjectedOffset,
	RangeBTwo2 = RangeBTwo + ProjectedOffset,

	case RangeAOne > RangeBTwo2 orelse RangeBOne2 > RangeATwo of
		true ->
			{true, Responese};
		false ->

			{Overlap3, Responese3 = #{overlap := ROverlap}} = case RangeAOne < RangeBOne2 of
				true ->
					Responese2 = Responese#{aInB := false},
					case RangeATwo < RangeBTwo2 of
						true ->
							Overlap2 = 	RangeATwo - RangeBOne2,
							{Overlap2, Responese2#{bInA := false}};
						false ->
							Option1 = RangeATwo - RangeBOne2,
							Option2 = RangeBTwo2 - RangeAOne,
							{iF(Option1 < Option2 , Option1 , -Option2), Responese2}
					end;
				false ->
					Responese2 = Responese#{bInA := false},
					case RangeATwo > RangeBTwo2 of
						true ->
							Overlap2 = RangeAOne - RangeBTwo2,
							{Overlap2, Responese2#{aInB := false}};
						false ->
							Option1 = RangeATwo - RangeBOne2,
							Option2 = RangeBTwo2 - RangeAOne,
							{iF(Option1 < Option2 , Option1 , -Option2), Responese2}
					end
			end,

			AbsOverlap = abs(Overlap3),
			case AbsOverlap < ROverlap of
				true ->
					OverlapN = case Overlap3 < 0 of
						true ->
							sat_erlang_vector:reverse(Axis);
						false ->
							Axis
					end,
					{false, Responese3#{overlap := AbsOverlap, overlapN := OverlapN} };
				false ->
					{false, Responese3}
			end
	end.

flattenPointsOn(Points, Normal) ->
	MinD = 1 bsl 32,
	MaxD = -(1 bsl 32),

	F = fun(Point, {Min, Max}) ->
		Dot = sat_erlang_vector:dot(Normal, Point),
		Min2 = iF(Dot < Min, Dot, Min),
		Max2 = iF(Dot > Max, Dot, Max),
		{Min2, Max2}
	end,
	lists:foldl(F, {MinD, MaxD}, Points).



iF(A, B, C) ->
	case A of
		true ->  B;
		false -> C
	end.

voronoiRegion(Line, Point) ->
	Len2 = sat_erlang_vector:len2(Line),
	Dp = sat_erlang_vector:dot(Line, Point),
	if
		Dp < 0 ->
			?LEFT_VORONOI_REGION;
		Dp > Len2 ->
			?RIGHT_VORONOI_REGION;
		true ->
			?MIDDLE_VORONOI_REGION
	end.



%%====================================================================
%% Internal functions
%%====================================================================
