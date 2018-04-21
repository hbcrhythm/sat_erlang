-module(sat_erlang_test).
-include("sat_erlang.hrl").
-compile(export_all).


test_circle_circle() ->
	application:start(sat_erlang),
	Circle1 = sat_erlang_circle:init(sat_erlang_vector:init(0, 0), 20),
	Circle2 = sat_erlang_circle:init(sat_erlang_vector:init(30, 0), 20),
	sat_erlang:testCircleCircle(Circle1, Circle2).

test_circle_polygon() ->
	application:start(sat_erlang),
	Circle1 = sat_erlang_circle:init(sat_erlang_vector:init(-10, 10), 20),
	Polygon = sat_erlang_polygon:init(sat_erlang_vector:init(0, 0), [sat_erlang_vector:init(0, 0), sat_erlang_vector:init(40, 0), sat_erlang_vector:init(40,40), sat_erlang_vector:init(0, 40) ]),
	sat_erlang:testPolygonCircle(Polygon, Circle1).

test_polygon_polygon() ->
	application:start(sat_erlang),
	Polygon1 = sat_erlang_polygon:init(sat_erlang_vector:init(60, 0), [sat_erlang_vector:init(0, 0), sat_erlang_vector:init(40, 0), sat_erlang_vector:init(40,40), sat_erlang_vector:init(0, 40) ]),
	Polygon2 = sat_erlang_polygon:init(sat_erlang_vector:init(30, 0), [sat_erlang_vector:init(0, 0), sat_erlang_vector:init(30, 0), sat_erlang_vector:init(0,30)]),
	sat_erlang:testPolygonPolygon(Polygon1, Polygon2).

test_point_in_circle() ->
	application:start(sat_erlang),
	Point = sat_erlang_vector:init(0, 19),
	Circle = sat_erlang_circle:init(sat_erlang_vector:init(20, 20), 20),
	sat_erlang:pointInCircle(Point, Circle),
	
	Point2 = sat_erlang_vector:init(0, 0),
	Circle2 = sat_erlang_circle:init(sat_erlang_vector:init(100, 100), 20),
	sat_erlang:pointInCircle(Point2, Circle2).



test_point_in_polygon() ->
	application:start(sat_erlang),
	Point = sat_erlang_vector:init(35, 5),
	Polygon = sat_erlang_polygon:init(sat_erlang_vector:init(30, 0), [sat_erlang_vector:init(0, 0), sat_erlang_vector:init(30, 0), sat_erlang_vector:init(0, 30)]),
	sat_erlang:pointInPolygon(Point, Polygon).