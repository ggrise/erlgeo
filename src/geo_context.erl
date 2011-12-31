% copyright Gabriel Grise <ggrise@gmail.com>

-module(geo_context, [Port, RefTable]).
-export([dispose/0, wkt/1,create/2, union/2, polygonize/1,
			difference/2, boundary/1, envelope/1, buffer/3,
			simplify/2, intersection/2, intersects/2, touches/2,
			disjoint/2, crosses/2, within/2, contains/2, type/1,
			overlaps/2, isValid/1,hasZ/1,isRing/1,isEmpty/1, coords/1, 
			equals/2, line_merge/1, point_on_surface/1, convex_hull/1, 
			sym_difference/2, topology_preserver_simplify/2, relate/2,relate/3, equals_exact/3]).


dispose_('$end_of_table') ->
	ok;
dispose_(Ptr) ->
	gen_server:call(Port, {geom_destroy, {ggeom, Ptr}}),
	dispose_(ets:next(RefTable, Ptr)).
dispose() ->
	V = ets:first(RefTable),
	dispose_(V),
	ets:delete_all_objects(RefTable).

type(Geom={ggeom, _}) ->
	case gen_server:call(Port, {geomType, Geom}) of
		{ok, Type} ->
			list_to_atom(Type);
		_ ->
			unknown
	end.

wkt(Geom={ggeom, _}) ->
	case gen_server:call(Port, {wKTWriter_create}) of
		{ok, Writer} ->
			Response = case gen_server:call(Port, {wKTWriter_write, Writer, Geom}) of
				{ok, Wkt} ->
					{ok, list_to_binary(Wkt)};
				E ->
					E
			end,
			gen_server:call(Port, {wKTWriter_destroy, Writer}),
			Response;
		E ->
			E
	end.

register_geometry(null) ->
	ok;
register_geometry({ggeom, Ptr}) ->
	ets:insert(RefTable, {Ptr, ggeom}).
is_registered({ggeom, Ptr}) ->
	ets:member(RefTable, Ptr);
is_registered(List) when is_list(List) ->
	lists:all(fun(G) ->
				is_registered(G)
		end).

%create a linearring
create_linearring(Port, Coords) when is_list(Coords) ->
	CoordObj = gen_server:call(Port, {terms_to_coordseq, Coords}),
	case CoordObj of
		{ok, C} ->
			gen_server:call(Port, {geom_createLinearRing,C});
		_ -> CoordObj
	end.

%create a list of linearring
create_linearrings(_Port, List, []) -> {ok, List};
create_linearrings(Port, List, [Head|T]) when is_list(Head) -> 
	L = create_linearring(Port, Head),
	case L of
		{ok, Linear} ->
			create_linearrings(Port, [Linear|List], T);
		_ ->
	%an error occured, we have to destroy all linearing
		lists:foreach(fun(Ring) -> 
			gen_server:call(Port, {geom_destroy, Ring}) end, List),
		L
	end.

create_polygon(Shell, Holes) ->
	%the polygon take ownership of the shell and holes
	case gen_server:call(Port, {geom_createPolygon, Shell, Holes}) of
		{ok, Polygon} ->
			register_geometry(Polygon),
			{ok, Polygon};
		E ->
			E
	end.


create_collection(Type, [], Out) ->
	L = lists:reverse(Out),
	gen_server:call(Port, {geom_createCollection, Type, L, length(L)});
create_collection(Type, [{ggeom, Geom}|List], Out) ->
	case gen_server:call(Port, {geomType, {ggeom, Geom}}) of
			{ok, Type} ->
				create_collection(Type, List, [{ggeom, Geom}|Out]);
			_ ->
				{error, typemismatch}
	end.

create(collection, [{ggeom,Geom}|List]) ->
	case gen_server:call(Port, {geomType, {ggeom, Geom}}) of
		{ok, Type} ->
			case create_collection(Type, [{ggeom,Geom}|List],[]) of
				{ok, Coll} ->
					register_geometry(Coll);
				Er ->
					Er
			end;
		E ->
			E
	end;


create(line, Coords) ->
	CoordObj = gen_server:call(Port, {terms_to_coordseq, Coords}),
	case CoordObj of
		{ok, C} ->
			case gen_server:call(Port, {geom_createLineString,C}) of 
				{ok, Line} ->
					register_geometry(Line),
					{ok, Line};
				E ->
					gen_server:call(port, {coordSeq_destroy, C}),
					E
			end;
		_ -> CoordObj
	end;



%factory
create(polygon, {ShellList, Holes}) ->
	Shell = create_linearring(Port, ShellList),
	case Shell of {error, _Reason} -> Shell;
		{ok, ShellRing} ->
			HolesRing = create_linearrings(Port, [], Holes),
			case HolesRing of
				{ok, List} ->
					%we can create safely the polygon
					create_polygon(ShellRing, List);
				{error, _Reason} ->
					gen_server:call(port, {geom_destroy, ShellRing}),
					HolesRing
			end
	end;
create(polygon, Shell) ->
	create(polygon, {Shell, []});

create(wkbh, Wkb) when is_binary(Wkb) ->
	case gen_server:call(Port, {wKBReader_create}) of
		{ok, Reader} ->
			Response = case gen_server:call(Port, {wKBReader_readHEX, Reader, Wkb, size(Wkb)}) of
				{ok, Geom} ->
					register_geometry(Geom),
					{ok, Geom};
				E ->
					E
			end,
			gen_server:call(Port, {wKBReader_destroy, Reader}),
			Response;
		E ->
			E
	end;


create(wkt, Wkt) when is_binary(Wkt) ->
	case gen_server:call(Port, {wKTReader_create}) of
		{ok, Reader} ->
			Response = case gen_server:call(Port, {wKTReader_read, Reader, Wkt}) of
				{ok, {ggeom, Geom}} ->
					register_geometry({ggeom, Geom}),
					{ok, {ggeom, Geom}};
				E ->
					E
			end,
			gen_server:call(Port, {wKTReader_destroy, Reader}),
			Response;
		E ->
			E
	end;
create(point, Coord={_X, _Y}) ->
	CoordObj = gen_server:call(Port, {terms_to_coordseq, [Coord]}),
	case CoordObj of
		{ok, C} ->
			RPoint = gen_server:call(Port, {geom_createPoint,C}),
			case RPoint of 
				   {ok, Point} -> 
					   register_geometry(Point),
					   {ok, Point};
				   {error, _} -> 
					   gen_server:call(Port, {coordSeq_destroy, C}),
					   RPoint
			end;
		_ -> 
			CoordObj
	end.
	

%operation that create a new geometry
call_operation(Op,Param) ->
	call_operation(Op,Param, []). 
call_operation(Op,[], OutParam) ->
	case gen_server:call(Port, list_to_tuple([Op|lists:reverse(OutParam)])) of
		{ok, Result} ->
			%all operation have a resulting geometry and the callee
			%get the ownership of it
			register_geometry(Result),
			{ok, Result};
		E ->
			E
	end;
call_operation(Op,[{ggeom, Ptr}|Tail], Out) ->
	%the geometry need to be a valid pointer,
	case is_registered({ggeom, Ptr}) of
		true ->
			call_operation(Op, Tail, [{ggeom, Ptr}|Out]);
		_ ->
			{error, geomnotreg}
	end.

union(Geom1, Geom2) ->
	call_operation(union, [Geom1, Geom2]).
polygonize(GeomList) ->
	case is_registered(GeomList) of
		true ->
			call_operation(polygonize, [GeomList, length(GeomList)]);
		_ ->
			{error, geomnotreg}
	end.
line_merge(Geom) ->
	call_operation(lineMerge, [Geom]).
difference(Geom1, Geom2) ->
	call_operation(difference, [Geom1, Geom2]).
point_on_surface(Geom) ->
	call_operation(pointOnSurface, [Geom]).
boundary(Geom) ->
	call_operation(boundary, [Geom]).
envelope(Geom) ->
	call_operation(envelope, [Geom]).
convex_hull(Geom) ->
	call_operation(convexHull, [Geom]).
sym_difference(Geom1, Geom2) ->
	call_operation(symDifference, [Geom1, Geom2]).
buffer(Geom, Width, Quadsegs) ->
	call_operation(buffer, [Geom, Width, Quadsegs]).
simplify(Geom1, Geom2) ->
	call_operation(simplify, [Geom1, Geom2]).
topology_preserver_simplify(Geom, Tolerance) ->
	call_operation(topologyPreserveSimplify, [Geom, Tolerance]).
intersection(Geom1, Geom2) ->
	call_operation(intersection, [Geom1, Geom2]).

%predicate
call_predicate(Op,Param) ->
	call_predicate(Op,Param, []). 
call_predicate(Op,[], OutParam) ->
	gen_server:call(Port, list_to_tuple([Op|lists:reverse(OutParam)]));
call_predicate(Op,[{ggeom, Ptr}|Tail], Out) ->
	%the geometry need to be a valid pointer,
	case is_registered({ggeom, Ptr}) of
		true ->
			call_predicate(Op, Tail, [{ggeom, Ptr}|Out]);
		_ ->
			{error, geomnotreg}
	end;
call_predicate(Op,[Param|Tail], Out) ->
	call_predicate(Op, Tail, [Param|Out]).

isValid(Geom1) ->
	call_predicate(isValid, [Geom1]).
isEmpty({ggeom, empty}) ->
	true;
isEmpty(Geom1 = {ggeom, _}) ->
	call_predicate(isEmpty, [Geom1]).
isRing(Geom1 = {ggeom, _}) ->
	call_predicate(isRing, [Geom1]).
hasZ(Geom1 = {ggeom, _}) ->
	call_predicate(hasZ, [Geom1]).


relate(Geom1, Geom2, Pattern) when is_list(Pattern) ->
	call_predicate(relatePattern, [Geom1, Geom2, Pattern]).
relate(Geom1, Geom2) ->
	call_predicate(relate, [Geom1, Geom2]).
intersects(Geom1, Geom2) ->
	call_predicate(intersects, [Geom1, Geom2]).
touches(Geom1, Geom2) ->
	call_predicate(touches, [Geom1, Geom2]).
disjoint(Geom1, Geom2) ->
	call_predicate(disjoint, [Geom1, Geom2]).
crosses(Geom1, Geom2) ->
	call_predicate(crosses, [Geom1, Geom2]).
within(Geom1, Geom2) ->
	call_predicate(within, [Geom1, Geom2]).
contains(Geom1, Geom2) ->
	call_predicate(contains, [Geom1, Geom2]).
overlaps(Geom1, Geom2) ->
	call_predicate(overlaps, [Geom1, Geom2]).
equals(Geom1, Geom2) ->
	call_predicate(equals, [Geom1, Geom2]).
equals_exact(Geom1, Geom2, Tolerance) ->
	call_predicate(equalsExact, [Geom1, Geom2, Tolerance]).

interiorRing0(_Geom, 0, Acc) ->
	lists:reverse(Acc);
interiorRing0(Geom, N, Acc) ->
	case gen_server:call(Port, {getInteriorRingN, Geom, N-1}) of
		{ok, Ring} ->
			io:format("~p~n", [Ring]),
			case coords0(Ring) of
				{ok, R} ->
					interiorRing0(Geom, N-1, [R|Acc]);
				_ ->
					[]
			end;
		_ ->
			[]
	end.

interiorRing(Geom={ggeom, _}) ->
	case gen_server:call(Port, {getNumInteriorRings, Geom}) of
		{ok, Count} ->
			interiorRing0(Geom, Count, []);
		_ ->
			[]
	end.

exteriorRing(Geom={ggeom, _}) ->
	case type(Geom) of
		'Polygon' ->
			Ring = gen_server:call(Port, {getExteriorRing, Geom}),
			case Ring of
				{ok, VRing} ->
					coords0(VRing);
				E ->
					E
			end;
		_ ->
			{error, notpolygon}
	end.

coords_geom('Point', Geom) ->
	case coords0(Geom) of
		{ok, [Coord]} ->
			Coord;
		E ->
			E
	end;
coords_geom('LineString', Geom) ->
	coords0(Geom);
coords_geom('LinearRing', Geom) ->
	coords0(Geom);
coords_geom('Polygon', Geom) ->
	case exteriorRing(Geom) of
		{ok, Ring} ->
			%Ext = coords0(Ring),
			{Ring, interiorRing(Geom)};
		_E ->
			error
	end.

coords0(G={ggeom, _}) ->
	CoordObj = gen_server:call(Port, {geom_getCoordSeq, G}),
	case CoordObj of
		{ok, C} ->
			gen_server:call(Port, {coordseq_to_terms, C});
		_ -> CoordObj
	end.
coords(G={ggeom, _}) ->
	coords_geom(type(G), G).

