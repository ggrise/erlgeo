% copyright Gabriel Grise <ggrise@gmail.com>
-module(geometry).
-include("geometry.hrl").
-export([new/0, create/3, from_wkb/2, to_wkt/2, to_wkb/2,from_wkt/2, disjoin/3, touches/3, intersects/3,
crosses/3, within/3, contains/3, overlaps/3, from_wkt_to_wkb/2, equals/3, dispose/1, dispose/2]).

new() ->
	{ok, Port} = geos_c:start_link(),
	RefTable = ets:new(refs,[set, private]),
	{?MODULE, #geocontext{port=Port,refs=RefTable}}.

geom_ref(Context, List) ->
	geom_ref(Context, List, []).
geom_ref(_Context, [], Acc) ->
	list_to_tuple(lists:reverse(Acc));
geom_ref(Context, [{ggeom, Ref} | List], Acc) when is_reference(Ref) ->
	Refs = Context#geocontext.refs,
	case ets:lookup(Refs, Ref) of 
		[] ->
			{error, invalidgeom};
		[{_,Pointer}|_] ->
			geom_ref(Context, List, [{ggeom, Pointer}|Acc])
	end;
geom_ref(Context, [Other|List], Acc) ->
	geom_ref(Context, List, [Other|Acc]).


call_ref(Context, Param) ->
	Port = Context#geocontext.port,
	Refs = Context#geocontext.refs,
	Param1 = geom_ref(Context, tuple_to_list(Param)),
	case Param1 of
		{error, _} -> Param1;
		_ ->
		Response = gen_server:call(Port, Param1),

		case Response of
			{ok, {ggeom, Pointer}} ->
				MyRef = make_ref(),
				ets:insert(Refs, {MyRef, Pointer}),
				{ok, {ggeom, MyRef}};
			_ -> Response
		end
	end.


dispose({_,Context}) when is_record(Context,geocontext) ->
	Port = Context#geocontext.port,
	gen_server:call(Port, {stop}).

dispose({ggeom, R} = Geom,{_, Context}) -> 
	Refs = Context#geocontext.refs,
	case call_ref(Context,  {geom_destroy, Geom}) of
		ok ->  ets:delete(Refs, R), ok;
		Other -> Other
	end.

	%case geom_ref(Context, [Geom]) of {error, _} = E -> E;
	%	[OGeom|_] ->
	%		case gen_server:call(Port,  {geom_destroy, OGeom}) of
	%			ok -> ets:delete(Refs, R), ok;
	%			Other -> Other
	%		end
	%end.

disjoin(Geom1, Geom2, {_, Context}) -> 
	call_ref(Context, {disjoin, Geom1, Geom2}).

touches(Geom1, Geom2, {_,Context}) ->
	call_ref(Context, {touches, Geom1, Geom2}).
intersects(Geom1, Geom2, {_,Context}) ->
	call_ref(Context, {intersects, Geom1, Geom2}).
crosses(Geom1, Geom2,{_,Context}) ->
	call_ref(Context, {crosses, Geom1, Geom2}).
within(Geom1, Geom2,{_,Context}) ->
	call_ref(Context, {within, Geom1, Geom2}).
contains(Geom1, Geom2, {_,Context}) ->
	call_ref(Context, {contains, Geom1, Geom2}).
overlaps(Geom1, Geom2, {_,Context}) ->
	call_ref(Context, {overlaps, Geom1, Geom2}).
equals(Geom1, Geom2, {_,Context}) ->
	call_ref(Context, {equals, Geom1, Geom2}).


create_linearring(Port, Coords) when is_list(Coords) ->
	CoordObj = gen_server:call(Port, {terms_to_coordseq, Coords}),
	case CoordObj of
		{ok, C} ->
			gen_server:call(Port, {geom_createLinearRing,C});
		_ -> CoordObj
	end.

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


to_wkt(Geometry, {_,Context}) ->
	call_ref(Context, {geomToWKT, Geometry}).
to_wkb(Geometry, {_,Context}) ->
	call_ref(Context, {geomToWKB, Geometry}).

from_wkt_to_wkb(GeomStr, {_,Context}) ->
	Port = Context#geocontext.port,
	gen_server:call(Port, {wkt_to_wkb, GeomStr}).
from_wkt(GeomStr, {_,Context}) ->
	call_ref(Context, {geomFromWKT, GeomStr}).

from_wkb(GeomStr, {_,Context}) ->
	call_ref(Context, {geomFromWKB_buf, GeomStr, size(GeomStr)}).


%    GEOS_POINT,
%    GEOS_LINESTRING,
%    GEOS_LINEARRING,
%    GEOS_POLYGON,
%    GEOS_MULTIPOINT,
%    GEOS_MULTILINESTRING,
%    GEOS_MULTIPOLYGON,
%    GEOS_GEOMETRYCOLLECTION


create(linestring, Coords, {_,Context}) when is_record(Context, geocontext) ->
	Port = Context#geocontext.port,
	CoordObj = gen_server:call(Port, {terms_to_coordseq, Coords}),
	case CoordObj of
		{ok, C} ->
			call_ref(Context, {geom_createLineString,C});
		_ -> CoordObj
	end;


create(point, Coord, {_,Context})  when is_record(Context, geocontext) ->
	Port = Context#geocontext.port,
	CoordObj = gen_server:call(Port, {terms_to_coordseq, [Coord]}),
	case CoordObj of
		{ok, C} ->
			RPoint = call_ref(Context, {geom_createPoint,C}),
			case RPoint of {ok, _Point} -> RPoint;
				   {error, _} -> gen_server:call(Port, {coordSeq_destroy, C}), RPoint
			end;
		_ -> CoordObj
	end;

create(polygon, {Shell, Holes}, {_,Context}) when is_record(Context, geocontext) ->
	Port = Context#geocontext.port,
	S = create_linearring(Port, Shell),
	case S of {error, _Reason} -> S; %fail on shell
		 {ok, ShellRing} ->
			HolesRing = create_linearrings(Port, [], Holes), 
			case HolesRing of 
				{ok, List} ->
					Polygon = call_ref(Port, {geom_createPolygon, ShellRing, List}),
					case Polygon of {ok, _} -> Polygon;
						{error, _} ->
							%must destroy everything
							gen_server:call(Port, {geom_destroy, ShellRing}),
							lists:foreach(fun(Hr) -> 
								gen_server:call(Port, {geom_destroy, Hr}) end, List),
							Polygon
					end;
				{error, _Reason} ->
					%%destroy 
					gen_server:call(port, {geom_destroy, ShellRing}),
					HolesRing
			end
	end.

