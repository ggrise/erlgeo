
# Erlgeo

Erlgeo is an erlang wrapper to the [GEOS](http://trac.osgeo.org/geos/ "Geometry Engine, Open Source") library. Erlgeo is currently *experimental* and should not be used in production.

## What is GEOS?
GEOS stand for **G**eometry **E**ngine - **O**pen **S**ource) is a C++ port of the  Java Topology Suite (JTS). As such, it aims to contain the complete functionality of JTS in C++. This includes all the  OpenGIS Simple Features for SQL spatial predicate functions and spatial operators, as well as specific JTS enhanced topology functions.

## How to use

	Intersection = fun(Context, {WktPoly1, WktPoly2}) ->
        {ok, P1} = Context:create(wkt, WktPoly1),
        {ok, P2} = Context:create(wkt, WktPoly2),
        {ok, Result} = Context:intersection(P1,P2),

        {ok, ResultWkt} = Context:wkt(Result),

        % dispose Geometry (geom_destroy)
        Context:dispose(P1),
        Context:dispose(P2),

        % if the geometry is use after being disposed an error is returned.

        {error, geomnotreg} = Context:wkt(P1),

        % If the geometry is not disposed manually, the geometry will be dispose when the function return

        {Context:coords(Result), ResultWkt}
    end,
    geometry:run(Intersection,
        {<<"POLYGON ((-1.0 50.5, -0.5 51.2, 0.3 50.9, -1 50.5))">>, <<"POLYGON((-0.7 50.3, 0.1 51.0, 0.6 50.1, -0.7 50.3))">>}).
                             

Result:

	{
		{[{0.0799999999999999,50.9825},{0.11754385964912316,50.96842105263158},{0.17664233576642407,50.86204379562044},{-0.18474576271186416,50.75084745762712},{0.0799999999999999,50.9825}],[]},

		<<"POLYGON ((0.0799999999999999 50.9825000000000017, 0.1175438596491232 50.9684210526315766, 0.1766423357664241 50.8620437956204370, -0.1847457627118642 50.7508474576271169, 0.0799999999999999 50.9825000000000017))">>
	}


## compile

	cd c_src
	wget http://download.osgeo.org/geos/geos-3.2.3.tar.bz2
	cd .. && ./rebar compile
	
## Documentation
No doc yet

## License
MIT
