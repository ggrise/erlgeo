-module(geometry).
-export([run/2, run/3, run/4]).

to_term(Context, Return = {ok, {ggeom, _}}) ->
	{ok, Geom} = Return,
	Context:coords(Geom);
to_term(_Context, E) ->
	E.


run(Fun, Args) when is_function(Fun, 2) ->
	case geos_c:start_link() of
		{ok, Port} ->
			RefTable = ets:new(refs,[set, private]),
			Context = geo_context:new(Port, RefTable),
			Value = Fun(Context, Args),
			Context:dispose(),
			catch gen_server:call(Port,{stop}),
			Value;
		_ ->
			{error, notstarted}
	end;

run(Function, Arg1) when is_atom(Function) ->
	run(fun(Context, _Args) ->
			to_term(Context, Context:Function(Arg1))
	end, []).

run(Function, Arg1, Arg2) when is_atom(Function) ->
	run(fun(Context, _Args) ->
			to_term(Context, Context:Function(Arg1, Arg2))
	end, []).

run(Function, Arg1, Arg2, Arg3) when is_atom(Function) ->
	run(fun(Context, _Args) ->
			to_term(Context, Context:Function(Arg1, Arg2, Arg3))
	end, []).

