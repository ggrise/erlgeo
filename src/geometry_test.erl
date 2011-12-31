-module(geometry_test).
-compile(export_all).
-include_lib("kernel/include/file.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-record(test_suite, {scale=0.0, cases=[]}).
-record(test_case, {desc = "", op=none, params=[], args=[], output=none}).


trim(Input) -> re:replace(re:replace(list_to_binary(Input), "^[ \t\n]+", "", [global, {return, binary}]), "[ \t\n]+$", "",[global, {return, binary}]).


read_attributes([], Acc) ->
	lists:reverse(Acc);
read_attributes([Head|Att], Acc) ->
	read_attributes(Att, [{Head#xmlAttribute.name, Head#xmlAttribute.value}|Acc]).

read_precision(XmlRoot) ->
	case xmerl_xpath:string("//precisionModel", XmlRoot) of 
		[XmlElement] when is_record(XmlElement, xmlElement) ->
			Attr = read_attributes(XmlElement#xmlElement.attributes,[]),
			case proplists:get_value(scale, Attr) of
				undefined ->
					0.0;
				V ->
					list_to_float(V)
			end;
		_ ->
			1.0
	end.

parse_case(Element = #xmlElement{name='desc', expanded_name=_, namespace=_, parents=_, attributes=_, content=_, language=_,
		xmlbase=_}, Testcase) ->
	[Text|_] = Element#xmlElement.content,
	Testcase#test_case{desc=iolist_to_binary(Text#xmlText.value)};

parse_case(Element = #xmlElement{name='test'}, Testcase) ->
	Param = Testcase#test_case.params,
	[OpElement] = xmerl_xpath:string("//op", Element),
	Attr = read_attributes(OpElement#xmlElement.attributes,[]),
	Test1 = case proplists:get_value(name, Attr) of
		undefined ->
			Testcase;
		Name ->
			Testcase#test_case{op=list_to_atom(Name)}
	end,
	Args0 = proplists:delete(name, Attr),
	Keys0 = proplists:get_keys(Args0),
	Keys = lists:sort(Keys0),
	Args = lists:foldl(fun(Key, A) ->
		Value = proplists:get_value(Key, Attr),
		NKey = list_to_atom(string:to_lower(Value)),
		NValue = proplists:get_value(NKey, Param),
		case NValue of
			undefined ->
				[trim(Value)|A];
			_ ->
				[proplists:get_value(NKey, Param)|A]
		end
	end, [], Keys),
	[Text|_] = OpElement#xmlElement.content,
    Value = binary_to_list(iolist_to_binary(Text#xmlText.value)),
	ValueS = trim(Value),

	Test1#test_case{args=lists:reverse(Args), output=ValueS};
parse_case(Element, Testcase) ->
	Args = Testcase#test_case.params,
	[Text|_] = Element#xmlElement.content,
	%io:format("@~p~n", [Text#xmlText.value]),
	%Value = lists:flatten(binary_to_list(iolist_to_binary(Text#xmlText.value))),
	ValueS = trim(Text#xmlText.value),
	Testcase#test_case{params=[{Element#xmlElement.name, ValueS}|Args]}.

read_cases([], Acc) ->
	lists:reverse(Acc);
read_cases([Head|Tail], Acc) ->
	CaseElements = xmerl_xpath:string("//case/*", Head),
	Case = lists:foldl(fun(Elem, Accf) ->
		parse_case(Elem, Accf)
	end, #test_case{}, CaseElements),
	read_cases(Tail, [Case|Acc]).
	
read_cases(XmlRoot) ->
	Cases = xmerl_xpath:string("//case", XmlRoot),
	read_cases(Cases, []).

process_test(File) ->
	{ Xml, _Rest } = xmerl_scan:file(File),
	%fetch the precision model
	Cases = read_cases(Xml),
	Precision = read_precision(Xml),
	Suite = #test_suite{scale=Precision, cases=Cases},
	Suite.
		
run_test(Dir) ->
	filelib:fold_files(Dir, "^.+\\.xml$", true, fun(File,_) ->
		Testsuite = process_test(File),
		io:format("Process: ~p~n", [File]), 
		geometry:run(fun(C, _) ->
			execute_test(File, C, Testsuite)
		end, [])
	end, []).	



argument_to_terms(_, _,[], Acc) ->
	lists:reverse(Acc);
argument_to_terms(C, Caller={in, relate}, [Pattern], Acc) ->
	argument_to_terms(C, Caller, [], [binary_to_list(Pattern)|Acc]);
argument_to_terms(C, Caller, [<<"true">>|Rest], Acc) ->
	argument_to_terms(C, Caller, Rest, [true | Acc]);
argument_to_terms(C, Caller, [<<"false">>|Rest], Acc) ->
	argument_to_terms(C, Caller, Rest, [false | Acc]);
argument_to_terms(Context, Caller, [Arg|Rest], Acc) ->
	%wkt
	V = case re:run(Arg, "^\\s*(GEOMETRYCOLLECTION|POLYGON|LINEARRING|MULTIPOLYGON|LINESTRING|MULTILINESTRING|MULTIPOINT|POINT).*$",[dotall]) of
		{match, _} ->
			case Context:create(wkt, Arg) of
				E = {error, _} ->
					io:format("Error: ~p ~p~n",[E, Arg]),
					Arg;
				{ok, Geom} ->
					Geom
			end;
		_ ->
			case re:run(Arg, "^010[1-7][0-9A-F]+$",[dotall]) of
				{match, _} ->
					case Context:create(wkbh, Arg) of
						E = {error, _} ->
							io:format("Error: ~p ~p~n",[E, Arg]),
							Arg;
						{ok, Geom} ->
							Geom
					end;
				_ ->
					case re:run(Arg, "^[.0-9\\-]+$") of
						{match, _} ->
							Number = binary_to_list(Arg),
							N = case Number of
								"-." ++ Num ->
									"-0." ++ Num;
								"." ++ Num ->
									"0." ++ Num;
								Num -> 
									Num
							end,
							case catch list_to_integer(N) of
								{'EXIT', _} ->
									list_to_float(N);
								E ->
									E
							end;
						_ ->
							binary_to_list(Arg)
					end
			end
	end,
	argument_to_terms(Context, Caller,Rest, [V|Acc]).

test(Context, Precision, Testcase, Args, Output) ->
	
	Op = Testcase#test_case.op,
	case catch erlang:apply(Context, Op, Args) of
			{'EXIT', Error} ->
				notfound;
			{ok, Output} ->
				passed;
			E = {error, _} ->
				{failed, E};
			{ok, {ggeom, empty}} ->
				%case Testcase#test_case.output of
				%	<<"GEOMETRYCOLLECTION EMPTY">> ->
				%		passed;
				%	_ ->
				%		{failed, notequals}
				%end;
				empty;
			{ok, {ggeom, Geom}} ->
				case Context:equals_exact(Output, {ggeom, Geom}, Precision) of
					{ok, true} ->
						passed;
					V ->
						io:format("~p(~p) : ~p / ~p ~p~n", [Op, Args, Output, Precision, Testcase#test_case.output]),
						io:format("~p~n", [V]),
						{failed, notequals}
				end;
			_ -> failed
		end.

	
execute_test(File, Context, Testsuite) ->
	lists:foreach(fun(Testcase) ->

		case Testcase#test_case.output of
			none ->
				skip;
			_ ->
		%io:format("~p ~p~n", [Testcase#test_case.op, Testcase#test_case.args])
			Args = argument_to_terms(Context, {in, Testcase#test_case.op}, Testcase#test_case.args, []),
			[Result|_] = argument_to_terms(Context, {out, Testcase#test_case.op}, [Testcase#test_case.output], []),
			
			%io:format("~p(~p) : ~p~n",[Testcase#test_case.op, Args, Result]),
			R = test(Context, Testsuite#test_suite.scale, Testcase, Args, Result)
		end
	end, Testsuite#test_suite.cases).
