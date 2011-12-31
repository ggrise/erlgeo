% copyright Gabriel Grise <ggrise@gmail.com>
-module(geos_c).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define('DRIVER_NAME', 'geos_drv').

-record(state, {port, refs}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    SearchDir = case code:priv_dir(erlgeo) of
		{error, _} ->
			filename:join([filename:dirname(code:which(geos_c)), "../priv/"]);
		V ->
			V
	end,
	case erl_ddll:load(SearchDir, atom_to_list(?DRIVER_NAME)) of
	ok ->
	    {ok, #state{port=open_port({spawn, ?DRIVER_NAME},[]),refs=dict:new()}};
	Error ->
	    Error
    end.


%store_ref(Reply, State) ->
%	case Reply of 
%		{ok, PtrObj} ->
%			Refs=State#state.refs,
%			Dict=dict:store(PtrObj, ok, Refs),
%			#state{port=State#state.port, refs=Dict};
%		_ -> State
%	end.

%pop_ref(PtrObj, State) ->
%	Refs=State#state.refs,
%	case dict:is_key(PtrObj, Refs) of
%		true -> {ok, #state{port=State#state.port, refs=dict:erase(PtrObj,Refs)}};
%		_ -> {noref, State}
%	end.

%has_ref(PtrObj, State) ->
%	Refs=State#state.refs,
%	dict:is_key(PtrObj, Refs).
%has_refs([Head|List], State) ->
%	case has_ref(Head,State) of 
%		true -> has_refs(List, State);
%		_ -> false
%	end;
	
%has_refs([], _State) ->
%	true.

handle_call({stop}, _From,State) ->
	{stop, normal, State};	

handle_call({coordSeq_getY,Coordsequence1,Int2}, _From, State) ->
		Value = term_to_binary({Coordsequence1,Int2}),
		Message = <<0, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({topologyPreserveSimplify,Geometry1,Double2}, _From, State) ->
		Value = term_to_binary({Geometry1,Double2}),
		Message = <<1, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};

handle_call({isValid,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<2, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geom_clone,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<3, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({coordSeq_getSize,Coordsequence1}, _From, State) ->
		Value = term_to_binary({Coordsequence1}),
		Message = <<4, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({intersection,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<5, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geomToWKB,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<6, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBReader_read,Wkbreader1,Char2,Size3}, _From, State) ->
		Value = term_to_binary({Wkbreader1,Char2,Size3}),
		Message = <<7, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geom_createPolygon,Geometry1,Geometry2}, _From, State) ->
	%pop coordinate seq on success
		Value = term_to_binary({Geometry1, length(Geometry2), Geometry2}),
		Message = <<8, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({intersects,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<9, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({within,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<10, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({preparedContainsProperly,Preparedgeometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Preparedgeometry1,Geometry2}),
		Message = <<11, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({getWKBByteOrder}, _From, State) ->
        Value = term_to_binary({}),
        Message = <<12, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({geom_getCoordSeq,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<13, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({preparedContains,Preparedgeometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Preparedgeometry1,Geometry2}),
		Message = <<14, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBWriter_writeHEX,Wkbwriter1,Geometry2}, _From, State) ->
		Value = term_to_binary({Wkbwriter1,Geometry2}),
		Message = <<15, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({relatePattern,Geometry1,Geometry2,Char3}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2,Char3}),
		Message = <<16, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({coordSeq_create,Int1,Int2}, _From, State) ->
        Value = term_to_binary({Int1,Int2}),
        Message = <<17, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({touches,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<18, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({distance,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<19, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({area,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<20, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({isSimple,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<21, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geomFromHEX_buf,Char1,Size2}, _From, State) ->
        Value = term_to_binary({Char1,Size2}),
        Message = <<22, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({coordSeq_setY,Coordsequence1,Int2,Double3}, _From, State) ->
        Value = term_to_binary({Coordsequence1,Int2,Double3}),
        Message = <<23, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({simplify,Geometry1,Double2}, _From, State) ->
		Value = term_to_binary({Geometry1,Double2}),
		Message = <<24, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({getNumCoordinates,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<25, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({coordSeq_clone,Coordsequence1}, _From, State) ->
		Value = term_to_binary({Coordsequence1}),
		Message = <<26, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geom_createLinearRing,Coordsequence1}, _From, State) ->
		%TODO POP the reference, because the ownership of the coordinateSeq
		%is on the line string
		Value = term_to_binary({Coordsequence1}),
		Message = <<27, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({relate,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<28, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKTReader_destroy,Wktreader1}, _From, State) ->
        		Value = term_to_binary({Wktreader1}),
       			Message = <<29, Value/binary>>,
        		Reply = send_command(State#state.port, Message),
        		{reply,Reply,State};


handle_call({wKBWriter_setOutputDimension,Wkbwriter1,Int2}, _From, State) ->
		Value = term_to_binary({Wkbwriter1,Int2}),
		Message = <<30, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({buffer,Geometry1,Double2,Int3}, _From, State) ->
		Value = term_to_binary({Geometry1,Double2,Int3}),
		Message = <<31, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({symDifference,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<32, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({convexHull,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<33, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBWriter_setByteOrde,Wkbwriter1,Int2}, _From, State) ->
		Value = term_to_binary({Wkbwriter1,Int2}),
		Message = <<34, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({coordSeq_getZ,Coordsequence1,Int2}, _From, State) ->
		Value = term_to_binary({Coordsequence1,Int2}),
		Message = <<35, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geomTypeId,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<36, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKTReader_create}, _From, State) ->
        Value = term_to_binary({}),
        Message = <<37, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({coordSeq_getOrdinate,Coordsequence1,Int2,Int3}, _From, State) ->
		Value = term_to_binary({Coordsequence1,Int2,Int3}),
		Message = <<38, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({getNumInteriorRings,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<39, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBReader_destroy,Wkbreader1}, _From, State) ->
       			Value = term_to_binary({Wkbreader1}),
        		Message = <<40, Value/binary>>,
        		Reply = send_command(State#state.port, Message),
        		{reply,Reply,State};

handle_call({geom_getDimensions,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<41, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBWriter_getIncludeSRID,Wkbwriter1}, _From, State) ->
		Value = term_to_binary({Wkbwriter1}),
		Message = <<42, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({prepare,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<43, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({equals,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<44, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({overlaps,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<45, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({preparedGeom_destroy,Preparedgeometry1}, _From, State) ->
			Value = term_to_binary({Preparedgeometry1}),
        		Message = <<46, Value/binary>>,
        		Reply = send_command(State#state.port, Message),
        		{reply,Reply,State};

handle_call({coordSeq_getDimensions,Coordsequence1}, _From, State) ->
		Value = term_to_binary({Coordsequence1}),
		Message = <<47, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({isValidReason,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<48, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({getCentroid,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<49, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBReader_readHEX,Wkbreader1,Char2,Size3}, _From, State) ->
		Value = term_to_binary({Wkbreader1,Char2,Size3}),
		Message = <<50, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({coordSeq_destroy,Coordsequence1}, _From, State) ->
			Value = term_to_binary({Coordsequence1}),
        		Message = <<51, Value/binary>>,
        		Reply = send_command(State#state.port, Message),
        		{reply,Reply,State};

handle_call({coordSeq_setZ,Coordsequence1,Int2,Double3}, _From, State) ->
		Value = term_to_binary({Coordsequence1,Int2,Double3}),
		Message = <<52, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({hasZ,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<53, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({envelope,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<54, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geomFromWKB_buf,Char1,Size2}, _From, State) ->
        Value = term_to_binary({Char1,Size2}),
        Message = <<55, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({isRing,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<56, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({getGeometryN,Geometry1,Int2}, _From, State) ->
		Value = term_to_binary({Geometry1,Int2}),
		Message = <<57, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({preparedIntersects,Preparedgeometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Preparedgeometry1,Geometry2}),
		Message = <<58, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBWriter_getOutputDimension,Wkbwriter1}, _From, State) ->
		Value = term_to_binary({Wkbwriter1}),
		Message = <<59, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBWriter_create}, _From, State) ->
        Value = term_to_binary({}),
        Message = <<60, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({normalize,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<61, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geomType,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<62, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({equalsExact,Geometry1,Geometry2,Double3}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2,Double3}),
		Message = <<63, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({boundary,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<64, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geomToHEX_buf,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<65, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({pointOnSurface,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<66, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKTWriter_create}, _From, State) ->
        Value = term_to_binary({}),
        Message = <<67, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({geom_createCollection,Int1,GeometryList2,Int3}, _From, State) ->
	%pop geometry
		Value = term_to_binary({Int1,GeometryList2,Int3}),
		Message = <<68, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geomToWKT,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<69, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({setWKBByteOrder,Int1}, _From, State) ->
        Value = term_to_binary({Int1}),
        Message = <<70, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({wKBWriter_getByteOrde,Wkbwriter1}, _From, State) ->
		Value = term_to_binary({Wkbwriter1}),
		Message = <<71, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKTReader_read,Wktreader1,Char2}, _From, State) ->
		Value = term_to_binary({Wktreader1,Char2}),
		Message = <<72, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({difference,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<73, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({version}, _From, State) ->
        Value = term_to_binary({}),
        Message = <<74, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({coordSeq_setOrdinate,Coordsequence1,Int2,Int3,Double4}, _From, State) ->
		Value = term_to_binary({Coordsequence1,Int2,Int3,Double4}),
		Message = <<75, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({getNumGeometries,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<76, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({coordSeq_getX,Coordsequence1,Int2}, _From, State) ->
		Value = term_to_binary({Coordsequence1,Int2}),
		Message = <<77, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({crosses,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<78, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({lineMerge,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<79, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({contains,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<80, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKTWriter_write,Wktwriter1,Geometry2}, _From, State) ->
		Value = term_to_binary({Wktwriter1,Geometry2}),
		Message = <<81, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({preparedCovers,Preparedgeometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Preparedgeometry1,Geometry2}),
		Message = <<82, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};

handle_call({getExteriorRing,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<83, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({setWKBOutputDims,Int1}, _From, State) ->
        Value = term_to_binary({Int1}),
        Message = <<84, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({wKTWriter_destroy,Wktwriter1}, _From, State) ->
        		Value = term_to_binary({Wktwriter1}),
        		Message = <<85, Value/binary>>,
        		Reply = send_command(State#state.port, Message),
        		{reply,Reply,State};


handle_call({polygonize,Geometry1,Int2}, _From, State) ->
		Value = term_to_binary({Geometry1,Int2}),
		Message = <<86, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBWriter_write,Wkbwriter1,Geometry2}, _From, State) ->
		Value = term_to_binary({Wkbwriter1,Geometry2}),
		Message = <<87, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geom_createLineString,Coordsequence1}, _From, State) ->
	%pop coord seq
		Value = term_to_binary({Coordsequence1}),
		Message = <<88, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBWriter_setIncludeSRID,Wkbwriter1,Char2}, _From, State) ->
		Value = term_to_binary({Wkbwriter1,Char2}),
		Message = <<89, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({geomFromWKT,Char1}, _From, State) ->
        Value = term_to_binary({Char1}),
        Message = <<90, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({coordSeq_setX,Coordsequence1,Int2,Double3}, _From, State) ->
		Value = term_to_binary({Coordsequence1,Int2,Double3}),
		Message = <<91, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({isEmpty,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<92, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({unionCascaded,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<93, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({setSRID,Geometry1,Int2}, _From, State) ->
		Value = term_to_binary({Geometry1,Int2}),
		Message = <<94, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({length,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<95, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBWriter_destroy,Wkbwriter1}, _From, State) ->
        		Value = term_to_binary({Wkbwriter1}),
        		Message = <<96, Value/binary>>,
       			Reply = send_command(State#state.port, Message),
        		{reply,Reply,State};


handle_call({geom_createPoint,Coordsequence1}, _From, State) ->
		Value = term_to_binary({Coordsequence1}),
		Message = <<97, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({polygonizer_getCutEdges,Geometry1,Int2}, _From, State) ->
		Value = term_to_binary({Geometry1,Int2}),
		Message = <<98, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({getInteriorRingN,Geometry1,Int2}, _From, State) ->
		Value = term_to_binary({Geometry1,Int2}),
		Message = <<99, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({getWKBOutputDims}, _From, State) ->
        Value = term_to_binary({}),
        Message = <<100, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call({union,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<101, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};

handle_call({geom_destroy,Geometry1}, _From, State) ->
        		Value = term_to_binary({Geometry1}),
        		Message = <<102, Value/binary>>,
        		Reply = send_command(State#state.port, Message),
        		{reply,Reply,State};


handle_call({getSRID,Geometry1}, _From, State) ->
		Value = term_to_binary({Geometry1}),
		Message = <<103, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({disjoint,Geometry1,Geometry2}, _From, State) ->
		Value = term_to_binary({Geometry1,Geometry2}),
		Message = <<104, Value/binary>>,
		Reply = send_command(State#state.port, Message),
		{reply,Reply,State};


handle_call({wKBReader_create}, _From, State) ->
        Value = term_to_binary({}),
        Message = <<105, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};

handle_call({terms_to_coordseq, List}, _From, State) ->
        Value = term_to_binary({List}),
        Message = <<106, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};

handle_call({coordseq_to_terms, Coord}, _From, State) ->
        Value = term_to_binary({Coord}),
        Message = <<107, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};

handle_call({wkt_to_wkb, Wkt}, _From, State) ->
        Value = term_to_binary({Wkt}),
        Message = <<108, Value/binary>>,
        Reply = send_command(State#state.port, Message),
        {reply,Reply,State};


handle_call(_Request, _From, State) ->
    Reply = {error, unknown_call},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, State) ->
    port_close(State#state.port),
    ok;
terminate(_Reason, State) ->
    port_close(State#state.port),
    ok.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_command(Port, Command) ->
    port_command(Port, Command),
    receive
	Data ->
	    Data
    after 1000 ->
	    io:format("Received nothing!~n"),
	    {error, timeout}
    end.

