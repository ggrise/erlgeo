/*
 *  
 * Copyright Gabriel Grise <ggrise@gmail.com>
 * 
 **/

#include "geos_drv.h"

static ErlDrvData start(ErlDrvPort port, char* cmd);
static void stop(ErlDrvData handle);
static void outputv(ErlDrvData handle, ErlIOVec *ev);

static ErlDrvEntry geos_driver_entry = {
    NULL, /* init */
    start, /* startup (defined below) */
    stop, /* shutdown (defined below) */
    NULL, /* output */
    NULL, /* ready_input */
    NULL, /* ready_output */
    "geos_drv", /* the name of the driver */
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    outputv, /* outputv (defined below) */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER, /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING /* ERL_DRV_FLAGs */
};
DRIVER_INIT(geos_driver) {
  return &geos_driver_entry;
} 

static void
geos_error (const char *fmt, ...)
{
	//i would like to send this error to
	//erlang, but there is no way to 
	//have the erlang port here
}


static ErlDrvData start(ErlDrvPort port, char* cmd) {

	geos_drv_t* retval = (geos_drv_t*) driver_alloc(sizeof(geos_drv_t));
	retval->port = port;
  	retval->handle = initGEOS_r(geos_error,geos_error);
    
  return (ErlDrvData) retval;
}   
    
    
static void stop(ErlDrvData handle) {

	geos_drv_t* driver_data = (geos_drv_t*) handle;
	finishGEOS_r(driver_data->handle);
	driver_free(driver_data);
}
static void erlWKT2WKB(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	char* arg1;
	unsigned char *wkb;
	size_t size;

	ERL_READ_PTR_CHAR(command, arg1);
	result = GEOSGeomFromWKT_r(arg0,arg1);
	if(result == NULL) {
		free(arg1);
		ERL_READ_ERROR(command, "invalidwkt");
	}
	free(arg1);
	
	wkb = GEOSGeomToWKB_buf_r(arg0,result,&size);
	if(result != NULL) {
		send_binary(command, wkb, size);
		free(wkb);
	} else 
		SEND_GEOS_EXCEPTION(command);
}
GEOSFunction cmd_functions[109] = {

	{erlGEOSCoordSeq_getY_r, 2},
	{erlGEOSTopologyPreserveSimplify_r, 2},
	{erlGEOSisValid_r, 1},
	{erlGEOSGeom_clone_r, 1},
	{erlGEOSCoordSeq_getSize_r, 1},
	{erlGEOSIntersection_r, 2},
	{erlGEOSGeomToWKB_buf_r, 1},
	{erlGEOSWKBReader_read_r, 3},
	{erlGEOSGeom_createPolygon_r, 3},
	{erlGEOSIntersects_r, 2},
	{erlGEOSWithin_r, 2},
	{erlGEOSPreparedContainsProperly_r, 2},
	{erlGEOS_getWKBByteOrder_r, 0},
	{erlGEOSGeom_getCoordSeq_r, 1},
	{erlGEOSPreparedContains_r, 2},
	{erlGEOSWKBWriter_writeHEX_r, 2},
	{erlGEOSRelatePattern_r, 3},
	{erlGEOSCoordSeq_create_r, 2},
	{erlGEOSTouches_r, 2},
	{erlGEOSDistance_r, 2},
	{erlGEOSArea_r, 1},
	{erlGEOSisSimple_r, 1},
	{erlGEOSGeomFromHEX_buf_r, 2},
	{erlGEOSCoordSeq_setY_r, 3},
	{erlGEOSSimplify_r, 2},
	{erlGEOSGetNumCoordinates_r, 1},
	{erlGEOSCoordSeq_clone_r, 1},
	{erlGEOSGeom_createLinearRing_r, 1},
	{erlGEOSRelate_r, 2},
	{erlGEOSWKTReader_destroy_r, 1},
	{erlGEOSWKBWriter_setOutputDimension_r, 2},
	{erlGEOSBuffer_r, 3},
	{erlGEOSSymDifference_r, 2},
	{erlGEOSConvexHull_r, 1},
	{erlGEOSWKBWriter_setByteOrder_r, 2},
	{erlGEOSCoordSeq_getZ_r, 2},
	{erlGEOSGeomTypeId_r, 1},
	{erlGEOSWKTReader_create_r, 0},
	{erlGEOSCoordSeq_getOrdinate_r, 3},
	{erlGEOSGetNumInteriorRings_r, 1},
	{erlGEOSWKBReader_destroy_r, 1},
	{erlGEOSGeom_getDimensions_r, 1},
	{erlGEOSWKBWriter_getIncludeSRID_r, 1},
	{erlGEOSPrepare_r, 1},
	{erlGEOSEquals_r, 2},
	{erlGEOSOverlaps_r, 2},
	{erlGEOSPreparedGeom_destroy_r, 1},
	{erlGEOSCoordSeq_getDimensions_r, 1},
	{erlGEOSisValidReason_r, 1},
	{erlGEOSGetCentroid_r, 1},
	{erlGEOSWKBReader_readHEX_r, 3},
	{erlGEOSCoordSeq_destroy_r, 1},
	{erlGEOSCoordSeq_setZ_r, 3},
	{erlGEOSHasZ_r, 1},
	{erlGEOSEnvelope_r, 1},
	{erlGEOSGeomFromWKB_buf_r, 2},
	{erlGEOSisRing_r, 1},
	{erlGEOSGetGeometryN_r, 2},
	{erlGEOSPreparedIntersects_r, 2},
	{erlGEOSWKBWriter_getOutputDimension_r, 1},
	{erlGEOSWKBWriter_create_r, 0},
	{erlGEOSNormalize_r, 1},
	{erlGEOSGeomType_r, 1},
	{erlGEOSEqualsExact_r, 3},
	{erlGEOSBoundary_r, 1},
	{erlGEOSGeomToHEX_buf_r, 1},
	{erlGEOSPointOnSurface_r, 1},
	{erlGEOSWKTWriter_create_r, 0},
	{erlGEOSGeom_createCollection_r, 3},
	{erlGEOSGeomToWKT_r, 1},
	{erlGEOS_setWKBByteOrder_r, 1},
	{erlGEOSWKBWriter_getByteOrder_r, 1},
	{erlGEOSWKTReader_read_r, 2},
	{erlGEOSDifference_r, 2},
	{erlGEOSversion, 0},
	{erlGEOSCoordSeq_setOrdinate_r, 4},
	{erlGEOSGetNumGeometries_r, 1},
	{erlGEOSCoordSeq_getX_r, 2},
	{erlGEOSCrosses_r, 2},
	{erlGEOSLineMerge_r, 1},
	{erlGEOSContains_r, 2},
	{erlGEOSWKTWriter_write_r, 2},
	{erlGEOSPreparedCovers_r, 2},
	{erlGEOSGetExteriorRing_r, 1},
	{erlGEOS_setWKBOutputDims_r, 1},
	{erlGEOSWKTWriter_destroy_r, 1},
	{erlGEOSPolygonize_r, 2},
	{erlGEOSWKBWriter_write_r, 2},
	{erlGEOSGeom_createLineString_r, 1},
	{erlGEOSWKBWriter_setIncludeSRID_r, 2},
	{erlGEOSGeomFromWKT_r, 1},
	{erlGEOSCoordSeq_setX_r, 3},
	{erlGEOSisEmpty_r, 1},
	{erlGEOSUnionCascaded_r, 1},
	{erlGEOSSetSRID_r, 2},
	{erlGEOSLength_r, 1},
	{erlGEOSWKBWriter_destroy_r, 1},
	{erlGEOSGeom_createPoint_r, 1},
	{erlGEOSPolygonizer_getCutEdges_r, 2},
	{erlGEOSGetInteriorRingN_r, 2},
	{erlGEOS_getWKBOutputDims_r, 0},
	{erlGEOSUnion_r, 2},
	{erlGEOSGeom_destroy_r, 1},
	{erlGEOSGetSRID_r, 1},
	{erlGEOSDisjoint_r, 2},
	{erlGEOSWKBReader_create_r, 0},
	{terms_to_coordseq,1},
	{coordseq_to_terms,2},
	{erlWKT2WKB, 1}
};
static void invoke(GEOSCommand *command) {

	GEOSFunction finfo;

	//verify if the type correspond to a valid function
	int elements = sizeof(cmd_functions)/(sizeof(GEOSFunction));

	if(command->type < 0 || command->type >= elements) {
		erl_send_error(command, "invalidcmd");
		return;
	}

	//retreive the function
	finfo = cmd_functions[command->type];

	//verify if the command and the selected function have the same
	//number of arguments	
	if(finfo.argc == command->argc) {

		finfo.function(command);

	} else {
		erl_send_error(command, "arguments");
	}
}

static void outputv(ErlDrvData handle, ErlIOVec *vec) {

	GEOSCommand command;
	int version;
	int index = 1;

	command.driver_data = (geos_drv_t*) handle;
	
	ErlDrvBinary* data = vec->binv[1];
	command.type = data->orig_bytes[0];
	

	//remove first byte
	char *buffer = (char *) data->orig_bytes;

	if(ei_decode_version(buffer, &index, &version)) {
		erl_send_error(&command, "erlversion");
		return;	
	}
	//decode tuple
	if(ei_decode_tuple_header(buffer, &index, &command.argc)) {
		erl_send_error(&command, "nottuple");
		return;
	}
	
	command.index = index;
	command.param_bytes = buffer;
	//invoke the function related to this command.
	invoke(&command);
}
