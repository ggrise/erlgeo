/*
 *
 * Copyright Gabriel Grise <ggrise@gmail.com>
 *
 */
#include "geos_drv.h"


void erlGEOSCoordSeq_getY_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence* arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	double arg3;

	result = GEOSCoordSeq_getY_r(arg0,arg1,arg2,&arg3);

	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	} else
		ERL_WRITE_DOUBLE(command, arg3);
}
void erlGEOSTopologyPreserveSimplify_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	double arg2;
	ERL_READ_DOUBLE(command, arg2);
	result = GEOSTopologyPreserveSimplify_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSisValid_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSisValid_r(arg0,arg1);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSGeom_clone_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGeom_clone_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSCoordSeq_getSize_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence* arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;

	result = GEOSCoordSeq_getSize_r(arg0,arg1,&arg2);

	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	}else
		ERL_WRITE_PTR_UNSIGNED_INT(command, arg2);

}
void erlGEOSIntersection_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSIntersection_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSGeomToWKB_buf_r(GEOSCommand *command) {
	unsigned char * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	size_t arg2;

	result = GEOSGeomToWKB_buf_r(arg0,arg1,&arg2);

	if(result != NULL) {
		send_binary(command, result, arg2);
		free(result);
	} else 
		SEND_GEOS_EXCEPTION(command);
	//ERL_WRITE_PTR_UNSIGNED_CHAR(command, result);
	//ERL_WRITE_PTR_SIZE_T(command, arg2);
}
void erlGEOSWKBReader_read_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBReader * arg1;
	ERL_READ_PTR_GEOSWKBREADER(command, arg1);
	unsigned char* arg2;
	ERL_READ_PTR_UNSIGNED_CHAR(command, arg2);
	size_t arg3;

	if(next_size_t(command, &arg3)) { 
		ERL_READ_ERROR(command, "readsizet");
		free(arg2);
		return;
	}
	result = GEOSWKBReader_read_r(arg0,arg1,arg2,arg3);
	free(arg2);
	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);

}
void erlGEOSGeom_createPolygon_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t context = command->driver_data->handle;
	GEOSGeometry *shell;
	ERL_READ_PTR_GEOSGEOMETRY(command, shell);

	unsigned int nholes;
	ERL_READ_UNSIGNED_INT(command, nholes);

	GEOSGeometry** holes;
	ERL_READ_PTR_PTR_GEOSGEOMETRY(command, holes);
	
	result = GEOSGeom_createPolygon_r(context,shell,holes,nholes);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
	free(holes);
}
/*void erlfinishGEOS_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	finishGEOS_r(arg0)
}*/
void erlGEOSIntersects_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSIntersects_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSWithin_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSWithin_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSPreparedContainsProperly_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSPreparedGeometry* arg1;
	ERL_READ_PTR_GEOSPREPAREDGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSPreparedContainsProperly_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOS_getWKBByteOrder_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	result = GEOS_getWKBByteOrder_r(arg0);

	ERL_WRITE_INT(command, result);
}
void erlGEOSGeom_getCoordSeq_r(GEOSCommand *command) {
	const GEOSCoordSequence* result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGeom_getCoordSeq_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSCOORDSEQUENCE(command, result);
}
void erlGEOSPreparedContains_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSPreparedGeometry* arg1;
	ERL_READ_PTR_GEOSPREPAREDGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSPreparedContains_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSWKBWriter_writeHEX_r(GEOSCommand *command) {
	unsigned char * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter * arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	size_t arg3;

	result = GEOSWKBWriter_writeHEX_r(arg0,arg1,arg2,&arg3);

	if(result != NULL) {
		send_binary(command, result, arg3);
		free(result);
	} else 
		SEND_GEOS_EXCEPTION(command);

//	ERL_WRITE_PTR_UNSIGNED_CHAR(command, result);
//	ERL_WRITE_PTR_SIZE_T(command, arg3);
}
void erlGEOSRelatePattern_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	char* arg3;
	ERL_READ_PTR_CHAR(command, arg3);
	result = GEOSRelatePattern_r(arg0,arg1,arg2,arg3);
	free(arg3);
	ERL_WRITE_CHAR(command, result);
}
void erlGEOSCoordSeq_create_r(GEOSCommand *command) {
	GEOSCoordSequence * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	unsigned int arg1;
	ERL_READ_UNSIGNED_INT(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	result = GEOSCoordSeq_create_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSCOORDSEQUENCE(command, result);
}
void erlGEOSTouches_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSTouches_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSDistance_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	double arg3;

	result = GEOSDistance_r(arg0,arg1,arg2,&arg3);
	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	} else
		ERL_WRITE_DOUBLE(command, arg3);
}
void erlGEOSArea_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	double arg2;

	result = GEOSArea_r(arg0,arg1,&arg2);

	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	} else
		ERL_WRITE_DOUBLE(command, arg2);
}
void erlGEOSisSimple_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSisSimple_r(arg0,arg1);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSGeomFromHEX_buf_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	unsigned char* arg1;
	ERL_READ_PTR_UNSIGNED_CHAR(command, arg1);
	size_t arg2;

	if(next_size_t(command, &arg2)) { 
		ERL_READ_ERROR(command, "readsizet");
		free(arg1);
		return;
	}

	result = GEOSGeomFromHEX_buf_r(arg0,arg1,arg2);
	free(arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSCoordSeq_setY_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence * arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	double arg3;
	ERL_READ_DOUBLE(command, arg3);
	result = GEOSCoordSeq_setY_r(arg0,arg1,arg2,arg3);

	ERL_WRITE_INT(command, result);
}
void erlGEOSSimplify_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	double arg2;
	ERL_READ_DOUBLE(command, arg2);
	result = GEOSSimplify_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSGetNumCoordinates_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGetNumCoordinates_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSCoordSeq_clone_r(GEOSCommand *command) {
	GEOSCoordSequence * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence* arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	result = GEOSCoordSeq_clone_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSCOORDSEQUENCE(command, result);
}
void erlGEOSGeom_createLinearRing_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence * arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	result = GEOSGeom_createLinearRing_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSRelate_r(GEOSCommand *command) {
	char * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSRelate_r(arg0,arg1,arg2);
	if(result == NULL) {
		SEND_GEOS_EXCEPTION(command);
	} else {
		ERL_WRITE_PTR_CHAR(command, result);
		free(result);
	}
}
void erlGEOSWKTReader_destroy_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKTReader * arg1;
	ERL_READ_PTR_GEOSWKTREADER(command, arg1);
	GEOSWKTReader_destroy_r(arg0,arg1);
	send_ok(command);	
}
void erlGEOSWKBWriter_setOutputDimension_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter * arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	int arg2;
	ERL_READ_INT(command, arg2);
	GEOSWKBWriter_setOutputDimension_r(arg0,arg1,arg2);
}
void erlGEOSBuffer_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	double arg2;
	ERL_READ_DOUBLE(command, arg2);
	int arg3;
	ERL_READ_INT(command, arg3);
	result = GEOSBuffer_r(arg0,arg1,arg2,arg3);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSSymDifference_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSSymDifference_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSConvexHull_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSConvexHull_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSWKBWriter_setByteOrder_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter * arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	int arg2;
	ERL_READ_INT(command, arg2);
	GEOSWKBWriter_setByteOrder_r(arg0,arg1,arg2);
	send_ok(command);	
}
void erlGEOSCoordSeq_getZ_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence* arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	double arg3;

	result = GEOSCoordSeq_getZ_r(arg0,arg1,arg2,&arg3);

	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	} else
		ERL_WRITE_DOUBLE(command, arg3);
}
void erlGEOSGeomTypeId_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGeomTypeId_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSWKTReader_create_r(GEOSCommand *command) {
	GEOSWKTReader * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	result = GEOSWKTReader_create_r(arg0);

	ERL_WRITE_PTR_GEOSWKTREADER(command, result);
}
void erlGEOSCoordSeq_getOrdinate_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence* arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	unsigned int arg3;
	ERL_READ_UNSIGNED_INT(command, arg3);
	double arg4;

	result = GEOSCoordSeq_getOrdinate_r(arg0,arg1,arg2,arg3,&arg4);

	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	} else
		ERL_WRITE_DOUBLE(command, arg4);
}
void erlGEOSGetNumInteriorRings_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGetNumInteriorRings_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSWKBReader_destroy_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBReader * arg1;
	ERL_READ_PTR_GEOSWKBREADER(command, arg1);
	GEOSWKBReader_destroy_r(arg0,arg1);
}
void erlGEOSGeom_getDimensions_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGeom_getDimensions_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSWKBWriter_getIncludeSRID_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter* arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	result = GEOSWKBWriter_getIncludeSRID_r(arg0,arg1);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSPrepare_r(GEOSCommand *command) {
	const GEOSPreparedGeometry* result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSPrepare_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSPREPAREDGEOMETRY(command, result);
}
void erlGEOSEquals_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSEquals_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSOverlaps_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSOverlaps_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSPreparedGeom_destroy_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSPreparedGeometry* arg1;
	ERL_READ_PTR_GEOSPREPAREDGEOMETRY(command, arg1);
	GEOSPreparedGeom_destroy_r(arg0,arg1);
}
void erlGEOSCoordSeq_getDimensions_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence* arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;

	result = GEOSCoordSeq_getDimensions_r(arg0,arg1,&arg2);

	//ERL_WRITE_INT(command, result);
	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	} else
		ERL_WRITE_PTR_UNSIGNED_INT(command, arg2);
}
void erlGEOSisValidReason_r(GEOSCommand *command) {
	char * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSisValidReason_r(arg0,arg1);

	if(result == NULL) {
		SEND_GEOS_EXCEPTION(command);
	} else {
		ERL_WRITE_PTR_CHAR(command, result);
		free(result);
	}
}
void erlGEOSGetCentroid_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGetCentroid_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSWKBReader_readHEX_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBReader * arg1;
	ERL_READ_PTR_GEOSWKBREADER(command, arg1);
	unsigned char* arg2;
	ERL_READ_PTR_UNSIGNED_CHAR(command, arg2);
	size_t arg3;

	if(next_size_t(command, &arg3)) { 
		ERL_READ_ERROR(command, "readsizet");
		free(arg2);
		return;
	}

	result = GEOSWKBReader_readHEX_r(arg0,arg1,arg2,arg3);
	free(arg2);
	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSCoordSeq_destroy_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence * arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	GEOSCoordSeq_destroy_r(arg0,arg1);
	send_ok(command);	
}
void erlGEOSCoordSeq_setZ_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence * arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	double arg3;
	ERL_READ_DOUBLE(command, arg3);
	result = GEOSCoordSeq_setZ_r(arg0,arg1,arg2,arg3);

	ERL_WRITE_INT(command, result);
}
void erlGEOSHasZ_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSHasZ_r(arg0,arg1);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSEnvelope_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSEnvelope_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSGeomFromWKB_buf_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	unsigned char* arg1;
	ERL_READ_PTR_UNSIGNED_CHAR(command, arg1);
	size_t arg2;

	if(next_size_t(command, &arg2)) { 
		ERL_READ_ERROR(command, "readsizet");
		free(arg1);
		return;
	}

	result = GEOSGeomFromWKB_buf_r(arg0,arg1,arg2);
	free(arg1);
	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSisRing_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSisRing_r(arg0,arg1);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSGetGeometryN_r(GEOSCommand *command) {
	const GEOSGeometry* result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	int arg2;
	ERL_READ_INT(command, arg2);
	result = GEOSGetGeometryN_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, (GEOSGeometry *) result);
}
void erlGEOSPreparedIntersects_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSPreparedGeometry* arg1;
	ERL_READ_PTR_GEOSPREPAREDGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSPreparedIntersects_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSWKBWriter_getOutputDimension_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter* arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	result = GEOSWKBWriter_getOutputDimension_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSWKBWriter_create_r(GEOSCommand *command) {
	GEOSWKBWriter * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	result = GEOSWKBWriter_create_r(arg0);

	ERL_WRITE_PTR_GEOSWKBWRITER(command, result);
}
void erlGEOSNormalize_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry * arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSNormalize_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
/*void erlinitGEOS_r(GEOSCommand *command) {
	GEOSContextHandle_t result;

	GEOSMessageHandler arg0;
	ERL_READ_GEOSMESSAGEHANDLER(command,arg0);
	GEOSMessageHandler arg1;
	ERL_READ_GEOSMESSAGEHANDLER(command,arg1);
	result = initGEOS_r(arg0,arg1);

	ERL_WRITE_GEOSCONTEXTHANDLE_T(command, result);
}*/
void erlGEOSGeomType_r(GEOSCommand *command) {
	char * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGeomType_r(arg0,arg1);

	if(result == NULL) {
		SEND_GEOS_EXCEPTION(command);
	} else {
		ERL_WRITE_PTR_CHAR(command, result);
		free(result);
	}
}
void erlGEOSEqualsExact_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	double arg3;
	ERL_READ_DOUBLE(command, arg3);
	result = GEOSEqualsExact_r(arg0,arg1,arg2,arg3);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSBoundary_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSBoundary_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSGeomToHEX_buf_r(GEOSCommand *command) {
	unsigned char *result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	size_t arg2;

	result = GEOSGeomToHEX_buf_r(arg0,arg1,&arg2);

	if(result != NULL) {
		send_binary(command, result, arg2);
		free(result);
	} else 
		SEND_GEOS_EXCEPTION(command);

//	ERL_WRITE_PTR_UNSIGNED_CHAR(command, result);
//	ERL_WRITE_PTR_SIZE_T(command, arg2);
}
void erlGEOSPointOnSurface_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSPointOnSurface_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSWKTWriter_create_r(GEOSCommand *command) {
	GEOSWKTWriter * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	result = GEOSWKTWriter_create_r(arg0);

	ERL_WRITE_PTR_GEOSWKTWRITER(command, result);
}
void erlGEOSGeom_createCollection_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	int arg1;
	ERL_READ_INT(command, arg1);
	GEOSGeometry * * arg2;
	ERL_READ_PTR_PTR_GEOSGEOMETRY(command, arg2);
	unsigned int arg3;
	ERL_READ_UNSIGNED_INT(command, arg3);
	result = GEOSGeom_createCollection_r(arg0,arg1,arg2,arg3);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
	free(arg2);
}
void erlGEOSGeomToWKT_r(GEOSCommand *command) {
	char * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGeomToWKT_r(arg0,arg1);

	if(result == NULL) {	
		SEND_GEOS_EXCEPTION(command);
	} else {
		ERL_WRITE_PTR_CHAR(command, result);
		free(result);
	}
}
void erlGEOS_setWKBByteOrder_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	int arg1;
	ERL_READ_INT(command, arg1);
	result = GEOS_setWKBByteOrder_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSWKBWriter_getByteOrder_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter* arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	result = GEOSWKBWriter_getByteOrder_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSWKTReader_read_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKTReader * arg1;
	ERL_READ_PTR_GEOSWKTREADER(command, arg1);
	char* arg2;
	ERL_READ_PTR_CHAR(command, arg2);
	result = GEOSWKTReader_read_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSDifference_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSDifference_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
/*void erlGEOSFree_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	void * arg1;

	GEOSFree_r(arg0,arg1)

	ERL_WRITE_PTR_VOID(command, arg1);
}*/
void erlGEOSversion(GEOSCommand *command) {
	const char * result;

	result = GEOSversion();

	ERL_WRITE_PTR_CHAR(command, result);
}
void erlGEOSCoordSeq_setOrdinate_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence * arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	unsigned int arg3;
	ERL_READ_UNSIGNED_INT(command, arg3);
	double arg4;
	ERL_READ_DOUBLE(command, arg4);
	result = GEOSCoordSeq_setOrdinate_r(arg0,arg1,arg2,arg3,arg4);

	ERL_WRITE_INT(command, result);
}
void erlGEOSGetNumGeometries_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGetNumGeometries_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSCoordSeq_getX_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence* arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	double arg3;

	result = GEOSCoordSeq_getX_r(arg0,arg1,arg2,&arg3);

	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	} else
		ERL_WRITE_DOUBLE(command, arg3);
}
void erlGEOSCrosses_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSCrosses_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSLineMerge_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSLineMerge_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSContains_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSContains_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSWKTWriter_write_r(GEOSCommand *command) {
	char * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKTWriter * arg1;
	ERL_READ_PTR_GEOSWKTWRITER(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSWKTWriter_write_r(arg0,arg1,arg2);

	if(result == NULL) {
		SEND_GEOS_EXCEPTION(command);
	} else {
		ERL_WRITE_PTR_CHAR(command, result);
		free(result);
	}
}
void erlGEOSPreparedCovers_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSPreparedGeometry* arg1;
	ERL_READ_PTR_GEOSPREPAREDGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSPreparedCovers_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSWKTReader_read(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSWKTReader * arg0;
	ERL_READ_PTR_GEOSWKTREADER(command, arg0);
	char* arg1;
	ERL_READ_PTR_CHAR(command, arg1);
	result = GEOSWKTReader_read(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSGetExteriorRing_r(GEOSCommand *command) {
	const GEOSGeometry* result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGetExteriorRing_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, (GEOSGeometry *) result);
}
void erlGEOS_setWKBOutputDims_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	int arg1;
	ERL_READ_INT(command, arg1);
	result = GEOS_setWKBOutputDims_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSWKTWriter_destroy_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKTWriter * arg1;
	ERL_READ_PTR_GEOSWKTWRITER(command, arg1);
	GEOSWKTWriter_destroy_r(arg0,arg1);
	send_ok(command);	
}
void erlGEOSPolygonize_r(GEOSCommand *command) {
	const GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	const GEOSGeometry** arg1;
	ERL_READ_PTR_PTR_GEOSGEOMETRY(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	result = GEOSPolygonize_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, (GEOSGeometry *) result);
	free(arg1);	
}
void erlGEOSWKBWriter_write_r(GEOSCommand *command) {
	unsigned char * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter * arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	size_t arg3;

	result = GEOSWKBWriter_write_r(arg0,arg1,arg2,&arg3);

	if(result != NULL) {
		send_binary(command, result, arg3);
		free(result);
	} else 
		SEND_GEOS_EXCEPTION(command);

	//ERL_WRITE_PTR_UNSIGNED_CHAR(command, result);
	//ERL_WRITE_PTR_SIZE_T(command, arg3);
}
void erlGEOSGeom_createLineString_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence * arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	result = GEOSGeom_createLineString_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSWKBWriter_setIncludeSRID_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter * arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	char arg2;
	ERL_READ_CHAR(command, arg2);
	GEOSWKBWriter_setIncludeSRID_r(arg0,arg1,arg2);
	send_ok(command);	
}
void erlGEOSGeomFromWKT_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	char* arg1;
	ERL_READ_PTR_CHAR(command, arg1);
	result = GEOSGeomFromWKT_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSCoordSeq_setX_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence * arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	double arg3;
	ERL_READ_DOUBLE(command, arg3);
	result = GEOSCoordSeq_setX_r(arg0,arg1,arg2,arg3);

	ERL_WRITE_INT(command, result);
}
void erlGEOSisEmpty_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSisEmpty_r(arg0,arg1);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSUnionCascaded_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSUnionCascaded_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSSetSRID_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry * arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	int arg2;
	ERL_READ_INT(command, arg2);
	GEOSSetSRID_r(arg0,arg1,arg2);
	send_ok(command);
}
void erlGEOSLength_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	double arg2;

	result = GEOSLength_r(arg0,arg1,&arg2);

	if(!result) {
		SEND_GEOS_EXCEPTION(command);
	} else
		ERL_WRITE_DOUBLE(command, arg2);
}
void erlGEOSWKBWriter_destroy_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSWKBWriter * arg1;
	ERL_READ_PTR_GEOSWKBWRITER(command, arg1);
	GEOSWKBWriter_destroy_r(arg0,arg1);
	send_ok(command);	
}
void erlGEOSGeom_createPoint_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSCoordSequence * arg1;
	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, arg1);
	result = GEOSGeom_createPoint_r(arg0,arg1);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSPolygonizer_getCutEdges_r(GEOSCommand *command) {
	const GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	const GEOSGeometry** arg1;
	ERL_READ_PTR_PTR_GEOSGEOMETRY(command, arg1);
	unsigned int arg2;
	ERL_READ_UNSIGNED_INT(command, arg2);
	result = GEOSPolygonizer_getCutEdges_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, (GEOSGeometry *) result);
	free(arg1);
}
void erlGEOSGetInteriorRingN_r(GEOSCommand *command) {
	const GEOSGeometry* result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	int arg2;
	ERL_READ_INT(command, arg2);
	result = GEOSGetInteriorRingN_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, (GEOSGeometry *) result);
}
void erlGEOS_getWKBOutputDims_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	result = GEOS_getWKBOutputDims_r(arg0);

	ERL_WRITE_INT(command, result);
}
void erlGEOSUnion_r(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSUnion_r(arg0,arg1,arg2);

	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSWKBReader_read(GEOSCommand *command) {
	GEOSGeometry * result;

	GEOSWKBReader * arg0;
	ERL_READ_PTR_GEOSWKBREADER(command, arg0);
	unsigned char* arg1;
	ERL_READ_PTR_UNSIGNED_CHAR(command, arg1);
	size_t arg2;

	if(next_size_t(command, &arg2)) { 
		ERL_READ_ERROR(command, "readsizet");
		free(arg1);
		return;
	}

	result = GEOSWKBReader_read(arg0,arg1,arg2);
	free(arg1);
	ERL_WRITE_PTR_GEOSGEOMETRY(command, result);
}
void erlGEOSGeom_destroy_r(GEOSCommand *command) {
	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry * arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeom_destroy_r(arg0,arg1);
	send_ok(command);	
}
void erlGEOSGetSRID_r(GEOSCommand *command) {
	int result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	result = GEOSGetSRID_r(arg0,arg1);

	ERL_WRITE_INT(command, result);
}
void erlGEOSDisjoint_r(GEOSCommand *command) {
	char result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	GEOSGeometry* arg1;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg1);
	GEOSGeometry* arg2;
	ERL_READ_PTR_GEOSGEOMETRY(command, arg2);
	result = GEOSDisjoint_r(arg0,arg1,arg2);

	ERL_WRITE_CHAR(command, result);
}
void erlGEOSWKBReader_create_r(GEOSCommand *command) {
	GEOSWKBReader * result;

	GEOSContextHandle_t arg0 = command->driver_data->handle;
	result = GEOSWKBReader_create_r(arg0);

	ERL_WRITE_PTR_GEOSWKBREADER(command, result);
}
