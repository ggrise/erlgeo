/*
 * 
 * Copyright Gabriel Grise <ggrise@gmail.com>
 *   
 **/

#include "geos_drv.h"
#include <string.h>
#include <stdio.h>

int next_char(GEOSCommand *command, char *p) {
	return ei_decode_char(command->param_bytes, &command->index, p);
}
int next_double(GEOSCommand *command, double *value) {
        return ei_decode_double(command->param_bytes, &command->index, value);
}
int next_long(GEOSCommand *command, long *value) {
        return ei_decode_long(command->param_bytes, &command->index, value);
}
int next_int(GEOSCommand *command, int *ivalue) {

	int type;
	int size;

	if(ei_get_type(command->param_bytes, &command->index, &type, &size)) {
                return -1;
        }


	switch(type) {
		case ERL_SMALL_INTEGER_EXT: {
			char cvalue;
			if(ei_decode_char(command->param_bytes, &command->index, &cvalue)) {
				return -1;
			}
			*ivalue = (unsigned char) cvalue;
			return 0;
		}
		case ERL_INTEGER_EXT: {
			long lvalue;
			if(next_long(command, &lvalue)) {
				return -1;
			} 
			*ivalue = (int) lvalue;
			return 0;
		}
		default:
			return -1;
	}
}
int next_number(GEOSCommand *command, double *value) {
	int type;
        int size;
        char *buffer = command->param_bytes;
        int *index = &command->index;
        int ivalue;

	if(ei_get_type(buffer, index, &type, &size)) {
                return -1;
        }

	int ret = 0;
	switch(type) {
		case ERL_SMALL_INTEGER_EXT:
		case ERL_INTEGER_EXT:
			if(next_int(command, &ivalue)) {
				ret = -1;
			} else
				*value = ivalue; break;
		case ERL_FLOAT_EXT:
			ret = next_double(command, value); break;
		default:
			ret = -1;
	}
	return ret;
}

int next_pointer(GEOSCommand *command, int argc, char *name, void** data) {
	int arity;
	char atom[MAXATOMLEN];
	unsigned long pointer;


	//decode tuple
	if(ei_decode_tuple_header(command->param_bytes, &command->index, &arity)) {
		return -1;
	}

	//verify if it's the right label
	if(arity!=argc) {
		return -1;
	}


	//decode pointer label
	if(ei_decode_atom(command->param_bytes,&command->index, atom)) {
		return -1;
	}

	if(strcmp(atom,name)) {
		return -1;
	}
	//decode the pointer
	if(ei_decode_ulong(command->param_bytes, &command->index, &pointer)) {
		return -1;
	}

	*data = (void *) pointer;
	return 0;
}
int next_geometry_list(GEOSCommand *command, GEOSGeometry*** geom) {
	int geoc;
	int i;

	
	if(ei_decode_list_header(command->param_bytes,&command->index, &geoc)) {
		return -1;
	}

	GEOSGeometry** geomlist = (GEOSGeometry **) malloc(geoc * sizeof(GEOSGeometry*));

	if(*geom == NULL) return -1;
	for(i = 0; i < geoc; i++) {
		if(next_geometry(command, &geomlist[i])) {
			free(geomlist);
			return -1;
		}
	}
	*geom = geomlist;

	return 0;
}
int next_geometry(GEOSCommand *command, GEOSGeometry** geom) {
	//long type;
	void *geom_ptr;

	if(next_pointer(command, 2, "ggeom", &geom_ptr)) {
		return -1;
	}
	
	//read the geometry type for sanity purpose
/*	
	if(ei_decode_long(command->param_bytes, &command->index, &type)) {
		return -1;
	}

	switch((int) type) {
		case GEOS_POINT:
    		case GEOS_LINESTRING:
    		case GEOS_LINEARRING:
    		case GEOS_POLYGON:
    		case GEOS_MULTIPOINT:
    		case GEOS_MULTILINESTRING:
    		case GEOS_MULTIPOLYGON:
    		case GEOS_GEOMETRYCOLLECTION: break;
		default:
			return -1;
	}*/
	*geom = (GEOSGeometry *)geom_ptr;

	return 0;
}

int next_prepared_geometry(GEOSCommand *command, GEOSPreparedGeometry** geom) {
	void *geom_ptr;
	if(next_pointer(command, 2, "gpreparedgeom", &geom_ptr)) {
		return -1;
	}
	*geom = (GEOSPreparedGeometry *)geom_ptr;
	return 0;
}
void send_pointer(GEOSCommand *command, char *name, void *pointer) {

  ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
			   ERL_DRV_ATOM, driver_mk_atom(name),
                           ERL_DRV_INT, (ErlDrvTermData) pointer,
                           ERL_DRV_TUPLE, 2,
                           ERL_DRV_TUPLE, 2}; 

  driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));

}

int next_coordsequence(GEOSCommand *command, GEOSCoordSequence** seq) {
	void *seq_ptr;
	if(next_pointer(command, 2, "gcoordseq", &seq_ptr)) {
		return -1;
	}
	*seq = (GEOSCoordSequence *)seq_ptr;
	return 0;
}

int next_wkbreader(GEOSCommand *command, GEOSWKBReader** reader) {
	void *reader_ptr;
	int p = next_pointer(command, 2, "gwkbreader", &reader_ptr);
	if(p) {
		return p;
	}
	*reader = (GEOSWKBReader *)reader_ptr;
	return 0;
}
int next_wktreader(GEOSCommand *command, GEOSWKTReader** reader) {
	void *reader_ptr;
	int p = next_pointer(command, 2, "gwktreader", &reader_ptr);
	if(p) {
		return p;
	}

	*reader = (GEOSWKTReader *)reader_ptr;
	return 0;
}
int next_wkbwriter(GEOSCommand *command, GEOSWKBWriter** writer) {
	void *writer_ptr;
	if(next_pointer(command, 2, "gwkbwriter", &writer_ptr)) {
		return -1;
	}
	*writer = (GEOSWKBWriter *)writer_ptr;
	return 0;
}

int next_wktwriter(GEOSCommand *command, GEOSWKTWriter** writer) {
	void *writer_ptr;
	if(next_pointer(command, 2, "gwktwriter", &writer_ptr)) {
		return -1;
	}
	*writer = (GEOSWKTWriter *)writer_ptr;
	return 0;
}


int next_string(GEOSCommand *command, char **data) {
        char *buffer = command->param_bytes;
        int *index = &command->index;
        int type;
        int size;

        if(ei_get_type(buffer, index, &type, &size)) {
                return -1;
        }

        if(type != ERL_STRING_EXT && type != ERL_BINARY_EXT) {
                return 1;
        }
        char* sdata = (char *) malloc(size);

	if(sdata == NULL) {
		return -1;
	}
        if(type == ERL_STRING_EXT) {
                if(ei_decode_string(buffer, index, sdata)) {
                        free(sdata);
                        return -1;
                }
        } else {
		long s;
                if(ei_decode_binary(buffer, index, sdata, &s)) {
                        free(sdata); return -1;
                }
        }
	*data = sdata;
        return 0;
}
int next_size_t(GEOSCommand *command, size_t *value) {
         return ei_decode_long(command->param_bytes, &command->index, (long *) value);
}

void send_ok(GEOSCommand *command) {
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok")}; 
  driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));

}
void send_int(GEOSCommand *command, long content) {

  ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
                           ERL_DRV_INT, (ErlDrvTermData) content,
                           ERL_DRV_TUPLE, 2}; 

  driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));

}

void send_double(GEOSCommand *command, double content) {

  ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
                           ERL_DRV_FLOAT, (ErlDrvTermData) &content,
                           ERL_DRV_TUPLE, 2}; 

  driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));

}

void send_string(GEOSCommand *command, const char* content) {

  ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
                           ERL_DRV_STRING, (ErlDrvTermData) content, strlen(content),
                           ERL_DRV_TUPLE, 2}; 

  driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));

}
void send_binary(GEOSCommand *command, unsigned char* content, long len) {

  ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
                           ERL_DRV_BUF2BINARY, (ErlDrvTermData)content, len,
                           ERL_DRV_TUPLE, 2}; 

  driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));

}
void send_boolean(GEOSCommand *command, char boolean) {

  ErlDrvTermData atom;	
  switch(boolean) {
	case 0: atom = driver_mk_atom("false"); break;
	case 1: atom = driver_mk_atom("true"); break;
	default:
		atom = driver_mk_atom("error"); break;
  }
  
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
                           ERL_DRV_ATOM, atom, 
                           ERL_DRV_TUPLE, 2}; 

  driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));

}
void send_geometry(GEOSCommand *command, GEOSGeometry *geom) {
	//GEOSContextHandle_t context = command->driver_data->handle;
	//int type = GEOSGeomTypeId_r(context, geom);

	if(geom == NULL) {
		erl_send_error(command, "nullgeom");
		return;
	}
	ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
			   ERL_DRV_ATOM, driver_mk_atom("ggeom"),
			 //  ERL_DRV_INT, (ErlDrvTermData) type,
			   ERL_DRV_INT, (ErlDrvTermData) geom,
			   ERL_DRV_TUPLE, 2,
			   ERL_DRV_TUPLE, 2};

  	driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}
void erl_send_error(GEOSCommand *command, char *atom) {
	
	ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("error"),
                           ERL_DRV_ATOM, driver_mk_atom(atom),
                           ERL_DRV_TUPLE, 2}; 
	driver_output_term(command->driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));

}
void terms_to_coordseq(GEOSCommand *command) {
        char *buffer = command->param_bytes;
        int *index = &command->index;
        int size;
	GEOSCoordSequence *coordSeq = NULL;
	
	GEOSContextHandle_t handle = command->driver_data->handle;
        
	if(ei_decode_list_header(buffer, index, &size)) {
          	erl_send_error(command, "notalist");
		return;
	}

	unsigned int i;
	for(i = 0; i < size; i++) {
		//for each element
		int digitc;
		double x,y,z;

		if(ei_decode_tuple_header(buffer, index, &digitc)) {
          		erl_send_error(command, "invalidlist");
			if(coordSeq!=NULL) GEOSCoordSeq_destroy_r(handle, coordSeq);
			return;
        	}
		if(coordSeq == NULL) {
			coordSeq = GEOSCoordSeq_create_r(handle, size, digitc);
			if(coordSeq == NULL) {
				erl_send_error(command, "create");
				return;
			}
		}
		
		if(next_number(command, &x) || GEOSCoordSeq_setX_r(handle, coordSeq, i, x)==0) {
			erl_send_error(command, "setx");
			GEOSCoordSeq_destroy_r(handle, coordSeq);
			return;

		}	
		if(next_number(command, &y) || GEOSCoordSeq_setY_r(handle, coordSeq, i, y)==0) {
			erl_send_error(command, "sety");
			GEOSCoordSeq_destroy_r(handle, coordSeq);
			return;

		}
		if(digitc == 3) {
			if(next_number(command, &z) || GEOSCoordSeq_setZ_r(handle, coordSeq, i, z)==0) {
				erl_send_error(command, "setz");
				GEOSCoordSeq_destroy_r(handle, coordSeq);
				return;

			}
		}
	}
	ERL_WRITE_PTR_GEOSCOORDSEQUENCE(command, coordSeq);
}
void coordseq_to_terms(GEOSCommand *command) {

	GEOSContextHandle_t handle = command->driver_data->handle;
	GEOSCoordSequence *coordSeq;
	int dims;

	ERL_READ_PTR_GEOSCOORDSEQUENCE(command, coordSeq);
	ERL_READ_INT(command, dims);
	
	unsigned int size;
	if(GEOSCoordSeq_getSize_r(handle, coordSeq, &size)==0) {
		erl_send_error(command, "invalidcs");
		return;
	}
	//if(GEOSCoordSeq_getDimensions_r(handle, coordSeq,&dims)==0) {
	//	erl_send_error(command, "invalidcs");
	//	return;
	//} always return 3
	

	int allocSize = 6 + (dims*2 + 2) * size + 1;


	
	double *digits = driver_alloc(sizeof(double) * dims * size);
	ErlDrvTermData *terms = driver_alloc(sizeof(ErlDrvTermData) * (allocSize));

	terms[0] = ERL_DRV_ATOM;
	terms[1] = driver_mk_atom("ok");

	int pos = 2;
	int i,d = 0;
		
	for(i = 0; i < size; i++) {

		GEOSCoordSeq_getX_r(handle,coordSeq,i, &digits[d]);

		terms[pos++] = ERL_DRV_FLOAT;
		terms[pos++] = (ErlDrvTermData) &digits[d];
		d++;

		if(dims >= 2) {
			GEOSCoordSeq_getY_r(handle,coordSeq,i, &digits[d]);
			terms[pos++] = ERL_DRV_FLOAT;
			terms[pos++] = (ErlDrvTermData) &digits[d];
			d++;
		}
		if(dims >= 3) {
			GEOSCoordSeq_getZ_r(handle,coordSeq,i, &digits[d]);
			terms[pos++] = ERL_DRV_FLOAT;
			terms[pos++] = (ErlDrvTermData) &digits[d];
			d++;
		}
		terms[pos++] = ERL_DRV_TUPLE;
		terms[pos++] = dims;
	
	}

	terms[pos++] = ERL_DRV_NIL;
	terms[pos++] = ERL_DRV_LIST;
	terms[pos++] = size+1;

	terms[pos++] = ERL_DRV_TUPLE;
	terms[pos] = 2;

	
  	driver_output_term(command->driver_data->port, terms, allocSize);

	driver_free(digits);
	driver_free(terms);
}

