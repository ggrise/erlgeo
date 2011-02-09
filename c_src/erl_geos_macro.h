
#define ERL_READ_ERROR(command, type) \
do { erl_send_error(command, type); return; } while(0)


/* READ macro
 * command is GEOSCommand variable, 
 * param the placeholder of the readed parameter
 **/


#define ERL_READ_CHAR(command,param) \
do { if(next_char(command, &param)) { ERL_READ_ERROR(command, "readchar"); }} while(0)

#define ERL_READ_DOUBLE(command,param) \
do { if(next_double(command, &param)) { ERL_READ_ERROR(command, "readdouble"); }} while(0)

#define ERL_READ_INT(command,param) \
do { if(next_int(command, &param)) { ERL_READ_ERROR(command, "readint"); }} while(0)

/*
 * the char *, can be considered as string
 */
#define ERL_READ_PTR_CHAR(command,param) \
do { if(next_string(command, &param)) { ERL_READ_ERROR(command, "readstr"); }} while(0)

#define ERL_READ_PTR_GEOSCOORDSEQUENCE(command,param) \
do { if(next_coordsequence(command, &param)) { ERL_READ_ERROR(command, "readcoordseq"); }} while(0)

#define ERL_READ_PTR_GEOSGEOMETRY(command,param) \
do { if(next_geometry(command, &param)) { ERL_READ_ERROR(command, "readgeom"); }} while(0)

#define ERL_READ_PTR_GEOSPREPAREDGEOMETRY(command,param) \
do { if(next_prepared_geometry(command, &param)) { ERL_READ_ERROR(command, "readprepgeom"); }} while(0)

#define ERL_READ_PTR_GEOSWKBREADER(command,param) \
do { if(next_wkbreader(command, &param)) { ERL_READ_ERROR(command, "readwkbreader"); }} while(0)

#define ERL_READ_PTR_GEOSWKBWRITER(command,param) \
do { if(next_wkbwriter(command, &param)) { ERL_READ_ERROR(command, "readwkbwriter"); }} while(0)

#define ERL_READ_PTR_GEOSWKTREADER(command,param) \
do { int r = next_wktreader(command, &param); if(r) { ERL_READ_ERROR(command, "readwktreader"); }} while(0)

#define ERL_READ_PTR_GEOSWKTWRITER(command,param) \
do { if(next_wktwriter(command, &param)) { ERL_READ_ERROR(command, "readwktwriter"); }} while(0)

#define ERL_READ_PTR_PTR_GEOSGEOMETRY(command,param) \
do { if(next_geometry_list(command, (GEOSGeometry ***) &param)) { ERL_READ_ERROR(command, "readgeomlist"); }} while(0)

/* this is used has a binary array */
#define ERL_READ_PTR_UNSIGNED_CHAR(command,param) \
do { if(next_string(command, (char **) &param)) { ERL_READ_ERROR(command, "readbinary"); }} while(0)

#define ERL_READ_SIZE_T(command,param) \
do { if(next_size_t(command, &param)) { ERL_READ_ERROR(command, "readsizet"); }} while(0)

#define ERL_READ_UNSIGNED_INT(command,param) \
do { int __value; if(next_int(command, &__value)) { ERL_READ_ERROR(command, "readuint"); } param = (unsigned int) __value; } while(0)

#define ERL_WRITE_CHAR(command,param) \
send_boolean(command,param)

#define ERL_WRITE_INT(command,param) \
send_int(command, param)
 
#define ERL_WRITE_PTR_CHAR(command,param) \
send_string(command, param) 

#define ERL_WRITE_DOUBLE(command,param) \
send_double(command, param)

#define ERL_WRITE_PTR_GEOSCOORDSEQUENCE(command,param) \
send_pointer(command, "gcoordseq", (void *) param)

#define ERL_WRITE_PTR_GEOSGEOMETRY(command,param) \
send_geometry(command, param)

#define ERL_WRITE_PTR_GEOSPREPAREDGEOMETRY(command,param) \
send_pointer(command, "gpreparedgeom", (void *) param)

#define ERL_WRITE_PTR_GEOSWKBREADER(command,param) \
send_pointer(command, "gwkbreader", (void *) param)

#define ERL_WRITE_PTR_GEOSWKBWRITER(command,param) \
send_pointer(command, "gwkbwriter", (void *) param)

#define ERL_WRITE_PTR_GEOSWKTREADER(command,param) \
send_pointer(command, "gwktreader", (void *) param)

#define ERL_WRITE_PTR_GEOSWKTWRITER(command,param) \
send_pointer(command, "gwktwriter", (void *) param)

#define ERL_WRITE_PTR_UNSIGNED_INT(command,param) \
send_int(command, param)

#define SEND_GEOS_EXCEPTION(command) \
erl_send_error(command, "exception")
