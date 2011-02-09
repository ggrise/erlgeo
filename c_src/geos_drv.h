#include <geos_c.h>
#include <ei.h>
#include <erl_driver.h>


typedef struct _geos_drv_t {
  ErlDrvPort port;
  GEOSContextHandle_t handle;
} geos_drv_t;

typedef struct _GEOSCommand {
        geos_drv_t *driver_data;
        int type;
        int argc;
        int index;
        char *param_bytes;
} GEOSCommand;

typedef struct _GEOSFunction {
        void (*function)(GEOSCommand *); 
        int argc;
} GEOSFunction;

#include "erl_geos_macro.h"
#include "erl_geos_util.h"
#include "erl_geos_func.h"
