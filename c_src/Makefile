GCC=gcc
LDFLAGS=-shared -Wall -g -lgeos_c -lei
SRCS=`ls *.c`
OUTPUT=geos_drv.so
CFLAGS=-o ${OUTPUT}

all:
	${GCC} -L/usr/local/erlang/otp_r13b01/lib/erlang/lib/erl_interface-3.6.2/lib/ -I/usr/local/erlang/otp_r13b01/lib/erlang/lib/erl_interface-3.6.2/include/ -I/usr/local/erlang/otp_r13b01/lib/erlang/usr/include/ -I../../../geos/geos-3.1.1/capi/ ${SRCS} ${LDFLAGS} ${CFLAGS}

clean:
	rm -rf ${OUTPUT}
