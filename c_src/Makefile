GCC=gcc
LDFLAGS=-undefined dynamic_lookup -shared -Wall -g -lgeos_c -lei
SRCS=`ls *.c`
OUTPUT=geos_drv.so
CFLAGS=-o ${OUTPUT}
ERL_PATH=/Users/ggrise/erl14/
ERL_INC=${ERL_PATH}/lib/erlang/usr/include/


all:
	${GCC} -Lgeos/lib \
	-L${ERL_PATH}/lib/erlang/lib/erl_interface-3.7.4/lib/ \
	-I${ERL_PATH}/lib/erlang/usr/include \
	-I${ERL_PATH}/lib/erlang/lib/erl_interface-3.7.4/include/ -Igeos/include ${SRCS} ${LDFLAGS} ${CFLAGS}

clean:
	rm -rf ${OUTPUT}
