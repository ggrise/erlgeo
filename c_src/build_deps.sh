#!/bin/bash
set -e

#GEOS_VSN=3.2.2
GEOS_VSN=3.2.3

if [ `basename $PWD` != "c_src" ]; then
    pushd c_src
fi

BASEDIR="$PWD"

case "$1" in
    clean)
        rm -rf  geos-$GEOS_VSN
        ;;
    *)
        tar -xjf geos-$GEOS_VSN.tar.bz2
		if [ -e patches/ ]; then
			for patch in patches/*; do
				echo $patch
				patch -p0 < $patch
			done
		fi

        (cd geos-$GEOS_VSN && \
            ./configure --disable-shared --enable-static --with-pic \
                        --prefix=$BASEDIR/geos && \
            make && make install)

        ;;
esac
