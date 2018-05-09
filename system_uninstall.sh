#!/bin/sh

BINDIR=/usr/local/bin/

FILES="mockmma mixima miximamma mmatomax mmatomax_int"

for f in $FILES ; do
    echo rm -f ${BINDIR}$f
    rm -f ${BINDIR}$f
done

echo rm -rf /usr/share/maxima/*/share/mixima/
rm -rf /usr/share/maxima/*/share/mixima/
