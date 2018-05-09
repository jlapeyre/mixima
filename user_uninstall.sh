#!/bin/sh

BINDIR=~/bin/

FILES="mockmma mixima miximamma mmatomax mmatomax_int"

for f in $FILES ; do
    echo rm -f ${BINDIR}$f
    rm -f ${BINDIR}$f
done

echo "rm -f ~/.maxima/mixima/*.lisp && rm -f ~/.maxima/mixima/*.mac && rmdir ~/.maxima/mixima/"
rm -f ~/.maxima/mixima/*.lisp && rm -f ~/.maxima/mixima/*.mac && rmdir ~/.maxima/mixima/
