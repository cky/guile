#!/bin/sh

[ -f GUILE-VERSION ] || {
  echo "autogen.sh: run this command only at the top of a Guile source tree."
  exit 1
}

aclocal -I .
autoheader
autoconf
automake

( echo "guile-readline..."; cd guile-readline; ./autogen.sh )
( echo "libltdl..."; cd libltdl; ./autogen.sh )
