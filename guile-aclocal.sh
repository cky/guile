#!/bin/sh

temp=/tmp/guile-aclocal.$$
trap "rm -rf $temp" 0 1 2 15

mkdir $temp
[ -d $temp ] || {
  echo "guile-aclocal.sh: cannot create temp directory"
  exit 1
}

# copy all installed aclocal files into $temp
cp `aclocal --print-ac-dir`/*.m4 $temp

# remove the .m4 files installed by Guile from $temp, so they don't
# cause conflicts
rm -f ${temp}/guile.m4 ${temp}/qthreads.m4

aclocal --acdir=$temp -I .
