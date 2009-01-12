#!/bin/sh
# Usage: sh -x ./autogen.sh

set -e

[ -f GUILE-VERSION ] || {
  echo "autogen.sh: run this command only at the top of guile-core."
  exit 1
}

######################################################################
### announce build tool versions
echo ""
autoconf --version
echo ""
automake --version
echo ""
libtool --version
echo ""
${M4:-/usr/bin/m4} --version
echo ""

######################################################################
### update infrastructure

autoreconf -i --force --verbose

echo "Now run configure and make."
