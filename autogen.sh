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

# Typical MacOS X installations rename 'libtoolize' to 'glibtoolize', so
# adjust to that.
if test "`uname -s`" = "Darwin"; then
  glibtoolize --version
else
  libtoolize --version
fi

echo ""
${M4:-m4} --version
echo ""
flex --version
echo ""

######################################################################
### update infrastructure

autoreconf -i --force --verbose

echo "Now run configure and make."
