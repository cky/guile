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
gnulib-tool --version
echo ""

######################################################################
### update infrastructure

gnulib-tool --update &&				\
autoreconf -i --force --verbose

echo "guile-readline..."
(cd guile-readline && ./autogen.sh)

# Copy versions of config.guess and config.sub from Guile CVS to
# build-aux and guile-readline.
cp -f config.guess config.sub build-aux/
cp -f config.guess config.sub guile-readline/

echo "Now run configure and make."
echo "You must pass the \`--enable-maintainer-mode' option to configure."
