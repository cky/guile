#!/bin/sh

set -e

[ -f GUILE-VERSION ] || {
  echo "autogen.sh: run this command only at the top of a Guile source tree."
  exit 1
}

./guile-aclocal.sh

######################################################################
### Libtool setup.

# Get a clean version.
rm -rf libltdl
libtoolize --force --copy --automake --ltdl

# Make sure we use a ./configure.in compatible autoconf in ./libltdl/
mv libltdl/configure.in libltdl/configure.tmp
echo 'AC_PREREQ(2.50)' > libltdl/configure.in
cat libltdl/configure.tmp >> libltdl/configure.in
rm libltdl/configure.tmp
######################################################################

autoheader
autoconf
automake --add-missing

# Make sure that libltdl uses the same autoconf version as the rest.
#
echo "libltdl..."
(cd libltdl && autoconf)
(cd libltdl && automake --gnu --add-missing)

echo "guile-readline..."
(cd guile-readline && ./autogen.sh)

echo "Now run configure and make."
echo "You must pass the \`--enable-maintainer-mode' option to configure."
