#!/bin/sh

[ -f GUILE-VERSION ] || {
  echo "autogen.sh: run this command only at the top of a Guile source tree."
  exit 1
}

./guile-aclocal.sh

libtoolize --copy --force --automake --ltdl
autoheader
autoconf
automake --add-missing

# Make sure that libltdl uses the same autoconf version as the rest.
#
( echo "libltdl..."; cd libltdl; autoconf )

( echo "guile-readline..."; cd guile-readline; ./autogen.sh )

echo "Now run configure and make."
echo "You must pass the \`--enable-maintainer-mode' option to configure."
