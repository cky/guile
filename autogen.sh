#!/bin/sh
# Usage: sh -x ./autogen.sh [WORKBOOK]

set -e

[ -f GUILE-VERSION ] || {
  echo "autogen.sh: run this command only at the top of guile-core."
  exit 1
}

######################################################################
### Find workbook and make symlinks.

workbook=../workbook                    # assume "cvs co hack"
test x$1 = x || workbook=$1
if [ ! -d $workbook ] ; then
    echo "ERROR: could not find workbook dir"
    echo "       re-run like so: $0 WORKBOOK"
    exit 1
fi
: found workbook at $workbook
workbook=`(cd $workbook ; pwd)`

workbookdistfiles="ANON-CVS HACKING INSTALL SNAPSHOTS"
for f in $workbookdistfiles ; do
    rm -f $f
    ln -s $workbook/build/dist-files/$f $f
done
rm -f examples/example.gdbinit
ln -s $workbook/build/dist-files/.gdbinit examples/example.gdbinit

# TODO: This should be moved to dist-guile
mscripts=../guile-scripts
rm -f BUGS
$mscripts/render-bugs > BUGS

######################################################################

# Make sure this matches the ACLOCAL invokation in Makefile.am

./guile-aclocal.sh

######################################################################
### Libtool setup.

# Get a clean version.
rm -rf libltdl
libtoolize --force --copy --automake --ltdl

# Fix older versions of libtool.
# Make sure we use a ./configure.in compatible autoconf in ./libltdl/
if [ -f libltdl/configure.in ]; then
	mv libltdl/configure.in libltdl/configure.tmp
	echo 'AC_PREREQ(2.50)' > libltdl/configure.in
	cat libltdl/configure.tmp >> libltdl/configure.in
	rm libltdl/configure.tmp
fi
######################################################################

autoheader
autoconf

# Automake has a bug that will let it only add one copy of a missing
# file.  We need two mdate-sh, tho, one in doc/ref/ and one in
# doc/tutorial/.  We run automake twice as a workaround.

automake --add-missing
automake --add-missing

# Make sure that libltdl uses the same autoconf version as the rest.
#
echo "libltdl..."
(cd libltdl && aclocal)
(cd libltdl && autoconf)
(cd libltdl && automake --gnu --add-missing)

echo "guile-readline..."
(cd guile-readline && ./autogen.sh)

echo "Now run configure and make."
echo "You must pass the \`--enable-maintainer-mode' option to configure."

# autogen.sh ends here
