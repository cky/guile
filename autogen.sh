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

workbookdistfiles="ANON-CVS HACKING SNAPSHOTS"
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
### update infrastructure

autoreconf -i --force

echo "guile-readline..."
(cd guile-readline && ./autogen.sh)

echo "Now run configure and make."
echo "You must pass the \`--enable-maintainer-mode' option to configure."
