#!/bin/sh

[ -f readline-activator.scm ] || {
  echo "autogen.sh: run this command only in the guile-readline directory."
  exit 1
}

(cd .. && guile-aclocal.sh)

libtoolize --copy --automake
autoconf
automake --add-missing
