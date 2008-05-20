#!/bin/sh

export ACLOCAL_AMFLAGS=
if test ! -f "`aclocal --print-ac-dir`/guile.m4"; then
  if test -f "`guile-config info datadir`/aclocal/guile.m4"; then
    ACLOCAL_AMFLAGS="-I `guile-config info datadir`/aclocal"
  else
    echo "warning: cannot find guile.m4";
  fi
fi

autoreconf -vif
