#!/bin/sh

if test -f "`aclocal --print-ac-dir`/guile.m4"; then
  aclocal
else
  if test -f "`guile-config info datadir`/aclocal/guile.m4"; then
    aclocal -I "`guile-config info datadir`/aclocal"
  else
    echo "Cannot find guile.m4";
    exit;
  fi
fi

autoheader
automake -a
autoconf
