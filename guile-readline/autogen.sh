#!/bin/sh

[ -f readline-activator.scm ] || {
  echo "autogen.sh: run this command only in the guile-readline directory."
  exit 1
}

autoreconf -i --force
