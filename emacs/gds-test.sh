#!/bin/sh
GUILE_LOAD_PATH=$(pwd) emacs --batch --no-site-file -q -l gds-test.el < gds-test.stdin
