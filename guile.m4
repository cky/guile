## An m4 macro to initialize a guile module.
## Enhance as required.

dnl Usage: AM_INIT_GUILE_MODULE(module-name)
dnl This macro will automatically get the guile version from the
dnl top-level srcdir, and will initialize automake.  It also
dnl defines the `module' variable.
AC_DEFUN([AM_INIT_GUILE_MODULE],[
. $srcdir/../GUILE-VERSION
## Beginning of AM_INIT_AUTOMAKE macro
AC_REQUIRE([AM_PROG_INSTALL])
## PACKAGE=[$1]
PACKAGE=$PACKAGE
AC_SUBST(PACKAGE)
## AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE")
## VERSION=[$2]
VERSION=$VERSION
AC_SUBST(VERSION)
AC_DEFINE_UNQUOTED(VERSION, "$VERSION")
AM_SANITY_CHECK
AC_ARG_PROGRAM
AC_PROG_MAKE_SET
## End of AM_INIT_AUTOMAKE macro
AC_CONFIG_AUX_DIR(..)
module=[$1]
AC_SUBST(module)])
