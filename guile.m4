## An m4 macro to initialize a guile module.
## Enhance as required.

dnl Usage: AM_INIT_GUILE_MODULE(module-name)
dnl This macro will automatically get the guile version from the
dnl top-level srcdir, and will initialize automake.  It also
dnl defines the `module' variable.
AC_DEFUN([AM_INIT_GUILE_MODULE],[
. $srcdir/../GUILE-VERSION
AM_INIT_AUTOMAKE($PACKAGE, $VERSION, no-define)
AC_CONFIG_AUX_DIR(..)
module=[$1]
AC_SUBST(module)])
