AC_DEFUN([GUILE_COMPAT],
 [guile_compat_save_CFLAGS="$CFLAGS"
  guile_compat_save_LIBS="$LIBS"
  CFLAGS="$GUILE_CFLAGS"
  LIBS="$GUILE_LDFLAGS"
  AC_CHECK_FUNCS([scm_c_define_module scm_c_read_string scm_gc_protect_object scm_list_1 scm_c_register_extension scm_make_real scm_num2double scm_c_define_gsubr])
  AC_MSG_CHECKING(for scm_t_bits)
  AC_CACHE_VAL(ac_cv_have_scm_t_bits,
               [AC_TRY_COMPILE([#include <libguile.h>],
                               [scm_t_bits a;],
                               ac_cv_have_scm_t_bits=yes,
                               ac_cv_have_scm_t_bits=no)])
  AC_MSG_RESULT($ac_cv_have_scm_t_bits)
  if test $ac_cv_have_scm_t_bits = yes; then
    AC_DEFINE(HAVE_SCM_T_BITS)
  fi
  LIBS="$guile_compat_save_LIBS"
  CFLAGS="$guile_compat_save_CFLAGS"])
