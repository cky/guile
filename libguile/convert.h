/* classes: h_files */

#ifndef SCM_CONVERT_H
#define SCM_CONVERT_H

/* Copyright (C) 2002 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */



#include "libguile/__scm.h"

SCM_API char *scm_c_scm2chars (SCM obj, char *dst);
SCM_API short *scm_c_scm2shorts (SCM obj, short *dst);
SCM_API int *scm_c_scm2ints (SCM obj, int *dst);
SCM_API long *scm_c_scm2longs (SCM obj, long *dst);
SCM_API float *scm_c_scm2floats (SCM obj, float *dst);
SCM_API double *scm_c_scm2doubles (SCM obj, double *dst);

SCM_API SCM scm_c_chars2scm (const char *src, long n);
SCM_API SCM scm_c_shorts2scm (const short *src, long n);
SCM_API SCM scm_c_ints2scm (const int *src, long n);
SCM_API SCM scm_c_longs2scm (const long *src, long n);
SCM_API SCM scm_c_floats2scm (const float *src, long n);
SCM_API SCM scm_c_doubles2scm (const double *src, long n);

#if SCM_HAVE_ARRAYS
SCM_API SCM scm_c_chars2byvect (const char *src, long n);
SCM_API SCM scm_c_shorts2svect (const short *src, long n);
SCM_API SCM scm_c_ints2ivect (const int *src, long n);
SCM_API SCM scm_c_uints2uvect (const unsigned int *src, long n);
SCM_API SCM scm_c_longs2ivect (const long *src, long n);
SCM_API SCM scm_c_ulongs2uvect (const unsigned long *src, long n);
SCM_API SCM scm_c_floats2fvect (const float *src, long n);
SCM_API SCM scm_c_doubles2dvect (const double *src, long n);
#endif

#endif /* SCM_CONVERT_H */
