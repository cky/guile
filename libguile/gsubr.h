/* classes: h_files */

#ifndef SCM_GSUBR_H
#define SCM_GSUBR_H

/* Copyright (C) 1995, 1996, 1998, 2000, 2001, 2006, 2008, 2009,
 *   2010, 2011 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#include "libguile/__scm.h"





SCM_API SCM scm_subr_objcode_trampoline (unsigned int nreq,
                                         unsigned int nopt,
                                         unsigned int rest);


/* Subrs 
 */

/* Max number of args to the C procedure backing a gsubr */
#define SCM_GSUBR_MAX 10

#define SCM_PRIMITIVE_P(x) (SCM_PROGRAM_P (x) && SCM_PROGRAM_IS_PRIMITIVE (x))

#define SCM_PRIMITIVE_GENERIC_P(x) (SCM_PROGRAM_P (x) && SCM_PROGRAM_IS_PRIMITIVE_GENERIC (x))

#define SCM_SUBRF(x)							\
  ((SCM (*) (void))							\
   SCM_POINTER_VALUE (SCM_SIMPLE_VECTOR_REF (SCM_PROGRAM_OBJTABLE (x), 0)))

#define SCM_SUBR_NAME(x) (SCM_SIMPLE_VECTOR_REF (SCM_PROGRAM_OBJTABLE (x), 1))

#define SCM_SUBR_GENERIC(x)						\
  ((SCM *)								\
   SCM_POINTER_VALUE (SCM_SIMPLE_VECTOR_REF (SCM_PROGRAM_OBJTABLE (x), 2)))

#define SCM_SET_SUBR_GENERIC(x, g) \
  (*SCM_SUBR_GENERIC (x) = (g))



SCM_API SCM scm_c_make_gsubr (const char *name,
			      int req, int opt, int rst, scm_t_subr fcn);
SCM_API SCM scm_c_make_gsubr_with_generic (const char *name,
					   int req, int opt, int rst,
					   scm_t_subr fcn, SCM *gf);
SCM_API SCM scm_c_define_gsubr (const char *name,
				int req, int opt, int rst, scm_t_subr fcn);
SCM_API SCM scm_c_define_gsubr_with_generic (const char *name,
					     int req, int opt, int rst,
					     scm_t_subr fcn, SCM *gf);

SCM_INTERNAL void scm_init_gsubr (void);

#endif  /* SCM_GSUBR_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
