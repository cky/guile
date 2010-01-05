/* classes: h_files */

#ifndef SCM_GSUBR_H
#define SCM_GSUBR_H

/* Copyright (C) 1995,1996,1998,2000,2001, 2006, 2008, 2009 Free Software Foundation, Inc.
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




/* Subrs 
 */

#define SCM_PRIMITIVE_P(x) (SCM_NIMP (x) && SCM_TYP7 (x) == scm_tc7_gsubr)
#define SCM_PRIMITIVE_GENERIC_P(x) (SCM_PRIMITIVE_P (x) && SCM_SUBR_GENERIC (x))

#define SCM_SUBR_META_INFO(x)  ((SCM *) SCM_CELL_WORD_3 (x))
#define SCM_SUBR_NAME(x) (SCM_SUBR_META_INFO (x) [0])
#define SCM_SUBRF(x) ((SCM (*)()) SCM_CELL_WORD_1 (x))
#define SCM_SUBR_PROPS(x) (SCM_SUBR_META_INFO (x) [1])
#define SCM_SUBR_GENERIC(x) ((SCM *) SCM_CELL_WORD_2 (x))
#define SCM_SET_SUBR_GENERIC(x, g) (*((SCM *) SCM_CELL_WORD_2 (x)) = (g))
#define SCM_SET_SUBR_GENERIC_LOC(x, g) (SCM_SET_CELL_WORD_2 (x, (scm_t_bits) g))

/* Return the most suitable subr type for a subr with REQ required arguments,
   OPT optional arguments, and REST (0 or 1) arguments.  This has to be in
   sync with `create_gsubr ()'.  */
#define SCM_SUBR_ARITY_TO_TYPE(req, opt, rest)				\
  (scm_tc7_gsubr | (SCM_GSUBR_MAKTYPE (req, opt, rest) << 8U))






/* Return an integer describing the arity of GSUBR, a subr of type
   `scm_tc7_gsubr'.  The result can be interpreted with `SCM_GSUBR_REQ ()'
   and similar.  */
#define SCM_GSUBR_TYPE(gsubr)  (SCM_CELL_TYPE (gsubr) >> 8)

#define SCM_GSUBR_MAKTYPE(req, opt, rst) ((req)|((opt)<<4)|((rst)<<8))
#define SCM_GSUBR_MAX    33
#define SCM_GSUBR_REQ(x) ((long)(x)&0xf)
#define SCM_GSUBR_OPT(x) (((long)(x)&0xf0)>>4)
#define SCM_GSUBR_REST(x) ((long)(x)>>8)

SCM_API SCM scm_c_make_gsubr (const char *name, 
			      int req, int opt, int rst, SCM (*fcn) ());
SCM_API SCM scm_c_make_gsubr_with_generic (const char *name,
					   int req, int opt, int rst,
					   SCM (*fcn) (), SCM *gf);
SCM_API SCM scm_c_define_gsubr (const char *name, 
				int req, int opt, int rst, SCM (*fcn) ());
SCM_API SCM scm_c_define_gsubr_with_generic (const char *name,
					     int req, int opt, int rst,
					     SCM (*fcn) (), SCM *gf);

SCM_INTERNAL SCM scm_i_gsubr_apply (SCM proc, SCM arg, ...);
SCM_INTERNAL SCM scm_i_gsubr_apply_list (SCM proc, SCM args);
SCM_INTERNAL SCM scm_i_gsubr_apply_array (SCM proc, SCM *args, int nargs,
                                          int headroom);
SCM_INTERNAL void scm_init_gsubr (void);

#endif  /* SCM_GSUBR_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
