/* This file contains definitions for discouraged features.  When you
   discourage something, move it here when that is feasible.

   A discouraged feature is one that shouldn't be used in new code
   since we have a better alternative now.  However, there is nothing
   wrong with using the old feature, so it is OK to continue to use
   it.

   Eventually, discouraged features can be deprecated since removing
   them will make Guile simpler.
*/

#ifndef SCM_DISCOURAGED_H
#define SCM_DISCOURAGED_H

/* Copyright (C) 2004 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

#include "libguile/__scm.h"

#if SCM_ENABLE_DISCOURAGED == 1

/* Discouraged because they do not follow the naming convention.  That
   is, they end in "P" but return a C boolean.  Also, SCM_BOOLP
   evaluates its argument twice.
*/

#define SCM_FALSEP		scm_is_false
#define SCM_NFALSEP		scm_is_true
#define SCM_BOOLP               scm_is_bool
#define SCM_EQ_P                scm_is_eq


/* Convert from a C boolean to a SCM boolean value */
#define SCM_BOOL		scm_from_bool

/* Convert from a C boolean to a SCM boolean value and negate it */
#define SCM_NEGATE_BOOL(f)	scm_from_bool(!(f))

/* SCM_BOOL_NOT returns the other boolean.  
 * The order of ^s here is important for Borland C++ (!?!?!)
 */
#define SCM_BOOL_NOT(x)		(SCM_PACK (SCM_UNPACK (x) \
					   ^ (SCM_UNPACK (SCM_BOOL_T) \
					      ^ SCM_UNPACK (SCM_BOOL_F))))

/* scm_to_int, scm_from_int are the official functions to do the job,
   but there is nothing wrong with using scm_num2int, etc.

   These could be trivially defined via macros, but we leave them as
   functions since existing code may take their addresses.
*/

SCM_API SCM scm_short2num (short n);
SCM_API SCM scm_ushort2num (unsigned short n);
SCM_API SCM scm_int2num (int n);
SCM_API SCM scm_uint2num (unsigned int n);
SCM_API SCM scm_long2num (long n);
SCM_API SCM scm_ulong2num (unsigned long n);
SCM_API SCM scm_size2num (size_t n);
SCM_API SCM scm_ptrdiff2num (scm_t_ptrdiff n);
SCM_API short scm_num2short (SCM num, unsigned long int pos,
			     const char *s_caller);
SCM_API unsigned short scm_num2ushort (SCM num, unsigned long int pos,
				       const char *s_caller);
SCM_API int scm_num2int (SCM num, unsigned long int pos,
			 const char *s_caller);
SCM_API unsigned int scm_num2uint (SCM num, unsigned long int pos,
				   const char *s_caller);
SCM_API long scm_num2long (SCM num, unsigned long int pos,
			   const char *s_caller);
SCM_API unsigned long scm_num2ulong (SCM num, unsigned long int pos,
				     const char *s_caller);
SCM_API scm_t_ptrdiff scm_num2ptrdiff (SCM num, unsigned long int pos,
                                       const char *s_caller);
SCM_API size_t scm_num2size (SCM num, unsigned long int pos,
			     const char *s_caller);
#if SCM_SIZEOF_LONG_LONG != 0
SCM_API SCM scm_long_long2num (long long sl);
SCM_API SCM scm_ulong_long2num (unsigned long long sl);
SCM_API long long scm_num2long_long (SCM num, unsigned long int pos,
				     const char *s_caller);
SCM_API unsigned long long scm_num2ulong_long (SCM num, unsigned long int pos,
					       const char *s_caller);
#endif

SCM_API SCM scm_make_real (double x);
SCM_API double scm_num2dbl (SCM a, const char * why);
SCM_API SCM scm_float2num (float n);
SCM_API SCM scm_double2num (double n);

/* The next two are implemented in numbers.c since they use features
   only available there.
*/
SCM_API float scm_num2float (SCM num, unsigned long int pos,
			     const char *s_caller);
SCM_API double scm_num2double (SCM num, unsigned long int pos,
			       const char *s_caller);

SCM_API SCM scm_make_complex (double x, double y);

void scm_i_init_discouraged (void);

#endif /* SCM_ENABLE_DISCOURAGED == 1 */

#endif /* SCM_DISCOURAGED_H */
