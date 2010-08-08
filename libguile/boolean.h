/* classes: h_files */

#ifndef SCM_BOOLEAN_H
#define SCM_BOOLEAN_H

/* Copyright (C) 1995,1996,2000, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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



/* Boolean Values. Obviously there are #t and #f, but there is also nil to deal
 * with. We choose to treat nil as a false boolean. All options might silently
 * break existing code, but this one seems most responsible.
 *
 */ 

/*
 * Use these macros if it's important (for correctness)
 * that #nil MUST be considered true
 */
#define scm_is_false_and_not_nil(x)     (scm_is_eq ((x), SCM_BOOL_F))
#define scm_is_true_or_nil(x)          (!scm_is_eq ((x), SCM_BOOL_F))

/*
 * Use these macros if #nil will never be tested,
 * for increased efficiency.
 */
#define scm_is_false_assume_not_nil(x)  (scm_is_eq ((x), SCM_BOOL_F))
#define scm_is_true_assume_not_nil(x)  (!scm_is_eq ((x), SCM_BOOL_F))

/*
 * See the comments preceeding the definitions of SCM_BOOL_F and
 * SCM_MATCHES_BITS_IN_COMMON in tags.h for more information on
 * how the following macro works.
 */
#define scm_is_false_or_nil(x)    \
  (SCM_MATCHES_BITS_IN_COMMON ((x), SCM_ELISP_NIL, SCM_BOOL_F))
#define scm_is_true_and_not_nil(x) (!scm_is_false_or_nil (x))

/* #nil is false. */
#define scm_is_false(x)  (scm_is_false_or_nil (x))
#define scm_is_true(x)   (!scm_is_false (x))

/*
 * Since we know SCM_BOOL_F and SCM_BOOL_T differ by exactly one bit,
 * and that SCM_BOOL_F and SCM_ELISP_NIL differ by exactly one bit,
 * and that they of course can't be the same bit (or else SCM_BOOL_T
 * and SCM_ELISP_NIL be would equal), it follows that SCM_BOOL_T and
 * SCM_ELISP_NIL differ by exactly two bits, and these are the bits
 * which will be ignored by SCM_MATCHES_BITS_IN_COMMON below.
 *
 * See the comments preceeding the definitions of SCM_BOOL_F and
 * SCM_MATCHES_BITS_IN_COMMON in tags.h for more information.
 *
 * If SCM_ENABLE_ELISP is true, then scm_is_bool_or_nil(x)
 * returns 1 if and only if x is one of the following: SCM_BOOL_F,
 * SCM_BOOL_T, SCM_ELISP_NIL, or SCM_XXX_ANOTHER_BOOLEAN_DONT_USE_0.
 * Otherwise, it returns 0.
 */
#define scm_is_bool_or_nil(x)  \
  (SCM_MATCHES_BITS_IN_COMMON ((x), SCM_BOOL_T, SCM_ELISP_NIL))
#define scm_is_bool_and_not_nil(x)  \
  (SCM_MATCHES_BITS_IN_COMMON ((x), SCM_BOOL_F, SCM_BOOL_T))

SCM_API int scm_is_bool (SCM);

#define scm_is_bool(x)   (scm_is_bool_or_nil (x))

#define scm_from_bool(x) ((x) ? SCM_BOOL_T : SCM_BOOL_F)
SCM_API int scm_to_bool (SCM x);



/* Older spellings for the above routines, kept around for
   compatibility. */
#define SCM_FALSEP(x)		(scm_is_false (x))
#define SCM_NFALSEP(x)		(scm_is_true (x))
#define SCM_BOOLP(x)            (scm_is_bool (x))
#define SCM_BOOL(x)		(scm_from_bool (x))
#define SCM_NEGATE_BOOL(f)	(scm_from_bool (!(f)))
#define SCM_BOOL_NOT(x)		(scm_not (x))



/*
 * The following macros efficiently implement boolean truth testing as
 * expected by most lisps, which treat '() aka SCM_EOL as false.
 *
 * Since we know SCM_ELISP_NIL and SCM_BOOL_F differ by exactly one
 * bit, and that SCM_ELISP_NIL and SCM_EOL differ by exactly one bit,
 * and that they of course can't be the same bit (or else SCM_BOOL_F
 * and SCM_EOL be would equal), it follows that SCM_BOOL_F and SCM_EOL
 * differ by exactly two bits, and these are the bits which will be
 * ignored by SCM_MATCHES_BITS_IN_COMMON below.
 *
 * See the comments preceeding the definitions of SCM_BOOL_F and
 * SCM_MATCHES_BITS_IN_COMMON in tags.h for more information.
 *
 * scm_is_lisp_false(x) returns 1 if and only if x is one of the
 * following: SCM_BOOL_F, SCM_ELISP_NIL, SCM_EOL or
 * SCM_XXX_ANOTHER_LISP_FALSE_DONT_USE.  Otherwise, it returns 0.
 */
#define scm_is_lisp_false(x)  \
  (SCM_MATCHES_BITS_IN_COMMON ((x), SCM_BOOL_F, SCM_EOL))



SCM_API SCM scm_not (SCM x);
SCM_API SCM scm_boolean_p (SCM obj);

SCM_INTERNAL void scm_init_boolean (void);

#endif  /* SCM_BOOLEAN_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
