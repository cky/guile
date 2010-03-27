/*	Copyright (C) 1995, 1996, 2000, 2001, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"

#include "libguile/validate.h"
#include "libguile/boolean.h"
#include "libguile/lang.h"
#include "libguile/tags.h"

#include "verify.h"



/*
 * These compile-time tests verify the properties needed for the
 * efficient test macros defined in boolean.h, which are defined in
 * terms of the SCM_MATCHES_BITS_IN_COMMON macro.
 *
 * See the comments preceeding the definitions of SCM_BOOL_F and
 * SCM_MATCHES_BITS_IN_COMMON in tags.h for more information.
 */
verify (SCM_VALUES_DIFFER_IN_EXACTLY_ONE_BIT_POSITION		\
		(SCM_BOOL_F, SCM_BOOL_T));
verify (SCM_VALUES_DIFFER_IN_EXACTLY_ONE_BIT_POSITION		\
		(SCM_ELISP_NIL, SCM_BOOL_F));
verify (SCM_VALUES_DIFFER_IN_EXACTLY_ONE_BIT_POSITION		\
		(SCM_ELISP_NIL, SCM_EOL));
verify (SCM_VALUES_DIFFER_IN_EXACTLY_TWO_BIT_POSITIONS		\
		(SCM_ELISP_NIL, SCM_BOOL_F, SCM_BOOL_T,		\
		 SCM_XXX_ANOTHER_BOOLEAN_DONT_USE_0));
verify (SCM_VALUES_DIFFER_IN_EXACTLY_TWO_BIT_POSITIONS		\
		(SCM_ELISP_NIL, SCM_BOOL_F, SCM_EOL,		\
		 SCM_XXX_ANOTHER_LISP_FALSE_DONT_USE));

SCM_DEFINE (scm_not, "not", 1, 0, 0, 
            (SCM x),
            "Return @code{#t} iff @var{x} is @code{#f}, else return @code{#f}.")
#define FUNC_NAME s_scm_not
{
  return scm_from_bool (scm_is_false_or_nil (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_boolean_p, "boolean?", 1, 0, 0, 
           (SCM obj),
            "Return @code{#t} iff @var{obj} is either @code{#t} or @code{#f}.")
#define FUNC_NAME s_scm_boolean_p
{
  return scm_from_bool (scm_is_bool_or_nil (obj));
}
#undef FUNC_NAME

int
scm_to_bool (SCM x)
{
  /* XXX Should this first test use scm_is_false_or_nil instead? */
  if (scm_is_eq (x, SCM_BOOL_F))
    return 0;
  else if (scm_is_eq (x, SCM_BOOL_T))
    return 1;
  else    
    scm_wrong_type_arg (NULL, 0, x);
}

/* We keep this primitive as a function in addition to the same-named macro
   because some applications (e.g., GNU LilyPond 2.13.9) expect it to be a
   function.  */
#undef scm_is_bool
int
scm_is_bool (SCM obj)
{
  /* This must match the macro definition of `scm_is_bool ()'.  */
  return scm_is_bool_and_not_nil (obj);
}


void
scm_init_boolean ()
{
#include "libguile/boolean.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
