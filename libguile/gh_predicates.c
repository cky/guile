/*      Copyright (C) 1995,1996,1997, 2000, 2001 Free Software Foundation, Inc.

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


/* type predicates and equality predicates */

#include "libguile/gh.h"

/* type predicates: tell you if an SCM object has a given type */
int 
gh_boolean_p (SCM val)
{
  return (SCM_NFALSEP (scm_boolean_p (val)));
}
int 
gh_symbol_p (SCM val)
{
  return (SCM_NFALSEP (scm_symbol_p (val)));
}
int 
gh_char_p (SCM val)
{
  return (SCM_NFALSEP (scm_char_p (val)));
}
int 
gh_vector_p (SCM val)
{
  return (SCM_NFALSEP (scm_vector_p (val)));
}
int 
gh_pair_p (SCM val)
{
  return (SCM_NFALSEP (scm_pair_p (val)));
}
int 
gh_number_p (SCM val)
{
  return (SCM_NFALSEP (scm_number_p (val)));
}
int 
gh_string_p (SCM val)
{
  return (SCM_NFALSEP (scm_string_p (val)));
}
int 
gh_procedure_p (SCM val)
{
  return (SCM_NFALSEP (scm_procedure_p (val)));
}
int 
gh_list_p (SCM val)
{
  return (SCM_NFALSEP (scm_list_p (val)));
}
int 
gh_inexact_p (SCM val)
{
  return (SCM_NFALSEP (scm_inexact_p (val)));
}
int 
gh_exact_p (SCM val)
{
  return (SCM_NFALSEP (scm_exact_p (val)));
}

/* the three types of equality */
int 
gh_eq_p (SCM x, SCM y)
{
  return (SCM_NFALSEP (scm_eq_p (x, y)));
}
int 
gh_eqv_p (SCM x, SCM y)
{
  return (SCM_NFALSEP (scm_eqv_p (x, y)));
}
int 
gh_equal_p (SCM x, SCM y)
{
  return (SCM_NFALSEP (scm_equal_p (x, y)));
}

/* equivalent to (string=? ...), but returns 0 or 1 rather than Scheme
   booleans */
int
gh_string_equal_p(SCM s1, SCM s2)
{
  return (SCM_NFALSEP (scm_string_equal_p(s1, s2)));
}

/* equivalent to (null? ...), but returns 0 or 1 rather than Scheme
   booleans */
int
gh_null_p(SCM l)
{
  return (SCM_NFALSEP(scm_null_p(l)));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
