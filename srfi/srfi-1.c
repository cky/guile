/* srfi-1.c --- SRFI-1 procedures for Guile
 *
 * 	Copyright (C) 2002, 2003 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives
 * permission for additional uses of the text contained in its release
 * of GUILE.
 *
 * The exception is that, if you link the GUILE library with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public
 * License.  Your use of that executable is in no way restricted on
 * account of linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public
 * License.
 *
 * This exception applies only to the code released by the Free
 * Software Foundation under the name GUILE.  If you copy code from
 * other Free Software Foundation releases into a copy of GUILE, as
 * the General Public License permits, the exception does not apply to
 * the code that you add in this way.  To avoid misleading anyone as
 * to the status of such modified files, you must delete this
 * exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

#include <libguile.h>
#include <libguile/lang.h>

#include "srfi-1.h"

/* The intent of this file is to gradually replace those Scheme
 * procedures in srfi-1.scm which extends core primitive procedures,
 * so that using srfi-1 won't have performance penalties.
 *
 * Please feel free to contribute any new replacements!
 */

static long
srfi1_ilength (SCM sx)
{
  long i = 0;
  SCM tortoise = sx;
  SCM hare = sx;

  do {
    if (SCM_NULL_OR_NIL_P(hare)) return i;
    if (SCM_NCONSP(hare)) return -2;
    hare = SCM_CDR(hare);
    i++;
    if (SCM_NULL_OR_NIL_P(hare)) return i;
    if (SCM_NCONSP(hare)) return -2;
    hare = SCM_CDR(hare);
    i++;
    /* For every two steps the hare takes, the tortoise takes one.  */
    tortoise = SCM_CDR(tortoise);
  }
  while (! SCM_EQ_P (hare, tortoise));

  /* If the tortoise ever catches the hare, then the list must contain
     a cycle.  */
  return -1;
}

/* Typechecking for multi-argument MAP and FOR-EACH.

   Verify that each element of the vector ARGV, except for the first,
   is a list and return minimum length.  Attribute errors to WHO,
   and claim that the i'th element of ARGV is WHO's i+2'th argument.  */
static inline int
check_map_args (SCM argv,
		long len,
		SCM gf,
		SCM proc,
		SCM args,
		const char *who)
{
  SCM const *ve = SCM_VELTS (argv);
  long i;

  for (i = SCM_VECTOR_LENGTH (argv) - 1; i >= 1; i--)
    {
      long elt_len;

      if (!(SCM_NULLP (ve[i]) || SCM_CONSP (ve[i])))
	{
	check_map_error:
	  if (gf)
	    scm_apply_generic (gf, scm_cons (proc, args));
	  else
	    scm_wrong_type_arg (who, i + 2, ve[i]);
	}
	
      elt_len = srfi1_ilength (ve[i]);
      if (elt_len < -1)
	goto check_map_error;

      if (len < 0 || (elt_len >= 0 && elt_len < len))
	len = elt_len;
    }
  if (len < 0)
    /* i == 0 */
    goto check_map_error;
  
  scm_remember_upto_here_1 (argv);
  return len;
}


SCM_GPROC (s_srfi1_map, "map", 2, 0, 1, scm_srfi1_map, g_srfi1_map);

/* Note: Currently, scm_srfi1_map applies PROC to the argument list(s)
   sequentially, starting with the first element(s).  This is used in
   the Scheme procedure `map-in-order', which guarantees sequential
   behaviour, is implemented using scm_map.  If the behaviour changes,
   we need to update `map-in-order'.
*/

SCM 
scm_srfi1_map (SCM proc, SCM arg1, SCM args)
#define FUNC_NAME s_srfi1_map
{
  long i, len;
  SCM res = SCM_EOL;
  SCM *pres = &res;
  SCM const *ve = &args;		/* Keep args from being optimized away. */

  len = srfi1_ilength (arg1);
  SCM_GASSERTn ((SCM_NULLP (arg1) || SCM_CONSP (arg1)) && len >= -1,
		g_srfi1_map,
		scm_cons2 (proc, arg1, args), SCM_ARG2, s_srfi1_map);
  SCM_VALIDATE_REST_ARGUMENT (args);
  if (SCM_NULLP (args))
    {
      scm_t_trampoline_1 call = scm_trampoline_1 (proc);
      SCM_GASSERT2 (call, g_srfi1_map, proc, arg1, SCM_ARG1, s_srfi1_map);
      SCM_GASSERT2 (len >= 0, g_srfi1_map, proc, arg1, SCM_ARG2, s_srfi1_map);
      while (SCM_NIMP (arg1))
	{
	  *pres = scm_list_1 (call (proc, SCM_CAR (arg1)));
	  pres = SCM_CDRLOC (*pres);
	  arg1 = SCM_CDR (arg1);
	}
      return res;
    }
  if (SCM_NULLP (SCM_CDR (args)))
    {
      SCM arg2 = SCM_CAR (args);
      int len2 = srfi1_ilength (arg2);
      scm_t_trampoline_2 call = scm_trampoline_2 (proc);
      SCM_GASSERTn (call, g_srfi1_map,
		    scm_cons2 (proc, arg1, args), SCM_ARG1, s_srfi1_map);
      if (len < 0 || (len2 >= 0 && len2 < len))
	len = len2;
      SCM_GASSERTn ((SCM_NULLP (arg2) || SCM_CONSP (arg2))
		    && len >= 0 && len2 >= -1,
		    g_srfi1_map,
		    scm_cons2 (proc, arg1, args),
		    len2 >= 0 ? SCM_ARG2 : SCM_ARG3,
		    s_srfi1_map);
      while (len > 0)
	{
	  *pres = scm_list_1 (call (proc, SCM_CAR (arg1), SCM_CAR (arg2)));
	  pres = SCM_CDRLOC (*pres);
	  arg1 = SCM_CDR (arg1);
	  arg2 = SCM_CDR (arg2);
	  --len;
	}
      return res;
    }
  args = scm_vector (arg1 = scm_cons (arg1, args));
  ve = SCM_VELTS (args);
  len = check_map_args (args, len, g_srfi1_map, proc, arg1, s_srfi1_map);
  while (len > 0)
    {
      arg1 = SCM_EOL;
      for (i = SCM_VECTOR_LENGTH (args) - 1; i >= 0; i--)
	{
	  arg1 = scm_cons (SCM_CAR (ve[i]), arg1);
	  SCM_VECTOR_SET (args, i, SCM_CDR (ve[i]));
	}
      *pres = scm_list_1 (scm_apply (proc, arg1, SCM_EOL));
      pres = SCM_CDRLOC (*pres);
      --len;
    }
  return res;
}
#undef FUNC_NAME

SCM_REGISTER_PROC (s_srfi1_map_in_order, "map-in-order", 2, 0, 1, scm_srfi1_map);

SCM_GPROC (s_srfi1_for_each, "for-each", 2, 0, 1, scm_srfi1_for_each, g_srfi1_for_each);

SCM 
scm_srfi1_for_each (SCM proc, SCM arg1, SCM args)
#define FUNC_NAME s_srfi1_for_each
{
  SCM const *ve = &args;		/* Keep args from being optimized away. */
  long i, len;
  len = srfi1_ilength (arg1);
  SCM_GASSERTn ((SCM_NULLP (arg1) || SCM_CONSP (arg1)) && len >= -1,
		g_srfi1_for_each, scm_cons2 (proc, arg1, args),
		SCM_ARG2, s_srfi1_for_each);
  SCM_VALIDATE_REST_ARGUMENT (args);
  if (SCM_NULLP (args))
    {
      scm_t_trampoline_1 call = scm_trampoline_1 (proc);
      SCM_GASSERT2 (call, g_srfi1_for_each, proc, arg1,
		    SCM_ARG1, s_srfi1_for_each);
      SCM_GASSERT2 (len >= 0, g_srfi1_for_each, proc, arg1,
		    SCM_ARG2, s_srfi1_map);
      while (SCM_NIMP (arg1))
	{
	  call (proc, SCM_CAR (arg1));
	  arg1 = SCM_CDR (arg1);
	}
      return SCM_UNSPECIFIED;
    }
  if (SCM_NULLP (SCM_CDR (args)))
    {
      SCM arg2 = SCM_CAR (args);
      int len2 = srfi1_ilength (arg2);
      scm_t_trampoline_2 call = scm_trampoline_2 (proc);
      SCM_GASSERTn (call, g_srfi1_for_each,
		    scm_cons2 (proc, arg1, args), SCM_ARG1, s_srfi1_for_each);
      if (len < 0 || (len2 >= 0 && len2 < len))
	len = len2;
      SCM_GASSERTn ((SCM_NULLP (arg2) || SCM_CONSP (arg2))
		    && len >= 0 && len2 >= -1,
		    g_srfi1_for_each,
		    scm_cons2 (proc, arg1, args),
		    len2 >= 0 ? SCM_ARG2 : SCM_ARG3,
		    s_srfi1_for_each);
      while (len > 0)
	{
	  call (proc, SCM_CAR (arg1), SCM_CAR (arg2));
	  arg1 = SCM_CDR (arg1);
	  arg2 = SCM_CDR (arg2);
	  --len;
	}
      return SCM_UNSPECIFIED;
    }
  args = scm_vector (arg1 = scm_cons (arg1, args));
  ve = SCM_VELTS (args);
  len = check_map_args (args, len, g_srfi1_for_each, proc, arg1,
			s_srfi1_for_each);
  while (len > 0)
    {
      arg1 = SCM_EOL;
      for (i = SCM_VECTOR_LENGTH (args) - 1; i >= 0; i--)
	{
	  arg1 = scm_cons (SCM_CAR (ve[i]), arg1);
	  SCM_VECTOR_SET (args, i, SCM_CDR (ve[i]));
	}
      scm_apply (proc, arg1, SCM_EOL);
      --len;
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static SCM
equal_trampoline (SCM proc, SCM arg1, SCM arg2)
{
  return scm_equal_p (arg1, arg2);
}

SCM_DEFINE (scm_srfi1_member, "member", 2, 1, 0,
           (SCM x, SCM lst, SCM pred),
	    "Return the first sublist of @var{lst} whose car is\n"
	    "@var{equal?} to @var{x} where the sublists of @var{lst} are\n"
	    "the non-empty lists returned by @code{(list-tail @var{lst}\n"
	    "@var{k})} for @var{k} less than the length of @var{lst}.  If\n"
	    "@var{x} does not occur in @var{lst}, then @code{#f} (not the\n"
	    "empty list) is returned.  If optional third argument @var{equal?}\n"
	    "isn't given, @code{equal?} is used for comparison.\n"
	    "(Extended from R5RS.)\n")
#define FUNC_NAME s_scm_srfi1_member
{
  scm_t_trampoline_2 equal_p;
  SCM_VALIDATE_LIST (2, lst);
  if (SCM_UNBNDP (pred))
    equal_p = equal_trampoline;
  else
    {
      equal_p = scm_trampoline_2 (pred);
      SCM_ASSERT (equal_p, pred, 3, FUNC_NAME);
    }
  for (; !SCM_NULL_OR_NIL_P (lst); lst = SCM_CDR (lst))
    {
      if (!SCM_FALSEP (equal_p (pred, SCM_CAR (lst), x)))
	return lst;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_srfi1_assoc, "assoc", 2, 1, 0,
	    (SCM key, SCM alist, SCM pred),
	    "Behaves like @code{assq} but uses third argument @var{pred?}\n"
	    "for key comparison.  If @var{pred?} is not supplied,\n"
	    "@code{equal?} is used.  (Extended from R5RS.)\n")
#define FUNC_NAME s_scm_srfi1_assoc
{
  SCM ls = alist;
  scm_t_trampoline_2 equal_p;
  if (SCM_UNBNDP (pred))
    equal_p = equal_trampoline;
  else
    {
      equal_p = scm_trampoline_2 (pred);
      SCM_ASSERT (equal_p, pred, 3, FUNC_NAME);
    }
  for(; SCM_CONSP (ls); ls = SCM_CDR (ls)) 
    {
      SCM tmp = SCM_CAR (ls);
      SCM_ASSERT_TYPE (SCM_CONSP (tmp), alist, SCM_ARG2, FUNC_NAME,
		       "association list");
      if (SCM_NFALSEP (equal_p (pred, SCM_CAR (tmp), key)))
	return tmp;
    }
  SCM_ASSERT_TYPE (SCM_NULL_OR_NIL_P (ls), alist, SCM_ARG2, FUNC_NAME,
		   "association list");
  return SCM_BOOL_F;
}
#undef FUNC_NAME

void
scm_init_srfi_1 (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "srfi/srfi-1.x"
#endif
}

/* End of srfi-1.c.  */
