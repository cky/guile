/* srfi-1.c --- SRFI-1 procedures for Guile
 *
 * 	Copyright (C) 1995, 1996, 1997, 2000, 2001, 2002, 2003 Free Software
 * 	Foundation, Inc.
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
    if (!SCM_CONSP (hare)) return -2;
    hare = SCM_CDR(hare);
    i++;
    if (SCM_NULL_OR_NIL_P(hare)) return i;
    if (!SCM_CONSP (hare)) return -2;
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

static SCM
equal_trampoline (SCM proc, SCM arg1, SCM arg2)
{
  return scm_equal_p (arg1, arg2);
}


/* scm_append and scm_append_x don't modify their list argument (only the
   lists within that list in the case of scm_append_x), hence making them
   suitable for direct use for concatentate.  */

SCM_REGISTER_PROC (s_srfi1_concatenate,   "concatenate",  1, 0, 0, scm_append);
SCM_REGISTER_PROC (s_srfi1_concatenate_x, "concatenate!", 1, 0, 0, scm_append_x);


SCM_DEFINE (scm_srfi1_delete, "delete", 2, 1, 0,
            (SCM x, SCM lst, SCM pred),
	    "Return a list containing the elements of @var{lst} but with\n"
	    "those equal to @var{x} deleted.  The returned elements will be\n"
	    "in the same order as they were in @var{lst}.\n"
	    "\n"
	    "Equality is determined by @var{pred}, or @code{equal?} if not\n"
	    "given.  An equality call is made just once for each element,\n"
	    "but the order in which the calls are made on the elements is\n"
	    "unspecified.\n"
	    "\n"
	    "The equality calls are always @code{(pred x elem)}, ie.@: the\n"
	    "given @var{x} is first.  This means for instance elements\n"
	    "greater than 5 can be deleted with @code{(delete 5 lst <)}.\n"
	    "\n"
	    "@var{lst} is not modified, but the returned list might share a\n"
	    "common tail with @var{lst}.")
#define FUNC_NAME s_scm_srfi1_delete
{
  scm_t_trampoline_2 equal_p;
  SCM  ret, *p, keeplst;

  if (SCM_UNBNDP (pred))
    return scm_delete (x, lst);

  equal_p = scm_trampoline_2 (pred);
  SCM_ASSERT (equal_p, pred, SCM_ARG3, FUNC_NAME);

  /* ret is the return list being constructed.  p is where to append to it,
     initially &ret then SCM_CDRLOC of the last pair.  lst progresses as
     elements are considered.

     Elements to be retained are not immediately copied, instead keeplst is
     the last pair in lst which is to be retained but not yet copied.  When
     there's no more deletions, *p can be set to keeplst to share the
     remainder of the original lst.  (The entire original lst if there's no
     deletions at all.)  */

  keeplst = lst;
  ret = SCM_EOL;
  p = &ret;

  for ( ; SCM_CONSP (lst); lst = SCM_CDR (lst))
    {
      if (! SCM_FALSEP (equal_p (pred, x, SCM_CAR (lst))))
        {
          /* delete this element, so copy from keeplst (inclusive) to lst
             (exclusive) onto ret */
          while (! SCM_EQ_P (keeplst, lst))
            {
              SCM c = scm_cons (SCM_CAR (keeplst), SCM_EOL);
              *p = c;
              p = SCM_CDRLOC (c);
              keeplst = SCM_CDR (keeplst);
            }

          keeplst = SCM_CDR (lst);
        }
    }

  /* final retained elements */
  *p = keeplst;

  /* demand that lst was a proper list */
  SCM_ASSERT_TYPE (SCM_NULL_OR_NIL_P (lst), lst, SCM_ARG2, FUNC_NAME, "list");

  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi1_delete_x, "delete!", 2, 1, 0,
            (SCM x, SCM lst, SCM pred),
	    "Return a list containing the elements of @var{lst} but with\n"
	    "those equal to @var{x} deleted.  The returned elements will be\n"
	    "in the same order as they were in @var{lst}.\n"
	    "\n"
	    "Equality is determined by @var{pred}, or @code{equal?} if not\n"
	    "given.  An equality call is made just once for each element,\n"
	    "but the order in which the calls are made on the elements is\n"
	    "unspecified.\n"
	    "\n"
	    "The equality calls are always @code{(pred x elem)}, ie.@: the\n"
	    "given @var{x} is first.  This means for instance elements\n"
	    "greater than 5 can be deleted with @code{(delete 5 lst <)}.\n"
	    "\n"
	    "@var{lst} may be modified to construct the returned list.")
#define FUNC_NAME s_scm_srfi1_delete_x
{
  scm_t_trampoline_2 equal_p;
  SCM walk;
  SCM *prev;

  if (SCM_UNBNDP (pred))
    return scm_delete_x (x, lst);

  equal_p = scm_trampoline_2 (pred);
  SCM_ASSERT (equal_p, pred, SCM_ARG3, FUNC_NAME);

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (! SCM_FALSEP (equal_p (pred, x, SCM_CAR (walk))))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }

  /* demand the input was a proper list */
  SCM_ASSERT_TYPE (SCM_NULL_OR_NIL_P (walk), walk, SCM_ARG2, FUNC_NAME,"list");
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi1_delete_duplicates, "delete-duplicates", 1, 1, 0,
	    (SCM lst, SCM pred),
	    "Return a list containing the elements of @var{lst} but without\n"
	    "duplicates.\n"
	    "\n"
	    "When elements are equal, only the first in @var{lst} is\n"
	    "retained.  Equal elements can be anywhere in @var{lst}, they\n"
	    "don't have to be adjacent.  The returned list will have the\n"
	    "retained elements in the same order as they were in @var{lst}.\n"
	    "\n"
	    "Equality is determined by @var{pred}, or @code{equal?} if not\n"
	    "given.  Calls @code{(pred x y)} are made with element @var{x}\n"
	    "being before @var{y} in @var{lst}.  A call is made at most once\n"
	    "for each combination, but the sequence of the calls across the\n"
	    "elements is unspecified.\n"
	    "\n"
	    "@var{lst} is not modified, but the return might share a common\n"
	    "tail with @var{lst}.\n"
	    "\n"
	    "In the worst case, this is an @math{O(N^2)} algorithm because\n"
	    "it must check each element against all those preceding it.  For\n"
	    "long lists it is more efficient to sort and then compare only\n"
	    "adjacent elements.")
#define FUNC_NAME s_scm_srfi1_delete_duplicates
{
  scm_t_trampoline_2 equal_p;
  SCM  ret, *p, keeplst, item, l;

  /* ret is the new list constructed.  p is where to append, initially &ret
     then SCM_CDRLOC of the last pair.  lst is advanced as each element is
     considered.

     Elements retained are not immediately appended to ret, instead keeplst
     is the last pair in lst which is to be kept but is not yet copied.
     Initially this is the first pair of lst, since the first element is
     always retained.

     *p is kept set to keeplst, so ret (inclusive) to lst (exclusive) is all
     the elements retained, making the equality search loop easy.

     If an item must be deleted, elements from keeplst (inclusive) to lst
     (exclusive) must be copied and appended to ret.  When there's no more
     deletions, *p is left set to keeplst, so ret shares structure with the
     original lst.  (ret will be the entire original lst if there are no
     deletions.)  */

  /* skip to end if an empty list (or something invalid) */
  ret = lst;
  if (SCM_CONSP (lst))
    {
      if (SCM_UNBNDP (pred))
        equal_p = equal_trampoline;
      else
        {
          equal_p = scm_trampoline_2 (pred);
          SCM_ASSERT (equal_p, pred, SCM_ARG2, FUNC_NAME);
        }

      keeplst = lst;
      p = &ret;

      /* loop over lst elements starting from second */
      for (;;)
        {
          lst = SCM_CDR (lst);
          if (! SCM_CONSP (lst))
            break;
          item = SCM_CAR (lst);

          /* loop searching ret upto lst */
          for (l = ret; ! SCM_EQ_P (l, lst); l = SCM_CDR (l))
            {
              if (! SCM_FALSEP (equal_p (pred, SCM_CAR (l), item)))
                {
                  /* duplicate, don't want this element, so copy keeplst
                     (inclusive) to lst (exclusive) onto ret */
                  while (! SCM_EQ_P (keeplst, lst))
                    {
                      SCM c = scm_cons (SCM_CAR (keeplst), SCM_EOL);
                      *p = c;
                      p = SCM_CDRLOC (c);
                      keeplst = SCM_CDR (keeplst);
                    }

                  keeplst = SCM_CDR (lst);  /* elem after the one deleted */
                  *p = keeplst;
                  break;
                }
            }
        }
    }

  /* demand that lst was a proper list */
  SCM_ASSERT_TYPE (SCM_NULL_OR_NIL_P (lst), lst, SCM_ARG1, FUNC_NAME, "list");

  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi1_delete_duplicates_x, "delete-duplicates!", 1, 1, 0,
	    (SCM lst, SCM pred),
	    "Return a list containing the elements of @var{lst} but without\n"
	    "duplicates.\n"
	    "\n"
	    "When elements are equal, only the first in @var{lst} is\n"
	    "retained.  Equal elements can be anywhere in @var{lst}, they\n"
	    "don't have to be adjacent.  The returned list will have the\n"
	    "retained elements in the same order as they were in @var{lst}.\n"
	    "\n"
	    "Equality is determined by @var{pred}, or @code{equal?} if not\n"
	    "given.  Calls @code{(pred x y)} are made with element @var{x}\n"
	    "being before @var{y} in @var{lst}.  A call is made at most once\n"
	    "for each combination, but the sequence of the calls across the\n"
	    "elements is unspecified.\n"
	    "\n"
	    "@var{lst} may be modified to construct the returned list.\n"
	    "\n"
	    "In the worst case, this is an @math{O(N^2)} algorithm because\n"
	    "it must check each element against all those preceding it.  For\n"
	    "long lists it is more efficient to sort and then compare only\n"
	    "adjacent elements.")
#define FUNC_NAME s_scm_srfi1_delete_duplicates_x
{
  scm_t_trampoline_2 equal_p;
  SCM  ret, endret, item, l;

  /* ret is the return list, constructed from the pairs in lst.  endret is
     the last pair of ret, initially the first pair.  lst is advanced as
     elements are considered.  */

  /* skip to end if an empty list (or something invalid) */
  ret = lst;
  if (SCM_CONSP (lst))
    {
      if (SCM_UNBNDP (pred))
        equal_p = equal_trampoline;
      else
        {
          equal_p = scm_trampoline_2 (pred);
          SCM_ASSERT (equal_p, pred, SCM_ARG2, FUNC_NAME);
        }

      endret = ret;

      /* loop over lst elements starting from second */
      for (;;)
        {
          lst = SCM_CDR (lst);
          if (! SCM_CONSP (lst))
            break;
          item = SCM_CAR (lst);

          /* is item equal to any element from ret to endret (inclusive)? */
          l = ret;
          for (;;)
            {
              if (! SCM_FALSEP (equal_p (pred, SCM_CAR (l), item)))
                break;  /* equal, forget this element */

              if (SCM_EQ_P (l, endret))
                {
                  /* not equal to any, so append this pair */
                  SCM_SETCDR (endret, lst);
                  endret = lst;
                  break;
                }
              l = SCM_CDR (l);
            }
        }

      /* terminate, in case last element was deleted */
      SCM_SETCDR (endret, SCM_EOL);
    }

  /* demand that lst was a proper list */
  SCM_ASSERT_TYPE (SCM_NULL_OR_NIL_P (lst), lst, SCM_ARG1, FUNC_NAME, "list");

  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi1_length_plus, "length+", 1, 0, 0,
            (SCM lst),
	    "Return the length of @var{lst}, or @code{#f} if @var{lst} is\n"
	    "circular.")
#define FUNC_NAME s_scm_srfi1_length_plus
{
  long len = scm_ilength (lst);
  return (len >= 0 ? SCM_MAKINUM (len) : SCM_BOOL_F);
}
#undef FUNC_NAME


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

SCM_DEFINE (scm_srfi1_partition, "partition", 2, 0, 0,
	    (SCM pred, SCM list),
	    "Partition the elements of @var{list} with predicate @var{pred}.\n"
	    "Return two values: the list of elements satifying @var{pred} and\n"
	    "the list of elements @emph{not} satisfying @var{pred}.  The order\n"
	    "of the output lists follows the order of @var{list}.  @var{list}\n"
	    "is not mutated.  One of the output lists may share memory with @var{list}.\n")
#define FUNC_NAME s_scm_srfi1_partition
{
  /* In this implementation, the output lists don't share memory with
     list, because it's probably not worth the effort. */
  scm_t_trampoline_1 call = scm_trampoline_1(pred);
  SCM kept = scm_cons(SCM_EOL, SCM_EOL);
  SCM kept_tail = kept;
  SCM dropped = scm_cons(SCM_EOL, SCM_EOL);
  SCM dropped_tail = dropped;
  
  SCM_ASSERT(call, pred, 2, FUNC_NAME);
  
  for (; !SCM_NULL_OR_NIL_P (list); list = SCM_CDR(list)) {
    SCM elt = SCM_CAR(list);
    SCM new_tail = scm_cons(SCM_CAR(list), SCM_EOL);
    if (SCM_NFALSEP(call(pred, elt))) {
      SCM_SETCDR(kept_tail, new_tail);
      kept_tail = new_tail;
    }
    else {
      SCM_SETCDR(dropped_tail, new_tail);
      dropped_tail = new_tail;
    }
  }
  /* re-use the initial conses for the values list */
  SCM_SETCAR(kept, SCM_CDR(kept));
  SCM_SETCDR(kept, dropped);
  SCM_SETCAR(dropped, SCM_CDR(dropped));
  SCM_SETCDR(dropped, SCM_EOL);
  return scm_values(kept);
}
#undef FUNC_NAME

void
scm_init_srfi_1 (void)
{
  SCM the_root_module = scm_lookup_closure_module (SCM_BOOL_F);
#ifndef SCM_MAGIC_SNARFER
#include "srfi/srfi-1.x"
#endif
  scm_c_extend_primitive_generic
    (SCM_VARIABLE_REF (scm_c_module_lookup (the_root_module, "map")),
     SCM_VARIABLE_REF (scm_c_lookup ("map")));
  scm_c_extend_primitive_generic
    (SCM_VARIABLE_REF (scm_c_module_lookup (the_root_module, "for-each")),
     SCM_VARIABLE_REF (scm_c_lookup ("for-each")));
}

/* End of srfi-1.c.  */
