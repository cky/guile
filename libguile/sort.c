/*      Copyright (C) 1999 Free Software Foundation, Inc.
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

/* Written in December 1998 by Roland Orre <orre@nada.kth.se>
 * This implements the same sort interface as slib/sort.scm
 * for lists and vectors where slib defines:
 * sorted?, merge, merge!, sort, sort!
 * For scsh compatibility are also sort-list and sort-list! defined.
 * In cases where a stable-sort is required use stable-sort or
 * stable-sort!.  As additional feature is
 * (restricted-vector-sort! vector less? startpos endpos)
 * added, where startpos and endpos allows you to sort part of a vector.
 * Thanks to Aubrey Jaffer for the slib/sort.scm library.
 * Thanks to Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
 * for the merge sort inspiration.
 * Thanks to Douglas C. Schmidt (schmidt@ics.uci.edu) for the
 * quicksort code.
 */

/* We need this to get the definitions for HAVE_ALLOCA_H, etc.  */
#include "scmconfig.h"

/* AIX requires this to be the first thing in the file.  The #pragma
   directive is indented so pre-ANSI compilers will ignore it, rather
   than choke on it.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "_scm.h"

#include "eval.h"
#include "unif.h"
#include "ramap.h"
#include "alist.h"

#include "sort.h"

/* The routine quicksort was extracted from the GNU C Library qsort.c
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)
   and adapted to guile by adding an extra pointer less
   to quicksort by Roland Orre <orre@nada.kth.se>.

   The reason to do this instead of using the library function qsort
   was to avoid dependency of the ANSI-C extensions for local functions
   and also to avoid obscure pool based solutions.
 */

/* Byte-wise swap two items of size SIZE. */
#define SWAP(a, b, size)						      \
  do									      \
    {									      \
      register size_t __size = (size);					      \
      register char *__a = (a), *__b = (b);				      \
      do								      \
	{								      \
	  char __tmp = *__a;						      \
	  *__a++ = *__b;						      \
	  *__b++ = __tmp;						      \
	} while (--__size > 0);						      \
    } while (0)

/* Discontinue quicksort algorithm when partition gets below this size.
   This particular magic number was chosen to work best on a Sun 4/260. */
#define MAX_THRESH 4

/* Stack node declarations used to store unfulfilled partition obligations. */
typedef struct
  {
    char *lo;
    char *hi;
  }
stack_node;

/* The next 4 #defines implement a very fast in-line stack abstraction. */
#define STACK_SIZE	(8 * sizeof(unsigned long int))
#define PUSH(low, high)	((void) ((top->lo = (low)), (top->hi = (high)), ++top))
#define	POP(low, high)	((void) (--top, (low = top->lo), (high = top->hi)))
#define	STACK_NOT_EMPTY	(stack < top)


/* Order size using quicksort.  This implementation incorporates
   four optimizations discussed in Sedgewick:

   1. Non-recursive, using an explicit stack of pointer that store the
   next array partition to sort.  To save time, this maximum amount
   of space required to store an array of MAX_INT is allocated on the
   stack.  Assuming a 32-bit integer, this needs only 32 *
   sizeof(stack_node) == 136 bits.  Pretty cheap, actually.

   2. Chose the pivot element using a median-of-three decision tree.
   This reduces the probability of selecting a bad pivot value and
   eliminates certain extraneous comparisons.

   3. Only quicksorts TOTAL_ELEMS / MAX_THRESH partitions, leaving
   insertion sort to order the MAX_THRESH items within each partition.
   This is a big win, since insertion sort is faster for small, mostly
   sorted array segments.

   4. The larger of the two sub-partitions is always pushed onto the
   stack first, with the algorithm then concentrating on the
   smaller partition.  This *guarantees* no more than log (n)
   stack size is needed (actually O(1) in this case)!  */

typedef int (*cmp_fun_t) (SCM less,
			  const void*,
			  const void*);

static void
quicksort (void *const pbase,
	   size_t total_elems,
	   size_t size,
	   cmp_fun_t cmp,
	   SCM less)
{
  register char *base_ptr = (char *) pbase;

  /* Allocating SIZE bytes for a pivot buffer facilitates a better
     algorithm below since we can do comparisons directly on the pivot. */
  char *pivot_buffer = (char *) alloca (size);
  const size_t max_thresh = MAX_THRESH * size;

  if (total_elems == 0)
    /* Avoid lossage with unsigned arithmetic below.  */
    return;

  if (total_elems > MAX_THRESH)
    {
      char *lo = base_ptr;
      char *hi = &lo[size * (total_elems - 1)];
      /* Largest size needed for 32-bit int!!! */
      stack_node stack[STACK_SIZE];
      stack_node *top = stack + 1;

      while (STACK_NOT_EMPTY)
	{
	  char *left_ptr;
	  char *right_ptr;

	  char *pivot = pivot_buffer;

	  /* Select median value from among LO, MID, and HI. Rearrange
	     LO and HI so the three values are sorted. This lowers the
	     probability of picking a pathological pivot value and
	     skips a comparison for both the LEFT_PTR and RIGHT_PTR. */

	  char *mid = lo + size * ((hi - lo) / size >> 1);

	  if ((*cmp) (less, (void *) mid, (void *) lo))
	    SWAP (mid, lo, size);
	  if ((*cmp) (less, (void *) hi, (void *) mid))
	    SWAP (mid, hi, size);
	  else
	    goto jump_over;
	  if ((*cmp) (less, (void *) mid, (void *) lo))
	    SWAP (mid, lo, size);
	jump_over:;
	  memcpy (pivot, mid, size);
	  pivot = pivot_buffer;

	  left_ptr = lo + size;
	  right_ptr = hi - size;

	  /* Here's the famous ``collapse the walls'' section of quicksort.
	     Gotta like those tight inner loops!  They are the main reason
	     that this algorithm runs much faster than others. */
	  do
	    {
	      while ((*cmp) (less, (void *) left_ptr, (void *) pivot))
		left_ptr += size;

	      while ((*cmp) (less, (void *) pivot, (void *) right_ptr))
		right_ptr -= size;

	      if (left_ptr < right_ptr)
		{
		  SWAP (left_ptr, right_ptr, size);
		  left_ptr += size;
		  right_ptr -= size;
		}
	      else if (left_ptr == right_ptr)
		{
		  left_ptr += size;
		  right_ptr -= size;
		  break;
		}
	    }
	  while (left_ptr <= right_ptr);

	  /* Set up pointers for next iteration.  First determine whether
	     left and right partitions are below the threshold size.  If so,
	     ignore one or both.  Otherwise, push the larger partition's
	     bounds on the stack and continue sorting the smaller one. */

	  if ((size_t) (right_ptr - lo) <= max_thresh)
	    {
	      if ((size_t) (hi - left_ptr) <= max_thresh)
		/* Ignore both small partitions. */
		POP (lo, hi);
	      else
		/* Ignore small left partition. */
		lo = left_ptr;
	    }
	  else if ((size_t) (hi - left_ptr) <= max_thresh)
	    /* Ignore small right partition. */
	    hi = right_ptr;
	  else if ((right_ptr - lo) > (hi - left_ptr))
	    {
	      /* Push larger left partition indices. */
	      PUSH (lo, right_ptr);
	      lo = left_ptr;
	    }
	  else
	    {
	      /* Push larger right partition indices. */
	      PUSH (left_ptr, hi);
	      hi = right_ptr;
	    }
	}
    }

  /* Once the BASE_PTR array is partially sorted by quicksort the rest
     is completely sorted using insertion sort, since this is efficient
     for partitions below MAX_THRESH size. BASE_PTR points to the beginning
     of the array to sort, and END_PTR points at the very last element in
     the array (*not* one beyond it!). */

  {
    char *const end_ptr = &base_ptr[size * (total_elems - 1)];
    char *tmp_ptr = base_ptr;
    char *thresh = min (end_ptr, base_ptr + max_thresh);
    register char *run_ptr;

    /* Find smallest element in first threshold and place it at the
       array's beginning.  This is the smallest array element,
       and the operation speeds up insertion sort's inner loop. */

    for (run_ptr = tmp_ptr + size; run_ptr <= thresh; run_ptr += size)
      if ((*cmp) (less, (void *) run_ptr, (void *) tmp_ptr))
	tmp_ptr = run_ptr;

    if (tmp_ptr != base_ptr)
      SWAP (tmp_ptr, base_ptr, size);

    /* Insertion sort, running from left-hand-side up to right-hand-side.  */

    run_ptr = base_ptr + size;
    while ((run_ptr += size) <= end_ptr)
      {
	tmp_ptr = run_ptr - size;
	while ((*cmp) (less, (void *) run_ptr, (void *) tmp_ptr))
	  tmp_ptr -= size;

	tmp_ptr += size;
	if (tmp_ptr != run_ptr)
	  {
	    char *trav;

	    trav = run_ptr + size;
	    while (--trav >= run_ptr)
	      {
		char c = *trav;
		char *hi, *lo;

		for (hi = lo = trav; (lo -= size) >= tmp_ptr; hi = lo)
		  *hi = *lo;
		*hi = c;
	      }
	  }
      }
  }
}				/* quicksort */


/* comparison routines */

static int 
subr2less (SCM less, const void *a, const void *b)
{
  return SCM_NFALSEP (SCM_SUBRF (less) (*(SCM *) a, *(SCM *) b));
}				/* subr2less */

static int 
subr2oless (SCM less, const void *a, const void *b)
{
  return SCM_NFALSEP (SCM_SUBRF (less) (*(SCM *) a,
					*(SCM *) b,
					SCM_UNDEFINED));
}				/* subr2oless */

static int 
lsubrless (SCM less, const void *a, const void *b)
{
  return SCM_NFALSEP (SCM_SUBRF (less)
		      (scm_cons (*(SCM *) a,
				 scm_cons (*(SCM *) b, SCM_EOL))));
}				/* lsubrless */

static int 
closureless (SCM code, const void *a, const void *b)
{
  SCM env, next;
  env  = SCM_EXTEND_ENV (SCM_CAR (SCM_CODE (code)),
			 scm_cons (*(SCM *) a,
				   scm_cons (*(SCM *) b, SCM_EOL)),
			 SCM_ENV (code));
  /* Evaluate the closure body */
  code = SCM_CDR (SCM_CODE (code));
  next = code;
  while (SCM_NNULLP (next = SCM_CDR (next)))
    {
      if (SCM_NIMP (SCM_CAR (code)))
	SCM_XEVAL (SCM_CAR (code), env);
      code = next;
    }
  return SCM_NFALSEP (SCM_XEVALCAR (code, env));
}				/* closureless */

static int 
applyless (SCM less, const void *a, const void *b)
{
  return SCM_NFALSEP (scm_apply ((SCM) less,
				 scm_cons (*(SCM *) a,
					   scm_cons (*(SCM *) b, SCM_EOL)),
				 SCM_EOL));
}				/* applyless */

static cmp_fun_t
scm_cmp_function (SCM p)
{
  switch (SCM_TYP7 (p))
    {
    case scm_tc7_subr_2:
    case scm_tc7_rpsubr:
    case scm_tc7_asubr:
      return subr2less;
    case scm_tc7_subr_2o:
      return subr2oless;
    case scm_tc7_lsubr:
      return lsubrless;
    case scm_tcs_closures:
      return closureless;
    default:
      return applyless;
    }
}				/* scm_cmp_function */

SCM_PROC (s_restricted_vector_sort_x, "restricted-vector-sort!", 4, 0, 0, scm_restricted_vector_sort_x);

/* Question: Is there any need to make this a more general array sort?
   It is probably enough to manage the vector type. */
/* endpos equal as for substring, i.e. endpos is not included. */
/* More natural wih length? */
SCM 
scm_restricted_vector_sort_x (SCM vec, SCM less, SCM startpos, SCM endpos)
{
  size_t  vlen, spos, len, size = sizeof (SCM);
  SCM *vp;

  SCM_ASSERT (SCM_NIMP (vec), vec, SCM_ARG1,  s_restricted_vector_sort_x);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2,  s_restricted_vector_sort_x);
  switch (SCM_TYP7 (vec))
    {
    case scm_tc7_vector:	/* the only type we manage is vector */
      break;
    case scm_tc7_ivect:	/* long */
    case scm_tc7_uvect:	/* unsigned */
    case scm_tc7_fvect:	/* float */
    case scm_tc7_dvect:	/* double */
    default:
      scm_wta (vec, (char *) SCM_ARG1, s_restricted_vector_sort_x);
    }
  vp = SCM_VELTS (vec);		/* vector pointer */
  vlen = SCM_LENGTH (vec);

  SCM_ASSERT (SCM_INUMP(startpos),
	      startpos, SCM_ARG3, s_restricted_vector_sort_x);
  spos = SCM_INUM (startpos);
  SCM_ASSERT ((spos >= 0) && (spos <= vlen),
	      startpos, SCM_ARG3, s_restricted_vector_sort_x);
  SCM_ASSERT ((SCM_INUMP (endpos)) && (SCM_INUM (endpos) <= vlen),
	      endpos, SCM_ARG4, s_restricted_vector_sort_x);
  len = SCM_INUM (endpos) - spos;

  quicksort (&vp[spos], len, size, scm_cmp_function (less), less);
  return SCM_UNSPECIFIED;
  /* return vec; */
}				/* scm_restricted_vector_sort_x */

/* (sorted? sequence less?)
 * is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
 * such that for all 1 <= i <= m,
 * (not (less? (list-ref list i) (list-ref list (- i 1)))). */
SCM_PROC (s_sorted_p, "sorted?", 2, 0, 0, scm_sorted_p);

SCM 
scm_sorted_p (SCM items, SCM less)
{
  long len, j;			/* list/vector length, temp j */
  SCM item, rest;		/* rest of items loop variable */
  SCM *vp;
  cmp_fun_t cmp = scm_cmp_function (less);

  if (SCM_NULLP (items))
    return SCM_BOOL_T;
  SCM_ASSERT (SCM_NIMP (items), items, SCM_ARG1, s_sorted_p);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2, s_sorted_p);

  if (SCM_CONSP (items))
    {
      len = scm_ilength (items); /* also checks that it's a pure list */
      SCM_ASSERT (len >= 0, items, SCM_ARG1, s_sorted_p);
      if (len <= 1)
	return SCM_BOOL_T;

      item = SCM_CAR (items);
      rest = SCM_CDR (items);
      j = len - 1;
      while (j > 0)
	{
	  if ((*cmp) (less, &SCM_CAR(rest), &item))
	    return SCM_BOOL_F;
	  else
	    {
	      item = SCM_CAR (rest);
	      rest = SCM_CDR (rest);
	      j--;
	    }
	}
      return SCM_BOOL_T;
    }
  else
    {
      switch (SCM_TYP7 (items))
	{
	case scm_tc7_vector:
	  {
	    vp = SCM_VELTS (items);	/* vector pointer */
	    len = SCM_LENGTH (items);
	    j = len - 1;
	    while (j > 0)
	      {
		if ((*cmp) (less, &vp[1], vp))
		  return SCM_BOOL_F;
		else
		  {
		    vp++;
		    j--;
		  }
	      }
	    return SCM_BOOL_T;
	  }
	  break;
	case scm_tc7_ivect:	/* long */
	case scm_tc7_uvect:	/* unsigned */
	case scm_tc7_fvect:	/* float */
	case scm_tc7_dvect:	/* double */
	default:
	  scm_wta (items, (char *) SCM_ARG1, s_sorted_p);
	}
    }
  return SCM_BOOL_F;
}				/* scm_sorted_p */

/* (merge a b less?)
   takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
   and returns a new list in which the elements of a and b have been stably
   interleaved so that (sorted? (merge a b less?) less?).
   Note:  this does _not_ accept vectors. */
SCM_PROC (s_merge, "merge", 3, 0, 0, scm_merge);

SCM 
scm_merge (SCM alist, SCM blist, SCM less)
{
  long alen, blen;		/* list lengths */
  SCM build, last;
  cmp_fun_t cmp = scm_cmp_function (less);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2,  s_merge);

  if (SCM_NULLP (alist))
    return blist;
  else if (SCM_NULLP (blist))
    return alist;
  else
    {
      alen = scm_ilength (alist);	/* checks that it's a pure list */
      blen = scm_ilength (blist);	/* checks that it's a pure list */
      SCM_ASSERT (alen > 0, alist, SCM_ARG1, s_merge);
      SCM_ASSERT (blen > 0, blist, SCM_ARG2, s_merge);
      if ((*cmp) (less, &SCM_CAR (alist), &SCM_CAR (blist)))
	{
	  build = scm_cons (SCM_CAR (alist), SCM_EOL);
	  alist = SCM_CDR (alist);
	  alen--;
	}
      else
	{
	  build = scm_cons (SCM_CAR (blist), SCM_EOL);
	  blist = SCM_CDR (blist);
	  blen--;
	}
      last = build;
      while ((alen > 0) && (blen > 0))
	{
	  if ((*cmp) (less, &SCM_CAR (alist), &SCM_CAR (blist)))
	    {
	      SCM_SETCDR (last, scm_cons (SCM_CAR (alist), SCM_EOL));
	      alist = SCM_CDR (alist);
	      alen--;
	    }
	  else
	    {
	      SCM_SETCDR (last, scm_cons (SCM_CAR (blist), SCM_EOL));
	      blist = SCM_CDR (blist);
	      blen--;
	    }
	  last = SCM_CDR (last);
	}
      if ((alen > 0) && (blen == 0))
	SCM_SETCDR (last, alist);
      else if ((alen == 0) && (blen > 0))
	SCM_SETCDR (last, blist);
    }
  return build;
}				/* scm_merge */

static SCM 
scm_merge_list_x (SCM alist, SCM blist,
		  long alen, long blen,
		  cmp_fun_t cmp, SCM less)
{
  SCM build, last;

  if (SCM_NULLP (alist))
    return blist;
  else if (SCM_NULLP (blist))
    return alist;
  else
    {
      if ((*cmp) (less, &SCM_CAR (alist), &SCM_CAR (blist)))
	{
	  build = alist;
	  alist = SCM_CDR (alist);
	  alen--;
	}
      else
	{
	  build = blist;
	  blist = SCM_CDR (blist);
	  blen--;
	}
      last = build;
      while ((alen > 0) && (blen > 0))
	{
	  if ((*cmp) (less, &SCM_CAR (alist), &SCM_CAR (blist)))
	    {
	      SCM_SETCDR (last, alist);
	      alist = SCM_CDR (alist);
	      alen--;
	    }
	  else
	    {
	      SCM_SETCDR (last, blist);
	      blist = SCM_CDR (blist);
	      blen--;
	    }
	  last = SCM_CDR (last);
	}
      if ((alen > 0) && (blen == 0))
	SCM_SETCDR (last, alist);
      else if ((alen == 0) && (blen > 0))
	SCM_SETCDR (last, blist);
    }
  return build;
}				/* scm_merge_list_x */

SCM_PROC (s_merge_x, "merge!", 3, 0, 0, scm_merge_x);

SCM 
scm_merge_x (SCM alist, SCM blist, SCM less)
{
  long alen, blen;		/* list lengths */

  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2,  s_merge_x);
  if (SCM_NULLP (alist))
    return blist;
  else if (SCM_NULLP (blist))
    return alist;
  else
    {
      alen = scm_ilength (alist);	/* checks that it's a pure list */
      blen = scm_ilength (blist);	/* checks that it's a pure list */
      SCM_ASSERT (alen >= 0, alist, SCM_ARG1, s_merge);
      SCM_ASSERT (blen >= 0, blist, SCM_ARG2, s_merge);
      return scm_merge_list_x (alist, blist,
			       alen, blen,
			       scm_cmp_function (less),
			       less);
    }
}				/* scm_merge_x */

/* This merge sort algorithm is same as slib's by Richard A. O'Keefe.
   The algorithm is stable. We also tried to use the algorithm used by
   scsh's merge-sort but that algorithm showed to not be stable, even
   though it claimed to be.
*/
static SCM 
scm_merge_list_step (SCM * seq,
		     cmp_fun_t cmp,
		     SCM less,
		     int n)
{
  if (n > 2)
    {
      long mid = n / 2;
      return scm_merge_list_x (scm_merge_list_step (seq, cmp, less, mid),
			       scm_merge_list_step (seq, cmp, less, n - mid),
			       mid, n - mid, cmp, less);
    }
  else if (n == 2)
    {
      SCM p = *seq;
      SCM rest = SCM_CDR (*seq);
      SCM x = SCM_CAR (*seq);
      SCM y = SCM_CAR (SCM_CDR (*seq));
      *seq = SCM_CDR (rest);
      SCM_SETCDR (rest, SCM_EOL);
      if ((*cmp) (less, &y, &x))
	{
	  SCM_CAR (p) = y;
	  SCM_CAR (rest) = x;
	}
      return p;
    }
  else if (n == 1)
    {
      SCM p = *seq;
      *seq = SCM_CDR (p);
      SCM_SETCDR (p, SCM_EOL);
      return p;
    }
  else
    return SCM_EOL;
}				/* scm_merge_list_step */


SCM_PROC (s_sort_x, "sort!", 2, 0, 0, scm_sort_x);

/* scm_sort_x manages lists and vectors, not stable sort */
SCM 
scm_sort_x (SCM items, SCM less)
{
  long len;			/* list/vector length */
  if (SCM_NULLP(items))
    return SCM_EOL;
  SCM_ASSERT (SCM_NIMP (items), items, SCM_ARG1, s_sort_x);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2,  s_sort_x);

  if (SCM_CONSP (items))
    {
      len = scm_ilength (items);
      SCM_ASSERT (len >= 0, items, SCM_ARG1, s_sort_x);
      return scm_merge_list_step (&items, scm_cmp_function (less), less, len);
    }
  else if (SCM_VECTORP (items))
    {
      len = SCM_LENGTH (items);
      scm_restricted_vector_sort_x (items,
				    less,
				    SCM_MAKINUM (0L),
				    SCM_MAKINUM (len));
      return items;
    }
  else
    return scm_wta (items, (char *) SCM_ARG1, s_sort_x);
}				/* scm_sort_x */

SCM_PROC (s_sort, "sort", 2, 0, 0, scm_sort);

/* scm_sort manages lists and vectors, not stable sort */
SCM 
scm_sort (SCM items, SCM less)
{
  SCM sortvec;			/* the vector we actually sort */
  long len;			/* list/vector length */
  if (SCM_NULLP(items))
    return SCM_EOL;
  SCM_ASSERT (SCM_NIMP (items), items, SCM_ARG1, s_sort);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2,  s_sort);
  if (SCM_CONSP (items))
    {
      len = scm_ilength (items);
      SCM_ASSERT (len >= 0, items, SCM_ARG1, s_sort);
      items = scm_list_copy (items);
      return scm_merge_list_step (&items, scm_cmp_function (less), less, len);
    }
  else if (SCM_VECTORP (items))
    {
      len = SCM_LENGTH (items);
      sortvec = scm_make_uve (len, scm_array_prototype (items));
      scm_array_copy_x (items, sortvec);
      scm_restricted_vector_sort_x (sortvec,
				    less,
				    SCM_MAKINUM (0L),
				    SCM_MAKINUM (len));
      return sortvec;
    }
  else
    return scm_wta (items, (char *) SCM_ARG1, s_sort_x);
}				/* scm_sort */

static void
scm_merge_vector_x (void *const vecbase,
		    void *const tempbase,
		    cmp_fun_t cmp,
		    SCM less,
		    long low,
		    long mid,
		    long high)
{
  register SCM *vp = (SCM *) vecbase;
  register SCM *temp = (SCM *) tempbase;
  long it;	     	/* Index for temp vector */
  long i1 = low;      	/* Index for lower vector segment */
  long i2 = mid + 1;  	/* Index for upper vector segment */

  /* Copy while both segments contain more characters */
  for (it = low; (i1 <= mid) && (i2 <= high); ++it)
    if ((*cmp) (less, &vp[i2], &vp[i1]))
      temp[it] = vp[i2++];
    else
      temp[it] = vp[i1++];

  /* Copy while first segment contains more characters */
  while (i1 <= mid)
    temp[it++] = vp[i1++];

  /* Copy while second segment contains more characters */
  while (i2 <= high)
    temp[it++] = vp[i2++];

  /* Copy back from temp to vp */
  for (it = low; it <= high; ++it)
    vp[it] = temp[it];
}	        		/* scm_merge_vector_x */

static void
scm_merge_vector_step (void *const vp,
		       void *const temp,
		       cmp_fun_t cmp,
		       SCM less,
		       long low,
		       long high)
{
  if (high > low)
    {
      long mid = (low + high) / 2;
      scm_merge_vector_step (vp, temp, cmp, less, low, mid);
      scm_merge_vector_step (vp, temp, cmp, less, mid+1, high);
      scm_merge_vector_x (vp, temp, cmp, less, low, mid, high);
    }
}				/* scm_merge_vector_step */


SCM_PROC (s_stable_sort_x, "stable-sort!", 2, 0, 0, scm_stable_sort_x);
/* stable-sort! manages lists and vectors */

SCM 
scm_stable_sort_x (SCM items, SCM less)
{
  long len;			/* list/vector length */

  if (SCM_NULLP (items))
    return SCM_EOL;
  SCM_ASSERT (SCM_NIMP (items), items, SCM_ARG1, s_stable_sort_x);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2, s_stable_sort_x);
  if (SCM_CONSP (items))
    {
      len = scm_ilength (items);
      SCM_ASSERT (len >= 0, items, SCM_ARG1, s_sort_x);
      return scm_merge_list_step (&items, scm_cmp_function (less), less, len);
    }
  else if (SCM_VECTORP (items))
    {
      SCM *temp, *vp;
      len = SCM_LENGTH (items);
      temp = malloc (len * sizeof(SCM));
      vp = SCM_VELTS (items);
      scm_merge_vector_step (vp,
			     temp,
			     scm_cmp_function (less),
			     less,
			     0,
			     len - 1);
      free(temp);
      return items;
    }
  else
    return scm_wta (items, (char *) SCM_ARG1, s_stable_sort_x);
}				/* scm_stable_sort_x */

SCM_PROC (s_stable_sort, "stable-sort", 2, 0, 0, scm_stable_sort);

/* stable_sort manages lists and vectors */
SCM
scm_stable_sort (SCM items, SCM less)
{
  long len;			/* list/vector length */
  if (SCM_NULLP (items))
    return SCM_EOL;
  SCM_ASSERT (SCM_NIMP (items), items, SCM_ARG1, s_stable_sort);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2, s_stable_sort);
  if (SCM_CONSP (items))
    {
      len = scm_ilength (items);
      SCM_ASSERT (len >= 0, items, SCM_ARG1, s_sort);
      items = scm_list_copy (items);
      return scm_merge_list_step (&items, scm_cmp_function (less), less, len);
    }
  else if (SCM_VECTORP (items))
    {
      SCM retvec;
      SCM *temp, *vp;
      len = SCM_LENGTH (items);
      retvec = scm_make_uve (len, scm_array_prototype (items));
      scm_array_copy_x (items, retvec);
      temp = malloc (len * sizeof (SCM));
      vp = SCM_VELTS (retvec);
      scm_merge_vector_step (vp,
			     temp,
			     scm_cmp_function (less),
			     less,
			     0,
			     len - 1);
      free (temp);
      return retvec;
    }
  else
    return scm_wta (items, (char *) SCM_ARG1, s_stable_sort);
}				/* scm_stable_sort */

SCM_PROC (s_sort_list_x, "sort-list!", 2, 0, 0, scm_sort_list_x);

SCM	/* stable */
scm_sort_list_x (SCM items, SCM less)	
{
  long len = scm_ilength (items);
  SCM_ASSERT (len >= 0, items, SCM_ARG1, s_sort_list_x);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2,  s_sort_list_x);
  return scm_merge_list_step (&items, scm_cmp_function (less), less, len);
}				/* scm_sort_list_x */

SCM_PROC (s_sort_list, "sort-list", 2, 0, 0, scm_sort_list);

SCM 	/* stable */
scm_sort_list (SCM items, SCM less)
{
  long len = scm_ilength (items);
  SCM_ASSERT (len >= 0, items, SCM_ARG1, s_sort_list);
  SCM_ASSERT (SCM_NIMP (less), less, SCM_ARG2, s_sort_list);
  items = scm_list_copy (items);
  return scm_merge_list_step (&items, scm_cmp_function (less), less, len);
}				/* scm_sort_list_x */

void
scm_init_sort ()
{
#include "sort.x"

  scm_add_feature ("sort");
}
