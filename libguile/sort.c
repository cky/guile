/* Copyright (C) 1999,2000,2001,2002 Free Software Foundation, Inc.
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



/* Written in December 1998 by Roland Orre <orre@nada.kth.se>
 * This implements the same sort interface as slib/sort.scm
 * for lists and vectors where slib defines:
 * sorted?, merge, merge!, sort, sort!
 * For scsh compatibility sort-list and sort-list! are also defined.
 * In cases where a stable-sort is required use stable-sort or
 * stable-sort!.  An additional feature is
 * (restricted-vector-sort! vector less? startpos endpos)
 * which allows you to sort part of a vector.
 * Thanks to Aubrey Jaffer for the slib/sort.scm library.
 * Thanks to Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
 * for the merge sort inspiration.
 * Thanks to Douglas C. Schmidt (schmidt@ics.uci.edu) for the
 * quicksort code.
 */

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/unif.h"
#include "libguile/ramap.h"
#include "libguile/feature.h"
#include "libguile/vectors.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/sort.h"


/* The routine quicksort was extracted from the GNU C Library qsort.c
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)
   and adapted to guile by adding an extra pointer less
   to quicksort by Roland Orre <orre@nada.kth.se>.

   The reason to do this instead of using the library function qsort
   was to avoid dependency of the ANSI-C extensions for local functions
   and also to avoid obscure pool based solutions.

   This sorting routine is not much more efficient than the stable
   version but doesn't consume extra memory.
 */

#define SWAP(a, b) do { const SCM _tmp = a; a = b; b = _tmp; } while (0)


/* Order size using quicksort.  This implementation incorporates
   four optimizations discussed in Sedgewick:

   1. Non-recursive, using an explicit stack of pointer that store the next
   array partition to sort.  To save time, this maximum amount of space
   required to store an array of MAX_SIZE_T is allocated on the stack.
   Assuming a bit width of 32 bits for size_t, this needs only
   32 * sizeof (stack_node) == 128 bytes.  Pretty cheap, actually.

   2. Chose the pivot element using a median-of-three decision tree.  This
   reduces the probability of selecting a bad pivot value and eliminates
   certain extraneous comparisons.

   3. Only quicksorts NR_ELEMS / MAX_THRESH partitions, leaving insertion sort
   to order the MAX_THRESH items within each partition.  This is a big win,
   since insertion sort is faster for small, mostly sorted array segments.

   4. The larger of the two sub-partitions is always pushed onto the
   stack first, with the algorithm then concentrating on the
   smaller partition.  This *guarantees* no more than log (n)
   stack size is needed (actually O(1) in this case)!  */


/* Discontinue quicksort algorithm when partition gets below this size.
 * This particular magic number was chosen to work best on a Sun 4/260. */
#define MAX_THRESH 4


/* Inline stack abstraction:  The stack size for quicksorting at most as many
 * elements as can be given by a value of type size_t is, as described above,
 * log (MAX_SIZE_T), which is the number of bits of size_t.  More accurately,
 * we would only need ceil (log (MAX_SIZE_T / MAX_THRESH)), but this is
 * ignored below. */

/* Stack node declarations used to store unfulfilled partition obligations. */
typedef struct {
    size_t lo;
    size_t hi;
} stack_node;

#define STACK_SIZE       (8 * sizeof (size_t))  /* assume 8 bit char */
#define PUSH(low, high)  ((void) ((top->lo = (low)), (top->hi = (high)), ++top))
#define	POP(low, high)	 ((void) (--top, (low = top->lo), (high = top->hi)))
#define	STACK_NOT_EMPTY	 (stack < top)


static void
quicksort (SCM *const base_ptr, size_t nr_elems, scm_t_trampoline_2 cmp, SCM less)
{
  static const char s_buggy_less[] = "buggy less predicate used when sorting";

  if (nr_elems == 0)
    /* Avoid lossage with unsigned arithmetic below.  */
    return;

  if (nr_elems > MAX_THRESH)
    {
      size_t lo = 0;
      size_t hi = nr_elems - 1;

      stack_node stack[STACK_SIZE];
      stack_node *top = stack + 1;

      while (STACK_NOT_EMPTY)
	{
	  size_t left;
	  size_t right;

	  /* Select median value from among LO, MID, and HI. Rearrange
	     LO and HI so the three values are sorted. This lowers the
	     probability of picking a pathological pivot value and
	     skips a comparison for both the left and right. */

	  size_t mid = lo + (hi - lo) / 2;

	  if (!SCM_FALSEP ((*cmp) (less, base_ptr[mid], base_ptr[lo])))
	    SWAP (base_ptr[mid], base_ptr[lo]);
	  if (!SCM_FALSEP ((*cmp) (less, base_ptr[hi], base_ptr[mid])))
	    SWAP (base_ptr[mid], base_ptr[hi]);
	  else
	    goto jump_over;
	  if (!SCM_FALSEP ((*cmp) (less, base_ptr[mid], base_ptr[lo])))
	    SWAP (base_ptr[mid], base_ptr[lo]);
	jump_over:;

	  left = lo + 1;
	  right = hi - 1;

	  /* Here's the famous ``collapse the walls'' section of quicksort.
	     Gotta like those tight inner loops!  They are the main reason
	     that this algorithm runs much faster than others. */
	  do
	    {
	      while (!SCM_FALSEP ((*cmp) (less, base_ptr[left], base_ptr[mid])))
		{
		  left++;
		  /* The comparison predicate may be buggy */
		  if (left > hi)
		    scm_misc_error (NULL, s_buggy_less, SCM_EOL);
		}

	      while (!SCM_FALSEP ((*cmp) (less, base_ptr[mid], base_ptr[right])))
		{
		  right--;
		  /* The comparison predicate may be buggy */
		  if (right < lo)
		    scm_misc_error (NULL, s_buggy_less, SCM_EOL);
		}

	      if (left < right)
		{
		  SWAP (base_ptr[left], base_ptr[right]);
		  left++;
		  right--;
		}
	      else if (left == right)
		{
		  left++;
		  right--;
		  break;
		}
	    }
	  while (left <= right);

	  /* Set up pointers for next iteration.  First determine whether
	     left and right partitions are below the threshold size.  If so,
	     ignore one or both.  Otherwise, push the larger partition's
	     bounds on the stack and continue sorting the smaller one. */

	  if ((size_t) (right - lo) <= MAX_THRESH)
	    {
	      if ((size_t) (hi - left) <= MAX_THRESH)
		/* Ignore both small partitions. */
		POP (lo, hi);
	      else
		/* Ignore small left partition. */
		lo = left;
	    }
	  else if ((size_t) (hi - left) <= MAX_THRESH)
	    /* Ignore small right partition. */
	    hi = right;
	  else if ((right - lo) > (hi - left))
	    {
	      /* Push larger left partition indices. */
	      PUSH (lo, right);
	      lo = left;
	    }
	  else
	    {
	      /* Push larger right partition indices. */
	      PUSH (left, hi);
	      hi = right;
	    }
	}
    }

  /* Once the BASE_PTR array is partially sorted by quicksort the rest is
     completely sorted using insertion sort, since this is efficient for
     partitions below MAX_THRESH size. BASE_PTR points to the beginning of the
     array to sort, and END idexes the very last element in the array (*not*
     one beyond it!). */

  {
    size_t tmp = 0;
    size_t end = nr_elems - 1;
    size_t thresh = min (end, MAX_THRESH);
    size_t run;

    /* Find smallest element in first threshold and place it at the
       array's beginning.  This is the smallest array element,
       and the operation speeds up insertion sort's inner loop. */

    for (run = tmp + 1; run <= thresh; run++)
      if (!SCM_FALSEP ((*cmp) (less, base_ptr[run], base_ptr[tmp])))
	tmp = run;

    if (tmp != 0)
      SWAP (base_ptr[tmp], base_ptr[0]);

    /* Insertion sort, running from left-hand-side up to right-hand-side.  */

    run = 1;
    while (++run <= end)
      {
	tmp = run - 1;
	while (!SCM_FALSEP ((*cmp) (less, base_ptr[run], base_ptr[tmp])))
	  {
	    /* The comparison predicate may be buggy */
	    if (tmp == 0)
	      scm_misc_error (NULL, s_buggy_less, SCM_EOL);

	    tmp--;
	  }

	tmp++;
	if (tmp != run)
	  {
            SCM to_insert = base_ptr[run];
            size_t hi, lo;

            for (hi = lo = run; --lo >= tmp; hi = lo)
              base_ptr[hi] = base_ptr[lo];
            base_ptr[hi] = to_insert;
	  }
      }
  }
}


static scm_t_trampoline_2
compare_function (SCM less, unsigned int arg_nr, const char* fname)
{
  const scm_t_trampoline_2 cmp = scm_trampoline_2 (less);
  SCM_ASSERT_TYPE (cmp != NULL, less, arg_nr, fname, "less predicate");
  return cmp;
}


/* Question: Is there any need to make this a more general array sort?
   It is probably enough to manage the vector type. */
/* endpos equal as for substring, i.e. endpos is not included. */
/* More natural with length? */

SCM_DEFINE (scm_restricted_vector_sort_x, "restricted-vector-sort!", 4, 0, 0, 
            (SCM vec, SCM less, SCM startpos, SCM endpos),
	    "Sort the vector @var{vec}, using @var{less} for comparing\n"
	    "the vector elements.  @var{startpos} and @var{endpos} delimit\n"
	    "the range of the vector which gets sorted.  The return value\n"
	    "is not specified.")
#define FUNC_NAME s_scm_restricted_vector_sort_x
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  size_t  vlen, spos, len;
  SCM *vp;

  SCM_VALIDATE_VECTOR (1, vec);
  vp = SCM_WRITABLE_VELTS (vec);		/* vector pointer */
  vlen = SCM_VECTOR_LENGTH (vec);

  SCM_VALIDATE_INUM_MIN_COPY (3, startpos, 0, spos);
  SCM_ASSERT_RANGE (3, startpos, spos <= vlen);
  SCM_VALIDATE_INUM_RANGE (4, endpos,0, vlen+1);
  len = SCM_INUM (endpos) - spos;

  quicksort (&vp[spos], len, cmp, less);
  scm_remember_upto_here_1 (vec);
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* (sorted? sequence less?)
 * is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
 * such that for all 1 <= i <= m,
 * (not (less? (list-ref list i) (list-ref list (- i 1)))). */
SCM_DEFINE (scm_sorted_p, "sorted?", 2, 0, 0,
            (SCM items, SCM less),
	    "Return @code{#t} iff @var{items} is a list or a vector such that\n"
	    "for all 1 <= i <= m, the predicate @var{less} returns true when\n"
	    "applied to all elements i - 1 and i")
#define FUNC_NAME s_scm_sorted_p
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  long len, j;			/* list/vector length, temp j */
  SCM item, rest;		/* rest of items loop variable */
  SCM const *vp;

  if (SCM_NULL_OR_NIL_P (items))
    return SCM_BOOL_T;

  if (SCM_CONSP (items))
    {
      len = scm_ilength (items); /* also checks that it's a pure list */
      SCM_ASSERT_RANGE (1, items, len >= 0);
      if (len <= 1)
	return SCM_BOOL_T;

      item = SCM_CAR (items);
      rest = SCM_CDR (items);
      j = len - 1;
      while (j > 0)
	{
	  if (!SCM_FALSEP ((*cmp) (less, SCM_CAR (rest), item)))
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
      SCM_VALIDATE_VECTOR (1, items);

      vp = SCM_VELTS (items);	/* vector pointer */
      len = SCM_VECTOR_LENGTH (items);
      j = len - 1;
      while (j > 0)
	{
	  if (!SCM_FALSEP ((*cmp) (less, vp[1], vp[0])))
	    return SCM_BOOL_F;
	  else
	    {
	      vp++;
	      j--;
	    }
	}
      return SCM_BOOL_T;
    }

  return SCM_BOOL_F;
}
#undef FUNC_NAME


/* (merge a b less?)
   takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
   and returns a new list in which the elements of a and b have been stably
   interleaved so that (sorted? (merge a b less?) less?).
   Note:  this does _not_ accept vectors. */
SCM_DEFINE (scm_merge, "merge", 3, 0, 0, 
            (SCM alist, SCM blist, SCM less),
	    "Merge two already sorted lists into one.\n"
	    "Given two lists @var{alist} and @var{blist}, such that\n"
	    "@code{(sorted? alist less?)} and @code{(sorted? blist less?)},\n"
	    "return a new list in which the elements of @var{alist} and\n"
	    "@var{blist} have been stably interleaved so that\n"
	    "@code{(sorted? (merge alist blist less?) less?)}.\n"
	    "Note:  this does _not_ accept vectors.")
#define FUNC_NAME s_scm_merge
{
  SCM build;

  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      const scm_t_trampoline_2 cmp = compare_function (less, 3, FUNC_NAME);
      long alen, blen;		/* list lengths */
      SCM last;

      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (1, alist, alen);
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (2, blist, blen);
      if (!SCM_FALSEP ((*cmp) (less, SCM_CAR (blist), SCM_CAR (alist))))
	{
	  build = scm_cons (SCM_CAR (blist), SCM_EOL);
	  blist = SCM_CDR (blist);
	  blen--;
	}
      else
	{
	  build = scm_cons (SCM_CAR (alist), SCM_EOL);
	  alist = SCM_CDR (alist);
	  alen--;
	}
      last = build;
      while ((alen > 0) && (blen > 0))
	{
	  if (!SCM_FALSEP ((*cmp) (less, SCM_CAR (blist), SCM_CAR (alist))))
	    {
	      SCM_SETCDR (last, scm_cons (SCM_CAR (blist), SCM_EOL));
	      blist = SCM_CDR (blist);
	      blen--;
	    }
	  else
	    {
	      SCM_SETCDR (last, scm_cons (SCM_CAR (alist), SCM_EOL));
	      alist = SCM_CDR (alist);
	      alen--;
	    }
	  last = SCM_CDR (last);
	}
      if ((alen > 0) && (blen == 0))
	SCM_SETCDR (last, alist);
      else if ((alen == 0) && (blen > 0))
	SCM_SETCDR (last, blist);
    }
  return build;
}
#undef FUNC_NAME


static SCM 
scm_merge_list_x (SCM alist, SCM blist,
		  long alen, long blen,
		  scm_t_trampoline_2 cmp, SCM less)
{
  SCM build, last;

  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      if (!SCM_FALSEP ((*cmp) (less, SCM_CAR (blist), SCM_CAR (alist))))
	{
	  build = blist;
	  blist = SCM_CDR (blist);
	  blen--;
	}
      else
	{
	  build = alist;
	  alist = SCM_CDR (alist);
	  alen--;
	}
      last = build;
      while ((alen > 0) && (blen > 0))
	{
	  if (!SCM_FALSEP ((*cmp) (less, SCM_CAR (blist), SCM_CAR (alist))))
	    {
	      SCM_SETCDR (last, blist);
	      blist = SCM_CDR (blist);
	      blen--;
	    }
	  else
	    {
	      SCM_SETCDR (last, alist);
	      alist = SCM_CDR (alist);
	      alen--;
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


SCM_DEFINE (scm_merge_x, "merge!", 3, 0, 0, 
            (SCM alist, SCM blist, SCM less),
	    "Takes two lists @var{alist} and @var{blist} such that\n"
	    "@code{(sorted? alist less?)} and @code{(sorted? blist less?)} and\n"
	    "returns a new list in which the elements of @var{alist} and\n"
	    "@var{blist} have been stably interleaved so that\n"
	    " @code{(sorted? (merge alist blist less?) less?)}.\n"
	    "This is the destructive variant of @code{merge}\n"
	    "Note:  this does _not_ accept vectors.")
#define FUNC_NAME s_scm_merge_x
{
  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      const scm_t_trampoline_2 cmp = compare_function (less, 3, FUNC_NAME);
      long alen, blen;		/* list lengths */
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (1, alist, alen);
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (2, blist, blen);
      return scm_merge_list_x (alist, blist, alen, blen, cmp, less);
    }
}
#undef FUNC_NAME


/* This merge sort algorithm is same as slib's by Richard A. O'Keefe.
   The algorithm is stable. We also tried to use the algorithm used by
   scsh's merge-sort but that algorithm showed to not be stable, even
   though it claimed to be.
*/
static SCM 
scm_merge_list_step (SCM * seq, scm_t_trampoline_2 cmp, SCM less, long n)
{
  SCM a, b;

  if (n > 2)
    {
      long mid = n / 2;
      a = scm_merge_list_step (seq, cmp, less, mid);
      b = scm_merge_list_step (seq, cmp, less, n - mid);
      return scm_merge_list_x (a, b, mid, n - mid, cmp, less);
    }
  else if (n == 2)
    {
      SCM p = *seq;
      SCM rest = SCM_CDR (*seq);
      SCM x = SCM_CAR (*seq);
      SCM y = SCM_CAR (SCM_CDR (*seq));
      *seq = SCM_CDR (rest);
      SCM_SETCDR (rest, SCM_EOL);
      if (!SCM_FALSEP ((*cmp) (less, y, x)))
	{
	  SCM_SETCAR (p, y);
	  SCM_SETCAR (rest, x);
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


SCM_DEFINE (scm_sort_x, "sort!", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector.  @var{less} is used for comparing the sequence\n"
	    "elements.  The sorting is destructive, that means that the\n"
	    "input sequence is modified to produce the sorted result.\n"
	    "This is not a stable sort.")
#define FUNC_NAME s_scm_sort_x
{
  long len;			/* list/vector length */
  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (SCM_CONSP (items))
    {
      const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
      SCM_VALIDATE_LIST_COPYLEN (1, items, len);
      return scm_merge_list_step (&items, cmp, less, len);
    }
  else if (SCM_VECTORP (items))
    {
      len = SCM_VECTOR_LENGTH (items);
      scm_restricted_vector_sort_x (items,
				    less,
				    SCM_MAKINUM (0L),
				    SCM_MAKINUM (len));
      return items;
    }
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort, "sort", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector.  @var{less} is used for comparing the sequence\n"
	    "elements.  This is not a stable sort.")
#define FUNC_NAME s_scm_sort
{
  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (SCM_CONSP (items))
    {
      const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
      long len;
  
      SCM_VALIDATE_LIST_COPYLEN (1, items, len);
      items = scm_list_copy (items);
      return scm_merge_list_step (&items, cmp, less, len);
    }
#if SCM_HAVE_ARRAYS
  /* support ordinary vectors even if arrays not available?  */
  else if (SCM_VECTORP (items))
    {
      long len = SCM_VECTOR_LENGTH (items);
      SCM sortvec = scm_make_uve (len, scm_array_prototype (items));

      scm_array_copy_x (items, sortvec);
      scm_restricted_vector_sort_x (sortvec,
				    less,
				    SCM_MAKINUM (0L),
				    SCM_MAKINUM (len));
      return sortvec;
    }
#endif
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


static void
scm_merge_vector_x (SCM vec,
		    SCM * temp,
		    scm_t_trampoline_2 cmp,
		    SCM less,
		    long low,
		    long mid,
		    long high)
{
  long it;	     	/* Index for temp vector */
  long i1 = low;      	/* Index for lower vector segment */
  long i2 = mid + 1;  	/* Index for upper vector segment */

  /* Copy while both segments contain more characters */
  for (it = low; (i1 <= mid) && (i2 <= high); ++it)
    {
      /*
	Every call of LESS might invoke GC.  For full correctness, we
	should reset the generation of vecbase and tempbase between
	every call of less.

       */
      register SCM *vp = SCM_WRITABLE_VELTS(vec);
      
      if (!SCM_FALSEP ((*cmp) (less, vp[i2], vp[i1])))
	temp[it] = vp[i2++];
      else
	temp[it] = vp[i1++];
    }

  {
    register SCM *vp = SCM_WRITABLE_VELTS(vec);
    
    /* Copy while first segment contains more characters */
    while (i1 <= mid)
      temp[it++] = vp[i1++];

    /* Copy while second segment contains more characters */
    while (i2 <= high)
      temp[it++] = vp[i2++];

    /* Copy back from temp to vp */
    for (it = low; it <= high; ++it)
      vp[it] = temp[it];
  }
} 	        		/* scm_merge_vector_x */


static void
scm_merge_vector_step (SCM vp,
		       SCM * temp,
		       scm_t_trampoline_2 cmp,
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


SCM_DEFINE (scm_stable_sort_x, "stable-sort!", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector. @var{less} is used for comparing the sequence elements.\n"
	    "The sorting is destructive, that means that the input sequence\n"
	    "is modified to produce the sorted result.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_stable_sort_x
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  long len;			/* list/vector length */

  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (SCM_CONSP (items))
    {
      SCM_VALIDATE_LIST_COPYLEN (1, items, len);
      return scm_merge_list_step (&items, cmp, less, len);
    }
  else if (SCM_VECTORP (items))
    {
      SCM *temp;
      len = SCM_VECTOR_LENGTH (items);

      /*
	 the following array does not contain any new references to
	 SCM objects, so we can get away with allocing it on the heap.
      */
      temp = scm_malloc (len * sizeof(SCM));

      scm_merge_vector_step (items, temp, cmp, less, 0, len - 1);
      free(temp);
      return items;
    }
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


SCM_DEFINE (scm_stable_sort, "stable-sort", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector. @var{less} is used for comparing the sequence elements.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_stable_sort
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);

  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (SCM_CONSP (items))
    {
      long len;			/* list/vector length */      
  
      SCM_VALIDATE_LIST_COPYLEN (1, items, len);
      items = scm_list_copy (items);
      return scm_merge_list_step (&items, cmp, less, len);
    }
#if SCM_HAVE_ARRAYS
  /* support ordinary vectors even if arrays not available?  */
  else if (SCM_VECTORP (items))
    {
      long len = SCM_VECTOR_LENGTH (items);
      SCM *temp = scm_malloc (len * sizeof (SCM));
      SCM retvec = scm_make_uve (len, scm_array_prototype (items));
      scm_array_copy_x (items, retvec);

      scm_merge_vector_step (retvec, temp, cmp, less, 0, len - 1);
      free (temp);
      return retvec;
    }
#endif
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort_list_x, "sort-list!", 2, 0, 0, 
            (SCM items, SCM less),
	    "Sort the list @var{items}, using @var{less} for comparing the\n"
	    "list elements. The sorting is destructive, that means that the\n"
	    "input list is modified to produce the sorted result.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_sort_list_x
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  long len;

  SCM_VALIDATE_LIST_COPYLEN (1, items, len);
  return scm_merge_list_step (&items, cmp, less, len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort_list, "sort-list", 2, 0, 0, 
	    (SCM items, SCM less),
	    "Sort the list @var{items}, using @var{less} for comparing the\n"
	    "list elements. This is a stable sort.")
#define FUNC_NAME s_scm_sort_list
{
  const scm_t_trampoline_2 cmp = compare_function (less, 2, FUNC_NAME);
  long len;

  SCM_VALIDATE_LIST_COPYLEN (1, items, len);
  items = scm_list_copy (items);
  return scm_merge_list_step (&items, cmp, less, len);
}
#undef FUNC_NAME


void
scm_init_sort ()
{
#include "libguile/sort.x"

  scm_add_feature ("sort");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
