/*	Copyright (C) 1995, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
 * 
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/eq.h"
#include "libguile/root.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/vectors.h"
#include "libguile/unif.h"


#if (SCM_DEBUG_DEPRECATED == 0)

/* The function scm_vector_set_length_x will disappear in the next release of
 * guile.
 */

/* 
 * This complicates things too much if allowed on any array.
 * C code can safely call it on arrays known to be used in a single
 * threaded manner.
 *
 * SCM_REGISTER_PROC(s_vector_set_length_x, "vector-set-length!", 2, 0, 0, scm_vector_set_length_x); 
 */
static char s_vector_set_length_x[] = "vector-set-length!";


SCM 
scm_vector_set_length_x (SCM vect, SCM len)
{
  long l;
  scm_sizet siz;
  scm_sizet sz;
  char *base;

  l = SCM_INUM (len);
  SCM_ASRTGO (SCM_NIMP (vect), badarg1);

#ifdef HAVE_ARRAYS
  if (SCM_TYP7 (vect) == scm_tc7_bvect)
    {
      l = (l + SCM_LONG_BIT - 1) / SCM_LONG_BIT;
    }
  sz = scm_uniform_element_size (vect);
  if (sz != 0)
    base = SCM_UVECTOR_BASE (vect);
  else
#endif
  switch (SCM_TYP7 (vect))
    {
    default:
    badarg1: scm_wta (vect, (char *) SCM_ARG1, s_vector_set_length_x);
    case scm_tc7_string:
      SCM_ASRTGO (!SCM_EQ_P (vect, scm_nullstr), badarg1);
      sz = sizeof (char);
      base = SCM_STRING_CHARS (vect);
      l++;
      break;
    case scm_tc7_vector:
    case scm_tc7_wvect:
      SCM_ASRTGO (!SCM_EQ_P (vect, scm_nullvect), badarg1);
      sz = sizeof (SCM);
      base = (char *) SCM_VECTOR_BASE (vect);
      break;
    }
  SCM_ASSERT (SCM_INUMP (len), len, SCM_ARG2, s_vector_set_length_x);
  if (!l)
    l = 1L;
  siz = l * sz;
  if (siz != l * sz)
    scm_memory_error (s_vector_set_length_x);
  SCM_REDEFER_INTS;
  SCM_SETCHARS (vect,
	    ((char *)
	     scm_must_realloc (base,
			       (long) SCM_LENGTH (vect) * sz,
			       (long) siz,
			       s_vector_set_length_x)));
  if (SCM_VECTORP (vect))
    {
      sz = SCM_LENGTH (vect);
      while (l > sz)
	SCM_VELTS (vect)[--l] = SCM_UNSPECIFIED;
    }
  else if (SCM_STRINGP (vect))
    SCM_STRING_CHARS (vect)[l - 1] = 0;
  SCM_SETLENGTH (vect, SCM_INUM (len), SCM_TYP7 (vect));
  SCM_REALLOW_INTS;
  return vect;
}

#endif /* (SCM_DEBUG_DEPRECATED == 0) */

SCM_DEFINE (scm_vector_p, "vector?", 1, 0, 0, 
	    (SCM obj),
	    "Returns @t{#t} if @var{obj} is a vector, otherwise returns @t{#f}. (r5rs)")
#define FUNC_NAME s_scm_vector_p
{
  if (SCM_IMP (obj))
    return SCM_BOOL_F;
  return SCM_BOOL (SCM_VECTORP (obj));
}
#undef FUNC_NAME

SCM_GPROC (s_vector_length, "vector-length", 1, 0, 0, scm_vector_length, g_vector_length);
/* Returns the number of elements in @var{vector} as an exact integer. (r5rs) */
SCM
scm_vector_length (SCM v)
{
  SCM_GASSERT1 (SCM_VECTORP(v),
		g_vector_length, v, SCM_ARG1, s_vector_length);
  return SCM_MAKINUM (SCM_VECTOR_LENGTH (v));
}

SCM_REGISTER_PROC (s_list_to_vector, "list->vector", 1, 0, 0, scm_vector);
/*
	    "@samp{List->vector} returns a newly\n"
	    "created vector initialized to the elements of the list @var{list}.\n\n"
	    "@format\n"
	    "@t{(vector->list '#(dah dah didah))\n"
	    "=>  (dah dah didah)\n"
	    "list->vector '(dididit dah))\n"
	    "=>  #(dididit dah)\n"
	    "}\n"
	    "@end format")
*/
SCM_DEFINE (scm_vector, "vector", 0, 0, 1, 
	    (SCM l),
	    "@deffnx primitive list->vector l\n"
	    "Returns a newly allocated vector whose elements contain the given\n"
	    "arguments.  Analogous to @samp{list}. (r5rs)\n\n"
	    "@format\n"
	    "@t{(vector 'a 'b 'c)                      ==>  #(a b c) }\n"
	    "@end format")
#define FUNC_NAME s_scm_vector
{
  SCM res;
  register SCM *data;
  int i;
  SCM_VALIDATE_LIST_COPYLEN (1,l,i);
  res = scm_c_make_vector (i, SCM_UNSPECIFIED);
  data = SCM_VELTS (res);
  for(; i && SCM_NIMP(l); --i, l = SCM_CDR (l))
    *data++ = SCM_CAR (l);
  return res;
}
#undef FUNC_NAME

SCM_GPROC (s_vector_ref, "vector-ref", 2, 0, 0, scm_vector_ref, g_vector_ref);

/*
           "@var{k} must be a valid index of @var{vector}.\n"
	   "@samp{Vector-ref} returns the contents of element @var{k} of\n"
	   "@var{vector}.\n\n"
	   "@format\n"
	   "@t{(vector-ref '#(1 1 2 3 5 8 13 21)\n"
	   "                5)\n"
	   "        ==>  8\n"
	   "(vector-ref '#(1 1 2 3 5 8 13 21)\n"
	   "    (let ((i (round (* 2 (acos -1)))))\n"
	   "      (if (inexact? i)\n"
	   "        (inexact->exact i)\n"
	   "           i))) \n"
	   "     ==> 13\n"
	   "}\n"
	   "@end format"
*/

SCM
scm_vector_ref (SCM v, SCM k)
#define FUNC_NAME s_vector_ref
{
  SCM_GASSERT2 (SCM_VECTORP (v),
		g_vector_ref, v, k, SCM_ARG1, s_vector_ref);
  SCM_GASSERT2 (SCM_INUMP (k),
		g_vector_ref, v, k, SCM_ARG2, s_vector_ref);
  SCM_ASSERT_RANGE (2, k, SCM_INUM (k) < SCM_VECTOR_LENGTH (v) && SCM_INUM (k) >= 0);
  return SCM_VELTS (v)[(long) SCM_INUM (k)];
}
#undef FUNC_NAME

SCM_GPROC (s_vector_set_x, "vector-set!", 3, 0, 0, scm_vector_set_x, g_vector_set_x);

/* (r5rs)
@var{k} must be a valid index of @var{vector}.
@samp{Vector-set!} stores @var{obj} in element @var{k} of @var{vector}.
The value returned by @samp{vector-set!} is unspecified.  
@c  <!>


@format
@t{(let ((vec (vector 0 '(2 2 2 2) "Anna")))
  (vector-set! vec 1 '("Sue" "Sue"))
  vec)      
          ==>  #(0 ("Sue" "Sue") "Anna")

(vector-set! '#(0 1 2) 1 "doe")  
          ==>  @emph{error}  ; constant vector
}
@end format
*/

SCM
scm_vector_set_x (SCM v, SCM k, SCM obj)
#define FUNC_NAME s_vector_set_x
{
  SCM_GASSERTn (SCM_VECTORP (v),
		g_vector_set_x, SCM_LIST3 (v, k, obj),
		SCM_ARG1, s_vector_set_x);
  SCM_GASSERTn (SCM_INUMP (k),
		g_vector_set_x, SCM_LIST3 (v, k, obj),
		SCM_ARG2, s_vector_set_x);
  SCM_ASSERT_RANGE (2, k, SCM_INUM (k) < SCM_VECTOR_LENGTH (v) && SCM_INUM (k) >= 0);
  SCM_VELTS(v)[(long) SCM_INUM(k)] = obj;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_vector, "make-vector", 1, 1, 0,
            (SCM k, SCM fill),
	    "Returns a newly allocated vector of @var{k} elements.  If a second\n"
	    "argument is given, then each element is initialized to @var{fill}.\n"
	    "Otherwise the initial contents of each element is unspecified. (r5rs)")
#define FUNC_NAME s_scm_make_vector
{
  SCM_VALIDATE_INUM_MIN (1, k, 0);
  if (SCM_UNBNDP (fill))
    fill = SCM_UNSPECIFIED;
  return scm_c_make_vector (SCM_INUM (k), fill);
}
#undef FUNC_NAME

SCM
scm_c_make_vector (unsigned long int k, SCM fill)
#define FUNC_NAME s_scm_make_vector
{
  SCM v;
  scm_bits_t *velts;

  SCM_NEWCELL (v);

  velts = (k != 0)
    ? scm_must_malloc (k * sizeof (scm_bits_t), FUNC_NAME)
    : NULL;

  SCM_DEFER_INTS;
  {
    unsigned long int j;

    for (j = 0; j != k; ++j)
      velts[j] = SCM_UNPACK (fill);

    SCM_SET_VECTOR_BASE (v, velts);
    SCM_SET_VECTOR_LENGTH (v, k, scm_tc7_vector);
  }
  SCM_ALLOW_INTS;

  return v;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_to_list, "vector->list", 1, 0, 0, 
           (SCM v),
	    "@samp{Vector->list} returns a newly allocated list of the objects contained\n"
	    "in the elements of @var{vector}.  (r5rs)\n\n"
	    "@format\n"
	    "@t{(vector->list '#(dah dah didah))\n"
	    "=>  (dah dah didah)\n"
	    "list->vector '(dididit dah))\n"
	    "=>  #(dididit dah)\n"
	    "}\n"
	    "@end format")
#define FUNC_NAME s_scm_vector_to_list
{
  SCM res = SCM_EOL;
  long i;
  SCM *data;
  SCM_VALIDATE_VECTOR (1,v);
  data = SCM_VELTS(v);
  for(i = SCM_VECTOR_LENGTH(v)-1;i >= 0;i--) res = scm_cons(data[i], res);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vector_fill_x, "vector-fill!", 2, 0, 0,
            (SCM v, SCM fill_x),
	    "Stores @var{fill} in every element of @var{vector}.\n"
	    "The value returned by @samp{vector-fill!} is unspecified. (r5rs)")
#define FUNC_NAME s_scm_vector_fill_x
{
  register long i;
  register SCM *data;
  SCM_VALIDATE_VECTOR (1,v);
  data = SCM_VELTS(v);
  for(i = SCM_VECTOR_LENGTH(v) - 1; i >= 0; i--)
    data[i] = fill_x;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM
scm_vector_equal_p(SCM x, SCM y)
{
  long i;
  for(i = SCM_VECTOR_LENGTH(x)-1;i >= 0;i--)
    if (SCM_FALSEP(scm_equal_p(SCM_VELTS(x)[i], SCM_VELTS(y)[i])))
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}


SCM_DEFINE (scm_vector_move_left_x, "vector-move-left!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Vector version of @code{substring-move-left!}.")
#define FUNC_NAME s_scm_vector_move_left_x
{
  long i;
  long j;
  long e;
  
  SCM_VALIDATE_VECTOR (1,vec1);
  SCM_VALIDATE_INUM_COPY (2,start1,i);
  SCM_VALIDATE_INUM_COPY (3,end1,e);
  SCM_VALIDATE_VECTOR (4,vec2);
  SCM_VALIDATE_INUM_COPY (5,start2,j);
  SCM_ASSERT_RANGE (2, start1, i <= SCM_VECTOR_LENGTH (vec1) && i >= 0);
  SCM_ASSERT_RANGE (5, start2, j <= SCM_VECTOR_LENGTH (vec2) && j >= 0);
  SCM_ASSERT_RANGE (3, end1, e <= SCM_VECTOR_LENGTH (vec1) && e >= 0);
  SCM_ASSERT_RANGE (5, start2, e-i+j <= SCM_VECTOR_LENGTH (vec2));
  while (i<e) SCM_VELTS (vec2)[j++] = SCM_VELTS (vec1)[i++];
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_move_right_x, "vector-move-right!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Vector version of @code{substring-move-right!}.")
#define FUNC_NAME s_scm_vector_move_right_x
{
  long i;
  long j;
  long e;

  SCM_VALIDATE_VECTOR (1,vec1);
  SCM_VALIDATE_INUM_COPY (2,start1,i);
  SCM_VALIDATE_INUM_COPY (3,end1,e);
  SCM_VALIDATE_VECTOR (4,vec2);
  SCM_VALIDATE_INUM_COPY (5,start2,j);
  SCM_ASSERT_RANGE (2, start1, i <= SCM_VECTOR_LENGTH (vec1) && i >= 0);
  SCM_ASSERT_RANGE (5, start2, j <= SCM_VECTOR_LENGTH (vec2) && j >= 0);
  SCM_ASSERT_RANGE (3, end1, e <= SCM_VECTOR_LENGTH (vec1) && e >= 0);
  j = e - i + j;
  SCM_ASSERT_RANGE (5, start2, j <= SCM_VECTOR_LENGTH (vec2));
  while (i < e)
    SCM_VELTS (vec2)[--j] = SCM_VELTS (vec1)[--e];
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_init_vectors ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/vectors.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
