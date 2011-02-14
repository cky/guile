/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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
#include "libguile/eq.h"
#include "libguile/root.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/vectors.h"
#include "libguile/arrays.h" /* Hit me with the ugly stick */
#include "libguile/generalized-vectors.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"
#include "libguile/dynwind.h"
#include "libguile/deprecation.h"

#include "libguile/bdw-gc.h"




#define VECTOR_MAX_LENGTH (SCM_T_BITS_MAX >> 8)

int
scm_is_vector (SCM obj)
{
  if (SCM_I_IS_VECTOR (obj))
    return 1;
  if  (SCM_I_ARRAYP (obj) && SCM_I_ARRAY_NDIM (obj) == 1)
    {
      SCM v = SCM_I_ARRAY_V (obj);
      return SCM_I_IS_VECTOR (v);
    }
  return 0;
}

int
scm_is_simple_vector (SCM obj)
{
  return SCM_I_IS_VECTOR (obj);
}

const SCM *
scm_vector_elements (SCM vec, scm_t_array_handle *h,
		     size_t *lenp, ssize_t *incp)
{
  if (SCM_I_WVECTP (vec))
    /* FIXME: We should check each (weak) element of the vector for NULL and
       convert it to SCM_BOOL_F.  */
    abort ();

  scm_generalized_vector_get_handle (vec, h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_elements (h);
}

SCM *
scm_vector_writable_elements (SCM vec, scm_t_array_handle *h,
			      size_t *lenp, ssize_t *incp)
{
  if (SCM_I_WVECTP (vec))
    /* FIXME: We should check each (weak) element of the vector for NULL and
       convert it to SCM_BOOL_F.  */
    abort ();

  scm_generalized_vector_get_handle (vec, h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_writable_elements (h);
}

SCM_DEFINE (scm_vector_p, "vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_vector_p
{
  return scm_from_bool (scm_is_vector (obj));
}
#undef FUNC_NAME

SCM_GPROC (s_vector_length, "vector-length", 1, 0, 0, scm_vector_length, g_vector_length);
/* Returns the number of elements in @var{vector} as an exact integer.  */
SCM
scm_vector_length (SCM v)
{
  if (SCM_I_IS_VECTOR (v))
    return scm_from_size_t (SCM_I_VECTOR_LENGTH (v));
  else if (SCM_I_ARRAYP (v) && SCM_I_ARRAY_NDIM (v) == 1)
    {
      scm_t_array_dim *dim = SCM_I_ARRAY_DIMS (v);
      return scm_from_size_t (dim->ubnd - dim->lbnd + 1);
    }
  else
    SCM_WTA_DISPATCH_1 (g_vector_length, v, 1, NULL);
}

size_t
scm_c_vector_length (SCM v)
{
  if (SCM_I_IS_VECTOR (v))
    return SCM_I_VECTOR_LENGTH (v);
  else
    return scm_to_size_t (scm_vector_length (v));
}

SCM_REGISTER_PROC (s_list_to_vector, "list->vector", 1, 0, 0, scm_vector);
/*
	    "Return a newly created vector initialized to the elements of"
	    "the list @var{list}.\n\n"
	    "@lisp\n"
	    "(vector->list '#(dah dah didah)) @result{} (dah dah didah)\n"
	    "(list->vector '(dididit dah)) @result{}   #(dididit dah)\n"
	    "@end lisp")
*/
SCM_DEFINE (scm_vector, "vector", 0, 0, 1, 
	    (SCM l),
	    "@deffnx {Scheme Procedure} list->vector l\n"
	    "Return a newly allocated vector composed of the\n"
	    "given arguments.  Analogous to @code{list}.\n"
	    "\n"
	    "@lisp\n"
	    "(vector 'a 'b 'c) @result{} #(a b c)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_vector
{
  SCM res;
  SCM *data;
  long i, len;
  scm_t_array_handle handle;

  SCM_VALIDATE_LIST_COPYLEN (1, l, len);

  res = scm_c_make_vector (len, SCM_UNSPECIFIED);
  data = scm_vector_writable_elements (res, &handle, NULL, NULL);
  i = 0;
  while (scm_is_pair (l) && i < len) 
    {
      data[i] = SCM_CAR (l);
      l = SCM_CDR (l);
      i += 1;
    }

  scm_array_handle_release (&handle);

  return res;
}
#undef FUNC_NAME

SCM_GPROC (s_vector_ref, "vector-ref", 2, 0, 0, scm_vector_ref, g_vector_ref);

/*
           "@var{k} must be a valid index of @var{vector}.\n"
	   "@samp{Vector-ref} returns the contents of element @var{k} of\n"
	   "@var{vector}.\n\n"
	   "@lisp\n"
	   "(vector-ref '#(1 1 2 3 5 8 13 21) 5) @result{} 8\n"
	   "(vector-ref '#(1 1 2 3 5 8 13 21)\n"
	   "    (let ((i (round (* 2 (acos -1)))))\n"
	   "      (if (inexact? i)\n"
	   "        (inexact->exact i)\n"
	   "           i))) @result{} 13\n"
	   "@end lisp"
*/

SCM
scm_vector_ref (SCM v, SCM k)
#define FUNC_NAME s_vector_ref
{
  return scm_c_vector_ref (v, scm_to_size_t (k));
}
#undef FUNC_NAME

SCM
scm_c_vector_ref (SCM v, size_t k)
{
  if (SCM_I_IS_VECTOR (v))
    {
      register SCM elt;

      if (k >= SCM_I_VECTOR_LENGTH (v))
	scm_out_of_range (NULL, scm_from_size_t (k));
      elt = (SCM_I_VECTOR_ELTS(v))[k];

      if ((elt == SCM_PACK (NULL)) && SCM_I_WVECTP (v))
	/* ELT was a weak pointer and got nullified by the GC.  */
	return SCM_BOOL_F;

      return elt;
    }
  else if (SCM_I_ARRAYP (v) && SCM_I_ARRAY_NDIM (v) == 1)
    {
      scm_t_array_dim *dim = SCM_I_ARRAY_DIMS (v);
      SCM vv = SCM_I_ARRAY_V (v);
      if (SCM_I_IS_VECTOR (vv))
	{
	  register SCM elt;

	  if (k >= dim->ubnd - dim->lbnd + 1)
	    scm_out_of_range (NULL, scm_from_size_t (k));
	  k = SCM_I_ARRAY_BASE (v) + k*dim->inc;
	  elt = (SCM_I_VECTOR_ELTS (vv))[k];

	  if ((elt == SCM_PACK (NULL)) && (SCM_I_WVECTP (vv)))
	    /* ELT was a weak pointer and got nullified by the GC.  */
	    return SCM_BOOL_F;

	  return elt;
	}
      scm_wrong_type_arg_msg (NULL, 0, v, "non-uniform vector");
    }
  else
    SCM_WTA_DISPATCH_2 (g_vector_ref, v, scm_from_size_t (k), 2, NULL);
}

SCM_GPROC (s_vector_set_x, "vector-set!", 3, 0, 0, scm_vector_set_x, g_vector_set_x);

/* "@var{k} must be a valid index of @var{vector}.\n"
   "@code{Vector-set!} stores @var{obj} in element @var{k} of @var{vector}.\n"
   "The value returned by @samp{vector-set!} is unspecified.\n"
   "@lisp\n"
   "(let ((vec (vector 0 '(2 2 2 2) "Anna")))\n"
   "  (vector-set! vec 1 '("Sue" "Sue"))\n"
   "  vec) @result{}  #(0 ("Sue" "Sue") "Anna")\n"
   "(vector-set! '#(0 1 2) 1 "doe") @result{} @emph{error} ; constant vector\n"
   "@end lisp"
*/

SCM
scm_vector_set_x (SCM v, SCM k, SCM obj)
#define FUNC_NAME s_vector_set_x
{
  scm_c_vector_set_x (v, scm_to_size_t (k), obj);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_vector_set_x (SCM v, size_t k, SCM obj)
{
  if (SCM_I_IS_VECTOR (v))
    {
      if (k >= SCM_I_VECTOR_LENGTH (v))
	scm_out_of_range (NULL, scm_from_size_t (k)); 
      (SCM_I_VECTOR_WELTS(v))[k] = obj;
      if (SCM_I_WVECTP (v))
	{
	  /* Make it a weak pointer.  */
	  GC_PTR link = (GC_PTR) & ((SCM_I_VECTOR_WELTS (v))[k]);
	  SCM_I_REGISTER_DISAPPEARING_LINK (link, obj);
	}
    }
  else if (SCM_I_ARRAYP (v) && SCM_I_ARRAY_NDIM (v) == 1)
    {
      scm_t_array_dim *dim = SCM_I_ARRAY_DIMS (v);
      SCM vv = SCM_I_ARRAY_V (v);
      if (SCM_I_IS_VECTOR (vv))
	{
	  if (k >= dim->ubnd - dim->lbnd + 1)
	    scm_out_of_range (NULL, scm_from_size_t (k));
	  k = SCM_I_ARRAY_BASE (v) + k*dim->inc;
	  (SCM_I_VECTOR_WELTS (vv))[k] = obj;

	  if (SCM_I_WVECTP (vv))
	    {
	      /* Make it a weak pointer.  */
	      GC_PTR link = (GC_PTR) & ((SCM_I_VECTOR_WELTS (vv))[k]);
	      SCM_I_REGISTER_DISAPPEARING_LINK (link, obj);
	    }
	}
      else
	scm_wrong_type_arg_msg (NULL, 0, v, "non-uniform vector");
    }
  else
    {
      if (SCM_UNPACK (g_vector_set_x))
	scm_apply_generic (g_vector_set_x,
			   scm_list_3 (v, scm_from_size_t (k), obj));
      else
	scm_wrong_type_arg_msg (NULL, 0, v, "vector");
    }
}

SCM_DEFINE (scm_make_vector, "make-vector", 1, 1, 0,
            (SCM k, SCM fill),
	    "Return a newly allocated vector of @var{k} elements.  If a\n"
	    "second argument is given, then each position is initialized to\n"
	    "@var{fill}.  Otherwise the initial contents of each position is\n"
	    "unspecified.")
#define FUNC_NAME s_scm_make_vector
{
  size_t l = scm_to_unsigned_integer (k, 0, VECTOR_MAX_LENGTH);

  if (SCM_UNBNDP (fill))
    fill = SCM_UNSPECIFIED;
  
  return scm_c_make_vector (l, fill);
}
#undef FUNC_NAME


SCM
scm_c_make_vector (size_t k, SCM fill)
#define FUNC_NAME s_scm_make_vector
{
  SCM *vector;

  vector = (SCM *)
    scm_gc_malloc ((k + SCM_I_VECTOR_HEADER_SIZE) * sizeof (SCM),
		   "vector");

  if (k > 0)
    {
      SCM *base;
      unsigned long int j;

      SCM_ASSERT_RANGE (1, scm_from_ulong (k), k <= VECTOR_MAX_LENGTH);

      base = vector + SCM_I_VECTOR_HEADER_SIZE;
      for (j = 0; j != k; ++j)
	base[j] = fill;
    }

  ((scm_t_bits *) vector)[0] = (k << 8) | scm_tc7_vector;
  ((scm_t_bits *) vector)[1] = 0;

  return PTR2SCM (vector);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_copy, "vector-copy", 1, 0, 0,
	    (SCM vec),
	    "Return a copy of @var{vec}.")
#define FUNC_NAME s_scm_vector_copy
{
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  const SCM *src;
  SCM result, *dst;

  src = scm_vector_elements (vec, &handle, &len, &inc);

  result = scm_c_make_vector (len, SCM_UNDEFINED);
  dst = SCM_I_VECTOR_WELTS (result);
  for (i = 0; i < len; i++, src += inc)
    dst[i] = *src;

  scm_array_handle_release (&handle);

  return result;
}
#undef FUNC_NAME


/* Weak vectors.  */

/* Allocate memory for the elements of a weak vector on behalf of the
   caller.  */
static SCM
make_weak_vector (scm_t_bits type, size_t c_size)
{
  SCM *vector;
  size_t total_size;

  total_size = (c_size + SCM_I_VECTOR_HEADER_SIZE) * sizeof (SCM);
  vector = (SCM *) scm_gc_malloc_pointerless (total_size, "weak vector");

  ((scm_t_bits *) vector)[0] = (c_size << 8) | scm_tc7_wvect;
  ((scm_t_bits *) vector)[1] = type;

  return PTR2SCM (vector);
}

/* Return a new weak vector.  The allocated vector will be of the given weak
   vector subtype.  It will contain SIZE elements which are initialized with
   the FILL object, or, if FILL is undefined, with an unspecified object.  */
SCM
scm_i_make_weak_vector (scm_t_bits type, SCM size, SCM fill)
{
  SCM wv, *base;
  size_t c_size, j;

  if (SCM_UNBNDP (fill))
    fill = SCM_UNSPECIFIED;

  c_size = scm_to_unsigned_integer (size, 0, VECTOR_MAX_LENGTH);
  wv = make_weak_vector (type, c_size);
  base = SCM_I_WVECT_GC_WVELTS (wv);

  for (j = 0; j != c_size; ++j)
    base[j] = fill;

  return wv;
}

/* Return a new weak vector with type TYPE and whose content are taken from
   list LST.  */
SCM
scm_i_make_weak_vector_from_list (scm_t_bits type, SCM lst)
{
  SCM wv, *elt;
  long c_size;

  c_size = scm_ilength (lst);
  SCM_ASSERT (c_size >= 0, lst, SCM_ARG2, "scm_i_make_weak_vector_from_list");

  wv = make_weak_vector(type, (size_t) c_size);

  for (elt = SCM_I_WVECT_GC_WVELTS (wv);
       scm_is_pair (lst);
       lst = SCM_CDR (lst), elt++)
    {
      *elt = SCM_CAR (lst);
    }

  return wv;
}



SCM_DEFINE (scm_vector_to_list, "vector->list", 1, 0, 0, 
	    (SCM v),
	    "Return a newly allocated list composed of the elements of @var{v}.\n"
	    "\n"
	    "@lisp\n"
	    "(vector->list '#(dah dah didah)) @result{}  (dah dah didah)\n"
	    "(list->vector '(dididit dah)) @result{}  #(dididit dah)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_vector_to_list
{
  SCM res = SCM_EOL;
  const SCM *data;
  scm_t_array_handle handle;
  size_t i, count, len;
  ssize_t inc;

  data = scm_vector_elements (v, &handle, &len, &inc);
  for (i = (len - 1) * inc, count = 0;
       count < len;
       i -= inc, count++)
    res = scm_cons (data[i], res);

  scm_array_handle_release (&handle);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vector_fill_x, "vector-fill!", 2, 0, 0,
            (SCM v, SCM fill),
	    "Store @var{fill} in every position of @var{vector}.  The value\n"
	    "returned by @code{vector-fill!} is unspecified.")
#define FUNC_NAME s_scm_vector_fill_x
{
  scm_t_array_handle handle;
  SCM *data;
  size_t i, len;
  ssize_t inc;

  data = scm_vector_writable_elements (v, &handle, &len, &inc);
  for (i = 0; i < len; i += inc)
    data[i] = fill;
  scm_array_handle_release (&handle);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM
scm_i_vector_equal_p (SCM x, SCM y)
{
  long i;
  for (i = SCM_I_VECTOR_LENGTH (x) - 1; i >= 0; i--)
    if (scm_is_false (scm_equal_p (SCM_I_VECTOR_ELTS (x)[i],
				   SCM_I_VECTOR_ELTS (y)[i])))
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}


SCM_DEFINE (scm_vector_move_left_x, "vector-move-left!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Copy elements from @var{vec1}, positions @var{start1} to @var{end1},\n"
	    "to @var{vec2} starting at position @var{start2}.  @var{start1} and\n"
	    "@var{start2} are inclusive indices; @var{end1} is exclusive.\n\n"
	    "@code{vector-move-left!} copies elements in leftmost order.\n"
	    "Therefore, in the case where @var{vec1} and @var{vec2} refer to the\n"
	    "same vector, @code{vector-move-left!} is usually appropriate when\n"
	    "@var{start1} is greater than @var{start2}.")
#define FUNC_NAME s_scm_vector_move_left_x
{
  scm_t_array_handle handle1, handle2;
  const SCM *elts1;
  SCM *elts2;
  size_t len1, len2;
  ssize_t inc1, inc2;
  size_t i, j, e;
  
  elts1 = scm_vector_elements (vec1, &handle1, &len1, &inc1);
  elts2 = scm_vector_writable_elements (vec2, &handle2, &len2, &inc2);

  i = scm_to_unsigned_integer (start1, 0, len1);
  e = scm_to_unsigned_integer (end1, i, len1);
  SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) <= len2);
  j = scm_to_unsigned_integer (start2, 0, len2);
  SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
  i *= inc1;
  e *= inc1;
  j *= inc2;
  for (; i < e; i += inc1, j += inc2)
    elts2[j] = elts1[i];

  scm_array_handle_release (&handle2);
  scm_array_handle_release (&handle1);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_move_right_x, "vector-move-right!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Copy elements from @var{vec1}, positions @var{start1} to @var{end1},\n"
	    "to @var{vec2} starting at position @var{start2}.  @var{start1} and\n"
	    "@var{start2} are inclusive indices; @var{end1} is exclusive.\n\n"
	    "@code{vector-move-right!} copies elements in rightmost order.\n"
	    "Therefore, in the case where @var{vec1} and @var{vec2} refer to the\n"
	    "same vector, @code{vector-move-right!} is usually appropriate when\n"
	    "@var{start1} is less than @var{start2}.")
#define FUNC_NAME s_scm_vector_move_right_x
{
  scm_t_array_handle handle1, handle2;
  const SCM *elts1;
  SCM *elts2;
  size_t len1, len2;
  ssize_t inc1, inc2;
  size_t i, j, e;
  
  elts1 = scm_vector_elements (vec1, &handle1, &len1, &inc1);
  elts2 = scm_vector_writable_elements (vec2, &handle2, &len2, &inc2);

  i = scm_to_unsigned_integer (start1, 0, len1);
  e = scm_to_unsigned_integer (end1, i, len1);
  SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) <= len2);
  j = scm_to_unsigned_integer (start2, 0, len2);
  SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
  j += (e - i);
  
  i *= inc1;
  e *= inc1;
  j *= inc2;
  while (i < e)
    {
      e -= inc1;
      j -= inc2;
      elts2[j] = elts1[e];
    }

  scm_array_handle_release (&handle2);
  scm_array_handle_release (&handle1);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static SCM
vector_handle_ref (scm_t_array_handle *h, size_t idx)
{
  if (idx > h->dims[0].ubnd)
    scm_out_of_range ("vector-handle-ref", scm_from_size_t (idx));
  return ((SCM*)h->elements)[idx];
}

static void
vector_handle_set (scm_t_array_handle *h, size_t idx, SCM val)
{
  if (idx > h->dims[0].ubnd)
    scm_out_of_range ("vector-handle-set!", scm_from_size_t (idx));
  ((SCM*)h->writable_elements)[idx] = val;
}

static void
vector_get_handle (SCM v, scm_t_array_handle *h)
{
  h->array = v;
  h->ndims = 1;
  h->dims = &h->dim0;
  h->dim0.lbnd = 0;
  h->dim0.ubnd = SCM_I_VECTOR_LENGTH (v) - 1;
  h->dim0.inc = 1;
  h->element_type = SCM_ARRAY_ELEMENT_TYPE_SCM;
  h->elements = h->writable_elements = SCM_I_VECTOR_WELTS (v);
}

/* the & ~2 allows catching scm_tc7_wvect as well. needs changing if you change
   tags.h. */
SCM_ARRAY_IMPLEMENTATION (scm_tc7_vector, 0x7f & ~2,
                          vector_handle_ref, vector_handle_set,
                          vector_get_handle)
SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_SCM, scm_make_vector)


void
scm_init_vectors ()
{
#include "libguile/vectors.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
