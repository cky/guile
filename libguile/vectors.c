/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "_scm.h"
#include "eq.h"

#include "vectors.h"



SCM_PROC(s_vector_p, "vector?", 1, 0, 0, scm_vector_p);

SCM
scm_vector_p(x)
     SCM x;
{
  if SCM_IMP(x) return SCM_BOOL_F;
  return SCM_VECTORP(x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_vector_length, "vector-length", 1, 0, 0, scm_vector_length);

SCM
scm_vector_length(v)
     SCM v;
{
  SCM_ASSERT(SCM_NIMP(v) && SCM_VECTORP(v), v, SCM_ARG1, s_vector_length);
  return SCM_MAKINUM(SCM_LENGTH(v));
}

SCM_PROC(s_list_to_vector, "list->vector", 1, 0, 0, scm_vector);
SCM_PROC(s_vector, "vector", 0, 0, 1, scm_vector);

SCM
scm_vector(l)
     SCM l;
{
  SCM res;
  register SCM *data;
  long i = scm_ilength(l);
  SCM_ASSERT(i >= 0, l, SCM_ARG1, s_vector);
  res = scm_make_vector(SCM_MAKINUM(i), SCM_UNSPECIFIED, SCM_UNDEFINED);
  data = SCM_VELTS(res);
  for(;i && SCM_NIMP(l);--i, l = SCM_CDR(l))
    *data++ = SCM_CAR(l);
  return res;
}

SCM_PROC(s_vector_ref, "vector-ref", 2, 0, 0, scm_vector_ref);

SCM
scm_vector_ref(v, k)
     SCM v;
     SCM k;
{
  SCM_ASSERT(SCM_NIMP(v) && SCM_VECTORP(v), v, SCM_ARG1, s_vector_ref);
  SCM_ASSERT(SCM_INUMP(k), k, SCM_ARG2, s_vector_ref);
  SCM_ASSERT((SCM_INUM(k) < SCM_LENGTH(v)) && (SCM_INUM(k) >= 0), k, SCM_OUTOFRANGE, s_vector_ref);
  return SCM_VELTS(v)[((long) SCM_INUM(k))];
}


SCM_PROC(s_vector_set_x, "vector-set!", 3, 0, 0, scm_vector_set_x);

SCM
scm_vector_set_x(v, k, obj)
     SCM v;
     SCM k;
     SCM obj;
{
  SCM_ASSERT(SCM_NIMP(v) && SCM_VECTORP(v), v, SCM_ARG1, s_vector_set_x);
  SCM_ASSERT(SCM_INUMP(k), k, SCM_ARG2, s_vector_set_x);
  SCM_ASSERT((SCM_INUM(k) < SCM_LENGTH(v)) && (SCM_INUM(k) >= 0), k, SCM_OUTOFRANGE, s_vector_set_x);
  SCM_VELTS(v)[((long) SCM_INUM(k))] = obj;
  return obj;
}


SCM_PROC(s_make_vector, "make-vector", 1, 2, 0, scm_make_vector);

SCM
scm_make_vector(k, fill, multip)
     SCM k;
     SCM fill;
     SCM multip;
{
  SCM v;
  int multi;
  register long i;
  register long j;
  register SCM *velts;

  SCM_ASSERT(SCM_INUMP(k) && (0 <= SCM_INUM (k)), k, SCM_ARG1, s_make_vector);
  if (SCM_UNBNDP(fill))
    fill = SCM_UNSPECIFIED;
  multi = !(SCM_UNBNDP(multip) || SCM_FALSEP(multip));
  i = SCM_INUM(k);
  SCM_NEWCELL(v);
  SCM_DEFER_INTS;
  SCM_SETCHARS(v, scm_must_malloc(i?(long)(i*sizeof(SCM)):1L, s_vector));
  SCM_SETLENGTH(v, i, scm_tc7_vector);
  velts = SCM_VELTS(v);
  j = 0;
  if (multi)
    {
      while ((fill != SCM_EOL) && (j < i))
	{
	  (velts)[j++] = SCM_CAR (fill);
	  fill = SCM_CDR (fill);
	}
    }
  while(--i >= j) (velts)[i] = fill;
  SCM_ALLOW_INTS;
  return v;
}


SCM_PROC(s_vector_to_list, "vector->list", 1, 0, 0, scm_vector_to_list);

SCM
scm_vector_to_list(v)
     SCM v;
{
  SCM res = SCM_EOL;
  long i;
  SCM *data;
  SCM_ASSERT(SCM_NIMP(v) && SCM_VECTORP(v), v, SCM_ARG1, s_vector_to_list);
  data = SCM_VELTS(v);
  for(i = SCM_LENGTH(v)-1;i >= 0;i--) res = scm_cons(data[i], res);
  return res;
}


SCM_PROC(s_vector_fill_x, "vector-fill!", 2, 0, 0, scm_vector_fill_x);

SCM
scm_vector_fill_x(v, fill_x)
     SCM v;
     SCM fill_x;
{
  register long i;
  register SCM *data;
  SCM_ASSERT(SCM_NIMP(v) && SCM_VECTORP(v), v, SCM_ARG1, s_vector_fill_x);
  data = SCM_VELTS(v);
  for(i = SCM_LENGTH(v)-1;i >= 0;i--) data[i] = fill_x;
  return SCM_UNSPECIFIED;
}



SCM
scm_vector_equal_p(x, y)
     SCM x;
     SCM y;
{
  long i;
  for(i = SCM_LENGTH(x)-1;i >= 0;i--)
    if (SCM_FALSEP(scm_equal_p(SCM_VELTS(x)[i], SCM_VELTS(y)[i])))
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}


SCM_PROC (s_vector_move_left_x, "vector-move-left!", 5, 0, 0, scm_vector_move_left_x);

SCM
scm_vector_move_left_x (vec1, start1, end1, vec2, start2)
     SCM vec1;
     SCM start1;
     SCM end1;
     SCM vec2;
     SCM start2;
{
  long i;
  long j;
  long e;
  
  SCM_ASSERT (SCM_NIMP (vec1) && SCM_VECTORP (vec1), vec1, SCM_ARG1, s_vector_move_left_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, SCM_ARG2, s_vector_move_left_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, SCM_ARG3, s_vector_move_left_x);
  SCM_ASSERT (SCM_NIMP (vec2) && SCM_VECTORP (vec2), vec2, SCM_ARG4, s_vector_move_left_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, SCM_ARG5, s_vector_move_left_x);
  i = SCM_INUM (start1);
  j = SCM_INUM (start2);
  e = SCM_INUM (end1);
  SCM_ASSERT (i <= SCM_LENGTH (vec1) && i >= 0, start1, SCM_OUTOFRANGE, s_vector_move_left_x);
  SCM_ASSERT (j <= SCM_LENGTH (vec2) && j >= 0, start2, SCM_OUTOFRANGE, s_vector_move_left_x);
  SCM_ASSERT (e <= SCM_LENGTH (vec1) && e >= 0, end1, SCM_OUTOFRANGE, s_vector_move_left_x);
  SCM_ASSERT (e-i+j <= SCM_LENGTH (vec2), start2, SCM_OUTOFRANGE, s_vector_move_left_x);
  while (i<e) SCM_VELTS (vec2)[j++] = SCM_VELTS (vec1)[i++];
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_vector_move_right_x, "vector-move-right!", 5, 0, 0, scm_vector_move_right_x);

SCM
scm_vector_move_right_x (vec1, start1, end1, vec2, start2)
     SCM vec1;
     SCM start1;
     SCM end1;
     SCM vec2;
     SCM start2;
{
  long i;
  long j;
  long e;

  SCM_ASSERT (SCM_NIMP (vec1) && SCM_VECTORP (vec1), vec1, SCM_ARG1, s_vector_move_right_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, SCM_ARG2, s_vector_move_right_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, SCM_ARG3, s_vector_move_right_x);
  SCM_ASSERT (SCM_NIMP (vec2) && SCM_VECTORP (vec2), vec2, SCM_ARG4, s_vector_move_right_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, SCM_ARG5, s_vector_move_right_x);
  i = SCM_INUM (start1);
  j = SCM_INUM (start2);
  e = SCM_INUM (end1);
  SCM_ASSERT (i <= SCM_LENGTH (vec1) && i >= 0, start1, SCM_OUTOFRANGE, s_vector_move_right_x);
  SCM_ASSERT (j <= SCM_LENGTH (vec2) && j >= 0, start2, SCM_OUTOFRANGE, s_vector_move_right_x);
  SCM_ASSERT (e <= SCM_LENGTH (vec1) && e >= 0, end1, SCM_OUTOFRANGE, s_vector_move_right_x);
  SCM_ASSERT ((j = e-i+j) <= SCM_LENGTH (vec2), start2, SCM_OUTOFRANGE, s_vector_move_right_x);
  while (i<e) SCM_VELTS (vec2)[--j] = SCM_VELTS (vec1)[--e];
  return SCM_UNSPECIFIED;
}



void
scm_init_vectors ()
{
#include "vectors.x"
}

