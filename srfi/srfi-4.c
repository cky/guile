/* srfi-4.c --- Homogeneous numeric vector datatypes.
 *
 * 	Copyright (C) 2001 Free Software Foundation, Inc.
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

#include "srfi-4.h"


/* For brevity and maintainability, we define our own types for the
   various integer and floating point types.  */
typedef unsigned char int_u8;
typedef signed char int_s8;
typedef unsigned short int_u16;
typedef signed short int_s16;
typedef unsigned int int_u32;
typedef signed int int_s32;
#ifdef HAVE_LONG_LONG
#if SIZEOF_LONG == 8
typedef unsigned long int_u64;
typedef signed long int_s64;
#else
typedef unsigned long long int_u64;
typedef signed long long int_s64;
#endif /* SIZEOF_LONG */
#endif /* HAVE_LONG_LONG */
typedef float float_f32;
typedef double float_f64;


/* Smob type code for homogeneous numeric vectors.  */
int scm_tc16_uvec = 0;


/* Accessor macros for the three components of a homogeneous numeric
   vector:
   - The type tag (one of the symbolic constants below).
   - The vector's length (counted in elements).
   - The address of the data area (holding the elements of the
     vector). */
#define SCM_UVEC_TYPE(u)   (SCM_CELL_WORD_1(u))
#define SCM_UVEC_LENGTH(u) (SCM_CELL_WORD_2(u))
#define SCM_UVEC_BASE(u)   (SCM_CELL_OBJECT_3(u))


/* Symbolic constants encoding the various types of homogeneous
   numeric vectors.  */
#define SCM_UVEC_U8  	0
#define SCM_UVEC_S8  	1
#define SCM_UVEC_U16 	2
#define SCM_UVEC_S16 	3
#define SCM_UVEC_U32 	4
#define SCM_UVEC_S32 	5
#define SCM_UVEC_U64 	6
#define SCM_UVEC_S64 	7
#define SCM_UVEC_F32 	8
#define SCM_UVEC_F64 	9


/* This array maps type tags to the size of the elements.  */
static int uvec_sizes[10] = {1, 1, 2, 2, 4, 4, 8, 8, 4, 8};


/* ================================================================ */
/* SMOB procedures.                                                 */
/* ================================================================ */


/* Smob print hook for homogeneous vectors.  */
static int
uvec_print (SCM uvec, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  switch (SCM_UVEC_TYPE (uvec))
    {
    case SCM_UVEC_U8:
      {
	int_u8 * p = (int_u8 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#u8(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_intprint (*p, 10, port);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_intprint (*p, 10, port);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

    case SCM_UVEC_S8:
      {
	int_s8 * p = (int_s8 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#s8(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_intprint (*p, 10, port);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_intprint (*p, 10, port);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

    case SCM_UVEC_U16:
      {
	int_u16 * p = (int_u16 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#u16(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_intprint (*p, 10, port);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_intprint (*p, 10, port);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

    case SCM_UVEC_S16:
      {
	int_s16 * p = (int_s16 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#s16(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_intprint (*p, 10, port);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_intprint (*p, 10, port);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

    case SCM_UVEC_U32:
      {
	int_u32 * p = (int_u32 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#u32(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_intprint (*p, 10, port);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_intprint (*p, 10, port);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

    case SCM_UVEC_S32:
      {
	int_s32 * p = (int_s32 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#s32(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_intprint (*p, 10, port);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_intprint (*p, 10, port);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

#ifdef HAVE_LONG_LONG
    case SCM_UVEC_U64:
      {
	int_u64 * p = (int_u64 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#u64(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_intprint (*p, 10, port);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_intprint (*p, 10, port);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

    case SCM_UVEC_S64:
      {
	int_s64 * p = (int_s64 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#s64(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_intprint (*p, 10, port);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_intprint (*p, 10, port);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }
#endif

    case SCM_UVEC_F32:
      {
	float_f32 * p = (float_f32 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#f32(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_iprin1 (scm_make_real (*p), port, pstate);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_iprin1 (scm_make_real (*p), port, pstate);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

    case SCM_UVEC_F64:
      {
	float_f64 * p = (float_f64 *) SCM_UVEC_BASE (uvec);
	int i = 0;

	scm_puts ("#f64(", port);
	if (SCM_UVEC_LENGTH (uvec) > 0)
	  {
	    scm_iprin1 (scm_make_real (*p), port, pstate);
	    p++;
	    i++;
	    for (; i < SCM_UVEC_LENGTH (uvec); i++)
	      {
		scm_puts (" ", port);
		scm_iprin1 (scm_make_real (*p), port, pstate);
		p++;
	      }
	  }
	scm_puts (")", port);
	break;
      }

    default:
      abort ();			/* Sanity check.  */
    }
  return 1;
}


/* Smob free hook for homogeneous numeric vectors. */
static size_t
uvec_free (SCM uvec)
{
  scm_gc_free (SCM_UVEC_BASE (uvec),
	       SCM_UVEC_LENGTH (uvec) * uvec_sizes[SCM_UVEC_TYPE (uvec)],
	       "uvec");
  return 0;
}


/* ================================================================ */
/* Utility procedures.                                              */
/* ================================================================ */


/* Create a new, uninitialized homogeneous numeric vector of type TYPE
   with space for LEN elements.  */
static SCM
make_uvec (const char * func_name, int type, int len)
{
  void * p;

  p = scm_gc_malloc (len * uvec_sizes[type], "uvec");
  SCM_RETURN_NEWSMOB3 (scm_tc16_uvec, type, len, p);
}


/* ================================================================ */
/* U8 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_u8vector_p, "u8vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type u8,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_u8vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_U8);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_u8vector, "make-u8vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_u8vector
{
  SCM uvec;
  int_u8 * p;
  int_u8 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_U8, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    {
      unsigned int s = scm_num2uint (fill, 2, FUNC_NAME);
      f = s;
      if ((unsigned int) f != s)
	scm_out_of_range_pos (FUNC_NAME, fill, SCM_MAKINUM (2));
    }
  p = (int_u8 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector, "u8vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_u8vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_u8vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector_length, "u8vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_u8vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U8)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector_ref, "u8vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_u8vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U8)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_short2num (((int_u8 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector_set_x, "u8vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_u8vector_ref
{
  int idx;
  int_u8 f;
  unsigned int s;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U8)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  s = scm_num2uint (value, 3, FUNC_NAME);
  f = s;
  if ((unsigned int) f != s)
    scm_out_of_range_pos (FUNC_NAME, value, SCM_MAKINUM (3));

  ((int_u8 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector_to_list, "u8vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_u8vector_to_list
{
  int idx;
  int_u8 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U8)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_u8 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (SCM_MAKINUM (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_u8vector, "list->u8vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "8-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_u8vector
{
  SCM uvec;
  SCM tmp;
  int_u8 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_U8, n);
  p = (int_u8 *) SCM_UVEC_BASE (uvec);
  tmp = l;
  while (SCM_CONSP (tmp))
    {
      int_u8 f;
      unsigned int s = scm_num2uint (SCM_CAR (tmp), 2, FUNC_NAME);
      f = s;
      if ((unsigned int) f != s)
	scm_out_of_range (FUNC_NAME, SCM_CAR (tmp));
      *p++ = f;
      tmp = SCM_CDR (tmp);
      arg_pos++;
    }
  scm_remember_upto_here_1 (l);
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* S8 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_s8vector_p, "s8vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type s8,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_s8vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_S8);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_s8vector, "make-s8vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_s8vector
{
  SCM uvec;
  int_s8 * p;
  int_s8 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_S8, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    {
      signed int s = scm_num2int (fill, 2, FUNC_NAME);
      f = s;
      if ((signed int) f != s)
	scm_out_of_range_pos (FUNC_NAME, fill, SCM_MAKINUM (2));
    }
  p = (int_s8 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector, "s8vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_s8vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_s8vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector_length, "s8vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_s8vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S8)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector_ref, "s8vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_s8vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S8)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_short2num (((int_s8 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector_set_x, "s8vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_s8vector_ref
{
  int idx;
  int_s8 f;
  signed int s;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S8)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  s = scm_num2int (value, 3, FUNC_NAME);
  f = s;
  if ((signed int) f != s)
    scm_out_of_range_pos (FUNC_NAME, value, SCM_MAKINUM (3));

  ((int_s8 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector_to_list, "s8vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_s8vector_to_list
{
  int idx;
  int_s8 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S8)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_s8 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (SCM_MAKINUM (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_s8vector, "list->s8vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "8-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_s8vector
{
  SCM uvec;
  SCM tmp;
  int_s8 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_S8, n);
  p = (int_s8 *) SCM_UVEC_BASE (uvec);
  tmp = l;
  while (SCM_CONSP (tmp))
    {
      int_s8 f;
      signed int s;

      s = scm_num2int (SCM_CAR (tmp), 2, FUNC_NAME);
      f = s;
      if ((signed int) f != s)
	scm_out_of_range (FUNC_NAME, SCM_CAR (tmp));
      *p++ = f;
      tmp = SCM_CDR (tmp);
      arg_pos++;
    }
  scm_remember_upto_here_1 (l);
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* U16 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_u16vector_p, "u16vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type u16,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_u16vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_U16);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_u16vector, "make-u16vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_u16vector
{
  SCM uvec;
  int_u16 * p;
  int_u16 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_U16, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2ushort (fill, 2, FUNC_NAME);
  p = (int_u16 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector, "u16vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_u16vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_u16vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector_length, "u16vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_u16vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U16)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector_ref, "u16vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_u16vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U16)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_ushort2num (((int_u16 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector_set_x, "u16vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_u16vector_ref
{
  int idx;
  int_u16 f;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U16)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  f = scm_num2ushort (value, 3, FUNC_NAME);

  ((int_u16 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector_to_list, "u16vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_u16vector_to_list
{
  int idx;
  int_u16 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U16)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_u16 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (SCM_MAKINUM (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_u16vector, "list->u16vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "16-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_u16vector
{
  SCM uvec;
  int_u16 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_U16, n);
  p = (int_u16 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
    {
      int_u16 f = scm_num2ushort (SCM_CAR (l), 2, FUNC_NAME);
      *p++ = f;
      l = SCM_CDR (l);
      arg_pos++;
    }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* S16 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_s16vector_p, "s16vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type s16,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_s16vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_S16);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_s16vector, "make-s16vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_s16vector
{
  SCM uvec;
  int_s16 * p;
  int_s16 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_S16, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2short (fill, 2, FUNC_NAME);
  p = (int_s16 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector, "s16vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_s16vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_s16vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector_length, "s16vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_s16vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S16)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector_ref, "s16vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_s16vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S16)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_short2num (((int_s16 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector_set_x, "s16vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_s16vector_ref
{
  int idx;
  int_s16 f;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S16)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  f = scm_num2short (value, 3, FUNC_NAME);

  ((int_s16 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector_to_list, "s16vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_s16vector_to_list
{
  int idx;
  int_s16 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S16)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_s16 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (SCM_MAKINUM (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_s16vector, "list->s16vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "16-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_s16vector
{
  SCM uvec;
  SCM tmp;
  int_s16 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_S16, n);
  p = (int_s16 *) SCM_UVEC_BASE (uvec);
  tmp = l;
  while (SCM_CONSP (tmp))
    {
      int_s16 f = scm_num2short (SCM_CAR (tmp), 2, FUNC_NAME);
      *p++ = f;
      tmp = SCM_CDR (tmp);
      arg_pos++;
    }
  scm_remember_upto_here_1 (l);
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* U32 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_u32vector_p, "u32vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type u32,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_u32vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_U32);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_u32vector, "make-u32vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_u32vector
{
  SCM uvec;
  int_u32 * p;
  int_u32 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_U32, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2uint (fill, 2, FUNC_NAME);
  p = (int_u32 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector, "u32vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_u32vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_u32vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector_length, "u32vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_u32vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector_ref, "u32vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_u32vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_uint2num (((int_u32 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector_set_x, "u32vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_u32vector_ref
{
  int idx;
  int_u32 f;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  f = scm_num2uint (value, 3, FUNC_NAME);

  ((int_u32 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector_to_list, "u32vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_u32vector_to_list
{
  int idx;
  int_u32 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_u32 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (scm_uint2num (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_u32vector, "list->u32vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "32-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_u32vector
{
  SCM uvec;
  int_u32 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_U32, n);
  p = (int_u32 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
    {
      int_u32 f;
      f = scm_num2uint (SCM_CAR (l), 2, FUNC_NAME);
      *p++ = f;
      l = SCM_CDR (l);
      arg_pos++;
    }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* S32 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_s32vector_p, "s32vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type s32,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_s32vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_S32);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_s32vector, "make-s32vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_s32vector
{
  SCM uvec;
  int_s32 * p;
  int_s32 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_S32, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2int (fill, 2, FUNC_NAME);
  p = (int_s32 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector, "s32vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_s32vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_s32vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector_length, "s32vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_s32vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector_ref, "s32vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_s32vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_int2num (((int_s32 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector_set_x, "s32vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_s32vector_ref
{
  int idx;
  int_s32 f;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  f = scm_num2int (value, 3, FUNC_NAME);

  ((int_s32 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector_to_list, "s32vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_s32vector_to_list
{
  int idx;
  int_s32 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_s32 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (scm_int2num (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_s32vector, "list->s32vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "32-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_s32vector
{
  SCM uvec;
  int_s32 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_S32, n);
  p = (int_s32 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
    {
      int_s32 f;
      f = scm_num2int (SCM_CAR (l), 2, FUNC_NAME);
      *p++ = f;
      l = SCM_CDR (l);
      arg_pos++;
    }
  return uvec;
}
#undef FUNC_NAME


#ifdef HAVE_LONG_LONG

/* ================================================================ */
/* U64 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_u64vector_p, "u64vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type u64,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_u64vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_U64);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_u64vector, "make-u64vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_u64vector
{
  SCM uvec;
  int_u64 * p;
  int_u64 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_U64, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2ulong_long (fill, 2, FUNC_NAME);
  p = (int_u64 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector, "u64vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_u64vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_u64vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector_length, "u64vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_u64vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector_ref, "u64vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_u64vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_ulong_long2num (((int_u64 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector_set_x, "u64vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_u64vector_ref
{
  int idx;
  int_u64 f;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  f = scm_num2ulong_long (value, 3, FUNC_NAME);

  ((int_u64 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector_to_list, "u64vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_u64vector_to_list
{
  int idx;
  int_u64 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_U64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_u64 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (scm_long_long2num (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_u64vector, "list->u64vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "64-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_u64vector
{
  SCM uvec;
  int_u64 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_U64, n);
  p = (int_u64 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
    {
      int_u64 f;
      f = scm_num2ulong_long (SCM_CAR (l), 2, FUNC_NAME);
      *p++ = f;
      l = SCM_CDR (l);
      arg_pos++;
    }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* S64 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_s64vector_p, "s64vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type s64,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_s64vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_S64);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_s64vector, "make-s64vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_s64vector
{
  SCM uvec;
  int_s64 * p;
  int_s64 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_S64, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2long_long (fill, 2, FUNC_NAME);
  p = (int_s64 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector, "s64vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_s64vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_s64vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector_length, "s64vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_s64vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector_ref, "s64vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_s64vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_long_long2num (((int_s64 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector_set_x, "s64vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_s64vector_ref
{
  int idx;
  int_s64 f;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  f = scm_num2long_long (value, 3, FUNC_NAME);

  ((int_s64 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector_to_list, "s64vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_s64vector_to_list
{
  int idx;
  int_s64 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_S64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_s64 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (scm_long_long2num (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_s64vector, "list->s64vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "64-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_s64vector
{
  SCM uvec;
  int_s64 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_S64, n);
  p = (int_s64 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
    {
      int_s64 f;
      f = scm_num2long_long (SCM_CAR (l), 2, FUNC_NAME);
      *p++ = f;
      l = SCM_CDR (l);
      arg_pos++;
    }
  return uvec;
}
#undef FUNC_NAME

#endif /* HAVE_LONG_LONG */


/* ================================================================ */
/* F32 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_f32vector_p, "f32vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type f32,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_f32vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_F32);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_f32vector, "make-f32vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_f32vector
{
  SCM uvec;
  float_f32 * p;
  float_f32 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_F32, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    {
      double d = scm_num2dbl (fill, FUNC_NAME);
      f = d;
#if 0
      /* This test somehow fails for even the simplest inexact
	 numbers, like 3.1.  Must find out how to check properly.  */
      if (f != d)
	scm_out_of_range_pos (FUNC_NAME, fill, SCM_MAKINUM (2));
#endif /* 0 */
    }
  p = (float_f32 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector, "f32vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_f32vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_f32vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector_length, "f32vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_f32vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_F32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector_ref, "f32vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_f32vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_F32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_make_real (((float_f32 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector_set_x, "f32vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_f32vector_ref
{
  int idx;
  float_f32 f;
  double d;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_F32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  d = scm_num2dbl (value, FUNC_NAME);
  f = d;
#if 0
      /* This test somehow fails for even the simplest inexact
	 numbers, like 3.1.  Must find out how to check properly.  */
  if (f != d)
    scm_out_of_range_pos (FUNC_NAME, value, SCM_MAKINUM (3));
#endif /* 0 */

  ((float_f32 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector_to_list, "f32vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_f32vector_to_list
{
  int idx;
  float_f32 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_F32)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (float_f32 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (scm_make_real (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_f32vector, "list->f32vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "8-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_f32vector
{
  SCM uvec;
  float_f32 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_F32, n);
  p = (float_f32 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
    {
      float_f32 f;
      double d;
      d = scm_num2dbl (SCM_CAR (l), FUNC_NAME);
      f = d;
#if 0
      /* This test somehow fails for even the simplest inexact
	 numbers, like 3.1.  Must find out how to check properly.  */
      if (d != f)
	scm_out_of_range_pos (FUNC_NAME, l, SCM_MAKINUM (1));
#endif /* 0 */
      *p++ = f;
      l = SCM_CDR (l);
      arg_pos++;
    }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* F64 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_f64vector_p, "f64vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type f64,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_f64vector_p
{
  return SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
		   SCM_UVEC_TYPE (obj) == SCM_UVEC_F64);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_f64vector, "make-f64vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_f64vector
{
  SCM uvec;
  float_f64 * p;
  float_f64 f;
  int count;

  SCM_VALIDATE_INUM (1, n);
  count = SCM_INUM (n);
  uvec = make_uvec (FUNC_NAME, SCM_UVEC_F64, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2dbl (fill, FUNC_NAME);
  p = (float_f64 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector, "f64vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_f64vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_f64vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector_length, "f64vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_f64vector_length
{
  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_F64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);
  return scm_int2num (SCM_UVEC_LENGTH (uvec));
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector_ref, "f64vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_f64vector_ref
{
  int idx;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_F64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  return scm_make_real (((float_f64 *) SCM_UVEC_BASE (uvec))[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector_set_x, "f64vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_f64vector_ref
{
  int idx;
  float_f64 f;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_F64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = scm_num2int (index, 2, FUNC_NAME);
  if (idx < 0 || idx >= SCM_UVEC_LENGTH (uvec))
    scm_out_of_range_pos (FUNC_NAME, index, SCM_MAKINUM (2));

  f = scm_num2dbl (value, FUNC_NAME);

  ((float_f64 *) SCM_UVEC_BASE (uvec))[idx] = f;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector_to_list, "f64vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_f64vector_to_list
{
  int idx;
  float_f64 * p;
  SCM res = SCM_EOL;

  SCM_VALIDATE_SMOB (1, uvec, uvec);
  if (SCM_UVEC_TYPE (uvec) != SCM_UVEC_F64)
    scm_wrong_type_arg (FUNC_NAME, 1, uvec);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (float_f64 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
    {
      p--;
      res = scm_cons (scm_make_real (*p), res);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_f64vector, "list->f64vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "8-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_f64vector
{
  SCM uvec;
  float_f64 * p;
  int n;
  int arg_pos = 1;

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (FUNC_NAME, SCM_UVEC_F64, n);
  p = (float_f64 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
    {
      float_f64 f = scm_num2dbl (SCM_CAR (l), FUNC_NAME);
      *p++ = f;
      l = SCM_CDR (l);
      arg_pos++;
    }
  return uvec;
}
#undef FUNC_NAME


/* Create the smob type for homogeneous numeric vectors and install
   the primitives.  */
void
scm_init_srfi_4 (void)
{
  scm_tc16_uvec = scm_make_smob_type ("uvec", 0);
  scm_set_smob_free (scm_tc16_uvec, uvec_free);
  scm_set_smob_print (scm_tc16_uvec, uvec_print);
#include "srfi/srfi-4.x"
}

/* End of srfi-4.c.  */
