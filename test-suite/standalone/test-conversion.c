/* Copyright (C) 1999,2000,2001,2003,2004 Free Software Foundation, Inc.
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

#include "libguile.h"

#include <stdio.h>
#include <assert.h>

static void
test_1 (const char *str, scm_t_intmax min, scm_t_intmax max,
	int result)
{
  int r = scm_is_signed_integer (scm_c_eval_string (str), min, max);
  if (r != result)
    {
      fprintf (stderr, "fail: scm_is_signed_integer (%s, %Ld, %Ld) == %d\n",
	       str, min, max, result);
      exit (1);
    }
}

static void
test_is_signed_integer ()
{
  test_1 ("'foo", 
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0);
  test_1 ("3.0", 
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0);
  test_1 ("(inexact->exact 3.0)", 
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  test_1 ("3.5",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0);
  test_1 ("most-positive-fixnum",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  test_1 ("(+ most-positive-fixnum 1)",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  test_1 ("most-negative-fixnum",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  test_1 ("(- most-negative-fixnum 1)",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  1);
  if (sizeof (scm_t_intmax) == 8)
    {
      test_1 ("(- (expt 2 63) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      1);
      test_1 ("(expt 2 63)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0);
      test_1 ("(- (expt 2 63))",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      1);
      test_1 ("(- (- (expt 2 63)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0);
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_1 ("(- (expt 2 31) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      1);
      test_1 ("(expt 2 31)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0);
      test_1 ("(- (expt 2 31))",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      1);
      test_1 ("(- (- (expt 2 31)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0);
    }
  else
    fprintf (stderr, "NOTE: skipped some tests.\n");

  /* bignum with range that fits into fixnum. */
  test_1 ("(+ most-positive-fixnum 1)",
	  -32768, 32767,
	  0);

  /* bignum with range that doesn't fit into fixnum, but probably
     fits into long. */
  test_1 ("(+ most-positive-fixnum 1)",
	  SCM_MOST_NEGATIVE_FIXNUM-1, SCM_MOST_POSITIVE_FIXNUM+1,
	  1);
}

static void
test_2 (const char *str, scm_t_uintmax min, scm_t_uintmax max,
	int result)
{
  int r = scm_is_unsigned_integer (scm_c_eval_string (str), min, max);
  if (r != result)
    {
      fprintf (stderr, "fail: scm_is_unsigned_integer (%s, %Lu, %Lu) == %d\n",
	       str, min, max, result);
      exit (1);
    }
}

static void
test_is_unsigned_integer ()
{
  test_2 ("'foo", 
	  0, SCM_T_UINTMAX_MAX,
	  0);
  test_2 ("3.0", 
	  0, SCM_T_UINTMAX_MAX,
	  0);
  test_2 ("(inexact->exact 3.0)", 
	  0, SCM_T_UINTMAX_MAX,
	  1);
  test_2 ("3.5",
	  0, SCM_T_UINTMAX_MAX,
	  0);
  test_2 ("most-positive-fixnum",
	  0, SCM_T_UINTMAX_MAX,
	  1);
  test_2 ("(+ most-positive-fixnum 1)",
	  0, SCM_T_UINTMAX_MAX,
	  1);
  test_2 ("most-negative-fixnum",
	  0, SCM_T_UINTMAX_MAX,
	  0);
  test_2 ("(- most-negative-fixnum 1)",
	  0, SCM_T_UINTMAX_MAX,
	  0);
  if (sizeof (scm_t_intmax) == 8)
    {
      test_2 ("(- (expt 2 64) 1)",
	      0, SCM_T_UINTMAX_MAX,
	      1);
      test_2 ("(expt 2 64)",
	      0, SCM_T_UINTMAX_MAX,
	      0);
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_2 ("(- (expt 2 32) 1)",
	      0, SCM_T_UINTMAX_MAX,
	      1);
      test_2 ("(expt 2 32)",
	      0, SCM_T_UINTMAX_MAX,
	      0);
    }
  else
    fprintf (stderr, "NOTE: skipped some tests.\n");

  /* bignum with range that fits into fixnum. */
  test_2 ("(+ most-positive-fixnum 1)",
	  0, 32767,
	  0);

  /* bignum with range that doesn't fit into fixnum, but probably
     fits into long. */
  test_2 ("(+ most-positive-fixnum 1)",
	  0, SCM_MOST_POSITIVE_FIXNUM+1,
	  1);
}

typedef struct {
  SCM val;
  scm_t_intmax min, max;
  scm_t_intmax result;
} to_signed_data;

static SCM
out_of_range_handler (void *data, SCM key, SCM args)
{
  return scm_equal_p (key, scm_str2symbol ("out-of-range"));
}

static SCM
wrong_type_handler (void *data, SCM key, SCM args)
{
  return scm_equal_p (key, scm_str2symbol ("wrong-type-arg"));
}

static SCM
any_handler (void *data, SCM key, SCM args)
{
  return SCM_BOOL_T;
}

static SCM
to_signed_integer_body (void *data)
{
  to_signed_data *d = (to_signed_data *)data;
  d->result = scm_to_signed_integer (d->val, d->min, d->max);
  return SCM_BOOL_F;
}

static void
test_3 (const char *str, scm_t_intmax min, scm_t_intmax max,
	scm_t_intmax result, int range_error, int type_error)
{
  to_signed_data data;
  data.val = scm_c_eval_string (str);
  data.min = min;
  data.max = max;
  
  if (range_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_signed_integer_body, &data,
					    out_of_range_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_signed_int (%s, %Ld, %Ld) -> out of range\n",
		   str, min, max);
	  exit (1);
	}
    }
  else if (type_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_signed_integer_body, &data,
					    wrong_type_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_signed_int (%s, %Ld, %Ld) -> wrong type\n",
		   str, min, max);
	  exit (1);
	}
    }
  else
    {
      if (scm_is_true (scm_internal_catch (SCM_BOOL_T,
					   to_signed_integer_body, &data,
					   any_handler, NULL))
	  || data.result != result)
	{
	  fprintf (stderr,
		   "fail: scm_to_signed_int (%s, %Ld, %Ld) = %Ld\n",
		   str, min, max, result);
	  exit (1);
	}
    }
}

static void
test_to_signed_integer ()
{
  test_3 ("'foo",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0, 0, 1);
  test_3 ("3.5",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  0, 0, 1);
  test_3 ("12",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  12, 0, 0);
  test_3 ("1000",
	  -999, 999,
	  0, 1, 0);
  test_3 ("-1000",
	  -999, 999,
	  0, 1, 0);
  test_3 ("most-positive-fixnum",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  SCM_MOST_POSITIVE_FIXNUM, 0, 0);
  test_3 ("most-negative-fixnum",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  SCM_MOST_NEGATIVE_FIXNUM, 0, 0);
  test_3 ("(+ most-positive-fixnum 1)",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  SCM_MOST_POSITIVE_FIXNUM+1, 0, 0);
  test_3 ("(- most-negative-fixnum 1)",
	  SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	  SCM_MOST_NEGATIVE_FIXNUM-1, 0, 0);
  if (sizeof (scm_t_intmax) == 8)
    {
      test_3 ("(- (expt 2 63) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MAX, 0, 0);
      test_3 ("(+ (- (expt 2 63)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MIN+1, 0, 0);
      test_3 ("(- (expt 2 63))",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MIN, 0, 0);
      test_3 ("(expt 2 63)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0, 1, 0);
      test_3 ("(- (- (expt 2 63)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0, 1, 0);
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_3 ("(- (expt 2 31) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MAX, 0, 0);
      test_3 ("(+ (- (expt 2 31)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MIN+1, 0, 0);
      test_3 ("(- (expt 2 31))",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      SCM_T_INTMAX_MIN, 0, 0);
      test_3 ("(expt 2 31)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0, 1, 0);
      test_3 ("(- (- (expt 2 31)) 1)",
	      SCM_T_INTMAX_MIN, SCM_T_INTMAX_MAX,
	      0, 1, 0);
    }
  else
    fprintf (stderr, "NOTE: skipped some tests.\n");
}

typedef struct {
  SCM val;
  scm_t_uintmax min, max;
  scm_t_uintmax result;
} to_unsigned_data;

static SCM
to_unsigned_integer_body (void *data)
{
  to_unsigned_data *d = (to_unsigned_data *)data;
  d->result = scm_to_unsigned_integer (d->val, d->min, d->max);
  return SCM_BOOL_F;
}

static void
test_4 (const char *str, scm_t_uintmax min, scm_t_uintmax max,
	scm_t_uintmax result, int range_error, int type_error)
{
  to_unsigned_data data;
  data.val = scm_c_eval_string (str);
  data.min = min;
  data.max = max;
  
  if (range_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_unsigned_integer_body, &data,
					    out_of_range_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_unsigned_int (%s, %Lu, %Lu) -> out of range\n",
		   str, min, max);
	  exit (1);
	}
    }
  else if (type_error)
    {
      if (scm_is_false (scm_internal_catch (SCM_BOOL_T,
					    to_unsigned_integer_body, &data,
					    wrong_type_handler, NULL)))
	{
	  fprintf (stderr,
		   "fail: scm_to_unsigned_int (%s, %Lu, %Lu) -> wrong type\n",
		   str, min, max);
	  exit (1);
	}
    }
  else
    {
      if (scm_is_true (scm_internal_catch (SCM_BOOL_T,
					   to_unsigned_integer_body, &data,
					   any_handler, NULL))
	  || data.result != result)
	{
	  fprintf (stderr,
		   "fail: scm_to_unsigned_int (%s, %Lu, %Lu) == %Lu\n",
		   str, min, max, result);
	  exit (1);
	}
    }
}

static void
test_to_unsigned_integer ()
{
  test_4 ("'foo",
	  0, SCM_T_UINTMAX_MAX,
	  0, 0, 1);
  test_4 ("3.5",
	  0, SCM_T_UINTMAX_MAX,
	  0, 0, 1);
  test_4 ("12",
	  0, SCM_T_UINTMAX_MAX,
	  12, 0, 0);
  test_4 ("1000",
	  0, 999,
	  0, 1, 0);
  test_4 ("most-positive-fixnum",
	  0, SCM_T_UINTMAX_MAX,
	  SCM_MOST_POSITIVE_FIXNUM, 0, 0);
  test_4 ("(+ most-positive-fixnum 1)",
	  0, SCM_T_UINTMAX_MAX,
	  SCM_MOST_POSITIVE_FIXNUM+1, 0, 0);
  if (sizeof (scm_t_intmax) == 8)
    {
      test_4 ("(- (expt 2 64) 1)",
	      0, SCM_T_UINTMAX_MAX,
	      SCM_T_UINTMAX_MAX, 0, 0);
      test_4 ("(expt 2 64)",
	      0, SCM_T_UINTMAX_MAX,
	      0, 1, 0);
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_4 ("(- (expt 2 32) 1)",
	      0, SCM_T_UINTMAX_MAX,
	      SCM_T_UINTMAX_MAX, 0, 0);
      test_4 ("(expt 2 32)",
	      0, SCM_T_UINTMAX_MAX,
	      0, 1, 0);
    }
  else
    fprintf (stderr, "NOTE: skipped some tests.\n");
}

static void
test_5 (scm_t_intmax val, const char *result)
{
  SCM res = scm_c_eval_string (result);
  if (scm_is_false (scm_equal_p (scm_from_signed_integer (val), res)))
    {
      fprintf (stderr, "fail: scm_from_signed_integer (%Ld) == %s\n",
	       val, result);
      exit (1);
    }
}

static void
test_from_signed_integer ()
{
  test_5 (12, "12");
  if (sizeof (scm_t_intmax) == 8)
    {
      test_5 (SCM_T_INTMAX_MAX, "(- (expt 2 63) 1)");
      test_5 (SCM_T_INTMAX_MIN, "(- (expt 2 63))");
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_5 (SCM_T_INTMAX_MAX, "(- (expt 2 31) 1)");
      test_5 (SCM_T_INTMAX_MIN, "(- (expt 2 31))");
    }
  test_5 (SCM_MOST_POSITIVE_FIXNUM, "most-positive-fixnum");
  test_5 (SCM_MOST_NEGATIVE_FIXNUM, "most-negative-fixnum");
  test_5 (SCM_MOST_POSITIVE_FIXNUM+1, "(+ most-positive-fixnum 1)");
  test_5 (SCM_MOST_NEGATIVE_FIXNUM-1, "(- most-negative-fixnum 1)");
}

static void
test_6 (scm_t_uintmax val, const char *result)
{
  SCM res = scm_c_eval_string (result);
  if (scm_is_false (scm_equal_p (scm_from_unsigned_integer (val), res)))
    {
      fprintf (stderr, "fail: scm_from_unsigned_integer (%Lu) == %s\n",
	       val, result);
      exit (1);
    }
}

static void
test_from_unsigned_integer ()
{
  test_6 (12, "12");
  if (sizeof (scm_t_intmax) == 8)
    {
      test_6 (SCM_T_UINTMAX_MAX, "(- (expt 2 64) 1)");
    }
  else if (sizeof (scm_t_intmax) == 4)
    {
      test_6 (SCM_T_UINTMAX_MAX, "(- (expt 2 32) 1)");
    }
  test_6 (SCM_MOST_POSITIVE_FIXNUM, "most-positive-fixnum");
  test_6 (SCM_MOST_POSITIVE_FIXNUM+1, "(+ most-positive-fixnum 1)");
}

static void
test_int_sizes ()
{
  SCM n = scm_from_int (91);

  /* Just list them here to check whether the macros expand to correct
     code. */
     
  scm_from_schar (91);
  scm_from_uchar (91);
  scm_from_char (91);
  scm_from_short (91);
  scm_from_int (91);
  scm_from_long (91);
#if SCM_SIZEOF_LONG_LONG != 0
  scm_from_long_long (91);
  scm_from_ulong_long (91);
#endif
  scm_from_size_t (91);
  scm_from_ssize_t (91);
  scm_from_int8 (91);
  scm_from_uint8 (91);
  scm_from_int16 (91);
  scm_from_uint16 (91);
  scm_from_int32 (91);
  scm_from_uint32 (91);
#if SCM_HAVE_T_INT64
  scm_from_int64 (91);
  scm_from_uint64 (91);
#endif

  scm_to_schar (n);
  scm_to_uchar (n);
  scm_to_char (n);
  scm_to_short (n);
  scm_to_int (n);
  scm_to_long (n);
#if SCM_SIZEOF_LONG_LONG != 0
  scm_to_long_long (n);
  scm_to_ulong_long (n);
#endif
  scm_to_size_t (n);
  scm_to_ssize_t (n);
  scm_to_int8 (n);
  scm_to_uint8 (n);
  scm_to_int16 (n);
  scm_to_uint16 (n);
  scm_to_int32 (n);
  scm_to_uint32 (n);
#if SCM_HAVE_T_INT64
  scm_to_int64 (n);
  scm_to_uint64 (n);
#endif

}

int
main (int argc, char *argv[])
{
  scm_init_guile();
  test_is_signed_integer ();
  test_is_unsigned_integer ();
  test_to_signed_integer ();
  test_to_unsigned_integer ();
  test_from_signed_integer ();
  test_from_unsigned_integer ();
  test_int_sizes ();
  return 0;
}
