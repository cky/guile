/* srfi-14.c --- SRFI-14 procedures for Guile
 *
 * Copyright (C) 2001, 2004, 2006, 2007, 2009, 2011 Free Software Foundation, Inc.
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
#  include <config.h>
#endif


#include <string.h>
#include <unictype.h>

#include "libguile.h"
#include "libguile/srfi-14.h"
#include "libguile/strings.h"
#include "libguile/chars.h"

/* Include the pre-computed standard charset data.  */
#include "libguile/srfi-14.i.c"

scm_t_char_range cs_full_ranges[] = {
  {0x0000, SCM_CODEPOINT_SURROGATE_START - 1}
  ,
  {SCM_CODEPOINT_SURROGATE_END + 1, SCM_CODEPOINT_MAX}
};

scm_t_char_set cs_full = {
  2,
  cs_full_ranges
};


#define SCM_CHARSET_DATA(charset) ((scm_t_char_set *) SCM_SMOB_DATA (charset))

#define SCM_CHARSET_SET(cs, idx)                        \
  scm_i_charset_set (SCM_CHARSET_DATA (cs), idx)

#define SCM_CHARSET_UNSET(cs, idx)                      \
  scm_i_charset_unset (SCM_CHARSET_DATA (cs), idx)

/* Smob type code for character sets.  */
int scm_tc16_charset = 0;
int scm_tc16_charset_cursor = 0;

/* True if N exists in charset CS.  */
int
scm_i_charset_get (scm_t_char_set *cs, scm_t_wchar n)
{
  size_t i;

  i = 0;
  while (i < cs->len)
    {
      if (cs->ranges[i].lo <= n && n <= cs->ranges[i].hi)
        return 1;
      i++;
    }

  return 0;
}

/* Put N into charset CS.  */
void
scm_i_charset_set (scm_t_char_set *cs, scm_t_wchar n)
{
  size_t i;
  size_t len;

  len = cs->len;

  i = 0;
  while (i < len)
    {
      /* Already in this range  */
      if (cs->ranges[i].lo <= n && n <= cs->ranges[i].hi)
        {
          return;
        }

      if (n == cs->ranges[i].lo - 1)
        {
          /* This char is one below the current range. */
          if (i > 0 && cs->ranges[i - 1].hi + 1 == n)
            {
              /* It is also one above the previous range.  */
              /* This is an impossible condition: in the previous
                 iteration, the test for 'one above the current range'
                 should already have inserted the character here.  */
              abort ();
            }
          else
            {
              /* Expand the range down by one.  */
              cs->ranges[i].lo = n;
              return;
            }
        }
      else if (n == cs->ranges[i].hi + 1)
        {
          /* This char is one above the current range.  */
          if (i < len - 1 && cs->ranges[i + 1].lo - 1 == n)
            {
              /* It is also one below the next range, so combine them.  */
              cs->ranges[i].hi = cs->ranges[i + 1].hi;
              if (i < len - 2)
                memmove (cs->ranges + (i + 1), cs->ranges + (i + 2),
                         sizeof (scm_t_char_range) * (len - i - 2));
              cs->ranges = scm_gc_realloc (cs->ranges,
                                           sizeof (scm_t_char_range) * len,
                                           sizeof (scm_t_char_range) * (len -
                                                                        1),
                                           "character-set");
              cs->len = len - 1;
              return;
            }
          else
            {
              /* Expand the range up by one.  */
              cs->ranges[i].hi = n;
              return;
            }
        }
      else if (n < cs->ranges[i].lo - 1)
        {
          /* This is a new range below the current one.  */
          cs->ranges = scm_gc_realloc (cs->ranges,
                                       sizeof (scm_t_char_range) * len,
                                       sizeof (scm_t_char_range) * (len + 1),
                                       "character-set");
          memmove (cs->ranges + (i + 1), cs->ranges + i,
                   sizeof (scm_t_char_range) * (len - i));
          cs->ranges[i].lo = n;
          cs->ranges[i].hi = n;
          cs->len = len + 1;
          return;
        }

      i++;
    }

  /* This is a new range above all previous ranges.  */
  if (len == 0)
    {
      cs->ranges = scm_gc_malloc (sizeof (scm_t_char_range), "character-set");
    }
  else
    {
      cs->ranges = scm_gc_realloc (cs->ranges,
                                   sizeof (scm_t_char_range) * len,
                                   sizeof (scm_t_char_range) * (len + 1),
                                   "character-set");
    }
  cs->ranges[len].lo = n;
  cs->ranges[len].hi = n;
  cs->len = len + 1;

  return;
}

/* Put LO to HI inclusive into charset CS.  */
static void
scm_i_charset_set_range (scm_t_char_set *cs, scm_t_wchar lo, scm_t_wchar hi)
{
  size_t i;

  i = 0;
  while (i < cs->len)
    {
      /* Already in this range  */
      if (cs->ranges[i].lo <= lo && cs->ranges[i].hi >= hi)
        return;

      /* cur:       +---+
         new: +---+
      */
      if (cs->ranges[i].lo - 1 > hi)
        {
          /* Add a new range below the current one.  */
          cs->ranges = scm_gc_realloc (cs->ranges,
                                       sizeof (scm_t_char_range) * cs->len,
                                       sizeof (scm_t_char_range) * (cs->len + 1),
                                       "character-set");
          memmove (cs->ranges + (i + 1), cs->ranges + i,
                   sizeof (scm_t_char_range) * (cs->len - i));
          cs->ranges[i].lo = lo;
          cs->ranges[i].hi = hi;
          cs->len += 1;
          return;
        }

      /* cur:      +---+  or     +---+  or    +---+
         new: +---+          +---+         +---+
      */
      if (cs->ranges[i].lo > lo
          && (cs->ranges[i].lo - 1 <= hi && cs->ranges[i].hi >= hi))
        {
          cs->ranges[i].lo = lo;
          return;
        }

      /* cur: +---+    or +---+     or +---+
         new:   +---+         +---+         +---+
      */
      else if (cs->ranges[i].hi + 1 >= lo && cs->ranges[i].hi < hi)
        {
          if (cs->ranges[i].lo > lo)
            cs->ranges[i].lo = lo;
          if (cs->ranges[i].hi < hi)
            cs->ranges[i].hi = hi;
          while (i < cs->len - 1)
            {
              /* cur: --+    +---+
                 new: -----+
              */
              if (cs->ranges[i + 1].lo - 1 > hi)
                break;
              
              /* cur: --+   +---+  or  --+  +---+  or --+ +--+
                 new: -----+           ------+        ---------+
              */
              /* Combine this range with the previous one.  */
              if (cs->ranges[i + 1].hi > hi)
                cs->ranges[i].hi = cs->ranges[i + 1].hi;
              if (i + 1 < cs->len)
                memmove (cs->ranges + i + 1, cs->ranges + i + 2,
                         sizeof (scm_t_char_range) * (cs->len - i - 2));
              cs->ranges = scm_gc_realloc (cs->ranges,
                                           sizeof (scm_t_char_range) * cs->len,
                                           sizeof (scm_t_char_range) * (cs->len - 1),
                                           "character-set");
              cs->len -= 1;
            }
          return;
        }
      i ++;
    }

  /* This is a new range above all previous ranges.  */
  if (cs->len == 0)
    {
      cs->ranges = scm_gc_malloc (sizeof (scm_t_char_range), "character-set");
    }
  else
    {
      cs->ranges = scm_gc_realloc (cs->ranges,
                                   sizeof (scm_t_char_range) * cs->len,
                                   sizeof (scm_t_char_range) * (cs->len + 1),
                                   "character-set");
    }
  cs->len += 1;
  cs->ranges[cs->len - 1].lo = lo;
  cs->ranges[cs->len - 1].hi = hi;

  return;
}

/* If N is in charset CS, remove it.  */
void
scm_i_charset_unset (scm_t_char_set *cs, scm_t_wchar n)
{
  size_t i;
  size_t len;

  len = cs->len;

  i = 0;
  while (i < len)
    {
      if (n < cs->ranges[i].lo)
        /* Not in this set.  */
        return;

      if (n == cs->ranges[i].lo && n == cs->ranges[i].hi)
        {
          /* Remove this one-character range.  */
          if (len == 1)
            {
              scm_gc_free (cs->ranges,
                           sizeof (scm_t_char_range) * cs->len,
                           "character-set");
              cs->ranges = NULL;
              cs->len = 0;
              return;
            }
          else if (i < len - 1)
            {
              memmove (cs->ranges + i, cs->ranges + (i + 1),
                       sizeof (scm_t_char_range) * (len - i - 1));
              cs->ranges = scm_gc_realloc (cs->ranges,
                                           sizeof (scm_t_char_range) * len,
                                           sizeof (scm_t_char_range) * (len -
                                                                        1),
                                           "character-set");
              cs->len = len - 1;
              return;
            }
          else if (i == len - 1)
            {
              cs->ranges = scm_gc_realloc (cs->ranges,
                                           sizeof (scm_t_char_range) * len,
                                           sizeof (scm_t_char_range) * (len -
                                                                        1),
                                           "character-set");
              cs->len = len - 1;
              return;
            }
        }
      else if (n == cs->ranges[i].lo)
        {
          /* Shrink this range from the left.  */
          cs->ranges[i].lo = n + 1;
          return;
        }
      else if (n == cs->ranges[i].hi)
        {
          /* Shrink this range from the right.  */
          cs->ranges[i].hi = n - 1;
          return;
        }
      else if (n > cs->ranges[i].lo && n < cs->ranges[i].hi)
        {
          /* Split this range into two pieces.  */
          cs->ranges = scm_gc_realloc (cs->ranges,
                                       sizeof (scm_t_char_range) * len,
                                       sizeof (scm_t_char_range) * (len + 1),
                                       "character-set");
          if (i < len - 1)
            memmove (cs->ranges + (i + 2), cs->ranges + (i + 1),
                     sizeof (scm_t_char_range) * (len - i - 1));
          cs->ranges[i + 1].hi = cs->ranges[i].hi;
          cs->ranges[i + 1].lo = n + 1;
          cs->ranges[i].hi = n - 1;
          cs->len = len + 1;
          return;
        }

      i++;
    }

  /* This value is above all ranges, so do nothing here.  */
  return;
}

static int
charsets_equal (scm_t_char_set *a, scm_t_char_set *b)
{
  if (a->len != b->len)
    return 0;

  if (memcmp (a->ranges, b->ranges, sizeof (scm_t_char_range) * a->len) != 0)
    return 0;

  return 1;
}

/* Return true if every character in A is also in B.  */
static int
charsets_leq (scm_t_char_set *a, scm_t_char_set *b)
{
  size_t i = 0, j = 0;
  scm_t_wchar alo, ahi;

  if (a->len == 0)
    return 1;
  if (b->len == 0)
    return 0;
  while (i < a->len)
    {
      alo = a->ranges[i].lo;
      ahi = a->ranges[i].hi;
      while (b->ranges[j].hi < alo)
        {
          if (j < b->len - 1)
            j++;
          else
            return 0;
        }
      if (alo < b->ranges[j].lo || ahi > b->ranges[j].hi)
        return 0;
      i++;
    }

  return 1;
}

/* Merge B into A. */
static void
charsets_union (scm_t_char_set *a, scm_t_char_set *b)
{
  size_t i = 0;
  scm_t_wchar blo, bhi;

  if (b->len == 0)
    return;

  if (a->len == 0)
    {
      a->len = b->len;
      a->ranges = scm_gc_malloc (sizeof (scm_t_char_range) * b->len,
                                 "character-set");
      memcpy (a->ranges, b->ranges, sizeof (scm_t_char_range) * b->len);
      return;
    }

  while (i < b->len)
    {
      blo = b->ranges[i].lo;
      bhi = b->ranges[i].hi;
      scm_i_charset_set_range (a, blo, bhi);

      i++;
    }

  return;
}

/* Remove elements not both in A and B from A. */
static void
charsets_intersection (scm_t_char_set *a, scm_t_char_set *b)
{
  size_t i = 0;
  scm_t_wchar blo, bhi, n;
  scm_t_char_set *c;

  if (a->len == 0)
    return;

  if (b->len == 0)
    {
      scm_gc_free (a->ranges, sizeof (scm_t_char_range) * a->len,
                   "character-set");
      a->len = 0;
      return;
    }

  c = (scm_t_char_set *) scm_malloc (sizeof (scm_t_char_set));
  c->len = 0;
  c->ranges = NULL;

  while (i < b->len)
    {
      blo = b->ranges[i].lo;
      bhi = b->ranges[i].hi;
      for (n = blo; n <= bhi; n++)
        if (scm_i_charset_get (a, n))
          scm_i_charset_set (c, n);
      i++;
    }
  scm_gc_free (a->ranges, sizeof (scm_t_char_range) * a->len,
               "character-set");

  a->len = c->len;
  if (c->len != 0)
    a->ranges = c->ranges;
  else
    a->ranges = NULL;
  free (c);
  return;
}

#define SCM_ADD_RANGE(low, high)                        \
  do {                                                  \
    p->ranges[idx].lo = (low);                          \
    p->ranges[idx++].hi = (high);                       \
  } while (0)
#define SCM_ADD_RANGE_SKIP_SURROGATES(low, high)                  \
  do {                                                            \
    p->ranges[idx].lo = (low);                                    \
    p->ranges[idx++].hi = SCM_CODEPOINT_SURROGATE_START - 1;      \
    p->ranges[idx].lo = SCM_CODEPOINT_SURROGATE_END + 1;          \
    p->ranges[idx++].hi = (high);                                 \
  } while (0)



/* Make P the compelement of Q.  */
static void
charsets_complement (scm_t_char_set *p, scm_t_char_set *q)
{
  int k, idx;

  idx = 0;
  if (q->len == 0)
    {
      /* Fill with all valid codepoints.  */
      p->len = 2;
      p->ranges = scm_gc_malloc (sizeof (scm_t_char_range) * 2,
                                 "character-set");
      SCM_ADD_RANGE_SKIP_SURROGATES (0, SCM_CODEPOINT_MAX);
      return;
    }

  if (p->len > 0)
    scm_gc_free (p->ranges, sizeof (scm_t_char_set) * p->len,
                 "character-set");

  /* Count the number of ranges needed for the output.  */
  p->len = 0;
  if (q->ranges[0].lo > 0)
    p->len++;
  if (q->ranges[q->len - 1].hi < SCM_CODEPOINT_MAX)
    p->len++;
  p->len += q->len;
  p->ranges =
    (scm_t_char_range *) scm_gc_malloc (sizeof (scm_t_char_range) * p->len,
                                        "character-set");
  if (q->ranges[0].lo > 0)
    {
      if (q->ranges[0].lo > SCM_CODEPOINT_SURROGATE_END)
        SCM_ADD_RANGE_SKIP_SURROGATES (0, q->ranges[0].lo - 1);
      else
        SCM_ADD_RANGE (0, q->ranges[0].lo - 1);
    }
  for (k = 1; k < q->len; k++)
    {
      if (q->ranges[k - 1].hi < SCM_CODEPOINT_SURROGATE_START
          && q->ranges[k].lo - 1 > SCM_CODEPOINT_SURROGATE_END)
        SCM_ADD_RANGE_SKIP_SURROGATES (q->ranges[k - 1].hi + 1, q->ranges[k].lo - 1);
      else
        SCM_ADD_RANGE (q->ranges[k - 1].hi + 1, q->ranges[k].lo - 1);
    }
  if (q->ranges[q->len - 1].hi < SCM_CODEPOINT_MAX)
    {
      if (q->ranges[q->len - 1].hi < SCM_CODEPOINT_SURROGATE_START)
        SCM_ADD_RANGE_SKIP_SURROGATES (q->ranges[q->len - 1].hi + 1, SCM_CODEPOINT_MAX);
      else
        SCM_ADD_RANGE (q->ranges[q->len - 1].hi + 1, SCM_CODEPOINT_MAX);
    }
  return;
}
#undef SCM_ADD_RANGE
#undef SCM_ADD_RANGE_SKIP_SURROGATES

/* Replace A with elements only found in one of A or B.  */
static void
charsets_xor (scm_t_char_set *a, scm_t_char_set *b)
{
  size_t i = 0;
  scm_t_wchar blo, bhi, n;

  if (b->len == 0)
    {
      return;
    }

  if (a->len == 0)
    {
      a->ranges =
        (scm_t_char_range *) scm_gc_malloc (sizeof (scm_t_char_range) *
                                            b->len, "character-set");
      a->len = b->len;
      memcpy (a->ranges, b->ranges, sizeof (scm_t_char_range) * a->len);
      return;
    }

  while (i < b->len)
    {
      blo = b->ranges[i].lo;
      bhi = b->ranges[i].hi;
      for (n = blo; n <= bhi; n++)
        {
          if (scm_i_charset_get (a, n))
            scm_i_charset_unset (a, n);
          else
            scm_i_charset_set (a, n);
        }

      i++;
    }
  return;
}

/* Smob print hook for character sets.  */
static int
charset_print (SCM charset, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  size_t i;
  int first = 1;
  scm_t_char_set *p;
  const size_t max_ranges_to_print = 50;

  p = SCM_CHARSET_DATA (charset);

  scm_puts ("#<charset {", port);
  for (i = 0; i < p->len; i++)
    {
      if (first)
        first = 0;
      else
        scm_puts (" ", port);
      scm_write (SCM_MAKE_CHAR (p->ranges[i].lo), port);
      if (p->ranges[i].lo != p->ranges[i].hi)
        {
          scm_puts ("..", port);
          scm_write (SCM_MAKE_CHAR (p->ranges[i].hi), port);
        }
      if (i >= max_ranges_to_print)
        {
          /* Too many to print here.  Quit early.  */
          scm_puts (" ...", port);
          break;
        }
    }
  scm_puts ("}>", port);
  return 1;
}

/* Smob print hook for character sets cursors.  */
static int
charset_cursor_print (SCM cursor, SCM port,
                      scm_print_state *pstate SCM_UNUSED)
{
  scm_t_char_set_cursor *cur;

  cur = (scm_t_char_set_cursor *) SCM_SMOB_DATA (cursor);

  scm_puts ("#<charset-cursor ", port);
  if (cur->range == (size_t) (-1))
    scm_puts ("(empty)", port);
  else
    {
      scm_write (scm_from_size_t (cur->range), port);
      scm_puts (":", port);
      scm_write (scm_from_int32 (cur->n), port);
    }
  scm_puts (">", port);
  return 1;
}


/* Create a new, empty character set.  */
static SCM
make_char_set (const char *func_name)
{
  scm_t_char_set *p;

  p = scm_gc_malloc (sizeof (scm_t_char_set), "character-set");
  memset (p, 0, sizeof (scm_t_char_set));
  SCM_RETURN_NEWSMOB (scm_tc16_charset, p);
}


SCM_DEFINE (scm_char_set_p, "char-set?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a character set, @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_char_set_p
{
  return scm_from_bool (SCM_SMOB_PREDICATE (scm_tc16_charset, obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_eq, "char-set=", 0, 0, 1,
	    (SCM char_sets),
	    "Return @code{#t} if all given character sets are equal.")
#define FUNC_NAME s_scm_char_set_eq
{
  int argnum = 1;
  scm_t_char_set *cs1_data = NULL;

  SCM_VALIDATE_REST_ARGUMENT (char_sets);

  while (!scm_is_null (char_sets))
    {
      SCM csi = SCM_CAR (char_sets);
      scm_t_char_set *csi_data;

      SCM_VALIDATE_SMOB (argnum, csi, charset);
      argnum++;
      csi_data = SCM_CHARSET_DATA (csi);
      if (cs1_data == NULL)
        cs1_data = csi_data;
      else if (!charsets_equal (cs1_data, csi_data))
        return SCM_BOOL_F;
      char_sets = SCM_CDR (char_sets);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_leq, "char-set<=", 0, 0, 1,
	    (SCM char_sets),
	    "Return @code{#t} if every character set @var{cs}i is a subset\n"
	    "of character set @var{cs}i+1.")
#define FUNC_NAME s_scm_char_set_leq
{
  int argnum = 1;
  scm_t_char_set *prev_data = NULL;

  SCM_VALIDATE_REST_ARGUMENT (char_sets);

  while (!scm_is_null (char_sets))
    {
      SCM csi = SCM_CAR (char_sets);
      scm_t_char_set *csi_data;

      SCM_VALIDATE_SMOB (argnum, csi, charset);
      argnum++;
      csi_data = SCM_CHARSET_DATA (csi);
      if (prev_data)
        {
          if (!charsets_leq (prev_data, csi_data))
            return SCM_BOOL_F;
        }
      prev_data = csi_data;
      char_sets = SCM_CDR (char_sets);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_hash, "char-set-hash", 1, 1, 0,
	    (SCM cs, SCM bound),
	    "Compute a hash value for the character set @var{cs}.  If\n"
	    "@var{bound} is given and non-zero, it restricts the\n"
	    "returned value to the range 0 @dots{} @var{bound - 1}.")
#define FUNC_NAME s_scm_char_set_hash
{
  const unsigned long default_bnd = 871;
  unsigned long bnd;
  scm_t_char_set *p;
  unsigned long val = 0;
  int k;
  scm_t_wchar c;

  SCM_VALIDATE_SMOB (1, cs, charset);

  if (SCM_UNBNDP (bound))
    bnd = default_bnd;
  else
    {
      bnd = scm_to_ulong (bound);
      if (bnd == 0)
        bnd = default_bnd;
    }

  p = SCM_CHARSET_DATA (cs);
  for (k = 0; k < p->len; k++)
    {
      for (c = p->ranges[k].lo; c <= p->ranges[k].hi; c++)
        val = c + (val << 1);
    }
  return scm_from_ulong (val % bnd);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_cursor, "char-set-cursor", 1, 0, 0,
            (SCM cs), "Return a cursor into the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_cursor
{
  scm_t_char_set *cs_data;
  scm_t_char_set_cursor *cur_data;

  SCM_VALIDATE_SMOB (1, cs, charset);
  cs_data = SCM_CHARSET_DATA (cs);
  cur_data =
    (scm_t_char_set_cursor *) scm_gc_malloc (sizeof (scm_t_char_set_cursor),
                                             "charset-cursor");
  if (cs_data->len == 0)
    {
      cur_data->range = (size_t) (-1);
      cur_data->n = 0;
    }
  else
    {
      cur_data->range = 0;
      cur_data->n = cs_data->ranges[0].lo;
    }
  SCM_RETURN_NEWSMOB (scm_tc16_charset_cursor, cur_data);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_ref, "char-set-ref", 2, 0, 0,
            (SCM cs, SCM cursor),
            "Return the character at the current cursor position\n"
            "@var{cursor} in the character set @var{cs}.  It is an error to\n"
            "pass a cursor for which @code{end-of-char-set?} returns true.")
#define FUNC_NAME s_scm_char_set_ref
{
  scm_t_char_set *cs_data;
  scm_t_char_set_cursor *cur_data;
  size_t i;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_SMOB (2, cursor, charset_cursor);

  cs_data = SCM_CHARSET_DATA (cs);
  cur_data = (scm_t_char_set_cursor *) SCM_SMOB_DATA (cursor);

  /* Validate that this cursor is still true.  */
  i = cur_data->range;
  if (i == (size_t) (-1)
      || i >= cs_data->len
      || cur_data->n < cs_data->ranges[i].lo
      || cur_data->n > cs_data->ranges[i].hi)
    SCM_MISC_ERROR ("invalid character set cursor: ~A", scm_list_1 (cursor));
  return SCM_MAKE_CHAR (cur_data->n);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_cursor_next, "char-set-cursor-next", 2, 0, 0,
            (SCM cs, SCM cursor),
            "Advance the character set cursor @var{cursor} to the next\n"
            "character in the character set @var{cs}.  It is an error if the\n"
            "cursor given satisfies @code{end-of-char-set?}.")
#define FUNC_NAME s_scm_char_set_cursor_next
{
  scm_t_char_set *cs_data;
  scm_t_char_set_cursor *cur_data;
  size_t i;

  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_SMOB (2, cursor, charset_cursor);

  cs_data = SCM_CHARSET_DATA (cs);
  cur_data = (scm_t_char_set_cursor *) SCM_SMOB_DATA (cursor);

  /* Validate that this cursor is still true.  */
  i = cur_data->range;
  if (i == (size_t) (-1)
      || i >= cs_data->len
      || cur_data->n < cs_data->ranges[i].lo
      || cur_data->n > cs_data->ranges[i].hi)
    SCM_MISC_ERROR ("invalid character set cursor: ~A", scm_list_1 (cursor));
  /* Increment the cursor.  */
  if (cur_data->n == cs_data->ranges[i].hi)
    {
      if (i + 1 < cs_data->len)
        {
          cur_data->range = i + 1;
          cur_data->n = cs_data->ranges[i + 1].lo;
        }
      else
        {
          /* This is the end of the road.  */
          cur_data->range = (size_t) (-1);
          cur_data->n = 0;
        }
    }
  else
    {
      cur_data->n = cur_data->n + 1;
    }

  return cursor;
}
#undef FUNC_NAME


SCM_DEFINE (scm_end_of_char_set_p, "end-of-char-set?", 1, 0, 0,
            (SCM cursor),
            "Return @code{#t} if @var{cursor} has reached the end of a\n"
            "character set, @code{#f} otherwise.")
#define FUNC_NAME s_scm_end_of_char_set_p
{
  scm_t_char_set_cursor *cur_data;
  SCM_VALIDATE_SMOB (1, cursor, charset_cursor);

  cur_data = (scm_t_char_set_cursor *) SCM_SMOB_DATA (cursor);
  if (cur_data->range == (size_t) (-1))
    return SCM_BOOL_T;

  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_fold, "char-set-fold", 3, 0, 0,
            (SCM kons, SCM knil, SCM cs),
            "Fold the procedure @var{kons} over the character set @var{cs},\n"
            "initializing it with @var{knil}.")
#define FUNC_NAME s_scm_char_set_fold
{
  scm_t_char_set *cs_data;
  int k;
  scm_t_wchar n;

  SCM_VALIDATE_PROC (1, kons);
  SCM_VALIDATE_SMOB (3, cs, charset);

  cs_data = SCM_CHARSET_DATA (cs);

  if (cs_data->len == 0)
    return knil;

  for (k = 0; k < cs_data->len; k++)
    for (n = cs_data->ranges[k].lo; n <= cs_data->ranges[k].hi; n++)
      {
        knil = scm_call_2 (kons, SCM_MAKE_CHAR (n), knil);
      }
  return knil;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_unfold, "char-set-unfold", 4, 1, 0,
	    (SCM p, SCM f, SCM g, SCM seed, SCM base_cs),
	    "This is a fundamental constructor for character sets.\n"
	    "@itemize @bullet\n"
	    "@item @var{g} is used to generate a series of ``seed'' values\n"
	    "from the initial seed: @var{seed}, (@var{g} @var{seed}),\n"
	    "(@var{g}^2 @var{seed}), (@var{g}^3 @var{seed}), @dots{}\n"
	    "@item @var{p} tells us when to stop -- when it returns true\n"
	    "when applied to one of the seed values.\n"
	    "@item @var{f} maps each seed value to a character. These\n"
	    "characters are added to the base character set @var{base_cs} to\n"
	    "form the result; @var{base_cs} defaults to the empty set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_char_set_unfold
{
  SCM result, tmp;

  SCM_VALIDATE_PROC (1, p);
  SCM_VALIDATE_PROC (2, f);
  SCM_VALIDATE_PROC (3, g);
  if (!SCM_UNBNDP (base_cs))
    {
      SCM_VALIDATE_SMOB (5, base_cs, charset);
      result = scm_char_set_copy (base_cs);
    }
  else
    result = make_char_set (FUNC_NAME);

  tmp = scm_call_1 (p, seed);
  while (scm_is_false (tmp))
    {
      SCM ch = scm_call_1 (f, seed);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (f));
      SCM_CHARSET_SET (result, SCM_CHAR (ch));

      seed = scm_call_1 (g, seed);
      tmp = scm_call_1 (p, seed);
    }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_unfold_x, "char-set-unfold!", 5, 0, 0,
	    (SCM p, SCM f, SCM g, SCM seed, SCM base_cs),
	    "This is a fundamental constructor for character sets.\n"
	    "@itemize @bullet\n"
	    "@item @var{g} is used to generate a series of ``seed'' values\n"
	    "from the initial seed: @var{seed}, (@var{g} @var{seed}),\n"
	    "(@var{g}^2 @var{seed}), (@var{g}^3 @var{seed}), @dots{}\n"
	    "@item @var{p} tells us when to stop -- when it returns true\n"
	    "when applied to one of the seed values.\n"
	    "@item @var{f} maps each seed value to a character. These\n"
	    "characters are added to the base character set @var{base_cs} to\n"
	    "form the result; @var{base_cs} defaults to the empty set.\n"
	    "@end itemize")
#define FUNC_NAME s_scm_char_set_unfold_x
{
  SCM tmp;

  SCM_VALIDATE_PROC (1, p);
  SCM_VALIDATE_PROC (2, f);
  SCM_VALIDATE_PROC (3, g);
  SCM_VALIDATE_SMOB (5, base_cs, charset);

  tmp = scm_call_1 (p, seed);
  while (scm_is_false (tmp))
    {
      SCM ch = scm_call_1 (f, seed);
      if (!SCM_CHARP (ch))
	SCM_MISC_ERROR ("procedure ~S returned non-char", scm_list_1 (f));
      SCM_CHARSET_SET (base_cs, SCM_CHAR (ch));

      seed = scm_call_1 (g, seed);
      tmp = scm_call_1 (p, seed);
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_for_each, "char-set-for-each", 2, 0, 0,
            (SCM proc, SCM cs),
            "Apply @var{proc} to every character in the character set\n"
            "@var{cs}.  The return value is not specified.")
#define FUNC_NAME s_scm_char_set_for_each
{
  scm_t_char_set *cs_data;
  int k;
  scm_t_wchar n;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_SMOB (2, cs, charset);

  cs_data = SCM_CHARSET_DATA (cs);

  if (cs_data->len == 0)
    return SCM_UNSPECIFIED;

  for (k = 0; k < cs_data->len; k++)
    for (n = cs_data->ranges[k].lo; n <= cs_data->ranges[k].hi; n++)
      {
        scm_call_1 (proc, SCM_MAKE_CHAR (n));
      }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_map, "char-set-map", 2, 0, 0,
	    (SCM proc, SCM cs),
	    "Map the procedure @var{proc} over every character in @var{cs}.\n"
	    "@var{proc} must be a character -> character procedure.")
#define FUNC_NAME s_scm_char_set_map
{
  SCM result;
  int k;
  scm_t_char_set *cs_data;
  scm_t_wchar n;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_SMOB (2, cs, charset);

  result = make_char_set (FUNC_NAME);
  cs_data = SCM_CHARSET_DATA (cs);

  if (cs_data->len == 0)
    return result;

  for (k = 0; k < cs_data->len; k++)
    for (n = cs_data->ranges[k].lo; n <= cs_data->ranges[k].hi; n++)
      {
        SCM ch = scm_call_1 (proc, SCM_MAKE_CHAR (n));
        if (!SCM_CHARP (ch))
          SCM_MISC_ERROR ("procedure ~S returned non-char",
                          scm_list_1 (proc));
        SCM_CHARSET_SET (result, SCM_CHAR (ch));
      }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_copy, "char-set-copy", 1, 0, 0,
	    (SCM cs),
	    "Return a newly allocated character set containing all\n"
	    "characters in @var{cs}.")
#define FUNC_NAME s_scm_char_set_copy
{
  SCM ret;
  scm_t_char_set *p1, *p2;

  SCM_VALIDATE_SMOB (1, cs, charset);
  ret = make_char_set (FUNC_NAME);
  p1 = SCM_CHARSET_DATA (cs);
  p2 = SCM_CHARSET_DATA (ret);
  p2->len = p1->len;

  if (p1->len == 0)
    p2->ranges = NULL;
  else
    {
      p2->ranges = scm_gc_malloc (sizeof (scm_t_char_range) * p1->len,
                                  "character-set");
      memcpy (p2->ranges, p1->ranges, sizeof (scm_t_char_range) * p1->len);
    }

  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set, "char-set", 0, 0, 1,
	    (SCM rest),
	    "Return a character set containing all given characters.")
#define FUNC_NAME s_scm_char_set
{
  SCM cs;
  int argnum = 1;

  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = make_char_set (FUNC_NAME);
  while (!scm_is_null (rest))
    {
      scm_t_wchar c;

      SCM_VALIDATE_CHAR_COPY (argnum, SCM_CAR (rest), c);
      argnum++;
      rest = SCM_CDR (rest);
      SCM_CHARSET_SET (cs, c);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_char_set, "list->char-set", 1, 1, 0,
	    (SCM list, SCM base_cs),
	    "Convert the character list @var{list} to a character set.  If\n"
	    "the character set @var{base_cs} is given, the character in this\n"
	    "set are also included in the result.")
#define FUNC_NAME s_scm_list_to_char_set
{
  SCM cs;

  SCM_VALIDATE_LIST (1, list);
  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (2, base_cs, charset);
      cs = scm_char_set_copy (base_cs);
    }
  while (!scm_is_null (list))
    {
      SCM chr = SCM_CAR (list);
      scm_t_wchar c;

      SCM_VALIDATE_CHAR_COPY (0, chr, c);
      list = SCM_CDR (list);


      SCM_CHARSET_SET (cs, c);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_char_set_x, "list->char-set!", 2, 0, 0,
            (SCM list, SCM base_cs),
            "Convert the character list @var{list} to a character set.  The\n"
            "characters are added to @var{base_cs} and @var{base_cs} is\n"
            "returned.")
#define FUNC_NAME s_scm_list_to_char_set_x
{
  SCM_VALIDATE_LIST (1, list);
  SCM_VALIDATE_SMOB (2, base_cs, charset);
  while (!scm_is_null (list))
    {
      SCM chr = SCM_CAR (list);
      scm_t_wchar c;

      SCM_VALIDATE_CHAR_COPY (0, chr, c);
      list = SCM_CDR (list);

      SCM_CHARSET_SET (base_cs, c);
    }
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_char_set, "string->char-set", 1, 1, 0,
	    (SCM str, SCM base_cs),
	    "Convert the string @var{str} to a character set.  If the\n"
	    "character set @var{base_cs} is given, the characters in this\n"
	    "set are also included in the result.")
#define FUNC_NAME s_scm_string_to_char_set
{
  SCM cs;
  size_t k = 0, len;

  SCM_VALIDATE_STRING (1, str);
  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (2, base_cs, charset);
      cs = scm_char_set_copy (base_cs);
    }
  len = scm_i_string_length (str);
  while (k < len)
    {
      scm_t_wchar c = scm_i_string_ref (str, k++);
      SCM_CHARSET_SET (cs, c);
    }
  scm_remember_upto_here_1 (str);
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_to_char_set_x, "string->char-set!", 2, 0, 0,
            (SCM str, SCM base_cs),
            "Convert the string @var{str} to a character set.  The\n"
            "characters from the string are added to @var{base_cs}, and\n"
            "@var{base_cs} is returned.")
#define FUNC_NAME s_scm_string_to_char_set_x
{
  size_t k = 0, len;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_SMOB (2, base_cs, charset);
  len = scm_i_string_length (str);
  while (k < len)
    {
      scm_t_wchar c = scm_i_string_ref (str, k++);
      SCM_CHARSET_SET (base_cs, c);
    }
  scm_remember_upto_here_1 (str);
  return base_cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_filter, "char-set-filter", 2, 1, 0,
	    (SCM pred, SCM cs, SCM base_cs),
	    "Return a character set containing every character from @var{cs}\n"
	    "so that it satisfies @var{pred}.  If provided, the characters\n"
	    "from @var{base_cs} are added to the result.")
#define FUNC_NAME s_scm_char_set_filter
{
  SCM ret;
  int k;
  scm_t_wchar n;
  scm_t_char_set *p;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);
  if (!SCM_UNBNDP (base_cs))
    {
      SCM_VALIDATE_SMOB (3, base_cs, charset);
      ret = scm_char_set_copy (base_cs);
    }
  else
    ret = make_char_set (FUNC_NAME);

  p = SCM_CHARSET_DATA (cs);

  if (p->len == 0)
    return ret;

  for (k = 0; k < p->len; k++)
    for (n = p->ranges[k].lo; n <= p->ranges[k].hi; n++)
      {
        SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (n));

        if (scm_is_true (res))
          SCM_CHARSET_SET (ret, n);
      }
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_filter_x, "char-set-filter!", 3, 0, 0,
	    (SCM pred, SCM cs, SCM base_cs),
	    "Return a character set containing every character from @var{cs}\n"
	    "so that it satisfies @var{pred}.  The characters are added to\n"
	    "@var{base_cs} and @var{base_cs} is returned.")
#define FUNC_NAME s_scm_char_set_filter_x
{
  int k;
  scm_t_wchar n;
  scm_t_char_set *p;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);
  SCM_VALIDATE_SMOB (3, base_cs, charset);
  p = SCM_CHARSET_DATA (cs);
  if (p->len == 0)
    return base_cs;

  for (k = 0; k < p->len; k++)
    for (n = p->ranges[k].lo; n <= p->ranges[k].hi; n++)
      {
        SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (n));

        if (scm_is_true (res))
          SCM_CHARSET_SET (base_cs, n);
      }
  return base_cs;
}
#undef FUNC_NAME


/* Return a character set containing all the characters from [LOWER,UPPER),
   giving range errors if ERROR, adding chars from BASE_CS, and recycling
   BASE_CS if REUSE is true.  */
static SCM
scm_i_ucs_range_to_char_set (const char *FUNC_NAME, SCM lower, SCM upper, 
                             SCM error, SCM base_cs, int reuse)
{
  SCM cs;
  size_t clower, cupper;

  clower = scm_to_size_t (lower);
  cupper = scm_to_size_t (upper) - 1;
  SCM_ASSERT_RANGE (2, upper, cupper >= clower);
  if (!SCM_UNBNDP (error))
    {
      if (scm_is_true (error))
        {
          SCM_ASSERT_RANGE (1, lower, SCM_IS_UNICODE_CHAR (clower));
          SCM_ASSERT_RANGE (2, upper, SCM_IS_UNICODE_CHAR (cupper));
          if (clower < SCM_CODEPOINT_SURROGATE_START 
              && cupper > SCM_CODEPOINT_SURROGATE_END)
            scm_error(scm_out_of_range_key,
                      FUNC_NAME, "invalid range - contains surrogate characters: ~S to ~S",
                      scm_list_2 (lower, upper), scm_list_1 (upper));
        }
    }

  if (SCM_UNBNDP (base_cs))
    cs = make_char_set (FUNC_NAME);
  else
    {
      SCM_VALIDATE_SMOB (3, base_cs, charset);
      if (reuse)
        cs = base_cs;
      else
        cs = scm_char_set_copy (base_cs);
    }

  if ((clower >= SCM_CODEPOINT_SURROGATE_START && clower <= SCM_CODEPOINT_SURROGATE_END)
      && (cupper >= SCM_CODEPOINT_SURROGATE_START && cupper <= SCM_CODEPOINT_SURROGATE_END))
    return cs;

  if (clower > SCM_CODEPOINT_MAX)
    clower = SCM_CODEPOINT_MAX;
  if (clower >= SCM_CODEPOINT_SURROGATE_START  && clower <= SCM_CODEPOINT_SURROGATE_END)
    clower = SCM_CODEPOINT_SURROGATE_END + 1;
  if (cupper > SCM_CODEPOINT_MAX)
    cupper = SCM_CODEPOINT_MAX;
  if (cupper >= SCM_CODEPOINT_SURROGATE_START && cupper <= SCM_CODEPOINT_SURROGATE_END)
    cupper = SCM_CODEPOINT_SURROGATE_START - 1;
  if (clower < SCM_CODEPOINT_SURROGATE_START && cupper > SCM_CODEPOINT_SURROGATE_END)
    {
      scm_i_charset_set_range (SCM_CHARSET_DATA (cs), clower, SCM_CODEPOINT_SURROGATE_START - 1);
      scm_i_charset_set_range (SCM_CHARSET_DATA (cs), SCM_CODEPOINT_SURROGATE_END + 1, cupper);
    }
  else
    scm_i_charset_set_range (SCM_CHARSET_DATA (cs), clower, cupper);
  return cs;
}

SCM_DEFINE (scm_ucs_range_to_char_set, "ucs-range->char-set", 2, 2, 0,
	    (SCM lower, SCM upper, SCM error, SCM base_cs),
	    "Return a character set containing all characters whose\n"
	    "character codes lie in the half-open range\n"
	    "[@var{lower},@var{upper}).\n"
	    "\n"
	    "If @var{error} is a true value, an error is signalled if the\n"
	    "specified range contains characters which are not valid\n"
	    "Unicode code points.  If @var{error} is @code{#f},\n"
	    "these characters are silently left out of the resulting\n"
	    "character set.\n"
	    "\n"
	    "The characters in @var{base_cs} are added to the result, if\n"
	    "given.")
#define FUNC_NAME s_scm_ucs_range_to_char_set
{
  return scm_i_ucs_range_to_char_set (FUNC_NAME, lower, upper, 
                                      error, base_cs, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_ucs_range_to_char_set_x, "ucs-range->char-set!", 4, 0, 0,
	    (SCM lower, SCM upper, SCM error, SCM base_cs),
	    "Return a character set containing all characters whose\n"
	    "character codes lie in the half-open range\n"
	    "[@var{lower},@var{upper}).\n"
	    "\n"
	    "If @var{error} is a true value, an error is signalled if the\n"
	    "specified range contains characters which are not contained in\n"
	    "the implemented character range.  If @var{error} is @code{#f},\n"
	    "these characters are silently left out of the resulting\n"
	    "character set.\n"
	    "\n"
	    "The characters are added to @var{base_cs} and @var{base_cs} is\n"
	    "returned.")
#define FUNC_NAME s_scm_ucs_range_to_char_set_x
{
  SCM_VALIDATE_SMOB (4, base_cs, charset);  
  return scm_i_ucs_range_to_char_set (FUNC_NAME, lower, upper, 
                                      error, base_cs, 1);
}
#undef FUNC_NAME

SCM_DEFINE (scm_to_char_set, "->char-set", 1, 0, 0,
	    (SCM x),
	    "Coerces x into a char-set. @var{x} may be a string, character or char-set. A string is converted to the set of its constituent characters; a character is converted to a singleton set; a char-set is returned as-is.")
#define FUNC_NAME s_scm_to_char_set
{
  if (scm_is_string (x))
    return scm_string_to_char_set (x, SCM_UNDEFINED);
  else if (SCM_CHARP (x))
    return scm_char_set (scm_list_1 (x));
  else if (SCM_SMOB_PREDICATE (scm_tc16_charset, x))
    return x;
  else
    scm_wrong_type_arg (NULL, 0, x);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_set_size, "char-set-size", 1, 0, 0,
	    (SCM cs),
	    "Return the number of elements in character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_size
{
  int k, count = 0;
  scm_t_char_set *cs_data;

  SCM_VALIDATE_SMOB (1, cs, charset);
  cs_data = SCM_CHARSET_DATA (cs);

  if (cs_data->len == 0)
    return scm_from_int (0);

  for (k = 0; k < cs_data->len; k++)
    count += cs_data->ranges[k].hi - cs_data->ranges[k].lo + 1;

  return scm_from_int (count);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_count, "char-set-count", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return the number of the elements int the character set\n"
	    "@var{cs} which satisfy the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_count
{
  int k, count = 0;
  scm_t_wchar n;
  scm_t_char_set *cs_data;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);
  cs_data = SCM_CHARSET_DATA (cs);
  if (cs_data->len == 0)
    return scm_from_int (0);

  for (k = 0; k < cs_data->len; k++)
    for (n = cs_data->ranges[k].lo; n <= cs_data->ranges[k].hi; n++)
      {
        SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (n));
        if (scm_is_true (res))
          count++;
      }
  return SCM_I_MAKINUM (count);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_to_list, "char-set->list", 1, 0, 0,
	    (SCM cs),
	    "Return a list containing the elements of the character set\n"
	    "@var{cs}.")
#define FUNC_NAME s_scm_char_set_to_list
{
  int k;
  scm_t_wchar n;
  SCM result = SCM_EOL;
  scm_t_char_set *p;

  SCM_VALIDATE_SMOB (1, cs, charset);
  p = SCM_CHARSET_DATA (cs);
  if (p->len == 0)
    return SCM_EOL;

  for (k = p->len - 1; k >= 0; k--)
    for (n = p->ranges[k].hi; n >= p->ranges[k].lo; n--)
      result = scm_cons (SCM_MAKE_CHAR (n), result);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_to_string, "char-set->string", 1, 0, 0,
	    (SCM cs),
	    "Return a string containing the elements of the character set\n"
	    "@var{cs}.  The order in which the characters are placed in the\n"
	    "string is not defined.")
#define FUNC_NAME s_scm_char_set_to_string
{
  int k;
  int count = 0;
  int idx = 0;
  int wide = 0;
  SCM result;
  scm_t_wchar n;
  scm_t_char_set *cs_data;
  char *buf;
  scm_t_wchar *wbuf;

  SCM_VALIDATE_SMOB (1, cs, charset);
  cs_data = SCM_CHARSET_DATA (cs);
  if (cs_data->len == 0)
    return scm_nullstr;

  if (cs_data->ranges[cs_data->len - 1].hi > 255)
    wide = 1;

  count = scm_to_int (scm_char_set_size (cs));
  if (wide)
    result = scm_i_make_wide_string (count, &wbuf, 0);
  else
    result = scm_i_make_string (count, &buf, 0);

  for (k = 0; k < cs_data->len; k++)
    for (n = cs_data->ranges[k].lo; n <= cs_data->ranges[k].hi; n++)
      {
        if (wide)
          wbuf[idx++] = n;
        else
          buf[idx++] = n;
      }
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_contains_p, "char-set-contains?", 2, 0, 0,
	    (SCM cs, SCM ch),
	    "Return @code{#t} iff the character @var{ch} is contained in the\n"
	    "character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_contains_p
{
  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_CHAR (2, ch);
  return scm_from_bool (SCM_CHARSET_GET (cs, SCM_CHAR (ch)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_every, "char-set-every", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return a true value if every character in the character set\n"
	    "@var{cs} satisfies the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_every
{
  int k;
  scm_t_wchar n;
  SCM res = SCM_BOOL_T;
  scm_t_char_set *cs_data;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);

  cs_data = SCM_CHARSET_DATA (cs);
  if (cs_data->len == 0)
    return SCM_BOOL_T;

  for (k = 0; k < cs_data->len; k++)
    for (n = cs_data->ranges[k].lo; n <= cs_data->ranges[k].hi; n++)
      {
        res = scm_call_1 (pred, SCM_MAKE_CHAR (n));
        if (scm_is_false (res))
          return res;
      }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_any, "char-set-any", 2, 0, 0,
	    (SCM pred, SCM cs),
	    "Return a true value if any character in the character set\n"
	    "@var{cs} satisfies the predicate @var{pred}.")
#define FUNC_NAME s_scm_char_set_any
{
  int k;
  scm_t_wchar n;
  scm_t_char_set *cs_data;

  SCM_VALIDATE_PROC (1, pred);
  SCM_VALIDATE_SMOB (2, cs, charset);

  cs_data = SCM_CHARSET_DATA (cs);
  if (cs_data->len == 0)
    return SCM_BOOL_T;

  for (k = 0; k < cs_data->len; k++)
    for (n = cs_data->ranges[k].lo; n <= cs_data->ranges[k].hi; n++)
      {
        SCM res = scm_call_1 (pred, SCM_MAKE_CHAR (n));
        if (scm_is_true (res))
          return res;
      }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_adjoin, "char-set-adjoin", 1, 0, 1,
            (SCM cs, SCM rest),
            "Add all character arguments to the first argument, which must\n"
            "be a character set.")
#define FUNC_NAME s_scm_char_set_adjoin
{
  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = scm_char_set_copy (cs);

  while (!scm_is_null (rest))
    {
      SCM chr = SCM_CAR (rest);
      scm_t_wchar c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      SCM_CHARSET_SET (cs, c);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_delete, "char-set-delete", 1, 0, 1,
            (SCM cs, SCM rest),
            "Delete all character arguments from the first argument, which\n"
            "must be a character set.")
#define FUNC_NAME s_scm_char_set_delete
{
  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);
  cs = scm_char_set_copy (cs);

  while (!scm_is_null (rest))
    {
      SCM chr = SCM_CAR (rest);
      scm_t_wchar c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      SCM_CHARSET_UNSET (cs, c);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_adjoin_x, "char-set-adjoin!", 1, 0, 1,
            (SCM cs, SCM rest),
            "Add all character arguments to the first argument, which must\n"
            "be a character set.")
#define FUNC_NAME s_scm_char_set_adjoin_x
{
  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  while (!scm_is_null (rest))
    {
      SCM chr = SCM_CAR (rest);
      scm_t_wchar c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      SCM_CHARSET_SET (cs, c);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_delete_x, "char-set-delete!", 1, 0, 1,
            (SCM cs, SCM rest),
            "Delete all character arguments from the first argument, which\n"
            "must be a character set.")
#define FUNC_NAME s_scm_char_set_delete_x
{
  SCM_VALIDATE_SMOB (1, cs, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  while (!scm_is_null (rest))
    {
      SCM chr = SCM_CAR (rest);
      scm_t_wchar c;

      SCM_VALIDATE_CHAR_COPY (1, chr, c);
      rest = SCM_CDR (rest);

      SCM_CHARSET_UNSET (cs, c);
    }
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_complement, "char-set-complement", 1, 0, 0,
            (SCM cs), "Return the complement of the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_complement
{
  SCM res;
  scm_t_char_set *p, *q;

  SCM_VALIDATE_SMOB (1, cs, charset);

  res = make_char_set (FUNC_NAME);
  p = SCM_CHARSET_DATA (res);
  q = SCM_CHARSET_DATA (cs);

  charsets_complement (p, q);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_union, "char-set-union", 0, 0, 1,
	    (SCM rest),
	    "Return the union of all argument character sets.")
#define FUNC_NAME s_scm_char_set_union
{
  int c = 1;
  SCM res;
  scm_t_char_set *p;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  res = make_char_set (FUNC_NAME);
  p = SCM_CHARSET_DATA (res);
  while (!scm_is_null (rest))
    {
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);


      charsets_union (p, (scm_t_char_set *) SCM_SMOB_DATA (cs));
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_intersection, "char-set-intersection", 0, 0, 1,
	    (SCM rest),
	    "Return the intersection of all argument character sets.")
#define FUNC_NAME s_scm_char_set_intersection
{
  SCM res;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  if (scm_is_null (rest))
    res = make_char_set (FUNC_NAME);
  else
    {
      scm_t_char_set *p;
      int argnum = 2;

      res = scm_char_set_copy (SCM_CAR (rest));
      p = SCM_CHARSET_DATA (res);
      rest = SCM_CDR (rest);

      while (scm_is_pair (rest))
        {
          SCM cs = SCM_CAR (rest);
          scm_t_char_set *cs_data;

          SCM_VALIDATE_SMOB (argnum, cs, charset);
          argnum++;
          cs_data = SCM_CHARSET_DATA (cs);
          rest = SCM_CDR (rest);
          charsets_intersection (p, cs_data);
        }
    }

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_difference, "char-set-difference", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the difference of all argument character sets.")
#define FUNC_NAME s_scm_char_set_difference
{
  int c = 2;
  SCM res, compl;
  scm_t_char_set *p, *q;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  res = scm_char_set_copy (cs1);
  p = SCM_CHARSET_DATA (res);
  compl = make_char_set (FUNC_NAME);
  q = SCM_CHARSET_DATA (compl);
  while (!scm_is_null (rest))
    {
      SCM cs = SCM_CAR (rest);
      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      rest = SCM_CDR (rest);

      charsets_complement (q, SCM_CHARSET_DATA (cs));
      charsets_intersection (p, q);
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_xor, "char-set-xor", 0, 0, 1,
	    (SCM rest),
	    "Return the exclusive-or of all argument character sets.")
#define FUNC_NAME s_scm_char_set_xor
{
  SCM res;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  if (scm_is_null (rest))
    res = make_char_set (FUNC_NAME);
  else
    {
      int argnum = 2;
      scm_t_char_set *p;

      res = scm_char_set_copy (SCM_CAR (rest));
      p = SCM_CHARSET_DATA (res);
      rest = SCM_CDR (rest);

      while (scm_is_pair (rest))
        {
          SCM cs = SCM_CAR (rest);
          scm_t_char_set *cs_data;

          SCM_VALIDATE_SMOB (argnum, cs, charset);
          argnum++;
          cs_data = SCM_CHARSET_DATA (cs);
          rest = SCM_CDR (rest);

          charsets_xor (p, cs_data);
        }
    }
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_diff_plus_intersection, "char-set-diff+intersection", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the difference and the intersection of all argument\n"
	    "character sets.")
#define FUNC_NAME s_scm_char_set_diff_plus_intersection
{
  int c = 2;
  SCM res1, res2;
  scm_t_char_set *p, *q;

  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  res1 = scm_char_set_copy (cs1);
  res2 = make_char_set (FUNC_NAME);
  p = SCM_CHARSET_DATA (res1);
  q = SCM_CHARSET_DATA (res2);
  while (!scm_is_null (rest))
    {
      SCM cs = SCM_CAR (rest);
      scm_t_char_set *r;

      SCM_VALIDATE_SMOB (c, cs, charset);
      c++;
      r = SCM_CHARSET_DATA (cs);

      charsets_union (q, r);
      charsets_intersection (p, r);
      rest = SCM_CDR (rest);
    }
  return scm_values (scm_list_2 (res1, res2));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_complement_x, "char-set-complement!", 1, 0, 0,
            (SCM cs), "Return the complement of the character set @var{cs}.")
#define FUNC_NAME s_scm_char_set_complement_x
{
  SCM_VALIDATE_SMOB (1, cs, charset);
  cs = scm_char_set_complement (cs);
  return cs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_union_x, "char-set-union!", 1, 0, 1,
            (SCM cs1, SCM rest),
            "Return the union of all argument character sets.")
#define FUNC_NAME s_scm_char_set_union_x
{
  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  cs1 = scm_char_set_union (scm_cons (cs1, rest));
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_intersection_x, "char-set-intersection!", 1, 0, 1,
            (SCM cs1, SCM rest),
            "Return the intersection of all argument character sets.")
#define FUNC_NAME s_scm_char_set_intersection_x
{
  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  cs1 = scm_char_set_intersection (scm_cons (cs1, rest));
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_difference_x, "char-set-difference!", 1, 0, 1,
            (SCM cs1, SCM rest),
            "Return the difference of all argument character sets.")
#define FUNC_NAME s_scm_char_set_difference_x
{
  SCM_VALIDATE_SMOB (1, cs1, charset);
  SCM_VALIDATE_REST_ARGUMENT (rest);

  cs1 = scm_char_set_difference (cs1, rest);
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_xor_x, "char-set-xor!", 1, 0, 1,
	    (SCM cs1, SCM rest),
	    "Return the exclusive-or of all argument character sets.")
#define FUNC_NAME s_scm_char_set_xor_x
{
  /* a side-effecting variant should presumably give consistent results:
     (define a (char-set #\a))
     (char-set-xor a a a) -> char set #\a
     (char-set-xor! a a a) -> char set #\a
   */
  cs1 = scm_char_set_xor (scm_cons (cs1, rest));
  return cs1;
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_set_diff_plus_intersection_x,
            "char-set-diff+intersection!", 2, 0, 1, (SCM cs1, SCM cs2,
                                                     SCM rest),
            "Return the difference and the intersection of all argument\n"
            "character sets.")
#define FUNC_NAME s_scm_char_set_diff_plus_intersection_x
{
  SCM diff, intersect;

  diff = scm_char_set_difference (cs1, scm_cons (cs2, rest));
  intersect =
    scm_char_set_intersection (scm_cons (cs1, scm_cons (cs2, rest)));
  cs1 = diff;
  cs2 = intersect;
  return scm_values (scm_list_2 (cs1, cs2));
}
#undef FUNC_NAME



/* Standard character sets.  */

SCM scm_char_set_lower_case;
SCM scm_char_set_upper_case;
SCM scm_char_set_title_case;
SCM scm_char_set_letter;
SCM scm_char_set_digit;
SCM scm_char_set_letter_and_digit;
SCM scm_char_set_graphic;
SCM scm_char_set_printing;
SCM scm_char_set_whitespace;
SCM scm_char_set_iso_control;
SCM scm_char_set_punctuation;
SCM scm_char_set_symbol;
SCM scm_char_set_hex_digit;
SCM scm_char_set_blank;
SCM scm_char_set_ascii;
SCM scm_char_set_empty;
SCM scm_char_set_designated;
SCM scm_char_set_full;


/* Create an empty character set and return it after binding it to NAME.  */
static inline SCM
define_charset (const char *name, const scm_t_char_set *p)
{
  SCM cs;

  SCM_NEWSMOB (cs, scm_tc16_charset, p);
  scm_c_define (name, cs);
  return cs;
}

SCM_DEFINE (scm_sys_char_set_dump, "%char-set-dump", 1, 0, 0, (SCM charset), 
            "Returns an association list containing debugging information\n"
            "for @var{charset}. The association list has the following entries."
            "@table @code\n"
            "@item char-set\n"
            "The char-set itself.\n"
            "@item len\n"
            "The number of character ranges the char-set contains\n"
            "@item ranges\n"
            "A list of lists where each sublist a range of code points\n"
            "and their associated characters"
            "@end table")
#define FUNC_NAME s_scm_sys_char_set_dump
{
  SCM e1, e2, e3;
  SCM ranges = SCM_EOL, elt;
  size_t i;
  scm_t_char_set *cs;
  char codepoint_string_lo[9], codepoint_string_hi[9];

  SCM_VALIDATE_SMOB (1, charset, charset);
  cs = SCM_CHARSET_DATA (charset);

  e1 = scm_cons (scm_from_latin1_symbol ("char-set"),
                 charset);
  e2 = scm_cons (scm_from_latin1_symbol ("n"),
                 scm_from_size_t (cs->len));

  for (i = 0; i < cs->len; i++)
    {
      if (cs->ranges[i].lo > 0xFFFF)
        sprintf (codepoint_string_lo, "U+%06x", cs->ranges[i].lo);
      else
        sprintf (codepoint_string_lo, "U+%04x", cs->ranges[i].lo);
      if (cs->ranges[i].hi > 0xFFFF)
        sprintf (codepoint_string_hi, "U+%06x", cs->ranges[i].hi);
      else
        sprintf (codepoint_string_hi, "U+%04x", cs->ranges[i].hi);

      elt = scm_list_4 (SCM_MAKE_CHAR (cs->ranges[i].lo),
                            SCM_MAKE_CHAR (cs->ranges[i].hi),
                            scm_from_locale_string (codepoint_string_lo),
                            scm_from_locale_string (codepoint_string_hi));
      ranges = scm_append (scm_list_2 (ranges,
                                       scm_list_1 (elt)));
    }
  e3 = scm_cons (scm_from_latin1_symbol ("ranges"),
                 ranges);

  return scm_list_3 (e1, e2, e3);
}
#undef FUNC_NAME




void
scm_init_srfi_14 (void)
{
  scm_tc16_charset = scm_make_smob_type ("character-set", 0);
  scm_set_smob_print (scm_tc16_charset, charset_print);

  scm_tc16_charset_cursor = scm_make_smob_type ("char-set-cursor", 0);
  scm_set_smob_print (scm_tc16_charset_cursor, charset_cursor_print);

  scm_char_set_upper_case =
    define_charset ("char-set:upper-case", &cs_upper_case);
  scm_char_set_lower_case =
    define_charset ("char-set:lower-case", &cs_lower_case);
  scm_char_set_title_case =
    define_charset ("char-set:title-case", &cs_title_case);
  scm_char_set_letter = define_charset ("char-set:letter", &cs_letter);
  scm_char_set_digit = define_charset ("char-set:digit", &cs_digit);
  scm_char_set_letter_and_digit =
    define_charset ("char-set:letter+digit", &cs_letter_plus_digit);
  scm_char_set_graphic = define_charset ("char-set:graphic", &cs_graphic);
  scm_char_set_printing = define_charset ("char-set:printing", &cs_printing);
  scm_char_set_whitespace =
    define_charset ("char-set:whitespace", &cs_whitespace);
  scm_char_set_iso_control =
    define_charset ("char-set:iso-control", &cs_iso_control);
  scm_char_set_punctuation =
    define_charset ("char-set:punctuation", &cs_punctuation);
  scm_char_set_symbol = define_charset ("char-set:symbol", &cs_symbol);
  scm_char_set_hex_digit =
    define_charset ("char-set:hex-digit", &cs_hex_digit);
  scm_char_set_blank = define_charset ("char-set:blank", &cs_blank);
  scm_char_set_ascii = define_charset ("char-set:ascii", &cs_ascii);
  scm_char_set_empty = define_charset ("char-set:empty", &cs_empty);
  scm_char_set_designated = define_charset ("char-set:designated", &cs_designated);
  scm_char_set_full = define_charset ("char-set:full", &cs_full);

#include "libguile/srfi-14.x"
}

/* End of srfi-14.c.  */
