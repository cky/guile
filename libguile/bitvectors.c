/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004, 2005, 2006, 2009, 2010 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/__scm.h"
#include "libguile/smob.h"
#include "libguile/strings.h"
#include "libguile/array-handle.h"
#include "libguile/bitvectors.h"
#include "libguile/arrays.h"
#include "libguile/generalized-vectors.h"
#include "libguile/srfi-4.h"

/* Bit vectors. Would be nice if they were implemented on top of bytevectors,
 * but alack, all we have is this crufty C.
 */

static scm_t_bits scm_tc16_bitvector;

#define IS_BITVECTOR(obj)       SCM_SMOB_PREDICATE(scm_tc16_bitvector,(obj))
#define BITVECTOR_BITS(obj)     ((scm_t_uint32 *)SCM_SMOB_DATA(obj))
#define BITVECTOR_LENGTH(obj)   ((size_t)SCM_SMOB_DATA_2(obj))

static int
bitvector_print (SCM vec, SCM port, scm_print_state *pstate)
{
  size_t bit_len = BITVECTOR_LENGTH (vec);
  size_t word_len = (bit_len+31)/32;
  scm_t_uint32 *bits = BITVECTOR_BITS (vec);
  size_t i, j;

  scm_puts ("#*", port);
  for (i = 0; i < word_len; i++, bit_len -= 32)
    {
      scm_t_uint32 mask = 1;
      for (j = 0; j < 32 && j < bit_len; j++, mask <<= 1)
	scm_putc ((bits[i] & mask)? '1' : '0', port);
    }
    
  return 1;
}

static SCM
bitvector_equalp (SCM vec1, SCM vec2)
{
  size_t bit_len = BITVECTOR_LENGTH (vec1);
  size_t word_len = (bit_len + 31) / 32;
  scm_t_uint32 last_mask =  ((scm_t_uint32)-1) >> (32*word_len - bit_len);
  scm_t_uint32 *bits1 = BITVECTOR_BITS (vec1);
  scm_t_uint32 *bits2 = BITVECTOR_BITS (vec2);

  /* compare lengths */
  if (BITVECTOR_LENGTH (vec2) != bit_len)
    return SCM_BOOL_F;
  /* avoid underflow in word_len-1 below. */
  if (bit_len == 0)
    return SCM_BOOL_T;
  /* compare full words */
  if (memcmp (bits1, bits2, sizeof (scm_t_uint32) * (word_len-1)))
    return SCM_BOOL_F;
  /* compare partial last words */
  if ((bits1[word_len-1] & last_mask) != (bits2[word_len-1] & last_mask))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

int
scm_is_bitvector (SCM vec)
{
  return IS_BITVECTOR (vec);
}

SCM_DEFINE (scm_bitvector_p, "bitvector?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} when @var{obj} is a bitvector, else\n"
	    "return @code{#f}.")
#define FUNC_NAME s_scm_bitvector_p
{
  return scm_from_bool (scm_is_bitvector (obj));
}
#undef FUNC_NAME

SCM
scm_c_make_bitvector (size_t len, SCM fill)
{
  size_t word_len = (len + 31) / 32;
  scm_t_uint32 *bits;
  SCM res;

  bits = scm_gc_malloc_pointerless (sizeof (scm_t_uint32) * word_len,
				    "bitvector");
  SCM_NEWSMOB2 (res, scm_tc16_bitvector, bits, len);

  if (!SCM_UNBNDP (fill))
    scm_bitvector_fill_x (res, fill);
  else
    memset (bits, 0, sizeof (scm_t_uint32) * word_len);
      
  return res;
}

SCM_DEFINE (scm_make_bitvector, "make-bitvector", 1, 1, 0,
	    (SCM len, SCM fill),
	    "Create a new bitvector of length @var{len} and\n"
	    "optionally initialize all elements to @var{fill}.")
#define FUNC_NAME s_scm_make_bitvector
{
  return scm_c_make_bitvector (scm_to_size_t (len), fill);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bitvector, "bitvector", 0, 0, 1,
	    (SCM bits),
	    "Create a new bitvector with the arguments as elements.")
#define FUNC_NAME s_scm_bitvector
{
  return scm_list_to_bitvector (bits);
}
#undef FUNC_NAME

size_t
scm_c_bitvector_length (SCM vec)
{
  scm_assert_smob_type (scm_tc16_bitvector, vec);
  return BITVECTOR_LENGTH (vec);
}

SCM_DEFINE (scm_bitvector_length, "bitvector-length", 1, 0, 0,
	    (SCM vec),
	    "Return the length of the bitvector @var{vec}.")
#define FUNC_NAME s_scm_bitvector_length
{
  return scm_from_size_t (scm_c_bitvector_length (vec));
}
#undef FUNC_NAME

const scm_t_uint32 *
scm_array_handle_bit_elements (scm_t_array_handle *h)
{
  return scm_array_handle_bit_writable_elements (h);
}

scm_t_uint32 *
scm_array_handle_bit_writable_elements (scm_t_array_handle *h)
{
  SCM vec = h->array;
  if (SCM_I_ARRAYP (vec))
    vec = SCM_I_ARRAY_V (vec);
  if (IS_BITVECTOR (vec))
    return BITVECTOR_BITS (vec) + h->base/32;
  scm_wrong_type_arg_msg (NULL, 0, h->array, "bit array");
}

size_t
scm_array_handle_bit_elements_offset (scm_t_array_handle *h)
{
  return h->base % 32;
}

const scm_t_uint32 *
scm_bitvector_elements (SCM vec,
			scm_t_array_handle *h,
			size_t *offp,
			size_t *lenp,
			ssize_t *incp)
{
  return scm_bitvector_writable_elements (vec, h, offp, lenp, incp);
}


scm_t_uint32 *
scm_bitvector_writable_elements (SCM vec,
				 scm_t_array_handle *h,
				 size_t *offp,
				 size_t *lenp,
				 ssize_t *incp)
{
  scm_generalized_vector_get_handle (vec, h);
  if (offp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *offp = scm_array_handle_bit_elements_offset (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_bit_writable_elements (h);
}

SCM
scm_c_bitvector_ref (SCM vec, size_t idx)
{
  scm_t_array_handle handle;
  const scm_t_uint32 *bits;

  if (IS_BITVECTOR (vec))
    {
      if (idx >= BITVECTOR_LENGTH (vec))
	scm_out_of_range (NULL, scm_from_size_t (idx));
      bits = BITVECTOR_BITS(vec);
      return scm_from_bool (bits[idx/32] & (1L << (idx%32)));
    }
  else
    {
      SCM res;
      size_t len, off;
      ssize_t inc;
  
      bits = scm_bitvector_elements (vec, &handle, &off, &len, &inc);
      if (idx >= len)
	scm_out_of_range (NULL, scm_from_size_t (idx));
      idx = idx*inc + off;
      res = scm_from_bool (bits[idx/32] & (1L << (idx%32)));
      scm_array_handle_release (&handle);
      return res;
    }
}

SCM_DEFINE (scm_bitvector_ref, "bitvector-ref", 2, 0, 0,
	    (SCM vec, SCM idx),
	    "Return the element at index @var{idx} of the bitvector\n"
	    "@var{vec}.")
#define FUNC_NAME s_scm_bitvector_ref
{
  return scm_c_bitvector_ref (vec, scm_to_size_t (idx));
}
#undef FUNC_NAME

void
scm_c_bitvector_set_x (SCM vec, size_t idx, SCM val)
{
  scm_t_array_handle handle;
  scm_t_uint32 *bits, mask;

  if (IS_BITVECTOR (vec))
    {
      if (idx >= BITVECTOR_LENGTH (vec))
	scm_out_of_range (NULL, scm_from_size_t (idx));
      bits = BITVECTOR_BITS(vec);
    }
  else
    {
      size_t len, off;
      ssize_t inc;
  
      bits = scm_bitvector_writable_elements (vec, &handle, &off, &len, &inc);
      if (idx >= len)
	scm_out_of_range (NULL, scm_from_size_t (idx));
      idx = idx*inc + off;
    }

  mask = 1L << (idx%32);
  if (scm_is_true (val))
    bits[idx/32] |= mask;
  else
    bits[idx/32] &= ~mask;

  if (!IS_BITVECTOR (vec))
      scm_array_handle_release (&handle);
}

SCM_DEFINE (scm_bitvector_set_x, "bitvector-set!", 3, 0, 0,
	    (SCM vec, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the bitvector\n"
	    "@var{vec} when @var{val} is true, else clear it.")
#define FUNC_NAME s_scm_bitvector_set_x
{
  scm_c_bitvector_set_x (vec, scm_to_size_t (idx), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bitvector_fill_x, "bitvector-fill!", 2, 0, 0,
	    (SCM vec, SCM val),
	    "Set all elements of the bitvector\n"
	    "@var{vec} when @var{val} is true, else clear them.")
#define FUNC_NAME s_scm_bitvector_fill_x
{
  scm_t_array_handle handle;
  size_t off, len;
  ssize_t inc;
  scm_t_uint32 *bits;

  bits = scm_bitvector_writable_elements (vec, &handle,
					  &off, &len, &inc);

  if (off == 0 && inc == 1 && len > 0)
    {
      /* the usual case
       */
      size_t word_len = (len + 31) / 32;
      scm_t_uint32 last_mask =  ((scm_t_uint32)-1) >> (32*word_len - len);

      if (scm_is_true (val))
	{
	  memset (bits, 0xFF, sizeof(scm_t_uint32)*(word_len-1));
	  bits[word_len-1] |= last_mask;
	}
      else
	{
	  memset (bits, 0x00, sizeof(scm_t_uint32)*(word_len-1));
	  bits[word_len-1] &= ~last_mask;
	}
    }
  else
    {
      size_t i;
      for (i = 0; i < len; i++)
	scm_array_handle_set (&handle, i*inc, val);
    }

  scm_array_handle_release (&handle);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_list_to_bitvector, "list->bitvector", 1, 0, 0,
	    (SCM list),
	    "Return a new bitvector initialized with the elements\n"
	    "of @var{list}.")
#define FUNC_NAME s_scm_list_to_bitvector
{
  size_t bit_len = scm_to_size_t (scm_length (list));
  SCM vec = scm_c_make_bitvector (bit_len, SCM_UNDEFINED);
  size_t word_len = (bit_len+31)/32;
  scm_t_array_handle handle;
  scm_t_uint32 *bits = scm_bitvector_writable_elements (vec, &handle,
							NULL, NULL, NULL);
  size_t i, j;

  for (i = 0; i < word_len && scm_is_pair (list); i++, bit_len -= 32)
    {
      scm_t_uint32 mask = 1;
      bits[i] = 0;
      for (j = 0; j < 32 && j < bit_len;
	   j++, mask <<= 1, list = SCM_CDR (list))
	if (scm_is_true (SCM_CAR (list)))
	  bits[i] |= mask;
    }

  scm_array_handle_release (&handle);

  return vec;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bitvector_to_list, "bitvector->list", 1, 0, 0,
	    (SCM vec),
	    "Return a new list initialized with the elements\n"
	    "of the bitvector @var{vec}.")
#define FUNC_NAME s_scm_bitvector_to_list
{
  scm_t_array_handle handle;
  size_t off, len;
  ssize_t inc;
  scm_t_uint32 *bits;
  SCM res = SCM_EOL;

  bits = scm_bitvector_writable_elements (vec, &handle,
					  &off, &len, &inc);

  if (off == 0 && inc == 1)
    {
      /* the usual case
       */
      size_t word_len = (len + 31) / 32;
      size_t i, j;

      for (i = 0; i < word_len; i++, len -= 32)
	{
	  scm_t_uint32 mask = 1;
	  for (j = 0; j < 32 && j < len; j++, mask <<= 1)
	    res = scm_cons ((bits[i] & mask)? SCM_BOOL_T : SCM_BOOL_F, res);
	}
    }
  else
    {
      size_t i;
      for (i = 0; i < len; i++)
	res = scm_cons (scm_array_handle_ref (&handle, i*inc), res);
    }

  scm_array_handle_release (&handle);
  
  return scm_reverse_x (res, SCM_EOL);
}
#undef FUNC_NAME

/* From mmix-arith.w by Knuth.

  Here's a fun way to count the number of bits in a tetrabyte.

  [This classical trick is called the ``Gillies--Miller method for
  sideways addition'' in {\sl The Preparation of Programs for an
  Electronic Digital Computer\/} by Wilkes, Wheeler, and Gill, second
  edition (Reading, Mass.:\ Addison--Wesley, 1957), 191--193. Some of
  the tricks used here were suggested by Balbir Singh, Peter
  Rossmanith, and Stefan Schwoon.]
*/

static size_t
count_ones (scm_t_uint32 x)
{
  x=x-((x>>1)&0x55555555);
  x=(x&0x33333333)+((x>>2)&0x33333333);
  x=(x+(x>>4))&0x0f0f0f0f;
  x=x+(x>>8);
  return (x+(x>>16)) & 0xff;
}

SCM_DEFINE (scm_bit_count, "bit-count", 2, 0, 0,
	    (SCM b, SCM bitvector),
	    "Return the number of occurrences of the boolean @var{b} in\n"
	    "@var{bitvector}.")
#define FUNC_NAME s_scm_bit_count
{
  scm_t_array_handle handle;
  size_t off, len;
  ssize_t inc;
  scm_t_uint32 *bits;
  int bit = scm_to_bool (b);
  size_t count = 0;

  bits = scm_bitvector_writable_elements (bitvector, &handle,
					  &off, &len, &inc);

  if (off == 0 && inc == 1 && len > 0)
    {
      /* the usual case
       */
      size_t word_len = (len + 31) / 32;
      scm_t_uint32 last_mask =  ((scm_t_uint32)-1) >> (32*word_len - len);
      size_t i;

      for (i = 0; i < word_len-1; i++)
	count += count_ones (bits[i]);
      count += count_ones (bits[i] & last_mask);
    }
  else
    {
      size_t i;
      for (i = 0; i < len; i++)
	if (scm_is_true (scm_array_handle_ref (&handle, i*inc)))
	  count++;
    }
  
  scm_array_handle_release (&handle);

  return scm_from_size_t (bit? count : len-count);
}
#undef FUNC_NAME

/* returns 32 for x == 0. 
*/
static size_t
find_first_one (scm_t_uint32 x)
{
  size_t pos = 0;
  /* do a binary search in x. */
  if ((x & 0xFFFF) == 0)
    x >>= 16, pos += 16;
  if ((x & 0xFF) == 0)
    x >>= 8, pos += 8;
  if ((x & 0xF) == 0)
    x >>= 4, pos += 4;
  if ((x & 0x3) == 0)
    x >>= 2, pos += 2;
  if ((x & 0x1) == 0)
    pos += 1;
  return pos;
}

SCM_DEFINE (scm_bit_position, "bit-position", 3, 0, 0,
           (SCM item, SCM v, SCM k),
	    "Return the index of the first occurrence of @var{item} in bit\n"
	    "vector @var{v}, starting from @var{k}.  If there is no\n"
	    "@var{item} entry between @var{k} and the end of\n"
	    "@var{bitvector}, then return @code{#f}.  For example,\n"
	    "\n"
	    "@example\n"
	    "(bit-position #t #*000101 0)  @result{} 3\n"
	    "(bit-position #f #*0001111 3) @result{} #f\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_position
{
  scm_t_array_handle handle;
  size_t off, len, first_bit;
  ssize_t inc;
  const scm_t_uint32 *bits;
  int bit = scm_to_bool (item);
  SCM res = SCM_BOOL_F;
  
  bits = scm_bitvector_elements (v, &handle, &off, &len, &inc);
  first_bit = scm_to_unsigned_integer (k, 0, len);

  if (off == 0 && inc == 1 && len > 0)
    {
      size_t i, word_len = (len + 31) / 32;
      scm_t_uint32 last_mask =  ((scm_t_uint32)-1) >> (32*word_len - len);
      size_t first_word = first_bit / 32;
      scm_t_uint32 first_mask =
	((scm_t_uint32)-1) << (first_bit - 32*first_word);
      scm_t_uint32 w;
      
      for (i = first_word; i < word_len; i++)
	{
	  w = (bit? bits[i] : ~bits[i]);
	  if (i == first_word)
	    w &= first_mask;
	  if (i == word_len-1)
	    w &= last_mask;
	  if (w)
	    {
	      res = scm_from_size_t (32*i + find_first_one (w));
	      break;
	    }
	}
    }
  else
    {
      size_t i;
      for (i = first_bit; i < len; i++)
	{
	  SCM elt = scm_array_handle_ref (&handle, i*inc);
	  if ((bit && scm_is_true (elt)) || (!bit && scm_is_false (elt)))
	    {
	      res = scm_from_size_t (i);
	      break;
	    }
	}
    }

  scm_array_handle_release (&handle);

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bit_set_star_x, "bit-set*!", 3, 0, 0,
	    (SCM v, SCM kv, SCM obj),
	    "Set entries of bit vector @var{v} to @var{obj}, with @var{kv}\n"
	    "selecting the entries to change.  The return value is\n"
	    "unspecified.\n"
	    "\n"
	    "If @var{kv} is a bit vector, then those entries where it has\n"
	    "@code{#t} are the ones in @var{v} which are set to @var{obj}.\n"
	    "@var{kv} and @var{v} must be the same length.  When @var{obj}\n"
	    "is @code{#t} it's like @var{kv} is OR'ed into @var{v}.  Or when\n"
	    "@var{obj} is @code{#f} it can be seen as an ANDNOT.\n"
	    "\n"
	    "@example\n"
	    "(define bv #*01000010)\n"
	    "(bit-set*! bv #*10010001 #t)\n"
	    "bv\n"
	    "@result{} #*11010011\n"
	    "@end example\n"
	    "\n"
	    "If @var{kv} is a u32vector, then its elements are\n"
	    "indices into @var{v} which are set to @var{obj}.\n"
	    "\n"
	    "@example\n"
	    "(define bv #*01000010)\n"
	    "(bit-set*! bv #u32(5 2 7) #t)\n"
	    "bv\n"
	    "@result{} #*01100111\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_set_star_x
{
  scm_t_array_handle v_handle;
  size_t v_off, v_len;
  ssize_t v_inc;
  scm_t_uint32 *v_bits;
  int bit;

  /* Validate that OBJ is a boolean so this is done even if we don't
     need BIT.
  */
  bit = scm_to_bool (obj);

  v_bits = scm_bitvector_writable_elements (v, &v_handle,
					    &v_off, &v_len, &v_inc);

  if (scm_is_bitvector (kv))
    {
      scm_t_array_handle kv_handle;
      size_t kv_off, kv_len;
      ssize_t kv_inc;
      const scm_t_uint32 *kv_bits;
      
      kv_bits = scm_bitvector_elements (v, &kv_handle,
					&kv_off, &kv_len, &kv_inc);

      if (v_len != kv_len)
	scm_misc_error (NULL,
			"bit vectors must have equal length",
			SCM_EOL);

      if (v_off == 0 && v_inc == 1 && kv_off == 0 && kv_inc == 1 && kv_len > 0)
	{
	  size_t word_len = (kv_len + 31) / 32;
	  scm_t_uint32 last_mask = ((scm_t_uint32)-1) >> (32*word_len - kv_len);
	  size_t i;
 
	  if (bit == 0)
	    {
	      for (i = 0; i < word_len-1; i++)
		v_bits[i] &= ~kv_bits[i];
	      v_bits[i] &= ~(kv_bits[i] & last_mask);
	    }
	  else
	    {
	      for (i = 0; i < word_len-1; i++)
		v_bits[i] |= kv_bits[i];
	      v_bits[i] |= kv_bits[i] & last_mask;
	    }
	}
      else
	{
	  size_t i;
	  for (i = 0; i < kv_len; i++)
	    if (scm_is_true (scm_array_handle_ref (&kv_handle, i*kv_inc)))
	      scm_array_handle_set (&v_handle, i*v_inc, obj);
	}
      
      scm_array_handle_release (&kv_handle);

    }
  else if (scm_is_true (scm_u32vector_p (kv)))
    {
      scm_t_array_handle kv_handle;
      size_t i, kv_len;
      ssize_t kv_inc;
      const scm_t_uint32 *kv_elts;

      kv_elts = scm_u32vector_elements (kv, &kv_handle, &kv_len, &kv_inc);
      for (i = 0; i < kv_len; i++, kv_elts += kv_inc)
	scm_array_handle_set (&v_handle, (*kv_elts)*v_inc, obj);

      scm_array_handle_release (&kv_handle);
    }
  else 
    scm_wrong_type_arg_msg (NULL, 0, kv, "bitvector or u32vector");

  scm_array_handle_release (&v_handle);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_bit_count_star, "bit-count*", 3, 0, 0,
           (SCM v, SCM kv, SCM obj),
	    "Return a count of how many entries in bit vector @var{v} are\n"
	    "equal to @var{obj}, with @var{kv} selecting the entries to\n"
	    "consider.\n"
	    "\n"
	    "If @var{kv} is a bit vector, then those entries where it has\n"
	    "@code{#t} are the ones in @var{v} which are considered.\n"
	    "@var{kv} and @var{v} must be the same length.\n"
	    "\n"
	    "If @var{kv} is a u32vector, then it contains\n"
	    "the indexes in @var{v} to consider.\n"
	    "\n"
	    "For example,\n"
	    "\n"
	    "@example\n"
	    "(bit-count* #*01110111 #*11001101 #t) @result{} 3\n"
	    "(bit-count* #*01110111 #u32(7 0 4) #f)  @result{} 2\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_count_star
{
  scm_t_array_handle v_handle;
  size_t v_off, v_len;
  ssize_t v_inc;
  const scm_t_uint32 *v_bits;
  size_t count = 0;
  int bit;

  /* Validate that OBJ is a boolean so this is done even if we don't
     need BIT.
  */
  bit = scm_to_bool (obj);

  v_bits = scm_bitvector_elements (v, &v_handle,
				   &v_off, &v_len, &v_inc);

  if (scm_is_bitvector (kv))
    {
      scm_t_array_handle kv_handle;
      size_t kv_off, kv_len;
      ssize_t kv_inc;
      const scm_t_uint32 *kv_bits;
      
      kv_bits = scm_bitvector_elements (v, &kv_handle,
					&kv_off, &kv_len, &kv_inc);

      if (v_len != kv_len)
	scm_misc_error (NULL,
			"bit vectors must have equal length",
			SCM_EOL);

      if (v_off == 0 && v_inc == 1 && kv_off == 0 && kv_inc == 1 && kv_len > 0)
	{
	  size_t i, word_len = (kv_len + 31) / 32;
	  scm_t_uint32 last_mask = ((scm_t_uint32)-1) >> (32*word_len - kv_len);
	  scm_t_uint32 xor_mask = bit? 0 : ((scm_t_uint32)-1);

	  for (i = 0; i < word_len-1; i++)
	    count += count_ones ((v_bits[i]^xor_mask) & kv_bits[i]);
	  count += count_ones ((v_bits[i]^xor_mask) & kv_bits[i] & last_mask);
 	}
      else
	{
	  size_t i;
	  for (i = 0; i < kv_len; i++)
	    if (scm_is_true (scm_array_handle_ref (&kv_handle, i)))
	      {
		SCM elt = scm_array_handle_ref (&v_handle, i*v_inc);
		if ((bit && scm_is_true (elt)) || (!bit && scm_is_false (elt)))
		  count++;
	      }
	}
      
      scm_array_handle_release (&kv_handle);

    }
  else if (scm_is_true (scm_u32vector_p (kv)))
    {
      scm_t_array_handle kv_handle;
      size_t i, kv_len;
      ssize_t kv_inc;
      const scm_t_uint32 *kv_elts;

      kv_elts = scm_u32vector_elements (kv, &kv_handle, &kv_len, &kv_inc);
      for (i = 0; i < kv_len; i++, kv_elts += kv_inc)
	{
	  SCM elt = scm_array_handle_ref (&v_handle, (*kv_elts)*v_inc);
	  if ((bit && scm_is_true (elt)) || (!bit && scm_is_false (elt)))
	    count++;
	}

      scm_array_handle_release (&kv_handle);
    }
  else 
    scm_wrong_type_arg_msg (NULL, 0, kv, "bitvector or u32vector");

  scm_array_handle_release (&v_handle);

  return scm_from_size_t (count);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bit_invert_x, "bit-invert!", 1, 0, 0, 
           (SCM v),
	    "Modify the bit vector @var{v} by replacing each element with\n"
	    "its negation.")
#define FUNC_NAME s_scm_bit_invert_x
{
  scm_t_array_handle handle;
  size_t off, len;
  ssize_t inc;
  scm_t_uint32 *bits;

  bits = scm_bitvector_writable_elements (v, &handle, &off, &len, &inc);
  
  if (off == 0 && inc == 1 && len > 0)
    {
      size_t word_len = (len + 31) / 32;
      scm_t_uint32 last_mask = ((scm_t_uint32)-1) >> (32*word_len - len);
      size_t i;

      for (i = 0; i < word_len-1; i++)
	bits[i] = ~bits[i];
      bits[i] = bits[i] ^ last_mask;
    }
  else
    {
      size_t i;
      for (i = 0; i < len; i++)
	scm_array_handle_set (&handle, i*inc,
			      scm_not (scm_array_handle_ref (&handle, i*inc)));
    }

  scm_array_handle_release (&handle);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM
scm_istr2bve (SCM str)
{
  scm_t_array_handle handle;
  size_t len = scm_i_string_length (str);
  SCM vec = scm_c_make_bitvector (len, SCM_UNDEFINED);
  SCM res = vec;

  scm_t_uint32 mask;
  size_t k, j;
  const char *c_str;
  scm_t_uint32 *data;

  data = scm_bitvector_writable_elements (vec, &handle, NULL, NULL, NULL);
  c_str = scm_i_string_chars (str);

  for (k = 0; k < (len + 31) / 32; k++)
    {
      data[k] = 0L;
      j = len - k * 32;
      if (j > 32)
	j = 32;
      for (mask = 1L; j--; mask <<= 1)
	switch (*c_str++)
	  {
	  case '0':
	    break;
	  case '1':
	    data[k] |= mask;
	    break;
	  default:
	    res = SCM_BOOL_F;
	    goto exit;
	  }
    }
  
 exit:
  scm_array_handle_release (&handle);
  scm_remember_upto_here_1 (str);
  return res;
}

/* FIXME: h->array should be h->vector */
static SCM
bitvector_handle_ref (scm_t_array_handle *h, size_t pos)
{
  return scm_c_bitvector_ref (h->array, pos);
}

static void
bitvector_handle_set (scm_t_array_handle *h, size_t pos, SCM val)
{
  scm_c_bitvector_set_x (h->array, pos, val);
}

static void
bitvector_get_handle (SCM bv, scm_t_array_handle *h)
{
  h->array = bv;
  h->ndims = 1;
  h->dims = &h->dim0;
  h->dim0.lbnd = 0;
  h->dim0.ubnd = BITVECTOR_LENGTH (bv) - 1;
  h->dim0.inc = 1;
  h->element_type = SCM_ARRAY_ELEMENT_TYPE_BIT;
  h->elements = h->writable_elements = BITVECTOR_BITS (bv);
}

SCM_ARRAY_IMPLEMENTATION (SCM_SMOB_TYPE_BITS (scm_tc16_bitvector),
                          SCM_SMOB_TYPE_MASK,
                          bitvector_handle_ref, bitvector_handle_set,
                          bitvector_get_handle)
SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_BIT, scm_make_bitvector)

void
scm_init_bitvectors ()
{
  scm_tc16_bitvector = scm_make_smob_type ("bitvector", 0);
  scm_set_smob_print (scm_tc16_bitvector, bitvector_print);
  scm_set_smob_equalp (scm_tc16_bitvector, bitvector_equalp);

#include "libguile/bitvectors.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
