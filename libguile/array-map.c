/* Copyright (C) 1996,1998,2000,2001,2004,2005, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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
#include "libguile/strings.h"
#include "libguile/arrays.h"
#include "libguile/smob.h"
#include "libguile/chars.h"
#include "libguile/eq.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/root.h"
#include "libguile/vectors.h"
#include "libguile/bitvectors.h"
#include "libguile/srfi-4.h"
#include "libguile/generalized-arrays.h"
#include "libguile/generalized-vectors.h"

#include "libguile/validate.h"
#include "libguile/array-map.h"


/* The WHAT argument for `scm_gc_malloc ()' et al.  */
static const char indices_gc_hint[] = "array-indices";


#define GVREF scm_c_generalized_vector_ref
#define GVSET scm_c_generalized_vector_set_x

static unsigned long
cind (SCM ra, long *ve)
{
  unsigned long i;
  int k;
  if (!SCM_I_ARRAYP (ra))
    return *ve;
  i = SCM_I_ARRAY_BASE (ra);
  for (k = 0; k < SCM_I_ARRAY_NDIM (ra); k++)
    i += (ve[k] - SCM_I_ARRAY_DIMS (ra)[k].lbnd) * SCM_I_ARRAY_DIMS (ra)[k].inc;
  return i;
}


/* Checker for scm_array mapping functions:
   return values: 4 --> shapes, increments, and bases are the same;
   3 --> shapes and increments are the same;
   2 --> shapes are the same;
   1 --> ras are at least as big as ra0;
   0 --> no match.
   */

int 
scm_ra_matchp (SCM ra0, SCM ras)
{
  SCM ra1;
  scm_t_array_dim dims;
  scm_t_array_dim *s0 = &dims;
  scm_t_array_dim *s1;
  unsigned long bas0 = 0;
  int i, ndim = 1;
  int exact = 2	  /* 4 */ ;  /* Don't care about values >2 (yet?) */

  if (scm_is_generalized_vector (ra0))
    {
      s0->lbnd = 0;
      s0->inc = 1;
      s0->ubnd = scm_c_generalized_vector_length (ra0) - 1;
    }
  else if (SCM_I_ARRAYP (ra0))
    {
      ndim = SCM_I_ARRAY_NDIM (ra0);
      s0 = SCM_I_ARRAY_DIMS (ra0);
      bas0 = SCM_I_ARRAY_BASE (ra0);
    }
  else
    return 0;

  while (SCM_NIMP (ras))
    {
      ra1 = SCM_CAR (ras);
      
      if (scm_is_generalized_vector (ra1))
	{
	  size_t length;
	  
	  if (1 != ndim)
	    return 0;
	  
	  length = scm_c_generalized_vector_length (ra1);
	  
	  switch (exact)
	    {
	    case 4:
	      if (0 != bas0)
		exact = 3;
	    case 3:
	      if (1 != s0->inc)
		exact = 2;
	    case 2:
	      if ((0 == s0->lbnd) && (s0->ubnd == length - 1))
		break;
	      exact = 1;
	    case 1:
	      if (s0->lbnd < 0 || s0->ubnd >= length)
		return 0;
	    }
	}
      else if (SCM_I_ARRAYP (ra1) && ndim == SCM_I_ARRAY_NDIM (ra1))
	{
	  s1 = SCM_I_ARRAY_DIMS (ra1);
	  if (bas0 != SCM_I_ARRAY_BASE (ra1))
	    exact = 3;
	  for (i = 0; i < ndim; i++)
	    switch (exact)
	      {
	      case 4:
	      case 3:
		if (s0[i].inc != s1[i].inc)
		  exact = 2;
	      case 2:
		if (s0[i].lbnd == s1[i].lbnd && s0[i].ubnd == s1[i].ubnd)
		  break;
		exact = 1;
	      default:
		if (s0[i].lbnd < s1[i].lbnd || s0[i].ubnd > s1[i].ubnd)
		  return (s0[i].lbnd <= s0[i].ubnd ? 0 : 1);
	      }
	}
      else
	return 0;

      ras = SCM_CDR (ras);
    }

  return exact;
}

/* array mapper: apply cproc to each dimension of the given arrays?. 
     int (*cproc) ();   procedure to call on unrolled arrays?
			   cproc (dest, source list) or
			   cproc (dest, data, source list).  
     SCM data;          data to give to cproc or unbound. 
     SCM ra0;           destination array.
     SCM lra;           list of source arrays.
     const char *what;  caller, for error reporting. */
int 
scm_ramapc (void *cproc_ptr, SCM data, SCM ra0, SCM lra, const char *what)
{
  SCM z;
  SCM vra0, ra1, vra1;
  SCM lvra, *plvra;
  long *vinds;
  int k, kmax;
  int (*cproc) ();

  cproc = cproc_ptr;
  switch (scm_ra_matchp (ra0, lra))
    {
    default:
    case 0:
      scm_misc_error (what, "array shape mismatch: ~S", scm_list_1 (ra0));
    case 2:
    case 3:
    case 4:			/* Try unrolling arrays */
      kmax = (SCM_I_ARRAYP (ra0) ? SCM_I_ARRAY_NDIM (ra0) - 1 : 0);
      if (kmax < 0)
	goto gencase;
      vra0 = scm_array_contents (ra0, SCM_UNDEFINED);
      if (SCM_IMP (vra0)) goto gencase;
      if (!SCM_I_ARRAYP (vra0))
	{
	  size_t length = scm_c_generalized_vector_length (vra0);
	  vra1 = scm_i_make_array (1);
	  SCM_I_ARRAY_BASE (vra1) = 0;
	  SCM_I_ARRAY_DIMS (vra1)->lbnd = 0;
	  SCM_I_ARRAY_DIMS (vra1)->ubnd = length - 1;
	  SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	  SCM_I_ARRAY_V (vra1) = vra0;
	  vra0 = vra1;
	}
      lvra = SCM_EOL;
      plvra = &lvra;
      for (z = lra; SCM_NIMP (z); z = SCM_CDR (z))
	{
	  ra1 = SCM_CAR (z);
	  vra1 = scm_i_make_array (1);
	  SCM_I_ARRAY_DIMS (vra1)->lbnd = SCM_I_ARRAY_DIMS (vra0)->lbnd;
	  SCM_I_ARRAY_DIMS (vra1)->ubnd = SCM_I_ARRAY_DIMS (vra0)->ubnd;
	  if (!SCM_I_ARRAYP (ra1))
	    {
	      SCM_I_ARRAY_BASE (vra1) = 0;
	      SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	      SCM_I_ARRAY_V (vra1) = ra1;
	    }
	  else if (!SCM_I_ARRAY_CONTP (ra1))
	    goto gencase;
	  else
	    {
	      SCM_I_ARRAY_BASE (vra1) = SCM_I_ARRAY_BASE (ra1);
	      SCM_I_ARRAY_DIMS (vra1)->inc = SCM_I_ARRAY_DIMS (ra1)[kmax].inc;
	      SCM_I_ARRAY_V (vra1) = SCM_I_ARRAY_V (ra1);
	    }
	  *plvra = scm_cons (vra1, SCM_EOL);
	  plvra = SCM_CDRLOC (*plvra);
	}
      return (SCM_UNBNDP (data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra));
    case 1:
    gencase:			/* Have to loop over all dimensions. */
      vra0 = scm_i_make_array (1);
    if (SCM_I_ARRAYP (ra0))
      {
	kmax = SCM_I_ARRAY_NDIM (ra0) - 1;
	if (kmax < 0)
	  {
	    SCM_I_ARRAY_DIMS (vra0)->lbnd = 0;
	    SCM_I_ARRAY_DIMS (vra0)->ubnd = 0;
	    SCM_I_ARRAY_DIMS (vra0)->inc = 1;
	  }
	else
	  {
	    SCM_I_ARRAY_DIMS (vra0)->lbnd = SCM_I_ARRAY_DIMS (ra0)[kmax].lbnd;
	    SCM_I_ARRAY_DIMS (vra0)->ubnd = SCM_I_ARRAY_DIMS (ra0)[kmax].ubnd;
	    SCM_I_ARRAY_DIMS (vra0)->inc = SCM_I_ARRAY_DIMS (ra0)[kmax].inc;
	  }
	SCM_I_ARRAY_BASE (vra0) = SCM_I_ARRAY_BASE (ra0);
	SCM_I_ARRAY_V (vra0) = SCM_I_ARRAY_V (ra0);
      }
    else
      {
	size_t length = scm_c_generalized_vector_length (ra0);
	kmax = 0;
	SCM_I_ARRAY_DIMS (vra0)->lbnd = 0;
	SCM_I_ARRAY_DIMS (vra0)->ubnd = length - 1;
	SCM_I_ARRAY_DIMS (vra0)->inc = 1;
	SCM_I_ARRAY_BASE (vra0) = 0;
	SCM_I_ARRAY_V (vra0) = ra0;
	ra0 = vra0;
      }
    lvra = SCM_EOL;
    plvra = &lvra;
    for (z = lra; SCM_NIMP (z); z = SCM_CDR (z))
      {
	ra1 = SCM_CAR (z);
	vra1 = scm_i_make_array (1);
	SCM_I_ARRAY_DIMS (vra1)->lbnd = SCM_I_ARRAY_DIMS (vra0)->lbnd;
	SCM_I_ARRAY_DIMS (vra1)->ubnd = SCM_I_ARRAY_DIMS (vra0)->ubnd;
	if (SCM_I_ARRAYP (ra1))
	  {
	    if (kmax >= 0)
	      SCM_I_ARRAY_DIMS (vra1)->inc = SCM_I_ARRAY_DIMS (ra1)[kmax].inc;
	    SCM_I_ARRAY_V (vra1) = SCM_I_ARRAY_V (ra1);
	  }
	else
	  {
	    SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	    SCM_I_ARRAY_V (vra1) = ra1;
	  }
	*plvra = scm_cons (vra1, SCM_EOL);
	plvra = SCM_CDRLOC (*plvra);
      }

    vinds = scm_gc_malloc_pointerless (sizeof(long) * SCM_I_ARRAY_NDIM (ra0),
				       indices_gc_hint);

    for (k = 0; k <= kmax; k++)
      vinds[k] = SCM_I_ARRAY_DIMS (ra0)[k].lbnd;
    k = kmax;
    do
      {
	if (k == kmax)
	  {
	    SCM y = lra;
	    SCM_I_ARRAY_BASE (vra0) = cind (ra0, vinds);
	    for (z = lvra; SCM_NIMP (z); z = SCM_CDR (z), y = SCM_CDR (y))
	      SCM_I_ARRAY_BASE (SCM_CAR (z)) = cind (SCM_CAR (y), vinds);
	    if (0 == (SCM_UNBNDP (data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra)))
	      return 0;
	    k--;
	    continue;
	  }
	if (vinds[k] < SCM_I_ARRAY_DIMS (ra0)[k].ubnd)
	  {
	    vinds[k]++;
	    k++;
	    continue;
	  }
	vinds[k] = SCM_I_ARRAY_DIMS (ra0)[k].lbnd - 1;
	k--;
      }
    while (k >= 0);

    return 1;
    }
}


SCM_DEFINE (scm_array_fill_x, "array-fill!", 2, 0, 0,
	    (SCM ra, SCM fill),
	    "Store @var{fill} in every element of @var{array}.  The value returned\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_array_fill_x
{
  scm_ramapc (scm_array_fill_int, fill, ra, SCM_EOL, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* to be used as cproc in scm_ramapc to fill an array dimension with
   "fill". */
int 
scm_array_fill_int (SCM ra, SCM fill, SCM ignore SCM_UNUSED)
#define FUNC_NAME s_scm_array_fill_x
{
  unsigned long i;
  unsigned long n = SCM_I_ARRAY_DIMS (ra)->ubnd - SCM_I_ARRAY_DIMS (ra)->lbnd + 1;
  long inc = SCM_I_ARRAY_DIMS (ra)->inc;
  unsigned long base = SCM_I_ARRAY_BASE (ra);

  ra = SCM_I_ARRAY_V (ra);

  for (i = base; n--; i += inc)
    GVSET (ra, i, fill);

  return 1;
}
#undef FUNC_NAME



static int 
racp (SCM src, SCM dst)
{
  long n = (SCM_I_ARRAY_DIMS (src)->ubnd - SCM_I_ARRAY_DIMS (src)->lbnd + 1);
  long inc_d, inc_s = SCM_I_ARRAY_DIMS (src)->inc;
  unsigned long i_d, i_s = SCM_I_ARRAY_BASE (src);
  dst = SCM_CAR (dst);
  inc_d = SCM_I_ARRAY_DIMS (dst)->inc;
  i_d = SCM_I_ARRAY_BASE (dst);
  src = SCM_I_ARRAY_V (src);
  dst = SCM_I_ARRAY_V (dst);

  for (; n-- > 0; i_s += inc_s, i_d += inc_d)
    GVSET (dst, i_d, GVREF (src, i_s));
  return 1;
}

SCM_REGISTER_PROC(s_array_copy_in_order_x, "array-copy-in-order!", 2, 0, 0, scm_array_copy_x);


SCM_DEFINE (scm_array_copy_x, "array-copy!", 2, 0, 0,
	    (SCM src, SCM dst),
	    "@deffnx {Scheme Procedure} array-copy-in-order! src dst\n"
	    "Copy every element from vector or array @var{source} to the\n"
	    "corresponding element of @var{destination}.  @var{destination} must have\n"
	    "the same rank as @var{source}, and be at least as large in each\n"
	    "dimension.  The order is unspecified.")
#define FUNC_NAME s_scm_array_copy_x
{
  scm_ramapc (racp, SCM_UNDEFINED, src, scm_cons (dst, SCM_EOL), FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Functions callable by ARRAY-MAP! */


int
scm_ra_eqp (SCM ra0, SCM ras)
{
  SCM ra1 = SCM_CAR (ras), ra2 = SCM_CAR (SCM_CDR (ras));
  scm_t_array_handle ra0_handle;
  scm_t_array_dim *ra0_dims;
  size_t n;
  ssize_t inc0;
  size_t i0 = 0;
  unsigned long i1 = SCM_I_ARRAY_BASE (ra1), i2 = SCM_I_ARRAY_BASE (ra2);
  long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra1 = SCM_I_ARRAY_V (ra1);
  ra2 = SCM_I_ARRAY_V (ra2);

  scm_array_get_handle (ra0, &ra0_handle);
  ra0_dims = scm_array_handle_dims (&ra0_handle);
  n = ra0_dims[0].ubnd - ra0_dims[0].lbnd + 1;
  inc0 = ra0_dims[0].inc;

  {
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if (scm_is_true (scm_array_handle_ref (&ra0_handle, i0)))
	if (!scm_is_eq (GVREF (ra1, i1), GVREF (ra2, i2)))
	  scm_array_handle_set (&ra0_handle, i0, SCM_BOOL_F);
  }

  scm_array_handle_release (&ra0_handle);
  return 1;
}

/* opt 0 means <, nonzero means >= */

static int
ra_compare (SCM ra0, SCM ra1, SCM ra2, int opt)
{
  scm_t_array_handle ra0_handle;
  scm_t_array_dim *ra0_dims;
  size_t n;
  ssize_t inc0;
  size_t i0 = 0;
  unsigned long i1 = SCM_I_ARRAY_BASE (ra1), i2 = SCM_I_ARRAY_BASE (ra2);
  long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra1 = SCM_I_ARRAY_V (ra1);
  ra2 = SCM_I_ARRAY_V (ra2);

  scm_array_get_handle (ra0, &ra0_handle);
  ra0_dims = scm_array_handle_dims (&ra0_handle);
  n = ra0_dims[0].ubnd - ra0_dims[0].lbnd + 1;
  inc0 = ra0_dims[0].inc;

  {
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if (scm_is_true (scm_array_handle_ref (&ra0_handle, i0)))
	if (opt ?
	    scm_is_true (scm_less_p (GVREF (ra1, i1), GVREF (ra2, i2))) :
	    scm_is_false (scm_less_p (GVREF (ra1, i1), GVREF (ra2, i2))))
	  scm_array_handle_set (&ra0_handle, i0, SCM_BOOL_F);
  }

  scm_array_handle_release (&ra0_handle);
  return 1;
}



int
scm_ra_lessp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (ras), SCM_CAR (SCM_CDR (ras)), 0);
}


int
scm_ra_leqp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (SCM_CDR (ras)), SCM_CAR (ras), 1);
}


int
scm_ra_grp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (SCM_CDR (ras)), SCM_CAR (ras), 0);
}


int
scm_ra_greqp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (ras), SCM_CAR (SCM_CDR (ras)), 1);
}


int
scm_ra_sum (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (!scm_is_null(ras))
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      GVSET (ra0, i0, scm_sum (GVREF(ra0, i0), GVREF(ra1, i1)));
	    break;
	  }
	}
    }
  return 1;
}



int
scm_ra_difference (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    {
      switch (SCM_TYP7 (ra0))
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0)
	      GVSET (ra0, i0, scm_difference (GVREF(ra0, i0), SCM_UNDEFINED));
	    break;
	  }
	}
    }
  else
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      GVSET (ra0, i0, scm_difference (GVREF (ra0, i0),
					      GVREF (ra1, i1)));
	    break;
	  }
	}
    }
  return 1;
}



int
scm_ra_product (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (!scm_is_null (ras))
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      GVSET (ra0, i0, scm_product (GVREF (ra0, i0),
					   GVREF (ra1, i1)));
	  }
	}
    }
  return 1;
}


int
scm_ra_divide (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    {
      switch (SCM_TYP7 (ra0))
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0)
	      GVSET (ra0, i0, scm_divide (GVREF (ra0, i0), SCM_UNDEFINED));
	    break;
	  }
	}
    }
  else
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      {
		SCM res =  scm_divide (GVREF (ra0, i0),
				       GVREF (ra1, i1));
		GVSET (ra0, i0, res);
	      }
	    break;
	  }
	}
    }
  return 1;
}


int
scm_array_identity (SCM dst, SCM src)
{
  return racp (SCM_CAR (src), scm_cons (dst, SCM_EOL));
}



static int 
ramap (SCM ra0, SCM proc, SCM ras)
{
  long i = SCM_I_ARRAY_DIMS (ra0)->lbnd;
  long inc = SCM_I_ARRAY_DIMS (ra0)->inc;
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd;
  long base = SCM_I_ARRAY_BASE (ra0) - i * inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    for (; i <= n; i++)
      GVSET (ra0, i*inc+base, scm_call_0 (proc));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      SCM args;
      unsigned long k, i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      ras = scm_vector (SCM_CDR (ras));
      
      for (; i <= n; i++, i1 += inc1)
	{
	  args = SCM_EOL;
	  for (k = scm_c_vector_length (ras); k--;)
	    args = scm_cons (GVREF (scm_c_vector_ref (ras, k), i), args);
	  args = scm_cons (GVREF (ra1, i1), args);
	  GVSET (ra0, i*inc+base, scm_apply_0 (proc, args));
	}
    }
  return 1;
}


SCM_REGISTER_PROC(s_array_map_in_order_x, "array-map-in-order!", 2, 0, 1, scm_array_map_x);

SCM_SYMBOL (sym_b, "b");

SCM_DEFINE (scm_array_map_x, "array-map!", 2, 0, 1,
	    (SCM ra0, SCM proc, SCM lra),
	    "@deffnx {Scheme Procedure} array-map-in-order! ra0 proc . lra\n"
	    "@var{array1}, @dots{} must have the same number of dimensions as\n"
	    "@var{array0} and have a range for each index which includes the range\n"
	    "for the corresponding index in @var{array0}.  @var{proc} is applied to\n"
	    "each tuple of elements of @var{array1} @dots{} and the result is stored\n"
	    "as the corresponding element in @var{array0}.  The value returned is\n"
	    "unspecified.  The order of application is unspecified.")
#define FUNC_NAME s_scm_array_map_x
{
  SCM_VALIDATE_PROC (2, proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);

  scm_ramapc (ramap, proc, ra0, lra, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static int
rafe (SCM ra0, SCM proc, SCM ras)
{
  long i = SCM_I_ARRAY_DIMS (ra0)->lbnd;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    for (; i <= n; i++, i0 += inc0)
      scm_call_1 (proc, GVREF (ra0, i0));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      SCM args;
      unsigned long k, i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      ras = scm_vector (SCM_CDR (ras));

      for (; i <= n; i++, i0 += inc0, i1 += inc1)
	{
	  args = SCM_EOL;
	  for (k = scm_c_vector_length (ras); k--;)
	    args = scm_cons (GVREF (scm_c_vector_ref (ras, k), i), args);
	  args = scm_cons2 (GVREF (ra0, i0), GVREF (ra1, i1), args);
	  scm_apply_0 (proc, args);
	}
    }
  return 1;
}


SCM_DEFINE (scm_array_for_each, "array-for-each", 2, 0, 1,
	    (SCM proc, SCM ra0, SCM lra),
	    "Apply @var{proc} to each tuple of elements of @var{array0} @dots{}\n"
	    "in row-major order.  The value returned is unspecified.")
#define FUNC_NAME s_scm_array_for_each
{
  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);
  scm_ramapc (rafe, proc, ra0, lra, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_array_index_map_x, "array-index-map!", 2, 0, 0,
	    (SCM ra, SCM proc),
	    "Apply @var{proc} to the indices of each element of @var{array} in\n"
	    "turn, storing the result in the corresponding element.  The value\n"
	    "returned and the order of application are unspecified.\n\n"
	    "One can implement @var{array-indexes} as\n"
	    "@lisp\n"
	    "(define (array-indexes array)\n"
	    "    (let ((ra (apply make-array #f (array-shape array))))\n"
	    "      (array-index-map! ra (lambda x x))\n"
	    "      ra))\n"
	    "@end lisp\n"
	    "Another example:\n"
	    "@lisp\n"
	    "(define (apl:index-generator n)\n"
	    "    (let ((v (make-uniform-vector n 1)))\n"
	    "      (array-index-map! v (lambda (i) i))\n"
	    "      v))\n"
	    "@end lisp")
#define FUNC_NAME s_scm_array_index_map_x
{
  unsigned long i;
  SCM_VALIDATE_PROC (2, proc);

  if (SCM_I_ARRAYP (ra))
    {
      SCM args = SCM_EOL;
      int j, k, kmax = SCM_I_ARRAY_NDIM (ra) - 1;
      long *vinds;

      if (kmax < 0)
	return scm_array_set_x (ra, scm_call_0 (proc), SCM_EOL);

      vinds = scm_gc_malloc_pointerless (sizeof(long) * SCM_I_ARRAY_NDIM (ra),
					 indices_gc_hint);

      for (k = 0; k <= kmax; k++)
	vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
      k = kmax;
      do
	{
	  if (k == kmax)
	    {
	      vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
	      i = cind (ra, vinds);
	      for (; vinds[k] <= SCM_I_ARRAY_DIMS (ra)[k].ubnd; vinds[k]++)
		{
		  for (j = kmax + 1, args = SCM_EOL; j--;)
		    args = scm_cons (scm_from_long (vinds[j]), args);
		  GVSET (SCM_I_ARRAY_V (ra), i, scm_apply_0 (proc, args));
		  i += SCM_I_ARRAY_DIMS (ra)[k].inc;
		}
	      k--;
	      continue;
	    }
	  if (vinds[k] < SCM_I_ARRAY_DIMS (ra)[k].ubnd)
	    {
	      vinds[k]++;
	      k++;
	      continue;
	    }
	  vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd - 1;
	  k--;
	}
      while (k >= 0);

      return SCM_UNSPECIFIED;
    }
  else if (scm_is_generalized_vector (ra))
    {
      size_t length = scm_c_generalized_vector_length (ra);
      for (i = 0; i < length; i++)
	GVSET (ra, i, scm_call_1 (proc, scm_from_ulong (i)));
      return SCM_UNSPECIFIED;
    }
  else 
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME


static int
array_compare (scm_t_array_handle *hx, scm_t_array_handle *hy,
               size_t dim, unsigned long posx, unsigned long posy)
{
  if (dim == scm_array_handle_rank (hx))
    return scm_is_true (scm_equal_p (scm_array_handle_ref (hx, posx),
                                     scm_array_handle_ref (hy, posy)));
  else
    {
      long incx, incy;
      size_t i;

      if (hx->dims[dim].lbnd != hy->dims[dim].lbnd
          || hx->dims[dim].ubnd != hy->dims[dim].ubnd)
        return 0;

      i = hx->dims[dim].ubnd - hx->dims[dim].lbnd + 1;
      
      incx = hx->dims[dim].inc;
      incy = hy->dims[dim].inc;
      posx += (i - 1) * incx;
      posy += (i - 1) * incy;

      for (; i > 0; i--, posx -= incx, posy -= incy)
        if (!array_compare (hx, hy, dim + 1, posx, posy))
          return 0;
      return 1;
    }
}

SCM
scm_array_equal_p (SCM x, SCM y)
{
  scm_t_array_handle hx, hy;
  SCM res;  
  
  scm_array_get_handle (x, &hx);
  scm_array_get_handle (y, &hy);
  
  res = scm_from_bool (hx.ndims == hy.ndims
                       && hx.element_type == hy.element_type);

  if (scm_is_true (res))
    res = scm_from_bool (array_compare (&hx, &hy, 0, 0, 0));

  scm_array_handle_release (&hy);
  scm_array_handle_release (&hx);

  return res;
}

static SCM scm_i_array_equal_p (SCM, SCM, SCM);
SCM_DEFINE (scm_i_array_equal_p, "array-equal?", 0, 2, 1,
            (SCM ra0, SCM ra1, SCM rest),
	    "Return @code{#t} iff all arguments are arrays with the same\n"
	    "shape, the same type, and have corresponding elements which are\n"
	    "either @code{equal?}  or @code{array-equal?}.  This function\n"
	    "differs from @code{equal?} in that all arguments must be arrays.")
#define FUNC_NAME s_scm_i_array_equal_p
{
  if (SCM_UNBNDP (ra0) || SCM_UNBNDP (ra1))
    return SCM_BOOL_T;
  
  while (!scm_is_null (rest))
    { if (scm_is_false (scm_array_equal_p (ra0, ra1)))
        return SCM_BOOL_F;
      ra0 = ra1;
      ra1 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_array_equal_p (ra0, ra1);
}
#undef FUNC_NAME


void
scm_init_array_map (void)
{
  scm_smobs[SCM_TC2SMOBNUM (scm_i_tc16_array)].equalp = scm_array_equal_p;
#include "libguile/array-map.x"
  scm_add_feature (s_scm_array_for_each);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
