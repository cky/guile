/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001 Free Software Foundation, Inc.
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


/* data initialization and C<->Scheme data conversion */

#include "libguile/gh.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

/* data conversion C->scheme */

SCM 
gh_bool2scm (int x)
{
  return SCM_BOOL(x);
}
SCM 
gh_int2scm (int x)
{
  return scm_long2num ((long) x);
}
SCM 
gh_ulong2scm (unsigned long x)
{
  return scm_ulong2num (x);
}
SCM 
gh_long2scm (long x)
{
  return scm_long2num (x);
}
SCM 
gh_double2scm (double x)
{
  return scm_make_real (x);
}
SCM 
gh_char2scm (char c)
{
 return SCM_MAKE_CHAR (c);
}
SCM 
gh_str2scm (const char *s, size_t len)
{
  return scm_mem2string (s, len);
}
SCM 
gh_str02scm (const char *s)
{
  return scm_makfrom0str (s);
}
/* Copy LEN characters at SRC into the *existing* Scheme string DST,
   starting at START.  START is an index into DST; zero means the
   beginning of the string.

   If START + LEN is off the end of DST, signal an out-of-range
   error.  */
void 
gh_set_substr (char *src, SCM dst, long start, size_t len)
{
  char *dst_ptr;
  size_t dst_len;

  SCM_ASSERT (SCM_STRINGP (dst), dst, SCM_ARG3, "gh_set_substr");

  dst_ptr = SCM_STRING_CHARS (dst);
  dst_len = SCM_STRING_LENGTH (dst);
  SCM_ASSERT (start + len <= dst_len, dst, SCM_ARG4, "gh_set_substr");
  
  memmove (dst_ptr + start, src, len);
  scm_remember_upto_here_1 (dst);
}

/* Return the symbol named SYMBOL_STR.  */
SCM 
gh_symbol2scm (const char *symbol_str)
{
  return scm_str2symbol(symbol_str);
}

SCM
gh_ints2scm (const int *d, long n)
{
  long i;
  SCM v = scm_c_make_vector (n, SCM_UNSPECIFIED);
  SCM *velts = SCM_VELTS(v);

  for (i = 0; i < n; ++i)
    velts[i] = (SCM_FIXABLE (d[i]) ? SCM_MAKINUM (d[i]) : scm_i_long2big (d[i]));

  return v;
}

SCM
gh_doubles2scm (const double *d, long n)
{
  long i;
  SCM v = scm_c_make_vector (n, SCM_UNSPECIFIED);
  SCM *velts = SCM_VELTS(v);

  for(i = 0; i < n; i++) 
    velts[i] = scm_make_real (d[i]);
  return v;
}

#ifdef HAVE_ARRAYS
/* Do not use this function for building normal Scheme vectors, unless
   you arrange for the elements to be protected from GC while you
   initialize the vector.  */
static SCM
makvect (char *m, size_t len, int type)
{
  return scm_alloc_cell (SCM_MAKE_UVECTOR_TAG (len, type), (scm_t_bits) m);
}

SCM
gh_chars2byvect (const char *d, long n)
{
  char *m = scm_gc_malloc (n * sizeof (char), "vector");
  memcpy (m, d, n * sizeof (char));
  return makvect (m, n, scm_tc7_byvect);
}

SCM
gh_shorts2svect (const short *d, long n)
{
  char *m = scm_gc_malloc (n * sizeof (short), "vector");
  memcpy (m, d, n * sizeof (short));
  return makvect (m, n, scm_tc7_svect);
}

SCM
gh_longs2ivect (const long *d, long n)
{
  char *m = scm_gc_malloc (n * sizeof (long), "vector");
  memcpy (m, d, n * sizeof (long));
  return makvect (m, n, scm_tc7_ivect);
}

SCM
gh_ulongs2uvect (const unsigned long *d, long n)
{
  char *m = scm_gc_malloc (n * sizeof (unsigned long), "vector");
  memcpy (m, d, n * sizeof (unsigned long));
  return makvect (m, n, scm_tc7_uvect);
}

SCM
gh_floats2fvect (const float *d, long n)
{
  char *m = scm_gc_malloc (n * sizeof (float), "vector");
  memcpy (m, d, n * sizeof (float));
  return makvect (m, n, scm_tc7_fvect);
}

SCM
gh_doubles2dvect (const double *d, long n)
{
  char *m = scm_gc_malloc (n * sizeof (double), "vector");
  memcpy (m, d, n * sizeof (double));
  return makvect (m, n, scm_tc7_dvect);
}
#endif

/* data conversion scheme->C */
int 
gh_scm2bool (SCM obj)
{
  return (SCM_FALSEP (obj)) ? 0 : 1;
}
unsigned long 
gh_scm2ulong (SCM obj)
{
  return scm_num2ulong (obj, SCM_ARG1, "gh_scm2ulong");
}
long 
gh_scm2long (SCM obj)
{
  return scm_num2long (obj, SCM_ARG1, "gh_scm2long");
}
int 
gh_scm2int (SCM obj)
{
  return (int) scm_num2int (obj, SCM_ARG1, "gh_scm2int");
}
double 
gh_scm2double (SCM obj)
{
  return scm_num2dbl (obj, "gh_scm2double");
}
char 
gh_scm2char (SCM obj)
#define FUNC_NAME "gh_scm2char"
{
  SCM_VALIDATE_CHAR (SCM_ARG1, obj);
  return SCM_CHAR (obj);
}
#undef FUNC_NAME

/* Convert a vector, weak vector, string, substring or uniform vector
   into an array of chars.  If result array in arg 2 is NULL, malloc a
   new one.  If out of memory, return NULL.  */
char *
gh_scm2chars (SCM obj, char *m)
{
  long i, n;
  long v;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
	    {
	      v = SCM_INUM (val);
	      if (v < -128 || v > 255)
		scm_out_of_range (0, obj);
	    }
	  else
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (char *) malloc (n * sizeof (char));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	m[i] = SCM_INUM (SCM_VELTS (obj)[i]);
      break;
#ifdef HAVE_ARRAYS
    case scm_tc7_byvect:
      n = SCM_UVECTOR_LENGTH (obj);
      if (m == 0)
	m = (char *) malloc (n * sizeof (char));
      if (m == NULL)
	return NULL;
      memcpy (m, SCM_VELTS (obj), n * sizeof (char));
      break;
#endif
    case scm_tc7_string:
      n = SCM_STRING_LENGTH (obj);
      if (m == 0)
	m = (char *) malloc (n * sizeof (char));
      if (m == NULL)
	return NULL;
      memcpy (m, SCM_VELTS (obj), n * sizeof (char));
      break;
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   shorts.  If result array in arg 2 is NULL, malloc a new one.  If
   out of memory, return NULL.  */
short *
gh_scm2shorts (SCM obj, short *m)
{
  long i, n;
  long v;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
	    {
	      v = SCM_INUM (val);
	      if (v < -32768 || v > 65535)
		scm_out_of_range (0, obj);
	    }
	  else
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (short *) malloc (n * sizeof (short));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	m[i] = SCM_INUM (SCM_VELTS (obj)[i]);
      break;
#ifdef HAVE_ARRAYS
    case scm_tc7_svect:
      n = SCM_UVECTOR_LENGTH (obj);
      if (m == 0)
	m = (short *) malloc (n * sizeof (short));
      if (m == NULL)
	return NULL;
      memcpy (m, SCM_VELTS (obj), n * sizeof (short));
      break;
#endif
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   longs.  If result array in arg 2 is NULL, malloc a new one.  If out
   of memory, return NULL.  */
long *
gh_scm2longs (SCM obj, long *m)
{
  long i, n;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (!SCM_INUMP (val) && !SCM_BIGP (val))
	    scm_wrong_type_arg (0, 0, obj);
	}
      if (m == 0)
	m = (long *) malloc (n * sizeof (long));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  m[i] = SCM_INUMP (val) 
	    ? SCM_INUM (val) 
	    : scm_num2long (val, 0, NULL);
	}
      break;
#ifdef HAVE_ARRAYS
    case scm_tc7_ivect:
    case scm_tc7_uvect:
      n = SCM_UVECTOR_LENGTH (obj);
      if (m == 0)
	m = (long *) malloc (n * sizeof (long));
      if (m == NULL)
	return NULL;
      memcpy (m, SCM_VELTS (obj), n * sizeof (long));
      break;
#endif
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   floats.  If result array in arg 2 is NULL, malloc a new one.  If
   out of memory, return NULL.  */
float *
gh_scm2floats (SCM obj, float *m)
{
  long i, n;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (!SCM_INUMP (val)
	      && !(SCM_BIGP (val) || SCM_REALP (val)))
	    scm_wrong_type_arg (0, 0, val);
	}
      if (m == 0)
	m = (float *) malloc (n * sizeof (float));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
	    m[i] = SCM_INUM (val);
	  else if (SCM_BIGP (val))
	    m[i] = scm_num2long (val, 0, NULL);
	  else
	    m[i] = SCM_REAL_VALUE (val);
	}
      break;
#ifdef HAVE_ARRAYS
    case scm_tc7_fvect:
      n = SCM_UVECTOR_LENGTH (obj);
      if (m == 0)
	m = (float *) malloc (n * sizeof (float));
      if (m == NULL)
	return NULL;
      memcpy (m, (float *) SCM_VELTS (obj), n * sizeof (float));
      break;

    case scm_tc7_dvect:
      n = SCM_UVECTOR_LENGTH (obj);
      if (m == 0)
	m = (float*) malloc (n * sizeof (float));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	m[i] = ((double *) SCM_VELTS (obj))[i];
      break;
#endif
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* Convert a vector, weak vector or uniform vector into an array of
   doubles.  If result array in arg 2 is NULL, malloc a new one.  If
   out of memory, return NULL.  */
double *
gh_scm2doubles (SCM obj, double *m)
{
  long i, n;
  SCM val;
  if (SCM_IMP (obj))
    scm_wrong_type_arg (0, 0, obj);
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_VECTOR_LENGTH (obj);
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (!SCM_INUMP (val)
	      && !(SCM_BIGP (val) || SCM_REALP (val)))
	    scm_wrong_type_arg (0, 0, val);
	}
      if (m == 0)
	m = (double *) malloc (n * sizeof (double));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	{
	  val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
	    m[i] = SCM_INUM (val);
	  else if (SCM_BIGP (val))
	    m[i] = scm_num2long (val, 0, NULL);
	  else
	    m[i] = SCM_REAL_VALUE (val);
	}
      break;
#ifdef HAVE_ARRAYS
    case scm_tc7_fvect:
      n = SCM_UVECTOR_LENGTH (obj);
      if (m == 0)
	m = (double *) malloc (n * sizeof (double));
      if (m == NULL)
	return NULL;
      for (i = 0; i < n; ++i)
	m[i] = ((float *) SCM_VELTS (obj))[i];
      break;

    case scm_tc7_dvect:
      n = SCM_UVECTOR_LENGTH (obj);
      if (m == 0)
	m = (double*) malloc (n * sizeof (double));
      if (m == NULL)
	return NULL;
      memcpy (m, SCM_VELTS (obj), n * sizeof (double));
      break;
#endif
    default:
      scm_wrong_type_arg (0, 0, obj);
    }
  return m;
}

/* string conversions between C and Scheme */

/* gh_scm2newstr() -- Given a Scheme string STR, return a pointer to a
   new copy of its contents, followed by a null byte.  If lenp is
   non-null, set *lenp to the string's length.

   This function uses malloc to obtain storage for the copy; the
   caller is responsible for freeing it.  If out of memory, NULL is
   returned.

   Note that Scheme strings may contain arbitrary data, including null
   characters.  This means that null termination is not a reliable way
   to determine the length of the returned value.  However, the
   function always copies the complete contents of STR, and sets
   *LEN_P to the true length of the string (when LEN_P is non-null).  */
char *
gh_scm2newstr (SCM str, size_t *lenp)
{
  char *ret_str;
  size_t len;

  SCM_ASSERT (SCM_STRINGP (str), str, SCM_ARG3, "gh_scm2newstr");

  len = SCM_STRING_LENGTH (str);

  ret_str = (char *) malloc ((len + 1) * sizeof (char));
  if (ret_str == NULL)
    return NULL;
  /* so we copy tmp_str to ret_str, which is what we will allocate */
  memcpy (ret_str, SCM_STRING_CHARS (str), len);
  scm_remember_upto_here_1 (str);
  /* now make sure we null-terminate it */
  ret_str[len] = '\0';

  if (lenp != NULL)
    {
      *lenp = len;
    }

  return ret_str;
}


/* Copy LEN characters at START from the Scheme string SRC to memory
   at DST.  START is an index into SRC; zero means the beginning of
   the string.  DST has already been allocated by the caller.

   If START + LEN is off the end of SRC, silently truncate the source
   region to fit the string.  If truncation occurs, the corresponding
   area of DST is left unchanged.  */
void 
gh_get_substr (SCM src, char *dst, long start, size_t len)
{
  size_t src_len, effective_length;
  SCM_ASSERT (SCM_STRINGP (src), src, SCM_ARG3, "gh_get_substr");

  src_len = SCM_STRING_LENGTH (src);
  effective_length = (len < src_len) ? len : src_len;
  memcpy (dst + start, SCM_STRING_CHARS (src), effective_length * sizeof (char));
  /* FIXME: must signal an error if len > src_len */
  scm_remember_upto_here_1 (src);
}


/* gh_scm2newsymbol() -- Given a Scheme symbol 'identifier, return a
   pointer to a string with the symbol characters "identifier",
   followed by a null byte.  If lenp is non-null, set *lenp to the
   string's length.

   This function uses malloc to obtain storage for the copy; the
   caller is responsible for freeing it.  If out of memory, NULL is
   returned.*/
char *
gh_symbol2newstr (SCM sym, size_t *lenp)
{
  char *ret_str;
  size_t len;

  SCM_ASSERT (SCM_SYMBOLP (sym), sym, SCM_ARG3, "gh_scm2newsymbol");

  len = SCM_SYMBOL_LENGTH (sym);

  ret_str = (char *) malloc ((len + 1) * sizeof (char));
  if (ret_str == NULL)
    return NULL;
  /* so we copy sym to ret_str, which is what we will allocate */
  memcpy (ret_str, SCM_SYMBOL_CHARS (sym), len);
  scm_remember_upto_here_1 (sym);
  /* now make sure we null-terminate it */
  ret_str[len] = '\0';

  if (lenp != NULL)
    {
      *lenp = len;
    }

  return ret_str;
}


/* create a new vector of the given length, all initialized to the
   given value */
SCM
gh_make_vector (SCM len, SCM fill)
{
  return scm_make_vector (len, fill);
}

/* set the given element of the given vector to the given value */
SCM 
gh_vector_set_x (SCM vec, SCM pos, SCM val)
{
  return scm_vector_set_x (vec, pos, val);
}

/* retrieve the given element of the given vector */
SCM 
gh_vector_ref (SCM vec, SCM pos)
{
  return scm_vector_ref (vec, pos);
}

/* returns the length of the given vector */
unsigned long 
gh_vector_length (SCM v)
{
  return (unsigned long) SCM_VECTOR_LENGTH (v);
}

#ifdef HAVE_ARRAYS
/* uniform vector support */

/* returns the length as a C unsigned long integer */
unsigned long
gh_uniform_vector_length (SCM v)
{
  return (unsigned long) SCM_UVECTOR_LENGTH (v);
}

/* gets the given element from a uniform vector; ilist is a list (or
   possibly a single integer) of indices, and its length is the
   dimension of the uniform vector */
SCM
gh_uniform_vector_ref (SCM v, SCM ilist)
{
  return scm_uniform_vector_ref (v, ilist);
}

/* sets an individual element in a uniform vector */
/* SCM */
/* gh_list_to_uniform_array ( */
#endif

/* Data lookups between C and Scheme

   Look up a symbol with a given name, and return the object to which
   it is bound.  gh_lookup examines the Guile top level, and
   gh_module_lookup checks the module namespace specified by the
   `vec' argument.

   The return value is the Scheme object to which SNAME is bound, or
   SCM_UNDEFINED if SNAME is not bound in the given context.
 */

SCM
gh_lookup (const char *sname)
{
  return gh_module_lookup (scm_current_module (), sname);
}


SCM
gh_module_lookup (SCM module, const char *sname)
#define FUNC_NAME "gh_module_lookup"
{
  SCM sym, var;

  SCM_VALIDATE_MODULE (SCM_ARG1, module);

  sym = scm_str2symbol (sname);
  var = scm_sym2var (sym, scm_module_lookup_closure (module), SCM_BOOL_F);
  if (var != SCM_BOOL_F)
    return SCM_VARIABLE_REF (var);
  else
    return SCM_UNDEFINED;
}
#undef FUNC_NAME

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
