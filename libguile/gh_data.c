/*      Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

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

#include <stdio.h>

#include <gh.h>

/* data conversion C->scheme */
SCM 
gh_int2scmb (int x)		/* this is being phased out */
{
  return (x ? SCM_BOOL_T : SCM_BOOL_F);
}
SCM 
gh_bool2scm (int x)
{
  return (x ? SCM_BOOL_T : SCM_BOOL_F);
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
  return scm_makdbl (x, 0.0);
}
SCM 
gh_char2scm (char c)
{
  return SCM_MAKICHR (c);
}
SCM 
gh_str2scm (char *s, int len)
{
  return scm_makfromstr (s, len, 0);
}
SCM 
gh_str02scm (char *s)
{
  return scm_makfrom0str (s);
}
/* Copy LEN characters at SRC into the *existing* Scheme string DST,
   starting at START.  START is an index into DST; zero means the
   beginning of the string.

   If START + LEN is off the end of DST, signal an out-of-range
   error.  */
void 
gh_set_substr (char *src, SCM dst, int start, int len)
{
  char *dst_ptr, dst_len, effective_length;

  SCM_ASSERT (SCM_NIMP (dst) && SCM_STRINGP (dst), dst, SCM_ARG3,
	      "gh_set_substr");
  scm_protect_object (dst);
  dst_ptr = SCM_CHARS (dst);
  dst_len = SCM_LENGTH (dst);
  effective_length = (len < dst_len) ? len : dst_len;
  memcpy (dst_ptr + start, src, effective_length);
  /* FIXME: must signal an error if len > dst_len */
  scm_unprotect_object (dst);
}

/* Return the symbol named SYMBOL_STR.  */
SCM 
gh_symbol2scm (char *symbol_str)
{
  return SCM_CAR (scm_intern (symbol_str, strlen (symbol_str)));
}


/* data conversion scheme->C */
int 
gh_scm2bool (SCM obj)
{
  return ((obj) == SCM_BOOL_F) ? 0 : 1;
}
unsigned long 
gh_scm2ulong (SCM obj)
{
  return scm_num2ulong (obj, (char *) SCM_ARG1, "gh_scm2ulong");
}
long 
gh_scm2long (SCM obj)
{
  return scm_num2long (obj, (char *) SCM_ARG1, "gh_scm2long");
}
int 
gh_scm2int (SCM obj)
{
  /* NOTE: possible loss of precision here */
  return (int) scm_num2long (obj, (char *) SCM_ARG1, "gh_scm2int");
}
double 
gh_scm2double (SCM obj)
{
  return scm_num2dbl (obj, "gh_scm2double");
}
char 
gh_scm2char (SCM obj)
{
  return SCM_ICHR (obj);
}

/* string conversions between C and Scheme */

/* gh_scm2newstr() -- Given a Scheme string STR, return a pointer to a
   new copy of its contents, followed by a null byte.  If lenp is
   non-null, set *lenp to the string's length.

   This function uses malloc to obtain storage for the copy; the
   caller is responsible for freeing it.

   Note that Scheme strings may contain arbitrary data, including null
   characters.  This means that null termination is not a reliable way
   to determine the length of the returned value.  However, the
   function always copies the complete contents of STR, and sets
   *LEN_P to the true length of the string (when LEN_P is non-null).  */
char *
gh_scm2newstr (SCM str, int *lenp)
{
  char *ret_str;
  int len;

  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str), str, SCM_ARG3,
	      "gh_scm2newstr");

  /* protect str from GC while we copy off its data */
  scm_protect_object (str);

  len = SCM_LENGTH (str);

  ret_str = (char *) scm_must_malloc ((len + 1) * sizeof (char),
				      "gh_scm2newstr");
  /* so we copy tmp_str to ret_str, which is what we will allocate */
  memcpy (ret_str, SCM_CHARS (str), len);
  /* now make sure we null-terminate it */
  ret_str[len] = '\0';

  scm_unprotect_object (str);

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
gh_get_substr (SCM src, char *dst, int start, int len)
{
  int src_len, effective_length;
  SCM_ASSERT (SCM_NIMP (src) && SCM_STRINGP (src), src, SCM_ARG3,
	      "gh_get_substr");

  scm_protect_object (src);
  src_len = SCM_LENGTH (src);
  effective_length = (len < src_len) ? len : src_len;
  memcpy (dst + start, SCM_CHARS (src), effective_length * sizeof (char));
  /* FIXME: must signal an error if len > src_len */
  scm_unprotect_object (src);
}


/* gh_scm2newsymbol() -- Given a Scheme symbol 'identifier, return a
   pointer to a string with the symbol characters "identifier",
   followed by a null byte.  If lenp is non-null, set *lenp to the
   string's length.

   This function uses malloc to obtain storage for the copy; the
   caller is responsible for freeing it. */
char *
gh_symbol2newstr (SCM sym, int *lenp)
{
  char *ret_str;
  int len;

  SCM_ASSERT (SCM_NIMP (sym) && SCM_SYMBOLP (sym), sym, SCM_ARG3,
	      "gh_scm2newsymbol");

  /* protect str from GC while we copy off its data */
  scm_protect_object (sym);

  len = SCM_LENGTH (sym);

  ret_str = (char *) scm_must_malloc ((len + 1) * sizeof (char),
				      "gh_symbol2newstr");
  /* so we copy tmp_str to ret_str, which is what we will allocate */
  memcpy (ret_str, SCM_CHARS (sym), len);
  /* now make sure we null-terminate it */
  ret_str[len] = '\0';

  scm_unprotect_object (sym);

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
  /* scm_make_vector() takes a third boolean argument which should be
     set to SCM_BOOL_T when you are dealing with multi-dimensional
     arrays; gh_make_vector() does not do multi-dimensional arrays */
  return scm_make_vector(len, fill, SCM_BOOL_F);
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
  return gh_scm2ulong (scm_vector_length (v));
}

/* Data lookups between C and Scheme

   Look up a symbol with a given name, and return the object to which
   it is bound.  gh_lookup examines the Guile top level, and
   gh_module_lookup checks the module namespace specified by the
   `vec' argument.

   The return value is the Scheme object to which SNAME is bound, or
   SCM_UNDEFINED if SNAME is not bound in the given context. [FIXME:
   should this be SCM_UNSPECIFIED?  Can a symbol ever legitimately be
   bound to SCM_UNDEFINED or SCM_UNSPECIFIED?  What is the difference?
   -twp] */

SCM
gh_lookup (char *sname)
{
  return gh_module_lookup (SCM_BOOL_F, sname);
}

SCM
gh_module_lookup (SCM vec, char *sname)
{
  SCM sym = gh_symbol2scm (sname);
  if ((scm_symbol_bound_p (vec, sym)) == SCM_BOOL_T)
    return scm_symbol_binding (vec, sym);
  else
    return SCM_UNDEFINED;
}
