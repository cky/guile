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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, Free Software Foundation gives permission
 * for additional uses of the text contained in its release of this library.
 *
 * The exception is that, if you link this library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking this library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by 
 * Free Software Foundation as part of this library.  If you copy
 * code from other releases distributed under the terms of the GPL into a copy of
 * this library, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from such code.
 *
 * If you write modifications of your own for this library, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

#include <stdio.h>
#include "_scm.h"

#include "weaks.h"



/* {Weak Vectors}
 */


SCM_PROC(s_make_weak_vector, "make-weak-vector", 1, 1, 0, scm_make_weak_vector);
#ifdef __STDC__
SCM
scm_make_weak_vector (SCM k, SCM fill)
#else
SCM
scm_make_weak_vector (k, fill)
     SCM k;
     SCM fill;
#endif
{
  SCM v;
  v = scm_make_vector (scm_sum (k, SCM_MAKINUM (1)), fill, SCM_UNDEFINED);
  SCM_DEFER_INTS;
  SCM_SETLENGTH(v, SCM_INUM (k), scm_tc7_wvect);
  SCM_VELTS(v)[0] = (SCM)0;
  SCM_SETVELTS(v, SCM_VELTS(v) + 1);
  SCM_ALLOW_INTS;
  return v;
}


SCM_PROC(s_weak_vector, "weak-vector", 0, 0, 1, scm_weak_vector);
SCM_PROC(s_list_to_weak_vector, "list->weak-vector", 1, 0, 0, scm_weak_vector);
#ifdef __STDC__
SCM
scm_weak_vector (SCM l)
#else
SCM
scm_weak_vector (l)
     SCM l;
#endif
{
  SCM res;
  register SCM *data;
  long i;

  i = scm_ilength (l);
  SCM_ASSERT (i >= 0, l, SCM_ARG1, s_weak_vector);
  res = scm_make_weak_vector (SCM_MAKINUM (i), SCM_UNSPECIFIED);
  data = SCM_VELTS (res);
  for (;
       i && SCM_NIMP (l) && SCM_CONSP (l);
       --i, l = SCM_CDR (l))
    *data++ = SCM_CAR (l);
  return res;
}


SCM_PROC(s_weak_vector_p, "weak-vector?", 1, 0, 0, scm_weak_vector_p);
#ifdef __STDC__
SCM
scm_weak_vector_p (SCM x)
#else
SCM
scm_weak_vector_p (x)
     SCM x;
#endif
{
  return ((SCM_NIMP (x) && SCM_WVECTP (x) && !SCM_IS_WHVEC (x))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}







SCM_PROC(s_make_weak_key_hash_table, "make-weak-key-hash-table", 1, 0, 0, scm_make_weak_key_hash_table);
#ifdef __STDC__
SCM
scm_make_weak_key_hash_table (SCM k)
#else
SCM
scm_make_weak_key_hash_table (k)
     SCM k;
#endif
{
  SCM v;
  SCM_ASSERT (SCM_INUMP (k), k, SCM_ARG1, s_make_weak_key_hash_table);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_ALLOW_INTS;
  SCM_VELTS (v)[-1] = 1;
  SCM_ALLOW_INTS;
  return v;
}


SCM_PROC (s_make_weak_value_hash_table, "make-weak-value-hash-table", 1, 0, 0, scm_make_weak_value_hash_table);
#ifdef __STDC__
SCM
scm_make_weak_value_hash_table (SCM k)
#else
SCM
scm_make_weak_value_hash_table (k)
     SCM k;
#endif
{
  SCM v;
  SCM_ASSERT (SCM_INUMP (k), k, SCM_ARG1, s_make_weak_value_hash_table);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_ALLOW_INTS;
  SCM_VELTS (v)[-1] = 2;
  SCM_ALLOW_INTS;
  return v;
}



SCM_PROC (s_make_doubly_weak_hash_table, "make-doubly-weak-hash-table", 1, 0, 0, scm_make_doubly_weak_hash_table);
#ifdef __STDC__
SCM
scm_make_doubly_weak_hash_table (SCM k)
#else
SCM
scm_make_doubly_weak_hash_table (k)
     SCM k;
#endif
{
  SCM v;
  SCM_ASSERT (SCM_INUMP (k), k, SCM_ARG1, s_make_doubly_weak_hash_table);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_ALLOW_INTS;
  SCM_VELTS (v)[-1] = 3;
  SCM_ALLOW_INTS;
  return v;
}

SCM_PROC(s_weak_key_hash_table_p, "weak-key-hash-table?", 1, 0, 0, scm_weak_key_hash_table_p);
#ifdef __STDC__
SCM
scm_weak_key_hash_table_p (SCM x)
#else
SCM
scm_weak_key_hash_table_p (x)
     SCM x;
#endif
{
  return ((SCM_NIMP (x) && SCM_WVECTP (x) && SCM_IS_WHVEC(x))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_PROC (s_weak_value_hash_table_p, "weak-value-hash-table?", 1, 0, 0, scm_weak_value_hash_table_p);
#ifdef __STDC__
SCM
scm_weak_value_hash_table_p (SCM x)
#else
SCM
scm_weak_value_hash_table_p (x)
     SCM x;
#endif
{
  return ((SCM_NIMP (x) && SCM_WVECTP (x) && SCM_IS_WHVEC_V(x))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_PROC (s_doubly_weak_hash_table_p, "doubly-weak-hash-table?", 1, 0, 0, scm_doubly_weak_hash_table_p);
#ifdef __STDC__
SCM
scm_doubly_weak_hash_table_p (SCM x)
#else
SCM
scm_doubly_weak_hash_table_p (x)
     SCM x;
#endif
{
  return ((SCM_NIMP (x) && SCM_WVECTP (x) && SCM_IS_WHVEC_B (x))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}




#ifdef __STDC__
void
scm_init_weaks (void)
#else
void
scm_init_weaks ()
#endif
{
#include "weaks.x"
}

