/*	Copyright (C) 1995,1996,1998 Free Software Foundation, Inc.
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
 * If you do not wish that, delete this exception notice.  */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include <stdio.h>
#include "_scm.h"

#include "scm_validate.h"
#include "weaks.h"



/* {Weak Vectors}
 */


SCM_DEFINE (scm_make_weak_vector, "make-weak-vector", 1, 1, 0,
           (SCM k, SCM fill),
"Return a weak vector with @var{size} elements. If the optional
argument @var{fill} is given, all entries in the vector will be set to
@var{fill}. The default value for @var{fill} is the empty list.")
#define FUNC_NAME s_scm_make_weak_vector
{
  SCM v;
  v = scm_make_vector (scm_sum (k, SCM_MAKINUM (2)), fill);
  SCM_DEFER_INTS;
  SCM_SETLENGTH(v, SCM_INUM (k), scm_tc7_wvect);
  SCM_VELTS(v)[0] = SCM_EOL;
  SCM_VELTS(v)[1] = (SCM)0;
  SCM_SETVELTS(v, SCM_VELTS(v) + 2);
  SCM_ALLOW_INTS;
  return v;
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_to_weak_vector, "list->weak-vector", 1, 0, 0, scm_weak_vector);

SCM_DEFINE (scm_weak_vector, "weak-vector", 0, 0, 1, 
           (SCM l),
"@deffnx primitive list->weak-vector l
Construct a weak vector from a list: @code{weak-vector} uses the list of
its arguments while @code{list->weak-vector} uses its only argument
@var{l} (a list) to construct a weak vector the same way
@code{vector->list} would.")
#define FUNC_NAME s_scm_weak_vector
{
  SCM res;
  register SCM *data;
  long i;

  i = scm_ilength (l);
  SCM_ASSERT (i >= 0, l, SCM_ARG1, FUNC_NAME);
  res = scm_make_weak_vector (SCM_MAKINUM (i), SCM_UNSPECIFIED);
  data = SCM_VELTS (res);
  for (;
       i && SCM_CONSP (l);
       --i, l = SCM_CDR (l))
    *data++ = SCM_CAR (l);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_vector_p, "weak-vector?", 1, 0, 0, 
           (SCM x),
"Return @var{#t} if @var{obj} is a weak vector. Note that all weak
hashes are also weak vectors.")
#define FUNC_NAME s_scm_weak_vector_p
{
  return SCM_BOOL(SCM_WVECTP (x) && !SCM_IS_WHVEC (x));
}
#undef FUNC_NAME







SCM_DEFINE (scm_make_weak_key_hash_table, "make-weak-key-hash-table", 1, 0, 0, 
           (SCM k),
"@deffnx primitive make-weak-value-hash-table size
@deffnx primitive make-doubly-weak-hash-table size
Return a weak hash table with @var{size} buckets. As with any hash
table, choosing a good size for the table requires some caution.

You can modify weak hash tables in exactly the same way you would modify
regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_hash_table
{
  SCM v;
  SCM_VALIDATE_INUM (1,k);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_ALLOW_INTS;
  SCM_VELTS (v)[-1] = 1;
  SCM_ALLOW_INTS;
  return v;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_hash_table, "make-weak-value-hash-table", 1, 0, 0, 
            (SCM k),
"")
#define FUNC_NAME s_scm_make_weak_value_hash_table
{
  SCM v;
  SCM_VALIDATE_INUM (1,k);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_ALLOW_INTS;
  SCM_VELTS (v)[-1] = 2;
  SCM_ALLOW_INTS;
  return v;
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_doubly_weak_hash_table, "make-doubly-weak-hash-table", 1, 0, 0, 
            (SCM k),
"")
#define FUNC_NAME s_scm_make_doubly_weak_hash_table
{
  SCM v;
  SCM_VALIDATE_INUM (1,k);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_ALLOW_INTS;
  SCM_VELTS (v)[-1] = 3;
  SCM_ALLOW_INTS;
  return v;
}
#undef FUNC_NAME

SCM_DEFINE (scm_weak_key_hash_table_p, "weak-key-hash-table?", 1, 0, 0, 
           (SCM x),
"@deffnx primitive weak-value-hash-table? obj
@deffnx primitive doubly-weak-hash-table? obj
Return @var{#t} if @var{obj} is the specified weak hash table. Note
that a doubly weak hash table is neither a weak key nor a weak value
hash table.")
#define FUNC_NAME s_scm_weak_key_hash_table_p
{
  return SCM_BOOL(SCM_WVECTP (x) && SCM_IS_WHVEC(x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_hash_table_p, "weak-value-hash-table?", 1, 0, 0, 
            (SCM x),
"")
#define FUNC_NAME s_scm_weak_value_hash_table_p
{
  return SCM_BOOL(SCM_WVECTP (x) && SCM_IS_WHVEC_V(x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_hash_table_p, "doubly-weak-hash-table?", 1, 0, 0, 
            (SCM x),
"")
#define FUNC_NAME s_scm_doubly_weak_hash_table_p
{
  return SCM_BOOL(SCM_WVECTP (x) && SCM_IS_WHVEC_B (x));
}
#undef FUNC_NAME





void
scm_init_weaks ()
{
#include "weaks.x"
}

