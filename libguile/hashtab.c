/*	Copyright (C) 1995, 1996, 1998, 1999 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "_scm.h"
#include "alist.h"
#include "hash.h"
#include "eval.h"

#include "scm_validate.h"
#include "hashtab.h"



SCM
scm_hash_fn_get_handle (SCM table,SCM obj,unsigned int (*hash_fn)(),SCM (*assoc_fn)(),void * closure)
{
  unsigned int k;
  SCM h;

  SCM_ASSERT (SCM_VECTORP (table), table, SCM_ARG1, "hash_fn_get_handle");
  if (SCM_LENGTH (table) == 0)
    return SCM_EOL;
  k = hash_fn (obj, SCM_LENGTH (table), closure);
  SCM_ASSERT ((0 <= k) && (k < SCM_LENGTH (table)),
	      scm_ulong2num (k),
	      SCM_OUTOFRANGE,
	      "hash_fn_get_handle");
  h = assoc_fn (obj, SCM_VELTS (table)[k], closure);
  return h;
}



SCM
scm_hash_fn_create_handle_x (SCM table,SCM obj,SCM init,unsigned int (*hash_fn)(),
                             SCM (*assoc_fn)(),void * closure)
{
  unsigned int k;
  SCM it;

  SCM_ASSERT (SCM_VECTORP (table), table, SCM_ARG1, "hash_fn_create_handle_x");
  if (SCM_LENGTH (table) == 0)
    return SCM_EOL;
  k = hash_fn (obj, SCM_LENGTH (table), closure);
  SCM_ASSERT ((0 <= k) && (k < SCM_LENGTH (table)),
	      scm_ulong2num (k),
	      SCM_OUTOFRANGE,
	      "hash_fn_create_handle_x");
  SCM_REDEFER_INTS;
  it = assoc_fn (obj, SCM_VELTS (table)[k], closure);
  if (SCM_NIMP (it))
    {
      return it;
    }
  {
    SCM new_bucket;
    SCM old_bucket;
    old_bucket = SCM_VELTS (table)[k];
    new_bucket = scm_acons (obj, init, old_bucket);
    SCM_VELTS(table)[k] = new_bucket;
    SCM_REALLOW_INTS;
    return SCM_CAR (new_bucket);
  }
}




SCM 
scm_hash_fn_ref (SCM table,SCM obj,SCM dflt,unsigned int (*hash_fn)(),
                 SCM (*assoc_fn)(),void * closure)
{
  SCM it;

  it = scm_hash_fn_get_handle (table, obj, hash_fn, assoc_fn, closure);
  if (SCM_IMP (it))
    return dflt;
  else
    return SCM_CDR (it);
}




SCM 
scm_hash_fn_set_x (SCM table,SCM obj,SCM val,unsigned int (*hash_fn)(),
                   SCM (*assoc_fn)(),void * closure)
{
  SCM it;

  it = scm_hash_fn_create_handle_x (table, obj, SCM_BOOL_F, hash_fn, assoc_fn, closure);
  SCM_SETCDR (it, val);
  return val;
}





SCM 
scm_hash_fn_remove_x (SCM table,SCM obj,unsigned int (*hash_fn)(),SCM (*assoc_fn)(),
                      SCM (*delete_fn)(),void * closure)
{
  unsigned int k;
  SCM h;

  SCM_ASSERT (SCM_VECTORP (table), table, SCM_ARG1, "hash_fn_remove_x");
  if (SCM_LENGTH (table) == 0)
    return SCM_EOL;
  k = hash_fn (obj, SCM_LENGTH (table), closure);
  SCM_ASSERT ((0 <= k) && (k < SCM_LENGTH (table)),
	      scm_ulong2num (k),
	      SCM_OUTOFRANGE,
	      "hash_fn_remove_x");
  h = assoc_fn (obj, SCM_VELTS (table)[k], closure);
  SCM_VELTS(table)[k] = delete_fn (h, SCM_VELTS(table)[k]);
  return h;
}




SCM_DEFINE (scm_hashq_get_handle, "hashq-get-handle", 2, 0, 0,
            (SCM table, SCM obj),
"@deffnx primitive hashv-get-handle table key
@deffnx primitive hash-get-handle table key
@deffnx primitive hashx-get-handle hasher assoc table key
These procedures are similar to their @code{-ref} cousins, but return a
@dfn{handle} from the hash table rather than the value associated with
@var{key}.  By convention, a handle in a hash table is the pair which
associates a key with a value.  Where @code{hashq-ref table key} returns
only a @code{value}, @code{hashq-get-handle table key} returns the pair
@code{(key . value)}.")
#define FUNC_NAME s_scm_hashq_get_handle
{
  return scm_hash_fn_get_handle (table, obj, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashq_create_handle_x, "hashq-create-handle!", 3, 0, 0,
            (SCM table, SCM obj, SCM init),
"@deffnx primitive hashv-create-handle! table key init
@deffnx primitive hash-create-handle! table key init
@deffnx primitive hashx-create-handle! hasher assoc table key init
These functions look up @var{key} in @var{table} and return its handle,
If @var{key} is not already present, a new handle is created which
associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hashq_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, obj, init, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashq_ref, "hashq-ref", 2, 1, 0,
            (SCM table, SCM obj, SCM dflt),
"@deffnx primitive hashv-ref table key [default]
@deffnx primitive hash-ref table key [default]
Look up @var{key} in the hash table @var{table}, and return the
value (if any) associated with it.  If @var{key} is not found,
return @var{default} (or @code{#f} if no @var{default} argument is
supplied).")
#define FUNC_NAME s_scm_hashq_ref
{
  if (dflt == SCM_UNDEFINED)
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, obj, dflt, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_set_x, "hashq-set!", 3, 0, 0,
            (SCM table, SCM obj, SCM val),
"@deffnx primitive hashv-set! table key value
@deffnx primitive hash-set! table key value
Find the entry in @var{table} associated with @var{key}, and store
@var{value} there.")
#define FUNC_NAME s_scm_hashq_set_x
{
  return scm_hash_fn_set_x (table, obj, val, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_remove_x, "hashq-remove!", 2, 0, 0,
            (SCM table, SCM obj),
"@deffnx primitive hashv-remove! table key
@deffnx primitive hash-remove! table key
Remove @var{key} (and any value associated with it) from @var{table}.")
#define FUNC_NAME s_scm_hashq_remove_x
{
  return scm_hash_fn_remove_x (table, obj, scm_ihashq, scm_sloppy_assq, scm_delq_x, 0);
}
#undef FUNC_NAME




SCM_DEFINE (scm_hashv_get_handle, "hashv-get-handle", 2, 0, 0,
            (SCM table, SCM obj),
"")
#define FUNC_NAME s_scm_hashv_get_handle
{
  return scm_hash_fn_get_handle (table, obj, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_create_handle_x, "hashv-create-handle!", 3, 0, 0,
            (SCM table, SCM obj, SCM init),
"")
#define FUNC_NAME s_scm_hashv_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, obj, init, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_ref, "hashv-ref", 2, 1, 0,
            (SCM table, SCM obj, SCM dflt),
"")
#define FUNC_NAME s_scm_hashv_ref
{
  if (dflt == SCM_UNDEFINED)
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, obj, dflt, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashv_set_x, "hashv-set!", 3, 0, 0,
            (SCM table, SCM obj, SCM val),
"")
#define FUNC_NAME s_scm_hashv_set_x
{
  return scm_hash_fn_set_x (table, obj, val, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_remove_x, "hashv-remove!", 2, 0, 0,
            (SCM table, SCM obj),
"")
#define FUNC_NAME s_scm_hashv_remove_x
{
  return scm_hash_fn_remove_x (table, obj, scm_ihashv, scm_sloppy_assv, scm_delv_x, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_get_handle, "hash-get-handle", 2, 0, 0,
            (SCM table, SCM obj),
"")
#define FUNC_NAME s_scm_hash_get_handle
{
  return scm_hash_fn_get_handle (table, obj, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hash_create_handle_x, "hash-create-handle!", 3, 0, 0,
            (SCM table, SCM obj, SCM init),
"")
#define FUNC_NAME s_scm_hash_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, obj, init, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hash_ref, "hash-ref", 2, 1, 0,
            (SCM table, SCM obj, SCM dflt),
"")
#define FUNC_NAME s_scm_hash_ref
{
  if (dflt == SCM_UNDEFINED)
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, obj, dflt, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_set_x, "hash-set!", 3, 0, 0,
            (SCM table, SCM obj, SCM val),
"")
#define FUNC_NAME s_scm_hash_set_x
{
  return scm_hash_fn_set_x (table, obj, val, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_remove_x, "hash-remove!", 2, 0, 0,
            (SCM table, SCM obj),
"")
#define FUNC_NAME s_scm_hash_remove_x
{
  return scm_hash_fn_remove_x (table, obj, scm_ihash, scm_sloppy_assoc, scm_delete_x, 0);
}
#undef FUNC_NAME




struct scm_ihashx_closure
{
  SCM hash;
  SCM assoc;
  SCM delete;
};



static unsigned int
scm_ihashx (SCM obj,unsigned int n,struct scm_ihashx_closure * closure)
{
  SCM answer;
  SCM_ALLOW_INTS;
  answer = scm_apply (closure->hash,
		      scm_listify (obj, scm_ulong2num ((unsigned long)n), SCM_UNDEFINED),
		      SCM_EOL);
  SCM_DEFER_INTS;
  return SCM_INUM (answer);
}



static SCM
scm_sloppy_assx (SCM obj,SCM alist,struct scm_ihashx_closure * closure)
{
  SCM answer;
  SCM_ALLOW_INTS;
  answer = scm_apply (closure->assoc,
		      scm_listify (obj, alist, SCM_UNDEFINED),
		      SCM_EOL);
  SCM_DEFER_INTS;
  return answer;
}




static SCM
scm_delx_x (SCM obj,SCM alist,struct scm_ihashx_closure * closure)
{
  SCM answer;
  SCM_ALLOW_INTS;
  answer = scm_apply (closure->delete,
		      scm_listify (obj, alist, SCM_UNDEFINED),
		      SCM_EOL);
  SCM_DEFER_INTS;
  return answer;
}



SCM_DEFINE (scm_hashx_get_handle, "hashx-get-handle", 4, 0, 0, 
            (SCM hash, SCM assoc, SCM table, SCM obj),
"")
#define FUNC_NAME s_scm_hashx_get_handle
{
  struct scm_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_get_handle (table, obj, scm_ihashx, scm_sloppy_assx, (void *)&closure);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashx_create_handle_x, "hashx-create-handle!", 5, 0, 0, 
            (SCM hash,SCM assoc,SCM table,SCM obj,SCM init),
"")
#define FUNC_NAME s_scm_hashx_create_handle_x
{
  struct scm_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_create_handle_x (table, obj, init, scm_ihashx, scm_sloppy_assx, (void *)&closure);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashx_ref, "hashx-ref", 4, 1, 0, 
            (SCM hash,SCM assoc,SCM table,SCM obj,SCM dflt),
"@deffnx primitive hashx-set! hasher assoc table key value
@deffnx primitive hashx-remove! hasher assoc table key
These behave the same way as the corresponding @code{ref} and
@code{set!} functions described above, but use @var{hasher} as a
hash function and @var{assoc} to compare keys.  @code{hasher} must
be a function that takes two arguments, a key to be hashed and a
table size.  @code{assoc} must be an associator function, like
@code{assoc}, @code{assq} or @code{assv}.

By way of illustration, @code{hashq-ref table key} is equivalent
to @code{hashx-ref hashq assq table key}.")
#define FUNC_NAME s_scm_hashx_ref
{
  struct scm_ihashx_closure closure;
  if (dflt == SCM_UNDEFINED)
    dflt = SCM_BOOL_F;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_ref (table, obj, dflt, scm_ihashx, scm_sloppy_assx, (void *)&closure);
}
#undef FUNC_NAME




SCM_DEFINE (scm_hashx_set_x, "hashx-set!", 5, 0, 0,
            (SCM hash, SCM assoc, SCM table, SCM obj, SCM val),
"")
#define FUNC_NAME s_scm_hashx_set_x
{
  struct scm_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_set_x (table, obj, val, scm_ihashx, scm_sloppy_assx, (void *)&closure);
}
#undef FUNC_NAME



SCM
scm_hashx_remove_x (SCM hash,SCM assoc,SCM delete,SCM table,SCM obj)
{
  struct scm_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  closure.delete = delete;
  return scm_hash_fn_remove_x (table, obj, scm_ihashx, scm_sloppy_assx, scm_delx_x, 0);
}

static SCM
fold_proc (void *proc, SCM key, SCM data, SCM value)
{
  return scm_apply ((SCM) proc, SCM_LIST3 (key, data, value), SCM_EOL);
}

SCM_DEFINE (scm_hash_fold, "hash-fold", 3, 0, 0, 
            (SCM proc, SCM init, SCM table),
"")
#define FUNC_NAME s_scm_hash_fold
{
  SCM_VALIDATE_PROC (1,proc);
  SCM_VALIDATE_VECTOR (3,table);
  return scm_internal_hash_fold (fold_proc, (void *) proc, init, table);
}
#undef FUNC_NAME

SCM
scm_internal_hash_fold (SCM (*fn) (), void *closure, SCM init, SCM table)
{
  int i, n = SCM_LENGTH (table);
  SCM result = init;
  for (i = 0; i < n; ++i)
    {
      SCM ls = SCM_VELTS (table)[i], handle;
      while (SCM_NNULLP (ls))
	{
	  SCM_ASSERT (SCM_CONSP (ls),
		      table, SCM_ARG1, s_scm_hash_fold);
	  handle = SCM_CAR (ls);
	  SCM_ASSERT (SCM_CONSP (handle),
		      table, SCM_ARG1, s_scm_hash_fold);
	  result = fn (closure, SCM_CAR (handle), SCM_CDR (handle), result);
	  ls = SCM_CDR (ls);
	}
    }
  return result;
}




void
scm_init_hashtab ()
{
#include "hashtab.x"
}
