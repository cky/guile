/* Copyright (C) 1995,1996,1998,1999,2000,2001 Free Software Foundation, Inc.
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




#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/hash.h"
#include "libguile/eval.h"
#include "libguile/root.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/hashtab.h"


SCM
scm_c_make_hash_table (unsigned long k)
{
  return scm_c_make_vector (k, SCM_EOL);
}


SCM
scm_hash_fn_get_handle (SCM table,SCM obj,unsigned long (*hash_fn)(),SCM (*assoc_fn)(),void * closure)
#define FUNC_NAME "scm_hash_fn_get_handle"
{
  unsigned long k;
  SCM h;

  SCM_VALIDATE_VECTOR (1, table);
  if (SCM_VECTOR_LENGTH (table) == 0)
    return SCM_BOOL_F;
  k = hash_fn (obj, SCM_VECTOR_LENGTH (table), closure);
  if (k >= SCM_VECTOR_LENGTH (table))
    scm_out_of_range ("hash_fn_get_handle", scm_ulong2num (k));
  h = assoc_fn (obj, SCM_VELTS (table)[k], closure);
  return h;
}
#undef FUNC_NAME


SCM
scm_hash_fn_create_handle_x (SCM table,SCM obj,SCM init,unsigned long (*hash_fn)(),
                             SCM (*assoc_fn)(),void * closure)
#define FUNC_NAME "scm_hash_fn_create_handle_x"
{
  unsigned long k;
  SCM it;

  SCM_ASSERT (SCM_VECTORP (table), table, SCM_ARG1, "hash_fn_create_handle_x");
  if (SCM_VECTOR_LENGTH (table) == 0)
    SCM_MISC_ERROR ("void hashtable", SCM_EOL);

  k = hash_fn (obj, SCM_VECTOR_LENGTH (table), closure);
  if (k >= SCM_VECTOR_LENGTH (table))
    scm_out_of_range ("hash_fn_create_handle_x", scm_ulong2num (k));
  SCM_REDEFER_INTS;
  it = assoc_fn (obj, SCM_VELTS (table)[k], closure);
  if (!SCM_FALSEP (it))
    {
      SCM_REALLOW_INTS;
      return it;
    }
  else
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
#undef FUNC_NAME


SCM 
scm_hash_fn_ref (SCM table,SCM obj,SCM dflt,unsigned long (*hash_fn)(),
                 SCM (*assoc_fn)(),void * closure)
{
  SCM it = scm_hash_fn_get_handle (table, obj, hash_fn, assoc_fn, closure);
  if (SCM_CONSP (it))
    return SCM_CDR (it);
  else
    return dflt;
}




SCM 
scm_hash_fn_set_x (SCM table,SCM obj,SCM val,unsigned long (*hash_fn)(),
                   SCM (*assoc_fn)(),void * closure)
{
  SCM it;

  it = scm_hash_fn_create_handle_x (table, obj, SCM_BOOL_F, hash_fn, assoc_fn, closure);
  SCM_SETCDR (it, val);
  return val;
}





SCM 
scm_hash_fn_remove_x (SCM table,SCM obj,unsigned long (*hash_fn)(),SCM (*assoc_fn)(),
                      SCM (*delete_fn)(),void * closure)
{
  unsigned long k;
  SCM h;

  SCM_ASSERT (SCM_VECTORP (table), table, SCM_ARG1, "hash_fn_remove_x");
  if (SCM_VECTOR_LENGTH (table) == 0)
    return SCM_EOL;
  k = hash_fn (obj, SCM_VECTOR_LENGTH (table), closure);
  if (k >= SCM_VECTOR_LENGTH (table))
    scm_out_of_range ("hash_fn_remove_x", scm_ulong2num (k));
  h = assoc_fn (obj, SCM_VELTS (table)[k], closure);
  SCM_VELTS(table)[k] = delete_fn (h, SCM_VELTS(table)[k]);
  return h;
}




SCM_DEFINE (scm_hashq_get_handle, "hashq-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_get_handle
{
  return scm_hash_fn_get_handle (table, key, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashq_create_handle_x, "hashq-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hashq_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashq_ref, "hashq-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{default} (or @code{#f} if no @var{default} argument\n"
	    "is supplied).  Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, key, dflt, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_set_x, "hashq-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{value} there. Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_set_x
{
  return scm_hash_fn_set_x (table, key, val, scm_ihashq, scm_sloppy_assq, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_remove_x, "hashq-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{eq?} for equality tests.")
#define FUNC_NAME s_scm_hashq_remove_x
{
  return scm_hash_fn_remove_x (table, key, scm_ihashq, scm_sloppy_assq,
			       scm_delq_x, 0);
}
#undef FUNC_NAME




SCM_DEFINE (scm_hashv_get_handle, "hashv-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_get_handle
{
  return scm_hash_fn_get_handle (table, key, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_create_handle_x, "hashv-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hashv_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init, scm_ihashv,
				      scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_ref, "hashv-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{default} (or @code{#f} if no @var{default} argument\n"
	    "is supplied).  Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, key, dflt, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashv_set_x, "hashv-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{value} there. Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_set_x
{
  return scm_hash_fn_set_x (table, key, val, scm_ihashv, scm_sloppy_assv, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_remove_x, "hashv-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{eqv?} for equality tests.")
#define FUNC_NAME s_scm_hashv_remove_x
{
  return scm_hash_fn_remove_x (table, key, scm_ihashv, scm_sloppy_assv,
			       scm_delv_x, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_get_handle, "hash-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{equal?} for equality testing.")
#define FUNC_NAME s_scm_hash_get_handle
{
  return scm_hash_fn_get_handle (table, key, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hash_create_handle_x, "hash-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hash_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hash_ref, "hash-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{default} (or @code{#f} if no @var{default} argument\n"
	    "is supplied).  Uses @code{equal?} for equality testing.")
#define FUNC_NAME s_scm_hash_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  return scm_hash_fn_ref (table, key, dflt, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_set_x, "hash-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{value} there. Uses @code{equal?} for equality\n"
	    "testing.")
#define FUNC_NAME s_scm_hash_set_x
{
  return scm_hash_fn_set_x (table, key, val, scm_ihash, scm_sloppy_assoc, 0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_remove_x, "hash-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{equal?} for equality tests.")
#define FUNC_NAME s_scm_hash_remove_x
{
  return scm_hash_fn_remove_x (table, key, scm_ihash, scm_sloppy_assoc,
			       scm_delete_x, 0);
}
#undef FUNC_NAME




typedef struct scm_t_ihashx_closure
{
  SCM hash;
  SCM assoc;
  SCM delete;
} scm_t_ihashx_closure;



static unsigned long
scm_ihashx (SCM obj, unsigned long n, scm_t_ihashx_closure *closure)
{
  SCM answer;
  SCM_DEFER_INTS;
  answer = scm_call_2 (closure->hash, obj, scm_ulong2num ((unsigned long) n));
  SCM_ALLOW_INTS;
  return SCM_INUM (answer);
}



static SCM
scm_sloppy_assx (SCM obj, SCM alist, scm_t_ihashx_closure *closure)
{
  SCM answer;
  SCM_DEFER_INTS;
  answer = scm_call_2 (closure->assoc, obj, alist);
  SCM_ALLOW_INTS;
  return answer;
}




static SCM
scm_delx_x (SCM obj, SCM alist, scm_t_ihashx_closure *closure)
{
  SCM answer;
  SCM_DEFER_INTS;
  answer = scm_call_2 (closure->delete, obj, alist);
  SCM_ALLOW_INTS;
  return answer;
}



SCM_DEFINE (scm_hashx_get_handle, "hashx-get-handle", 4, 0, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key),
	    "This behaves the same way as the corresponding\n"
	    "@code{-get-handle} function, but uses @var{hash} as a hash\n"
	    "function and @var{assoc} to compare keys.  @code{hash} must be\n"
	    "a function that takes two arguments, a key to be hashed and a\n"
	    "table size.  @code{assoc} must be an associator function, like\n"
	    "@code{assoc}, @code{assq} or @code{assv}.")
#define FUNC_NAME s_scm_hashx_get_handle
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_get_handle (table, key, scm_ihashx, scm_sloppy_assx,
				 (void *)&closure);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashx_create_handle_x, "hashx-create-handle!", 5, 0, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key, SCM init),
	    "This behaves the same way as the corresponding\n"
	    "@code{-create-handle} function, but uses @var{hash} as a hash\n"
	    "function and @var{assoc} to compare keys.  @code{hash} must be\n"
	    "a function that takes two arguments, a key to be hashed and a\n"
	    "table size.  @code{assoc} must be an associator function, like\n"
	    "@code{assoc}, @code{assq} or @code{assv}.")
#define FUNC_NAME s_scm_hashx_create_handle_x
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_create_handle_x (table, key, init, scm_ihashx,
				      scm_sloppy_assx, (void *)&closure);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashx_ref, "hashx-ref", 4, 1, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key, SCM dflt),
	    "This behaves the same way as the corresponding @code{ref}\n"
	    "function, but uses @var{hash} as a hash function and\n"
	    "@var{assoc} to compare keys.  @code{hash} must be a function\n"
	    "that takes two arguments, a key to be hashed and a table size.\n"
	    "@code{assoc} must be an associator function, like @code{assoc},\n"
	    "@code{assq} or @code{assv}.\n"
	    "\n"
	    "By way of illustration, @code{hashq-ref table key} is\n"
	    "equivalent to @code{hashx-ref hashq assq table key}.")
#define FUNC_NAME s_scm_hashx_ref
{
  scm_t_ihashx_closure closure;
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_ref (table, key, dflt, scm_ihashx, scm_sloppy_assx,
			  (void *)&closure);
}
#undef FUNC_NAME




SCM_DEFINE (scm_hashx_set_x, "hashx-set!", 5, 0, 0,
            (SCM hash, SCM assoc, SCM table, SCM key, SCM val),
	    "This behaves the same way as the corresponding @code{set!}\n"
	    "function, but uses @var{hash} as a hash function and\n"
	    "@var{assoc} to compare keys.  @code{hash} must be a function\n"
	    "that takes two arguments, a key to be hashed and a table size.\n"
	    "@code{assoc} must be an associator function, like @code{assoc},\n"
	    "@code{assq} or @code{assv}.\n"
	    "\n"
	    " By way of illustration, @code{hashq-set! table key} is\n"
	    "equivalent to @code{hashx-set!  hashq assq table key}.")
#define FUNC_NAME s_scm_hashx_set_x
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  return scm_hash_fn_set_x (table, key, val, scm_ihashx, scm_sloppy_assx,
			    (void *)&closure);
}
#undef FUNC_NAME



SCM
scm_hashx_remove_x (SCM hash, SCM assoc, SCM delete, SCM table, SCM obj)
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  closure.delete = delete;
  return scm_hash_fn_remove_x (table, obj, scm_ihashx, scm_sloppy_assx, scm_delx_x, 0);
}

static SCM
fold_proc (void *proc, SCM key, SCM data, SCM value)
{
  return scm_call_3 (SCM_PACK (proc), key, data, value);
}

SCM_DEFINE (scm_hash_fold, "hash-fold", 3, 0, 0, 
            (SCM proc, SCM init, SCM table),
	    "An iterator over hash-table elements.\n"
            "Accumulates and returns a result by applying PROC successively.\n"
            "The arguments to PROC are \"(key value prior-result)\" where key\n"
            "and value are successive pairs from the hash table TABLE, and\n"
            "prior-result is either INIT (for the first application of PROC)\n"
            "or the return value of the previous application of PROC.\n"
            "For example, @code{(hash-fold acons '() tab)} will convert a hash\n"
            "table into an a-list of key-value pairs.")
#define FUNC_NAME s_scm_hash_fold
{
  SCM_VALIDATE_PROC (1,proc);
  SCM_VALIDATE_VECTOR (3,table);
  return scm_internal_hash_fold (fold_proc, (void *) SCM_UNPACK (proc), init, table);
}
#undef FUNC_NAME

SCM
scm_internal_hash_fold (SCM (*fn) (), void *closure, SCM init, SCM table)
{
  long i, n = SCM_VECTOR_LENGTH (table);
  SCM result = init;
  for (i = 0; i < n; ++i)
    {
      SCM ls = SCM_VELTS (table)[i], handle;
      while (!SCM_NULLP (ls))
	{
	  SCM_ASSERT (SCM_CONSP (ls),
		      table, SCM_ARG3, s_scm_hash_fold);
	  handle = SCM_CAR (ls);
	  SCM_ASSERT (SCM_CONSP (handle),
		      table, SCM_ARG3, s_scm_hash_fold);
	  result = fn (closure, SCM_CAR (handle), SCM_CDR (handle), result);
	  ls = SCM_CDR (ls);
	}
    }
  return result;
}




void
scm_init_hashtab ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/hashtab.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
