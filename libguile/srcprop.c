/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002, 2006, 2008, 2009, 2010, 2011 Free Software Foundation
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

#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/smob.h"
#include "libguile/alist.h"
#include "libguile/debug.h"
#include "libguile/hashtab.h"
#include "libguile/hash.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/weaks.h"
#include "libguile/gc.h"

#include "libguile/validate.h"
#include "libguile/srcprop.h"
#include "libguile/private-options.h"


/* {Source Properties}
 *
 * Properties of source list expressions.
 * Four of these have special meaning:
 *
 * filename    string   The name of the source file.
 * copy        list     A copy of the list expression.
 * line	       integer	The source code line number.
 * column      integer	The source code column number.
 *
 * Most properties above can be set by the reader.
 *
 */

SCM_GLOBAL_SYMBOL (scm_sym_filename, "filename");
SCM_GLOBAL_SYMBOL (scm_sym_copy, "copy");
SCM_GLOBAL_SYMBOL (scm_sym_line, "line");
SCM_GLOBAL_SYMBOL (scm_sym_column, "column");

static SCM scm_source_whash;
static scm_i_pthread_mutex_t source_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;


/*
 *  Source properties are stored as double cells with the
 *  following layout:
  
 * car = tag
 * cbr = pos
 * ccr = copy
 * cdr = alist
 */

#define SRCPROPSP(p) (SCM_SMOB_PREDICATE (scm_tc16_srcprops, (p)))
#define SRCPROPPOS(p) (SCM_SMOB_DATA(p))
#define SRCPROPLINE(p) (SRCPROPPOS(p) >> 12)
#define SRCPROPCOL(p) (SRCPROPPOS(p) & 0x0fffL)
#define SRCPROPCOPY(p) (SCM_SMOB_OBJECT_2(p))
#define SRCPROPALIST(p) (SCM_SMOB_OBJECT_3(p))
#define SRCPROPMAKPOS(l, c) (((l) << 12) + (c))
#define SETSRCPROPPOS(p, l, c) (SCM_SET_SMOB_DATA_1 (p, SRCPROPMAKPOS (l, c)))
#define SETSRCPROPLINE(p, l) SETSRCPROPPOS (p, l, SRCPROPCOL (p))
#define SETSRCPROPCOL(p, c) SETSRCPROPPOS (p, SRCPROPLINE (p), c)
#define SETSRCPROPCOPY(p, c) (SCM_SET_SMOB_OBJECT_2 (p, c))
#define SETSRCPROPALIST(p, l) (SCM_SET_SMOB_OBJECT_3 (p, l))


static SCM scm_srcprops_to_alist (SCM obj);


scm_t_bits scm_tc16_srcprops;

static int
srcprops_print (SCM obj, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<srcprops ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (scm_srcprops_to_alist (obj), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return 1;
}


/*
 * We remember the last file name settings, so we can share that alist
 * entry.  This works because scm_set_source_property_x does not use
 * assoc-set! for modifying the alist.
 *
 * This variable contains a protected cons, whose cdr is the cached
 * alist
 */
static SCM scm_last_alist_filename;

SCM
scm_make_srcprops (long line, int col, SCM filename, SCM copy, SCM alist)
{
  if (!SCM_UNBNDP (filename))
    {
      SCM old_alist = alist;

      /*
	have to extract the acons, and operate on that, for
	thread safety.
       */
      SCM last_acons = SCM_CDR (scm_last_alist_filename);
      if (scm_is_null (old_alist)
	  && scm_is_eq (SCM_CDAR (last_acons), filename))
	{
	  alist = last_acons;
	}
      else
	{
	  alist = scm_acons (scm_sym_filename, filename, alist);
	  if (scm_is_null (old_alist))
	    SCM_SETCDR (scm_last_alist_filename, alist);
	}
    }
  
  SCM_RETURN_NEWSMOB3 (scm_tc16_srcprops,
		       SRCPROPMAKPOS (line, col),
		       SCM_UNPACK (copy),
		       SCM_UNPACK (alist));
}


static SCM
scm_srcprops_to_alist (SCM obj)
{
  SCM alist = SRCPROPALIST (obj);
  if (!SCM_UNBNDP (SRCPROPCOPY (obj)))
    alist = scm_acons (scm_sym_copy, SRCPROPCOPY (obj), alist);
  alist = scm_acons (scm_sym_column, scm_from_int (SRCPROPCOL (obj)), alist);
  alist = scm_acons (scm_sym_line, scm_from_int (SRCPROPLINE (obj)), alist);
  return alist;
}

SCM_DEFINE (scm_source_properties, "source-properties", 1, 0, 0, 
            (SCM obj),
	    "Return the source property association list of @var{obj}.")
#define FUNC_NAME s_scm_source_properties
{
  SCM p;
  SCM_VALIDATE_NIM (1, obj);

  scm_i_pthread_mutex_lock (&source_lock);
  p = scm_hashq_ref (scm_source_whash, obj, SCM_EOL); 
  scm_i_pthread_mutex_unlock (&source_lock);

  if (SRCPROPSP (p))
    return scm_srcprops_to_alist (p);
  else
    /* list from set-source-properties!, or SCM_EOL for not found */
    return p;
}
#undef FUNC_NAME

/* Perhaps this procedure should look through an alist
   and try to make a srcprops-object...? */
SCM_DEFINE (scm_set_source_properties_x, "set-source-properties!", 2, 0, 0,
            (SCM obj, SCM alist),
	    "Install the association list @var{alist} as the source property\n"
	    "list for @var{obj}.")
#define FUNC_NAME s_scm_set_source_properties_x
{
  SCM_VALIDATE_NIM (1, obj);

  scm_i_pthread_mutex_lock (&source_lock);
  scm_hashq_set_x (scm_source_whash, obj, alist);
  scm_i_pthread_mutex_unlock (&source_lock);

  return alist;
}
#undef FUNC_NAME

int
scm_i_has_source_properties (SCM obj)
#define FUNC_NAME "%set-source-properties"
{
  int ret;
  
  SCM_VALIDATE_NIM (1, obj);

  scm_i_pthread_mutex_lock (&source_lock);
  ret = scm_is_true (scm_hashq_ref (scm_source_whash, obj, SCM_BOOL_F));
  scm_i_pthread_mutex_unlock (&source_lock);

  return ret;
}
#undef FUNC_NAME
  

void
scm_i_set_source_properties_x (SCM obj, long line, int col, SCM fname)
#define FUNC_NAME "%set-source-properties"
{
  SCM_VALIDATE_NIM (1, obj);

  scm_i_pthread_mutex_lock (&source_lock);
  scm_hashq_set_x (scm_source_whash, obj,
                   scm_make_srcprops (line, col, fname,
                                      SCM_COPY_SOURCE_P
                                      ? scm_copy_tree (obj)
                                      : SCM_UNDEFINED,
                                      SCM_EOL));
  scm_i_pthread_mutex_unlock (&source_lock);
}
#undef FUNC_NAME

SCM_DEFINE (scm_source_property, "source-property", 2, 0, 0,
            (SCM obj, SCM key),
	    "Return the source property specified by @var{key} from\n"
	    "@var{obj}'s source property list.")
#define FUNC_NAME s_scm_source_property
{
  SCM p;
  SCM_VALIDATE_NIM (1, obj);

  scm_i_pthread_mutex_lock (&source_lock);
  p = scm_hashq_ref (scm_source_whash, obj, SCM_EOL);
  scm_i_pthread_mutex_unlock (&source_lock);

  if (!SRCPROPSP (p))
    goto alist;
  if (scm_is_eq (scm_sym_line, key))
    p = scm_from_int (SRCPROPLINE (p));
  else if (scm_is_eq (scm_sym_column, key))
    p = scm_from_int (SRCPROPCOL (p));
  else if (scm_is_eq (scm_sym_copy, key))
    p = SRCPROPCOPY (p);
  else
    {
      p = SRCPROPALIST (p);
    alist:
      p = scm_assoc (key, p);
      return (SCM_NIMP (p) ? SCM_CDR (p) : SCM_BOOL_F);
    }
  return SCM_UNBNDP (p) ? SCM_BOOL_F : p;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_source_property_x, "set-source-property!", 3, 0, 0,
            (SCM obj, SCM key, SCM datum),
	    "Set the source property of object @var{obj}, which is specified by\n"
	    "@var{key} to @var{datum}.  Normally, the key will be a symbol.")
#define FUNC_NAME s_scm_set_source_property_x
{
  SCM p;
  SCM_VALIDATE_NIM (1, obj);

  scm_i_pthread_mutex_lock (&source_lock);
  p = scm_hashq_ref (scm_source_whash, obj, SCM_EOL);

  if (scm_is_eq (scm_sym_line, key))
    {
      if (SRCPROPSP (p))
	SETSRCPROPLINE (p, scm_to_int (datum));
      else
	scm_hashq_set_x (scm_source_whash, obj,
                         scm_make_srcprops (scm_to_int (datum), 0,
                                            SCM_UNDEFINED, SCM_UNDEFINED, p));
    }
  else if (scm_is_eq (scm_sym_column, key))
    {
      if (SRCPROPSP (p))
	SETSRCPROPCOL (p, scm_to_int (datum));
      else
	scm_hashq_set_x (scm_source_whash, obj,
                         scm_make_srcprops (0, scm_to_int (datum),
                                            SCM_UNDEFINED, SCM_UNDEFINED, p));
    }
  else if (scm_is_eq (scm_sym_copy, key))
    {
      if (SRCPROPSP (p))
	SETSRCPROPCOPY (p, datum);
      else
	scm_hashq_set_x (scm_source_whash, obj,
                         scm_make_srcprops (0, 0, SCM_UNDEFINED, datum, p));
    }
  else
    {
      if (SRCPROPSP (p))
	SETSRCPROPALIST (p, scm_acons (key, datum, SRCPROPALIST (p)));
      else
	scm_hashq_set_x (scm_source_whash, obj,
                         scm_acons (key, datum, p));
    }
  scm_i_pthread_mutex_unlock (&source_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_cons_source, "cons-source", 3, 0, 0, 
            (SCM xorig, SCM x, SCM y),
	    "Create and return a new pair whose car and cdr are @var{x} and @var{y}.\n"
	    "Any source properties associated with @var{xorig} are also associated\n"
	    "with the new pair.")
#define FUNC_NAME s_scm_cons_source
{
  SCM p, z;
  z = scm_cons (x, y);
  scm_i_pthread_mutex_lock (&source_lock);
  /* Copy source properties possibly associated with xorig. */
  p = scm_hashq_ref (scm_source_whash, xorig, SCM_BOOL_F);
  if (scm_is_true (p))
    scm_hashq_set_x (scm_source_whash, z, p);
  scm_i_pthread_mutex_unlock (&source_lock);
  return z;
}
#undef FUNC_NAME


void
scm_init_srcprop ()
{
  scm_tc16_srcprops = scm_make_smob_type ("srcprops", 0);
  scm_set_smob_print (scm_tc16_srcprops, srcprops_print);

  scm_source_whash = scm_make_weak_key_hash_table (scm_from_int (2047));
  scm_c_define ("source-whash", scm_source_whash);

  scm_last_alist_filename = scm_cons (SCM_EOL,
				      scm_acons (SCM_EOL, SCM_EOL, SCM_EOL));

#include "libguile/srcprop.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
