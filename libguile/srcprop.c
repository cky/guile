/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002, 2006 Free Software Foundation
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */




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

#include "libguile/validate.h"
#include "libguile/srcprop.h"

/* {Source Properties}
 *
 * Properties of source list expressions.
 * Five of these have special meaning and optimized storage:
 *
 * filename    string   The name of the source file.
 * copy        list     A copy of the list expression.
 * line	       integer	The source code line number.
 * column      integer	The source code column number.
 * breakpoint  boolean	Sets a breakpoint on this form.
 *
 * Most properties above can be set by the reader.
 *
 */

SCM_GLOBAL_SYMBOL (scm_sym_filename, "filename");
SCM_GLOBAL_SYMBOL (scm_sym_copy, "copy");
SCM_GLOBAL_SYMBOL (scm_sym_line, "line");
SCM_GLOBAL_SYMBOL (scm_sym_column, "column");
SCM_GLOBAL_SYMBOL (scm_sym_breakpoint, "breakpoint");

scm_t_bits scm_tc16_srcprops;
static scm_t_srcprops_chunk *srcprops_chunklist = 0;
static scm_t_srcprops *srcprops_freelist = 0;


static SCM
srcprops_mark (SCM obj)
{
  scm_gc_mark (SRCPROPFNAME (obj));
  scm_gc_mark (SRCPROPCOPY (obj));
  return SRCPROPPLIST (obj);
}


static size_t
srcprops_free (SCM obj)
{
  *((scm_t_srcprops **) SCM_SMOB_DATA (obj)) = srcprops_freelist;
  srcprops_freelist = (scm_t_srcprops *) SCM_SMOB_DATA (obj);
  return 0; /* srcprops_chunks are not freed until leaving guile */
}


static int
srcprops_print (SCM obj, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<srcprops ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (scm_srcprops_to_plist (obj), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return 1;
}


int
scm_c_source_property_breakpoint_p (SCM form)
{
  SCM obj = scm_whash_lookup (scm_source_whash, form);
  return SRCPROPSP (obj) && SRCPROPBRK (obj);
}


SCM
scm_make_srcprops (long line, int col, SCM filename, SCM copy, SCM plist)
{
  register scm_t_srcprops *ptr;
  SCM_CRITICAL_SECTION_START;
  if ((ptr = srcprops_freelist) != NULL)
    srcprops_freelist = *(scm_t_srcprops **)ptr;
  else
    {
      size_t i;
      scm_t_srcprops_chunk *mem;
      size_t n = sizeof (scm_t_srcprops_chunk)
	            + sizeof (scm_t_srcprops) * (SRCPROPS_CHUNKSIZE - 1);
      SCM_SYSCALL (mem = (scm_t_srcprops_chunk *) scm_malloc (n));
      if (mem == NULL)
	scm_memory_error ("srcprops");
      scm_gc_register_collectable_memory (mem, n, "srcprops");
      
      mem->next = srcprops_chunklist;
      srcprops_chunklist = mem;
      ptr = &mem->srcprops[0];
      for (i = 1; i < SRCPROPS_CHUNKSIZE - 1; ++i)
	*(scm_t_srcprops **)&ptr[i] = &ptr[i + 1];
      *(scm_t_srcprops **)&ptr[SRCPROPS_CHUNKSIZE - 1] = 0;
      srcprops_freelist = (scm_t_srcprops *) &ptr[1];
    }
  ptr->pos = SRCPROPMAKPOS (line, col);
  ptr->fname = filename;
  ptr->copy = copy;
  ptr->plist = plist;
  SCM_CRITICAL_SECTION_END;
  SCM_RETURN_NEWSMOB (scm_tc16_srcprops, ptr);
}


SCM
scm_srcprops_to_plist (SCM obj)
{
  SCM plist = SRCPROPPLIST (obj);
  if (!SCM_UNBNDP (SRCPROPCOPY (obj)))
    plist = scm_acons (scm_sym_copy, SRCPROPCOPY (obj), plist);
  if (!SCM_UNBNDP (SRCPROPFNAME (obj)))
    plist = scm_acons (scm_sym_filename, SRCPROPFNAME (obj), plist);
  plist = scm_acons (scm_sym_column, scm_from_int (SRCPROPCOL (obj)), plist);
  plist = scm_acons (scm_sym_line, scm_from_int (SRCPROPLINE (obj)), plist);
  plist = scm_acons (scm_sym_breakpoint, scm_from_bool (SRCPROPBRK (obj)), plist);
  return plist;
}

SCM_DEFINE (scm_source_properties, "source-properties", 1, 0, 0, 
            (SCM obj),
	    "Return the source property association list of @var{obj}.")
#define FUNC_NAME s_scm_source_properties
{
  SCM p;
  SCM_VALIDATE_NIM (1, obj);
  if (SCM_MEMOIZEDP (obj))
    obj = SCM_MEMOIZED_EXP (obj);
  else if (!scm_is_pair (obj))
    SCM_WRONG_TYPE_ARG (1, obj);
  p = scm_hashq_ref (scm_source_whash, obj, SCM_EOL);
  if (SRCPROPSP (p))
    return scm_srcprops_to_plist (p);
  else
    /* list from set-source-properties!, or SCM_EOL for not found */
    return p;
}
#undef FUNC_NAME

/* Perhaps this procedure should look through an alist
   and try to make a srcprops-object...? */
SCM_DEFINE (scm_set_source_properties_x, "set-source-properties!", 2, 0, 0,
            (SCM obj, SCM plist),
	    "Install the association list @var{plist} as the source property\n"
	    "list for @var{obj}.")
#define FUNC_NAME s_scm_set_source_properties_x
{
  SCM handle;
  SCM_VALIDATE_NIM (1, obj);
  if (SCM_MEMOIZEDP (obj))
    obj = SCM_MEMOIZED_EXP (obj);
  else if (!scm_is_pair (obj))
    SCM_WRONG_TYPE_ARG(1, obj);
  handle = scm_hashq_create_handle_x (scm_source_whash, obj, plist);
  SCM_SETCDR (handle, plist);
  return plist;
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
  if (SCM_MEMOIZEDP (obj))
    obj = SCM_MEMOIZED_EXP (obj);
  else if (!scm_is_pair (obj))
    SCM_WRONG_TYPE_ARG (1, obj);
  p = scm_hashq_ref (scm_source_whash, obj, SCM_EOL);
  if (!SRCPROPSP (p))
    goto plist;
  if      (scm_is_eq (scm_sym_breakpoint, key)) p = scm_from_bool (SRCPROPBRK (p));
  else if (scm_is_eq (scm_sym_line,       key)) p = scm_from_int (SRCPROPLINE (p));
  else if (scm_is_eq (scm_sym_column,     key)) p = scm_from_int (SRCPROPCOL (p));
  else if (scm_is_eq (scm_sym_filename,   key)) p = SRCPROPFNAME (p);
  else if (scm_is_eq (scm_sym_copy,       key)) p = SRCPROPCOPY (p);
  else
    {
      p = SRCPROPPLIST (p);
    plist:
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
  scm_whash_handle h;
  SCM p;
  SCM_VALIDATE_NIM (1, obj);
  if (SCM_MEMOIZEDP (obj))
    obj = SCM_MEMOIZED_EXP (obj);
  else if (!scm_is_pair (obj))
    SCM_WRONG_TYPE_ARG (1, obj);
  h = scm_whash_get_handle (scm_source_whash, obj);
  if (SCM_WHASHFOUNDP (h))
    p = SCM_WHASHREF (scm_source_whash, h);
  else
    {
      h = scm_whash_create_handle (scm_source_whash, obj);
      p = SCM_EOL;
    }
  if (scm_is_eq (scm_sym_breakpoint, key))
    {
      if (SRCPROPSP (p))
	{
	  if (scm_is_false (datum))
	    CLEARSRCPROPBRK (p);
	  else
	    SETSRCPROPBRK (p);
	}
      else
	{
	  SCM sp = scm_make_srcprops (0, 0, SCM_UNDEFINED, SCM_UNDEFINED, p);
	  SCM_WHASHSET (scm_source_whash, h, sp);
	  if (scm_is_false (datum))
	    CLEARSRCPROPBRK (sp);
	  else
	    SETSRCPROPBRK (sp);
	}
    }
  else if (scm_is_eq (scm_sym_line, key))
    {
      if (SRCPROPSP (p))
	SETSRCPROPLINE (p, scm_to_int (datum));
      else
	SCM_WHASHSET (scm_source_whash, h,
		      scm_make_srcprops (scm_to_int (datum), 0,
					 SCM_UNDEFINED, SCM_UNDEFINED, p));
    }
  else if (scm_is_eq (scm_sym_column, key))
    {
      if (SRCPROPSP (p))
	SETSRCPROPCOL (p, scm_to_int (datum));
      else
	SCM_WHASHSET (scm_source_whash, h,
		      scm_make_srcprops (0, scm_to_int (datum),
					 SCM_UNDEFINED, SCM_UNDEFINED, p));
    }
  else if (scm_is_eq (scm_sym_filename, key))
    {
      if (SRCPROPSP (p))
	SRCPROPFNAME (p) = datum;
      else
	SCM_WHASHSET (scm_source_whash, h, scm_make_srcprops (0, 0, datum, SCM_UNDEFINED, p));
    }
  else if (scm_is_eq (scm_sym_copy, key))
    {
      if (SRCPROPSP (p))
	SRCPROPCOPY (p) = datum;
      else
	SCM_WHASHSET (scm_source_whash, h, scm_make_srcprops (0, 0, SCM_UNDEFINED, datum, p));
    }
  else
    {
      if (SRCPROPSP (p))
	SRCPROPPLIST (p) = scm_acons (key, datum, SRCPROPPLIST (p));
      else
	SCM_WHASHSET (scm_source_whash, h, scm_acons (key, datum, p));
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void
scm_init_srcprop ()
{
  scm_tc16_srcprops = scm_make_smob_type ("srcprops", 0);
  scm_set_smob_mark (scm_tc16_srcprops, srcprops_mark);
  scm_set_smob_free (scm_tc16_srcprops, srcprops_free);
  scm_set_smob_print (scm_tc16_srcprops, srcprops_print);

  scm_source_whash = scm_make_weak_key_hash_table (scm_from_int (2047));
  scm_c_define ("source-whash", scm_source_whash);

#include "libguile/srcprop.x"
}

void
scm_finish_srcprop ()
{
  register scm_t_srcprops_chunk *ptr = srcprops_chunklist, *next;
  size_t n= sizeof (scm_t_srcprops_chunk)
    + sizeof (scm_t_srcprops) * (SRCPROPS_CHUNKSIZE - 1);
  while (ptr)
    {
      next = ptr->next;
      scm_gc_unregister_collectable_memory (ptr, n, "srcprops");
      free ((char *) ptr);
      ptr = next;
    }
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
