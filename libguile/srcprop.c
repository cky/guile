/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN */


#include <stdio.h>
#include "_scm.h"
#include "smob.h"
#include "alist.h"
#include "debug.h"
#include "hashtab.h"
#include "hash.h"
#include "weaks.h"

#include "srcprop.h"

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

SCM scm_i_filename;
SCM scm_i_copy;
SCM scm_i_line;
SCM scm_i_column;
SCM scm_i_breakpoint;

long scm_tc16_srcprops;
static scm_srcprops_chunk *srcprops_chunklist = 0;
static scm_srcprops *srcprops_freelist = 0;


static SCM marksrcprops SCM_P ((SCM obj));

static SCM
marksrcprops (obj)
     SCM obj;
{
  scm_gc_mark (SRCPROPFNAME (obj));
  scm_gc_mark (SRCPROPCOPY (obj));
  return SRCPROPPLIST (obj);
}


static scm_sizet freesrcprops SCM_P ((SCM obj));

static scm_sizet
freesrcprops (obj)
     SCM obj;
{
  *((scm_srcprops **) SCM_CDR (obj)) = srcprops_freelist;
  srcprops_freelist = (scm_srcprops *) SCM_CDR (obj);
  return 0; /* srcprops_chunks are not freed until leaving guile */
}


static int prinsrcprops SCM_P ((SCM obj, SCM port, scm_print_state *pstate));

static int
prinsrcprops (obj, port, pstate)
     SCM obj;
     SCM port;
     scm_print_state *pstate;
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<srcprops ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (scm_srcprops_to_plist (obj), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return 1;
}

static scm_smobfuns srcpropssmob =
{marksrcprops, freesrcprops, prinsrcprops, 0};


SCM
scm_make_srcprops (line, col, filename, copy, plist)
     int line;
     int col;
     SCM filename;
     SCM copy;
     SCM plist;
{
  register SCM ans;
  register scm_srcprops *ptr;
  SCM_DEFER_INTS;
  if ((ptr = srcprops_freelist) != NULL)
    srcprops_freelist = *(scm_srcprops **)ptr;
  else
    {
      int i;
      scm_srcprops_chunk *mem;
      scm_sizet n = sizeof (scm_srcprops_chunk)
	            + sizeof (scm_srcprops) * (SRCPROPS_CHUNKSIZE - 1);
      SCM_SYSCALL (mem = (scm_srcprops_chunk *) malloc (n));
      SCM_ASSERT (mem, SCM_UNDEFINED, SCM_NALLOC, "srcprops");
      scm_mallocated += n;
      mem->next = srcprops_chunklist;
      srcprops_chunklist = mem;
      ptr = &mem->srcprops[0];
      for (i = 1; i < SRCPROPS_CHUNKSIZE - 1; ++i)
	*(scm_srcprops **)&ptr[i] = &ptr[i + 1];
      *(scm_srcprops **)&ptr[SRCPROPS_CHUNKSIZE - 1] = 0;
      srcprops_freelist = (scm_srcprops *) &ptr[1];
    }
  SCM_NEWCELL (ans);
  SCM_SETCAR (ans, scm_tc16_srcprops);
  ptr->pos = SRCPROPMAKPOS (line, col);
  ptr->fname = filename;
  ptr->copy = copy;
  ptr->plist = plist;
  SCM_SETCDR (ans, (SCM) ptr);
  SCM_ALLOW_INTS;
  return ans;
}


SCM
scm_srcprops_to_plist (obj)
     SCM obj;
{
  SCM plist = SRCPROPPLIST (obj);
  if (!SCM_UNBNDP (SRCPROPCOPY (obj)))
    plist = scm_acons (scm_i_copy, SRCPROPCOPY (obj), plist);
  if (!SCM_UNBNDP (SRCPROPFNAME (obj)))
    plist = scm_acons (scm_i_filename, SRCPROPFNAME (obj), plist);
  plist = scm_acons (scm_i_column, SCM_MAKINUM (SRCPROPCOL (obj)), plist);
  plist = scm_acons (scm_i_line, SCM_MAKINUM (SRCPROPLINE (obj)), plist);
  plist = scm_acons (scm_i_breakpoint, SRCPROPBRK (obj), plist);
  return plist;
}

SCM_PROC (s_source_properties, "source-properties", 1, 0, 0, scm_source_properties);

SCM
scm_source_properties (obj)
     SCM obj;
{
  SCM p;
  SCM_ASSERT (SCM_NIMP (obj), obj, SCM_ARG1, s_source_properties);
  if (SCM_MEMOIZEDP (obj))
    obj = SCM_MEMOIZED_EXP (obj);
#ifndef SCM_RECKLESS
  else if (SCM_NCONSP (obj))
    scm_wrong_type_arg (s_source_properties, 1, obj);
#endif
  p = scm_hashq_ref (scm_source_whash, obj, (SCM) NULL);
  if (p != (SCM) NULL && SRCPROPSP (p))
    return scm_srcprops_to_plist (p);
  return SCM_EOL;
}

/* Perhaps this procedure should look through an alist
   and try to make a srcprops-object...? */
SCM_PROC (s_set_source_properties_x, "set-source-properties!", 2, 0, 0, scm_set_source_properties_x);

SCM
scm_set_source_properties_x (obj, plist)
     SCM obj;
     SCM plist;
{
  SCM handle;
  SCM_ASSERT (SCM_NIMP (obj), obj, SCM_ARG1, s_set_source_properties_x);
  if (SCM_MEMOIZEDP (obj))
    obj = SCM_MEMOIZED_EXP (obj);
#ifndef SCM_RECKLESS
  else if (SCM_NCONSP (obj))
    scm_wrong_type_arg (s_set_source_properties_x, 1, obj);
#endif
  handle = scm_hashq_create_handle_x (scm_source_whash, obj, plist);
  SCM_SETCDR (handle, plist);
  return plist;
}

SCM_PROC (s_source_property, "source-property", 2, 0, 0, scm_source_property);

SCM
scm_source_property (obj, key)
     SCM obj;
     SCM key;
{
  SCM p;
  SCM_ASSERT (SCM_NIMP (obj), obj, SCM_ARG1, s_source_property);
  if (SCM_MEMOIZEDP (obj))
    obj = SCM_MEMOIZED_EXP (obj);
#ifndef SCM_RECKLESS
  else if (SCM_NCONSP (obj))
    scm_wrong_type_arg (s_source_property, 1, obj);
#endif
  p = scm_hashq_ref (scm_source_whash, obj, SCM_EOL);
  if (SCM_IMP (p) || !SRCPROPSP (p))
    goto plist;
  if      (scm_i_breakpoint == key) p = SRCPROPBRK (p);
  else if (scm_i_line       == key) p = SCM_MAKINUM (SRCPROPLINE (p));
  else if (scm_i_column     == key) p = SCM_MAKINUM (SRCPROPCOL (p));
  else if (scm_i_filename   == key) p = SRCPROPFNAME (p);
  else if (scm_i_copy       == key) p = SRCPROPCOPY (p);
  else
    {
      p = SRCPROPPLIST (p);
    plist:
      p = scm_assoc (key, p);
      return (SCM_NIMP (p) ? SCM_CDR (p) : SCM_BOOL_F);
    }
  return SCM_UNBNDP (p) ? SCM_BOOL_F : p;
}

SCM_PROC (s_set_source_property_x, "set-source-property!", 3, 0, 0, scm_set_source_property_x);

SCM
scm_set_source_property_x (obj, key, datum)
     SCM obj;
     SCM key;
     SCM datum;
{
  scm_whash_handle h;
  SCM p;
  SCM_ASSERT (SCM_NIMP (obj), obj, SCM_ARG1, s_set_source_property_x);
  if (SCM_MEMOIZEDP (obj))
    obj = SCM_MEMOIZED_EXP (obj);
#ifndef SCM_RECKLESS
  else if (SCM_NCONSP (obj))
    scm_wrong_type_arg (s_set_source_property_x, 1, obj);
#endif
  h = scm_whash_get_handle (scm_source_whash, obj);
  if (SCM_WHASHFOUNDP (h))
    p = SCM_WHASHREF (scm_source_whash, h);
  else
    {
      h = scm_whash_create_handle (scm_source_whash, obj);
      p = SCM_EOL;
    }
  if (scm_i_breakpoint == key)
    {
      if (SCM_FALSEP (datum))
	CLEARSRCPROPBRK (SCM_NIMP (p) && SRCPROPSP (p)
			 ? p
			 : SCM_WHASHSET (scm_source_whash, h,
					 scm_make_srcprops (0,
							    0,
							    SCM_UNDEFINED,
							    SCM_UNDEFINED,
							    p)));
      else
	SETSRCPROPBRK (SCM_NIMP (p) && SRCPROPSP (p)
		       ? p
		       : SCM_WHASHSET (scm_source_whash, h,
				       scm_make_srcprops (0,
							  0,
							  SCM_UNDEFINED,
							  SCM_UNDEFINED,
							  p)));
    }
  else if (scm_i_line == key)
    {
      SCM_ASSERT (SCM_INUMP (datum),
		  datum, SCM_ARG3, s_set_source_property_x);
      if (SCM_NIMP (p) && SRCPROPSP (p))
	SETSRCPROPLINE (p, SCM_INUM (datum));
      else
	SCM_WHASHSET (scm_source_whash, h,
		      scm_make_srcprops (SCM_INUM (datum), 0,
					 SCM_UNDEFINED, SCM_UNDEFINED, p));
    }
  else if (scm_i_column == key)
    {
      SCM_ASSERT (SCM_INUMP (datum),
		  datum, SCM_ARG3, s_set_source_property_x);
      if (SCM_NIMP (p) && SRCPROPSP (p))
	SETSRCPROPCOL (p, SCM_INUM (datum));
      else
	SCM_WHASHSET (scm_source_whash, h,
		      scm_make_srcprops (0, SCM_INUM (datum),
					 SCM_UNDEFINED, SCM_UNDEFINED, p));
    }
  else if (scm_i_filename == key)
    {
      if (SCM_NIMP (p) && SRCPROPSP (p))
	SRCPROPFNAME (p) = datum;
      else
	SCM_WHASHSET (scm_source_whash, h, scm_make_srcprops (0, 0, datum, SCM_UNDEFINED, p));
    }
  else if (scm_i_filename == key)
    {
      if (SCM_NIMP (p) && SRCPROPSP (p))
	SRCPROPCOPY (p) = datum;
      else
	SCM_WHASHSET (scm_source_whash, h, scm_make_srcprops (0, 0, SCM_UNDEFINED, datum, p));
    }
  else
    SCM_WHASHSET (scm_source_whash, h, scm_acons (key, datum, p));
  return SCM_UNSPECIFIED;
}


void
scm_init_srcprop ()
{
  scm_tc16_srcprops = scm_newsmob (&srcpropssmob);
  scm_source_whash = scm_make_weak_key_hash_table (SCM_MAKINUM (2047));

  scm_i_filename = SCM_CAR (scm_sysintern ("filename", SCM_UNDEFINED));
  scm_i_copy = SCM_CAR (scm_sysintern ("copy", SCM_UNDEFINED));
  scm_i_line = SCM_CAR (scm_sysintern ("line", SCM_UNDEFINED));
  scm_i_column = SCM_CAR (scm_sysintern ("column", SCM_UNDEFINED));
  scm_i_breakpoint = SCM_CAR (scm_sysintern ("breakpoint", SCM_UNDEFINED));

  scm_sysintern ("source-whash", scm_source_whash);
#include "srcprop.x"
}

void
scm_finish_srcprop ()
{
  register scm_srcprops_chunk *ptr = srcprops_chunklist, *next;
  while (ptr)
    {
      next = ptr->next;
      free ((char *) ptr);
      scm_mallocated -= sizeof (scm_srcprops_chunk)
	                + sizeof (scm_srcprops) * (SRCPROPS_CHUNKSIZE - 1);
      ptr = next;
    }
}
