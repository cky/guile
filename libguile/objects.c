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


/* This file and objects.h contains those minimal pieces of the Guile
 * Object Oriented Programming System which need to be included in
 * libguile.  See the comments in objects.h.
 */

#include "_scm.h"

#include "struct.h"
#include "procprop.h"

#include "objects.h"


SCM scm_metaclass_standard;
SCM scm_metaclass_operator;

SCM_PROC (s_set_object_procedure_x, "set-object-procedure!", 1, 0, 1, scm_set_object_procedure_x);

SCM
scm_set_object_procedure_x (SCM obj, SCM procs)
{
  SCM proc[4], *pp, p, setp, arity;
  int i, a, r;
  SCM_ASSERT (SCM_NIMP (obj) && SCM_STRUCTP (obj)
	      && ((SCM_CLASS_FLAGS (obj) & SCM_CLASSF_OPERATOR)
		  || SCM_I_ENTITYP (obj)),
	      obj,
	      SCM_ARG1,
	      s_set_object_procedure_x);
  for (i = 0; i < 4; ++i)
    proc[i] = SCM_BOOL_F;
  i = 0;
  while (SCM_NIMP (procs))
    {
      if (i == 4)
	scm_wrong_num_args (scm_makfrom0str (s_set_object_procedure_x));
      p = SCM_CAR (procs);
      setp = 0;
      SCM_ASSERT (SCM_NIMP (p), p, SCM_ARG2 + i, s_set_object_procedure_x);
      if (SCM_CLOSUREP (p))
	{
	  arity = scm_procedure_property (p, scm_sym_arity);
	  a = SCM_INUM (SCM_CAR (arity));
	  /* Closures have zero optional args */
	  r = SCM_NFALSEP (SCM_CADDR (arity));
	  if (a == 1 || (a <= 1 && r))
	    {
	      if (SCM_NFALSEP (proc[0]))
		goto ambiguous;
	      proc[0] = setp = p;
	    }
	  if (a == 2 || (a <= 2 && r))
	    {
	      if (SCM_NFALSEP (proc[1]))
		goto ambiguous;
	      proc[1] = setp = p;
	    }
	  if (a == 3 || (a <= 3 && r))
	    {
	      if (SCM_NFALSEP (proc[2]))
		goto ambiguous;
	      proc[2] = setp = p;
	    }
	  if (a <= 4 && r)
	    {
	      if (SCM_NFALSEP (proc[3]))
		goto ambiguous;
	      proc[3] = setp = p;
	    }
	}
      else if (SCM_TYP7 (p) == scm_tc7_subr_1)
	{
	  if (SCM_NFALSEP (proc[0]))
	    goto ambiguous;
	  proc[0] = setp = p;
	}
      else if (SCM_TYP7 (p) == scm_tc7_subr_2)
	{
	  if (SCM_NFALSEP (proc[1]))
	    goto ambiguous;
	  proc[1] = setp = p;
	}
      else if (SCM_TYP7 (p) == scm_tc7_subr_3)
	{
	  if (SCM_NFALSEP (proc[2]))
	    goto ambiguous;
	  proc[2] = setp = p;
	}
      else if (SCM_TYP7 (p) == scm_tc7_lsubr_2)
	{
	  if (SCM_NFALSEP (proc[3]))
	    {
	    ambiguous:
	      SCM_ASSERT (0, p, "Ambiguous procedure arities",
			  s_set_object_procedure_x);
	    }
	  proc[3] = setp = p;
	}
      SCM_ASSERT (setp, p, SCM_ARG2 + i, s_set_object_procedure_x);
      ++i;
      procs = SCM_CDR (procs);
    }
  pp = (SCM_I_ENTITYP (obj)
	? &SCM_ENTITY_PROC_0 (obj)
	: &SCM_OPERATOR_CLASS (obj)->proc0);
  for (i = 0; i < 4; ++i)
    *pp++ = proc[i];
  return SCM_UNSPECIFIED;
}

static SCM
make_class_object (SCM meta,
		   SCM pl,
		   SCM layout,
		   unsigned long flags,
		   char* subr)
{
  SCM c;
  SCM_ASSERT (SCM_NIMP (meta) && SCM_STRUCTP (meta), meta, SCM_ARG1, subr);
  SCM_ASSERT (SCM_NIMP (layout) && SCM_STRINGP (layout),
	      layout,
	      SCM_ARG2,
	      subr);
  layout = scm_make_struct_layout (scm_string_append (SCM_LIST2 (pl, layout)));
  c = scm_make_struct (meta,
		       SCM_INUM0,
		       SCM_LIST4 (layout, SCM_BOOL_F, SCM_EOL, SCM_EOL));
  SCM_SET_CLASS_FLAGS (c, flags);
  return c;
}

SCM_PROC (s_make_class_object, "make-class-object", 2, 0, 0, scm_make_class_object);

SCM
scm_make_class_object (SCM metaclass, SCM layout)
{
  unsigned long flags = 0;
  if (metaclass == scm_metaclass_operator)
    flags = SCM_CLASSF_OPERATOR;
  return make_class_object (metaclass,
			    scm_nullstr,
			    layout,
			    flags,
			    s_make_class_object);
}

SCM_PROC (s_make_subclass_object, "make-subclass-object", 2, 0, 0, scm_make_subclass_object);

SCM
scm_make_subclass_object (SCM class, SCM layout)
{
  SCM pl;
  SCM_ASSERT (SCM_NIMP (class) && SCM_STRUCTP (class),
	      class,
	      SCM_ARG1,
	      s_make_subclass_object);
  pl = SCM_STRUCT_DATA (class)[scm_vtable_index_layout];
  pl = scm_makfromstr (SCM_CHARS (pl), (scm_sizet) SCM_LENGTH (pl), 0);
  return make_class_object (scm_metaclass_standard,
			    pl,
			    layout,
			    SCM_CLASS_FLAGS (class),
			    s_make_subclass_object);
}

void
scm_init_objects ()
{
  SCM ms = scm_makfrom0str (SCM_METACLASS_STANDARD_LAYOUT);
  SCM ml = scm_make_struct_layout (ms);
  SCM mt = scm_make_vtable_vtable (ml, SCM_INUM0,
				   SCM_LIST3 (SCM_BOOL_F, SCM_EOL, SCM_EOL));
  
  SCM os = scm_makfrom0str (SCM_METACLASS_OPERATOR_LAYOUT);
  SCM ol = scm_make_struct_layout (os);
  SCM ot = scm_make_vtable_vtable (ol, SCM_INUM0,
				   SCM_LIST3 (SCM_BOOL_F, SCM_EOL, SCM_EOL));
  
  SCM es = scm_makfrom0str (SCM_ENTITY_LAYOUT);
  SCM el = scm_make_struct_layout (es);
  SCM et = scm_make_struct (mt, SCM_INUM0,
			    SCM_LIST4 (el, SCM_BOOL_F, SCM_EOL, SCM_EOL));

  scm_sysintern ("<standard-metaclass>", mt);
  scm_metaclass_standard = mt;
  scm_sysintern ("<operator-metaclass>", ot);
  scm_metaclass_operator = ot;
  SCM_SET_CLASS_FLAGS (et, SCM_CLASSF_OPERATOR | SCM_CLASSF_ENTITY);
  scm_sysintern ("<entity-class>", et);

#include "objects.x"
}
