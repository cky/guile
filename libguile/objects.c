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
#include "chars.h"
#include "keywords.h"
#include "smob.h"

#include "objects.h"


SCM scm_metaclass_standard;
SCM scm_metaclass_operator;

/* These variables are filled in by the object system when loaded. */
SCM scm_class_boolean, scm_class_char, scm_class_pair;
SCM scm_class_procedure, scm_class_string, scm_class_symbol;
SCM scm_class_procedure_with_setter;
SCM scm_class_vector, scm_class_null;
SCM scm_class_integer, scm_class_real, scm_class_complex;
SCM scm_class_unknown;

SCM *scm_port_class = 0;
SCM *scm_smob_class = 0;

SCM (*scm_make_extended_class) (char *type_name);
void (*scm_make_port_classes) (int ptobnum, char *type_name);
void (*scm_change_object_class) (SCM, SCM, SCM);

/* This function is used for efficient type dispatch.  */
SCM
scm_class_of (SCM x)
{
  switch (SCM_ITAG3 (x))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      return scm_class_integer;

    case scm_tc3_imm24:
      if (SCM_ICHRP (x))
	return scm_class_char;
      else
	{
	  switch (SCM_ISYMNUM (x))
	    {
	    case SCM_ISYMNUM (SCM_BOOL_F):
	    case SCM_ISYMNUM (SCM_BOOL_T):
	      return scm_class_boolean;
	    case SCM_ISYMNUM (SCM_EOL):
	      return scm_class_null;
	    default:
	      return scm_class_unknown;
	    }
	}

    case scm_tc3_cons:
      switch (SCM_TYP7 (x))
	{
	case scm_tcs_cons_nimcar:
	  return scm_class_pair;
	case scm_tcs_closures:
	  return scm_class_procedure;
	case scm_tcs_symbols:
	  return scm_class_symbol;
	case scm_tc7_vector:
	case scm_tc7_wvect:
	case scm_tc7_bvect:
	case scm_tc7_byvect:
	case scm_tc7_svect:
	case scm_tc7_ivect:
	case scm_tc7_uvect:
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
	  return scm_class_vector;
	case scm_tc7_string:
	case scm_tc7_substring:
	  return scm_class_string;
	case scm_tc7_asubr:
	case scm_tc7_subr_0:
	case scm_tc7_subr_1:
	case scm_tc7_cxr:
	case scm_tc7_subr_3:
	case scm_tc7_subr_2:
	case scm_tc7_rpsubr:
	case scm_tc7_subr_1o:
	case scm_tc7_subr_2o:
	case scm_tc7_lsubr_2:
	case scm_tc7_lsubr:
	case scm_tc7_cclo:
	  return scm_class_procedure;
	case scm_tc7_pws:
	  return scm_class_procedure_with_setter;

	case scm_tc7_port:
	  return scm_port_class[(SCM_WRTNG & SCM_CAR (x)
				 ? (SCM_RDNG & SCM_CAR (x)
				    ? SCM_INOUT_PCLASS_INDEX | SCM_PTOBNUM (x)
				    : SCM_OUT_PCLASS_INDEX | SCM_PTOBNUM (x))
				 : SCM_IN_PCLASS_INDEX | SCM_PTOBNUM (x))];
	case scm_tc7_smob:
	  {
	    SCM type = SCM_TYP16 (x);
	    if (type == scm_tc16_flo)
	      {
		if (SCM_CAR (x) & SCM_IMAG_PART)
		  return scm_class_complex;
		else
		  return scm_class_real;
	      }
	    else
	      return scm_smob_class[SCM_TC2SMOBNUM (type)];
	  }
	case scm_tcs_cons_gloc:
	  /* must be a struct */
	  if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS_VALID)
	    return SCM_CLASS_OF (x);
	  else if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS)
	    {
	      /* Goops object */
	      if (SCM_OBJ_CLASS_REDEF (x) != SCM_BOOL_F)
		scm_change_object_class (x,
					 SCM_CLASS_OF (x),         /* old */
					 SCM_OBJ_CLASS_REDEF (x)); /* new */
	      return SCM_CLASS_OF (x);
	    }
	  else
	    {
	      /* ordinary struct */
	      SCM handle = scm_struct_create_handle (SCM_STRUCT_VTABLE (x));
	      if (SCM_NFALSEP (SCM_STRUCT_TABLE_CLASS (SCM_CDR (handle))))
		return SCM_STRUCT_TABLE_CLASS (SCM_CDR (handle));
	      else
		{
		  SCM name = SCM_STRUCT_TABLE_NAME (SCM_CDR (handle));
		  SCM class = scm_make_extended_class (SCM_NFALSEP (name)
						       ? SCM_ROCHARS (name)
						       : 0);
		  SCM_SET_STRUCT_TABLE_CLASS (handle, class);
		  return class;
		}
	    }
	default:
	  if (SCM_CONSP (x))
	    return scm_class_pair;
	  else
	    return scm_class_unknown;
	}

    case scm_tc3_cons_gloc:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
    case scm_tc3_closure:
      /* Never reached */
      break;
    }
  return scm_class_unknown;
}

SCM_PROC (s_entity_p, "entity?", 1, 0, 0, scm_entity_p);

SCM
scm_entity_p (SCM obj)
{
  return (SCM_NIMP (obj) && SCM_STRUCTP (obj) && SCM_I_ENTITYP (obj)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

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

/* The following procedures are not a part of Goops but a minimal
 * object system built upon structs.  They are here for those who
 * want to implement their own object system.
 */

SCM
scm_i_make_class_object (SCM meta,
			 SCM layout_string,
			 unsigned long flags)
{
  SCM c;
  SCM layout = scm_make_struct_layout (layout_string);
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
  SCM_ASSERT (SCM_NIMP (metaclass) && SCM_STRUCTP (metaclass),
	      metaclass, SCM_ARG1, s_make_class_object);
  SCM_ASSERT (SCM_NIMP (layout) && SCM_STRINGP (layout),
	      layout, SCM_ARG2, s_make_class_object);
  if (metaclass == scm_metaclass_operator)
    flags = SCM_CLASSF_OPERATOR;
  return scm_i_make_class_object (metaclass, layout, flags);
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
  SCM_ASSERT (SCM_NIMP (layout) && SCM_STRINGP (layout),
	      layout,
	      SCM_ARG2,
	      s_make_subclass_object);
  pl = SCM_STRUCT_DATA (class)[scm_vtable_index_layout];
  /* Convert symbol->string */
  pl = scm_makfromstr (SCM_CHARS (pl), (scm_sizet) SCM_LENGTH (pl), 0);
  return scm_i_make_class_object (SCM_STRUCT_VTABLE (class),
				  scm_string_append (SCM_LIST2 (pl, layout)),
				  SCM_CLASS_FLAGS (class));
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

  scm_sysintern ("<class>", mt);
  scm_metaclass_standard = mt;
  scm_sysintern ("<operator-class>", ot);
  scm_metaclass_operator = ot;
  SCM_SET_CLASS_FLAGS (et, SCM_CLASSF_OPERATOR | SCM_CLASSF_ENTITY);
  SCM_SET_CLASS_DESTRUCTOR (et, scm_struct_free_entity);
  scm_sysintern ("<entity>", et);

#include "objects.x"
}
