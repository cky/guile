/* Copyright (C) 1995,1996,1997,1999,2000,2001 Free Software Foundation, Inc.
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



#include "libguile/_scm.h"

#include "libguile/objects.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/procs.h"



/* {Procedures}
 */

scm_subr_entry *scm_subr_table;

/* libguile contained approx. 700 primitive procedures on 24 Aug 1999. */

int scm_subr_table_size = 0;
int scm_subr_table_room = 750;

SCM 
scm_make_subr_opt (const char *name, int type, SCM (*fcn) (), int set)
{
  SCM symbol;
  SCM symcell;
  register SCM z;
  int entry;

  if (scm_subr_table_size == scm_subr_table_room)
    {
      scm_sizet new_size = scm_subr_table_room * 3 / 2;
      void *new_table
	= scm_must_realloc ((char *) scm_subr_table,
			    sizeof (scm_subr_entry) * scm_subr_table_room,
			    sizeof (scm_subr_entry) * new_size, 
			    "scm_subr_table");
      scm_subr_table = new_table;
      scm_subr_table_room = new_size;
    }

  SCM_NEWCELL (z);
  if (set)
    {
      symcell = scm_sysintern (name, SCM_UNDEFINED);
      symbol = SCM_CAR (symcell);
    }
  else
    {
      symcell = SCM_BOOL_F; /* to avoid warning */
      symbol = scm_str2symbol (name);
    }
  
  entry = scm_subr_table_size;
  scm_subr_table[entry].handle = z;
  scm_subr_table[entry].name = symbol;
  scm_subr_table[entry].generic = 0;
  scm_subr_table[entry].properties = SCM_EOL;
  
  SCM_SET_SUBRF (z, fcn);
  SCM_SET_CELL_TYPE (z, (entry << 8) + type);
  scm_subr_table_size++;
  
  if (set)
    SCM_SETCDR (symcell, z);
  
  return z;
}

/* This function isn't currently used since subrs are never freed. */
/* *fixme* Need mutex here. */
void
scm_free_subr_entry (SCM subr)
{
  int entry = SCM_SUBRNUM (subr);
  /* Move last entry in table to the free position */
  scm_subr_table[entry] = scm_subr_table[scm_subr_table_size - 1];
  SCM_SET_SUBRNUM (scm_subr_table[entry].handle, entry);
  scm_subr_table_size--;
}

SCM 
scm_make_subr (const char *name, int type, SCM (*fcn) ())
{
  return scm_make_subr_opt (name, type, fcn, 1);
}

SCM
scm_make_subr_with_generic (const char *name, int type, SCM (*fcn) (), SCM *gf)
{
  SCM subr = scm_make_subr_opt (name, type, fcn, 1);
  scm_subr_table[scm_subr_table_size - 1].generic = gf;
  return subr;
}

void
scm_mark_subr_table ()
{
  int i;
  for (i = 0; i < scm_subr_table_size; ++i)
    {
      SCM_SETGCMARK (scm_subr_table[i].name);
      if (scm_subr_table[i].generic && *scm_subr_table[i].generic)
	scm_gc_mark (*scm_subr_table[i].generic);
      if (SCM_NIMP (scm_subr_table[i].properties))
	scm_gc_mark (scm_subr_table[i].properties);
    }
}


#ifdef CCLO
SCM 
scm_makcclo (SCM proc, long len)
{
  scm_bits_t *base = scm_must_malloc (len * sizeof (scm_bits_t), "compiled-closure");
  unsigned long i;
  SCM s;

  for (i = 0; i < len; ++i)
    base [i] = SCM_UNPACK (SCM_UNSPECIFIED);

  SCM_NEWCELL (s);
  SCM_DEFER_INTS;
  SCM_SET_CCLO_BASE (s, base);
  SCM_SET_CCLO_LENGTH (s, len);
  SCM_SET_CCLO_SUBR (s, proc);
  SCM_ALLOW_INTS;
  return s;
}

/* Undocumented debugging procedure */
#ifdef GUILE_DEBUG
SCM_DEFINE (scm_make_cclo, "make-cclo", 2, 0, 0,
            (SCM proc, SCM len),
	    "Create a compiled closure for @var{proc}, which reserves\n"
	    "@var{len} objects for its usage.")
#define FUNC_NAME s_scm_make_cclo
{
  return scm_makcclo (proc, SCM_INUM (len));
}
#undef FUNC_NAME
#endif
#endif



SCM_DEFINE (scm_procedure_p, "procedure?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a procedure.")
#define FUNC_NAME s_scm_procedure_p
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_cons_gloc:
	if (!SCM_I_OPERATORP (obj))
	  break;
      case scm_tcs_closures:
      case scm_tcs_subrs:
#ifdef CCLO
      case scm_tc7_cclo:
#endif
      case scm_tc7_pws:
	return SCM_BOOL_T;
      case scm_tc7_smob:
	return SCM_BOOL (SCM_SMOB_DESCRIPTOR (obj).apply);
      default:
	return SCM_BOOL_F;
      }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_closure_p, "closure?", 1, 0, 0, 
           (SCM obj),
	    "Return @code{#t} if @var{obj} is a closure.")
#define FUNC_NAME s_scm_closure_p
{
  return SCM_BOOL (SCM_CLOSUREP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_thunk_p, "thunk?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a thunk.")
#define FUNC_NAME s_scm_thunk_p
{
  if (SCM_NIMP (obj))
    {
    again:
      switch (SCM_TYP7 (obj))
	{
	case scm_tcs_closures:
	  return SCM_BOOL (!SCM_CONSP (SCM_CLOSURE_FORMALS (obj)));
	case scm_tc7_subr_0:
	case scm_tc7_subr_1o:
	case scm_tc7_lsubr:
	case scm_tc7_rpsubr:
	case scm_tc7_asubr:
#ifdef CCLO
	case scm_tc7_cclo:
#endif
	  return SCM_BOOL_T;
	case scm_tc7_pws:
	  obj = SCM_PROCEDURE (obj);
	  goto again;
	default:
	  ;
	}
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Only used internally. */
int
scm_subr_p (SCM obj)
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_subrs:
	return 1;
      default:
	;
      }
  return 0;
}

SCM_DEFINE (scm_procedure_documentation, "procedure-documentation", 1, 0, 0, 
           (SCM proc),
	    "Return the documentation string associated with @code{proc}.  By\n"
	    "convention, if a procedure contains more than one expression and the\n"
	    "first expression is a string constant, that string is assumed to contain\n"
	    "documentation for that procedure.")
#define FUNC_NAME s_scm_procedure_documentation
{
  SCM code;
  SCM_ASSERT (SCM_EQ_P (scm_procedure_p (proc), SCM_BOOL_T) && SCM_NIMP (proc),
	      proc, SCM_ARG1, FUNC_NAME);
  switch (SCM_TYP7 (proc))
    {
    case scm_tcs_closures:
      code = SCM_CDR (SCM_CODE (proc));
      if (SCM_IMP (SCM_CDR (code)))
	return SCM_BOOL_F;
      code = SCM_CAR (code);
      if (SCM_IMP (code))
	return SCM_BOOL_F;
      if (SCM_STRINGP (code))
	return code;
    default:
      return SCM_BOOL_F;
/*
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
*/
    }
}
#undef FUNC_NAME


/* Procedure-with-setter
 */

SCM_DEFINE (scm_procedure_with_setter_p, "procedure-with-setter?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a procedure with an\n"
	    "associated setter procedure.")
#define FUNC_NAME s_scm_procedure_with_setter_p
{
  return SCM_BOOL(SCM_PROCEDURE_WITH_SETTER_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_procedure_with_setter, "make-procedure-with-setter", 2, 0, 0, 
            (SCM procedure, SCM setter),
	    "Create a new procedure which behaves like @var{procedure}, but\n"
	    "with the associated setter @var{setter}.")
#define FUNC_NAME s_scm_make_procedure_with_setter
{
  SCM z;
  SCM_VALIDATE_PROC (1, procedure);
  SCM_VALIDATE_PROC (2, setter);
  SCM_NEWCELL2 (z);
  SCM_ENTER_A_SECTION;
  SCM_SET_CELL_OBJECT_1 (z, procedure);
  SCM_SET_CELL_OBJECT_2 (z, setter);
  SCM_SET_CELL_TYPE (z, scm_tc7_pws);
  SCM_EXIT_A_SECTION;
  return z;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure, "procedure", 1, 0, 0, 
            (SCM proc),
	    "Return the procedure of @var{proc}, which must be either a\n"
	    "procedure with setter, or an operator struct.")
#define FUNC_NAME s_scm_procedure
{
  SCM_VALIDATE_NIM (1, proc);
  if (SCM_PROCEDURE_WITH_SETTER_P (proc))
    return SCM_PROCEDURE (proc);
  else if (SCM_STRUCTP (proc))
    {
      SCM_ASSERT (SCM_I_OPERATORP (proc), proc, SCM_ARG1, FUNC_NAME);
      return proc;
    }
  SCM_WRONG_TYPE_ARG (1, proc);
  return SCM_BOOL_F; /* not reached */
}
#undef FUNC_NAME

SCM_GPROC (s_setter, "setter", 1, 0, 0, scm_setter, g_setter);

SCM
scm_setter (SCM proc)
{
  SCM_GASSERT1 (SCM_NIMP (proc), g_setter, proc, SCM_ARG1, s_setter);
  if (SCM_PROCEDURE_WITH_SETTER_P (proc))
    return SCM_SETTER (proc);
  else if (SCM_STRUCTP (proc))
    {
      SCM setter;
      SCM_GASSERT1 (SCM_I_OPERATORP (proc),
		    g_setter, proc, SCM_ARG1, s_setter);
      setter = (SCM_I_ENTITYP (proc)
		? SCM_ENTITY_SETTER (proc)
		: SCM_OPERATOR_SETTER (proc));
      if (SCM_NIMP (setter))
	return setter;
      /* fall through */
    }
  SCM_WTA_DISPATCH_1 (g_setter, proc, SCM_ARG1, s_setter);
  return SCM_BOOL_F; /* not reached */
}


void
scm_init_subr_table ()
{
  scm_subr_table
    = ((scm_subr_entry *)
       scm_must_malloc (sizeof (scm_subr_entry) * scm_subr_table_room,
			"scm_subr_table"));
}

void
scm_init_procs ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/procs.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
