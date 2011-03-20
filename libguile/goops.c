/* Copyright (C) 1998,1999,2000,2001,2002,2003,2004,2008,2009,2010,2011
 * Free Software Foundation, Inc.
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


/* This software is a derivative work of other copyrighted softwares; the
 * copyright notices of these softwares are placed in the file COPYRIGHTS
 *
 * This file is based upon stklos.c from the STk distribution by
 * Erick Gallesio <eg@unice.fr>.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/async.h"
#include "libguile/chars.h"
#include "libguile/debug.h"
#include "libguile/dynl.h"
#include "libguile/dynwind.h"
#include "libguile/eval.h"
#include "libguile/gsubr.h"
#include "libguile/hashtab.h"
#include "libguile/keywords.h"
#include "libguile/macros.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/procprop.h"
#include "libguile/programs.h"
#include "libguile/random.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"
#include "libguile/weaks.h"
#include "libguile/vm.h"

#include "libguile/validate.h"
#include "libguile/goops.h"

#define SPEC_OF(x)  SCM_SLOT (x, scm_si_specializers)

/* Port classes */
#define SCM_IN_PCLASS_INDEX       0
#define SCM_OUT_PCLASS_INDEX      SCM_I_MAX_PORT_TYPE_COUNT
#define SCM_INOUT_PCLASS_INDEX    (2 * SCM_I_MAX_PORT_TYPE_COUNT)

/* this file is a mess. in theory, though, we shouldn't have many SCM references
   -- most of the references should be to vars. */

static SCM var_slot_unbound = SCM_BOOL_F;
static SCM var_slot_missing = SCM_BOOL_F;
static SCM var_compute_cpl = SCM_BOOL_F;
static SCM var_no_applicable_method = SCM_BOOL_F;
static SCM var_change_class = SCM_BOOL_F;

SCM_SYMBOL (sym_slot_unbound, "slot-unbound");
SCM_SYMBOL (sym_slot_missing, "slot-missing");
SCM_SYMBOL (sym_compute_cpl, "compute-cpl");
SCM_SYMBOL (sym_no_applicable_method, "no-applicable-method");
SCM_SYMBOL (sym_memoize_method_x, "memoize-method!");
SCM_SYMBOL (sym_change_class, "change-class");

SCM_VARIABLE (scm_var_make_extended_generic, "make-extended-generic");


/* FIXME, exports should come from the scm file only */
#define DEFVAR(v, val)                                          \
  { scm_module_define (scm_module_goops, (v), (val));           \
    scm_module_export (scm_module_goops, scm_list_1 ((v)));     \
  }


/* Class redefinition protocol:

   A class is represented by a heap header h1 which points to a
   malloc:ed memory block m1.

   When a new version of a class is created, a new header h2 and
   memory block m2 are allocated.  The headers h1 and h2 then switch
   pointers so that h1 refers to m2 and h2 to m1.  In this way, names
   bound to h1 will point to the new class at the same time as h2 will
   be a handle which the GC will use to free m1.

   The `redefined' slot of m1 will be set to point to h1.  An old
   instance will have its class pointer (the CAR of the heap header)
   pointing to m1.  The non-immediate `redefined'-slot in m1 indicates
   the class modification and the new class pointer can be found via
   h1.
*/

#define TEST_CHANGE_CLASS(obj, class)				       \
	{							       \
	  class = SCM_CLASS_OF (obj);				       \
          if (scm_is_true (SCM_OBJ_CLASS_REDEF (obj)))		       \
	    {							       \
	      scm_change_object_class (obj, class, SCM_OBJ_CLASS_REDEF (obj));\
	      class = SCM_CLASS_OF (obj);			       \
	    }							       \
	}

#define NXT_MTHD_METHODS(m)	(SCM_VELTS (m)[1])
#define NXT_MTHD_ARGS(m)	(SCM_VELTS (m)[2])

#define SCM_GOOPS_UNBOUND SCM_UNBOUND
#define SCM_GOOPS_UNBOUNDP(x) ((x) == SCM_GOOPS_UNBOUND)

static int goops_loaded_p = 0;
static scm_t_rstate *goops_rstate;

/* These variables are filled in by the object system when loaded. */
SCM scm_class_boolean, scm_class_char, scm_class_pair;
SCM scm_class_procedure, scm_class_string, scm_class_symbol;
SCM scm_class_primitive_generic;
SCM scm_class_vector, scm_class_null;
SCM scm_class_integer, scm_class_real, scm_class_complex, scm_class_fraction;
SCM scm_class_unknown;
SCM scm_class_top, scm_class_object, scm_class_class;
SCM scm_class_applicable;
SCM scm_class_applicable_struct, scm_class_applicable_struct_with_setter;
SCM scm_class_generic, scm_class_generic_with_setter;
SCM scm_class_accessor;
SCM scm_class_extended_generic, scm_class_extended_generic_with_setter;
SCM scm_class_extended_accessor;
SCM scm_class_method;
SCM scm_class_accessor_method;
SCM scm_class_procedure_class;
SCM scm_class_applicable_struct_class;
SCM scm_class_number, scm_class_list;
SCM scm_class_keyword;
SCM scm_class_port, scm_class_input_output_port;
SCM scm_class_input_port, scm_class_output_port;
SCM scm_class_foreign_slot;
SCM scm_class_self, scm_class_protected;
SCM scm_class_hidden, scm_class_opaque, scm_class_read_only;
SCM scm_class_protected_hidden, scm_class_protected_opaque, scm_class_protected_read_only;
SCM scm_class_scm;
SCM scm_class_int, scm_class_float, scm_class_double;

static SCM class_foreign;
static SCM class_hashtable;
static SCM class_fluid;
static SCM class_dynamic_state;
static SCM class_frame;
static SCM class_objcode;
static SCM class_vm;
static SCM class_vm_cont;
static SCM class_bytevector;
static SCM class_uvec;

/* Port classes.  Allocate 3 times the maximum number of port types so that
   input ports, output ports, and in/out ports can be stored at different
   offsets.  See `SCM_IN_PCLASS_INDEX' et al.  */
SCM scm_port_class[3 * SCM_I_MAX_PORT_TYPE_COUNT];

/* SMOB classes.  */
SCM scm_smob_class[SCM_I_MAX_SMOB_TYPE_COUNT];

SCM scm_no_applicable_method;

SCM_SYMBOL (scm_sym_define_public, "define-public");

static SCM scm_make_unbound (void);
static SCM scm_unbound_p (SCM obj);
static SCM scm_assert_bound (SCM value, SCM obj);
static SCM scm_at_assert_bound_ref (SCM obj, SCM index);
static SCM scm_sys_goops_loaded (void);
static SCM scm_make_extended_class_from_symbol (SCM type_name_sym, 
						int applicablep);

/* This function is used for efficient type dispatch.  */
SCM_DEFINE (scm_class_of, "class-of", 1, 0, 0,
	    (SCM x),
	    "Return the class of @var{x}.")
#define FUNC_NAME s_scm_class_of
{
  switch (SCM_ITAG3 (x))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      return scm_class_integer;

    case scm_tc3_imm24:
      if (SCM_CHARP (x))
	return scm_class_char;
      else if (scm_is_bool (x))
        return scm_class_boolean;
      else if (scm_is_null (x))
        return scm_class_null;
      else
        return scm_class_unknown;

    case scm_tc3_cons:
      switch (SCM_TYP7 (x))
	{
	case scm_tcs_cons_nimcar:
	  return scm_class_pair;
	case scm_tc7_symbol:
	  return scm_class_symbol;
	case scm_tc7_vector:
	case scm_tc7_wvect:
	  return scm_class_vector;
	case scm_tc7_pointer:
	  return class_foreign;
	case scm_tc7_hashtable:
	  return class_hashtable;
	case scm_tc7_fluid:
	  return class_fluid;
	case scm_tc7_dynamic_state:
	  return class_dynamic_state;
        case scm_tc7_frame:
	  return class_frame;
        case scm_tc7_objcode:
	  return class_objcode;
        case scm_tc7_vm:
	  return class_vm;
        case scm_tc7_vm_cont:
	  return class_vm_cont;
	case scm_tc7_bytevector:
          if (SCM_BYTEVECTOR_ELEMENT_TYPE (x) == SCM_ARRAY_ELEMENT_TYPE_VU8)
            return class_bytevector;
          else
            return class_uvec;
	case scm_tc7_string:
	  return scm_class_string;
        case scm_tc7_number:
          switch SCM_TYP16 (x) {
          case scm_tc16_big:
            return scm_class_integer;
          case scm_tc16_real:
            return scm_class_real;
          case scm_tc16_complex:
            return scm_class_complex;
	  case scm_tc16_fraction:
	    return scm_class_fraction;
          }
	case scm_tc7_program:
	  if (SCM_PROGRAM_IS_PRIMITIVE_GENERIC (x) && *SCM_SUBR_GENERIC (x))
	    return scm_class_primitive_generic;
	  else
	    return scm_class_procedure;

	case scm_tc7_smob:
	  {
	    scm_t_bits type = SCM_TYP16 (x);
	    if (type != scm_tc16_port_with_ps)
	      return scm_smob_class[SCM_TC2SMOBNUM (type)];
	    x = SCM_PORT_WITH_PS_PORT (x);
	    /* fall through to ports */
	  }
	case scm_tc7_port:
	  return scm_port_class[(SCM_WRTNG & SCM_CELL_WORD_0 (x)
				 ? (SCM_RDNG & SCM_CELL_WORD_0 (x)
				    ? SCM_INOUT_PCLASS_INDEX | SCM_PTOBNUM (x)
				    : SCM_OUT_PCLASS_INDEX | SCM_PTOBNUM (x))
				 : SCM_IN_PCLASS_INDEX | SCM_PTOBNUM (x))];
	case scm_tcs_struct:
	  if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS_VALID)
	    return SCM_CLASS_OF (x);
	  else if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS)
	    {
	      /* Goops object */
	      if (! scm_is_false (SCM_OBJ_CLASS_REDEF (x)))
		scm_change_object_class (x,
					 SCM_CLASS_OF (x),         /* old */
					 SCM_OBJ_CLASS_REDEF (x)); /* new */
	      return SCM_CLASS_OF (x);
	    }
	  else
	    {
	      /* ordinary struct */
	      SCM handle = scm_struct_create_handle (SCM_STRUCT_VTABLE (x));
	      if (scm_is_true (SCM_STRUCT_TABLE_CLASS (SCM_CDR (handle))))
		return SCM_STRUCT_TABLE_CLASS (SCM_CDR (handle));
	      else
		{
		  SCM class, name;

		  name = SCM_STRUCT_TABLE_NAME (SCM_CDR (handle));
		  if (!scm_is_symbol (name))
		    name = scm_string_to_symbol (scm_nullstr);

		  class =
		    scm_make_extended_class_from_symbol (name,
							 SCM_STRUCT_APPLICABLE_P (x));
		  SCM_SET_STRUCT_TABLE_CLASS (SCM_CDR (handle), class);
		  return class;
		}
	    }
	default:
	  if (scm_is_pair (x))
	    return scm_class_pair;
	  else
	    return scm_class_unknown;
	}

    case scm_tc3_struct:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
      /* case scm_tc3_unused: */
      /* Never reached */
      break;
    }
  return scm_class_unknown;
}
#undef FUNC_NAME

/******************************************************************************
 *
 * Compute-cpl
 *
 *   This version doesn't fully handle multiple-inheritance. It serves
 *   only for booting classes and will be overloaded in Scheme
 *
 ******************************************************************************/

static SCM
map (SCM (*proc) (SCM), SCM ls)
{
  if (scm_is_null (ls))
    return ls;
  else
    {
      SCM res = scm_cons (proc (SCM_CAR (ls)), SCM_EOL);
      SCM h = res;
      ls = SCM_CDR (ls);
      while (!scm_is_null (ls))
	{
	  SCM_SETCDR (h, scm_cons (proc (SCM_CAR (ls)), SCM_EOL));
	  h = SCM_CDR (h);
	  ls = SCM_CDR (ls);
	}
      return res;
    }
}

static SCM
filter_cpl (SCM ls)
{
  SCM res = SCM_EOL;
  while (!scm_is_null (ls))
    {
      SCM el = SCM_CAR (ls);
      if (scm_is_false (scm_c_memq (el, res)))
	res = scm_cons (el, res);
      ls = SCM_CDR (ls);
    }
  return res;
}

static SCM
compute_cpl (SCM class)
{
  if (goops_loaded_p)
    return scm_call_1 (SCM_VARIABLE_REF (var_compute_cpl), class);
  else
    {
      SCM supers = SCM_SLOT (class, scm_si_direct_supers);
      SCM ls = scm_append (scm_acons (class, supers,
				      map (compute_cpl, supers)));
      return scm_reverse_x (filter_cpl (ls), SCM_EOL);
    }
}

/******************************************************************************
 *
 * compute-slots
 *
 ******************************************************************************/

static SCM
remove_duplicate_slots (SCM l, SCM res, SCM slots_already_seen)
{
  SCM tmp;

  if (scm_is_null (l))
    return res;

  tmp = SCM_CAAR (l);
  if (!scm_is_symbol (tmp))
    scm_misc_error ("%compute-slots", "bad slot name ~S", scm_list_1 (tmp));

  if (scm_is_false (scm_c_memq (tmp, slots_already_seen))) {
    res 	       = scm_cons (SCM_CAR (l), res);
    slots_already_seen = scm_cons (tmp, slots_already_seen);
  }

  return remove_duplicate_slots (SCM_CDR (l), res, slots_already_seen);
}

static SCM
build_slots_list (SCM dslots, SCM cpl)
{
  register SCM res = dslots;

  for (cpl = SCM_CDR (cpl); !scm_is_null (cpl); cpl = SCM_CDR (cpl))
    res = scm_append (scm_list_2 (SCM_SLOT (SCM_CAR (cpl),
					    scm_si_direct_slots),
				  res));

  /* res contains a list of slots. Remove slots which appears more than once */
  return remove_duplicate_slots (scm_reverse (res), SCM_EOL, SCM_EOL);
}

static SCM
maplist (SCM ls)
{
  SCM orig = ls;
  while (!scm_is_null (ls))
    {
      if (!scm_is_pair (SCM_CAR (ls)))
	SCM_SETCAR (ls, scm_cons (SCM_CAR (ls), SCM_EOL));
      ls = SCM_CDR (ls);
    }
  return orig;
}


SCM_DEFINE (scm_sys_compute_slots, "%compute-slots", 1, 0, 0,
	    (SCM class),
	    "Return a list consisting of the names of all slots belonging to\n"
	    "class @var{class}, i. e. the slots of @var{class} and of all of\n"
	    "its superclasses.")
#define FUNC_NAME s_scm_sys_compute_slots
{
  SCM_VALIDATE_CLASS (1, class);
  return build_slots_list (SCM_SLOT (class, scm_si_direct_slots),
			   SCM_SLOT (class, scm_si_cpl));
}
#undef FUNC_NAME


/******************************************************************************
 *
 * compute-getters-n-setters
 *
 *   This version doesn't handle slot options. It serves only for booting
 * classes and will be overloaded in Scheme.
 *
 ******************************************************************************/

SCM_KEYWORD (k_init_value, "init-value");
SCM_KEYWORD (k_init_thunk, "init-thunk");

static SCM
compute_getters_n_setters (SCM slots)
{
  SCM res = SCM_EOL;
  SCM *cdrloc = &res;
  long i   = 0;

  for (  ; !scm_is_null (slots); slots = SCM_CDR (slots))
    {
      SCM init = SCM_BOOL_F;
      SCM options = SCM_CDAR (slots);
      if (!scm_is_null (options))
	{
	  init = scm_get_keyword (k_init_value, options, 0);
	  if (init)
            {
              init = scm_primitive_eval (scm_list_3 (scm_sym_lambda,
                                                     SCM_EOL,
                                                     scm_list_2 (scm_sym_quote,
                                                                 init)));
            }
	  else
	    init = scm_get_keyword (k_init_thunk, options, SCM_BOOL_F);
	}
      *cdrloc = scm_cons (scm_cons (SCM_CAAR (slots),
				    scm_cons (init,
					      scm_from_int (i++))),
			  SCM_EOL);
      cdrloc = SCM_CDRLOC (*cdrloc);
    }
  return res;
}

/******************************************************************************
 *
 * initialize-object
 *
 ******************************************************************************/

/*fixme* Manufacture keywords in advance */
SCM
scm_i_get_keyword (SCM key, SCM l, long len, SCM default_value, const char *subr)
{
  long i;

  for (i = 0; i != len; i += 2)
    {
      SCM obj = SCM_CAR (l);

      if (!scm_is_keyword (obj))
	scm_misc_error (subr, "bad keyword: ~S", scm_list_1 (obj));
      else if (scm_is_eq (obj, key))
	return SCM_CADR (l);
      else
	l = SCM_CDDR (l);
    }

  return default_value;
}


SCM_DEFINE (scm_get_keyword, "get-keyword", 3, 0, 0,
	    (SCM key, SCM l, SCM default_value),
	    "Determine an associated value for the keyword @var{key} from\n"
	    "the list @var{l}.  The list @var{l} has to consist of an even\n"
	    "number of elements, where, starting with the first, every\n"
	    "second element is a keyword, followed by its associated value.\n"
	    "If @var{l} does not hold a value for @var{key}, the value\n"
	    "@var{default_value} is returned.")
#define FUNC_NAME s_scm_get_keyword
{
  long len;

  SCM_ASSERT (scm_is_keyword (key), key, SCM_ARG1, FUNC_NAME);
  len = scm_ilength (l);
  if (len < 0 || len % 2 == 1)
    scm_misc_error (FUNC_NAME, "Bad keyword-value list: ~S", scm_list_1 (l));

  return scm_i_get_keyword (key, l, len, default_value, FUNC_NAME);
}
#undef FUNC_NAME


SCM_KEYWORD (k_init_keyword, "init-keyword");

static SCM get_slot_value (SCM class, SCM obj, SCM slotdef);
static SCM set_slot_value (SCM class, SCM obj, SCM slotdef, SCM value);

SCM_DEFINE (scm_sys_initialize_object, "%initialize-object", 2, 0, 0,
	    (SCM obj, SCM initargs),
	    "Initialize the object @var{obj} with the given arguments\n"
	    "@var{initargs}.")
#define FUNC_NAME s_scm_sys_initialize_object
{
  SCM tmp, get_n_set, slots;
  SCM class       = SCM_CLASS_OF (obj);
  long n_initargs;

  SCM_VALIDATE_INSTANCE (1, obj);
  n_initargs = scm_ilength (initargs);
  SCM_ASSERT ((n_initargs & 1) == 0, initargs, SCM_ARG2, FUNC_NAME);

  get_n_set = SCM_SLOT (class, scm_si_getters_n_setters);
  slots     = SCM_SLOT (class, scm_si_slots);

  /* See for each slot how it must be initialized */
  for (;
       !scm_is_null (slots);
       get_n_set = SCM_CDR (get_n_set), slots = SCM_CDR (slots))
    {
      SCM slot_name  = SCM_CAR (slots);
      SCM slot_value = 0;

      if (!scm_is_null (SCM_CDR (slot_name)))
	{
	  /* This slot admits (perhaps) to be initialized at creation time */
	  long n = scm_ilength (SCM_CDR (slot_name));
	  if (n & 1) /* odd or -1 */
	    SCM_MISC_ERROR ("class contains bogus slot definition: ~S",
			    scm_list_1 (slot_name));
	  tmp 	= scm_i_get_keyword (k_init_keyword,
				     SCM_CDR (slot_name),
				     n,
				     0,
				     FUNC_NAME);
	  slot_name = SCM_CAR (slot_name);
	  if (tmp)
	    {
	      /* an initarg was provided for this slot */
	      if (!scm_is_keyword (tmp))
		SCM_MISC_ERROR ("initarg must be a keyword. It was ~S",
				scm_list_1 (tmp));
	      slot_value = scm_i_get_keyword (tmp,
					      initargs,
					      n_initargs,
					      0,
					      FUNC_NAME);
	    }
	}

      if (slot_value)
	/* set slot to provided value */
	set_slot_value (class, obj, SCM_CAR (get_n_set), slot_value);
      else
	{
	  /* set slot to its :init-form if it exists */
	  tmp = SCM_CADAR (get_n_set);
	  if (scm_is_true (tmp))
	    {
	      slot_value = get_slot_value (class, obj, SCM_CAR (get_n_set));
	      if (SCM_GOOPS_UNBOUNDP (slot_value))
                set_slot_value (class,
                                obj,
                                SCM_CAR (get_n_set),
                                scm_call_0 (tmp));
	    }
	}
    }

  return obj;
}
#undef FUNC_NAME

/* NOTE: The following macros are interdependent with code
 *       in goops.scm:compute-getters-n-setters
 */
#define SCM_GNS_INSTANCE_ALLOCATED_P(gns)	\
  (SCM_I_INUMP (SCM_CDDR (gns))			\
   || (scm_is_pair (SCM_CDDR (gns))		\
       && scm_is_pair (SCM_CDDDR (gns))		\
       && scm_is_pair (SCM_CDDDDR (gns))))
#define SCM_GNS_INDEX(gns)			\
  (SCM_I_INUMP (SCM_CDDR (gns))			\
   ? SCM_I_INUM (SCM_CDDR (gns))		\
   : scm_to_long (SCM_CAR (SCM_CDDDDR (gns))))
#define SCM_GNS_SIZE(gns)			\
  (SCM_I_INUMP (SCM_CDDR (gns))			\
   ? 1						\
   : scm_to_long (SCM_CADR (SCM_CDDDDR (gns))))

SCM_KEYWORD (k_class, "class");
SCM_KEYWORD (k_allocation, "allocation");
SCM_KEYWORD (k_instance, "instance");

SCM_DEFINE (scm_sys_prep_layout_x, "%prep-layout!", 1, 0, 0,
	    (SCM class),
	    "")
#define FUNC_NAME s_scm_sys_prep_layout_x
{
  SCM slots, getters_n_setters, nfields;
  unsigned long int n, i;
  char *s;
  SCM layout;

  SCM_VALIDATE_INSTANCE (1, class);
  slots = SCM_SLOT (class, scm_si_slots);
  getters_n_setters = SCM_SLOT (class, scm_si_getters_n_setters);
  nfields = SCM_SLOT (class, scm_si_nfields);
  if (!SCM_I_INUMP (nfields) || SCM_I_INUM (nfields) < 0)
    SCM_MISC_ERROR ("bad value in nfields slot: ~S",
		    scm_list_1 (nfields));
  n = 2 * SCM_I_INUM (nfields);
  if (n < sizeof (SCM_CLASS_CLASS_LAYOUT) - 1
      && SCM_SUBCLASSP (class, scm_class_class))
    SCM_MISC_ERROR ("class object doesn't have enough fields: ~S",
		    scm_list_1 (nfields));

  layout = scm_i_make_string (n, &s, 0);
  i = 0;
  while (scm_is_pair (getters_n_setters))
    {
      if (SCM_GNS_INSTANCE_ALLOCATED_P (SCM_CAR (getters_n_setters)))
	{
	  SCM type;
	  int len, index, size;
	  char p, a;

	  if (i >= n || !scm_is_pair (slots))
	    goto inconsistent;
	  
	  /* extract slot type */
	  len = scm_ilength (SCM_CDAR (slots));
	  type = scm_i_get_keyword (k_class, SCM_CDAR (slots),
				    len, SCM_BOOL_F, FUNC_NAME);
	  /* determine slot GC protection and access mode */
	  if (scm_is_false (type))
	    {
	      p = 'p';
	      a = 'w';
	    }
	  else
	    {
	      if (!SCM_CLASSP (type))
		SCM_MISC_ERROR ("bad slot class", SCM_EOL);
	      else if (SCM_SUBCLASSP (type, scm_class_foreign_slot))
		{
		  if (SCM_SUBCLASSP (type, scm_class_self))
		    p = 's';
		  else if (SCM_SUBCLASSP (type, scm_class_protected))
		    p = 'p';
		  else
		    p = 'u';

		  if (SCM_SUBCLASSP (type, scm_class_opaque))
		    a = 'o';
		  else if (SCM_SUBCLASSP (type, scm_class_read_only))
		    a = 'r';
		  else if (SCM_SUBCLASSP (type, scm_class_hidden))
		    a = 'h';
		  else
		    a = 'w';
		}
	      else
		{
		  p = 'p';
		  a = 'w';
		}
	    }
      
	  index = SCM_GNS_INDEX (SCM_CAR (getters_n_setters));
	  if (index != (i >> 1))
	    goto inconsistent;
	  size = SCM_GNS_SIZE (SCM_CAR (getters_n_setters));
	  while (size)
	    {
	      s[i++] = p;
	      s[i++] = a;
	      --size;
	    }
	}
      slots = SCM_CDR (slots);
      getters_n_setters = SCM_CDR (getters_n_setters);
    }
  if (!scm_is_null (slots))
    {
    inconsistent:
      SCM_MISC_ERROR ("inconsistent getters-n-setters", SCM_EOL);
    }
  SCM_SET_VTABLE_LAYOUT (class, scm_string_to_symbol (layout));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void prep_hashsets (SCM);

SCM_DEFINE (scm_sys_inherit_magic_x, "%inherit-magic!", 2, 0, 0,
	    (SCM class, SCM dsupers),
	    "")
#define FUNC_NAME s_scm_sys_inherit_magic_x
{
  SCM_VALIDATE_INSTANCE (1, class);
  scm_i_struct_inherit_vtable_magic (SCM_CLASS_OF (class), class);
  SCM_SET_CLASS_FLAGS (class, SCM_CLASSF_GOOPS_OR_VALID);

  prep_hashsets (class);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
prep_hashsets (SCM class)
{
  unsigned int i;

  for (i = 0; i < 8; ++i)
    SCM_SET_HASHSET (class, i, scm_c_uniform32 (goops_rstate));
}

/******************************************************************************/

SCM
scm_basic_basic_make_class (SCM class, SCM name, SCM dsupers, SCM dslots)
{
  SCM z, cpl, slots, nfields, g_n_s;

  /* Allocate one instance */
  z = scm_make_struct (class, SCM_INUM0, SCM_EOL);

  /* Initialize its slots */
  SCM_SET_SLOT (z, scm_si_direct_supers, dsupers);
  cpl   = compute_cpl (z);
  slots = build_slots_list (maplist (dslots), cpl);
  nfields = scm_from_int (scm_ilength (slots));
  g_n_s = compute_getters_n_setters (slots);

  SCM_SET_SLOT (z, scm_vtable_index_name, name);
  SCM_SET_SLOT (z, scm_si_direct_slots, dslots);
  SCM_SET_SLOT (z, scm_si_direct_subclasses, SCM_EOL);
  SCM_SET_SLOT (z, scm_si_direct_methods, SCM_EOL);
  SCM_SET_SLOT (z, scm_si_cpl, cpl);
  SCM_SET_SLOT (z, scm_si_slots, slots);
  SCM_SET_SLOT (z, scm_si_nfields, nfields);
  SCM_SET_SLOT (z, scm_si_getters_n_setters, g_n_s);
  SCM_SET_SLOT (z, scm_si_redefined, SCM_BOOL_F);

  /* Add this class in the direct-subclasses slot of dsupers */
  {
    SCM tmp;
    for (tmp = dsupers; !scm_is_null (tmp); tmp = SCM_CDR (tmp))
      SCM_SET_SLOT (SCM_CAR (tmp), scm_si_direct_subclasses,
		    scm_cons (z, SCM_SLOT (SCM_CAR (tmp),
					   scm_si_direct_subclasses)));
  }

  return z;
}

SCM
scm_basic_make_class (SCM class, SCM name, SCM dsupers, SCM dslots)
{
  SCM z = scm_basic_basic_make_class (class, name, dsupers, dslots);
  scm_sys_prep_layout_x (z);
  scm_sys_inherit_magic_x (z, dsupers);
  return z;
}

/******************************************************************************/

SCM_SYMBOL (sym_layout, "layout");
SCM_SYMBOL (sym_flags, "flags");
SCM_SYMBOL (sym_self, "%self");
SCM_SYMBOL (sym_instance_finalizer, "instance-finalizer");
SCM_SYMBOL (sym_reserved_0, "%reserved-0");
SCM_SYMBOL (sym_reserved_1, "%reserved-1");
SCM_SYMBOL (sym_print, "print");
SCM_SYMBOL (sym_procedure, "procedure");
SCM_SYMBOL (sym_setter, "setter");
SCM_SYMBOL (sym_redefined, "redefined");
SCM_SYMBOL (sym_h0, "h0");
SCM_SYMBOL (sym_h1, "h1");
SCM_SYMBOL (sym_h2, "h2");
SCM_SYMBOL (sym_h3, "h3");
SCM_SYMBOL (sym_h4, "h4");
SCM_SYMBOL (sym_h5, "h5");
SCM_SYMBOL (sym_h6, "h6");
SCM_SYMBOL (sym_h7, "h7");
SCM_SYMBOL (sym_name, "name");
SCM_SYMBOL (sym_direct_supers, "direct-supers");
SCM_SYMBOL (sym_direct_slots, "direct-slots");
SCM_SYMBOL (sym_direct_subclasses, "direct-subclasses");
SCM_SYMBOL (sym_direct_methods, "direct-methods");
SCM_SYMBOL (sym_cpl, "cpl");
SCM_SYMBOL (sym_default_slot_definition_class, "default-slot-definition-class");
SCM_SYMBOL (sym_slots, "slots");
SCM_SYMBOL (sym_getters_n_setters, "getters-n-setters");
SCM_SYMBOL (sym_keyword_access, "keyword-access");
SCM_SYMBOL (sym_nfields, "nfields");


static SCM
build_class_class_slots ()
{
  /* has to be kept in sync with SCM_VTABLE_BASE_LAYOUT and
     SCM_CLASS_CLASS_LAYOUT */
  return scm_list_n (
    scm_list_3 (sym_layout, k_class, scm_class_protected_read_only),
    scm_list_3 (sym_flags, k_class, scm_class_hidden),
    scm_list_3 (sym_self, k_class, scm_class_self),
    scm_list_3 (sym_instance_finalizer, k_class, scm_class_hidden),
    scm_list_1 (sym_print),
    scm_list_3 (sym_name, k_class, scm_class_protected_hidden),
    scm_list_3 (sym_reserved_0, k_class, scm_class_hidden),
    scm_list_3 (sym_reserved_1, k_class, scm_class_hidden),
    scm_list_1 (sym_redefined),
    scm_list_3 (sym_h0, k_class, scm_class_int),
    scm_list_3 (sym_h1, k_class, scm_class_int),
    scm_list_3 (sym_h2, k_class, scm_class_int),
    scm_list_3 (sym_h3, k_class, scm_class_int),
    scm_list_3 (sym_h4, k_class, scm_class_int),
    scm_list_3 (sym_h5, k_class, scm_class_int),
    scm_list_3 (sym_h6, k_class, scm_class_int),
    scm_list_3 (sym_h7, k_class, scm_class_int),
    scm_list_1 (sym_direct_supers),
    scm_list_1 (sym_direct_slots),
    scm_list_1 (sym_direct_subclasses),
    scm_list_1 (sym_direct_methods),
    scm_list_1 (sym_cpl),
    scm_list_1 (sym_default_slot_definition_class),
    scm_list_1 (sym_slots),
    scm_list_1 (sym_getters_n_setters),
    scm_list_1 (sym_keyword_access),
    scm_list_1 (sym_nfields),
    SCM_UNDEFINED);
}

static void
create_basic_classes (void)
{
  /* SCM slots_of_class = build_class_class_slots (); */

  /**** <class> ****/
  SCM cs = scm_from_locale_string (SCM_CLASS_CLASS_LAYOUT);
  SCM name = scm_from_latin1_symbol ("<class>");
  scm_class_class = scm_make_vtable_vtable (cs, SCM_INUM0, SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_class, (SCM_CLASSF_GOOPS_OR_VALID
					 | SCM_CLASSF_METACLASS));

  SCM_SET_SLOT (scm_class_class, scm_vtable_index_name, name);
  SCM_SET_SLOT (scm_class_class, scm_si_direct_supers, SCM_EOL);  /* will be changed */
  /* SCM_SET_SLOT (scm_class_class, scm_si_direct_slots, slots_of_class); */
  SCM_SET_SLOT (scm_class_class, scm_si_direct_subclasses, SCM_EOL);
  SCM_SET_SLOT (scm_class_class, scm_si_direct_methods, SCM_EOL);
  SCM_SET_SLOT (scm_class_class, scm_si_cpl, SCM_EOL);  /* will be changed */
  /* SCM_SET_SLOT (scm_class_class, scm_si_slots, slots_of_class); */
  SCM_SET_SLOT (scm_class_class, scm_si_nfields, scm_from_int (SCM_N_CLASS_SLOTS));
  /* SCM_SET_SLOT (scm_class_class, scm_si_getters_n_setters,
                   compute_getters_n_setters (slots_of_class)); */
  SCM_SET_SLOT (scm_class_class, scm_si_redefined, SCM_BOOL_F);

  prep_hashsets (scm_class_class);

  DEFVAR(name, scm_class_class);

  /**** <top> ****/
  name = scm_from_latin1_symbol ("<top>");
  scm_class_top = scm_basic_make_class (scm_class_class, name,
                                        SCM_EOL, SCM_EOL);

  DEFVAR(name, scm_class_top);

  /**** <object> ****/
  name	 = scm_from_latin1_symbol ("<object>");
  scm_class_object = scm_basic_make_class (scm_class_class, name,
                                           scm_list_1 (scm_class_top), SCM_EOL);

  DEFVAR (name, scm_class_object);

  /* <top> <object> and <class> were partially initialized. Correct them here */
  SCM_SET_SLOT (scm_class_object, scm_si_direct_subclasses, scm_list_1 (scm_class_class));

  SCM_SET_SLOT (scm_class_class, scm_si_direct_supers, scm_list_1 (scm_class_object));
  SCM_SET_SLOT (scm_class_class, scm_si_cpl, scm_list_3 (scm_class_class, scm_class_object, scm_class_top));
}

/******************************************************************************/

SCM_DEFINE (scm_instance_p, "instance?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is an instance.")
#define FUNC_NAME s_scm_instance_p
{
  return scm_from_bool (SCM_INSTANCEP (obj));
}
#undef FUNC_NAME


/******************************************************************************
 *
 * Meta object accessors
 *
 ******************************************************************************/
SCM_DEFINE (scm_class_name, "class-name",  1, 0, 0,
	    (SCM obj),
	    "Return the class name of @var{obj}.")
#define FUNC_NAME s_scm_class_name
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_name);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_supers, "class-direct-supers", 1, 0, 0,
	    (SCM obj),
	    "Return the direct superclasses of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_supers
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_supers);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_slots, "class-direct-slots", 1, 0, 0,
	    (SCM obj),
	    "Return the direct slots of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_slots
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_slots);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_subclasses, "class-direct-subclasses", 1, 0, 0,
	    (SCM obj),
	    "Return the direct subclasses of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_subclasses
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref(obj, sym_direct_subclasses);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_methods, "class-direct-methods", 1, 0, 0,
	    (SCM obj),
	    "Return the direct methods of the class @var{obj}")
#define FUNC_NAME s_scm_class_direct_methods
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_methods);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_precedence_list, "class-precedence-list", 1, 0, 0,
	    (SCM obj),
	    "Return the class precedence list of the class @var{obj}.")
#define FUNC_NAME s_scm_class_precedence_list
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_cpl);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_slots, "class-slots", 1, 0, 0,
	    (SCM obj),
	    "Return the slot list of the class @var{obj}.")
#define FUNC_NAME s_scm_class_slots
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_slots);
}
#undef FUNC_NAME

SCM_DEFINE (scm_generic_function_name, "generic-function-name", 1, 0, 0,
	    (SCM obj),
	    "Return the name of the generic function @var{obj}.")
#define FUNC_NAME s_scm_generic_function_name
{
  SCM_VALIDATE_GENERIC (1, obj);
  return scm_procedure_property (obj, scm_sym_name);
}
#undef FUNC_NAME

SCM_SYMBOL (sym_methods, "methods");
SCM_SYMBOL (sym_extended_by, "extended-by");
SCM_SYMBOL (sym_extends, "extends");

static
SCM fold_downward_gf_methods (SCM method_lists, SCM gf)
{
  SCM gfs = scm_slot_ref (gf, sym_extended_by);
  method_lists = scm_cons (scm_slot_ref (gf, sym_methods), method_lists);
  while (!scm_is_null (gfs))
    {
      method_lists = fold_downward_gf_methods (method_lists, SCM_CAR (gfs));
      gfs = SCM_CDR (gfs);
    }
  return method_lists;
}

static
SCM fold_upward_gf_methods (SCM method_lists, SCM gf)
{
  if (SCM_IS_A_P (gf, scm_class_extended_generic))
    {
      SCM gfs = scm_slot_ref (gf, sym_extends);
      while (!scm_is_null (gfs))
	{
	  SCM methods = scm_slot_ref (SCM_CAR (gfs), sym_methods);
	  method_lists = fold_upward_gf_methods (scm_cons (methods,
							   method_lists),
						 SCM_CAR (gfs));
	  gfs = SCM_CDR (gfs);
	}
    }
  return method_lists;
}

SCM_DEFINE (scm_generic_function_methods, "generic-function-methods", 1, 0, 0,
	    (SCM obj),
	    "Return the methods of the generic function @var{obj}.")
#define FUNC_NAME s_scm_generic_function_methods
{
  SCM methods;
  SCM_VALIDATE_GENERIC (1, obj);
  methods = fold_upward_gf_methods (SCM_EOL, obj);
  methods = fold_downward_gf_methods (methods, obj);
  return scm_append (methods);
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_generic_function, "method-generic-function", 1, 0, 0,
	    (SCM obj),
	    "Return the generic function for the method @var{obj}.")
#define FUNC_NAME s_scm_method_generic_function
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, scm_from_latin1_symbol ("generic-function"));
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_specializers, "method-specializers", 1, 0, 0,
	    (SCM obj),
	    "Return specializers of the method @var{obj}.")
#define FUNC_NAME s_scm_method_specializers
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, scm_from_latin1_symbol ("specializers"));
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_procedure, "method-procedure", 1, 0, 0,
	    (SCM obj),
	    "Return the procedure of the method @var{obj}.")
#define FUNC_NAME s_scm_method_procedure
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, sym_procedure);
}
#undef FUNC_NAME

/******************************************************************************
 *
 * S l o t   a c c e s s
 *
 ******************************************************************************/

SCM_DEFINE (scm_make_unbound, "make-unbound", 0, 0, 0,
	    (),
	    "Return the unbound value.")
#define FUNC_NAME s_scm_make_unbound
{
  return SCM_GOOPS_UNBOUND;
}
#undef FUNC_NAME

SCM_DEFINE (scm_unbound_p, "unbound?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is unbound.")
#define FUNC_NAME s_scm_unbound_p
{
  return SCM_GOOPS_UNBOUNDP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_assert_bound, "assert-bound", 2, 0, 0,
	    (SCM value, SCM obj),
	    "Return @var{value} if it is bound, and invoke the\n"
	    "@var{slot-unbound} method of @var{obj} if it is not.")
#define FUNC_NAME s_scm_assert_bound
{
  if (SCM_GOOPS_UNBOUNDP (value))
    return scm_call_1 (SCM_VARIABLE_REF (var_slot_unbound), obj);
  return value;
}
#undef FUNC_NAME

SCM_DEFINE (scm_at_assert_bound_ref, "@assert-bound-ref", 2, 0, 0,
	    (SCM obj, SCM index),
	    "Like @code{assert-bound}, but use @var{index} for accessing\n"
	    "the value from @var{obj}.")
#define FUNC_NAME s_scm_at_assert_bound_ref
{
  SCM value = SCM_SLOT (obj, scm_to_int (index));
  if (SCM_GOOPS_UNBOUNDP (value))
    return scm_call_1 (SCM_VARIABLE_REF (var_slot_unbound), obj);
  return value;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_fast_slot_ref, "%fast-slot-ref", 2, 0, 0,
	    (SCM obj, SCM index),
	    "Return the slot value with index @var{index} from @var{obj}.")
#define FUNC_NAME s_scm_sys_fast_slot_ref
{
  scm_t_bits i;

  SCM_VALIDATE_INSTANCE (1, obj);
  i = scm_to_unsigned_integer (index, 0,
			       SCM_I_INUM (SCM_SLOT (SCM_CLASS_OF (obj),
						     scm_si_nfields))
			       - 1);
  return SCM_SLOT (obj, i);
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_fast_slot_set_x, "%fast-slot-set!", 3, 0, 0,
	    (SCM obj, SCM index, SCM value),
	    "Set the slot with index @var{index} in @var{obj} to\n"
	    "@var{value}.")
#define FUNC_NAME s_scm_sys_fast_slot_set_x
{
  scm_t_bits i;

  SCM_VALIDATE_INSTANCE (1, obj);
  i = scm_to_unsigned_integer (index, 0,
			       SCM_I_INUM (SCM_SLOT (SCM_CLASS_OF (obj),
						     scm_si_nfields))
			       - 1);

  SCM_SET_SLOT (obj, i, value);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/** Utilities **/

/* In the future, this function will return the effective slot
 * definition associated with SLOT_NAME.  Now it just returns some of
 * the information which will be stored in the effective slot
 * definition.
 */

static SCM
slot_definition_using_name (SCM class, SCM slot_name)
{
  register SCM slots = SCM_SLOT (class, scm_si_getters_n_setters);
  for (; !scm_is_null (slots); slots = SCM_CDR (slots))
    if (SCM_CAAR (slots) == slot_name)
      return SCM_CAR (slots);
  return SCM_BOOL_F;
}

static SCM
get_slot_value (SCM class SCM_UNUSED, SCM obj, SCM slotdef)
#define FUNC_NAME "%get-slot-value"
{
  SCM access = SCM_CDDR (slotdef);
  /* Two cases here:
   *	- access is an integer (the offset of this slot in the slots vector)
   *	- otherwise (car access) is the getter function to apply
   *
   * Instances have never more than SCM_MOST_POSITIVE_FIXNUM slots, so
   * we can just assume fixnums here.
   */
  if (SCM_I_INUMP (access))
    /* Don't poke at the slots directly, because scm_struct_ref handles the
       access bits for us. */
    return scm_struct_ref (obj, access);
  else
    return scm_call_1 (SCM_CAR (access), obj);
}
#undef FUNC_NAME

static SCM
get_slot_value_using_name (SCM class, SCM obj, SCM slot_name)
{
  SCM slotdef = slot_definition_using_name (class, slot_name);
  if (scm_is_true (slotdef))
    return get_slot_value (class, obj, slotdef);
  else
    return scm_call_3 (SCM_VARIABLE_REF (var_slot_missing), class, obj, slot_name);
}

static SCM
set_slot_value (SCM class SCM_UNUSED, SCM obj, SCM slotdef, SCM value)
#define FUNC_NAME "%set-slot-value"
{
  SCM access = SCM_CDDR (slotdef);
  /* Two cases here:
   *	- access is an integer (the offset of this slot in the slots vector)
   *	- otherwise (cadr access) is the setter function to apply
   *
   * Instances have never more than SCM_MOST_POSITIVE_FIXNUM slots, so
   * we can just assume fixnums here.
   */
  if (SCM_I_INUMP (access))
    /* obey permissions bits via going through struct-set! */
    scm_struct_set_x (obj, access, value);
  else
    /* ((cadr l) obj value) */
    scm_call_2 (SCM_CADR (access), obj, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
set_slot_value_using_name (SCM class, SCM obj, SCM slot_name, SCM value)
{
  SCM slotdef = slot_definition_using_name (class, slot_name);
  if (scm_is_true (slotdef))
    return set_slot_value (class, obj, slotdef, value);
  else
    return scm_call_4 (SCM_VARIABLE_REF (var_slot_missing), class, obj, slot_name, value);
}

static SCM
test_slot_existence (SCM class SCM_UNUSED, SCM obj, SCM slot_name)
{
  register SCM l;

  for (l = SCM_ACCESSORS_OF (obj); !scm_is_null (l); l = SCM_CDR (l))
    if (scm_is_eq (SCM_CAAR (l), slot_name))
      return SCM_BOOL_T;

  return SCM_BOOL_F;
}

		/* ======================================== */

SCM_DEFINE (scm_slot_ref_using_class, "slot-ref-using-class", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_ref_using_class
{
  SCM res;

  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  res = get_slot_value_using_name (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return scm_call_3 (SCM_VARIABLE_REF (var_slot_unbound), class, obj, slot_name);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_slot_set_using_class_x, "slot-set-using-class!", 4, 0, 0,
	    (SCM class, SCM obj, SCM slot_name, SCM value),
	    "")
#define FUNC_NAME s_scm_slot_set_using_class_x
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  return set_slot_value_using_name (class, obj, slot_name, value);
}
#undef FUNC_NAME


SCM_DEFINE (scm_slot_bound_using_class_p, "slot-bound-using-class?", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_bound_using_class_p
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value_using_name (class, obj, slot_name))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_exists_using_class_p, "slot-exists-using-class?", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_exists_using_class_p
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);
  return test_slot_existence (class, obj, slot_name);
}
#undef FUNC_NAME


		/* ======================================== */

SCM_DEFINE (scm_slot_ref, "slot-ref", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return the value from @var{obj}'s slot with the name\n"
	    "@var{slot_name}.")
#define FUNC_NAME s_scm_slot_ref
{
  SCM res, class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS (obj, class);

  res = get_slot_value_using_name (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return scm_call_3 (SCM_VARIABLE_REF (var_slot_unbound), class, obj, slot_name);
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_set_x, "slot-set!", 3, 0, 0,
	    (SCM obj, SCM slot_name, SCM value),
	    "Set the slot named @var{slot_name} of @var{obj} to @var{value}.")
#define FUNC_NAME s_scm_slot_set_x
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS(obj, class);

  return set_slot_value_using_name (class, obj, slot_name, value);
}
#undef FUNC_NAME

const char *scm_s_slot_set_x = s_scm_slot_set_x;

SCM_DEFINE (scm_slot_bound_p, "slot-bound?", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return @code{#t} if the slot named @var{slot_name} of @var{obj}\n"
	    "is bound.")
#define FUNC_NAME s_scm_slot_bound_p
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS(obj, class);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value_using_name (class,
							 obj,
							 slot_name))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_exists_p, "slot-exists?", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return @code{#t} if @var{obj} has a slot named @var{slot_name}.")
#define FUNC_NAME s_scm_slot_exists_p
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  SCM_VALIDATE_SYMBOL (2, slot_name);
  TEST_CHANGE_CLASS (obj, class);

  return test_slot_existence (class, obj, slot_name);
}
#undef FUNC_NAME


/******************************************************************************
 *
 * %allocate-instance (the low level instance allocation primitive)
 *
 ******************************************************************************/

static void clear_method_cache (SCM);

SCM_DEFINE (scm_sys_allocate_instance, "%allocate-instance", 2, 0, 0,
	    (SCM class, SCM initargs),
	    "Create a new instance of class @var{class} and initialize it\n"
	    "from the arguments @var{initargs}.")
#define FUNC_NAME s_scm_sys_allocate_instance
{
  SCM obj;
  scm_t_signed_bits n, i;
  SCM layout;

  SCM_VALIDATE_CLASS (1, class);

  /* FIXME: duplicates some of scm_make_struct. */

  n = SCM_I_INUM (SCM_SLOT (class, scm_si_nfields));
  obj = scm_i_alloc_struct (SCM_STRUCT_DATA (class), n);

  layout = SCM_VTABLE_LAYOUT (class);

  /* Set all SCM-holding slots to unbound */
  for (i = 0; i < n; i++)
    {
      scm_t_wchar c = scm_i_symbol_ref (layout, i*2);
      if (c == 'p')
        SCM_STRUCT_DATA (obj)[i] = SCM_UNPACK (SCM_GOOPS_UNBOUND);
      else if (c == 's')
        SCM_STRUCT_DATA (obj)[i] = SCM_UNPACK (obj);
      else
        SCM_STRUCT_DATA (obj)[i] = 0;
    }

  if (SCM_CLASS_FLAGS (class) & SCM_CLASSF_PURE_GENERIC)
    clear_method_cache (obj);

  return obj;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_set_object_setter_x, "%set-object-setter!", 2, 0, 0,
	    (SCM obj, SCM setter),
	    "")
#define FUNC_NAME s_scm_sys_set_object_setter_x
{
  SCM_ASSERT (SCM_STRUCTP (obj)
              && (SCM_OBJ_CLASS_FLAGS (obj) & SCM_CLASSF_PURE_GENERIC),
	      obj,
	      SCM_ARG1,
	      FUNC_NAME);
  SCM_SET_GENERIC_SETTER (obj, setter);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/******************************************************************************
 *
 * %modify-instance (used by change-class to modify in place)
 *
 ******************************************************************************/

SCM_DEFINE (scm_sys_modify_instance, "%modify-instance", 2, 0, 0,
	    (SCM old, SCM new),
	    "")
#define FUNC_NAME s_scm_sys_modify_instance
{
  SCM_VALIDATE_INSTANCE (1, old);
  SCM_VALIDATE_INSTANCE (2, new);

  /* Exchange the data contained in old and new. We exchange rather than
   * scratch the old value with new to be correct with GC.
   * See "Class redefinition protocol above".
   */
  SCM_CRITICAL_SECTION_START;
  {
    scm_t_bits word0, word1;
    word0 = SCM_CELL_WORD_0 (old);
    word1 = SCM_CELL_WORD_1 (old);
    SCM_SET_CELL_WORD_0 (old, SCM_CELL_WORD_0 (new));
    SCM_SET_CELL_WORD_1 (old, SCM_CELL_WORD_1 (new));
    SCM_SET_CELL_WORD_0 (new, word0);
    SCM_SET_CELL_WORD_1 (new, word1);
  }
  SCM_CRITICAL_SECTION_END;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_modify_class, "%modify-class", 2, 0, 0,
	    (SCM old, SCM new),
	    "")
#define FUNC_NAME s_scm_sys_modify_class
{
  SCM_VALIDATE_CLASS (1, old);
  SCM_VALIDATE_CLASS (2, new);

  SCM_CRITICAL_SECTION_START;
  {
    scm_t_bits word0, word1;
    word0 = SCM_CELL_WORD_0 (old);
    word1 = SCM_CELL_WORD_1 (old);
    SCM_SET_CELL_WORD_0 (old, SCM_CELL_WORD_0 (new));
    SCM_SET_CELL_WORD_1 (old, SCM_CELL_WORD_1 (new));
    SCM_STRUCT_DATA (old)[scm_vtable_index_self] = SCM_UNPACK (old);
    SCM_SET_CELL_WORD_0 (new, word0);
    SCM_SET_CELL_WORD_1 (new, word1);
    SCM_STRUCT_DATA (new)[scm_vtable_index_self] = SCM_UNPACK (new);
  }
  SCM_CRITICAL_SECTION_END;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_invalidate_class, "%invalidate-class", 1, 0, 0,
	    (SCM class),
	    "")
#define FUNC_NAME s_scm_sys_invalidate_class
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_CLEAR_CLASS_FLAGS (class, SCM_CLASSF_GOOPS_VALID);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* When instances change class, they finally get a new body, but
 * before that, they go through purgatory in hell.  Odd as it may
 * seem, this data structure saves us from eternal suffering in
 * infinite recursions.
 */

static scm_t_bits **hell;
static long n_hell = 1;		/* one place for the evil one himself */
static long hell_size = 4;
static SCM hell_mutex;

static long
burnin (SCM o)
{
  long i;
  for (i = 1; i < n_hell; ++i)
    if (SCM_STRUCT_DATA (o) == hell[i])
      return i;
  return 0;
}

static void
go_to_hell (void *o)
{
  SCM obj = SCM_PACK ((scm_t_bits) o);
  scm_lock_mutex (hell_mutex);
  if (n_hell >= hell_size)
    {
      hell_size *= 2;
      hell = scm_realloc (hell, hell_size * sizeof(*hell));
    }
  hell[n_hell++] = SCM_STRUCT_DATA (obj);
  scm_unlock_mutex (hell_mutex);
}

static void
go_to_heaven (void *o)
{
  scm_lock_mutex (hell_mutex);
  hell[burnin (SCM_PACK ((scm_t_bits) o))] = hell[--n_hell];
  scm_unlock_mutex (hell_mutex);
}


SCM_SYMBOL (scm_sym_change_class, "change-class");

static SCM
purgatory (void *args)
{
  return scm_apply_0 (SCM_VARIABLE_REF (var_change_class),
		      SCM_PACK ((scm_t_bits) args));
}

/* This function calls the generic function change-class for all
 * instances which aren't currently undergoing class change.
 */

void
scm_change_object_class (SCM obj, SCM old_class SCM_UNUSED, SCM new_class)
{
  if (!burnin (obj))
    scm_internal_dynamic_wind (go_to_hell, purgatory, go_to_heaven,
			       (void *) SCM_UNPACK (scm_list_2 (obj, new_class)),
			       (void *) SCM_UNPACK (obj));
}

/******************************************************************************
 *
 *   GGGG                FFFFF
 *  G                    F
 *  G  GG                FFF
 *  G   G                F
 *   GGG  E N E R I C    F    U N C T I O N S
 *
 * This implementation provides
 *	- generic functions (with class specializers)
 *	- multi-methods
 *	- next-method
 *	- a hard-coded MOP for standard gf, which can be overloaded for non-std gf
 *
 ******************************************************************************/

SCM_KEYWORD (k_name, "name");

SCM_GLOBAL_SYMBOL (scm_sym_args, "args");


SCM
scm_apply_generic (SCM gf, SCM args)
{
  return scm_apply (SCM_STRUCT_PROCEDURE (gf), args, SCM_EOL);
}

SCM
scm_call_generic_0 (SCM gf)
{
  return scm_call_0 (SCM_STRUCT_PROCEDURE (gf));
}

SCM
scm_call_generic_1 (SCM gf, SCM a1)
{
  return scm_call_1 (SCM_STRUCT_PROCEDURE (gf), a1);
}

SCM
scm_call_generic_2 (SCM gf, SCM a1, SCM a2)
{
  return scm_call_2 (SCM_STRUCT_PROCEDURE (gf), a1, a2);
}

SCM
scm_call_generic_3 (SCM gf, SCM a1, SCM a2, SCM a3)
{
  return scm_call_3 (SCM_STRUCT_PROCEDURE (gf), a1, a2, a3);
}

SCM_SYMBOL (sym_delayed_compile, "delayed-compile");
static SCM
make_dispatch_procedure (SCM gf)
{
  static SCM var = SCM_BOOL_F;
  if (var == SCM_BOOL_F)
    var = scm_module_variable (scm_c_resolve_module ("oop goops dispatch"),
                               sym_delayed_compile);
  return scm_call_1 (SCM_VARIABLE_REF (var), gf);
}

static void
clear_method_cache (SCM gf)
{
  SCM_SET_GENERIC_DISPATCH_PROCEDURE (gf, make_dispatch_procedure (gf));
  SCM_CLEAR_GENERIC_EFFECTIVE_METHODS (gf);
}

SCM_DEFINE (scm_sys_invalidate_method_cache_x, "%invalidate-method-cache!", 1, 0, 0,
	    (SCM gf),
	    "")
#define FUNC_NAME s_scm_sys_invalidate_method_cache_x
{
  SCM_ASSERT (SCM_PUREGENERICP (gf), gf, SCM_ARG1, FUNC_NAME);
  clear_method_cache (gf);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_generic_capability_p, "generic-capability?", 1, 0, 0,
	    (SCM proc),
	    "")
#define FUNC_NAME s_scm_generic_capability_p
{
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
	      proc, SCM_ARG1, FUNC_NAME);
  return (SCM_PRIMITIVE_GENERIC_P (proc) ? SCM_BOOL_T : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_enable_primitive_generic_x, "enable-primitive-generic!", 0, 0, 1,
	    (SCM subrs),
	    "")
#define FUNC_NAME s_scm_enable_primitive_generic_x
{
  SCM_VALIDATE_REST_ARGUMENT (subrs);
  while (!scm_is_null (subrs))
    {
      SCM subr = SCM_CAR (subrs);
      SCM_ASSERT (SCM_PRIMITIVE_GENERIC_P (subr), subr, SCM_ARGn, FUNC_NAME);
      SCM_SET_SUBR_GENERIC (subr,
                            scm_make (scm_list_3 (scm_class_generic,
                                                  k_name,
                                                  SCM_SUBR_NAME (subr))));
      subrs = SCM_CDR (subrs);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_primitive_generic_x, "set-primitive-generic!", 2, 0, 0,
	    (SCM subr, SCM generic),
	    "")
#define FUNC_NAME s_scm_set_primitive_generic_x
{
  SCM_ASSERT (SCM_PRIMITIVE_GENERIC_P (subr), subr, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_PUREGENERICP (generic), generic, SCM_ARG2, FUNC_NAME);
  SCM_SET_SUBR_GENERIC (subr, generic);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_primitive_generic_generic, "primitive-generic-generic", 1, 0, 0,
	    (SCM subr),
	    "")
#define FUNC_NAME s_scm_primitive_generic_generic
{
  if (SCM_PRIMITIVE_GENERIC_P (subr))
    {
      if (!*SCM_SUBR_GENERIC (subr))
	scm_enable_primitive_generic_x (scm_list_1 (subr));
      return *SCM_SUBR_GENERIC (subr);
    }
  SCM_WRONG_TYPE_ARG (SCM_ARG1, subr);
}
#undef FUNC_NAME

typedef struct t_extension {
  struct t_extension *next;
  SCM extended;
  SCM extension;
} t_extension;


/* Hint for `scm_gc_malloc ()' et al. when allocating `t_extension'
   objects.  */
static const char extension_gc_hint[] = "GOOPS extension";

static t_extension *extensions = 0;

void
scm_c_extend_primitive_generic (SCM extended, SCM extension)
{
  if (goops_loaded_p)
    {
      SCM gf, gext;
      if (!*SCM_SUBR_GENERIC (extended))
	scm_enable_primitive_generic_x (scm_list_1 (extended));
      gf = *SCM_SUBR_GENERIC (extended);
      gext = scm_call_2 (SCM_VARIABLE_REF (scm_var_make_extended_generic),
			 gf,
			 SCM_SUBR_NAME (extension));
      SCM_SET_SUBR_GENERIC (extension, gext);
    }
  else
    {
      t_extension *e = scm_gc_malloc (sizeof (t_extension),
				      extension_gc_hint);
      t_extension **loc = &extensions;
      /* Make sure that extensions are placed before their own
       * extensions in the extensions list.  O(N^2) algorithm, but
       * extensions of primitive generics are rare.
       */
      while (*loc && extension != (*loc)->extended)
	loc = &(*loc)->next;
      e->next = *loc;
      e->extended = extended;
      e->extension = extension;
      *loc = e;
    }
}

static void
setup_extended_primitive_generics ()
{
  while (extensions)
    {
      t_extension *e = extensions;
      scm_c_extend_primitive_generic (e->extended, e->extension);
      extensions = e->next;
    }
}

/******************************************************************************
 *
 * Protocol for calling a generic fumction
 * This protocol is roughly equivalent to (parameter are a little bit different
 * for efficiency reasons):
 *
 * 	+ apply-generic (gf args)
 *		+ compute-applicable-methods (gf args ...)
 *			+ sort-applicable-methods (methods args)
 *		+ apply-methods (gf methods args)
 *
 * apply-methods calls make-next-method to build the "continuation" of a a
 * method.  Applying a next-method will call apply-next-method which in
 * turn will call  apply again to call effectively the following method.
 *
 ******************************************************************************/

static int
applicablep (SCM actual, SCM formal)
{
  /* We already know that the cpl is well formed. */
  return scm_is_true (scm_c_memq (formal, SCM_SLOT (actual, scm_si_cpl)));
}

static int
more_specificp (SCM m1, SCM m2, SCM const *targs)
{
  register SCM s1, s2;
  register long i;
  /*
   * Note:
   *   m1 and m2 can have != length (i.e. one can be one element longer than the
   * other when we have a dotted parameter list). For instance, with the call
   *   (M 1)
   * with
   *   (define-method M (a . l) ....)
   *   (define-method M (a) ....)
   *
   * we consider that the second method is more specific.
   *
   * BTW, targs is an array of types. We don't need it's size since
   * we already know that m1 and m2 are applicable (no risk to go past
   * the end of this array).
   *
   */
  for (i=0, s1=SPEC_OF(m1), s2=SPEC_OF(m2); ; i++, s1=SCM_CDR(s1), s2=SCM_CDR(s2)) {
    if (scm_is_null(s1)) return 1;
    if (scm_is_null(s2)) return 0;
    if (SCM_CAR(s1) != SCM_CAR(s2)) {
      register SCM l, cs1 = SCM_CAR(s1), cs2 = SCM_CAR(s2);

      for (l = SCM_SLOT (targs[i], scm_si_cpl);   ; l = SCM_CDR(l)) {
	if (cs1 == SCM_CAR(l))
	  return 1;
	if (cs2 == SCM_CAR(l))
	  return 0;
      }
      return 0;/* should not occur! */
    }
  }
  return 0; /* should not occur! */
}

#define BUFFSIZE 32		/* big enough for most uses */

static SCM
scm_i_vector2list (SCM l, long len)
{
  long j;
  SCM z = scm_c_make_vector (len, SCM_UNDEFINED);

  for (j = 0; j < len; j++, l = SCM_CDR (l)) {
    SCM_SIMPLE_VECTOR_SET (z, j, SCM_CAR (l));
  }
  return z;
}

static SCM
sort_applicable_methods (SCM method_list, long size, SCM const *targs)
{
  long i, j, incr;
  SCM *v, vector = SCM_EOL;
  SCM buffer[BUFFSIZE];
  SCM save = method_list;
  scm_t_array_handle handle;

  /* For reasonably sized method_lists we can try to avoid all the
   * consing and reorder the list in place...
   * This idea is due to David McClain <Dave_McClain@msn.com>
   */
  if (size <= BUFFSIZE)
    {
      for (i = 0;  i < size; i++)
	{
	  buffer[i]   = SCM_CAR (method_list);
	  method_list = SCM_CDR (method_list);
	}
      v = buffer;
    }
  else
    {
      /* Too many elements in method_list to keep everything locally */
      vector = scm_i_vector2list (save, size);
      v = scm_vector_writable_elements (vector, &handle, NULL, NULL);
    }

  /* Use a simple shell sort since it is generally faster than qsort on
   * small vectors (which is probably mostly the case when we have to
   * sort a list of applicable methods).
   */
  for (incr = size / 2; incr; incr /= 2)
    {
      for (i = incr; i < size; i++)
	{
	  for (j = i - incr; j >= 0; j -= incr)
	    {
	      if (more_specificp (v[j], v[j+incr], targs))
		break;
	      else
		{
		  SCM tmp = v[j + incr];
		  v[j + incr] = v[j];
		  v[j] = tmp;
		}
	    }
	}
    }

  if (size <= BUFFSIZE)
    {
      /* We did it in locally, so restore the original list (reordered) in-place */
      for (i = 0, method_list = save; i < size; i++, v++)
	{
	  SCM_SETCAR (method_list, *v);
	  method_list = SCM_CDR (method_list);
	}
      return save;
    }

  /* If we are here, that's that we did it the hard way... */
  scm_array_handle_release (&handle);
  return scm_vector_to_list (vector);
}

SCM
scm_compute_applicable_methods (SCM gf, SCM args, long len, int find_method_p)
{
  register long i;
  long count = 0;
  SCM l, fl, applicable = SCM_EOL;
  SCM save = args;
  SCM buffer[BUFFSIZE];
  SCM const *types;
  SCM *p;
  SCM tmp = SCM_EOL;
  scm_t_array_handle handle;

  /* Build the list of arguments types */
  if (len >= BUFFSIZE) 
    {
      tmp = scm_c_make_vector (len, SCM_UNDEFINED);
      types = p = scm_vector_writable_elements (tmp, &handle, NULL, NULL);

    /*
      note that we don't have to work to reset the generation
      count. TMP is a new vector anyway, and it is found
      conservatively.
    */
    }
  else
    types = p = buffer;

  for (  ; !scm_is_null (args); args = SCM_CDR (args))
    *p++ = scm_class_of (SCM_CAR (args));
  
  /* Build a list of all applicable methods */
  for (l = scm_generic_function_methods (gf); !scm_is_null (l); l = SCM_CDR (l))
    {
      fl = SPEC_OF (SCM_CAR (l));
      for (i = 0; ; i++, fl = SCM_CDR (fl))
	{
	  if (SCM_INSTANCEP (fl)
	      /* We have a dotted argument list */
	      || (i >= len && scm_is_null (fl)))
	    {	/* both list exhausted */
	      applicable = scm_cons (SCM_CAR (l), applicable);
	      count     += 1;
	      break;
	    }
	  if (i >= len
	      || scm_is_null (fl)
	      || !applicablep (types[i], SCM_CAR (fl)))
	    break;
	}
    }

  if (len >= BUFFSIZE)
      scm_array_handle_release (&handle);

  if (count == 0)
    {
      if (find_method_p)
	return SCM_BOOL_F;
      scm_call_2 (SCM_VARIABLE_REF (var_no_applicable_method), gf, save);
      /* if we are here, it's because no-applicable-method hasn't signaled an error */
      return SCM_BOOL_F;
    }

  return (count == 1
	  ? applicable
	  : sort_applicable_methods (applicable, count, types));
}

#if 0
SCM_PROC (s_sys_compute_applicable_methods, "%compute-applicable-methods", 2, 0, 0, scm_sys_compute_applicable_methods);
#endif

static const char s_sys_compute_applicable_methods[] = "%compute-applicable-methods";

SCM
scm_sys_compute_applicable_methods (SCM gf, SCM args)
#define FUNC_NAME s_sys_compute_applicable_methods
{
  long n;
  SCM_VALIDATE_GENERIC (1, gf);
  n = scm_ilength (args);
  SCM_ASSERT (n >= 0, args, SCM_ARG2, FUNC_NAME);
  return scm_compute_applicable_methods (gf, args, n, 1);
}
#undef FUNC_NAME

SCM_SYMBOL (sym_compute_applicable_methods, "compute-applicable-methods");
SCM_VARIABLE_INIT (var_compute_applicable_methods, "compute-applicable-methods",
                   scm_c_define_gsubr (s_sys_compute_applicable_methods, 2, 0, 0,
                                       scm_sys_compute_applicable_methods));

/******************************************************************************
 *
 * A simple make (which will be redefined later in Scheme)
 * This version handles only creation of gf, methods and classes (no instances)
 *
 * Since this code will disappear when Goops will be fully booted,
 * no precaution is taken to be efficient.
 *
 ******************************************************************************/

SCM_KEYWORD (k_setter,		"setter");
SCM_KEYWORD (k_specializers,	"specializers");
SCM_KEYWORD (k_procedure,	"procedure");
SCM_KEYWORD (k_formals,		"formals");
SCM_KEYWORD (k_body,		"body");
SCM_KEYWORD (k_make_procedure,	"make-procedure");
SCM_KEYWORD (k_dsupers,		"dsupers");
SCM_KEYWORD (k_slots,		"slots");
SCM_KEYWORD (k_gf,		"generic-function");

SCM_DEFINE (scm_make, "make",  0, 0, 1,
	    (SCM args),
	    "Make a new object.  @var{args} must contain the class and\n"
	    "all necessary initialization information.")
#define FUNC_NAME s_scm_make
{
  SCM class, z;
  long len = scm_ilength (args);

  if (len <= 0 || (len & 1) == 0)
    SCM_WRONG_NUM_ARGS ();

  class = SCM_CAR(args);
  args  = SCM_CDR(args);

  if (class == scm_class_generic || class == scm_class_accessor)
    {
      z = scm_make_struct (class, SCM_INUM0,
                           scm_list_4 (SCM_BOOL_F,
                                       SCM_EOL,
				       SCM_INUM0,
				       SCM_EOL));
      scm_set_procedure_property_x (z, scm_sym_name,
				    scm_get_keyword (k_name,
						     args,
						     SCM_BOOL_F));
      clear_method_cache (z);
      if (class == scm_class_accessor)
	{
	  SCM setter = scm_get_keyword (k_setter, args, SCM_BOOL_F);
	  if (scm_is_true (setter))
	    scm_sys_set_object_setter_x (z, setter);
	}
    }
  else
    {
      z = scm_sys_allocate_instance (class, args);

      if (class == scm_class_method
	  || class == scm_class_accessor_method)
	{
	  SCM_SET_SLOT (z, scm_si_generic_function,
	    scm_i_get_keyword (k_gf,
			       args,
			       len - 1,
			       SCM_BOOL_F,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_specializers,
	    scm_i_get_keyword (k_specializers,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_procedure,
	    scm_i_get_keyword (k_procedure,
			       args,
			       len - 1,
			       SCM_BOOL_F,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_formals,
	    scm_i_get_keyword (k_formals,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_body,
	    scm_i_get_keyword (k_body,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_make_procedure,
	    scm_i_get_keyword (k_make_procedure,
			       args,
			       len - 1,
			       SCM_BOOL_F,
			       FUNC_NAME));
	}
      else
	{
	  /* In all the others case, make a new class .... No instance here */
	  SCM_SET_SLOT (z, scm_vtable_index_name,
	    scm_i_get_keyword (k_name,
			       args,
			       len - 1,
			       scm_from_latin1_symbol ("???"),
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_direct_supers,
	    scm_i_get_keyword (k_dsupers,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	  SCM_SET_SLOT (z, scm_si_direct_slots,
	    scm_i_get_keyword (k_slots,
			       args,
			       len - 1,
			       SCM_EOL,
			       FUNC_NAME));
	}
    }
  return z;
}
#undef FUNC_NAME

SCM_DEFINE (scm_find_method, "find-method", 0, 0, 1,
	    (SCM l),
	    "")
#define FUNC_NAME s_scm_find_method
{
  SCM gf;
  long len = scm_ilength (l);

  if (len == 0)
    SCM_WRONG_NUM_ARGS ();

  gf = SCM_CAR(l); l = SCM_CDR(l);
  SCM_VALIDATE_GENERIC (1, gf);
  if (scm_is_null (SCM_SLOT (gf, scm_si_methods)))
    SCM_MISC_ERROR ("no methods for generic ~S", scm_list_1 (gf));

  return scm_compute_applicable_methods (gf, l, len - 1, 1);
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_method_more_specific_p, "%method-more-specific?", 3, 0, 0,
	    (SCM m1, SCM m2, SCM targs),
	    "Return true if method @var{m1} is more specific than @var{m2} "
	    "given the argument types (classes) listed in @var{targs}.")
#define FUNC_NAME s_scm_sys_method_more_specific_p
{
  SCM l, v, result;
  SCM *v_elts;
  long i, len, m1_specs, m2_specs;
  scm_t_array_handle handle;

  SCM_VALIDATE_METHOD (1, m1);
  SCM_VALIDATE_METHOD (2, m2);

  len = scm_ilength (targs);
  m1_specs = scm_ilength (SPEC_OF (m1));
  m2_specs = scm_ilength (SPEC_OF (m2));
  SCM_ASSERT ((len >= m1_specs) || (len >= m2_specs),
	      targs, SCM_ARG3, FUNC_NAME);

  /* Verify that all the arguments of TARGS are classes and place them
     in a vector.  */

  v = scm_c_make_vector (len, SCM_EOL);
  v_elts = scm_vector_writable_elements (v, &handle, NULL, NULL);

  for (i = 0, l = targs;
       i < len && scm_is_pair (l);
       i++, l = SCM_CDR (l))
    {
      SCM_ASSERT (SCM_CLASSP (SCM_CAR (l)), targs, SCM_ARG3, FUNC_NAME);
      v_elts[i] = SCM_CAR (l);
    }
  result = more_specificp (m1, m2, v_elts) ? SCM_BOOL_T: SCM_BOOL_F;

  scm_array_handle_release (&handle);

  return result;
}
#undef FUNC_NAME



/******************************************************************************
 *
 * Initializations
 *
 ******************************************************************************/

static void
fix_cpl (SCM c, SCM before, SCM after)
{
  SCM cpl = SCM_SLOT (c, scm_si_cpl);
  SCM ls = scm_c_memq (after, cpl);
  SCM tail = scm_delq1_x (before, SCM_CDR (ls));
  if (scm_is_false (ls))
    /* if this condition occurs, fix_cpl should not be applied this way */
    abort ();
  SCM_SETCAR (ls, before);
  SCM_SETCDR (ls, scm_cons (after, tail));
  {
    SCM dslots = SCM_SLOT (c, scm_si_direct_slots);
    SCM slots = build_slots_list (maplist (dslots), cpl);
    SCM g_n_s = compute_getters_n_setters (slots);
    SCM_SET_SLOT (c, scm_si_slots, slots);
    SCM_SET_SLOT (c, scm_si_getters_n_setters, g_n_s);
  }
}


static void
make_stdcls (SCM *var, char *name, SCM meta, SCM super, SCM slots)
{
   SCM tmp = scm_from_locale_symbol (name);

   *var = scm_basic_make_class (meta, tmp,
                                scm_is_pair (super) ? super : scm_list_1 (super),
                                slots);
   DEFVAR(tmp, *var);
}


SCM_KEYWORD (k_slot_definition, "slot-definition");

static void
create_standard_classes (void)
{
  SCM slots;
  SCM method_slots = scm_list_n (scm_from_latin1_symbol ("generic-function"),
				 scm_from_latin1_symbol ("specializers"),
				 sym_procedure,
				 scm_from_latin1_symbol ("formals"),
				 scm_from_latin1_symbol ("body"),
				 scm_from_latin1_symbol ("make-procedure"),
                                 SCM_UNDEFINED);
  SCM amethod_slots = scm_list_1 (scm_list_3 (scm_from_latin1_symbol ("slot-definition"),
					      k_init_keyword,
					      k_slot_definition));
  SCM gf_slots = scm_list_4 (scm_from_latin1_symbol ("methods"),
			     scm_list_3 (scm_from_latin1_symbol ("n-specialized"),
					 k_init_value,
					 SCM_INUM0),
			     scm_list_3 (scm_from_latin1_symbol ("extended-by"),
					 k_init_value,
					 SCM_EOL),
                             scm_from_latin1_symbol ("effective-methods"));
  SCM setter_slots = scm_list_1 (sym_setter);
  SCM egf_slots = scm_list_1 (scm_list_3 (scm_from_latin1_symbol ("extends"),
					  k_init_value,
					  SCM_EOL));
  /* Foreign class slot classes */
  make_stdcls (&scm_class_foreign_slot,	   "<foreign-slot>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_protected,	   "<protected-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_hidden,	   "<hidden-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_opaque,	   "<opaque-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_read_only,	   "<read-only-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_self,		   "<self-slot>",
	       scm_class_class, scm_class_read_only,       SCM_EOL);
  make_stdcls (&scm_class_protected_opaque, "<protected-opaque-slot>",
	       scm_class_class,
	       scm_list_2 (scm_class_protected, scm_class_opaque),
	       SCM_EOL);
  make_stdcls (&scm_class_protected_hidden, "<protected-hidden-slot>",
	       scm_class_class,
	       scm_list_2 (scm_class_protected, scm_class_hidden),
	       SCM_EOL);
  make_stdcls (&scm_class_protected_read_only, "<protected-read-only-slot>",
	       scm_class_class,
	       scm_list_2 (scm_class_protected, scm_class_read_only),
	       SCM_EOL);
  make_stdcls (&scm_class_scm,		   "<scm-slot>",
	       scm_class_class, scm_class_protected, SCM_EOL);
  make_stdcls (&scm_class_int,		   "<int-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_float,	   "<float-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);
  make_stdcls (&scm_class_double,	   "<double-slot>",
	       scm_class_class, scm_class_foreign_slot,	   SCM_EOL);

  /* Continue initialization of class <class> */

  slots = build_class_class_slots ();
  SCM_SET_SLOT (scm_class_class, scm_si_direct_slots, slots);
  SCM_SET_SLOT (scm_class_class, scm_si_slots, slots);
  SCM_SET_SLOT (scm_class_class, scm_si_getters_n_setters,
		compute_getters_n_setters (slots));

  /* scm_class_generic functions classes */
  make_stdcls (&scm_class_procedure_class, "<procedure-class>",
	       scm_class_class, scm_class_class, SCM_EOL);
  make_stdcls (&scm_class_applicable_struct_class,    "<applicable-struct-class>",
	       scm_class_class, scm_class_procedure_class, SCM_EOL);
  SCM_SET_VTABLE_FLAGS (scm_class_applicable_struct_class, SCM_VTABLE_FLAG_APPLICABLE_VTABLE);
  make_stdcls (&scm_class_method,	   "<method>",
	       scm_class_class, scm_class_object,	   method_slots);
  make_stdcls (&scm_class_accessor_method, "<accessor-method>",
	       scm_class_class, scm_class_method,   amethod_slots);
  make_stdcls (&scm_class_applicable,	   "<applicable>",
	       scm_class_class, scm_class_top, SCM_EOL);
  make_stdcls (&scm_class_applicable_struct,	   "<applicable-struct>",
	       scm_class_applicable_struct_class,
	       scm_list_2 (scm_class_object, scm_class_applicable),
	       scm_list_1 (sym_procedure));
  make_stdcls (&scm_class_generic,	   "<generic>",
	       scm_class_applicable_struct_class, scm_class_applicable_struct,   gf_slots);
  SCM_SET_CLASS_FLAGS (scm_class_generic, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_extended_generic, "<extended-generic>",
	       scm_class_applicable_struct_class, scm_class_generic, egf_slots);
  SCM_SET_CLASS_FLAGS (scm_class_extended_generic, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_generic_with_setter, "<generic-with-setter>",
	       scm_class_applicable_struct_class, scm_class_generic, setter_slots);
  SCM_SET_CLASS_FLAGS (scm_class_generic_with_setter, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_accessor,	   "<accessor>",
	       scm_class_applicable_struct_class, scm_class_generic_with_setter, SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_accessor, SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_extended_generic_with_setter,
	       "<extended-generic-with-setter>",
	       scm_class_applicable_struct_class,
	       scm_list_2 (scm_class_generic_with_setter,
			   scm_class_extended_generic),
	       SCM_EOL);
  SCM_SET_CLASS_FLAGS (scm_class_extended_generic_with_setter,
		       SCM_CLASSF_PURE_GENERIC);
  make_stdcls (&scm_class_extended_accessor, "<extended-accessor>",
	       scm_class_applicable_struct_class,
	       scm_list_2 (scm_class_accessor,
			   scm_class_extended_generic_with_setter),
	       SCM_EOL);
  fix_cpl (scm_class_extended_accessor,
	   scm_class_extended_generic, scm_class_generic);
  SCM_SET_CLASS_FLAGS (scm_class_extended_accessor, SCM_CLASSF_PURE_GENERIC);

  /* Primitive types classes */
  make_stdcls (&scm_class_boolean, 	   "<boolean>",
	       scm_class_class, scm_class_top, 	    	   SCM_EOL);
  make_stdcls (&scm_class_char,		   "<char>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_list,	   	   "<list>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_pair,		   "<pair>",
	       scm_class_class, scm_class_list,		   SCM_EOL);
  make_stdcls (&scm_class_null,		   "<null>",
	       scm_class_class, scm_class_list,		   SCM_EOL);
  make_stdcls (&scm_class_string,	   "<string>",
	       scm_class_class, scm_class_top,	    	   SCM_EOL);
  make_stdcls (&scm_class_symbol,	   "<symbol>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_vector,	   "<vector>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_foreign,	           "<foreign>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_hashtable,	   "<hashtable>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_fluid,		   "<fluid>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_dynamic_state,	   "<dynamic-state>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_frame,		   "<frame>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_objcode,		   "<objcode>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_vm,		   "<vm>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_vm_cont,		   "<vm-continuation>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_bytevector,	   "<bytevector>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&class_uvec,		   "<uvec>",
	       scm_class_class, class_bytevector,	   SCM_EOL);
  make_stdcls (&scm_class_number,	   "<number>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_complex,	   "<complex>",
	       scm_class_class, scm_class_number, 	   SCM_EOL);
  make_stdcls (&scm_class_real,		   "<real>",
	       scm_class_class, scm_class_complex,	   SCM_EOL);
  make_stdcls (&scm_class_integer,	   "<integer>",
	       scm_class_class, scm_class_real,		   SCM_EOL);
  make_stdcls (&scm_class_fraction,	   "<fraction>",
	       scm_class_class, scm_class_real,		   SCM_EOL);
  make_stdcls (&scm_class_keyword,	   "<keyword>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_unknown,	   "<unknown>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_procedure,	   "<procedure>",
	       scm_class_procedure_class, scm_class_applicable, SCM_EOL);
  make_stdcls (&scm_class_primitive_generic, "<primitive-generic>",
	       scm_class_procedure_class, scm_class_procedure, SCM_EOL);
  make_stdcls (&scm_class_port,		   "<port>",
	       scm_class_class, scm_class_top,		   SCM_EOL);
  make_stdcls (&scm_class_input_port,	   "<input-port>",
	       scm_class_class, scm_class_port,		   SCM_EOL);
  make_stdcls (&scm_class_output_port,	   "<output-port>",
	       scm_class_class, scm_class_port,		   SCM_EOL);
  make_stdcls (&scm_class_input_output_port, "<input-output-port>",
	       scm_class_class,
	       scm_list_2 (scm_class_input_port, scm_class_output_port),
	       SCM_EOL);
}

/**********************************************************************
 *
 * Smob classes
 *
 **********************************************************************/

static SCM
make_class_from_template (char const *template, char const *type_name, SCM supers, int applicablep)
{
  SCM class, name;
  if (type_name)
    {
      char buffer[100];
      sprintf (buffer, template, type_name);
      name = scm_from_locale_symbol (buffer);
    }
  else
    name = SCM_GOOPS_UNBOUND;

  class = scm_basic_make_class (applicablep ? scm_class_procedure_class : scm_class_class,
                                name, supers, SCM_EOL);

  /* Only define name if doesn't already exist. */
  if (!SCM_GOOPS_UNBOUNDP (name)
      && scm_is_false (scm_module_variable (scm_module_goops, name)))
    DEFVAR (name, class);
  return class;
}

static SCM
make_class_from_symbol (SCM type_name_sym, SCM supers, int applicablep)
{
  SCM class, name;
  if (type_name_sym != SCM_BOOL_F)
    {
      name = scm_string_append (scm_list_3 (scm_from_locale_string ("<"),
					    scm_symbol_to_string (type_name_sym),
					    scm_from_locale_string (">")));
      name = scm_string_to_symbol (name);
    }
  else
    name = SCM_GOOPS_UNBOUND;

  class = scm_basic_make_class (applicablep ? scm_class_procedure_class : scm_class_class,
                                name, supers, SCM_EOL);

  /* Only define name if doesn't already exist. */
  if (!SCM_GOOPS_UNBOUNDP (name)
      && scm_is_false (scm_module_variable (scm_module_goops, name)))
    DEFVAR (name, class);
  return class;
}

SCM
scm_make_extended_class (char const *type_name, int applicablep)
{
  return make_class_from_template ("<%s>",
				   type_name,
				   scm_list_1 (applicablep
					       ? scm_class_applicable
					       : scm_class_top),
				   applicablep);
}

static SCM
scm_make_extended_class_from_symbol (SCM type_name_sym, int applicablep)
{
  return make_class_from_symbol (type_name_sym,
				 scm_list_1 (applicablep
					     ? scm_class_applicable
					     : scm_class_top),
				 applicablep);
}

void
scm_i_inherit_applicable (SCM c)
{
  if (!SCM_SUBCLASSP (c, scm_class_applicable))
    {
      SCM dsupers = SCM_SLOT (c, scm_si_direct_supers);
      SCM cpl = SCM_SLOT (c, scm_si_cpl);
      /* patch scm_class_applicable into direct-supers */
      SCM top = scm_c_memq (scm_class_top, dsupers);
      if (scm_is_false (top))
	dsupers = scm_append (scm_list_2 (dsupers,
					  scm_list_1 (scm_class_applicable)));
      else
	{
	  SCM_SETCAR (top, scm_class_applicable);
	  SCM_SETCDR (top, scm_cons (scm_class_top, SCM_CDR (top)));
	}
      SCM_SET_SLOT (c, scm_si_direct_supers, dsupers);
      /* patch scm_class_applicable into cpl */
      top = scm_c_memq (scm_class_top, cpl);
      if (scm_is_false (top))
	abort ();
      else
	{
	  SCM_SETCAR (top, scm_class_applicable);
	  SCM_SETCDR (top, scm_cons (scm_class_top, SCM_CDR (top)));
	}
      /* add class to direct-subclasses of scm_class_applicable */
      SCM_SET_SLOT (scm_class_applicable,
		    scm_si_direct_subclasses,
		    scm_cons (c, SCM_SLOT (scm_class_applicable,
					   scm_si_direct_subclasses)));
    }
}

static void
create_smob_classes (void)
{
  long i;

  for (i = 0; i < SCM_I_MAX_SMOB_TYPE_COUNT; ++i)
    scm_smob_class[i] = 0;

  scm_smob_class[SCM_TC2SMOBNUM (scm_tc16_keyword)] = scm_class_keyword;

  for (i = 0; i < scm_numsmob; ++i)
    if (!scm_smob_class[i])
      scm_smob_class[i] = scm_make_extended_class (SCM_SMOBNAME (i),
						   scm_smobs[i].apply != 0);
}

void
scm_make_port_classes (long ptobnum, char *type_name)
{
  SCM c, class = make_class_from_template ("<%s-port>",
					   type_name,
					   scm_list_1 (scm_class_port),
					   0);
  scm_port_class[SCM_IN_PCLASS_INDEX + ptobnum]
    = make_class_from_template ("<%s-input-port>",
				type_name,
				scm_list_2 (class, scm_class_input_port),
				0);
  scm_port_class[SCM_OUT_PCLASS_INDEX + ptobnum]
    = make_class_from_template ("<%s-output-port>",
				type_name,
				scm_list_2 (class, scm_class_output_port),
				0);
  scm_port_class[SCM_INOUT_PCLASS_INDEX + ptobnum]
    = c
    = make_class_from_template ("<%s-input-output-port>",
				type_name,
				scm_list_2 (class, scm_class_input_output_port),
				0);
  /* Patch cpl (since this tree is too complex for the C level compute-cpl) */
  SCM_SET_SLOT (c, scm_si_cpl,
		scm_cons2 (c, class, SCM_SLOT (scm_class_input_output_port, scm_si_cpl)));
}

static void
create_port_classes (void)
{
  long i;

  for (i = 0; i < scm_numptob; ++i)
    scm_make_port_classes (i, SCM_PTOBNAME (i));
}

static SCM
make_struct_class (void *closure SCM_UNUSED,
		   SCM vtable, SCM data, SCM prev SCM_UNUSED)
{
  SCM sym = SCM_STRUCT_TABLE_NAME (data);
  if (scm_is_true (sym))
    {
      int applicablep = SCM_CLASS_FLAGS (vtable) & SCM_VTABLE_FLAG_APPLICABLE;

      SCM_SET_STRUCT_TABLE_CLASS (data, 
				  scm_make_extended_class_from_symbol (sym, applicablep));
    }

  scm_remember_upto_here_2 (data, vtable);
  return SCM_UNSPECIFIED;
}

static void
create_struct_classes (void)
{
  scm_internal_hash_fold (make_struct_class, 0, SCM_BOOL_F, scm_struct_table);
}

/**********************************************************************
 *
 * C interface
 *
 **********************************************************************/

void
scm_load_goops ()
{
  if (!goops_loaded_p)
    scm_c_resolve_module ("oop goops");
}


SCM_SYMBOL (sym_o, "o");
SCM_SYMBOL (sym_x, "x");

SCM_KEYWORD (k_accessor, "accessor");
SCM_KEYWORD (k_getter, "getter");

SCM
scm_ensure_accessor (SCM name)
{
  SCM gf = scm_call_2 (SCM_TOP_LEVEL_LOOKUP_CLOSURE, name, SCM_BOOL_F);
  if (!SCM_IS_A_P (gf, scm_class_accessor))
    {
      gf = scm_make (scm_list_3 (scm_class_generic, k_name, name));
      gf = scm_make (scm_list_5 (scm_class_accessor,
				 k_name, name, k_setter, gf));
    }
  return gf;
}

#ifdef GUILE_DEBUG
/*
 * Debugging utilities
 */

SCM_DEFINE (scm_pure_generic_p, "pure-generic?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a pure generic.")
#define FUNC_NAME s_scm_pure_generic_p
{
  return scm_from_bool (SCM_PUREGENERICP (obj));
}
#undef FUNC_NAME

#endif /* GUILE_DEBUG */

/*
 * Initialization
 */

SCM_DEFINE (scm_sys_goops_loaded, "%goops-loaded", 0, 0, 0,
	    (),
	    "Announce that GOOPS is loaded and perform initialization\n"
	    "on the C level which depends on the loaded GOOPS modules.")
#define FUNC_NAME s_scm_sys_goops_loaded
{
  goops_loaded_p = 1;
  var_compute_applicable_methods =
    scm_module_variable (scm_module_goops, sym_compute_applicable_methods);
  var_slot_unbound =
    scm_module_variable (scm_module_goops, sym_slot_unbound);
  var_slot_missing =
    scm_module_variable (scm_module_goops, sym_slot_missing);
  var_compute_cpl =
    scm_module_variable (scm_module_goops, sym_compute_cpl);
  var_no_applicable_method =
    scm_module_variable (scm_module_goops, sym_no_applicable_method);
  var_change_class =
    scm_module_variable (scm_module_goops, sym_change_class);
  setup_extended_primitive_generics ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM scm_module_goops;

SCM
scm_init_goops_builtins (void)
{
  scm_module_goops = scm_current_module ();

  goops_rstate = scm_c_make_rstate ("GOOPS", 5);

#include "libguile/goops.x"

  hell = scm_calloc (hell_size * sizeof (*hell));
  hell_mutex = scm_make_mutex ();

  create_basic_classes ();
  create_standard_classes ();
  create_smob_classes ();
  create_struct_classes ();
  create_port_classes ();

  {
    SCM name = scm_from_latin1_symbol ("no-applicable-method");
    scm_no_applicable_method =
      scm_make (scm_list_3 (scm_class_generic, k_name, name));
    DEFVAR (name, scm_no_applicable_method);
  }

  return SCM_UNSPECIFIED;
}

void
scm_init_goops ()
{
  scm_c_define_gsubr ("%init-goops-builtins", 0, 0, 0,
		      scm_init_goops_builtins);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
