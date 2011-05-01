/* classes: h_files */

#ifndef SCM_GOOPS_H
#define SCM_GOOPS_H

/* Copyright (C) 1998,1999,2000,2001,2002,2003, 2006, 2008, 2009, 2011 Free Software Foundation, Inc.
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
 * This file is based upon stklos.h from the STk distribution by
 * Erick Gallesio <eg@unice.fr>.
 */

#include "libguile/__scm.h"

#include "libguile/validate.h"

/* {Class flags}
 *
 * These are used for efficient identification of instances of a
 * certain class or its subclasses when traversal of the inheritance
 * graph would be too costly.
 */
#define SCM_VTABLE_FLAG_GOOPS_CLASS SCM_VTABLE_FLAG_GOOPS_0
#define SCM_VTABLE_FLAG_GOOPS_VALID SCM_VTABLE_FLAG_GOOPS_1
#define SCM_VTABLE_FLAG_GOOPS_PURE_GENERIC SCM_VTABLE_FLAG_GOOPS_2

#define SCM_CLASS_OF(x)         SCM_STRUCT_VTABLE (x)
#define SCM_CLASS_FLAGS(class) (SCM_VTABLE_FLAGS (class))
#define SCM_OBJ_CLASS_FLAGS(obj) (SCM_STRUCT_VTABLE_FLAGS (obj))
#define SCM_SET_CLASS_FLAGS(c, f) (SCM_SET_VTABLE_FLAGS (c, f))
#define SCM_CLEAR_CLASS_FLAGS(c, f) (SCM_CLEAR_VTABLE_FLAGS (c, f))

#define SCM_CLASSF_METACLASS     (SCM_VTABLE_FLAG_GOOPS_CLASS|SCM_VTABLE_FLAG_VTABLE)
#define SCM_CLASSF_PURE_GENERIC  SCM_VTABLE_FLAG_GOOPS_PURE_GENERIC
#define SCM_CLASSF_GOOPS_VALID   SCM_VTABLE_FLAG_GOOPS_VALID
#define SCM_CLASSF_GOOPS         SCM_VTABLE_FLAG_GOOPS_CLASS
#define SCM_CLASSF_GOOPS_OR_VALID (SCM_CLASSF_GOOPS | SCM_CLASSF_GOOPS_VALID)

/*
 * scm_class_class
 */

/* see also, SCM_VTABLE_BASE_LAYOUT, and build_class_class_slots */
#define SCM_CLASS_CLASS_LAYOUT                  \
  "pw" /* redefined */                          \
  "uw" /* h0 */                                 \
  "uw" /* h1 */                                 \
  "uw" /* h2 */                                 \
  "uw" /* h3 */                                 \
  "uw" /* h4 */                                 \
  "uw" /* h5 */                                 \
  "uw" /* h6 */                                 \
  "uw" /* h7 */                                 \
  "pw" /* direct supers */                      \
  "pw" /* direct slots */                       \
  "pw" /* direct subclasses */                  \
  "pw" /* direct methods */                     \
  "pw" /* cpl */                                \
  "pw" /* default-slot-definition-class */      \
  "pw" /* slots */                              \
  "pw" /* getters-n-setters */                  \
  "pw" /* keyword access */                     \
  "pw" /* nfields */

#define scm_si_redefined         (scm_vtable_offset_user + 0)
#define scm_si_h0                (scm_vtable_offset_user + 1)
#define scm_si_hashsets          scm_si_h0
#define scm_si_h1                (scm_vtable_offset_user + 2)
#define scm_si_h2                (scm_vtable_offset_user + 3)
#define scm_si_h3                (scm_vtable_offset_user + 4)
#define scm_si_h4                (scm_vtable_offset_user + 5)
#define scm_si_h5                (scm_vtable_offset_user + 6)
#define scm_si_h6                (scm_vtable_offset_user + 7)
#define scm_si_h7                (scm_vtable_offset_user + 8)
#define scm_si_direct_supers 	 (scm_vtable_offset_user + 9) /* (class ...) */
#define scm_si_direct_slots	 (scm_vtable_offset_user + 10) /* ((name . options) ...) */
#define scm_si_direct_subclasses (scm_vtable_offset_user + 11) /* (class ...) */
#define scm_si_direct_methods	 (scm_vtable_offset_user + 12) /* (methods ...) */
#define scm_si_cpl		 (scm_vtable_offset_user + 13) /* (class ...) */
#define scm_si_slotdef_class	 (scm_vtable_offset_user + 14)
#define scm_si_slots		 (scm_vtable_offset_user + 15) /* ((name . options) ...) */
#define scm_si_name_access	 (scm_vtable_offset_user + 16)
#define scm_si_getters_n_setters scm_si_name_access
#define scm_si_keyword_access	 (scm_vtable_offset_user + 17)
#define scm_si_nfields		 (scm_vtable_offset_user + 18) /* an integer */
#define SCM_N_CLASS_SLOTS	 (scm_vtable_offset_user + 19)

typedef struct scm_t_method {
  SCM generic_function;
  SCM specializers;
  SCM procedure;
} scm_t_method;

#define SCM_METHOD(obj) ((scm_t_method *) SCM_STRUCT_DATA (obj))

#define SCM_OBJ_CLASS_REDEF(x)  (SCM_PACK (SCM_STRUCT_VTABLE_DATA (x) [scm_si_redefined]))
#define SCM_INST(x)	       SCM_STRUCT_DATA (x)

#define SCM_CLASS_OF(x)        SCM_STRUCT_VTABLE (x)
#define SCM_ACCESSORS_OF(x)    (SCM_PACK (SCM_STRUCT_VTABLE_DATA (x)[scm_si_getters_n_setters]))

#define SCM_CLASSP(x) \
  (SCM_STRUCTP (x) && SCM_STRUCT_VTABLE_FLAGS (x) & SCM_CLASSF_METACLASS)
#define SCM_VALIDATE_CLASS(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, CLASSP, "class")

#define SCM_INSTANCEP(x) \
  (SCM_STRUCTP (x) && (SCM_STRUCT_VTABLE_FLAGS (x) & SCM_CLASSF_GOOPS))
#define SCM_VALIDATE_INSTANCE(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, INSTANCEP, "instance")

#define SCM_PUREGENERICP(x) \
  (SCM_STRUCTP (x) && (SCM_STRUCT_VTABLE_FLAGS (x) & SCM_CLASSF_PURE_GENERIC))
#define SCM_VALIDATE_PUREGENERIC(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, PUREGENERICP, "pure generic function")

#define SCM_SLOT(x, i)         (SCM_STRUCT_SLOT_REF (x, i))
#define SCM_SET_SLOT(x, i, v)  (SCM_STRUCT_SLOT_SET (x, i, v))
#define SCM_INSTANCE_HASH(c, i) (SCM_INST (c) [scm_si_hashsets + (i)])
#define SCM_SET_HASHSET(c, i, h)  (SCM_INST (c) [scm_si_hashsets + (i)] = (h))

#define SCM_SUBCLASSP(c1, c2)  (scm_is_true (scm_c_memq (c2, SCM_SLOT (c1, scm_si_cpl))))
#define SCM_IS_A_P(x, c) \
  (SCM_INSTANCEP (x) && SCM_SUBCLASSP (SCM_CLASS_OF (x), c))

#define SCM_GENERICP(x) \
  (SCM_INSTANCEP (x) && SCM_SUBCLASSP (SCM_CLASS_OF (x), scm_class_generic))
#define SCM_VALIDATE_GENERIC(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, GENERICP, "generic function")

#define SCM_METHODP(x) \
  (SCM_INSTANCEP (x) && SCM_SUBCLASSP (SCM_CLASS_OF (x), scm_class_method))
#define SCM_VALIDATE_METHOD(pos, x) SCM_MAKE_VALIDATE_MSG (pos, x, METHODP, "method")

#define SCM_SET_CLASS_DESTRUCTOR(c, d) SCM_SET_VTABLE_DESTRUCTOR (c, d)

#define SCM_SET_GENERIC_DISPATCH_PROCEDURE(G,C) (SCM_STRUCT_SLOT_SET (G, scm_si_dispatch_procedure, (C)))
#define SCM_CLEAR_GENERIC_EFFECTIVE_METHODS(G) (SCM_STRUCT_SLOT_SET (G, scm_si_effective_methods, SCM_EOL));

#define SCM_GENERIC_SETTER(G) (SCM_PACK (SCM_STRUCT_DATA (G) [scm_si_generic_setter]))
#define SCM_SET_GENERIC_SETTER(G,C) (SCM_STRUCT_DATA (G) [scm_si_generic_setter] = SCM_UNPACK (C))

#define scm_si_dispatch_procedure scm_applicable_struct_index_procedure /* 0 */
#define scm_si_methods            1
#define scm_si_n_specialized	  2
#define scm_si_extended_by	  3
#define scm_si_effective_methods  4
#define scm_si_generic_setter     5

#define scm_si_generic_function	 0  /* offset of gf    slot in a <method> */
#define scm_si_specializers	 1  /* offset of spec. slot in a <method> */
#define scm_si_procedure 	 2  /* offset of proc. slot in a <method> */
#define scm_si_formals		 3  /* offset of form. slot in a <method> */
#define scm_si_body		 4  /* offset of body  slot in a <method> */
#define scm_si_make_procedure	 5  /* offset of makep.slot in a <method> */

/* C interface */
SCM_API SCM scm_class_boolean;
SCM_API SCM scm_class_char;
SCM_API SCM scm_class_pair;
SCM_API SCM scm_class_procedure;
SCM_API SCM scm_class_string;
SCM_API SCM scm_class_symbol;
SCM_API SCM scm_class_primitive_generic;
SCM_API SCM scm_class_vector;
SCM_API SCM scm_class_null;
SCM_API SCM scm_class_real;
SCM_API SCM scm_class_complex;
SCM_API SCM scm_class_integer;
SCM_API SCM scm_class_fraction;
SCM_API SCM scm_class_unknown;
SCM_API SCM scm_port_class[];
SCM_API SCM scm_smob_class[];
SCM_API SCM scm_class_top;
SCM_API SCM scm_class_object;
SCM_API SCM scm_class_class;
SCM_API SCM scm_class_applicable;
SCM_API SCM scm_class_applicable_struct;
SCM_API SCM scm_class_applicable_struct_with_setter;
SCM_API SCM scm_class_generic;
SCM_API SCM scm_class_generic_with_setter;
SCM_API SCM scm_class_accessor;
SCM_API SCM scm_class_extended_generic;
SCM_API SCM scm_class_extended_generic_with_setter;
SCM_API SCM scm_class_extended_accessor;
SCM_API SCM scm_class_method;
SCM_API SCM scm_class_accessor_method;
SCM_API SCM scm_class_procedure_class;
SCM_API SCM scm_class_applicable_struct_class;
SCM_API SCM scm_class_number;
SCM_API SCM scm_class_list;
SCM_API SCM scm_class_keyword;
SCM_API SCM scm_class_port;
SCM_API SCM scm_class_input_output_port;
SCM_API SCM scm_class_input_port;
SCM_API SCM scm_class_output_port;
SCM_API SCM scm_class_foreign_slot;
SCM_API SCM scm_class_self;
SCM_API SCM scm_class_protected;
SCM_API SCM scm_class_hidden;
SCM_API SCM scm_class_opaque;
SCM_API SCM scm_class_read_only;
SCM_API SCM scm_class_protected_hidden;
SCM_API SCM scm_class_protected_opaque;
SCM_API SCM scm_class_protected_read_only;
SCM_API SCM scm_class_scm;
SCM_API SCM scm_class_int;
SCM_API SCM scm_class_float;
SCM_API SCM scm_class_double;
SCM_API const char *scm_s_slot_set_x;

SCM_API SCM scm_no_applicable_method;

SCM_API SCM scm_module_goops;

SCM_API SCM scm_goops_version (void);
SCM_API SCM scm_oldfmt (SCM);
SCM_API char *scm_c_oldfmt0 (char *);
SCM_API char *scm_c_oldfmt (char *, int n);
SCM_API void scm_load_goops (void);
SCM_API SCM scm_make_extended_class (char const *type_name, int applicablep);
SCM_API void scm_make_port_classes (long ptobnum, char *type_name);
SCM_API SCM scm_ensure_accessor (SCM name);
SCM_API SCM scm_class_of (SCM obj);

/* Low level functions exported */
SCM_API SCM scm_make_next_method (SCM methods, SCM args, SCM gf);
SCM_API SCM scm_basic_basic_make_class (SCM c, SCM name, SCM dsupers, SCM dslots);
SCM_API SCM scm_basic_make_class (SCM c, SCM name, SCM dsupers, SCM dslots);

/* Primitives exported */
SCM_API SCM scm_sys_allocate_instance (SCM c, SCM initargs);
SCM_API SCM scm_sys_set_object_setter_x (SCM obj, SCM setter);
SCM_API SCM scm_slot_ref (SCM obj, SCM slot_name);
SCM_API SCM scm_slot_set_x (SCM obj, SCM slot_name, SCM value);

SCM_API SCM scm_compute_applicable_methods (SCM gf, SCM args, long len, int scm_find_method);
SCM_API SCM scm_sys_compute_applicable_methods (SCM gf, SCM args);
#ifdef GUILE_DEBUG
SCM_API SCM scm_pure_generic_p (SCM obj);
#endif

SCM_API SCM scm_sys_compute_slots (SCM c);
SCM_INTERNAL void scm_i_inherit_applicable (SCM c);
SCM_INTERNAL SCM scm_i_get_keyword (SCM key, SCM l, long len,
				    SCM default_value, const char *subr);
SCM_API SCM scm_get_keyword (SCM key, SCM l, SCM default_value);
SCM_API SCM scm_sys_initialize_object (SCM obj, SCM initargs);
SCM_API SCM scm_sys_prep_layout_x (SCM c);
SCM_API SCM scm_sys_inherit_magic_x (SCM c, SCM dsupers);
SCM_API SCM scm_instance_p (SCM obj);
SCM_API SCM scm_class_name (SCM obj);
SCM_API SCM scm_class_direct_supers (SCM obj);
SCM_API SCM scm_class_direct_slots (SCM obj);
SCM_API SCM scm_class_direct_subclasses (SCM obj);
SCM_API SCM scm_class_direct_methods (SCM obj);
SCM_API SCM scm_class_precedence_list (SCM obj);
SCM_API SCM scm_class_slots (SCM obj);
SCM_API SCM scm_generic_function_name (SCM obj);
SCM_API SCM scm_generic_function_methods (SCM obj);
SCM_API SCM scm_method_generic_function (SCM obj);
SCM_API SCM scm_method_specializers (SCM obj);
SCM_API SCM scm_method_procedure (SCM obj);
SCM_API SCM scm_sys_fast_slot_ref (SCM obj, SCM index);
SCM_API SCM scm_sys_fast_slot_set_x (SCM obj, SCM index, SCM value);
SCM_API SCM scm_slot_ref_using_class (SCM cls, SCM obj, SCM slot_name);
SCM_API SCM scm_slot_set_using_class_x (SCM cls, SCM obj, SCM slot_name, SCM value);
SCM_API SCM scm_slot_bound_using_class_p (SCM cls, SCM obj, SCM slot_name);
SCM_API SCM scm_slot_exists_using_class_p (SCM cls, SCM obj, SCM slot_name);
SCM_API SCM scm_slot_bound_p (SCM obj, SCM slot_name);
SCM_API SCM scm_slot_exists_p (SCM obj, SCM slot_name);
SCM_API SCM scm_sys_modify_instance (SCM old, SCM newinst);
SCM_API SCM scm_sys_modify_class (SCM old, SCM newcls);
SCM_API SCM scm_sys_invalidate_class (SCM cls);
SCM_API SCM scm_sys_invalidate_method_cache_x (SCM gf);
SCM_API SCM scm_generic_capability_p (SCM proc);
SCM_API SCM scm_enable_primitive_generic_x (SCM subrs);
SCM_INTERNAL SCM scm_set_primitive_generic_x (SCM subr, SCM generic);
SCM_API SCM scm_primitive_generic_generic (SCM subr);
SCM_API void scm_c_extend_primitive_generic (SCM subr, SCM extension);
SCM_API SCM stklos_version (void);
SCM_API SCM scm_make (SCM args);
SCM_API SCM scm_find_method (SCM args);
SCM_API SCM scm_sys_method_more_specific_p (SCM m1, SCM m2, SCM targs);
SCM_API void scm_change_object_class (SCM, SCM, SCM);
/* The following are declared in __scm.h
SCM_API SCM scm_call_generic_0 (SCM gf);
SCM_API SCM scm_call_generic_1 (SCM gf, SCM a1);
SCM_API SCM scm_call_generic_2 (SCM gf, SCM a1, SCM a2);
SCM_API SCM scm_apply_generic (SCM gf, SCM args);
*/
SCM_API SCM scm_call_generic_3 (SCM gf, SCM a1, SCM a2, SCM a3);

SCM_INTERNAL SCM scm_i_define_class_for_vtable (SCM vtable);


SCM_INTERNAL SCM scm_init_goops_builtins (void);
SCM_INTERNAL void scm_init_goops (void);

#endif  /* SCM_GOOPS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
