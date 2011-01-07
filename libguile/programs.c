/* Copyright (C) 2001, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include "_scm.h"
#include "instructions.h"
#include "modules.h"
#include "programs.h"
#include "procprop.h" /* scm_sym_name */
#include "srcprop.h"  /* scm_sym_filename */
#include "vm.h"


static SCM write_program = SCM_BOOL_F;

SCM_DEFINE (scm_make_program, "make-program", 1, 2, 0,
	    (SCM objcode, SCM objtable, SCM free_variables),
	    "")
#define FUNC_NAME s_scm_make_program
{
  SCM_VALIDATE_OBJCODE (1, objcode);
  if (SCM_UNLIKELY (SCM_UNBNDP (objtable)))
    objtable = SCM_BOOL_F;
  else if (scm_is_true (objtable))
    SCM_VALIDATE_VECTOR (2, objtable);

  if (SCM_UNBNDP (free_variables) || scm_is_false (free_variables))
    {
      SCM ret = scm_words (scm_tc7_program, 3);
      SCM_SET_CELL_OBJECT_1 (ret, objcode);
      SCM_SET_CELL_OBJECT_2 (ret, objtable);
      return ret;
    }
  else
    {
      size_t i, len;
      SCM ret;
      SCM_VALIDATE_VECTOR (3, free_variables);
      len = scm_c_vector_length (free_variables);
      if (SCM_UNLIKELY (len >> 16))
        SCM_OUT_OF_RANGE (3, free_variables);
      ret = scm_words (scm_tc7_program | (len<<16), 3 + len);
      SCM_SET_CELL_OBJECT_1 (ret, objcode);
      SCM_SET_CELL_OBJECT_2 (ret, objtable);
      for (i = 0; i < len; i++)
        SCM_SET_CELL_OBJECT (ret, 3+i,
                             SCM_SIMPLE_VECTOR_REF (free_variables, i));
      return ret;
    }
}
#undef FUNC_NAME

void
scm_i_program_print (SCM program, SCM port, scm_print_state *pstate)
{
  static int print_error = 0;

  if (scm_is_false (write_program) && scm_module_system_booted_p)
    write_program = scm_module_local_variable
      (scm_c_resolve_module ("system vm program"),
       scm_from_latin1_symbol ("write-program"));
  
  if (SCM_PROGRAM_IS_CONTINUATION (program))
    {
      /* twingliness */
      scm_puts ("#<continuation ", port);
      scm_uintprint (SCM_UNPACK (program), 16, port);
      scm_putc ('>', port);
    }
  else if (SCM_PROGRAM_IS_PARTIAL_CONTINUATION (program))
    {
      /* twingliness */
      scm_puts ("#<partial-continuation ", port);
      scm_uintprint (SCM_UNPACK (program), 16, port);
      scm_putc ('>', port);
    }
  else if (scm_is_false (write_program) || print_error)
    {
      scm_puts ("#<program ", port);
      scm_uintprint (SCM_UNPACK (program), 16, port);
      scm_putc ('>', port);
    }
  else
    {
      print_error = 1;
      scm_call_2 (SCM_VARIABLE_REF (write_program), program, port);
      print_error = 0;
    }
}


/*
 * Scheme interface
 */

SCM_DEFINE (scm_program_p, "program?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_program_p
{
  return scm_from_bool (SCM_PROGRAM_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_base, "program-base", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_base
{
  const struct scm_objcode *c_objcode;

  SCM_VALIDATE_PROGRAM (1, program);

  c_objcode = SCM_PROGRAM_DATA (program);
  return scm_from_unsigned_integer ((scm_t_bits) SCM_C_OBJCODE_BASE (c_objcode));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_objects, "program-objects", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_objects
{
  SCM_VALIDATE_PROGRAM (1, program);
  return SCM_PROGRAM_OBJTABLE (program);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_module, "program-module", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_module
{
  SCM objs, mod;
  SCM_VALIDATE_PROGRAM (1, program);
  objs = SCM_PROGRAM_OBJTABLE (program);
  /* If a program is the result of compiling GLIL to assembly, then if
     it has an objtable, the first entry will be a module.  But some
     programs are hand-coded trampolines, like boot programs and
     primitives and the like.  So if a program happens to have a
     non-module in the first slot of the objtable, assume that it is
     such a trampoline, and just return #f for the module.  */
  mod = scm_is_true (objs) ? scm_c_vector_ref (objs, 0) : SCM_BOOL_F;
  return SCM_MODULEP (mod) ? mod : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_meta, "program-meta", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_meta
{
  SCM metaobj;
  
  SCM_VALIDATE_PROGRAM (1, program);

  metaobj = scm_objcode_meta (SCM_PROGRAM_OBJCODE (program));
  if (scm_is_true (metaobj))
    return scm_make_program (metaobj, SCM_PROGRAM_OBJTABLE (program),
                             SCM_BOOL_F);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_bindings, "program-bindings", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_bindings
{
  SCM meta;
  
  SCM_VALIDATE_PROGRAM (1, program);

  meta = scm_program_meta (program);
  if (scm_is_false (meta))
    return SCM_BOOL_F;
  
  return scm_car (scm_call_0 (meta));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_sources, "program-sources", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_sources
{
  SCM meta, sources, ret, filename;
  
  SCM_VALIDATE_PROGRAM (1, program);

  meta = scm_program_meta (program);
  if (scm_is_false (meta))
    return SCM_EOL;
  
  filename = SCM_BOOL_F;
  ret = SCM_EOL;
  for (sources = scm_cadr (scm_call_0 (meta)); !scm_is_null (sources);
       sources = scm_cdr (sources))
    {
      SCM x = scm_car (sources);
      if (scm_is_pair (x))
        {
          if (scm_is_number (scm_car (x)))
            {
              SCM addr = scm_car (x);
              ret = scm_acons (addr, scm_cons (filename, scm_cdr (x)),
                               ret);
            }
          else if (scm_is_eq (scm_car (x), scm_sym_filename))
            filename = scm_cdr (x);
        }
    }
  return scm_reverse_x (ret, SCM_UNDEFINED);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_arities, "program-arities", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_arities
{
  SCM meta;
  
  SCM_VALIDATE_PROGRAM (1, program);

  meta = scm_program_meta (program);
  if (scm_is_false (meta))
    return SCM_BOOL_F;

  return scm_caddr (scm_call_0 (meta));
}
#undef FUNC_NAME

SCM
scm_i_program_properties (SCM program)
#define FUNC_NAME "%program-properties"
{
  SCM meta;
  
  SCM_VALIDATE_PROGRAM (1, program);

  meta = scm_program_meta (program);
  if (scm_is_false (meta))
    return SCM_EOL;
  
  return scm_cdddr (scm_call_0 (meta));
}
#undef FUNC_NAME

static SCM
program_source (SCM program, size_t ip, SCM sources)
{
  SCM source = SCM_BOOL_F;

  while (!scm_is_null (sources)
         && scm_to_size_t (scm_caar (sources)) <= ip)
    {
      source = scm_car (sources);
      sources = scm_cdr (sources);
    }
  
  return source; /* (addr . (filename . (line . column))) */
}

SCM_DEFINE (scm_program_source, "program-source", 2, 1, 0,
	    (SCM program, SCM ip, SCM sources),
	    "")
#define FUNC_NAME s_scm_program_source
{
  SCM_VALIDATE_PROGRAM (1, program);
  if (SCM_UNBNDP (sources))
    sources = scm_program_sources (program);
  return program_source (program, scm_to_size_t (ip), sources);
}
#undef FUNC_NAME
    
extern SCM
scm_c_program_source (SCM program, size_t ip)
{
  return program_source (program, ip, scm_program_sources (program));
}

SCM_DEFINE (scm_program_num_free_variables, "program-num-free-variables", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_num_free_variables
{
  SCM_VALIDATE_PROGRAM (1, program);
  return scm_from_ulong (SCM_PROGRAM_NUM_FREE_VARIABLES (program));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_free_variable_ref, "program-free-variable-ref", 2, 0, 0,
	    (SCM program, SCM i),
	    "")
#define FUNC_NAME s_scm_program_free_variable_ref
{
  unsigned long idx;
  SCM_VALIDATE_PROGRAM (1, program);
  SCM_VALIDATE_ULONG_COPY (2, i, idx);
  if (idx >= SCM_PROGRAM_NUM_FREE_VARIABLES (program))
    SCM_OUT_OF_RANGE (2, i);
  return SCM_PROGRAM_FREE_VARIABLE_REF (program, idx);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_free_variable_set_x, "program-free-variable-set!", 3, 0, 0,
	    (SCM program, SCM i, SCM x),
	    "")
#define FUNC_NAME s_scm_program_free_variable_set_x
{
  unsigned long idx;
  SCM_VALIDATE_PROGRAM (1, program);
  SCM_VALIDATE_ULONG_COPY (2, i, idx);
  if (idx >= SCM_PROGRAM_NUM_FREE_VARIABLES (program))
    SCM_OUT_OF_RANGE (2, i);
  SCM_PROGRAM_FREE_VARIABLE_SET (program, idx, x);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_objcode, "program-objcode", 1, 0, 0,
	    (SCM program),
	    "Return a @var{program}'s object code.")
#define FUNC_NAME s_scm_program_objcode
{
  SCM_VALIDATE_PROGRAM (1, program);

  return SCM_PROGRAM_OBJCODE (program);
}
#undef FUNC_NAME

/* procedure-minimum-arity support. */
static void
parse_arity (SCM arity, int *req, int *opt, int *rest)
{
  SCM x = scm_cddr (arity);
  
  if (scm_is_pair (x))
    {
      *req = scm_to_int (scm_car (x));
      x = scm_cdr (x);
      if (scm_is_pair (x))
        {
          *opt = scm_to_int (scm_car (x));
          x = scm_cdr (x);
          if (scm_is_pair (x))
            *rest = scm_is_true (scm_car (x));
          else
            *rest = 0;
        }
      else
        *opt = *rest = 0;
    }
  else
    *req = *opt = *rest = 0;
}
  
int
scm_i_program_arity (SCM program, int *req, int *opt, int *rest)
{
  SCM arities;
  
  arities = scm_program_arities (program);
  if (!scm_is_pair (arities))
    return 0;

  parse_arity (scm_car (arities), req, opt, rest);
  arities = scm_cdr (arities);
  
  for (; scm_is_pair (arities); arities = scm_cdr (arities))
    {
      int thisreq, thisopt, thisrest;

      parse_arity (scm_car (arities), &thisreq, &thisopt, &thisrest);

      if (thisreq < *req
          || (thisreq == *req
              && ((thisrest && (!*rest || thisopt > *opt))
                  || (!thisrest && !*rest && thisopt > *opt))))
        {
          *req = thisreq;
          *opt = thisopt;
          *rest = thisrest;
        }
    }

  return 1;
}



void
scm_bootstrap_programs (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_programs",
                            (scm_t_extension_init_func)scm_init_programs, NULL);
}

void
scm_init_programs (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/programs.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
