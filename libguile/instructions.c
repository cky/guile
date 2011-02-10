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
#include "threads.h"
#include "instructions.h"


struct scm_instruction {
  enum scm_opcode opcode;	/* opcode */
  const char *name;		/* instruction name */
  signed char len;		/* Instruction length.  This may be -1 for
				   the loader (see the `VM_LOADER'
				   macro).  */
  signed char npop;		/* The number of values popped.  This may be
				   -1 for insns like `call' which can take
				   any number of arguments.  */
  char npush;			/* the number of values pushed */
  SCM symname;                  /* filled in later */
};

#define SCM_VALIDATE_LOOKUP_INSTRUCTION(pos, var, cvar)               \
  do {                                                                \
    cvar = scm_lookup_instruction_by_name (var);                      \
    SCM_ASSERT_TYPE (cvar, var, pos, FUNC_NAME, "INSTRUCTION_P");     \
  } while (0)


static scm_i_pthread_mutex_t itable_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;


static struct scm_instruction*
fetch_instruction_table ()
{
  static struct scm_instruction *table = NULL;

  scm_i_pthread_mutex_lock (&itable_lock);
  if (SCM_UNLIKELY (!table))
    {
      size_t bytes = SCM_VM_NUM_INSTRUCTIONS * sizeof(struct scm_instruction);
      int i;
      table = malloc (bytes);
      memset (table, 0, bytes);
#define VM_INSTRUCTION_TO_TABLE 1
#include <libguile/vm-expand.h>
#include <libguile/vm-i-system.i>
#include <libguile/vm-i-scheme.i>
#include <libguile/vm-i-loader.i>
#undef VM_INSTRUCTION_TO_TABLE
      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        {
          table[i].opcode = i;
          if (table[i].name)
            table[i].symname = scm_from_locale_symbol (table[i].name);
          else
            table[i].symname = SCM_BOOL_F;
        }
    }
  scm_i_pthread_mutex_unlock (&itable_lock);

  return table;
}

static struct scm_instruction *
scm_lookup_instruction_by_name (SCM name)
{
  static SCM instructions_by_name = SCM_BOOL_F;
  struct scm_instruction *table = fetch_instruction_table ();
  SCM op;

  if (SCM_UNLIKELY (scm_is_false (instructions_by_name)))
    {
      unsigned int i;

      instructions_by_name =
        scm_make_hash_table (SCM_I_MAKINUM (SCM_VM_NUM_INSTRUCTIONS));

      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        if (scm_is_true (table[i].symname))
          scm_hashq_set_x (instructions_by_name, table[i].symname,
                           SCM_I_MAKINUM (i));
    }

  op = scm_hashq_ref (instructions_by_name, name, SCM_UNDEFINED);
  if (SCM_I_INUMP (op))
    return &table[SCM_I_INUM (op)];

  return NULL;
}


/* Scheme interface */

SCM_DEFINE (scm_instruction_list, "instruction-list", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_instruction_list
{
  SCM list = SCM_EOL;
  int i;
  struct scm_instruction *ip = fetch_instruction_table ();
  for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
    if (ip[i].name)
      list = scm_cons (ip[i].symname, list);
  return scm_reverse_x (list, SCM_EOL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_p, "instruction?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_instruction_p
{
  return scm_from_bool (scm_lookup_instruction_by_name (obj) != NULL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_length, "instruction-length", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_length
{
  struct scm_instruction *ip;
  SCM_VALIDATE_LOOKUP_INSTRUCTION (1, inst, ip);
  return SCM_I_MAKINUM (ip->len);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_pops, "instruction-pops", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_pops
{
  struct scm_instruction *ip;
  SCM_VALIDATE_LOOKUP_INSTRUCTION (1, inst, ip);
  return SCM_I_MAKINUM (ip->npop);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_pushes, "instruction-pushes", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_pushes
{
  struct scm_instruction *ip;
  SCM_VALIDATE_LOOKUP_INSTRUCTION (1, inst, ip);
  return SCM_I_MAKINUM (ip->npush);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_to_opcode, "instruction->opcode", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_to_opcode
{
  struct scm_instruction *ip;
  SCM_VALIDATE_LOOKUP_INSTRUCTION (1, inst, ip);
  return SCM_I_MAKINUM (ip->opcode);
}
#undef FUNC_NAME

SCM_DEFINE (scm_opcode_to_instruction, "opcode->instruction", 1, 0, 0,
	    (SCM op),
	    "")
#define FUNC_NAME s_scm_opcode_to_instruction
{
  scm_t_signed_bits opcode;
  SCM ret = SCM_BOOL_F;

  SCM_MAKE_VALIDATE (1, op, I_INUMP);
  opcode = SCM_I_INUM (op);

  if (opcode >= 0 && opcode < SCM_VM_NUM_INSTRUCTIONS)
    ret = fetch_instruction_table ()[opcode].symname;

  if (scm_is_false (ret))
    scm_wrong_type_arg_msg (FUNC_NAME, 1, op, "INSTRUCTION_P");

  return ret;
}
#undef FUNC_NAME

void
scm_bootstrap_instructions (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_instructions",
                            (scm_t_extension_init_func)scm_init_instructions,
                            NULL);
}

void
scm_init_instructions (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/instructions.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
