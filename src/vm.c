/* Copyright (C) 2000 Free Software Foundation, Inc.
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

#define SCM_DEBUG_TYPING_STRICTNESS 0
#include "config.h"
#include "vm.h"

/* default stack size in the number of SCM */
#define VM_DEFAULT_STACK_SIZE	(16 * 1024)   /* = 64KB */
#define VM_MAXIMUM_STACK_SIZE	(1024 * 1024) /* = 4MB */

/* I sometimes use this for debugging. */
#define vm_puts(OBJ)				\
{						\
  scm_display (OBJ, scm_def_errp);		\
  scm_newline (scm_def_errp);			\
}


/*
 * Instruction
 */

#define INSTRUCTION_HASH_SIZE op_last
#define INSTRUCTION_HASH(ADDR) (((int) (ADDR) >> 1) % INSTRUCTION_HASH_SIZE)

/* These variables are defined in VM engines when they are first called. */
static struct scm_instruction *scm_regular_instruction_table = 0;
static struct scm_instruction *scm_debug_instruction_table = 0;

/* Hash table for finding instructions from addresses */
static struct inst_hash {
  void *addr;
  struct scm_instruction *inst;
  struct inst_hash *next;
} *scm_instruction_hash_table[INSTRUCTION_HASH_SIZE];

static long scm_instruction_tag;

static SCM
make_instruction (struct scm_instruction *instp)
{
  SCM_RETURN_NEWSMOB (scm_instruction_tag, instp);
}

static int
print_instruction (SCM obj, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<instruction ", port);
  scm_puts (SCM_INSTRUCTION_DATA (obj)->name, port);
  scm_putc ('>', port);
  return 1;
}

static void
init_instruction_type ()
{
  scm_instruction_tag = scm_make_smob_type ("instruction", 0);
  scm_set_smob_print (scm_instruction_tag, print_instruction);
}

/* C interface */

static struct scm_instruction *
find_instruction_by_name (const char *name)
{
  struct scm_instruction *p;
  for (p = scm_regular_instruction_table; p->opcode != op_last; p++)
    if (strcmp (name, p->name) == 0)
      return p;
  return 0;
}

static struct scm_instruction *
find_instruction_by_code (SCM code)
{
  struct inst_hash *p;
  void *addr = SCM_CODE_TO_ADDR (code);
  for (p = scm_instruction_hash_table[INSTRUCTION_HASH (addr)]; p; p = p->next)
    if (p->addr == addr)
      return p->inst;
  return 0;
}

#ifdef HAVE_LABELS_AS_VALUES
static void *
instruction_code_to_debug_addr (SCM code)
{
  struct scm_instruction *p = find_instruction_by_code (code);
  return scm_debug_instruction_table[p->opcode].addr;
}
#endif

/* Scheme interface */

SCM_DEFINE (scm_instruction_p, "instruction?", 1, 0, 0,
	    (SCM obj),
"")
#define FUNC_NAME s_scm_instruction_p
{
  return SCM_BOOL (SCM_INSTRUCTION_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_system_instruction_p, "system-instruction?", 1, 0, 0,
	    (SCM obj),
"")
#define FUNC_NAME s_scm_system_instruction_p
{
  return SCM_BOOL (SCM_SYSTEM_INSTRUCTION_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_functional_instruction_p, "functional-instruction?", 1, 0, 0,
	    (SCM obj),
"")
#define FUNC_NAME s_scm_functional_instruction_p
{
  return SCM_BOOL (SCM_FUNCTIONAL_INSTRUCTION_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_name_p, "instruction-name?", 1, 0, 0,
	    (SCM name),
"")
#define FUNC_NAME s_scm_instruction_name_p
{
  SCM_VALIDATE_SYMBOL (1, name);
  return SCM_BOOL (find_instruction_by_name (SCM_CHARS (name)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_to_instruction, "symbol->instruction", 1, 0, 0,
	    (SCM name),
"")
#define FUNC_NAME s_scm_symbol_to_instruction
{
  struct scm_instruction *p;
  SCM_VALIDATE_SYMBOL (1, name);

  p = find_instruction_by_name (SCM_CHARS (name));
  if (!p)
    SCM_MISC_ERROR ("No such instruction: ~S", SCM_LIST1 (name));

  return p->obj;
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_list, "instruction-list", 0, 0, 0,
	    (),
"")
#define FUNC_NAME s_scm_instruction_list
{
  SCM list = SCM_EOL;
  struct scm_instruction *p;
  for (p = scm_regular_instruction_table; p->opcode != op_last; p++)
    list = scm_cons (p->obj, list);
  return scm_reverse_x (list, SCM_EOL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_opcode, "instruction-opcode", 1, 0, 0,
	    (SCM inst),
"")
#define FUNC_NAME s_scm_instruction_opcode
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  return SCM_MAKINUM (SCM_INSTRUCTION_DATA (inst)->opcode);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_name, "instruction-name", 1, 0, 0,
	    (SCM inst),
"")
#define FUNC_NAME s_scm_instruction_name
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  return SCM_CAR (scm_intern0 (SCM_INSTRUCTION_DATA (inst)->name));
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_type, "instruction-type", 1, 0, 0,
	    (SCM inst),
"")
#define FUNC_NAME s_scm_instruction_type
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  return SCM_MAKINUM (SCM_INSTRUCTION_DATA (inst)->type);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_scheme_name, "instruction-scheme-name", 1, 0, 0,
	    (SCM inst),
"")
#define FUNC_NAME s_scm_instruction_scheme_name
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  if (SCM_FUNCTIONAL_INSTRUCTION_P (inst))
    return SCM_CAR (scm_intern0 (SCM_INSTRUCTION_DATA (inst)->sname));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_arity, "instruction-arity", 1, 0, 0,
	    (SCM inst),
"")
#define FUNC_NAME s_scm_instruction_arity
{
  SCM_VALIDATE_INSTRUCTION (1, inst);
  if (SCM_FUNCTIONAL_INSTRUCTION_P (inst))
    {
      struct scm_instruction *p = SCM_INSTRUCTION_DATA (inst);
      return SCM_LIST2 (SCM_MAKINUM (p->nargs), SCM_BOOL (p->restp));
    }
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


/*
 * Bytecode
 */

static long scm_bytecode_tag;

static SCM
make_bytecode (int size)
{
  struct scm_bytecode *p
    = scm_must_malloc (sizeof (*p) + (size * sizeof (SCM)), "make_bytecode");
  p->size = size;
  SCM_RETURN_NEWSMOB (scm_bytecode_tag, p);
}

static SCM
mark_bytecode (SCM bytecode)
{
  int i;
  struct scm_instruction *p;

  int size = SCM_BYTECODE_SIZE (bytecode);
  SCM *base = SCM_BYTECODE_BASE (bytecode);

  for (i = 0; i < size; i++)
    {
      p = find_instruction_by_code (base[i]);
      switch (p->type)
	{
	case INST_NONE:
	  break;
	case INST_SCM:
	case INST_TOP:
	case INST_EXT:
	case INST_CODE:
	  scm_gc_mark (base[++i]);
	  break;
	case INST_INUM: /* a fixed integer; we don't need to mark it */
	case INST_ADDR: /* real memory address; we shouldn't mark it! */
	  i++;
	}
    }
  return SCM_BOOL_F;
}

static int
print_bytecode (SCM obj, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<bytecode 0x", port);
  scm_intprint ((long) SCM_BYTECODE_BASE (obj), 16, port);
  scm_putc ('>', port);
  return 1;
}

static scm_sizet
free_bytecode (SCM bytecode)
{
  int size = (sizeof (struct scm_bytecode)
	      + (SCM_BYTECODE_SIZE (bytecode) * sizeof (SCM)));
  if (SCM_BYTECODE_EXTS (bytecode))
    {
      size += (SCM_BYTECODE_EXTS (bytecode)[0] + 1) * sizeof (int);
      scm_must_free (SCM_BYTECODE_EXTS (bytecode));
    }
  scm_must_free (SCM_BYTECODE_DATA (bytecode));
  return size;
}

static void
init_bytecode_type ()
{
  scm_bytecode_tag = scm_make_smob_type ("bytecode", 0);
  scm_set_smob_mark (scm_bytecode_tag, mark_bytecode);
  scm_set_smob_print (scm_bytecode_tag, print_bytecode);
  scm_set_smob_free (scm_bytecode_tag, free_bytecode);
}

/* Internal functions */

static SCM
lookup_variable (SCM sym)
{
  SCM eclo = scm_standard_eval_closure (scm_selected_module ());
  SCM var = scm_eval_closure_lookup (eclo, sym, SCM_BOOL_F);
  if (SCM_FALSEP (var))
    var = scm_eval_closure_lookup (eclo, sym, SCM_BOOL_T);
  return var;
}

/* Scheme interface */

SCM_DEFINE (scm_bytecode_p, "bytecode?", 1, 0, 0,
	    (SCM obj),
"")
#define FUNC_NAME s_scm_bytecode_p
{
  return SCM_BOOL (SCM_BYTECODE_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_bytecode, "make-bytecode", 1, 0, 0,
	    (SCM code),
"")
#define FUNC_NAME s_scm_make_bytecode
{
  int i, size, len, offset;
  SCM header, body, nreqs, restp, nvars, nexts, exts, bytecode;
  SCM *old, *new, *address;

  /* Type check */
  SCM_VALIDATE_VECTOR (1, code);
  SCM_ASSERT_RANGE (1, code, SCM_LENGTH (code) == 2);
  header = SCM_VELTS (code)[0];
  body   = SCM_VELTS (code)[1];
  SCM_VALIDATE_VECTOR (1, header);
  SCM_VALIDATE_VECTOR (2, body);
  SCM_ASSERT_RANGE (1, header, SCM_LENGTH (header) == 5);
  nreqs = SCM_VELTS (header)[0];
  restp = SCM_VELTS (header)[1];
  nvars = SCM_VELTS (header)[2];
  nexts = SCM_VELTS (header)[3];
  exts  = SCM_VELTS (header)[4];
  SCM_VALIDATE_INUM (1, nreqs);
  SCM_VALIDATE_BOOL (2, restp);
  SCM_VALIDATE_INUM (3, nvars);
  SCM_VALIDATE_INUM (4, nexts);
  SCM_VALIDATE_VECTOR (5, exts);

  /* Create a new bytecode */
  size     = SCM_LENGTH (body);
  old      = SCM_VELTS (body);
  bytecode = make_bytecode (size);
  new      = SCM_BYTECODE_BASE (bytecode);

  /* Initialize the header */
  SCM_BYTECODE_NREQS (bytecode) = SCM_INUM (nreqs);
  SCM_BYTECODE_RESTP (bytecode) = SCM_FALSEP (restp) ? 0 : 1;
  SCM_BYTECODE_NVARS (bytecode) = SCM_INUM (nvars);
  SCM_BYTECODE_NEXTS (bytecode) = SCM_INUM (nexts);
  len = SCM_LENGTH (exts);
  if (len == 0)
    {
      SCM_BYTECODE_EXTS (bytecode) = NULL;
    }
  else
    {
      SCM_BYTECODE_EXTS (bytecode) =
	scm_must_malloc ((len + 1) * sizeof (int), FUNC_NAME);
      SCM_BYTECODE_EXTS (bytecode)[0] = len;
      for (i = 0; i < len; i++)
	SCM_BYTECODE_EXTS (bytecode)[i + 1] = SCM_INUM (SCM_VELTS (exts)[i]);
    }

  /* Initialize the body */
  for (i = 0; i < size; i++)
    {
      struct scm_instruction *p;

      /* Process instruction */
      if (!SCM_SYMBOLP (old[i])
	  || !(p = find_instruction_by_name (SCM_CHARS (old[i]))))
	SCM_MISC_ERROR ("Invalid instruction: ~S", SCM_LIST1 (old[i]));
      new[i] = SCM_ADDR_TO_CODE (p->addr);

      /* Process arguments */
      if (p->type == INST_NONE)
	continue;
      if (++i >= size)
	SCM_MISC_ERROR ("Unexpected end of code", SCM_EOL);
      switch (p->type)
	{
	case INST_NONE:
	  /* never come here */
	case INST_INUM:
	  SCM_VALIDATE_INUM (1, old[i]);
	  /* fall through */
	case INST_SCM:
	  /* just copy */
	  new[i] = old[i];
	  break;
	case INST_TOP:
	  /* top-level variable */
	  SCM_VALIDATE_SYMBOL (1, old[i]);
	  new[i] = lookup_variable (old[i]);
	  break;
	case INST_EXT:
	  /* just copy for now */
	  SCM_VALIDATE_CONS (1, old[i]);
	  SCM_VALIDATE_INUM (1, SCM_CAR (old[i]));
	  SCM_VALIDATE_INUM (1, SCM_CDR (old[i]));
	  new[i] = old[i];
	  break;
	case INST_CODE:
	  /* another bytecode */
	  new[i] = scm_make_bytecode (old[i]);
	  break;
	case INST_ADDR:
	  /* real address */
	  SCM_VALIDATE_INUM (1, old[i]);
	  /* Without the following intermediate variables, type conversion
	     fails on my machine.  Casting doesn't work well, why? */
	  offset  = SCM_INUM (old[i]);
	  address = new + offset;
	  new[i] = SCM_VM_MAKE_ADDRESS (address);
	  break;
	}
    }
  return bytecode;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytecode_decode, "bytecode-decode", 1, 0, 0,
	    (SCM bytecode),
"")
#define FUNC_NAME s_scm_bytecode_decode
{
  int i, size, offset;
  SCM code, *old, *new;

  SCM_VALIDATE_BYTECODE (1, bytecode);

  size = SCM_BYTECODE_SIZE (bytecode);
  old  = SCM_BYTECODE_BASE (bytecode);
  code = scm_make_vector (SCM_MAKINUM (size), SCM_BOOL_F);
  new  = SCM_VELTS (code);

  for (i = 0; i < size; i++)
    {
      struct scm_instruction *p;

      /* Process instruction */
      p = find_instruction_by_code (old[i]);
      if (!p)
	{
	broken:
	  SCM_MISC_ERROR ("Broken bytecode", SCM_EOL);
	}
      new[i] = scm_instruction_name (p->obj);

      /* Process arguments */
      if (p->type == INST_NONE)
	continue;
      if (++i >= size)
	goto broken;
      switch (p->type)
	{
	case INST_NONE:
	  /* never come here */
	case INST_INUM:
	case INST_SCM:
	case INST_EXT:
	  /* just copy */
	  new[i] = old[i];
	  break;
	case INST_TOP:
	  /* top-level variable */
	  new[i] = SCM_CAR (old[i]);
	  break;
	case INST_CODE:
	  /* another bytecode */
	  new[i] = scm_bytecode_decode (old[i]);
	  break;
	case INST_ADDR:
	  /* program address */
	  offset = SCM_VM_ADDRESS (old[i]) - old;
	  new[i] = SCM_MAKINUM (offset);
	  break;
	}
    }
  return code;
}
#undef FUNC_NAME


/*
 * Program
 */

static long scm_program_tag;

static SCM
make_program (SCM code, SCM env)
{
  SCM_RETURN_NEWSMOB2 (scm_program_tag, SCM_UNPACK (code), SCM_UNPACK (env));
}

static SCM
mark_program (SCM program)
{
  scm_gc_mark (SCM_PROGRAM_CODE (program));
  return SCM_PROGRAM_ENV (program);
}

static SCM scm_program_name (SCM program);

static int
print_program (SCM obj, SCM port, scm_print_state *pstate)
{
  SCM name = scm_program_name (obj);
  scm_puts ("#<program ", port);
  if (SCM_FALSEP (name))
    {
      scm_puts ("0x", port);
      scm_intprint ((long) SCM_PROGRAM_BASE (obj), 16, port);
    }
  else
    {
      scm_display (name, port);
    }
  scm_putc ('>', port);
  return 1;
}

static SCM scm_vm_apply (SCM vm, SCM program, SCM args);
static SCM make_vm (int stack_size);

static SCM
apply_program (SCM program, SCM args)
{
  return scm_vm_apply (make_vm (VM_DEFAULT_STACK_SIZE), program, args);
}

static void
init_program_type ()
{
  scm_program_tag = scm_make_smob_type ("program", 0);
  scm_set_smob_mark (scm_program_tag, mark_program);
  scm_set_smob_print (scm_program_tag, print_program);
  scm_set_smob_apply (scm_program_tag, apply_program, 0, 0, 1);
}

/* Scheme interface */

SCM_DEFINE (scm_program_p, "program?", 1, 0, 0,
	    (SCM obj),
"")
#define FUNC_NAME s_scm_program_p
{
  return SCM_BOOL (SCM_PROGRAM_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_program, "make-program", 2, 0, 0,
	    (SCM bytecode, SCM parent),
"")
#define FUNC_NAME s_scm_make_program
{
  SCM_VALIDATE_BYTECODE (1, bytecode);
  return make_program (bytecode, parent);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_name, "program-name", 1, 0, 0,
	    (SCM program),
"")
#define FUNC_NAME s_scm_program_name
{
  SCM_VALIDATE_PROGRAM (1, program);
  return scm_object_property (program, scm_sym_name);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_code, "program-code", 1, 0, 0,
	    (SCM program),
"")
#define FUNC_NAME s_scm_program_code
{
  SCM_VALIDATE_PROGRAM (1, program);
  return SCM_PROGRAM_CODE (program);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_base, "program-base", 1, 0, 0,
	    (SCM program),
"")
#define FUNC_NAME s_scm_program_base
{
  SCM_VALIDATE_PROGRAM (1, program);
  return SCM_VM_MAKE_ADDRESS (SCM_PROGRAM_BASE (program));
}
#undef FUNC_NAME


/*
 * VM Frame
 */

static long scm_vm_frame_tag;

/* This is used for debugging */
struct scm_vm_frame {
  int size;
  SCM program;
  SCM variables;
  SCM dynamic_link;
  SCM external_link;
  SCM stack_pointer;
  SCM return_address;
};

#define SCM_VM_FRAME_P(OBJ)	SCM_SMOB_PREDICATE (scm_vm_frame_tag, OBJ)
#define SCM_VM_FRAME_DATA(FR)	((struct scm_vm_frame *) SCM_SMOB_DATA (FR))
#define SCM_VALIDATE_VM_FRAME(POS,OBJ) SCM_MAKE_VALIDATE (POS, OBJ, VM_FRAME_P)

static SCM
make_vm_frame (SCM *fp)
{
  int i;
  int size = SCM_INUM (SCM_VM_FRAME_SIZE (fp));
  struct scm_vm_frame *p = scm_must_malloc (sizeof (*p), "make_vm_frame");
  p->program        = SCM_VM_FRAME_PROGRAM (fp);
  p->dynamic_link   = SCM_VM_FRAME_DYNAMIC_LINK (fp);
  p->external_link  = SCM_VM_FRAME_EXTERNAL_LINK (fp);
  p->stack_pointer  = SCM_VM_FRAME_STACK_POINTER (fp);
  p->return_address = SCM_VM_FRAME_RETURN_ADDRESS (fp);

  if (!SCM_FALSEP (p->dynamic_link))
    p->dynamic_link = make_vm_frame (SCM_VM_ADDRESS (p->dynamic_link));

  size += SCM_PROGRAM_NREQS (p->program) + SCM_PROGRAM_RESTP (p->program);
  p->variables = scm_make_vector (SCM_MAKINUM (size), SCM_BOOL_F);
  for (i = 0; i < size; i++)
    SCM_VELTS (p->variables)[i] = SCM_VM_FRAME_VARIABLE (fp, i);

  SCM_RETURN_NEWSMOB (scm_vm_frame_tag, p);
}

static SCM
mark_vm_frame (SCM frame)
{
  struct scm_vm_frame *p = SCM_VM_FRAME_DATA (frame);
  scm_gc_mark (p->program);
  scm_gc_mark (p->dynamic_link);
  scm_gc_mark (p->external_link);
  return p->variables;
}

static void
init_vm_frame_type ()
{
  scm_vm_frame_tag = scm_make_smob_type ("vm-frame", 0);
  scm_set_smob_mark (scm_vm_frame_tag, mark_vm_frame);
}

/* Scheme interface */

SCM_DEFINE (scm_frame_p, "frame?", 1, 0, 0,
	    (SCM obj),
"")
#define FUNC_NAME s_scm_frame_p
{
  return SCM_BOOL (SCM_VM_FRAME_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_program, "frame-program", 1, 0, 0,
	    (SCM frame),
"")
#define FUNC_NAME s_scm_frame_program
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_VM_FRAME_DATA (frame)->program;
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_variables, "frame-variables", 1, 0, 0,
	    (SCM frame),
"")
#define FUNC_NAME s_scm_frame_variables
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_VM_FRAME_DATA (frame)->variables;
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_dynamic_link, "frame-dynamic-link", 1, 0, 0,
	    (SCM frame),
"")
#define FUNC_NAME s_scm_frame_dynamic_link
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_VM_FRAME_DATA (frame)->dynamic_link;
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_external_link, "frame-external-link", 1, 0, 0,
	    (SCM frame),
"")
#define FUNC_NAME s_scm_frame_external_link
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_VM_FRAME_DATA (frame)->external_link;
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_stack_pointer, "frame-stack-pointer", 1, 0, 0,
	    (SCM frame),
"")
#define FUNC_NAME s_scm_frame_stack_pointer
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_VM_FRAME_DATA (frame)->stack_pointer;
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_return_address, "frame-return-address", 1, 0, 0,
	    (SCM frame),
"")
#define FUNC_NAME s_scm_frame_return_address
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_VM_FRAME_DATA (frame)->return_address;
}
#undef FUNC_NAME


/*
 * VM Continuation
 */

static long scm_vm_cont_tag;

static SCM
capture_vm_cont (struct scm_vm *vmp)
{
  struct scm_vm *p = scm_must_malloc (sizeof (*p), "capture_vm_cont");
  p->stack_size = vmp->stack_limit - vmp->sp;
  p->stack_base = scm_must_malloc (p->stack_size * sizeof (SCM),
				   "capture_vm_cont");
  p->stack_limit = p->stack_base + p->stack_size - 1;
  p->pc = vmp->pc;
  p->sp = (SCM *) (vmp->stack_limit - vmp->sp);
  p->fp = (SCM *) (vmp->stack_limit - vmp->fp);
  memcpy (p->stack_base, vmp->sp + 1, vmp->stack_size * sizeof (SCM));
  SCM_RETURN_NEWSMOB (scm_vm_cont_tag, p);
}

static void
reinstate_vm_cont (struct scm_vm *vmp, SCM cont)
{
  struct scm_vm *p = SCM_VM_CONT_VMP (cont);
  if (vmp->stack_size < p->stack_size)
    {
      puts ("FIXME: Need to expand");
      abort ();
    }
  vmp->pc = p->pc;
  vmp->sp = vmp->stack_limit - (int) p->sp;
  vmp->fp = vmp->stack_limit - (int) p->fp;
  memcpy (vmp->sp + 1, p->stack_base, p->stack_size * sizeof (SCM));
}

static SCM
mark_vm_cont (SCM cont)
{
  SCM *p;
  struct scm_vm *vmp = SCM_VM_CONT_VMP (cont);
  for (p = vmp->stack_base; p <= vmp->stack_limit; p++)
    if (SCM_NIMP (*p))
      scm_gc_mark (*p);
  return SCM_BOOL_F;
}

static scm_sizet
free_vm_cont (SCM cont)
{
  struct scm_vm *p = SCM_VM_CONT_VMP (cont);
  int size = sizeof (struct scm_vm) + p->stack_size * sizeof (SCM);
  scm_must_free (p->stack_base);
  scm_must_free (p);
  return size;
}

static void
init_vm_cont_type ()
{
  scm_vm_cont_tag = scm_make_smob_type ("vm-cont", 0);
  scm_set_smob_mark (scm_vm_cont_tag, mark_vm_cont);
  scm_set_smob_free (scm_vm_cont_tag, free_vm_cont);
}


/*
 * VM
 */

static long scm_vm_tag;

static SCM
make_vm (int stack_size)
{
  struct scm_vm *vmp = scm_must_malloc (sizeof (struct scm_vm), "make_vm");
  vmp->stack_size  = stack_size;
  vmp->stack_base  = scm_must_malloc (stack_size * sizeof (SCM), "make_vm");
  vmp->stack_limit = vmp->stack_base + vmp->stack_size - 1;
  vmp->sp    	   = vmp->stack_limit;
  vmp->ac    	   = SCM_BOOL_F;
  vmp->pc    	   = NULL;
  vmp->fp    	   = NULL;
  vmp->options     = SCM_EOL;
  vmp->boot_hook   = scm_make_hook (SCM_MAKINUM (1));
  vmp->halt_hook   = scm_make_hook (SCM_MAKINUM (1));
  vmp->next_hook   = scm_make_hook (SCM_MAKINUM (1));
  vmp->call_hook   = scm_make_hook (SCM_MAKINUM (1));
  vmp->apply_hook  = scm_make_hook (SCM_MAKINUM (1));
  vmp->return_hook = scm_make_hook (SCM_MAKINUM (1));
  SCM_RETURN_NEWSMOB (scm_vm_tag, vmp);
}

static SCM
mark_vm (SCM vm)
{
  SCM *p;
  struct scm_vm *vmp = SCM_VM_DATA (vm);
  for (p = vmp->sp + 1; p <= vmp->stack_limit; p++)
    if (SCM_NIMP (*p))
      scm_gc_mark (*p);

  scm_gc_mark (vmp->ac);
  scm_gc_mark (vmp->boot_hook);
  scm_gc_mark (vmp->halt_hook);
  scm_gc_mark (vmp->next_hook);
  scm_gc_mark (vmp->call_hook);
  scm_gc_mark (vmp->apply_hook);
  scm_gc_mark (vmp->return_hook);
  return vmp->options;
}

static void
init_vm_type ()
{
  scm_vm_tag = scm_make_smob_type ("vm", sizeof (struct scm_vm));
  scm_set_smob_mark (scm_vm_tag, mark_vm);
}

/* Scheme interface */

SCM_DEFINE (scm_vm_version, "vm-version", 0, 0, 0,
	    (),
"")
#define FUNC_NAME s_scm_vm_version
{
  return scm_makfrom0str (VERSION);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_p, "vm?", 1, 0, 0,
	    (SCM obj),
"")
#define FUNC_NAME s_scm_vm_p
{
  return SCM_BOOL (SCM_VM_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_vm, "make-vm", 0, 0, 0,
	    (),
"")
#define FUNC_NAME s_scm_make_vm
{
  return make_vm (VM_DEFAULT_STACK_SIZE);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_ac, "vm:ac", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_ac
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->ac;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_pc, "vm:pc", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_pc
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_MAKE_ADDRESS (SCM_VM_DATA (vm)->pc);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_sp, "vm:sp", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_sp
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_MAKE_ADDRESS (SCM_VM_DATA (vm)->sp);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_fp, "vm:fp", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_fp
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_MAKE_ADDRESS (SCM_VM_DATA (vm)->fp);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_current_frame, "vm-current-frame", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_current_frame
{
  SCM_VALIDATE_VM (1, vm);
  return make_vm_frame (SCM_VM_DATA (vm)->fp);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_fetch_code, "vm-fetch-code", 2, 0, 0,
	    (SCM vm, SCM addr),
"")
#define FUNC_NAME s_scm_vm_fetch_code
{
  SCM *p, list;
  struct scm_instruction *inst;

  SCM_VALIDATE_VM (1, vm);
  SCM_VALIDATE_INUM (2, addr);

  p = SCM_VM_ADDRESS (addr);

  inst = find_instruction_by_code (*p);
  if (!inst)
    SCM_MISC_ERROR ("Broken bytecode", SCM_LIST1 (addr));

  list = SCM_LIST1 (scm_instruction_name (inst->obj));
  if (inst->type != INST_NONE)
    {
      if (inst->type == INST_ADDR)
	{
	  p = SCM_CODE_TO_ADDR (p[1]);
	  SCM_SETCDR (list, SCM_LIST1 (SCM_VM_MAKE_ADDRESS (p)));
	}
      else
	SCM_SETCDR (list, SCM_LIST1 (p[1]));
    }
  return list;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_stack_to_list, "vm-stack->list", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_stack_to_list
{
  struct scm_vm *vmp;
  SCM *p, list = SCM_EOL;

  SCM_VALIDATE_VM (1, vm);

  vmp = SCM_VM_DATA (vm);
  for (p = vmp->sp + 1; p <= vmp->stack_limit; p++)
    list = scm_cons (*p, list);
  return list;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_option, "vm-option", 2, 0, 0,
	    (SCM vm, SCM key),
"")
#define FUNC_NAME s_scm_vm_option
{
  SCM_VALIDATE_VM (1, vm);
  SCM_VALIDATE_SYMBOL (2, key);
  return scm_assq_ref (SCM_VM_DATA (vm)->options, key);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_set_option_x, "vm-set-option!", 3, 0, 0,
	    (SCM vm, SCM key, SCM val),
"")
#define FUNC_NAME s_scm_vm_set_option_x
{
  SCM_VALIDATE_VM (1, vm);
  SCM_VALIDATE_SYMBOL (2, key);
  SCM_VM_DATA (vm)->options
    = scm_assq_set_x (SCM_VM_DATA (vm)->options, key, val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_boot_hook, "vm-boot-hook", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_boot_hook
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->boot_hook;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_halt_hook, "vm-halt-hook", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_halt_hook
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->halt_hook;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_next_hook, "vm-next-hook", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_next_hook
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->next_hook;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_call_hook, "vm-call-hook", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_call_hook
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->call_hook;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_apply_hook, "vm-apply-hook", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_apply_hook
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->apply_hook;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_return_hook, "vm-return-hook", 1, 0, 0,
	    (SCM vm),
"")
#define FUNC_NAME s_scm_vm_return_hook
{
  SCM_VALIDATE_VM (1, vm);
  return SCM_VM_DATA (vm)->return_hook;
}
#undef FUNC_NAME

SCM_SYMBOL (sym_debug, "debug");

static SCM scm_regular_vm (SCM vm, SCM program);
static SCM scm_debug_vm (SCM vm, SCM program);

#define VM_CODE(name) SCM_ADDR_TO_CODE (find_instruction_by_name (name)->addr)

SCM_DEFINE (scm_vm_run, "vm-run", 2, 0, 0,
	    (SCM vm, SCM program),
"")
#define FUNC_NAME s_scm_vm_run
{
  SCM bootcode;
  static SCM template[5];

  SCM_VALIDATE_VM (1, vm);
  SCM_VALIDATE_PROGRAM (2, program);

  if (SCM_EQ_P (template[0], SCM_PACK (0)))
    {
      template[0] = VM_CODE ("%loadc");
      template[1] = SCM_BOOL_F; /* overwritten */
      template[2] = VM_CODE ("%call");
      template[3] = SCM_MAKINUM (0);
      template[4] = VM_CODE ("%halt");
    }

  /* Create a boot program */
  bootcode = make_bytecode (5);
  memcpy (SCM_BYTECODE_BASE (bootcode), template, sizeof (SCM) * 5);
  SCM_BYTECODE_BASE (bootcode)[1] = program;
  SCM_BYTECODE_SIZE (bootcode)    = 5;
  SCM_BYTECODE_EXTS (bootcode)    = NULL;
  SCM_BYTECODE_NREQS (bootcode)   = 0;
  SCM_BYTECODE_RESTP (bootcode)   = 0;
  SCM_BYTECODE_NVARS (bootcode)   = 0;
  SCM_BYTECODE_NEXTS (bootcode)   = 0;
  program = SCM_MAKE_PROGRAM (bootcode, SCM_BOOL_F);

  if (SCM_FALSEP (scm_vm_option (vm, sym_debug)))
    return scm_regular_vm (vm, program);
  else
    return scm_debug_vm (vm, program);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_apply, "vm-apply", 3, 0, 0,
	    (SCM vm, SCM program, SCM args),
"")
#define FUNC_NAME s_scm_vm_apply
{
  int len;
  SCM bootcode;
  static SCM template[7];

  SCM_VALIDATE_VM (1, vm);
  SCM_VALIDATE_PROGRAM (2, program);
  SCM_VALIDATE_LIST_COPYLEN (3, args, len);

  if (SCM_EQ_P (template[0], SCM_PACK (0)))
    {
      template[0] = VM_CODE ("%push-list");
      template[1] = SCM_EOL; /* overwritten */
      template[2] = VM_CODE ("%loadc");
      template[3] = SCM_BOOL_F; /* overwritten */
      template[4] = VM_CODE ("%call");
      template[5] = SCM_MAKINUM (0); /* overwritten */
      template[6] = VM_CODE ("%halt");
    }

  /* Create a boot program */
  bootcode = make_bytecode (7);
  memcpy (SCM_BYTECODE_BASE (bootcode), template, sizeof (SCM) * 7);
  SCM_BYTECODE_BASE (bootcode)[1] = args;
  SCM_BYTECODE_BASE (bootcode)[3] = program;
  SCM_BYTECODE_BASE (bootcode)[5] = SCM_MAKINUM (len);
  SCM_BYTECODE_SIZE (bootcode)    = 7;
  SCM_BYTECODE_EXTS (bootcode)    = NULL;
  SCM_BYTECODE_NREQS (bootcode)   = 0;
  SCM_BYTECODE_RESTP (bootcode)   = 0;
  SCM_BYTECODE_NVARS (bootcode)   = 0;
  SCM_BYTECODE_NEXTS (bootcode)   = 0;
  program = SCM_MAKE_PROGRAM (bootcode, SCM_BOOL_F);

  if (SCM_FALSEP (scm_vm_option (vm, sym_debug)))
    return scm_regular_vm (vm, program);
  else
    return scm_debug_vm (vm, program);
}
#undef FUNC_NAME


/*
 * The VM engines
 */

/* We don't want to snarf the engines */
#ifndef SCM_MAGIC_SNARFER

/* the regular engine */
#define VM_ENGINE SCM_VM_REGULAR_ENGINE
#include "vm_engine.c"
#undef VM_ENGINE

/* the debug engine */
#define VM_ENGINE SCM_VM_DEBUG_ENGINE
#include "vm_engine.c"
#undef VM_ENGINE

#endif /* not SCM_MAGIC_SNARFER */


/*
 * Initialize
 */

static SCM scm_module_vm;

void
scm_init_vm ()
{
  SCM old_module;

  /* Initialize the module */
  scm_module_vm = scm_make_module (scm_read_0str ("(vm vm)"));
  old_module = scm_select_module (scm_module_vm);

  init_instruction_type ();
  init_bytecode_type ();
  init_program_type ();
  init_vm_frame_type ();
  init_vm_cont_type ();
  init_vm_type ();

#include "vm.x"

  scm_select_module (old_module);

  /* Initialize instruction tables */
  {
    int i;
    struct scm_instruction *p;

    SCM vm = make_vm (0);
    scm_regular_vm (vm, SCM_BOOL_F);
    scm_debug_vm (vm, SCM_BOOL_F);

    /* hash table */
    for (i = 0; i < INSTRUCTION_HASH_SIZE; i++)
      scm_instruction_hash_table[i] = NULL;

    for (p = scm_regular_instruction_table; p->opcode != op_last; p++)
      {
	int hash;
	struct inst_hash *data;
	SCM inst = scm_permanent_object (make_instruction (p));
	p->obj = inst;
	if (p->restp) p->type = INST_INUM;
	hash = INSTRUCTION_HASH (p->addr);
	data = scm_must_malloc (sizeof (*data), "inst_hash");
	data->addr = p->addr;
	data->inst = p;
	data->next = scm_instruction_hash_table[hash];
	scm_instruction_hash_table[hash] = data;
      }
  }
}

void
scm_init_vm_vm_module ()
{
  scm_register_module_xxx ("vm vm", (void *) scm_init_vm);
}
