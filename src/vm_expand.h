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

#include "config.h"

#ifndef VM_LABEL
#define VM_LABEL(TAG) l_##TAG## 
#define VM_OPCODE(TAG) op_##TAG## 

#ifdef HAVE_LABELS_AS_VALUES
#define VM_TAG(TAG) VM_LABEL(TAG):
#define VM_ADDR(TAG) &&VM_LABEL(TAG)
#else /* not HAVE_LABELS_AS_VALUES */
#define VM_TAG(TAG) case VM_OPCODE(TAG):
#define VM_ADDR(TAG) NULL
#endif /* not HAVE_LABELS_AS_VALUES */
#endif /* VM_LABEL */

#undef SCM_DEFINE_INSTRUCTION
#undef SCM_DEFINE_VM_FUNCTION
#ifdef VM_INSTRUCTION_TO_TABLE
/*
 * These will go to scm_instruction_table in vm.c
 */
#define SCM_DEFINE_INSTRUCTION(TAG,NAME,TYPE) \
  {VM_OPCODE(TAG), TYPE, NAME, SCM_PACK (0), NULL, 0, 0},
#define SCM_DEFINE_VM_FUNCTION(TAG,SNAME,NAME,NARGS,RESTP) \
  {VM_OPCODE(TAG), INST_NONE, NAME, SCM_PACK (0), SNAME, NARGS, RESTP},

#else
#ifdef VM_INSTRUCTION_TO_LABEL
/*
 * These will go to jump_table in vm_engine.c
 */
#define SCM_DEFINE_INSTRUCTION(TAG,NAME,TYPE)		   VM_ADDR(TAG),
#define SCM_DEFINE_VM_FUNCTION(TAG,SNAME,NAME,NARGS,RESTP) VM_ADDR(TAG),

#else
#ifdef VM_INSTRUCTION_TO_OPCODE
/*
 * These will go to scm_opcode in vm.h
 */
#define SCM_DEFINE_INSTRUCTION(TAG,NAME,TYPE)		   VM_OPCODE(TAG),
#define SCM_DEFINE_VM_FUNCTION(TAG,SNAME,NAME,NARGS,RESTP) VM_OPCODE(TAG),

#else /* Otherwise */
/*
 * These are directly included in vm_engine.c
 */
#define SCM_DEFINE_INSTRUCTION(TAG,NAME,TYPE)		   VM_TAG(TAG)
#define SCM_DEFINE_VM_FUNCTION(TAG,SNAME,NAME,NARGS,RESTP) VM_TAG(TAG)

#endif /* VM_INSTRUCTION_TO_OPCODE */
#endif /* VM_INSTRUCTION_TO_LABEL */
#endif /* VM_INSTRUCTION_TO_TABLE */
