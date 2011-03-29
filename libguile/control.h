/* Copyright (C) 2010, 2011  Free Software Foundation, Inc.
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

#ifndef SCM_CONTROL_H
#define SCM_CONTROL_H


#define SCM_F_PROMPT_ESCAPE 0x1

#define SCM_PROMPT_P(x)		(!SCM_IMP (x) && SCM_TYP7(x) == scm_tc7_prompt)
#define SCM_PROMPT_FLAGS(x)	(SCM_CELL_WORD ((x), 0) >> 8)
#define SCM_PROMPT_ESCAPE_P(x)	(SCM_PROMPT_FLAGS (x) & SCM_F_PROMPT_ESCAPE)
#define SCM_PROMPT_TAG(x)	(SCM_CELL_OBJECT ((x), 1))
#define SCM_PROMPT_REGISTERS(x)	((struct scm_prompt_registers*)SCM_CELL_WORD ((x), 2))
#define SCM_PROMPT_DYNWINDS(x)	(SCM_CELL_OBJECT ((x), 3))

#define SCM_PROMPT_SETJMP(p)	(SCM_I_SETJMP (SCM_PROMPT_REGISTERS (p)->regs))

struct scm_prompt_registers
{
  scm_t_uint8 *ip;
  SCM *sp;
  SCM *fp;
  scm_t_int64 cookie;
  scm_i_jmp_buf regs;  
};


SCM_INTERNAL SCM scm_c_make_prompt (SCM k, SCM *fp, SCM *sp,
                                    scm_t_uint8 *abort_ip,
                                    scm_t_uint8 escape_only_p,
                                    scm_t_int64 vm_cookie,
                                    SCM winds);
SCM_INTERNAL SCM scm_i_prompt_pop_abort_args_x (SCM vm);

SCM_INTERNAL void scm_c_abort (SCM vm, SCM tag, size_t n, SCM *argv,
                               scm_t_int64 cookie) SCM_NORETURN;
SCM_INTERNAL SCM scm_at_abort (SCM tag, SCM args) SCM_NORETURN;


SCM_INTERNAL void scm_i_prompt_print (SCM exp, SCM port, scm_print_state *pstate);
SCM_INTERNAL void scm_init_control (void);


#endif /* SCM_CONTROL_H */
