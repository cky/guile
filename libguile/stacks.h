/* classes: h_files */

#ifndef SCM_STACKS_H
#define SCM_STACKS_H
/* Copyright (C) 1995,1996,2000,2001 Free Software Foundation
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"

/* {Frames and stacks}
 */

typedef struct scm_t_info_frame {
  /* SCM flags; */
  scm_t_bits flags;
  SCM source;
  SCM proc;
  SCM args;
} scm_t_info_frame;
#define SCM_FRAME_N_SLOTS (sizeof (scm_t_info_frame) / sizeof (SCM))

#define SCM_STACK(obj) ((scm_t_stack *) SCM_STRUCT_DATA (obj))
#define SCM_STACK_LAYOUT "pwuourpW"
typedef struct scm_t_stack {
  SCM id;			/* Stack id */
  scm_t_info_frame *frames;	/* Info frames */
  unsigned long length;		/* Stack length */
  unsigned long tail_length;
  scm_t_info_frame tail[1];
} scm_t_stack;

#if (SCM_DEBUG_DEPRECATED == 0)
# define scm_info_frame scm_t_info_frame
# define scm_stack scm_t_stack
#endif

extern SCM scm_t_stackype;

#define SCM_STACKP(obj) (SCM_STRUCTP (obj) && SCM_EQ_P (SCM_STRUCT_VTABLE (obj), scm_t_stackype))
#define SCM_STACK_LENGTH(stack) (SCM_STACK (stack) -> length)

#define SCM_FRAMEP(obj) \
  (SCM_CONSP (obj) && SCM_STACKP (SCM_CAR (obj)) \
   && SCM_INUMP (SCM_CDR (obj)) && SCM_INUM (SCM_CDR (obj)) >= 0 \
   && ((unsigned long int) SCM_INUM (SCM_CDR (obj)) \
       < SCM_STACK_LENGTH (SCM_CAR (obj))))

#define SCM_FRAME_REF(frame, slot) \
(SCM_STACK (SCM_CAR (frame)) -> frames[SCM_INUM (SCM_CDR (frame))].slot) \

#define SCM_FRAME_NUMBER(frame) \
(SCM_BACKWARDS_P \
 ? SCM_INUM (SCM_CDR (frame)) \
 : (SCM_STACK_LENGTH (SCM_CAR (frame)) \
    - SCM_INUM (SCM_CDR (frame)) \
    - 1)) \

#define SCM_FRAME_FLAGS(frame) SCM_FRAME_REF (frame, flags)
#define SCM_FRAME_SOURCE(frame) SCM_FRAME_REF (frame, source)
#define SCM_FRAME_PROC(frame) SCM_FRAME_REF (frame, proc)
#define SCM_FRAME_ARGS(frame) SCM_FRAME_REF (frame, args)
#define SCM_FRAME_PREV(frame) scm_frame_previous (frame)
#define SCM_FRAME_NEXT(frame) scm_frame_next (frame)

#define SCM_FRAMEF_VOID		(1L << 2)
#define SCM_FRAMEF_REAL		(1L << 3)
#define SCM_FRAMEF_PROC 	(1L << 4)
#define SCM_FRAMEF_EVAL_ARGS 	(1L << 5)
#define SCM_FRAMEF_OVERFLOW	(1L << 6)

#define SCM_FRAME_VOID_P(f)       (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_VOID)
#define SCM_FRAME_REAL_P(f)       (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_REAL)
#define SCM_FRAME_PROC_P(f)       (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_PROC)
#define SCM_FRAME_EVAL_ARGS_P(f)  (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_EVAL_ARGS)
#define SCM_FRAME_OVERFLOW_P(f)   (SCM_FRAME_FLAGS (f) & SCM_FRAMEF_OVERFLOW)



SCM scm_stack_p (SCM obj);
SCM scm_make_stack (SCM obj, SCM args);
SCM scm_stack_id (SCM stack);
SCM scm_stack_ref (SCM stack, SCM i);
SCM scm_stack_length (SCM stack);

SCM scm_frame_p (SCM obj);
SCM scm_last_stack_frame (SCM obj);
SCM scm_frame_number (SCM frame);
SCM scm_frame_source (SCM frame);
SCM scm_frame_procedure (SCM frame);
SCM scm_frame_arguments (SCM frame);
SCM scm_frame_previous (SCM frame);
SCM scm_frame_next (SCM frame);
SCM scm_frame_real_p (SCM frame);
SCM scm_frame_procedure_p (SCM frame);
SCM scm_frame_evaluating_args_p (SCM frame);
SCM scm_frame_overflow_p (SCM frame);


void scm_init_stacks (void);

#endif /* SCM_STACKS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
