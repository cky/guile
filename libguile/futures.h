/* classes: h_files */

#ifndef SCM_FUTURES_H
#define SCM_FUTURES_H

/* Copyright (C) 2002, 2003 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"
#include "libguile/threads.h"



typedef struct scm_t_future {
  SCM data;
  scm_t_mutex mutex;
  scm_t_cond cond;
  int status;
  int die_p;
} scm_t_future;

#define SCM_FUTURE_DEAD 0
#define SCM_FUTURE_SIGNAL_ME -1
#define SCM_FUTURE_COMPUTING 1
#define SCM_FUTURE_TASK_ASSIGNED 2

#define SCM_VALIDATE_FUTURE(pos, obj) \
  SCM_ASSERT_TYPE (SCM_TYP16_PREDICATE (scm_tc16_future, obj), \
		   obj, pos, FUNC_NAME, "future");
#define SCM_FUTURE(future) ((scm_t_future *) SCM_CELL_WORD_2 (future))
#define SCM_FUTURE_MUTEX(future) (&SCM_FUTURE (future)->mutex)
#define SCM_FUTURE_COND(future) (&SCM_FUTURE (future)->cond)
#define SCM_FUTURE_STATUS(future) (SCM_FUTURE (future)->status)
#define SCM_SET_FUTURE_STATUS(future, x) \
 do { SCM_FUTURE (future)->status = (x); } while (0)
#define SCM_FUTURE_ALIVE_P(future) (SCM_FUTURE_STATUS (future))
#define SCM_FUTURE_DATA(future) (SCM_FUTURE (future)->data)
#define SCM_SET_FUTURE_DATA(future, x) \
 do { SCM_FUTURE (future)->data = (x); } while (0)
#define SCM_FUTURE_NEXT SCM_CELL_OBJECT_1
#define SCM_FUTURE_NEXTLOC(x) ((SCM *) SCM_CELL_WORD_LOC ((x), 1))
#define SCM_SET_FUTURE_NEXT SCM_SET_CELL_OBJECT_1

SCM_API scm_t_bits scm_tc16_future;

extern SCM *scm_loc_sys_thread_handler;

SCM_API SCM scm_i_make_future (SCM thunk);
SCM_API SCM scm_make_future (SCM thunk);
SCM_API SCM scm_future_ref (SCM future);

void scm_init_futures (void);

#endif  /* SCM_FUTURES_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
