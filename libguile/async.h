/* classes: h_files */

#ifndef SCM_ASYNC_H
#define SCM_ASYNC_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2004 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */



#include "libguile/__scm.h"
#include "libguile/root.h"



#define scm_mask_ints (scm_root->block_asyncs != 0)



SCM_API void scm_async_click (void);
SCM_API void scm_switch (void);
SCM_API SCM scm_async (SCM thunk);
SCM_API SCM scm_async_mark (SCM a);
SCM_API SCM scm_system_async_mark (SCM a);
SCM_API SCM scm_system_async_mark_for_thread (SCM a, SCM thread);
SCM_API void scm_i_queue_async_cell (SCM cell, scm_root_state *);
SCM_API SCM scm_run_asyncs (SCM list_of_a);
SCM_API SCM scm_noop (SCM args);
SCM_API SCM scm_call_with_blocked_asyncs (SCM proc);
SCM_API SCM scm_call_with_unblocked_asyncs (SCM proc);
void *scm_c_call_with_blocked_asyncs (void *(*p) (void *d), void *d);
void *scm_c_call_with_unblocked_asyncs (void *(*p) (void *d), void *d);
void scm_with_blocked_asyncs (void);
void scm_with_unblocked_asyncs (void);
SCM_API void scm_init_async (void);

#if (SCM_ENABLE_DEPRECATED == 1)

SCM_API SCM scm_system_async (SCM thunk);
SCM_API SCM scm_unmask_signals (void);
SCM_API SCM scm_mask_signals (void);

#endif

#endif  /* SCM_ASYNC_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
