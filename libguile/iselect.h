/* classes: h_files */

#ifndef SCM_ISELECT_H
#define SCM_ISELECT_H

/* Copyright (C) 1997,1998,2000,2001, 2002, 2006, 2013 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"

/* Needed for FD_SET on some systems.  */
#include <sys/types.h>

#include <sys/select.h>

SCM_API int scm_std_select (int fds,
			    fd_set *rfds,
			    fd_set *wfds,
			    fd_set *efds,
			    struct timeval *timeout);

#define SELECT_TYPE fd_set

#endif  /* SCM_ISELECT_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
