/* classes: h_files */

#ifndef SCM_WIN32_SOCKET_H
#define SCM_WIN32_SOCKET_H

/* Copyright (C) 2001 Free Software Foundation, Inc.
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

#ifdef SCM_HAVE_WINSOCK2_H
# include <winsock2.h>
#endif

int scm_i_socket_errno (void);
char * scm_i_socket_strerror (int error);
void scm_i_init_socket_Win32 (void);
char * scm_i_socket_filename (char *file);

struct servent * getservent (void);
void setservent (int stayopen);
void endservent (void);
struct protoent * getprotoent (void);
void setprotoent (int stayopen);
void endprotoent (void);

#endif /* SCM_WIN32_SOCKET_H */
