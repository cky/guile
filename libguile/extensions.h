/* classes: h_files */

#ifndef SCM_EXTENSIONS_H
#define SCM_EXTENSIONS_H

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



SCM_API void scm_c_register_extension (const char *lib, const char *init,
				       void (*func) (void *), void *data);

SCM_API void scm_c_load_extension (const char *lib, const char *init);
SCM_API SCM scm_load_extension (SCM lib, SCM init);

SCM_API void scm_init_extensions (void);

#endif  /* SCM_EXTENSIONS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
