/* guile-ltdl.h -- dlopen function actually used by guile
   Copyright (C) 1998-2000, 2002 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

As a special exception to the GNU Lesser General Public License,
if you distribute this file as part of a program or library that
is built using GNU libtool, you may include it under the same
distribution terms that you use for the rest of that program.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307  USA
*/

/* Only include this header file once. */
#ifndef SCM_LTDL_H
#define SCM_LTDL_H 1

typedef struct scm_i_lt_dlhandle_struct *scm_lt_dlhandle;
typedef struct scm_i_lt_dlsymlist_struct scm_lt_dlsymlist;
typedef void * scm_lt_ptr;

void            scm_lt_dlpreload_default (const scm_lt_dlsymlist *preloads);
int             scm_lt_dlinit (void);
scm_lt_dlhandle scm_lt_dlopenext (const char *filename);
scm_lt_ptr      scm_lt_dlsym (scm_lt_dlhandle handle, const char *name);
const char     *scm_lt_dlerror (void);
int             scm_lt_dlclose (scm_lt_dlhandle handle);

#endif /* !SCM_LTDL_H */
