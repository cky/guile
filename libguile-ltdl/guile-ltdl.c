/* ltdl.c -- system independent dlopen wrapper
   Copyright (C) 1998, 1999, 2000, 2002 Free Software Foundation, Inc.
   Originally by Thomas Tanner <tanner@ffii.org>
   This file is part of GNU Libtool.

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
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307  USA

*/

#include "guile-ltdl.h"

#define lt_dlhandle_struct scm_i_lt_dlhandle_struct

#define LT_SCOPE static
#define SCMLTXT static
#define SCMLTSTATIC static

#ifdef __GNUC__
#define SCM_UNUSED __attribute__ ((unused))
#else
#define SCM_UNUSED
#endif

#include "raw-ltdl.h"
#include "raw-ltdl.c"

void
scm_lt_dlset_preloaded_symbols (void)
{
  extern const lt_dlsymlist lt_preloaded_symbols[];
  lt_dlpreload_default(lt_preloaded_symbols);
}

int
scm_lt_dlinit (void)
{
  return lt_dlinit ();
}

scm_lt_dlhandle
scm_lt_dlopenext (const char *filename)
{
  return lt_dlopenext (filename);
}

scm_lt_ptr
scm_lt_dlsym (scm_lt_dlhandle handle, const char *name)
{
  return lt_dlsym (handle, name);
}

const char *
scm_lt_dlerror (void)
{
  return lt_dlerror ();
}

int
scm_lt_dlclose (scm_lt_dlhandle handle)
{
  return lt_dlclose (handle);
}
