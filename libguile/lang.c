/*	Copyright (C) 1999, 2000, 2001, 2006, 2008, 2010 Free Software Foundation, Inc.
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



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"

#include "libguile/eval.h"
#include "libguile/macros.h"
#include "libguile/root.h"

#include "libguile/validate.h"
#include "libguile/lang.h"



/* {Multi-language support}
 */

void
scm_init_lang ()
{
#include "libguile/lang.x"

  scm_c_define ("%nil", SCM_ELISP_NIL);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
