/*      Copyright (C) 1995,1996,1997, 2000, 2001 Free Software Foundation, Inc.

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


#include "libguile/gh.h"

void 
gh_display (SCM x)
{
  scm_display (x, scm_current_output_port ());
}

void 
gh_write (SCM x)
{
  scm_write (x, scm_current_output_port ());
}

void 
gh_newline ()
{
  scm_newline (scm_current_output_port ());
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
