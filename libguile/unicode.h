/* classes: h_files */

#ifndef SCM_UNICODE_H
#define SCM_UNICODE_H

/* Copyright (C) 2014 Free Software Foundation, Inc.
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.  If not, see
 * <http://www.gnu.org/licenses/>.
 */



#include "libguile/__scm.h"

SCM_INTERNAL SCM scm_formal_name_to_char (SCM);
SCM_INTERNAL SCM scm_char_to_formal_name (SCM);
SCM_INTERNAL void scm_init_unicode (void);

#endif  /* SCM_UNICODE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
