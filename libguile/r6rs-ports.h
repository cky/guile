#ifndef SCM_R6RS_PORTS_H
#define SCM_R6RS_PORTS_H

/* Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
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

/* R6RS I/O Ports.  */

SCM_API SCM scm_eof_object (void);
SCM_API SCM scm_open_bytevector_input_port (SCM, SCM);
SCM_API SCM scm_make_custom_binary_input_port (SCM, SCM, SCM, SCM, SCM);
SCM_API SCM scm_get_u8 (SCM);
SCM_API SCM scm_lookahead_u8 (SCM);
SCM_API SCM scm_get_bytevector_n (SCM, SCM);
SCM_API SCM scm_get_bytevector_n_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_get_bytevector_some (SCM);
SCM_API SCM scm_get_bytevector_all (SCM);
SCM_API SCM scm_put_u8 (SCM, SCM);
SCM_API SCM scm_put_bytevector (SCM, SCM, SCM, SCM);
SCM_API SCM scm_open_bytevector_output_port (SCM);
SCM_API SCM scm_make_custom_binary_output_port (SCM, SCM, SCM, SCM, SCM);
SCM_API SCM scm_get_string_n_x (SCM, SCM, SCM, SCM);

SCM_API void scm_init_r6rs_ports (void);
SCM_INTERNAL void scm_register_r6rs_ports (void);

#endif /* SCM_R6RS_PORTS_H */
