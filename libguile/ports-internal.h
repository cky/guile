/*
 * ports-internal.h - internal-only declarations for ports.
 *
 * Copyright (C) 2013, 2014 Free Software Foundation, Inc.
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

#ifndef SCM_PORTS_INTERNAL
#define SCM_PORTS_INTERNAL

#include "libguile/_scm.h"
#include "libguile/ports.h"

enum scm_port_encoding_mode {
  SCM_PORT_ENCODING_MODE_UTF8,
  SCM_PORT_ENCODING_MODE_ICONV
};

typedef enum scm_port_encoding_mode scm_t_port_encoding_mode;

/* This is a separate object so that only those ports that use iconv
   cause finalizers to be registered (FIXME: although currently in 2.0
   finalizers are always registered for ports anyway).  */
struct scm_iconv_descriptors
{
  /* input/output iconv conversion descriptors */
  void *input_cd;
  void *output_cd;
};

typedef struct scm_iconv_descriptors scm_t_iconv_descriptors;

struct scm_port_internal
{
  unsigned at_stream_start_for_bom_read  : 1;
  unsigned at_stream_start_for_bom_write : 1;
  scm_t_port_encoding_mode encoding_mode;
  scm_t_iconv_descriptors *iconv_descriptors;
  unsigned char pending_eof: 1;

  /* When non-NULL, this is the method called by 'setvbuf' for this port.
     It must create read and write buffers for PORT with the specified
     sizes (a size of 0 is for unbuffered ports, which should use the
     'shortbuf' field.)  Size -1 means to use the port's preferred buffer
     size.  */
  /* XXX: In 2.2 make this a property of the 'scm_t_ptob_descriptor'.  */
  void (*setvbuf) (SCM port, long read_size, long write_size);

  /* Key-value properties.  */
  SCM alist;
};

typedef struct scm_port_internal scm_t_port_internal;

#define SCM_UNICODE_BOM  0xFEFFUL  /* Unicode byte-order mark */

#define SCM_PORT_GET_INTERNAL(x)                                \
  ((scm_t_port_internal *) (SCM_PTAB_ENTRY(x)->input_cd))

SCM_INTERNAL scm_t_iconv_descriptors *
scm_i_port_iconv_descriptors (SCM port, scm_t_port_rw_active mode);

#endif
