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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/bytevectors.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/r6rs-ports.h"
#include "libguile/strings.h"
#include "libguile/validate.h"
#include "libguile/values.h"
#include "libguile/vectors.h"



/* Unimplemented features.  */


/* Transoders are currently not implemented since Guile 1.8 is not
   Unicode-capable.  Thus, most of the code here assumes the use of the
   binary transcoder.  */
static inline void
transcoders_not_implemented (void)
{
  fprintf (stderr, "%s: warning: transcoders not implemented\n",
	   PACKAGE_NAME);
}


/* End-of-file object.  */

SCM_DEFINE (scm_eof_object, "eof-object", 0, 0, 0,
	    (void),
	    "Return the end-of-file object.")
#define FUNC_NAME s_scm_eof_object
{
  return (SCM_EOF_VAL);
}
#undef FUNC_NAME


/* Input ports.  */

#ifndef MIN
# define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

/* Bytevector input ports or "bip" for short.  */
static scm_t_bits bytevector_input_port_type = 0;

static inline SCM
make_bip (SCM bv)
{
  SCM port;
  char *c_bv;
  unsigned c_len;
  scm_t_port *c_port;
  const unsigned long mode_bits = SCM_OPN | SCM_RDNG;

  scm_i_scm_pthread_mutex_lock (&scm_i_port_table_mutex);

  port = scm_new_port_table_entry (bytevector_input_port_type);

  /* Prevent BV from being GC'd.  */
  SCM_SETSTREAM (port, SCM_UNPACK (bv));

  /* Have the port directly access the bytevector.  */
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  c_len = SCM_BYTEVECTOR_LENGTH (bv);

  c_port = SCM_PTAB_ENTRY (port);
  c_port->read_pos = c_port->read_buf = (unsigned char *) c_bv;
  c_port->read_end = (unsigned char *) c_bv + c_len;
  c_port->read_buf_size = c_len;

  /* Mark PORT as open, readable and unbuffered (hmm, how elegant...).  */
  SCM_SET_CELL_TYPE (port, bytevector_input_port_type | mode_bits);

  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  return port;
}

static int
bip_fill_input (SCM port)
{
  int result;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);

  if (c_port->read_pos >= c_port->read_end)
    result = EOF;
  else
    result = (int) *c_port->read_pos;

  return result;
}

static scm_t_off
bip_seek (SCM port, scm_t_off offset, int whence)
#define FUNC_NAME "bip_seek"
{
  scm_t_off c_result = 0;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);

  switch (whence)
    {
    case SEEK_CUR:
      offset += c_port->read_pos - c_port->read_buf;
      /* Fall through.  */

    case SEEK_SET:
      if (c_port->read_buf + offset <= c_port->read_end)
	{
	  c_port->read_pos = c_port->read_buf + offset;
	  c_result = offset;
	}
      else
	scm_out_of_range (FUNC_NAME, scm_from_int (offset));
      break;

    case SEEK_END:
      if (c_port->read_end - offset >= c_port->read_buf)
	{
	  c_port->read_pos = c_port->read_end - offset;
	  c_result = c_port->read_pos - c_port->read_buf;
	}
      else
	scm_out_of_range (FUNC_NAME, scm_from_int (offset));
      break;

    default:
      scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
			      "invalid `seek' parameter");
    }

  return c_result;
}
#undef FUNC_NAME


/* Instantiate the bytevector input port type.  */
static inline void
initialize_bytevector_input_ports (void)
{
  bytevector_input_port_type =
    scm_make_port_type ("r6rs-bytevector-input-port", bip_fill_input,
			NULL);

  scm_set_port_seek (bytevector_input_port_type, bip_seek);
}


SCM_DEFINE (scm_open_bytevector_input_port,
	    "open-bytevector-input-port", 1, 1, 0,
	    (SCM bv, SCM transcoder),
	    "Return an input port whose contents are drawn from "
	    "bytevector @var{bv}.")
#define FUNC_NAME s_scm_open_bytevector_input_port
{
  SCM_VALIDATE_BYTEVECTOR (1, bv);
  if (!SCM_UNBNDP (transcoder) && !scm_is_false (transcoder))
    transcoders_not_implemented ();

  return (make_bip (bv));
}
#undef FUNC_NAME


/* Custom binary ports.  The following routines are shared by input and
   output custom binary ports.  */

#define SCM_CBP_GET_POSITION_PROC(_port)			\
  SCM_SIMPLE_VECTOR_REF (SCM_PACK (SCM_STREAM (_port)), 1)
#define SCM_CBP_SET_POSITION_PROC(_port)			\
  SCM_SIMPLE_VECTOR_REF (SCM_PACK (SCM_STREAM (_port)), 2)
#define SCM_CBP_CLOSE_PROC(_port)				\
  SCM_SIMPLE_VECTOR_REF (SCM_PACK (SCM_STREAM (_port)), 3)

static scm_t_off
cbp_seek (SCM port, scm_t_off offset, int whence)
#define FUNC_NAME "cbp_seek"
{
  SCM result;
  scm_t_off c_result = 0;

  switch (whence)
    {
    case SEEK_CUR:
      {
	SCM get_position_proc;

	get_position_proc = SCM_CBP_GET_POSITION_PROC (port);
	if (SCM_LIKELY (scm_is_true (get_position_proc)))
	  result = scm_call_0 (get_position_proc);
	else
	  scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
				  "R6RS custom binary port does not "
				  "support `port-position'");

	offset += scm_to_int (result);
	/* Fall through.  */
      }

    case SEEK_SET:
      {
	SCM set_position_proc;

	set_position_proc = SCM_CBP_SET_POSITION_PROC (port);
	if (SCM_LIKELY (scm_is_true (set_position_proc)))
	  result = scm_call_1 (set_position_proc, scm_from_int (offset));
	else
	  scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
				  "R6RS custom binary port does not "
				  "support `set-port-position!'");

	/* Assuming setting the position succeeded.  */
	c_result = offset;
	break;
      }

    default:
      /* `SEEK_END' cannot be supported.  */
      scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
			      "R6RS custom binary ports do not "
			      "support `SEEK_END'");
    }

  return c_result;
}
#undef FUNC_NAME

static int
cbp_close (SCM port)
{
  SCM close_proc;

  close_proc = SCM_CBP_CLOSE_PROC (port);
  if (scm_is_true (close_proc))
    /* Invoke the `close' thunk.  */
    scm_call_0 (close_proc);

  return 1;
}


/* Custom binary input port ("cbip" for short).  */

static scm_t_bits custom_binary_input_port_type = 0;

/* Size of the buffer embedded in custom binary input ports.  */
#define CBIP_BUFFER_SIZE  4096

/* Return the bytevector associated with PORT.  */
#define SCM_CBIP_BYTEVECTOR(_port)				\
  SCM_SIMPLE_VECTOR_REF (SCM_PACK (SCM_STREAM (_port)), 4)

/* Return the various procedures of PORT.  */
#define SCM_CBIP_READ_PROC(_port)				\
  SCM_SIMPLE_VECTOR_REF (SCM_PACK (SCM_STREAM (_port)), 0)


static inline SCM
make_cbip (SCM read_proc, SCM get_position_proc,
	   SCM set_position_proc, SCM close_proc)
{
  SCM port, bv, method_vector;
  char *c_bv;
  unsigned c_len;
  scm_t_port *c_port;
  const unsigned long mode_bits = SCM_OPN | SCM_RDNG;

  /* Use a bytevector as the underlying buffer.  */
  c_len = CBIP_BUFFER_SIZE;
  bv = scm_c_make_bytevector (c_len);
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);

  /* Store the various methods and bytevector in a vector.  */
  method_vector = scm_c_make_vector (5, SCM_BOOL_F);
  SCM_SIMPLE_VECTOR_SET (method_vector, 4, bv);
  SCM_SIMPLE_VECTOR_SET (method_vector, 0, read_proc);
  SCM_SIMPLE_VECTOR_SET (method_vector, 1, get_position_proc);
  SCM_SIMPLE_VECTOR_SET (method_vector, 2, set_position_proc);
  SCM_SIMPLE_VECTOR_SET (method_vector, 3, close_proc);

  scm_i_pthread_mutex_lock (&scm_i_port_table_mutex);

  port = scm_new_port_table_entry (custom_binary_input_port_type);

  /* Attach it the method vector.  */
  SCM_SETSTREAM (port, SCM_UNPACK (method_vector));

  /* Have the port directly access the buffer (bytevector).  */
  c_port = SCM_PTAB_ENTRY (port);
  c_port->read_pos = c_port->read_buf = (unsigned char *) c_bv;
  c_port->read_end = (unsigned char *) c_bv;
  c_port->read_buf_size = c_len;

  /* Mark PORT as open, readable and unbuffered (hmm, how elegant...).  */
  SCM_SET_CELL_TYPE (port, custom_binary_input_port_type | mode_bits);

  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  return port;
}

static int
cbip_fill_input (SCM port)
#define FUNC_NAME "cbip_fill_input"
{
  int result;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);

 again:
  if (c_port->read_pos >= c_port->read_end)
    {
      /* Invoke the user's `read!' procedure.  */
      unsigned c_octets;
      SCM bv, read_proc, octets;

      /* Use the bytevector associated with PORT as the buffer passed to the
	 `read!' procedure, thereby avoiding additional allocations.  */
      bv = SCM_CBIP_BYTEVECTOR (port);
      read_proc = SCM_CBIP_READ_PROC (port);

      /* The assumption here is that C_PORT's internal buffer wasn't changed
	 behind our back.  */
      assert (c_port->read_buf ==
	      (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv));
      assert ((unsigned) c_port->read_buf_size
	      == SCM_BYTEVECTOR_LENGTH (bv));

      octets = scm_call_3 (read_proc, bv, SCM_INUM0,
			   SCM_I_MAKINUM (CBIP_BUFFER_SIZE));
      c_octets = scm_to_uint (octets);

      c_port->read_pos = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);
      c_port->read_end = (unsigned char *) c_port->read_pos + c_octets;

      if (c_octets > 0)
	goto again;
      else
	result = EOF;
    }
  else
    result = (int) *c_port->read_pos;

  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_custom_binary_input_port,
	    "make-custom-binary-input-port", 5, 0, 0,
	    (SCM id, SCM read_proc, SCM get_position_proc,
	     SCM set_position_proc, SCM close_proc),
	    "Return a new custom binary input port whose input is drained "
	    "by invoking @var{read_proc} and passing it a bytevector, an "
	    "index where octets should be written, and an octet count.")
#define FUNC_NAME s_scm_make_custom_binary_input_port
{
  SCM_VALIDATE_STRING (1, id);
  SCM_VALIDATE_PROC (2, read_proc);

  if (!scm_is_false (get_position_proc))
    SCM_VALIDATE_PROC (3, get_position_proc);

  if (!scm_is_false (set_position_proc))
    SCM_VALIDATE_PROC (4, set_position_proc);

  if (!scm_is_false (close_proc))
    SCM_VALIDATE_PROC (5, close_proc);

  return (make_cbip (read_proc, get_position_proc, set_position_proc,
		     close_proc));
}
#undef FUNC_NAME


/* Instantiate the custom binary input port type.  */
static inline void
initialize_custom_binary_input_ports (void)
{
  custom_binary_input_port_type =
    scm_make_port_type ("r6rs-custom-binary-input-port",
			cbip_fill_input, NULL);

  scm_set_port_seek (custom_binary_input_port_type, cbp_seek);
  scm_set_port_close (custom_binary_input_port_type, cbp_close);
}



/* Binary input.  */

/* We currently don't support specific binary input ports.  */
#define SCM_VALIDATE_BINARY_INPUT_PORT SCM_VALIDATE_OPINPORT

SCM_DEFINE (scm_get_u8, "get-u8", 1, 0, 0,
	    (SCM port),
	    "Read an octet from @var{port}, a binary input port, "
	    "blocking as necessary.")
#define FUNC_NAME s_scm_get_u8
{
  SCM result;
  int c_result;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);

  c_result = scm_get_byte_or_eof (port);
  if (c_result == EOF)
    result = SCM_EOF_VAL;
  else
    result = SCM_I_MAKINUM ((unsigned char) c_result);

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_lookahead_u8, "lookahead-u8", 1, 0, 0,
	    (SCM port),
	    "Like @code{get-u8} but does not update @var{port} to "
	    "point past the octet.")
#define FUNC_NAME s_scm_lookahead_u8
{
  int u8;
  SCM result;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);

  u8 = scm_get_byte_or_eof (port);
  if (u8 == EOF)
    result = SCM_EOF_VAL;
  else
    {
      scm_unget_byte (u8, port);
      result = SCM_I_MAKINUM ((scm_t_uint8) u8);
    }

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_bytevector_n, "get-bytevector-n", 2, 0, 0,
	    (SCM port, SCM count),
	    "Read @var{count} octets from @var{port}, blocking as "
	    "necessary and return a bytevector containing the octets "
	    "read.  If fewer bytes are available, a bytevector smaller "
	    "than @var{count} is returned.")
#define FUNC_NAME s_scm_get_bytevector_n
{
  SCM result;
  char *c_bv;
  unsigned c_count;
  size_t c_read;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);
  c_count = scm_to_uint (count);

  result = scm_c_make_bytevector (c_count);
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (result);

  if (SCM_LIKELY (c_count > 0))
    /* XXX: `scm_c_read ()' does not update the port position.  */
    c_read = scm_c_read (port, c_bv, c_count);
  else
    /* Don't invoke `scm_c_read ()' since it may block.  */
    c_read = 0;

  if ((c_read == 0) && (c_count > 0))
    {
      if (SCM_EOF_OBJECT_P (scm_peek_char (port)))
	result = SCM_EOF_VAL;
      else
	result = scm_null_bytevector;
    }
  else
    {
      if (c_read < c_count)
	result = scm_c_shrink_bytevector (result, c_read);
    }

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_bytevector_n_x, "get-bytevector-n!", 4, 0, 0,
	    (SCM port, SCM bv, SCM start, SCM count),
	    "Read @var{count} bytes from @var{port} and store them "
	    "in @var{bv} starting at index @var{start}.  Return either "
	    "the number of bytes actually read or the end-of-file "
	    "object.")
#define FUNC_NAME s_scm_get_bytevector_n_x
{
  SCM result;
  char *c_bv;
  unsigned c_start, c_count, c_len;
  size_t c_read;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);
  SCM_VALIDATE_BYTEVECTOR (2, bv);
  c_start = scm_to_uint (start);
  c_count = scm_to_uint (count);

  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  c_len = SCM_BYTEVECTOR_LENGTH (bv);

  if (SCM_UNLIKELY (c_start + c_count > c_len))
    scm_out_of_range (FUNC_NAME, count);

  if (SCM_LIKELY (c_count > 0))
    c_read = scm_c_read (port, c_bv + c_start, c_count);
  else
    /* Don't invoke `scm_c_read ()' since it may block.  */
    c_read = 0;

  if ((c_read == 0) && (c_count > 0))
    {
      if (SCM_EOF_OBJECT_P (scm_peek_char (port)))
	result = SCM_EOF_VAL;
      else
	result = SCM_I_MAKINUM (0);
    }
  else
    result = scm_from_size_t (c_read);

  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_get_bytevector_some, "get-bytevector-some", 1, 0, 0,
	    (SCM port),
	    "Read from @var{port}, blocking as necessary, until data "
	    "are available or and end-of-file is reached.  Return either "
	    "a new bytevector containing the data read or the "
	    "end-of-file object.")
#define FUNC_NAME s_scm_get_bytevector_some
{
  /* Read at least one byte, unless the end-of-file is already reached, and
     read while characters are available (buffered).  */

  SCM result;
  char *c_bv;
  unsigned c_len;
  size_t c_total;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);

  c_len = 4096;
  c_bv = (char *) scm_gc_malloc_pointerless (c_len, SCM_GC_BYTEVECTOR);
  c_total = 0;

  do
    {
      int c_chr;

      if (c_total + 1 > c_len)
	{
	  /* Grow the bytevector.  */
	  c_bv = (char *) scm_gc_realloc (c_bv, c_len, c_len * 2,
					  SCM_GC_BYTEVECTOR);
	  c_len *= 2;
	}

      /* We can't use `scm_c_read ()' since it blocks.  */
      c_chr = scm_getc (port);
      if (c_chr != EOF)
	{
	  c_bv[c_total] = (char) c_chr;
	  c_total++;
	}
    }
  while ((scm_is_true (scm_char_ready_p (port)))
	 && (!SCM_EOF_OBJECT_P (scm_peek_char (port))));

  if (c_total == 0)
    {
      result = SCM_EOF_VAL;
      scm_gc_free (c_bv, c_len, SCM_GC_BYTEVECTOR);
    }
  else
    {
      if (c_len > c_total)
	{
	  /* Shrink the bytevector.  */
	  c_bv = (char *) scm_gc_realloc (c_bv, c_len, c_total,
					  SCM_GC_BYTEVECTOR);
	  c_len = (unsigned) c_total;
	}

      result = scm_c_take_bytevector ((signed char *) c_bv, c_len);
    }

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_bytevector_all, "get-bytevector-all", 1, 0, 0,
	    (SCM port),
	    "Read from @var{port}, blocking as necessary, until "
	    "the end-of-file is reached.  Return either "
	    "a new bytevector containing the data read or the "
	    "end-of-file object (if no data were available).")
#define FUNC_NAME s_scm_get_bytevector_all
{
  SCM result;
  char *c_bv;
  unsigned c_len, c_count;
  size_t c_read, c_total;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);

  c_len = c_count = 4096;
  c_bv = (char *) scm_gc_malloc_pointerless (c_len, SCM_GC_BYTEVECTOR);
  c_total = c_read = 0;

  do
    {
      if (c_total + c_read > c_len)
	{
	  /* Grow the bytevector.  */
	  c_bv = (char *) scm_gc_realloc (c_bv, c_len, c_len * 2,
					  SCM_GC_BYTEVECTOR);
	  c_count = c_len;
	  c_len *= 2;
	}

      /* `scm_c_read ()' blocks until C_COUNT bytes are available or EOF is
	 reached.  */
      c_read = scm_c_read (port, c_bv + c_total, c_count);
      c_total += c_read, c_count -= c_read;
    }
  while (!SCM_EOF_OBJECT_P (scm_peek_char (port)));

  if (c_total == 0)
    {
      result = SCM_EOF_VAL;
      scm_gc_free (c_bv, c_len, SCM_GC_BYTEVECTOR);
    }
  else
    {
      if (c_len > c_total)
	{
	  /* Shrink the bytevector.  */
	  c_bv = (char *) scm_gc_realloc (c_bv, c_len, c_total,
					  SCM_GC_BYTEVECTOR);
	  c_len = (unsigned) c_total;
	}

      result = scm_c_take_bytevector ((signed char *) c_bv, c_len);
    }

  return result;
}
#undef FUNC_NAME



/* Binary output.  */

/* We currently don't support specific binary input ports.  */
#define SCM_VALIDATE_BINARY_OUTPUT_PORT SCM_VALIDATE_OPOUTPORT


SCM_DEFINE (scm_put_u8, "put-u8", 2, 0, 0,
	    (SCM port, SCM octet),
	    "Write @var{octet} to binary port @var{port}.")
#define FUNC_NAME s_scm_put_u8
{
  scm_t_uint8 c_octet;

  SCM_VALIDATE_BINARY_OUTPUT_PORT (1, port);
  c_octet = scm_to_uint8 (octet);

  scm_putc ((char) c_octet, port);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_put_bytevector, "put-bytevector", 2, 2, 0,
	    (SCM port, SCM bv, SCM start, SCM count),
	    "Write the contents of @var{bv} to @var{port}, optionally "
	    "starting at index @var{start} and limiting to @var{count} "
	    "octets.")
#define FUNC_NAME s_scm_put_bytevector
{
  char *c_bv;
  unsigned c_start, c_count, c_len;

  SCM_VALIDATE_BINARY_OUTPUT_PORT (1, port);
  SCM_VALIDATE_BYTEVECTOR (2, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);

  if (start != SCM_UNDEFINED)
    {
      c_start = scm_to_uint (start);

      if (count != SCM_UNDEFINED)
	{
	  c_count = scm_to_uint (count);
	  if (SCM_UNLIKELY (c_start + c_count > c_len))
	    scm_out_of_range (FUNC_NAME, count);
	}
      else
	{
	  if (SCM_UNLIKELY (c_start >= c_len))
	    scm_out_of_range (FUNC_NAME, start);
	  else
	    c_count = c_len - c_start;
	}
    }
  else
    c_start = 0, c_count = c_len;

  scm_c_write (port, c_bv + c_start, c_count);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Bytevector output port ("bop" for short).  */

/* Implementation of "bops".

   Each bop has an internal buffer, of type `scm_t_bop_buffer', attached to
   it.  The procedure returned along with the output port is actually an
   applicable SMOB.  The SMOB holds a reference to the port.  When applied,
   the SMOB swallows the port's internal buffer, turning it into a
   bytevector, and resets it.

   XXX: Access to a bop's internal buffer is not thread-safe.  */

static scm_t_bits bytevector_output_port_type = 0;

SCM_SMOB (bytevector_output_port_procedure,
	  "r6rs-bytevector-output-port-procedure",
	  0);

#define SCM_GC_BOP "r6rs-bytevector-output-port"
#define SCM_BOP_BUFFER_INITIAL_SIZE 4096

/* Representation of a bop's internal buffer.  */
typedef struct
{
  size_t total_len;
  size_t len;
  size_t pos;
  char  *buffer;
} scm_t_bop_buffer;


/* Accessing a bop's buffer.  */
#define SCM_BOP_BUFFER(_port)		\
  ((scm_t_bop_buffer *) SCM_STREAM (_port))
#define SCM_SET_BOP_BUFFER(_port, _buf)		\
  (SCM_SETSTREAM ((_port), (scm_t_bits) (_buf)))


static inline void
bop_buffer_init (scm_t_bop_buffer *buf)
{
  buf->total_len = buf->len = buf->pos = 0;
  buf->buffer = NULL;
}

static inline void
bop_buffer_grow (scm_t_bop_buffer *buf, size_t min_size)
{
  char *new_buf;
  size_t new_size;

  for (new_size = buf->total_len
	 ? buf->total_len : SCM_BOP_BUFFER_INITIAL_SIZE;
       new_size < min_size;
       new_size *= 2);

  if (buf->buffer)
    new_buf = scm_gc_realloc ((void *) buf->buffer, buf->total_len,
			      new_size, SCM_GC_BOP);
  else
    new_buf = scm_gc_malloc_pointerless (new_size, SCM_GC_BOP);

  buf->buffer = new_buf;
  buf->total_len = new_size;
}

static inline SCM
make_bop (void)
{
  SCM port, bop_proc;
  scm_t_port *c_port;
  scm_t_bop_buffer *buf;
  const unsigned long mode_bits = SCM_OPN | SCM_WRTNG;

  scm_i_pthread_mutex_lock (&scm_i_port_table_mutex);

  port = scm_new_port_table_entry (bytevector_output_port_type);

  buf = (scm_t_bop_buffer *) scm_gc_malloc (sizeof (* buf), SCM_GC_BOP);
  bop_buffer_init (buf);

  c_port = SCM_PTAB_ENTRY (port);
  c_port->write_buf = c_port->write_pos = c_port->write_end = NULL;
  c_port->write_buf_size = 0;

  SCM_SET_BOP_BUFFER (port, buf);

  /* Mark PORT as open and writable.  */
  SCM_SET_CELL_TYPE (port, bytevector_output_port_type | mode_bits);

  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  /* Make the bop procedure.  */
  SCM_NEWSMOB (bop_proc, bytevector_output_port_procedure, buf);

  return (scm_values (scm_list_2 (port, bop_proc)));
}

/* Write SIZE octets from DATA to PORT.  */
static void
bop_write (SCM port, const void *data, size_t size)
{
  scm_t_bop_buffer *buf;

  buf = SCM_BOP_BUFFER (port);

  if (buf->pos + size > buf->total_len)
    bop_buffer_grow (buf, buf->pos + size);

  memcpy (buf->buffer + buf->pos, data, size);
  buf->pos += size;
  buf->len = (buf->len > buf->pos) ? buf->len : buf->pos;
}

static scm_t_off
bop_seek (SCM port, scm_t_off offset, int whence)
#define FUNC_NAME "bop_seek"
{
  scm_t_bop_buffer *buf;

  buf = SCM_BOP_BUFFER (port);
  switch (whence)
    {
    case SEEK_CUR:
      offset += (scm_t_off) buf->pos;
      /* Fall through.  */

    case SEEK_SET:
      if (offset < 0 || (unsigned) offset > buf->len)
	scm_out_of_range (FUNC_NAME, scm_from_int (offset));
      else
	buf->pos = offset;
      break;

    case SEEK_END:
      if (offset < 0 || (unsigned) offset >= buf->len)
	scm_out_of_range (FUNC_NAME, scm_from_int (offset));
      else
	buf->pos = buf->len - (offset + 1);
      break;

    default:
      scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
			      "invalid `seek' parameter");
    }

  return buf->pos;
}
#undef FUNC_NAME

/* Fetch data from a bop.  */
SCM_SMOB_APPLY (bytevector_output_port_procedure,
		bop_proc_apply, 0, 0, 0, (SCM bop_proc))
{
  SCM bv;
  scm_t_bop_buffer *buf, result_buf;

  buf = (scm_t_bop_buffer *) SCM_SMOB_DATA (bop_proc);

  result_buf = *buf;
  bop_buffer_init (buf);

  if (result_buf.len == 0)
    bv = scm_c_take_bytevector (NULL, 0);
  else
    {
      if (result_buf.total_len > result_buf.len)
	/* Shrink the buffer.  */
	result_buf.buffer = scm_gc_realloc ((void *) result_buf.buffer,
					    result_buf.total_len,
					    result_buf.len,
					    SCM_GC_BOP);

      bv = scm_c_take_bytevector ((signed char *) result_buf.buffer,
				       result_buf.len);
    }

  return bv;
}

SCM_DEFINE (scm_open_bytevector_output_port,
	    "open-bytevector-output-port", 0, 1, 0,
	    (SCM transcoder),
	    "Return two values: an output port and a procedure.  The latter "
	    "should be called with zero arguments to obtain a bytevector "
	    "containing the data accumulated by the port.")
#define FUNC_NAME s_scm_open_bytevector_output_port
{
  if (!SCM_UNBNDP (transcoder) && !scm_is_false (transcoder))
    transcoders_not_implemented ();

  return (make_bop ());
}
#undef FUNC_NAME

static inline void
initialize_bytevector_output_ports (void)
{
  bytevector_output_port_type =
    scm_make_port_type ("r6rs-bytevector-output-port",
			NULL, bop_write);

  scm_set_port_seek (bytevector_output_port_type, bop_seek);
}


/* Custom binary output port ("cbop" for short).  */

static scm_t_bits custom_binary_output_port_type;

/* Return the various procedures of PORT.  */
#define SCM_CBOP_WRITE_PROC(_port)				\
  SCM_SIMPLE_VECTOR_REF (SCM_PACK (SCM_STREAM (_port)), 0)


static inline SCM
make_cbop (SCM write_proc, SCM get_position_proc,
	   SCM set_position_proc, SCM close_proc)
{
  SCM port, method_vector;
  scm_t_port *c_port;
  const unsigned long mode_bits = SCM_OPN | SCM_WRTNG;

  /* Store the various methods and bytevector in a vector.  */
  method_vector = scm_c_make_vector (4, SCM_BOOL_F);
  SCM_SIMPLE_VECTOR_SET (method_vector, 0, write_proc);
  SCM_SIMPLE_VECTOR_SET (method_vector, 1, get_position_proc);
  SCM_SIMPLE_VECTOR_SET (method_vector, 2, set_position_proc);
  SCM_SIMPLE_VECTOR_SET (method_vector, 3, close_proc);

  scm_i_pthread_mutex_lock (&scm_i_port_table_mutex);

  port = scm_new_port_table_entry (custom_binary_output_port_type);

  /* Attach it the method vector.  */
  SCM_SETSTREAM (port, SCM_UNPACK (method_vector));

  /* Have the port directly access the buffer (bytevector).  */
  c_port = SCM_PTAB_ENTRY (port);
  c_port->write_buf = c_port->write_pos = c_port->write_end = NULL;
  c_port->write_buf_size = c_port->read_buf_size = 0;

  /* Mark PORT as open, writable and unbuffered.  */
  SCM_SET_CELL_TYPE (port, custom_binary_output_port_type | mode_bits);

  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  return port;
}

/* Write SIZE octets from DATA to PORT.  */
static void
cbop_write (SCM port, const void *data, size_t size)
#define FUNC_NAME "cbop_write"
{
  long int c_result;
  size_t c_written;
  SCM bv, write_proc, result;

  /* XXX: Allocating a new bytevector at each `write' call is inefficient,
     but necessary since (1) we don't control the lifetime of the buffer
     pointed to by DATA, and (2) the `write!' procedure could capture the
     bytevector it is passed.  */
  bv = scm_c_make_bytevector (size);
  memcpy (SCM_BYTEVECTOR_CONTENTS (bv), data, size);

  write_proc = SCM_CBOP_WRITE_PROC (port);

  /* Since the `write' procedure of Guile's ports has type `void', it must
     try hard to write exactly SIZE bytes, regardless of how many bytes the
     sink can handle.  */
  for (c_written = 0;
       c_written < size;
       c_written += c_result)
    {
      result = scm_call_3 (write_proc, bv,
			   scm_from_size_t (c_written),
			   scm_from_size_t (size - c_written));

      c_result = scm_to_long (result);
      if (SCM_UNLIKELY (c_result < 0
			|| (size_t) c_result > (size - c_written)))
	scm_wrong_type_arg_msg (FUNC_NAME, 0, result,
				"R6RS custom binary output port `write!' "
				"returned a incorrect integer");
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_custom_binary_output_port,
	    "make-custom-binary-output-port", 5, 0, 0,
	    (SCM id, SCM write_proc, SCM get_position_proc,
	     SCM set_position_proc, SCM close_proc),
	    "Return a new custom binary output port whose output is drained "
	    "by invoking @var{write_proc} and passing it a bytevector, an "
	    "index where octets should be written, and an octet count.")
#define FUNC_NAME s_scm_make_custom_binary_output_port
{
  SCM_VALIDATE_STRING (1, id);
  SCM_VALIDATE_PROC (2, write_proc);

  if (!scm_is_false (get_position_proc))
    SCM_VALIDATE_PROC (3, get_position_proc);

  if (!scm_is_false (set_position_proc))
    SCM_VALIDATE_PROC (4, set_position_proc);

  if (!scm_is_false (close_proc))
    SCM_VALIDATE_PROC (5, close_proc);

  return (make_cbop (write_proc, get_position_proc, set_position_proc,
		     close_proc));
}
#undef FUNC_NAME


/* Instantiate the custom binary output port type.  */
static inline void
initialize_custom_binary_output_ports (void)
{
  custom_binary_output_port_type =
    scm_make_port_type ("r6rs-custom-binary-output-port",
			NULL, cbop_write);

  scm_set_port_seek (custom_binary_output_port_type, cbp_seek);
  scm_set_port_close (custom_binary_output_port_type, cbp_close);
}


/* Transcoded ports ("tp" for short).  */
static scm_t_bits transcoded_port_type = 0;

#define TP_INPUT_BUFFER_SIZE 4096

#define SCM_TP_BINARY_PORT(_port) SCM_PACK (SCM_STREAM (_port))

static inline SCM
make_tp (SCM binary_port, unsigned long mode)
{
  SCM port;
  scm_t_port *c_port;
  const unsigned long mode_bits = SCM_OPN | mode;
  
  scm_i_pthread_mutex_lock (&scm_i_port_table_mutex);

  port = scm_new_port_table_entry (transcoded_port_type);

  SCM_SETSTREAM (port, SCM_UNPACK (binary_port));

  SCM_SET_CELL_TYPE (port, transcoded_port_type | mode_bits);

  if (SCM_INPUT_PORT_P (port))
    {
      c_port = SCM_PTAB_ENTRY (port);
      c_port->read_buf = scm_gc_malloc_pointerless (TP_INPUT_BUFFER_SIZE,
                                                    "port buffer");
      c_port->read_pos = c_port->read_end = c_port->read_buf;
      c_port->read_buf_size = TP_INPUT_BUFFER_SIZE;
      
      SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) & ~SCM_BUF0);
    }
  
  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  return port;
}

static void
tp_write (SCM port, const void *data, size_t size)
{
  scm_c_write (SCM_TP_BINARY_PORT (port), data, size);
}

static int
tp_fill_input (SCM port)
{
  size_t count;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);
  SCM bport = SCM_TP_BINARY_PORT (port);
  scm_t_port *c_bport = SCM_PTAB_ENTRY (bport);

  /* We can't use `scm_c_read' here, since it blocks until the whole
     block has been read or EOF. */
  
  if (c_bport->rw_active == SCM_PORT_WRITE)
    scm_force_output (bport);

  if (c_bport->read_pos >= c_bport->read_end)
    scm_fill_input (bport);
  
  count = c_bport->read_end - c_bport->read_pos;
  if (count > c_port->read_buf_size)
    count = c_port->read_buf_size;

  memcpy (c_port->read_buf, c_bport->read_pos, count);
  c_bport->read_pos += count;

  if (c_bport->rw_random)
    c_bport->rw_active = SCM_PORT_READ;

  if (count == 0)
    return EOF;
  else
    {
      c_port->read_pos = c_port->read_buf;
      c_port->read_end = c_port->read_buf + count;
      return *c_port->read_buf;
    }
}

static void
tp_flush (SCM port)
{
  SCM binary_port = SCM_TP_BINARY_PORT (port);
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);
  size_t count = c_port->write_pos - c_port->write_buf;

  scm_c_write (binary_port, c_port->write_buf, count);

  c_port->write_pos = c_port->write_buf;
  c_port->rw_active = SCM_PORT_NEITHER;

  scm_force_output (binary_port);
}

static int
tp_close (SCM port)
{
  if (SCM_OUTPUT_PORT_P (port))
    tp_flush (port);
  return scm_is_true (scm_close_port (SCM_TP_BINARY_PORT (port))) ? 0 : -1;
}

static inline void
initialize_transcoded_ports (void)
{
  transcoded_port_type =
    scm_make_port_type ("r6rs-transcoded-port", tp_fill_input, tp_write);
  
  scm_set_port_flush (transcoded_port_type, tp_flush);
  scm_set_port_close (transcoded_port_type, tp_close);
}

SCM_DEFINE (scm_i_make_transcoded_port,
	    "%make-transcoded-port", 1, 0, 0,
	    (SCM port),
	    "Return a new port which reads and writes to @var{port}")
#define FUNC_NAME s_scm_i_make_transcoded_port
{
  SCM result;
  unsigned long mode = 0;
  
  SCM_VALIDATE_PORT (SCM_ARG1, port);

  if (scm_is_true (scm_output_port_p (port)))
    mode |= SCM_WRTNG;
  else if (scm_is_true (scm_input_port_p (port)))
    mode |=  SCM_RDNG;
  
  result = make_tp (port, mode);

  /* FIXME: We should actually close `port' "in a special way" here,
     according to R6RS.  As there is no way to do that in Guile without
     rendering the underlying port unusable for our purposes as well, we
     just leave it open. */
  
  return result;
}
#undef FUNC_NAME


/* Textual I/O */

SCM_DEFINE (scm_get_string_n_x,
            "get-string-n!", 4, 0, 0,
            (SCM port, SCM str, SCM start, SCM count),
            "Read up to @var{count} characters from @var{port} into "
            "@var{str}, starting at @var{start}.  If no characters "
            "can be read before the end of file is encountered, the end "
            "of file object is returned.  Otherwise, the number of "
            "characters read is returned.")
#define FUNC_NAME s_scm_get_string_n_x
{
  size_t c_start, c_count, c_len, c_end, j;
  scm_t_wchar c;

  SCM_VALIDATE_OPINPORT (1, port);
  SCM_VALIDATE_STRING (2, str);
  c_len = scm_c_string_length (str);
  c_start = scm_to_size_t (start);
  c_count = scm_to_size_t (count);
  c_end = c_start + c_count;

  if (SCM_UNLIKELY (c_end > c_len))
    scm_out_of_range (FUNC_NAME, count);

  for (j = c_start; j < c_end; j++)
    {
      c = scm_getc (port);
      if (c == EOF)
        {
          size_t chars_read = j - c_start;
          return chars_read == 0 ? SCM_EOF_VAL : scm_from_size_t (chars_read);
        }
      scm_c_string_set_x (str, j, SCM_MAKE_CHAR (c));
    }
  return count;
}
#undef FUNC_NAME


/* Initialization.  */

void
scm_register_r6rs_ports (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_r6rs_ports",
			    (scm_t_extension_init_func) scm_init_r6rs_ports,
			    NULL);
}

void
scm_init_r6rs_ports (void)
{
#include "libguile/r6rs-ports.x"

  initialize_bytevector_input_ports ();
  initialize_custom_binary_input_ports ();
  initialize_bytevector_output_ports ();
  initialize_custom_binary_output_ports ();
  initialize_transcoded_ports ();
}
