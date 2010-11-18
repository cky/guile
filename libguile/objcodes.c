/* Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <assert.h>
#include <alignof.h>

#include "_scm.h"
#include "programs.h"
#include "objcodes.h"

/* SCM_OBJCODE_COOKIE, defined in _scm.h, is a magic value prepended
   to objcode on disk but not in memory.

   The length of the header must be a multiple of 8 bytes.  */
verify (((sizeof (SCM_OBJCODE_COOKIE) - 1) & 7) == 0);


/*
 * Objcode type
 */

/* The words in an objcode SCM object are as follows:
     - scm_tc7_objcode | the flags for this objcode
     - the struct scm_objcode C object
     - the parent of this objcode, if this is a slice, or #f if none
     - the file descriptor this objcode came from if this was mmaped,
       or 0 if none
 */

static SCM
make_objcode_by_mmap (int fd)
#define FUNC_NAME "make_objcode_by_mmap"
{
  int ret;
  char *addr;
  struct stat st;
  SCM sret = SCM_BOOL_F;
  struct scm_objcode *data;

  ret = fstat (fd, &st);
  if (ret < 0)
    SCM_SYSERROR;

  if (st.st_size <= sizeof (struct scm_objcode) + strlen (SCM_OBJCODE_COOKIE))
    scm_misc_error (FUNC_NAME, "object file too small (~a bytes)",
		    scm_list_1 (SCM_I_MAKINUM (st.st_size)));

  addr = mmap (0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (addr == MAP_FAILED)
    {
      (void) close (fd);
      SCM_SYSERROR;
    }

  if (memcmp (addr, SCM_OBJCODE_COOKIE, strlen (SCM_OBJCODE_COOKIE)))
    {
      SCM args = scm_list_1 (scm_from_locale_stringn
                             (addr, strlen (SCM_OBJCODE_COOKIE)));
      (void) close (fd);
      (void) munmap (addr, st.st_size);
      scm_misc_error (FUNC_NAME, "bad header on object file: ~s", args);
    }

  data = (struct scm_objcode*)(addr + strlen (SCM_OBJCODE_COOKIE));

  if (data->len + data->metalen != (st.st_size - sizeof (*data) - strlen (SCM_OBJCODE_COOKIE)))
    {
      (void) close (fd);
      (void) munmap (addr, st.st_size);
      scm_misc_error (FUNC_NAME, "bad length header (~a, ~a)",
		      scm_list_2 (scm_from_size_t (st.st_size),
				  scm_from_uint32 (sizeof (*data) + data->len
						   + data->metalen)));
    }

  sret = scm_double_cell (scm_tc7_objcode | (SCM_F_OBJCODE_IS_MMAP<<8),
                          (scm_t_bits)(addr + strlen (SCM_OBJCODE_COOKIE)),
                          SCM_UNPACK (SCM_BOOL_F),
                          (scm_t_bits)fd);

  /* FIXME: we leak ourselves and the file descriptor. but then again so does
     dlopen(). */
  return scm_permanent_object (sret);
}
#undef FUNC_NAME

SCM
scm_c_make_objcode_slice (SCM parent, const scm_t_uint8 *ptr)
#define FUNC_NAME "make-objcode-slice"
{
  const struct scm_objcode *data, *parent_data;
  const scm_t_uint8 *parent_base;

  SCM_VALIDATE_OBJCODE (1, parent);
  parent_data = SCM_OBJCODE_DATA (parent);
  parent_base = SCM_C_OBJCODE_BASE (parent_data);

  if (ptr < parent_base
      || ptr >= (parent_base + parent_data->len + parent_data->metalen
                 - sizeof (struct scm_objcode)))
    scm_misc_error
      (FUNC_NAME, "offset out of bounds (~a vs ~a + ~a + ~a)",
       scm_list_4 (scm_from_unsigned_integer ((scm_t_bits) ptr),
                   scm_from_unsigned_integer ((scm_t_bits) parent_base),
                   scm_from_uint32 (parent_data->len),
                   scm_from_uint32 (parent_data->metalen)));

  /* Make sure bytecode for the objcode-meta is suitable aligned.  Failing to
     do so leads to SIGBUS/SIGSEGV on some arches (e.g., SPARC).  */
  assert ((((scm_t_bits) ptr) &
	   (alignof_type (struct scm_objcode) - 1UL)) == 0);

  data = (struct scm_objcode*) ptr;
  assert (SCM_C_OBJCODE_BASE (data) + data->len + data->metalen
	  <= parent_base + parent_data->len + parent_data->metalen);

  return scm_double_cell (scm_tc7_objcode | (SCM_F_OBJCODE_IS_SLICE<<8),
                          (scm_t_bits)data, SCM_UNPACK (parent), 0);
}
#undef FUNC_NAME


/*
 * Scheme interface
 */

SCM_DEFINE (scm_objcode_p, "objcode?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_objcode_p
{
  return scm_from_bool (SCM_OBJCODE_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_objcode_meta, "objcode-meta", 1, 0, 0,
	    (SCM objcode),
	    "")
#define FUNC_NAME s_scm_objcode_meta
{
  SCM_VALIDATE_OBJCODE (1, objcode);

  if (SCM_OBJCODE_META_LEN (objcode) == 0)
    return SCM_BOOL_F;
  else
    return scm_c_make_objcode_slice (objcode, (SCM_OBJCODE_BASE (objcode)
                                               + SCM_OBJCODE_LEN (objcode)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytecode_to_objcode, "bytecode->objcode", 1, 0, 0,
	    (SCM bytecode),
	    "")
#define FUNC_NAME s_scm_bytecode_to_objcode
{
  size_t size;
  const scm_t_uint8 *c_bytecode;
  struct scm_objcode *data;

  if (!scm_is_bytevector (bytecode))
    scm_wrong_type_arg (FUNC_NAME, 1, bytecode);

  size = SCM_BYTEVECTOR_LENGTH (bytecode);
  c_bytecode = (const scm_t_uint8*)SCM_BYTEVECTOR_CONTENTS (bytecode);
  
  SCM_ASSERT_RANGE (0, bytecode, size >= sizeof(struct scm_objcode));
  data = (struct scm_objcode*)c_bytecode;

  if (data->len + data->metalen != (size - sizeof (*data)))
    scm_misc_error (FUNC_NAME, "bad bytevector size (~a != ~a)",
		    scm_list_2 (scm_from_size_t (size),
				scm_from_uint32 (sizeof (*data) + data->len + data->metalen)));

  /* foolishly, we assume that as long as bytecode is around, that c_bytecode
     will be of the same length; perhaps a bad assumption? */
  return scm_double_cell (scm_tc7_objcode | (SCM_F_OBJCODE_IS_BYTEVECTOR<<8),
                          (scm_t_bits)data, SCM_UNPACK (bytecode), 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_load_objcode, "load-objcode", 1, 0, 0,
	    (SCM file),
	    "")
#define FUNC_NAME s_scm_load_objcode
{
  int fd;
  char *c_file;

  SCM_VALIDATE_STRING (1, file);

  c_file = scm_to_locale_string (file);
  fd = open (c_file, O_RDONLY);
  free (c_file);
  if (fd < 0) SCM_SYSERROR;

  return make_objcode_by_mmap (fd);
}
#undef FUNC_NAME

SCM_DEFINE (scm_objcode_to_bytecode, "objcode->bytecode", 1, 0, 0,
	    (SCM objcode),
	    "")
#define FUNC_NAME s_scm_objcode_to_bytecode
{
  scm_t_int8 *s8vector;
  scm_t_uint32 len;

  SCM_VALIDATE_OBJCODE (1, objcode);

  len = sizeof (struct scm_objcode) + SCM_OBJCODE_TOTAL_LEN (objcode);

  s8vector = scm_malloc (len);
  memcpy (s8vector, SCM_OBJCODE_DATA (objcode), len);

  return scm_c_take_bytevector (s8vector, len);
}
#undef FUNC_NAME

SCM_DEFINE (scm_write_objcode, "write-objcode", 2, 0, 0,
	    (SCM objcode, SCM port),
	    "")
#define FUNC_NAME s_scm_write_objcode
{
  SCM_VALIDATE_OBJCODE (1, objcode);
  SCM_VALIDATE_OUTPUT_PORT (2, port);
  
  scm_c_write (port, SCM_OBJCODE_COOKIE, strlen (SCM_OBJCODE_COOKIE));
  scm_c_write (port, SCM_OBJCODE_DATA (objcode),
               sizeof (struct scm_objcode) + SCM_OBJCODE_TOTAL_LEN (objcode));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_i_objcode_print (SCM objcode, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<objcode ", port);
  scm_uintprint ((scm_t_bits)SCM_OBJCODE_BASE (objcode), 16, port);
  scm_puts (">", port);
}


void
scm_bootstrap_objcodes (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_objcodes",
                            (scm_t_extension_init_func)scm_init_objcodes, NULL);
}

/* Before, we used __BYTE_ORDER, but that is not defined on all
   systems. So punt and use automake, PDP endianness be damned. */
#ifdef WORDS_BIGENDIAN
#define SCM_BYTE_ORDER 4321
#else
#define SCM_BYTE_ORDER 1234
#endif

void
scm_init_objcodes (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/objcodes.x"
#endif

  scm_c_define ("word-size", scm_from_size_t (sizeof(SCM)));
  scm_c_define ("byte-order", scm_from_uint16 (SCM_BYTE_ORDER));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
