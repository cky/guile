/* Copyright (C) 2001, 2009 Free Software Foundation, Inc.
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

#include <verify.h>

#include "_scm.h"
#include "vm-bootstrap.h"
#include "programs.h"
#include "objcodes.h"

/* The endianness marker in objcode.  */
#ifdef WORDS_BIGENDIAN
# define OBJCODE_ENDIANNESS "BE"
#else
# define OBJCODE_ENDIANNESS "LE"
#endif

#define _OBJCODE_STRINGIFY(x)  # x
#define OBJCODE_STRINGIFY(x)   _OBJCODE_STRINGIFY (x)

/* The word size marker in objcode.  */
#define OBJCODE_WORD_SIZE  OBJCODE_STRINGIFY (SIZEOF_VOID_P)

/* The objcode magic header.  */
#define OBJCODE_COOKIE						\
  "GOOF-0.6-" OBJCODE_ENDIANNESS "-" OBJCODE_WORD_SIZE "---"

/* The length of the header must be a multiple of 8 bytes.  */
verify (((sizeof (OBJCODE_COOKIE) - 1) & 7) == 0);



/*
 * Objcode type
 */

scm_t_bits scm_tc16_objcode;

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

  if (st.st_size <= sizeof (struct scm_objcode) + strlen (OBJCODE_COOKIE))
    scm_misc_error (FUNC_NAME, "object file too small (~a bytes)",
		    scm_list_1 (SCM_I_MAKINUM (st.st_size)));

  addr = mmap (0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (addr == MAP_FAILED)
    {
      (void) close (fd);
      SCM_SYSERROR;
    }

  if (memcmp (addr, OBJCODE_COOKIE, strlen (OBJCODE_COOKIE)))
    {
      SCM args = scm_list_1 (scm_from_locale_stringn
                             (addr, strlen (OBJCODE_COOKIE)));
      (void) close (fd);
      (void) munmap (addr, st.st_size);
      scm_misc_error (FUNC_NAME, "bad header on object file: ~s", args);
    }

  data = (struct scm_objcode*)(addr + strlen (OBJCODE_COOKIE));

  if (data->len + data->metalen != (st.st_size - sizeof (*data) - strlen (OBJCODE_COOKIE)))
    {
      (void) close (fd);
      (void) munmap (addr, st.st_size);
      scm_misc_error (FUNC_NAME, "bad length header (~a, ~a)",
		      scm_list_2 (scm_from_size_t (st.st_size),
				  scm_from_uint32 (sizeof (*data) + data->len
						   + data->metalen)));
    }

  SCM_NEWSMOB3 (sret, scm_tc16_objcode, addr + strlen (OBJCODE_COOKIE),
                SCM_PACK (SCM_BOOL_F), fd);
  SCM_SET_SMOB_FLAGS (sret, SCM_F_OBJCODE_IS_MMAP);

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
  SCM ret;

  SCM_VALIDATE_OBJCODE (1, parent);
  parent_data = SCM_OBJCODE_DATA (parent);
  
  if (ptr < parent_data->base
      || ptr >= (parent_data->base + parent_data->len + parent_data->metalen
                 - sizeof (struct scm_objcode)))
    scm_misc_error (FUNC_NAME, "offset out of bounds (~a vs ~a + ~a + ~a)",
		    scm_list_4 (scm_from_ulong ((unsigned long)ptr),
				scm_from_ulong ((unsigned long)parent_data->base),
				scm_from_uint32 (parent_data->len),
				scm_from_uint32 (parent_data->metalen)));

  data = (struct scm_objcode*)ptr;
  if (data->base + data->len + data->metalen > parent_data->base + parent_data->len + parent_data->metalen)
    abort ();

  SCM_NEWSMOB2 (ret, scm_tc16_objcode, data, parent);
  SCM_SET_SMOB_FLAGS (ret, SCM_F_OBJCODE_IS_SLICE);
  return ret;
}
#undef FUNC_NAME

static SCM
objcode_mark (SCM obj)
{
  return SCM_SMOB_OBJECT_2 (obj);
}


/*
 * Scheme interface
 */

SCM_DEFINE (scm_objcode_p, "objcode?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_objcode_p
{
  return SCM_BOOL (SCM_OBJCODE_P (obj));
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
  ssize_t increment;
  scm_t_array_handle handle;
  const scm_t_uint8 *c_bytecode;
  struct scm_objcode *data;
  SCM objcode;

  if (scm_is_false (scm_u8vector_p (bytecode)))
    scm_wrong_type_arg (FUNC_NAME, 1, bytecode);

  c_bytecode = scm_u8vector_elements (bytecode, &handle, &size, &increment);
  data = (struct scm_objcode*)c_bytecode;
  SCM_NEWSMOB2 (objcode, scm_tc16_objcode, data, bytecode);
  scm_array_handle_release (&handle);

  SCM_ASSERT_RANGE (0, bytecode, size >= sizeof(struct scm_objcode));
  if (data->len + data->metalen != (size - sizeof (*data)))
    scm_misc_error (FUNC_NAME, "bad u8vector size (~a != ~a)",
		    scm_list_2 (scm_from_size_t (size),
				scm_from_uint32 (sizeof (*data) + data->len + data->metalen)));
  assert (increment == 1);
  SCM_SET_SMOB_FLAGS (objcode, SCM_F_OBJCODE_IS_U8VECTOR);
  
  /* foolishly, we assume that as long as bytecode is around, that c_bytecode
     will be of the same length; perhaps a bad assumption? */

  return objcode;
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
  scm_t_uint8 *u8vector;
  scm_t_uint32 len;

  SCM_VALIDATE_OBJCODE (1, objcode);

  len = sizeof(struct scm_objcode) + SCM_OBJCODE_TOTAL_LEN (objcode);
  /* FIXME:  Is `gc_malloc' ok here? */
  u8vector = scm_gc_malloc (len, "objcode-u8vector");
  memcpy (u8vector, SCM_OBJCODE_DATA (objcode), len);

  return scm_take_u8vector (u8vector, len);
}
#undef FUNC_NAME

SCM_DEFINE (scm_write_objcode, "write-objcode", 2, 0, 0,
	    (SCM objcode, SCM port),
	    "")
#define FUNC_NAME s_scm_write_objcode
{
  SCM_VALIDATE_OBJCODE (1, objcode);
  SCM_VALIDATE_OUTPUT_PORT (2, port);
  
  scm_c_write (port, OBJCODE_COOKIE, strlen (OBJCODE_COOKIE));
  scm_c_write (port, SCM_OBJCODE_DATA (objcode),
               sizeof (struct scm_objcode) + SCM_OBJCODE_TOTAL_LEN (objcode));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void
scm_bootstrap_objcodes (void)
{
  scm_tc16_objcode = scm_make_smob_type ("objcode", 0);
  scm_set_smob_mark (scm_tc16_objcode, objcode_mark);
  scm_c_register_extension ("libguile", "scm_init_objcodes",
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
  scm_bootstrap_vm ();

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
