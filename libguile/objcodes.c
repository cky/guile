/* Copyright (C) 2001 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

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

#include "vm-bootstrap.h"
#include "programs.h"
#include "objcodes.h"

/* nb, the length of the header should be a multiple of 8 bytes */
#define OBJCODE_COOKIE "GOOF-0.5"


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
		    SCM_LIST1 (SCM_I_MAKINUM (st.st_size)));

  addr = mmap (0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (addr == MAP_FAILED)
    SCM_SYSERROR;

  if (memcmp (addr, OBJCODE_COOKIE, strlen (OBJCODE_COOKIE)))
    SCM_SYSERROR;

  data = (struct scm_objcode*)(addr + strlen (OBJCODE_COOKIE));

  if (data->len + data->metalen != (st.st_size - sizeof (*data) - strlen (OBJCODE_COOKIE)))
    scm_misc_error (FUNC_NAME, "bad length header (~a, ~a)",
		    SCM_LIST2 (scm_from_size_t (st.st_size),
                               scm_from_uint32 (sizeof (*data) + data->len + data->metalen)));

  SCM_NEWSMOB3 (sret, scm_tc16_objcode, addr + strlen (OBJCODE_COOKIE),
                SCM_PACK (SCM_BOOL_F), fd);
  SCM_SET_SMOB_FLAGS (sret, SCM_F_OBJCODE_IS_MMAP);

  /* FIXME: we leak ourselves and the file descriptor. but then again so does
     dlopen(). */
  return scm_permanent_object (sret);
}
#undef FUNC_NAME

SCM
scm_c_make_objcode_slice (SCM parent, scm_t_uint8 *ptr)
#define FUNC_NAME "make-objcode-slice"
{
  struct scm_objcode *data, *parent_data;
  SCM ret;

  SCM_VALIDATE_OBJCODE (1, parent);
  parent_data = SCM_OBJCODE_DATA (parent);
  
  if (ptr < parent_data->base
      || ptr >= (parent_data->base + parent_data->len + parent_data->metalen
                 - sizeof (struct scm_objcode)))
    scm_misc_error (FUNC_NAME, "offset out of bounds (~a vs ~a + ~a + ~a)",
		    SCM_LIST4 (scm_from_ulong ((ulong)ptr),
                               scm_from_ulong ((ulong)parent_data->base),
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
  assert (increment == 1);
  SCM_ASSERT_RANGE (0, bytecode, size < 1<<31);
  SCM_ASSERT_RANGE (0, bytecode, size >= sizeof(*data));
  SCM_SET_SMOB_FLAGS (objcode, SCM_F_OBJCODE_IS_U8VECTOR);
  
  /* foolishly, we assume that as long as bytecode is around, that c_bytecode
     will be of the same length; perhaps a bad assumption? */
  /* FIXME: check length of bytecode */

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
}

void
scm_init_objcodes (void)
{
  scm_bootstrap_vm ();

#ifndef SCM_MAGIC_SNARFER
#include "objcodes.x"
#endif

  scm_c_define ("word-size", scm_from_size_t (sizeof(SCM)));
  scm_c_define ("byte-order", scm_from_uint16 (__BYTE_ORDER));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
