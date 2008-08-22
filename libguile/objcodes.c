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

#define OBJCODE_COOKIE "GOOF-0.5"


/*
 * Objcode type
 */

scm_t_bits scm_tc16_objcode;

static SCM
make_objcode (size_t size)
#define FUNC_NAME "make_objcode"
{
  struct scm_objcode *p = scm_gc_malloc (sizeof (struct scm_objcode),
					 "objcode");
  p->size = size;
  p->base = scm_gc_malloc (size, "objcode-base");
  p->fd   = -1;
  SCM_RETURN_NEWSMOB (scm_tc16_objcode, p);
}
#undef FUNC_NAME

static SCM
make_objcode_by_mmap (int fd)
#define FUNC_NAME "make_objcode_by_mmap"
{
  int ret;
  char *addr;
  struct stat st;
  struct scm_objcode *p;

  ret = fstat (fd, &st);
  if (ret < 0)
    SCM_SYSERROR;

  if (st.st_size <= strlen (OBJCODE_COOKIE))
    scm_misc_error (FUNC_NAME, "object file too small (~a bytes)",
		    SCM_LIST1 (SCM_I_MAKINUM (st.st_size)));

  addr = mmap (0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (addr == MAP_FAILED)
    SCM_SYSERROR;

  if (memcmp (addr, OBJCODE_COOKIE, strlen (OBJCODE_COOKIE)))
    SCM_SYSERROR;

  p = scm_gc_malloc (sizeof (struct scm_objcode), "objcode");
  p->size = st.st_size;
  p->base = addr;
  p->fd   = fd;
  SCM_RETURN_NEWSMOB (scm_tc16_objcode, p);
}
#undef FUNC_NAME

static scm_sizet
objcode_free (SCM obj)
#define FUNC_NAME "objcode_free"
{
  size_t size = sizeof (struct scm_objcode);
  struct scm_objcode *p = SCM_OBJCODE_DATA (obj);

  if (p->fd >= 0)
    {
      int rv;
      rv = munmap (p->base, p->size);
      if (rv < 0) SCM_SYSERROR;
      rv = close (p->fd);
      if (rv < 0) SCM_SYSERROR;
    }
  else
    scm_gc_free (p->base, p->size, "objcode-base");

  scm_gc_free (p, size, "objcode");

  return 0;
}
#undef FUNC_NAME


/*
 * Scheme interface
 */

#if 0
SCM_DEFINE (scm_do_pair, "do-pair", 2, 0, 0,
	    (SCM car, SCM cdr),
	    "This is a stupid test to see how cells work.  (Ludo)")
{
  static SCM room[512];
  static SCM *where = &room[0];
  SCM the_pair;
  size_t incr;

  if ((scm_t_bits)where & 6)
    {
      /* Align the cell pointer so that Guile considers it as a
	 non-immediate object (see tags.h).  */
      incr = (scm_t_bits)where & 6;
      incr = (~incr) & 7;
      where += incr;
    }

  printf ("do-pair: pool @ %p, pair @ %p\n", &room[0], where);
  where[0] = car;
  where[1] = cdr;

  the_pair = PTR2SCM (where);
  /* This doesn't work because SCM_SET_GC_MARK will look for some sort of a
     "mark bitmap" at the end of a supposed cell segment which doesn't
     exist.  */

  return (the_pair);
}
#endif

SCM_DEFINE (scm_objcode_p, "objcode?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_objcode_p
{
  return SCM_BOOL (SCM_OBJCODE_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytecode_to_objcode, "bytecode->objcode", 3, 0, 0,
	    (SCM bytecode, SCM nlocs, SCM nexts),
	    "")
#define FUNC_NAME s_scm_bytecode_to_objcode
{
  size_t size;
  ssize_t increment;
  scm_t_array_handle handle;
  char *base;
  const scm_t_uint8 *c_bytecode;
  SCM objcode;

  if (scm_u8vector_p (bytecode) != SCM_BOOL_T)
    scm_wrong_type_arg (FUNC_NAME, 1, bytecode);
  SCM_VALIDATE_NUMBER (2, nlocs);
  SCM_VALIDATE_NUMBER (3, nexts);

  c_bytecode = scm_u8vector_elements (bytecode, &handle, &size, &increment);
  assert (increment == 1);

  /* Account for the 10 byte-long header.  */
  size += 10;
  objcode = make_objcode (size);
  base = SCM_OBJCODE_BASE (objcode);

  memcpy (base, OBJCODE_COOKIE, 8);
  base[8] = scm_to_uint8 (nlocs);
  base[9] = scm_to_uint8 (nexts);

  memcpy (base + 10, c_bytecode, size - 10);

  scm_array_handle_release (&handle);

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

SCM_DEFINE (scm_objcode_to_u8vector, "objcode->u8vector", 1, 0, 0,
	    (SCM objcode),
	    "")
#define FUNC_NAME s_scm_objcode_to_u8vector
{
  scm_t_uint8 *u8vector;
  size_t size;

  SCM_VALIDATE_OBJCODE (1, objcode);

  size = SCM_OBJCODE_SIZE (objcode);
  /* FIXME:  Is `gc_malloc' ok here? */
  u8vector = scm_gc_malloc (size, "objcode-u8vector");
  memcpy (u8vector, SCM_OBJCODE_BASE (objcode), size);

  return scm_take_u8vector (u8vector, size);
}
#undef FUNC_NAME

SCM_DEFINE (scm_objcode_to_program, "objcode->program", 1, 0, 0,
	    (SCM objcode),
	    "")
#define FUNC_NAME s_scm_objcode_to_program
{
  SCM prog;
  size_t size;
  char *base;
  struct scm_program *p;

  SCM_VALIDATE_OBJCODE (1, objcode);

  base = SCM_OBJCODE_BASE (objcode);
  size = SCM_OBJCODE_SIZE (objcode);
  prog = scm_c_make_program (base + 10, size - 10, objcode);
  p = SCM_PROGRAM_DATA (prog);
  p->nlocs = base[8];
  p->nexts = base[9];
  return prog;
}
#undef FUNC_NAME


void
scm_bootstrap_objcodes (void)
{
  scm_tc16_objcode = scm_make_smob_type ("objcode", 0);
  scm_set_smob_free (scm_tc16_objcode, objcode_free);
}

void
scm_init_objcodes (void)
{
  scm_bootstrap_vm ();

#ifndef SCM_MAGIC_SNARFER
#include "objcodes.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
