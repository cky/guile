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

#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "programs.h"
#include "objcodes.h"

#define OBJCODE_COOKIE "GOOF-0.5"


/*
 * Objcode type
 */

scm_bits_t scm_tc16_objcode;

static SCM
make_objcode (size_t size)
#define FUNC_NAME "make_objcode"
{
  struct scm_objcode *p = SCM_MUST_MALLOC (sizeof (struct scm_objcode));
  p->size = size;
  p->base = SCM_MUST_MALLOC (size);
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
  if (ret < 0) SCM_SYSERROR;

  addr = mmap (0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (addr == MAP_FAILED) SCM_SYSERROR;

  p = SCM_MUST_MALLOC (sizeof (struct scm_objcode));
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
  size_t size = (sizeof (struct scm_objcode));
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
    {
      size += p->size;
      scm_must_free (p->base);
    }

  scm_must_free (p);
  return size;
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
  return SCM_BOOL (SCM_OBJCODE_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytecode_to_objcode, "bytecode->objcode", 3, 0, 0,
	    (SCM bytecode, SCM nlocs, SCM nexts),
	    "")
#define FUNC_NAME s_scm_bytecode_to_objcode
{
  size_t size;
  char *base;
  SCM objcode;

  SCM_VALIDATE_STRING (1, bytecode);
  SCM_VALIDATE_INUM (2, nlocs);
  SCM_VALIDATE_INUM (3, nexts);

  size = SCM_STRING_LENGTH (bytecode) + 10;
  objcode = make_objcode (size);
  base = SCM_OBJCODE_BASE (objcode);

  memcpy (base, OBJCODE_COOKIE, 8);
  base[8] =  SCM_INUM (nlocs);
  base[9] =  SCM_INUM (nexts);
  memcpy (base + 10, SCM_STRING_CHARS (bytecode), size - 10);
  return objcode;
}
#undef FUNC_NAME

SCM_DEFINE (scm_load_objcode, "load-objcode", 1, 0, 0,
	    (SCM file),
	    "")
#define FUNC_NAME s_scm_load_objcode
{
  int fd;

  SCM_VALIDATE_STRING (1, file);

  fd = open (SCM_STRING_CHARS (file), O_RDONLY);
  if (fd < 0) SCM_SYSERROR;

  return make_objcode_by_mmap (fd);
}
#undef FUNC_NAME

SCM_DEFINE (scm_objcode_to_string, "objcode->string", 1, 0, 0,
	    (SCM objcode),
	    "")
#define FUNC_NAME s_scm_objcode_to_string
{
  SCM_VALIDATE_OBJCODE (1, objcode);
  return scm_makfromstr (SCM_OBJCODE_BASE (objcode),
			 SCM_OBJCODE_SIZE (objcode),
			 0);
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

  SCM_VALIDATE_OBJCODE (1, objcode);

  base = SCM_OBJCODE_BASE (objcode);
  size = SCM_OBJCODE_SIZE (objcode);
  prog = scm_c_make_program (base + 10, size - 10, objcode);
  SCM_PROGRAM_NLOCS (prog) = base[8];
  SCM_PROGRAM_NEXTS (prog) = base[9];
  return prog;
}
#undef FUNC_NAME


void
scm_init_objcodes (void)
{
  scm_tc16_objcode = scm_make_smob_type ("objcode", 0);
  scm_set_smob_free (scm_tc16_objcode, objcode_free);

#ifndef SCM_MAGIC_SNARFER
#include "objcodes.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
