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

#ifndef _SCM_PROGRAMS_H_
#define _SCM_PROGRAMS_H_

#include <libguile.h>

/*
 * Programs
 */

typedef unsigned char scm_byte_t;

struct scm_program {
  size_t size;			/* the size of the program  */
  unsigned char nargs;		/* the number of arguments */
  unsigned char nrest;		/* the number of rest argument (0 or 1) */
  unsigned char nlocs;		/* the number of local variables */
  unsigned char nexts;		/* the number of external variables */
  scm_byte_t *base;		/* program base address */
  SCM meta;			/* meta data */
  SCM objs;			/* constant objects */
  SCM external;			/* external environment */
  SCM holder;			/* the owner of bytecode */
};

extern scm_t_bits scm_tc16_program;

#define SCM_PROGRAM_P(x)	(SCM_SMOB_PREDICATE (scm_tc16_program, x))
#define SCM_PROGRAM_DATA(x)	((struct scm_program *) SCM_SMOB_DATA (x))
#define SCM_VALIDATE_PROGRAM(p,x) SCM_MAKE_VALIDATE (p, x, PROGRAM_P)

extern SCM scm_c_make_program (void *addr, size_t size, SCM holder);
extern SCM scm_c_make_closure (SCM program, SCM external);

extern void scm_bootstrap_programs (void);
extern void scm_init_programs (void);

#endif /* _SCM_PROGRAMS_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
