/* GDB interface for Guile
 * Copyright (C) 1996 Mikael Djurfeldt
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
 */

#include <stdio.h>
#include "_scm.h"
#include "tag.h"
#include "strports.h"
#include "read.h"
#include "eval.h"
#include "chars.h"

#include "gdbint.h"

/* {Support for debugging with gdb}
 *
 * TODO:
 *
 * 1. Redirect outputs
 * 2. Catch errors
 * 3. Prevent print from causing segmentation fault when given broken pairs
 */

#include <stdio.h>
#include "_scm.h"

#define GDB_TYPE SCM

#include "gdb_interface.h"



/* Be carefull when this macro is true.
   scm_gc_heap_lock is set during gc.
 */
#define SCM_GC_P (scm_gc_heap_lock)

/* Macros that encapsulate blocks of code which can be called by the
 * debugger.
 */
#define SCM_BEGIN_FOREIGN_BLOCK \
{ \
  old_ints = scm_ints_disabled; scm_ints_disabled = 1; \
  old_gc = scm_block_gc; scm_block_gc = 1; \
  scm_print_carefully_p = 1; \
} \


#define SCM_END_FOREIGN_BLOCK \
{ \
  scm_print_carefully_p = 0; \
  scm_block_gc = old_gc; \
  scm_ints_disabled = old_ints; \
} \


#define RESET_STRING { gdb_output_length = 0; }

#define SEND_STRING(str) \
{ \
  gdb_output = str; \
  gdb_output_length = strlen (str); \
} \


/* {Gdb interface}
 */

unsigned short gdb_options = GDB_HAVE_BINDINGS;

char *gdb_language = "lisp/c";

SCM gdb_result;

char *gdb_output;

int gdb_output_length;

int scm_print_carefully_p;

static SCM gdb_input_port;
static int port_mark_p, stream_mark_p, string_mark_p;

static SCM tok_buf;
static int tok_buf_mark_p;

static SCM gdb_output_port;
static int old_ints, old_gc;


static void unmark_port SCM_P ((SCM port));

static void
unmark_port (port)
     SCM port;
{
  SCM stream, string;
  port_mark_p = SCM_GC8MARKP (port);
  SCM_CLRGC8MARK (port);
  stream = SCM_STREAM (port);
  stream_mark_p = SCM_GCMARKP (stream);
  SCM_CLRGCMARK (stream);
  string = SCM_CDR (stream);
  string_mark_p = SCM_GC8MARKP (string);
  SCM_CLRGC8MARK (string);
}


static void remark_port SCM_P ((SCM port));

static void
remark_port (port)
     SCM port;
{
  SCM stream = SCM_STREAM (port);
  SCM string = SCM_CDR (stream);
  if (string_mark_p) SCM_SETGC8MARK (string);
  if (stream_mark_p) SCM_SETGCMARK (stream);
  if (port_mark_p) SCM_SETGC8MARK (port);
}


int
gdb_maybe_valid_type_p (value)
     SCM value;
{
  if (SCM_IMP (value) || scm_cellp (value))
    return scm_tag (value) != SCM_MAKINUM (-1);
  return 0;
}


int
gdb_read (str)
     char *str;
{
  SCM ans;
  int status = 0;
  RESET_STRING;
  /* Need to be restrictive about what to read? */
  if (SCM_GC_P)
    {
      char *p;
      for (p = str; *p != '\0'; ++p)
	switch (*p)
	  {
	  case '(':
	  case '\'':
	  case '"':
	    SEND_STRING ("Can't read this kind of expressions during gc");
	    return -1;
	  case '#':
	    if (*++p == '\0')
	      goto premature;
	    if (*p == '\\')
	      {
		if (*++p != '\0')
		  continue;
	      premature:
		SEND_STRING ("Premature end of lisp expression");
		return -1;
	      }
	  default:
	    continue;
	  }
    }
  SCM_BEGIN_FOREIGN_BLOCK;
  unmark_port (gdb_input_port);
  /* Replace string in input port and reset stream */
  ans = SCM_CDR (SCM_STREAM (gdb_input_port));
  SCM_SETCHARS (ans, str);
  SCM_SETLENGTH (ans, strlen (str), scm_tc7_string);
  SCM_SETCAR (SCM_STREAM (gdb_input_port), SCM_INUM0);
  /* Read one object */
  tok_buf_mark_p = SCM_GC8MARKP (tok_buf);
  SCM_CLRGC8MARK (tok_buf);
  ans = scm_lreadr (&tok_buf, gdb_input_port, 0, SCM_BOOL_F, &ans);
  if (SCM_GC_P)
    {
      if (SCM_NIMP (ans))
	{
	  SEND_STRING ("Non-immediate created during gc.  Memory may be trashed.");
	  status = -1;
	  goto exit;
	}
    }
  gdb_result = ans;
  /* Protect answer from future GC */
  if (SCM_NIMP (ans))
    scm_permanent_object (ans);
exit:
  if (tok_buf_mark_p)
    SCM_SETGC8MARK (tok_buf);
  remark_port (gdb_input_port);
  SCM_END_FOREIGN_BLOCK;
  return status;
}


int
gdb_eval (exp)
     SCM exp;
{
  RESET_STRING;
  if (SCM_IMP (exp))
    {
      gdb_result = exp;
      return 0;
    }
  if (SCM_GC_P)
    {
      SEND_STRING ("Can't evaluate lisp expressions during gc");
      return -1;
    }
  SCM_BEGIN_FOREIGN_BLOCK;
  {
    SCM env = scm_top_level_env (SCM_CDR (scm_top_level_lookup_closure_var));
    gdb_result = scm_permanent_object (scm_ceval (exp, env));
  }
  SCM_END_FOREIGN_BLOCK;
  return 0;
}


int
gdb_print (obj)
     SCM obj;
{
  RESET_STRING;
  SCM_BEGIN_FOREIGN_BLOCK;
  /* Reset stream */
  SCM_SETCAR (SCM_STREAM (gdb_output_port), SCM_INUM0);
  scm_write (obj, gdb_output_port);
  scm_display (SCM_MAKICHR (0), gdb_output_port);
  SEND_STRING (SCM_CHARS (SCM_CDR (SCM_STREAM (gdb_output_port))));
  SCM_END_FOREIGN_BLOCK;
  return 0;
}


int
gdb_binding (name, value)
     SCM name;
     SCM value;
{
  RESET_STRING;
  if (SCM_GC_P)
    {
      SEND_STRING ("Can't create new bindings during gc");
      return -1;
    }
  SCM_BEGIN_FOREIGN_BLOCK;
  {
    SCM vcell = scm_sym2vcell (name,
			       SCM_CDR (scm_top_level_lookup_closure_var),
			       SCM_BOOL_T);
    SCM_SETCDR (vcell, value);
  }
  SCM_END_FOREIGN_BLOCK;
  return 0;
}

void
scm_init_gdbint ()
{
  static char *s = "scm_init_gdb_interface";
  SCM port;

  scm_print_carefully_p = 0;
  
  port = scm_mkstrport (SCM_INUM0,
			scm_make_string (SCM_MAKINUM (80), SCM_UNDEFINED),
			SCM_OPN | SCM_WRTNG,
			s);
  gdb_output_port = scm_permanent_object (port);
  
  port = scm_mkstrport (SCM_INUM0,
			scm_make_string (SCM_MAKINUM (0), SCM_UNDEFINED),
			SCM_OPN | SCM_RDNG,
			s);
  gdb_input_port = scm_permanent_object (port);

  tok_buf = scm_permanent_object (scm_makstr (30L, 0));
}
