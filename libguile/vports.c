/*	Copyright (C) 1995,1996,1998,1999, 2000, 2001 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/chars.h"
#include "libguile/fports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/vports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - soft ports}
 * 
 */


static scm_bits_t scm_tc16_sfport;


static void
sf_flush (SCM port)
{
  scm_port_t *pt = SCM_PTAB_ENTRY (port);
  SCM stream = SCM_PACK (pt->stream);

  if (pt->write_pos > pt->write_buf)
    {
      /* write the byte. */
      scm_apply (SCM_VELTS (stream)[0], SCM_MAKE_CHAR (*pt->write_buf),
		 scm_listofnull);
      pt->write_pos = pt->write_buf;
  
      /* flush the output.  */
      {
	SCM f = SCM_VELTS (stream)[2];

	if (!SCM_FALSEP (f))
	  scm_apply (f, SCM_EOL, SCM_EOL);
      }
    }
}

static void
sf_write (SCM port, const void *data, size_t size)
{
  SCM p = SCM_PACK (SCM_STREAM (port));

  scm_apply (SCM_VELTS (p)[1], 
	     scm_cons (scm_makfromstr ((char *) data, size, 0), SCM_EOL),
	     SCM_EOL);
}

/* calling the flush proc (element 2) is in case old code needs it,
   but perhaps softports could the use port buffer in the same way as
   fports.  */

/* places a single char in the input buffer.  */
static int 
sf_fill_input (SCM port)
{
  SCM p = SCM_PACK (SCM_STREAM (port));
  SCM ans;

  ans = scm_apply (SCM_VELTS (p)[3], SCM_EOL, SCM_EOL); /* get char.  */
  if (SCM_FALSEP (ans) || SCM_EOF_OBJECT_P (ans))
    return EOF;
  SCM_ASSERT (SCM_CHARP (ans), ans, SCM_ARG1, "sf_fill_input");
  {
    scm_port_t *pt = SCM_PTAB_ENTRY (port);    

    *pt->read_buf = SCM_CHAR (ans);
    pt->read_pos = pt->read_buf;
    pt->read_end = pt->read_buf + 1;
    return *pt->read_buf;
  }
}


static int 
sf_close (SCM port)
{
  SCM p = SCM_PACK (SCM_STREAM (port));
  SCM f = SCM_VELTS (p)[4];
  if (SCM_FALSEP (f))
    return 0;
  f = scm_apply (f, SCM_EOL, SCM_EOL);
  errno = 0;
  return SCM_FALSEP (f) ? EOF : 0;
}



SCM_DEFINE (scm_make_soft_port, "make-soft-port", 2, 0, 0,
           (SCM pv, SCM modes),
	    "Return a port capable of receiving or delivering characters as\n"
	    "specified by the @var{modes} string (@pxref{File Ports,\n"
	    "open-file}).  @var{pv} must be a vector of length 5.  Its\n"
	    "components are as follows:\n"
	    "\n"
	    "@enumerate 0\n"
	    "@item\n"
	    "procedure accepting one character for output\n"
	    "@item\n"
	    "procedure accepting a string for output\n"
	    "@item\n"
	    "thunk for flushing output\n"
	    "@item\n"
	    "thunk for getting one character\n"
	    "@item\n"
	    "thunk for closing port (not by garbage collection)\n"
	    "@end enumerate\n"
	    "\n"
	    "For an output-only port only elements 0, 1, 2, and 4 need be\n"
	    "procedures.  For an input-only port only elements 3 and 4 need\n"
	    "be procedures.  Thunks 2 and 4 can instead be @code{#f} if\n"
	    "there is no useful operation for them to perform.\n"
	    "\n"
	    "If thunk 3 returns @code{#f} or an @code{eof-object}\n"
	    "(@pxref{Input, eof-object?, ,r5rs, The Revised^5 Report on\n"
	    "Scheme}) it indicates that the port has reached end-of-file.\n"
	    "For example:\n"
	    "\n"
	    "@lisp\n"
	    "(define stdout (current-output-port))\n"
	    "(define p (make-soft-port\n"
	    "           (vector\n"
	    "            (lambda (c) (write c stdout))\n"
	    "            (lambda (s) (display s stdout))\n"
	    "            (lambda () (display \".\" stdout))\n"
	    "            (lambda () (char-upcase (read-char)))\n"
	    "            (lambda () (display \"@@\" stdout)))\n"
	    "           \"rw\"))\n"
	    "\n"
	    "(write p p) @result{} #<input-output: soft 8081e20>\n"
	    "@end lisp")
#define FUNC_NAME s_scm_make_soft_port
{
  scm_port_t *pt;
  SCM z;
  SCM_VALIDATE_VECTOR_LEN (1,pv,5);
  SCM_VALIDATE_STRING (2, modes);
  SCM_STRING_COERCE_0TERMINATION_X (modes);
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  pt = scm_add_to_port_table (z);
  scm_port_non_buffer (pt);
  SCM_SET_CELL_TYPE (z, scm_tc16_sfport | scm_mode_bits (SCM_STRING_CHARS (modes)));
  SCM_SETPTAB_ENTRY (z, pt);
  SCM_SETSTREAM (z, SCM_UNPACK (pv));
  SCM_ALLOW_INTS;
  return z;
}
#undef FUNC_NAME


static scm_bits_t
scm_make_sfptob ()
{
  scm_bits_t tc = scm_make_port_type ("soft", sf_fill_input, sf_write);

  scm_set_port_mark (tc, scm_markstream);
  scm_set_port_flush (tc, sf_flush);
  scm_set_port_close (tc, sf_close);

  return tc;
}

void
scm_init_vports ()
{
  scm_tc16_sfport = scm_make_sfptob ();

#ifndef SCM_MAGIC_SNARFER
#include "libguile/vports.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
