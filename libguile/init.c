/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 */


#include <stdio.h>
#include "_scm.h"

/* Everybody has an init function.  */
#include "alist.h"
#include "append.h"
#include "arbiters.h"
#include "async.h"
#include "boolean.h"
#include "chars.h"
#include "continuations.h"
#ifdef DEBUG_EXTENSIONS
#include "debug.h"
#endif
#include "dynwind.h"
#include "eq.h"
#include "error.h"
#include "eval.h"
#include "fdsocket.h"
#include "feature.h"
#include "filesys.h"
#include "fports.h"
#include "gc.h"
#include "gdbint.h"
#include "gsubr.h"
#include "hash.h"
#include "hashtab.h"
#include "ioext.h"
#include "kw.h"
#include "list.h"
#include "load.h"
#include "mallocs.h"
#include "mbstrings.h"
#include "numbers.h"
#include "objprop.h"
#include "options.h"
#include "pairs.h"
#include "ports.h"
#include "posix.h"
#include "print.h"
#include "procprop.h"
#include "procs.h"
#include "ramap.h"
#include "read.h"
#include "scmsigs.h"
#include "sequences.h"
#include "simpos.h"
#include "smob.h"
#include "socket.h"
#include "srcprop.h"
#include "stackchk.h"
#include "stime.h"
#include "strings.h"
#include "strop.h"
#include "strorder.h"
#include "strports.h"
#include "struct.h"
#include "symbols.h"
#include "tag.h"
#include "throw.h"
#include "unif.h"
#include "variable.h"
#include "vectors.h"
#include "version.h"
#include "vports.h"
#include "weaks.h"

#include "init.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


void
scm_start_stack (base, in, out, err)
     void * base;
     FILE * in;
     FILE * out;
     FILE * err;
{
  struct scm_port_table * pt;

  scm_stack_base = base;

  /* Create standard ports from stdio files, if requested to do so.
   */

  if (!in)
    {
      scm_def_inp = SCM_BOOL_F;
    }
  else
    {
      SCM_NEWCELL (scm_def_inp);
      pt = scm_add_to_port_table (scm_def_inp);
      SCM_CAR (scm_def_inp) = (scm_tc16_fport | SCM_OPN | SCM_RDNG);
      SCM_SETPTAB_ENTRY (scm_def_inp, pt);
      SCM_SETSTREAM (scm_def_inp, (SCM)in);
      if (isatty (fileno (in)))
	{
	  scm_setbuf0 (scm_def_inp); /* turn off stdin buffering */
	  SCM_CAR (scm_def_inp) |= SCM_BUF0;
	}
      scm_set_port_revealed_x (scm_def_inp, SCM_MAKINUM (1));
    }

  if (!out)
    {
      scm_def_outp = SCM_BOOL_F;
    }
  else
    {
      SCM_NEWCELL (scm_def_outp);
      pt = scm_add_to_port_table (scm_def_outp);
      SCM_CAR (scm_def_outp) = (scm_tc16_fport | SCM_OPN | SCM_WRTNG);
      SCM_SETPTAB_ENTRY (scm_def_outp, pt);
      SCM_SETSTREAM (scm_def_outp, (SCM)out);
      scm_set_port_revealed_x (scm_def_outp, SCM_MAKINUM (1));
    }

  if (!err)
    {
      scm_def_errp = SCM_BOOL_F;
    }
  else
    {
      SCM_NEWCELL (scm_def_errp);
      pt = scm_add_to_port_table (scm_def_errp);
      SCM_CAR (scm_def_errp) = (scm_tc16_fport | SCM_OPN | SCM_WRTNG);
      SCM_SETPTAB_ENTRY (scm_def_errp, pt);
      SCM_SETSTREAM (scm_def_errp, (SCM)err);
      scm_set_port_revealed_x (scm_def_errp, SCM_MAKINUM (1));
    }

  scm_cur_inp = scm_def_inp;
  scm_cur_outp = scm_def_outp;
  scm_cur_errp = scm_def_errp;


  scm_progargs = SCM_BOOL_F;	/* vestigial */
  scm_exitval = SCM_BOOL_F;	/* vestigial */

  scm_top_level_lookup_thunk_var = SCM_BOOL_F;
  scm_system_transformer = SCM_BOOL_F;

  /* Create an object to hold the root continuation.
   */
  SCM_NEWCELL (scm_rootcont);
  SCM_SETJMPBUF (scm_rootcont, scm_must_malloc ((long) sizeof (regs), "continuation"));
  SCM_CAR (scm_rootcont) = scm_tc7_contin;
  /* The root continuation if further initialized by scm_restart_stack. */

  /* Create the look-aside stack for variables that are shared between
   * captured continuations.
   */
  scm_continuation_stack = scm_make_vector (SCM_MAKINUM (512), SCM_UNDEFINED, SCM_UNDEFINED);
  /* The continuation stack is further initialized by scm_restart_stack. */

  /* The remainder of stack initialization is factored out to another function so that
   * if this stack is ever exitted, it can be re-entered using scm_restart_stack.
   */
  scm_restart_stack (base);
}


void
scm_restart_stack (base)
     void * base;
{
  scm_dynwinds = SCM_EOL;
  SCM_DYNENV (scm_rootcont) = SCM_EOL;
  SCM_THROW_VALUE (scm_rootcont) = SCM_EOL;
#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (scm_rootcont) = last_debug_info_frame = 0;
#endif
  SCM_BASE (scm_rootcont) = base;
  scm_continuation_stack_ptr = SCM_MAKINUM (0);
}

#if 0
static char remsg[] = "remove\n#define ", addmsg[] = "add\n#define ";

#ifdef __STDC__
static void 
fixconfig (char *s1, char *s2, int s)
#else
static void 
fixconfig (s1, s2, s)
     char *s1;
     char *s2;
     int s;
#endif
{
  fputs (s1, stderr);
  fputs (s2, stderr);
  fputs ("\nin ", stderr);
  fputs (s ? "setjump" : "scmfig", stderr);
  fputs (".h and recompile scm\n", stderr);
  exit (1);
}



static void
check_config ()
{
  scm_sizet j;

  j = HEAP_SEG_SIZE;
  if (HEAP_SEG_SIZE != j)
    fixconfig ("reduce", "size of HEAP_SEG_SIZE", 0);

#ifdef SCM_SINGLES
  if (sizeof (float) != sizeof (long))
      fixconfig (remsg, "SCM_SINGLES", 0);
#endif /* def SCM_SINGLES */


#ifdef SCM_BIGDIG
  if (2 * SCM_BITSPERDIG / SCM_CHAR_BIT > sizeof (long))
      fixconfig (remsg, "SCM_BIGDIG", 0);
#ifndef SCM_DIGSTOOBIG
  if (SCM_DIGSPERLONG * sizeof (SCM_BIGDIG) > sizeof (long))
      fixconfig (addmsg, "SCM_DIGSTOOBIG", 0);
#endif
#endif

#ifdef SCM_STACK_GROWS_UP
  if (((SCM_STACKITEM *) & j - stack_start_ptr) < 0)
    fixconfig (remsg, "SCM_STACK_GROWS_UP", 1);
#else
  if ((stack_start_ptr - (SCM_STACKITEM *) & j) < 0)
    fixconfig (addmsg, "SCM_STACK_GROWS_UP", 1);
#endif
}
#endif



#ifdef _UNICOS
typedef int setjmp_type;
#else
typedef long setjmp_type;
#endif

/* Fire up Scheme.
 *
 * argc and argv are made the return values of program-arguments.
 *
 * in, out, and err, if not NULL, become the standard ports.
 *	If NULL is passed, your "initfunc" should set up the 
 *      standard ports.
 *
 * boot_cmd is a string containing a Scheme expression to evaluate
 *      to get things rolling.
 *
 * result is returned a string containing a printed result of evaluating
 * 	the boot command.   
 *
 * the return value is:
 *	scm_boot_ok       - evaluation concluded normally
 *	scm_boot_error    - evaluation concluded with a Scheme error
 *	scm_boot_emem     - allocation error mallocing *result
 *	scm_boot_ereenter - scm_boot_guile was called re-entrantly, which is
 *                          prohibited.
 */

int
scm_boot_guile (result, argc, argv, in, out, err, init_func, boot_cmd)
     char ** result;
     int argc;
     char ** argv;
     FILE * in;
     FILE * out;
     FILE * err;
     void (*init_func) ();
     char * boot_cmd;
{
  static int initialized = 0;
  static int live = 0;
  SCM_STACKITEM i;
  setjmp_type setjmp_val;
  int stat;

  if (live)			/* This function is not re-entrant. */
    {
      return scm_boot_ereenter;
    }

  live = 1;

  scm_ints_disabled = 1;
  scm_block_gc = 1;
  
  if (initialized)
    {
      scm_restart_stack (&i);
    }
  else
    {
      scm_ports_prehistory ();
      scm_smob_prehistory ();
      scm_tables_prehistory ();
      scm_init_storage (0);
      scm_start_stack (&i, in, out, err);
      scm_init_gsubr ();
      scm_init_feature ();
      scm_init_alist ();
      scm_init_append ();
      scm_init_arbiters ();
      scm_init_async ();
      scm_init_boolean ();
      scm_init_chars ();
      scm_init_continuations ();
#ifdef DEBUG_EXTENSIONS
      scm_init_debug ();
#endif
      scm_init_dynwind ();
      scm_init_eq ();
      scm_init_error ();
      scm_init_fdsocket ();
      scm_init_fports ();
      scm_init_filesys ();
      scm_init_gc ();
      scm_init_gdbint ();
      scm_init_hash ();
      scm_init_hashtab ();
      scm_init_ioext ();
      scm_init_kw ();
      scm_init_list ();
      scm_init_mallocs ();
      scm_init_numbers ();
      scm_init_objprop ();
#if DEBUG_EXTENSIONS
      /* Excluding this until it's really needed makes the binary
       * smaller after linking.  */
      scm_init_options ();
#endif
      scm_init_pairs ();
      scm_init_ports ();
      scm_init_posix ();
      scm_init_procs ();
      scm_init_procprop ();
      scm_init_scmsigs ();
      scm_init_socket ();
#ifdef DEBUG_EXTENSIONS
      scm_init_srcprop ();
#endif
      scm_init_stackchk ();
      scm_init_strports ();
      scm_init_struct ();
      scm_init_symbols ();
      scm_init_tag ();
      scm_init_load ();
      scm_init_print ();
      scm_init_read ();
      scm_init_sequences ();
      scm_init_stime ();
      scm_init_strings ();
      scm_init_strorder ();
      scm_init_mbstrings ();
      scm_init_strop ();
      scm_init_throw ();
      scm_init_variable ();
      scm_init_vectors ();
      scm_init_version ();
      scm_init_weaks ();
      scm_init_vports ();
      scm_init_eval ();
      scm_init_ramap ();
      scm_init_unif ();
      scm_init_simpos ();
      scm_progargs = scm_makfromstrs (argc, argv);
      scm_init_load_path ();
      initialized = 1;
    }

  scm_block_gc = 0;		/* permit the gc to run */
  /* ints still disabled */

  {
    SCM command;

    command = scm_makfrom0str (boot_cmd);

    setjmp_val = setjmp (SCM_JMPBUF (scm_rootcont));

#ifdef STACK_CHECKING
    scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
#endif
    if (!setjmp_val)
      {
	SCM last = SCM_UNDEFINED;
	scm_init_signals ();

	/* Call the initialization function passed in by the user, if
           present.  */
	if (init_func) (*init_func) ();

	/* Evaluate boot_cmd string.  */
	{
	  SCM p;
	  SCM form;

	  p = scm_mkstrport (SCM_MAKINUM (0),
			     command,
			     SCM_OPN | SCM_RDNG,
			     "boot_guile");
	  while (1)
	    {
	      form = scm_read (p, SCM_BOOL_F, SCM_BOOL_F);
	      if (SCM_EOF_VAL == form)
		break;
	      last = scm_eval_x (form);
	    }

	}

	scm_restore_signals ();
	/* This tick gives any pending
	 * asyncs a chance to run.  This must be done after
	 * the call to scm_restore_signals.
	 */
	SCM_ASYNC_TICK;

	scm_ints_disabled = 1;	/* Hopefully redundant but just to be sure. */

	{
	  SCM str_answer;

	  str_answer = scm_strprint_obj (last);
	  *result = (char *)malloc (1 + SCM_LENGTH (str_answer));
	  if (!*result)
	    stat = scm_boot_emem;
	  else
	    {
	      memcpy (*result, SCM_CHARS (str_answer), SCM_LENGTH (str_answer));
	      (*result)[SCM_LENGTH (str_answer)] = 0;
	      stat = scm_boot_ok;
	    }
	}
      }
    else
      {
	/* This is reached if an unhandled throw terminated Scheme.
	 * Such an occurence should be extremely unlikely -- it indicates
	 * a programming error in the boot code.
	 *
	 * Details of the bogus exception are stored in scm_exitval even 
	 * though that isn't currently reflected in the return value.
	 * !!!
	 */

	scm_restore_signals ();
	/* This tick gives any pending
	 * asyncs a chance to run.  This must be done after
	 * the call to scm_restore_signals.
	 *
	 * Note that an unhandled exception during signal handling
	 * will put as back at the call to scm_restore_signals immediately
	 * preceeding.   A sufficiently bogus signal handler could
	 * conceivably cause an infinite loop here.
	 */
	SCM_ASYNC_TICK;

	scm_ints_disabled = 1;	/* Hopefully redundant but just to be sure. */

	{
	  SCM str_answer;

	  str_answer = scm_strprint_obj (scm_exitval);
	  *result = (char *)malloc (1 + SCM_LENGTH (str_answer));
	  if (!*result)
	    stat = scm_boot_emem;
	  else
	    {
	      memcpy (*result, SCM_CHARS (str_answer), SCM_LENGTH (str_answer));
	      (*result)[SCM_LENGTH (str_answer)] = 0;
	      stat = scm_boot_error;
	    }
	}
      }
  }

  scm_block_gc = 1;
  live = 0;
  return stat;
}
