/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
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

/* Include the headers for just about everything.
   We call all their initialization functions.  */

#include <stdio.h>
#include "_scm.h"

/* Everybody has an init function.  */
#include "alist.h"
#include "arbiters.h"
#include "async.h"
#include "backtrace.h"
#include "boolean.h"
#include "chars.h"
#include "continuations.h"
#ifdef DEBUG_EXTENSIONS
#include "debug.h"
#endif
#include "dynl.h"
#include "dynwind.h"
#include "eq.h"
#include "error.h"
#include "eval.h"
#include "evalext.h"
#include "feature.h"
#include "filesys.h"
#include "fluids.h"
#include "fports.h"
#include "gc.h"
#include "gdbint.h"
#include "gsubr.h"
#include "hash.h"
#include "hashtab.h"
#ifdef GUILE_ISELECT
#include "iselect.h"
#endif
#include "ioext.h"
#include "keywords.h"
#include "list.h"
#include "load.h"
#include "macros.h"
#include "mallocs.h"
#include "modules.h"
#include "net_db.h"
#include "numbers.h"
#include "objects.h"
#include "objprop.h"
#include "options.h"
#include "pairs.h"
#include "ports.h"
#include "posix.h"
#ifdef HAVE_REGCOMP
#include "regex-posix.h"
#endif
#include "print.h"
#include "procprop.h"
#include "procs.h"
#include "ramap.h"
#include "random.h"
#include "read.h"
#include "scmsigs.h"
#include "script.h"
#include "simpos.h"
#include "smob.h"
#include "socket.h"
#include "sort.h"
#include "srcprop.h"
#include "stackchk.h"
#include "stacks.h"
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
#include "guardians.h"

#include "init.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Setting up the stack.  */

static void start_stack SCM_P ((void *base));
static void restart_stack SCM_P ((void * base));

static void
start_stack (base)
     void * base;
{
  SCM root;

  root = scm_permanent_object (scm_make_root (SCM_UNDEFINED));
  scm_set_root (SCM_ROOT_STATE (root));
  scm_stack_base = base;

  scm_exitval = SCM_BOOL_F;	/* vestigial */

  scm_top_level_lookup_closure_var = SCM_BOOL_F;
  scm_system_transformer = SCM_BOOL_F;

  scm_root->fluids = scm_make_initial_fluids ();

  /* Create an object to hold the root continuation.
   */
  SCM_NEWCELL (scm_rootcont);
  SCM_SETJMPBUF (scm_rootcont, scm_must_malloc ((long) sizeof (scm_contregs),
						"continuation"));
  SCM_SETCAR (scm_rootcont, scm_tc7_contin);
  SCM_SEQ (scm_rootcont) = 0;
  /* The root continuation if further initialized by restart_stack. */

  /* Create the look-aside stack for variables that are shared between
   * captured continuations.
   */
  scm_continuation_stack = scm_make_vector (SCM_MAKINUM (512), SCM_UNDEFINED);
  /* The continuation stack is further initialized by restart_stack. */

  /* The remainder of stack initialization is factored out to another
   * function so that if this stack is ever exitted, it can be
   * re-entered using restart_stack.  */
  restart_stack (base);
}


static void
restart_stack (base)
     void * base;
{
  scm_dynwinds = SCM_EOL;
  SCM_DYNENV (scm_rootcont) = SCM_EOL;
  SCM_THROW_VALUE (scm_rootcont) = SCM_EOL;
#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (scm_rootcont) = scm_last_debug_frame = 0;
#endif
  SCM_BASE (scm_rootcont) = base;
  scm_continuation_stack_ptr = SCM_MAKINUM (0);
}

#if 0
static char remsg[] = "remove\n#define ", addmsg[] = "add\n#define ";


static void fixconfig SCM_P ((char *s1, char *s2, int s));

static void 
fixconfig (s1, s2, s)
     char *s1;
     char *s2;
     int s;
{
  fputs (s1, stderr);
  fputs (s2, stderr);
  fputs ("\nin ", stderr);
  fputs (s ? "setjump" : "scmfig", stderr);
  fputs (".h and recompile scm\n", stderr);
  exit (1);
}


static void check_config SCM_P ((void));

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



/* initializing standard and current I/O ports */

/* Like scm_fdes_to_port, except that:
   - NAME is a standard C string, not a Guile string
   - we set the revealed count for FILE's file descriptor to 1, so
     that fdes won't be closed when the port object is GC'd.  */
static SCM
scm_standard_stream_to_port (int fdes, char *mode, char *name)
{
  SCM port = scm_fdes_to_port (fdes, mode, scm_makfrom0str (name));
  scm_set_port_revealed_x (port, SCM_MAKINUM (1));
  return port;
}

/* Create standard ports from stdin, stdout, and stderr.  */
static void
scm_init_standard_ports ()
{
  /* From the SCSH manual:

     It can be useful to turn I/O buffering off in some cases, for
     example when an I/O stream is to be shared by multiple
     subprocesses.  For this reason, scsh allocates an unbuffered port
     for file descriptor 0 at start-up time.

     Because shells frequently share stdin with subprocesses, if the
     shell does buffered reads, it might ``steal'' input intended for
     a subprocess.  For this reason, all shells, including sh, csh,
     and scsh, read stdin unbuffered.  Applications that can tolerate
     buffered input on stdin can reset \ex{(current-input-port)} to
     block buffering for higher performance.  */

  /* stdout and stderr are also now unbuffered if connected to
     a terminal, since line buffered output is no longer available.  */
  scm_def_inp
    = scm_standard_stream_to_port (0, 
				   isatty (0) ? "r0" : "r",
				   "standard input");
  scm_def_outp = scm_standard_stream_to_port (1,
					      isatty (1) ? "wl" : "w",
					      "standard output");
  scm_def_errp = scm_standard_stream_to_port (2,
					      isatty (2) ? "w0" : "w",
					      "standard error");

  scm_cur_inp = scm_def_inp;
  scm_cur_outp = scm_def_outp;
  scm_cur_errp = scm_def_errp;
  scm_cur_loadp = SCM_BOOL_F;
}



/* Loading the startup Scheme files.  */

/* The boot code "ice-9/boot-9" is only loaded by scm_boot_guile when
   this is false.  The unexec code uses this, to keep ice_9 from being
   loaded into dumped guile executables.  */
int scm_ice_9_already_loaded = 0;

void
scm_load_startup_files ()
{
  /* We want a path only containing directories from GUILE_LOAD_PATH,
     SCM_SITE_DIR and SCM_LIBRARY_DIR when searching for the site init
     file, so we do this before loading Ice-9.  */
  SCM init_path = scm_sys_search_load_path (scm_makfrom0str ("init.scm"));

  /* Load Ice-9.  */
  if (!scm_ice_9_already_loaded)
    {
      scm_primitive_load_path (scm_makfrom0str ("ice-9/boot-9.scm"));

      /* Load the init.scm file.  */
      if (SCM_NFALSEP (init_path))
	scm_primitive_load (init_path);
  
      scm_post_boot_init_modules ();
    }
}



/* The main init code.  */

#ifdef _UNICOS
typedef int setjmp_type;
#else
typedef long setjmp_type;
#endif

/* All the data needed to invoke the main function.  */
struct main_func_closure
{
  /* the function to call */
  void (*main_func) SCM_P ((void *closure, int argc, char **argv));
  void *closure;		/* dummy data to pass it */
  int argc;
  char **argv;			/* the argument list it should receive */
};


static void scm_boot_guile_1 SCM_P ((SCM_STACKITEM *base, 
				     struct main_func_closure *closure));
static SCM invoke_main_func SCM_P ((void *body_data));


/* Fire up the Guile Scheme interpreter.

   Call MAIN_FUNC, passing it CLOSURE, ARGC, and ARGV.  MAIN_FUNC
   should do all the work of the program (initializing other packages,
   reading user input, etc.) before returning.  When MAIN_FUNC
   returns, call exit (0); this function never returns.  If you want
   some other exit value, MAIN_FUNC may call exit itself.

   scm_boot_guile arranges for program-arguments to return the strings
   given by ARGC and ARGV.  If MAIN_FUNC modifies ARGC/ARGV, should
   call scm_set_program_arguments with the final list, so Scheme code
   will know which arguments have been processed.

   scm_boot_guile establishes a catch-all catch handler which prints
   an error message and exits the process.  This means that Guile
   exits in a coherent way when system errors occur and the user isn't
   prepared to handle it.  If the user doesn't like this behavior,
   they can establish their own universal catcher to shadow this one.

   Why must the caller do all the real work from MAIN_FUNC?  The
   garbage collector assumes that all local variables of type SCM will
   be above scm_boot_guile's stack frame on the stack.  If you try to
   manipulate SCM values after this function returns, it's the luck of
   the draw whether the GC will be able to find the objects you
   allocate.  So, scm_boot_guile function exits, rather than
   returning, to discourage people from making that mistake.  */


void
scm_boot_guile (argc, argv, main_func, closure)
     int argc;
     char ** argv;
     void (*main_func) ();
     void *closure;
{
  /* The garbage collector uses the address of this variable as one
     end of the stack, and the address of one of its own local
     variables as the other end.  */
  SCM_STACKITEM dummy;
  struct main_func_closure c;

  c.main_func = main_func;
  c.closure = closure;
  c.argc = argc;
  c.argv = argv;

  scm_boot_guile_1 (&dummy, &c);
}


/* Record here whether SCM_BOOT_GUILE_1 has already been called.  This
   variable is now here and not inside SCM_BOOT_GUILE_1 so that one
   can tweak it. This is necessary for unexec to work. (Hey, "1-live"
   is the name of a local radiostation...) */

int scm_boot_guile_1_live = 0;

static void
scm_boot_guile_1 (base, closure)
     SCM_STACKITEM *base;
     struct main_func_closure *closure;
{
  static int initialized = 0;
  /* static int live = 0; */
  setjmp_type setjmp_val;

  /* This function is not re-entrant. */
  if (scm_boot_guile_1_live)
    abort ();

  scm_boot_guile_1_live = 1;

  scm_ints_disabled = 1;
  scm_block_gc = 1;
  
  if (initialized)
    {
      restart_stack (base);
    }
  else
    {
      scm_ports_prehistory ();
      scm_smob_prehistory ();
      scm_tables_prehistory ();
      scm_init_storage (0);
      scm_init_root ();
#ifdef USE_THREADS
      scm_init_threads (base);
#endif
      start_stack (base);
      scm_init_gsubr ();
      scm_init_feature ();
      scm_init_alist ();
      scm_init_arbiters ();
      scm_init_async ();
      scm_init_boolean ();
      scm_init_chars ();
      scm_init_continuations ();
      scm_init_dynwind ();
      scm_init_eq ();
      scm_init_error ();
      scm_init_fluids ();
      scm_init_backtrace ();	/* Requires fluids */
      scm_init_fports ();
      scm_init_filesys ();
      scm_init_gc ();
      scm_init_gdbint ();
      scm_init_hash ();
      scm_init_hashtab ();
#ifdef GUILE_ISELECT
      scm_init_iselect ();
#endif
      scm_init_ioext ();
      scm_init_keywords ();
      scm_init_list ();
      scm_init_macros ();
      scm_init_mallocs ();
      scm_init_modules ();
      scm_init_net_db ();
      scm_init_numbers ();
      scm_init_objprop ();
      scm_init_options ();
      scm_init_pairs ();
      scm_init_ports ();
      scm_init_posix ();
#ifdef HAVE_REGCOMP
      scm_init_regex_posix ();
#endif
      scm_init_procs ();
      scm_init_procprop ();
      scm_init_scmsigs ();
      scm_init_socket ();
      scm_init_sort ();
#ifdef DEBUG_EXTENSIONS
      scm_init_srcprop ();
#endif
      scm_init_stackchk ();
      scm_init_struct ();	/* Requires struct */
      scm_init_stacks ();
      scm_init_strports ();
      scm_init_symbols ();
      scm_init_tag ();
      scm_init_load ();
      scm_init_objects ();	/* Requires struct */
      scm_init_print ();	/* Requires struct */
      scm_init_read ();
      scm_init_stime ();
      scm_init_strings ();
      scm_init_strorder ();
      scm_init_strop ();
      scm_init_throw ();
      scm_init_variable ();
      scm_init_vectors ();
      scm_init_version ();
      scm_init_weaks ();
      scm_init_guardian ();
      scm_init_vports ();
      scm_init_eval ();
      scm_init_evalext ();
#ifdef DEBUG_EXTENSIONS
      scm_init_debug ();	/* Requires macro smobs */
#endif
      scm_init_ramap ();
      scm_init_random ();
      scm_init_unif ();
      scm_init_simpos ();
      scm_init_load_path ();
      scm_init_standard_ports ();
      scm_init_dynamic_linking ();
      scm_init_script ();
      initialized = 1;
    }

  scm_block_gc = 0;		/* permit the gc to run */
  /* ints still disabled */

#ifdef STACK_CHECKING
  scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
#endif

  setjmp_val = setjmp (SCM_JMPBUF (scm_rootcont));
  if (!setjmp_val)
    {
      scm_set_program_arguments (closure->argc, closure->argv, 0);
      scm_internal_lazy_catch (SCM_BOOL_T, invoke_main_func, closure,
			       scm_handle_by_message, 0);
    }

  scm_restore_signals ();

  /* This tick gives any pending
   * asyncs a chance to run.  This must be done after
   * the call to scm_restore_signals.
   */
  SCM_ASYNC_TICK;

  /* If the caller doesn't want this, they should return from
     main_func themselves.  */
  exit (0);
}


static SCM
invoke_main_func (body_data)
     void *body_data;
{
  struct main_func_closure *closure = (struct main_func_closure *) body_data;

  scm_load_startup_files ();

  (*closure->main_func) (closure->closure, closure->argc, closure->argv);

  /* never reached */
  return SCM_UNDEFINED;
}
