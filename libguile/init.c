/* Copyright (C) 1995,1996,1997,1998,1999, 2000, 2001 Free Software Foundation, Inc.
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


/* Include the headers for just about everything.
   We call all their initialization functions.  */

#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "libguile/_scm.h"

/* Everybody has an init function.  */
#include "libguile/alist.h"
#include "libguile/arbiters.h"
#include "libguile/async.h"
#include "libguile/backtrace.h"
#include "libguile/boolean.h"
#include "libguile/chars.h"
#include "libguile/continuations.h"
#ifdef DEBUG_EXTENSIONS
#include "libguile/debug.h"
#endif
#ifdef GUILE_DEBUG_MALLOC
#include "libguile/debug-malloc.h"
#endif
#include "libguile/dynl.h"
#include "libguile/dynwind.h"
#include "libguile/environments.h"
#include "libguile/eq.h"
#include "libguile/error.h"
#include "libguile/eval.h"
#include "libguile/evalext.h"
#include "libguile/feature.h"
#include "libguile/filesys.h"
#include "libguile/fluids.h"
#include "libguile/fports.h"
#include "libguile/gc.h"
#include "libguile/gdbint.h"
#include "libguile/goops.h"
#include "libguile/gsubr.h"
#include "libguile/hash.h"
#include "libguile/hashtab.h"
#include "libguile/hooks.h"
#ifdef GUILE_ISELECT
#include "libguile/iselect.h"
#endif
#include "libguile/ioext.h"
#include "libguile/keywords.h"
#include "libguile/lang.h"
#include "libguile/list.h"
#include "libguile/load.h"
#include "libguile/macros.h"
#include "libguile/mallocs.h"
#include "libguile/modules.h"
#include "libguile/net_db.h"
#include "libguile/numbers.h"
#include "libguile/objects.h"
#include "libguile/objprop.h"
#include "libguile/options.h"
#include "libguile/pairs.h"
#include "libguile/ports.h"
#include "libguile/posix.h"
#ifdef HAVE_REGCOMP
#include "libguile/regex-posix.h"
#endif
#include "libguile/print.h"
#include "libguile/procprop.h"
#include "libguile/procs.h"
#include "libguile/properties.h"
#include "libguile/ramap.h"
#include "libguile/random.h"
#include "libguile/rdelim.h"
#include "libguile/read.h"
#include "libguile/rw.h"
#include "libguile/scmsigs.h"
#include "libguile/script.h"
#include "libguile/simpos.h"
#include "libguile/smob.h"
#include "libguile/socket.h"
#include "libguile/sort.h"
#include "libguile/srcprop.h"
#include "libguile/stackchk.h"
#include "libguile/stacks.h"
#include "libguile/stime.h"
#include "libguile/strings.h"
#include "libguile/strop.h"
#include "libguile/strorder.h"
#include "libguile/strports.h"
#include "libguile/struct.h"
#include "libguile/symbols.h"
#include "libguile/tag.h"
#include "libguile/throw.h"
#include "libguile/unif.h"
#include "libguile/values.h"
#include "libguile/variable.h"
#include "libguile/vectors.h"
#include "libguile/version.h"
#include "libguile/vports.h"
#include "libguile/weaks.h"
#include "libguile/guardians.h"

#include "libguile/init.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Setting up the stack.  */

static void
restart_stack (void *base)
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

static void
start_stack (void *base)
{
  SCM root;

  root = scm_permanent_object (scm_make_root (SCM_UNDEFINED));
  scm_set_root (SCM_ROOT_STATE (root));
  scm_stack_base = base;

  scm_exitval = SCM_BOOL_F;	/* vestigial */

#if SCM_DEBUG_DEPRECATED == 0
  scm_top_level_lookup_closure_var = SCM_BOOL_F;
  scm_system_transformer = SCM_BOOL_F;
#endif

  scm_root->fluids = scm_make_initial_fluids ();

  /* Create an object to hold the root continuation.
   */
  {
    scm_contregs *contregs = scm_must_malloc (sizeof (scm_contregs),
					      "continuation");
    contregs->num_stack_items = 0;
    contregs->seq = 0;
    SCM_NEWSMOB (scm_rootcont, scm_tc16_continuation, contregs);
  }
  /* The root continuation is further initialized by restart_stack. */

  /* Create the look-aside stack for variables that are shared between
   * captured continuations.
   */
  scm_continuation_stack = scm_c_make_vector (512, SCM_UNDEFINED);
  /* The continuation stack is further initialized by restart_stack. */

  /* The remainder of stack initialization is factored out to another
   * function so that if this stack is ever exitted, it can be
   * re-entered using restart_stack.  */
  restart_stack (base);
}


#if 0
static char remsg[] = "remove\n#define ", addmsg[] = "add\n#define ";


static void 
fixconfig (char *s1,char *s2,int s)
{
  fputs (s1, stderr);
  fputs (s2, stderr);
  fputs ("\nin ", stderr);
  fputs (s ? "setjump" : "scmfig", stderr);
  fputs (".h and recompile scm\n", stderr);
  exit (1);
}


static void
check_config (void)
{
  scm_sizet j;

  j = HEAP_SEG_SIZE;
  if (HEAP_SEG_SIZE != j)
    fixconfig ("reduce", "size of HEAP_SEG_SIZE", 0);

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

typedef struct
{
  int fdes;
  char *mode;
  char *name;
} stream_body_data;

/* proc to be called in scope of exception handler stream_handler. */
static SCM
stream_body (void *data)
{
  stream_body_data *body_data = (stream_body_data *) data;
  SCM port = scm_fdes_to_port (body_data->fdes, body_data->mode,
			       scm_makfrom0str (body_data->name));

  SCM_REVEALED (port) = 1;
  return port;
}

/* exception handler for stream_body.  */
static SCM
stream_handler (void *data, SCM tag, SCM throw_args)
{
  return SCM_BOOL_F;
}

/* Convert a file descriptor to a port, using scm_fdes_to_port.
   - NAME is a C string, not a Guile string
   - set the revealed count for FILE's file descriptor to 1, so
   that fdes won't be closed when the port object is GC'd.
   - catch exceptions: allow Guile to be able to start up even
   if it has been handed bogus stdin/stdout/stderr.  replace the
   bad ports with void ports.  */
static SCM
scm_standard_stream_to_port (int fdes, char *mode, char *name)
{
  SCM port;
  stream_body_data body_data;

  body_data.fdes = fdes;
  body_data.mode = mode;
  body_data.name = name;
  port = scm_internal_catch (SCM_BOOL_T, stream_body, &body_data, 
			     stream_handler, NULL);
  if (SCM_FALSEP (port))
    port = scm_void_port (mode);
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

  scm_def_inp
    = scm_standard_stream_to_port (0, 
				   isatty (0) ? "r0" : "r",
				   "standard input");
  scm_def_outp = scm_standard_stream_to_port (1,
					      isatty (1) ? "w0" : "w",
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
  void (*main_func)(void *closure, int argc, char **argv);
  void *closure;		/* dummy data to pass it */
  int argc;
  char **argv;			/* the argument list it should receive */
};


static void scm_init_guile_1 (SCM_STACKITEM *base);
static void scm_boot_guile_1 (SCM_STACKITEM *base,
			      struct main_func_closure *closure);
static SCM invoke_main_func(void *body_data);


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
scm_boot_guile (int argc, char ** argv, void (*main_func) (), void *closure)
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

void
scm_init_guile ()
{
  scm_init_guile_1 ((SCM_STACKITEM *)scm_get_stack_base ());
}

int scm_initialized_p = 0;

static void
scm_init_guile_1 (SCM_STACKITEM *base)
{
  if (scm_initialized_p)
    return;

  if (base == NULL)
    {
      fprintf (stderr, "cannot determine stack base!\n");
      abort ();
    }

  scm_ints_disabled = 1;
  scm_block_gc = 1;
  
  scm_ports_prehistory ();
  scm_smob_prehistory ();
  scm_tables_prehistory ();
#ifdef GUILE_DEBUG_MALLOC
  scm_debug_malloc_prehistory ();
#endif
  scm_init_storage ();
  scm_struct_prehistory ();	  /* requires storage */
  scm_symbols_prehistory ();      /* requires storage */
  scm_weaks_prehistory ();	  /* requires storage */
  scm_init_subr_table ();
  scm_environments_prehistory (); /* requires storage */
  scm_init_continuations ();
  scm_init_root ();		  /* requires continuations */
#ifdef USE_THREADS
  scm_init_threads (base);
#endif
  start_stack (base);
  scm_init_gsubr ();
  scm_init_procprop ();
  scm_init_environments ();
  scm_init_feature ();
  scm_init_alist ();
  scm_init_arbiters ();
  scm_init_async ();
  scm_init_boolean ();
  scm_init_chars ();
#ifdef GUILE_DEBUG_MALLOC
  scm_init_debug_malloc ();
#endif
  scm_init_dynwind ();
  scm_init_eq ();
  scm_init_error ();
  scm_init_fluids ();
  scm_init_backtrace ();	/* Requires fluids */
  scm_init_fports ();
  scm_init_strports ();
  scm_init_gdbint ();           /* Requires strports */
  scm_init_hash ();
  scm_init_hashtab ();
  scm_init_objprop ();
  scm_init_properties ();
  scm_init_hooks ();        /* Requires objprop until hook names are removed */
  scm_init_gc ();		/* Requires hooks, async */
#ifdef GUILE_ISELECT
  scm_init_iselect ();
#endif
  scm_init_ioext ();
  scm_init_keywords ();
  scm_init_list ();
  scm_init_macros ();
  scm_init_mallocs ();
  scm_init_modules ();
  scm_init_numbers ();
  scm_init_options ();
  scm_init_pairs ();
  scm_init_ports ();
#ifdef HAVE_POSIX
  scm_init_filesys ();
  scm_init_posix ();
#endif
#ifdef HAVE_REGCOMP
  scm_init_regex_posix ();
#endif
  scm_init_procs ();
  scm_init_scmsigs ();
#ifdef HAVE_NETWORKING
  scm_init_net_db ();
  scm_init_socket ();
#endif
  scm_init_sort ();
#ifdef DEBUG_EXTENSIONS
  scm_init_srcprop ();
#endif
  scm_init_stackchk ();
  scm_init_struct ();
  scm_init_stacks ();   /* Requires struct */
  scm_init_symbols ();
  scm_init_tag ();
  scm_init_values ();   /* Requires struct */
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
  scm_init_guardians ();
  scm_init_vports ();
  scm_init_eval ();
  scm_init_evalext ();
#ifdef DEBUG_EXTENSIONS
  scm_init_debug ();	/* Requires macro smobs */
#endif
  scm_init_random ();
#ifdef HAVE_ARRAYS
  scm_init_ramap ();
  scm_init_unif ();
#endif
  scm_init_simpos ();
  scm_init_load_path ();
  scm_init_standard_ports ();  /* Requires fports */
  scm_init_dynamic_linking ();
  scm_init_lang ();
  scm_init_script ();

  scm_init_oop_goops_goopscore_module ();
  
  scm_initialized_p = 1;

  scm_block_gc = 0;		/* permit the gc to run */
  /* ints still disabled */

#ifdef STACK_CHECKING
  scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
#endif

  scm_load_startup_files ();

  /* these are located here, not from a deep understanding of the
     module system, but as a way of avoiding segv and other
     undesirable side effects that arise from various alternatives.  */
  scm_init_rdelim ();
  scm_init_rw ();
}

/* Record here whether SCM_BOOT_GUILE_1 has already been called.  This
   variable is now here and not inside SCM_BOOT_GUILE_1 so that one
   can tweak it. This is necessary for unexec to work. (Hey, "1-live"
   is the name of a local radiostation...) */

int scm_boot_guile_1_live = 0;

static void
scm_boot_guile_1 (SCM_STACKITEM *base, struct main_func_closure *closure)
{
  scm_init_guile_1 (base);

  /* This function is not re-entrant. */
  if (scm_boot_guile_1_live)
    abort ();

  scm_boot_guile_1_live = 1;

  scm_set_program_arguments (closure->argc, closure->argv, 0);
  invoke_main_func (closure);

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
invoke_main_func (void *body_data)
{
  struct main_func_closure *closure = (struct main_func_closure *) body_data;

  (*closure->main_func) (closure->closure, closure->argc, closure->argv);

  /* never reached */
  return SCM_UNDEFINED;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
