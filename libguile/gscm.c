/*	Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
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
#include <sys/param.h>
#include "gscm.h"
#include "_scm.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif



extern char *getenv ();


/* {Top Level Evaluation}
 * 
 * Top level evaluation has to establish a dynamic root context,
 * enable Scheme signal handlers, and catch global escapes (errors, quits,
 * aborts, restarts, and execs) from the interpreter.
 */


/* {Printing Objects to Strings} 
 */

#ifdef __STDC__
static GSCM_status
gscm_portprint_obj (SCM port, SCM obj)
#else
static GSCM_status
gscm_portprint_obj (port, obj)
     SCM port;
     SCM obj;
#endif
{
  scm_iprin1 (obj, port, 1);
  return GSCM_OK;
}


struct seval_str_frame
{
  GSCM_status status;
  SCM * answer;
  GSCM_top_level top;
  char * str;
};

#ifdef __STDC__
static void
_seval_str_fn (void * vframe)
#else
static void
_seval_str_fn (vframe)
     void * vframe;
#endif
{
  struct seval_str_frame * frame;
  frame = (struct seval_str_frame *)vframe;
  frame->status = gscm_seval_str (frame->answer, frame->top, frame->str);
}



#ifdef __STDC__
static GSCM_status
gscm_strprint_obj (SCM * answer, SCM obj)
#else
static GSCM_status
gscm_strprint_obj (answer, obj)
     SCM * answer;
     SCM obj;
#endif
{
  SCM str;
  SCM port;
  GSCM_status stat;
  str = scm_makstr (64, 0);
  port = scm_mkstrport (SCM_MAKINUM (0), str, SCM_OPN | SCM_WRTNG, "gscm_strprint_obj");
  stat = gscm_portprint_obj (port, obj);
  if (stat == GSCM_OK)
    *answer = str;
  else
    *answer = SCM_BOOL_F;
  return stat;
}

#ifdef __STDC__
static GSCM_status
gscm_cstr (char ** answer, SCM obj)
#else
static GSCM_status
gscm_cstr (answer, obj)
     char ** answer;
     SCM obj;
#endif
{
  GSCM_status stat;

  *answer = (char *)malloc (SCM_LENGTH (obj));
  stat = GSCM_OK;
  if (!*answer)
    stat = GSCM_OUT_OF_MEM;
  else
    memcpy (*answer, SCM_CHARS (obj), SCM_LENGTH (obj));
  return stat;
}
     

/* {Invoking The Interpreter}
 */

#ifdef __STDC__
static SCM
gscm_silent_repl (SCM env)
#else
static SCM
gscm_silent_repl (env)
     SCM env;
#endif
{
  SCM source;
  SCM answer;
  answer = SCM_UNSPECIFIED;
  while ((source = scm_read (SCM_UNDEFINED, SCM_UNDEFINED, SCM_UNDEFINED)) != SCM_EOF_VAL)
    answer = scm_eval_x (source);
  return answer;
}


#ifdef _UNICOS
typedef int setjmp_type;
#else
typedef long setjmp_type;
#endif

#ifdef __STDC__
static GSCM_status
_eval_port (SCM * answer, GSCM_top_level toplvl, SCM port, int printp)
#else
static GSCM_status
_eval_port (answer, toplvl, port, printp)
     SCM * answer;
     GSCM_top_level toplvl;
     SCM port;
     int printp;
#endif
{
  SCM saved_inp;
  GSCM_status status;
  setjmp_type i;
  static int deja_vu = 0;
  SCM ignored;

  if (deja_vu)
    return GSCM_ILLEGALLY_REENTERED;

  ++deja_vu;
  /* Take over signal handlers for all the interesting signals.
   */
  scm_init_signals ();


  /* Default return values:
   */
  if (!answer)
    answer = &ignored;
  status = GSCM_OK;
  *answer = SCM_BOOL_F;

  /* Perform evalutation under a new dynamic root.
   *
   */
  SCM_BASE (scm_rootcont) = (SCM_STACKITEM *) & i;
#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (scm_rootcont) = last_debug_info_frame = 0;
#endif
  saved_inp = scm_cur_inp;
  i = setjmp (SCM_JMPBUF (scm_rootcont));
#ifdef SCM_STACK_CHECK
  scm_check_stack_p = 1;
#endif
  if (!i)
    {
      scm_gc_heap_lock = 0;
      scm_ints_disabled = 0;
      /* need to close loading files here. */
      scm_cur_inp = port;
      {
	SCM top_env;
	top_env = SCM_EOL;
	*answer = gscm_silent_repl (top_env);
      }
      scm_cur_inp = saved_inp;
      if (printp)
	status = gscm_strprint_obj (answer, *answer);
    }
  else
    {
      scm_cur_inp = saved_inp;
      *answer = scm_exitval;
      if (printp)
	gscm_strprint_obj (answer, *answer);
      status = GSCM_ERROR;
    }

  scm_gc_heap_lock = 1;
  scm_ints_disabled = 1;
  scm_restore_signals ();
  --deja_vu;
  return status;
}

#ifdef __STDC__
static GSCM_status
seval_str (SCM *answer, GSCM_top_level toplvl, char * str)
#else
static GSCM_status
seval_str (answer, toplvl, str)
     SCM *answer;
     GSCM_top_level toplvl;
     char * str;
#endif
{
  SCM scheme_str;
  SCM port;
  GSCM_status status;

  scheme_str = scm_makfromstr (str, strlen (str), 0);
  port = scm_mkstrport (SCM_MAKINUM (0), scheme_str, SCM_OPN | SCM_RDNG, "gscm_seval_str");
  status = _eval_port (answer, toplvl, port, 0);
  return status;
}


#ifdef __STDC__
GSCM_status
gscm_seval_str (SCM *answer, GSCM_top_level toplvl, char * str)
#else
GSCM_status
gscm_seval_str (answer, toplvl, str)
     SCM *answer;
     GSCM_top_level toplvl;
     char * str;
#endif
{
  SCM_STACKITEM i;
  GSCM_status status;
  scm_stack_base = &i;
  status = seval_str (answer, toplvl, str);
  scm_stack_base = 0;
  return status;
}

#ifdef __STDC__
void
format_load_command (char * buf, char *file_name)
#else
void
format_load_command (buf, file_name)
     char * buf;
     char *file_name;
#endif
{
  char quoted_name[MAXPATHLEN + 1];
  int source;
  int dest;

  for (source = dest = 0; file_name[source]; ++source)
    {
      if (file_name[source] == '"')
	quoted_name[dest++] = '\\';
      quoted_name[dest++] = file_name[source];
    }
  quoted_name[dest] = 0;
  sprintf (buf, "(%%try-load \"%s\")", quoted_name);
}

#ifdef __STDC__
GSCM_status
gscm_seval_file (SCM *answer, GSCM_top_level toplvl, char * file_name)
#else
GSCM_status
gscm_seval_file (answer, toplvl, file_name)
     SCM *answer;
     GSCM_top_level toplvl;
     char * file_name;
#endif
{
  char command[MAXPATHLEN * 3];
  format_load_command (command, file_name);
  return gscm_seval_str (answer, toplvl, command);
}


#ifdef __STDC__
static GSCM_status
eval_str (char ** answer, GSCM_top_level toplvl, char * str)
#else
static GSCM_status
eval_str (answer, toplvl, str)
     char ** answer;
     GSCM_top_level toplvl;
     char * str;
#endif
{
  SCM sanswer;
  SCM scheme_str;
  SCM port;
  GSCM_status status;

  scheme_str = scm_makfromstr (str, strlen (str), 0);
  port = scm_mkstrport (SCM_MAKINUM(0), scheme_str, SCM_OPN | SCM_RDNG, "gscm_eval_str");
  status = _eval_port (&sanswer, toplvl, port, 1);
  if (answer)
    {
      if (status == GSCM_OK)
	status = gscm_cstr (answer, sanswer);
      else
	*answer = 0;
    }
  return status;
}


#ifdef __STDC__
GSCM_status
gscm_eval_str (char ** answer, GSCM_top_level toplvl, char * str)
#else
GSCM_status
gscm_eval_str (answer, toplvl, str)
     char ** answer;
     GSCM_top_level toplvl;
     char * str;
#endif
{
  SCM_STACKITEM i;
  GSCM_status status;
  scm_stack_base = &i;
  status = eval_str (answer, toplvl, str);
  scm_stack_base = 0;
  return status;
}


#ifdef __STDC__
GSCM_status
gscm_eval_file (char ** answer, GSCM_top_level toplvl, char * file_name)
#else
GSCM_status
gscm_eval_file (answer, toplvl, file_name)
     char ** answer;
     GSCM_top_level toplvl;
     char * file_name;
#endif
{
  char command[MAXPATHLEN * 3];
  format_load_command (command, file_name);
  return gscm_eval_str (answer, toplvl, command);
}




/* {Error Messages}
 */


#ifdef __GNUC__
# define AT(X)  [X] =
#else
# define AT(X)
#endif 

static char * gscm_error_msgs[] =
{
  AT(GSCM_OK) "No error.",
  AT(GSCM_ERROR) "ERROR in init file.",
  AT(GSCM_ILLEGALLY_REENTERED) "Gscm function was illegally reentered.",
  AT(GSCM_OUT_OF_MEM) "Out of memory.",
  AT(GSCM_ERROR_OPENING_FILE) "Error opening file.",
  AT(GSCM_ERROR_OPENING_INIT_FILE) "Error opening init file."
};

#ifdef __STDC__
char *
gscm_error_msg (int n)
#else
char *
gscm_error_msg (n)
     int n;
#endif
{
  if ((n < 0) || (n > (sizeof (gscm_error_msgs) / sizeof (char *))))
    return "Unrecognized error.";
  else
    return gscm_error_msgs[n];
}



/* {Defining New Procedures}
 */

#ifdef __STDC__
SCM
gscm_make_subr (SCM (*fn)(), int req, int opt, int varp, char * doc)
#else
SCM
gscm_make_subr (fn, req, opt, varp, doc)
     SCM (*fn)();
     int req;
     int opt;
     int varp;
     char * doc;
#endif
{
  return scm_make_gsubr ("*anonymous*", req, opt, varp, fn);
}

#ifdef __STDC__
int
gscm_2_char (SCM c)
#else
int
gscm_2_char (c)
     SCM c;
#endif
{
  SCM_ASSERT (SCM_ICHRP (c), c, SCM_ARG1, "gscm_2_char");
  return SCM_ICHR (c);
}



#ifdef __STDC__
void
gscm_2_str (char ** out, int * len_out, SCM * objp)
#else
void
gscm_2_str (out, len_out, objp)
     char ** out;
     int * len_out;
     SCM * objp;
#endif
{
  SCM_ASSERT (SCM_NIMP (*objp) && SCM_STRINGP (*objp), *objp, SCM_ARG3, "gscm_2_str");
  if (out)
    *out = SCM_CHARS (*objp);
  if (len_out)
    *len_out = SCM_LENGTH (*objp);
}


#ifdef __STDC__
void
gscm_error (char * message, SCM args)
#else
void
gscm_error (message, args)
     char * message;
     SCM args;
#endif
{
  SCM errsym;
  SCM str;

  errsym = SCM_CAR (scm_intern ("error", 5));
  str = scm_makfrom0str (message);
  scm_throw (errsym, scm_cons (str, args));
}


#ifdef __STDC__
GSCM_status
gscm_run_scm (int argc, char ** argv, FILE * in, FILE * out, FILE * err, GSCM_status (*initfn)(), char * initfile, char * initcmd)
#else
GSCM_status
gscm_run_scm (argc, argv, in, out, err, initfn, initfile, initcmd)
     int argc;
     char ** argv;
     FILE * in;
     FILE * out;
     FILE * err;
     GSCM_status (*initfn)();
     char * initfile;
     char * initcmd;
#endif
{
  SCM_STACKITEM i;
  GSCM_status status;
  GSCM_top_level top;

  scm_ports_prehistory ();
  scm_smob_prehistory ();
  scm_tables_prehistory ();
  scm_init_storage (0);
  scm_start_stack (&i, in, out, err);
  scm_init_gsubr ();
  scm_init_curry ();
  scm_init_feature ();
/*  scm_init_debug ();  */
  scm_init_alist ();
  scm_init_append ();
  scm_init_arbiters ();
  scm_init_async ();
  scm_init_boolean ();
  scm_init_chars ();
  scm_init_continuations ();
  scm_init_dynwind ();
  scm_init_eq ();
  scm_init_error ();
  scm_init_fports ();
  scm_init_files ();
  scm_init_gc ();
  scm_init_hash ();
  scm_init_hashtab ();
  scm_init_kw ();
  scm_init_list ();
  scm_init_lvectors ();
  scm_init_numbers ();
  scm_init_pairs ();
  scm_init_ports ();
  scm_init_procs ();
  scm_init_procprop ();
  scm_init_scmsigs ();
  scm_init_stackchk ();
  scm_init_strports ();
  scm_init_struct ();
  scm_init_symbols ();
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
  scm_init_weaks ();
  scm_init_vports ();
  scm_init_eval ();
  scm_init_ramap ();
  scm_init_unif ();
  scm_init_simpos ();
  scm_init_elisp ();
  scm_init_mallocs ();
  scm_init_cnsvobj ();
  scm_init_guile ();
  initfn ();

  /* Save the argument list to be the return value of (program-arguments).
   */
  scm_progargs = scm_makfromstrs (argc, argv);

  scm_gc_heap_lock = 0;
  errno = 0;
  scm_ints_disabled = 1;

/*  init_basic (); */

/*   init_init(); */

  if (initfile == NULL)
    {
      initfile = getenv ("GUILE_INIT_PATH");
      if (initfile == NULL)
	initfile = SCM_IMPLINIT;
    }

  if (initfile == NULL)
    {
      status = GSCM_OK;
    }
  else
    {
      SCM answer;

      status = gscm_seval_file (&answer, -1, initfile);
      if ((status == GSCM_OK) && (answer == SCM_BOOL_F))
	status = GSCM_ERROR_OPENING_INIT_FILE;
    }

  top = SCM_EOL;

  if (status == GSCM_OK)
    {
      scm_sysintern ("*stdin*", scm_cur_inp);
      status = gscm_seval_str (0, top, initcmd);
    }
  return status;
}



#ifdef __STDC__
void
scm_init_guile (void)
#else
void
scm_init_guile ()
#endif
{
#include "gscm.x"
}

