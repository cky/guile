#include <libguile.h>
#include <stdlib.h>
#include <stdio.h>

void set_flag (void *data);
void func1 (void);
void func2 (void);
void func3 (void);
void func4 (void);
void check_flag1 (const char *msg, void (*func)(void), int val);
SCM check_flag1_body (void *data);
SCM return_tag (void *data, SCM tag, SCM args);
void check_cont (int rewindable);
SCM check_cont_body (void *data);

int flag1, flag2, flag3;

void
set_flag (void *data)
{
  int *f = (int *)data;
  *f = 1;
}

/* FUNC1 should leave flag1 zero.
 */

void
func1 ()
{
  scm_begin_frame (0);
  flag1 = 0;
  scm_on_unwind (set_flag, &flag1, 0);
  scm_end_frame ();
}

/* FUNC2 should set flag1.
 */

void
func2 ()
{
  scm_begin_frame (0);
  flag1 = 0;
  scm_on_unwind (set_flag, &flag1, SCM_F_WIND_EXPLICITELY);
  scm_end_frame ();
}

/* FUNC3 should set flag1.
 */

void
func3 ()
{
  scm_begin_frame (0);
  flag1 = 0;
  scm_on_unwind (set_flag, &flag1, 0);
  scm_misc_error ("func3", "gratuitous error", SCM_EOL);
  scm_end_frame ();
}

/* FUNC4 should set flag1.
 */

void
func4 ()
{
  scm_begin_frame (0);
  flag1 = 0;
  scm_on_unwind (set_flag, &flag1, SCM_F_WIND_EXPLICITELY);
  scm_misc_error ("func4", "gratuitous error", SCM_EOL);
  scm_end_frame ();
}

SCM
check_flag1_body (void *data)
{
  void (*f)(void) = (void (*)(void))data;
  f ();
  return SCM_UNSPECIFIED;
}

SCM
return_tag (void *data, SCM tag, SCM args)
{
  return tag;
}

void
check_flag1 (const char *tag, void (*func)(void), int val)
{
  scm_internal_catch (SCM_BOOL_T,
		      check_flag1_body, func,
		      return_tag, NULL);
  if (flag1 != val)
    {
      printf ("%s failed\n", tag);
      exit (1);
    }
}

SCM
check_cont_body (void *data)
{
  scm_t_frame_flags flags = (data? SCM_F_FRAME_REWINDABLE : 0);
  int first;
  SCM val;

  scm_begin_frame (flags);

  val = scm_make_continuation (&first);
  scm_end_frame ();
  return val;
}

void
check_cont (int rewindable)
{
  SCM res;
  
  res = scm_internal_catch (SCM_BOOL_T,
			    check_cont_body, (void *)rewindable,
			    return_tag, NULL);

  /* RES is now either the created continuation, the value passed to
     the continuation, or a catch-tag, such as 'misc-error.
   */

  if (SCM_NFALSEP (scm_procedure_p (res)))
    {
      /* a continuation, invoke it */
      scm_call_1 (res, SCM_BOOL_F);
    }
  else if (SCM_FALSEP (res))
    {
      /* the result of invoking the continuation, frame must be
	 rewindable */
      if (rewindable)
	return;
      printf ("continuation not blocked\n");
      exit (1);
    }
  else
    {
      /* the catch tag, frame must not have been rewindable. */
      if (!rewindable)
	return;
      printf ("continuation didn't work\n");
      exit (1);
    }
}
  
static void
inner_main (void *data, int argc, char **argv)
{
  check_flag1 ("func1", func1, 0);
  check_flag1 ("func2", func2, 1);
  check_flag1 ("func3", func3, 1);
  check_flag1 ("func4", func4, 1);

  check_cont (0);
  check_cont (1);

  exit (0);
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;
}
