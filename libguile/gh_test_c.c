/*      Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

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


#include <stdio.h>
#include <math.h>
#include <assert.h>

#include <gh.h>

SCM c_factorial (SCM s_n);
SCM c_sin (SCM s_x);
SCM c_vector_test (SCM s_length);

/* the gh_enter() routine, the standard entryp point for the gh_
   interface, makes you use a separate main function */
void 
main_prog (int argc, char *argv[])
{
  int done;
  char input_str[1000];
  SCM cf;
  SCM result_dummy;

  result_dummy = gh_eval_str ("(display \"hello guile\n\")");
  gh_display (result_dummy);

  printf ("\ntesting gh_define\n");
  gh_define ("test_symbol", gh_double2scm (2.5));
  gh_eval_str ("(display test_symbol) (newline)");

  /* test playing with symbols */
  {
    SCM sym;
    char *sym_string;
    sym = gh_symbol2scm ("a-test-symbol");
    sym_string = gh_symbol2newstr (sym, NULL);
    printf ("the symbol was <%s>; after converting to Scheme and back to\n",
	    "a-test-symbol");
    printf ("a C string it is now <%s>\n", sym_string);
    free (sym_string);
  }

  /* here result dummy should be a string object */
  result_dummy = gh_eval_str ("\"test_string\"");
  assert (gh_string_p (result_dummy));
  {
    char *s;
    s = gh_scm2newstr (result_dummy, NULL);
    printf ("result of converting \"test_string\" from SCM to C is <%s>\n", s);
    free (s);			/* remember to free s!! */
  }

  gh_eval_str ("(define (square x) (* x x))");
  gh_eval_str ("(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))");

  gh_eval_str ("(display (square 9)) (newline)");
  gh_eval_str ("(display (fact 100)) (newline)");

  gh_eval_str_with_standard_handler ("(display \"dude!\n\")");

  /* in this next line I have a wilful typo: dosplay is not a defined
     procedure, so it should throw an error */
  gh_eval_str_with_standard_handler ("(dosplay \"dude!\n\")");

  /* now define some new primitives in C */
  cf = gh_new_procedure1_0 ("c_factorial", c_factorial);
  gh_new_procedure1_0 ("c_sin", c_sin);
  gh_new_procedure1_0 ("c_vector_test", c_vector_test);

  /* now try some (eval ...) action from C */
  {
    SCM l = SCM_EOL;
    l = gh_cons (gh_str02scm ("hello world"), l);
    l = gh_cons (gh_symbol2scm ("display"), l);
    printf ("expression is: ");
    gh_display (l);
    gh_newline ();
    /* Don't have a function for evaluating sexps yet.  */
  }

  printf ("testing the predicates for procedure? and vector?\n");
  printf ("gh_procedure_p(c_factorial) is %d, gh_vector_p(c_factorial) is %d\n",
	  gh_procedure_p (cf), gh_vector_p (cf));

  /* Test calling procedures.  */
  {
    SCM list = gh_eval_str ("list");

    printf ("testing gh_apply\n");
    printf ("gh_apply (list, '(1 2)) => ");
    gh_display (gh_apply (list, gh_cons (gh_int2scm (1),
					 gh_cons (gh_int2scm (2), 
						  SCM_EOL))));
    gh_newline ();

    printf ("gh_call0 (list) => ");
    gh_display (gh_call0 (list));
    gh_newline ();

    printf ("gh_call1 (list, 1) => ");
    gh_display (gh_call1 (list, gh_int2scm (1)));
    gh_newline ();

    printf ("gh_call2 (list, 1, 2) => ");
    gh_display (gh_call2 (list, gh_int2scm (1), gh_int2scm (2)));
    gh_newline ();

    printf ("gh_call3 (list, 1, 2, 3) => ");
    gh_display (gh_call3 (list,
			  gh_int2scm (1), gh_int2scm (2), gh_int2scm (3)));
    gh_newline ();
  }

  /* now sit in a scheme eval loop: I input the expressions, have
     guile evaluate them, and then get another expression. */
  done = 0;
  while (!done)
    {
      printf ("\n%s> ", argv[0]);
      if (gets (input_str) == NULL)
	{
	  done = 1;
	}
      else
	{
/*        gh_display(gh_eval_str_with_standard_handler(input_str)); */
	  gh_display (gh_eval_str_with_stack_saving_handler (input_str));
	}
    }
}

int 
main (int argc, char *argv[])
{
  gh_enter (argc, argv, main_prog);
  return 0;
}

SCM 
c_factorial (SCM s_n)
{
  int i, n;
  unsigned long result = 1;

  n = gh_scm2ulong (s_n);

  for (i = 1; i <= n; ++i)
    {
      result = result * i;
    }
  return gh_ulong2scm (result);
}

/* a sin routine in C, callable from scheme.  it is named c_sin() to
   distinguish it from the default scheme sin function */
SCM 
c_sin (SCM s_x)
{
  double x = gh_scm2double (s_x);

  return gh_double2scm (sin (x));
}

/* play around with vectors in guile: this routine creates a vector of
   the given length, initializes it all to zero except element 2 which
   is set to 1.9. */
SCM 
c_vector_test (SCM s_length)
{
  SCM xvec;
  unsigned long c_length;

  c_length = gh_scm2ulong (s_length);
  printf ("requested length for vector: %ld\n", c_length);

  /* create a vector filled witth 0.0 entries */
  xvec = gh_vector (c_length, gh_double2scm (0.0));
  /* set the second element in it to some floating point value */
  gh_vset (xvec, 2, gh_double2scm (1.9));

  return xvec;
}
