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


/* gh_test_repl -- a program that demonstrates starting Guile, adding
   some privmitive procedures and entering a REPL form C */

#include <stdio.h>
#include <math.h>

#include <gh.h>

SCM c_factorial (SCM s_n);
SCM c_sin (SCM s_x);
SCM c_vector_test (SCM s_length);

/* the gh_enter() routine, the standard entryp point for the gh_
   interface, makes you use a separate main function */
void 
main_prog (int argc, char *argv[])
{
  SCM cf;

  gh_eval_str ("(display \"hello guile\n\")");

  gh_eval_str ("(define (square x) (* x x))");
  gh_eval_str ("(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))");

  gh_eval_str ("(display (square 9)) (newline)");
  gh_eval_str ("(display (fact 100)) (newline)");

  gh_eval_str ("(define s \"A string\")");
  gh_eval_str ("(define p '(A . pair))");
  gh_eval_str ("(display s)");
  gh_eval_str ("(display p)");
  gh_eval_str ("(display (string? s))");
  gh_eval_str ("(display (pair? s))");

  /* now define some new primitives in C */
  cf = gh_new_procedure1_0 ("c-factorial", c_factorial);
  gh_new_procedure1_0 ("c-sin", c_sin);
  gh_new_procedure1_0 ("c-vector-test", c_vector_test);

  /* now try some (eval ...) action from C */
  {
    SCM l = SCM_EOL;
    l = gh_cons (gh_str02scm ("hello world"), l);
    l = gh_cons (gh_symbol2scm ("'display"), l);
    gh_display (l);
  }

  {
    SCM a_string;
    a_string = gh_str02scm ("A string");

    printf ("testing the predicates for pair? and string?\n");
    printf ("gh_pair_p(a_string) is %d, gh_string_p(a_string) is %d\n",
	    gh_pair_p (a_string), gh_string_p (a_string));
  }

  printf ("testing the predicates for procedure? and vector?\n");
  printf ("gh_procedure_p(c_factorial) is %d, gh_vector_p(c_factorial) is %d\n",
	  gh_procedure_p (cf), gh_vector_p (cf));
  gh_eval_str("(c-vector-test 200)");

  gh_repl ();
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
  printf ("VECTOR test -- requested length for vector: %ld", c_length);

  /* create a vector filled witth 0.0 entries */
  xvec = gh_make_vector (s_length, gh_double2scm (0.0));
  /* set the second element in it to some floating point value */
  gh_vector_set (xvec, gh_int2scm(2), gh_double2scm (1.9));

  /* I think I can use == because Scheme's doubles should be the same
     as C doubles, with no operations in between */
  if (gh_scm2double(gh_vector_ref (xvec, gh_int2scm(2))) == 1.9) {
    printf("... PASS\n");
  } else {
    printf("... FAIL\n");
  }

  return xvec;
}
