/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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


/* This file is read twice in order to produce debugging versions of
 * scm_ceval and scm_apply.  These functions, scm_deval and
 * scm_dapply, are produced when we define the preprocessor macro
 * DEVAL.  The file is divided into sections which are treated
 * differently with respect to DEVAL.  The heads of these sections are
 * marked with the string "SECTION:".
 */


/* SECTION: This code is compiled once.
 */

#ifndef DEVAL

/* We need this to get the definitions for HAVE_ALLOCA_H, etc.  */
#include "scmconfig.h"

/* AIX requires this to be the first thing in the file.  The #pragma
   directive is indented so pre-ANSI compilers will ignore it, rather
   than choke on it.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include <stdio.h>
#include "_scm.h"
#include "debug.h"
#include "alist.h"
#include "eq.h"
#include "continuations.h"
#include "throw.h"
#include "smob.h"
#include "markers.h"
#include "procprop.h"
#include "hashtab.h"
#include "hash.h"
#include "srcprop.h"
#include "stackchk.h"
#include "objects.h"
#include "feature.h"

#include "eval.h"


/* The evaluator contains a plethora of EVAL symbols.
 * This is an attempt at explanation.
 *
 * The following macros should be used in code which is read twice
 * (where the choice of evaluator is hard soldered):
 *
 *   SCM_CEVAL is the symbol used within one evaluator to call itself.
 *   Originally, it is defined to scm_ceval, but is redefined to
 *   scm_deval during the second pass.
 *  
 *   SIDEVAL corresponds to SCM_CEVAL, but is used in situations where
 *   only side effects of expressions matter.  All immediates are
 *   ignored.
 *  
 *   EVALIM is used when it is known that the expression is an
 *   immediate.  (This macro never calls an evaluator.)
 *  
 *   EVALCAR evaluates the car of an expression.
 *  
 *   EVALCELLCAR is like EVALCAR, but is used when it is known that the
 *   car is a lisp cell.
 *
 * The following macros should be used in code which is read once
 * (where the choice of evaluator is dynamic):
 *
 *   XEVAL takes care of immediates without calling an evaluator.  It
 *   then calls scm_ceval *or* scm_deval, depending on the debugging
 *   mode.
 *  
 *   XEVALCAR corresponds to EVALCAR, but uses scm_ceval *or* scm_deval
 *   depending on the debugging mode.
 *
 * The main motivation for keeping this plethora is efficiency
 * together with maintainability (=> locality of code).
 */

#define EVALCELLCAR(x, env) (SCM_SYMBOLP (SCM_CAR(x)) \
			     ? *scm_lookupcar(x, env) \
			     : SCM_CEVAL(SCM_CAR(x), env))

#ifdef MEMOIZE_LOCALS
#define EVALIM(x, env) (SCM_ILOCP(x)?*scm_ilookup((x), env):x)
#else
#define EVALIM(x, env) x
#endif
#define EVALCAR(x, env) (SCM_NCELLP(SCM_CAR(x))\
			? (SCM_IMP(SCM_CAR(x)) \
			   ? EVALIM(SCM_CAR(x), env) \
			   : SCM_GLOC_VAL(SCM_CAR(x))) \
			: EVALCELLCAR(x, env))
#ifdef DEBUG_EXTENSIONS
#define XEVALCAR(x, env) (SCM_NCELLP(SCM_CAR(x)) \
			  ? (SCM_IMP(SCM_CAR(x)) \
			     ? EVALIM(SCM_CAR(x), env) \
			     : SCM_GLOC_VAL(SCM_CAR(x))) \
			  : (SCM_SYMBOLP(SCM_CAR(x)) \
			     ? *scm_lookupcar(x, env) \
			     : (*scm_ceval_ptr) (SCM_CAR(x), env)))
#else
#define XEVALCAR(x, env) EVALCAR(x, env)
#endif

#define EXTEND_ENV SCM_EXTEND_ENV

#ifdef MEMOIZE_LOCALS

SCM *
scm_ilookup (iloc, env)
     SCM iloc;
     SCM env;
{
  register int ir = SCM_IFRAME (iloc);
  register SCM er = env;
  for (; 0 != ir; --ir)
    er = SCM_CDR (er);
  er = SCM_CAR (er);
  for (ir = SCM_IDIST (iloc); 0 != ir; --ir)
    er = SCM_CDR (er);
  if (SCM_ICDRP (iloc))
    return SCM_CDRLOC (er);
  return SCM_CARLOC (SCM_CDR (er));
}
#endif

#ifdef USE_THREADS

/* The Lookup Car Race
    - by Eva Luator

   Memoization of variables and special forms is done while executing
   the code for the first time.  As long as there is only one thread
   everything is fine, but as soon as two threads execute the same
   code concurrently `for the first time' they can come into conflict.

   This memoization includes rewriting variable references into more
   efficient forms and expanding macros.  Furthermore, macro expansion
   includes `compiling' special forms like `let', `cond', etc. into
   tree-code instructions.

   There shouldn't normally be a problem with memoizing local and
   global variable references (into ilocs and glocs), because all
   threads will mutate the code in *exactly* the same way and (if I
   read the C code correctly) it is not possible to observe a half-way
   mutated cons cell.  The lookup procedure can handle this
   transparently without any critical sections.

   It is different with macro expansion, because macro expansion
   happens outside of the lookup procedure and can't be
   undone. Therefore it can't cope with it.  It has to indicate
   failure when it detects a lost race and hope that the caller can
   handle it.  Luckily, it turns out that this is the case.

   An example to illustrate this: Suppose that the follwing form will
   be memoized concurrently by two threads

       (let ((x 12)) x)

   Let's first examine the lookup of X in the body.  The first thread
   decides that it has to find the symbol "x" in the environment and
   starts to scan it.  Then the other thread takes over and actually
   overtakes the first.  It looks up "x" and substitutes an
   appropriate iloc for it.  Now the first thread continues and
   completes its lookup.  It comes to exactly the same conclusions as
   the second one and could - without much ado - just overwrite the
   iloc with the same iloc.

   But let's see what will happen when the race occurs while looking
   up the symbol "let" at the start of the form.  It could happen that
   the second thread interrupts the lookup of the first thread and not
   only substitutes a gloc for it but goes right ahead and replaces it
   with the compiled form (#@let* (x 12) x).  Now, when the first
   thread completes its lookup, it would replace the #@let* with a
   gloc pointing to the "let" binding, effectively reverting the form
   to (let (x 12) x).  This is wrong.  It has to detect that it has
   lost the race and the evaluator has to reconsider the changed form
   completely.

   This race condition could be resolved with some kind of traffic
   light (like mutexes) around scm_lookupcar, but I think that it is
   best to avoid them in this case.  They would serialize memoization
   completely and because lookup involves calling arbitrary Scheme
   code (via the lookup-thunk), threads could be blocked for an
   arbitrary amount of time or even deadlock.  But with the current
   solution a lot of unnecessary work is potentially done. */

/* SCM_LOOKUPCAR1 is was SCM_LOOKUPCAR used to be but is allowed to
   return NULL to indicate a failed lookup due to some race conditions
   between threads.  This only happens when VLOC is the first cell of
   a special form that will eventually be memoized (like `let', etc.)
   In that case the whole lookup is bogus and the caller has to
   reconsider the complete special form.

   SCM_LOOKUPCAR is still there, of course.  It just calls
   SCM_LOOKUPCAR1 and aborts on recieving NULL.  So SCM_LOOKUPCAR
   should only be called when it is known that VLOC is not the first
   pair of a special form.  Otherwise, use SCM_LOOKUPCAR1 and check
   for NULL.  I think I've found the only place where this applies. */

#endif /* USE_THREADS */

static SCM *
scm_lookupcar1 (SCM vloc, SCM genv)
{
  SCM env = genv;
  register SCM *al, fl, var = SCM_CAR (vloc);
#ifdef USE_THREADS
  register SCM var2 = var;
#endif
#ifdef MEMOIZE_LOCALS
  register SCM iloc = SCM_ILOC00;
#endif
  for (; SCM_NIMP (env); env = SCM_CDR (env))
    {
      if (SCM_BOOL_T == scm_procedure_p (SCM_CAR (env)))
	break;
      al = SCM_CARLOC (env);
      for (fl = SCM_CAR (*al); SCM_NIMP (fl); fl = SCM_CDR (fl))
	{
	  if (SCM_NCONSP (fl))
	    {
	      if (fl == var)
	      {
#ifdef MEMOIZE_LOCALS
#ifdef USE_THREADS
		if (SCM_CAR (vloc) != var)
		  goto race;
#endif
		SCM_SETCAR (vloc, iloc + SCM_ICDR);
#endif
		return SCM_CDRLOC (*al);
	      }
	      else
		break;
	    }
	  al = SCM_CDRLOC (*al);
	  if (SCM_CAR (fl) == var)
	    {
#ifdef MEMOIZE_LOCALS
#ifndef RECKLESS		/* letrec inits to SCM_UNDEFINED */
	      if (SCM_UNBNDP (SCM_CAR (*al)))
		{
		  env = SCM_EOL;
		  goto errout;
		}
#endif
#ifdef USE_THREADS
	      if (SCM_CAR (vloc) != var)
		goto race;
#endif
	      SCM_SETCAR (vloc, iloc);
#endif
	      return SCM_CARLOC (*al);
	    }
#ifdef MEMOIZE_LOCALS
	  iloc += SCM_IDINC;
#endif
	}
#ifdef MEMOIZE_LOCALS
      iloc = (~SCM_IDSTMSK) & (iloc + SCM_IFRINC);
#endif
    }
  {
    SCM top_thunk, vcell;
    if (SCM_NIMP(env))
      {
	top_thunk = SCM_CAR(env);	/* env now refers to a top level env thunk */
	env = SCM_CDR (env);
      }
    else
      top_thunk = SCM_BOOL_F;
    vcell = scm_sym2vcell (var, top_thunk, SCM_BOOL_F);
    if (vcell == SCM_BOOL_F)
      goto errout;
    else
      var = vcell;
  }
#ifndef RECKLESS
  if (SCM_NNULLP (env) || SCM_UNBNDP (SCM_CDR (var)))
    {
      var = SCM_CAR (var);
    errout:
      /* scm_everr (vloc, genv,...) */
      scm_misc_error (NULL,
		      SCM_NULLP (env)
		      ? "Unbound variable: %S"
		      : "Damaged environment: %S",
		      scm_listify (var, SCM_UNDEFINED));
    }
#endif
#ifdef USE_THREADS
  if (SCM_CAR (vloc) != var2)
    {
      /* Some other thread has changed the very cell we are working
         on.  In effect, it must have done our job or messed it up
         completely. */
    race:
      var = SCM_CAR (vloc);
      if ((var & 7) == 1)
	return SCM_GLOC_VAL_LOC (var);
#ifdef MEMOIZE_LOCALS
      if ((var & 127) == (127 & SCM_ILOC00))
	return scm_ilookup (var, genv);
#endif
      /* We can't cope with anything else than glocs and ilocs.  When
         a special form has been memoized (i.e. `let' into `#@let') we
         return NULL and expect the calling function to do the right
         thing.  For the evaluator, this means going back and redoing
         the dispatch on the car of the form. */
      return NULL;
    }
#endif /* USE_THREADS */

  SCM_SETCAR (vloc, var + 1);
  /* Except wait...what if the var is not a vcell,
   * but syntax or something....  */
  return SCM_CDRLOC (var);
}

#ifdef USE_THREADS
SCM *
scm_lookupcar (vloc, genv)
     SCM vloc;
     SCM genv;
{
  SCM *loc = scm_lookupcar1 (vloc, genv);
  if (loc == NULL)
    abort ();
  return loc;
}
#else /* not USE_THREADS */
#define scm_lookupcar scm_lookupcar1
#endif

#define unmemocar scm_unmemocar

SCM 
scm_unmemocar (form, env)
     SCM form;
     SCM env;
{
#ifdef DEBUG_EXTENSIONS
  register int ir;
#endif
  SCM c;

  if (SCM_IMP (form))
    return form;
  c = SCM_CAR (form);
  if (1 == (c & 7))
    SCM_SETCAR (form, SCM_CAR (c - 1));
#ifdef MEMOIZE_LOCALS
#ifdef DEBUG_EXTENSIONS
  else if (SCM_ILOCP (c))
    {
      for (ir = SCM_IFRAME (c); ir != 0; --ir)
	env = SCM_CDR (env);
      env = SCM_CAR (SCM_CAR (env));
      for (ir = SCM_IDIST (c); ir != 0; --ir)
	env = SCM_CDR (env);
      SCM_SETCAR (form, SCM_ICDRP (c) ? env : SCM_CAR (env));
    }
#endif
#endif
  return form;
}


SCM
scm_eval_car (pair, env)
     SCM pair;
     SCM env;
{
  return XEVALCAR (pair, env);
}


/* 
 * The following rewrite expressions and
 * some memoized forms have different syntax 
 */

static char s_expression[] = "missing or extra expression";
static char s_test[] = "bad test";
static char s_body[] = "bad body";
static char s_bindings[] = "bad bindings";
static char s_variable[] = "bad variable";
static char s_clauses[] = "bad or missing clauses";
static char s_formals[] = "bad formals";
#define ASSYNT(_cond, _arg, _pos, _subr) if(!(_cond))scm_wta(_arg, (char *)_pos, _subr);

SCM scm_i_dot, scm_i_quote, scm_i_quasiquote, scm_i_lambda, scm_i_let,
  scm_i_arrow, scm_i_else, scm_i_unquote, scm_i_uq_splicing, scm_i_apply;
SCM scm_i_define, scm_i_and, scm_i_begin, scm_i_case, scm_i_cond,
  scm_i_do, scm_i_if, scm_i_let, scm_i_letrec, scm_i_letstar,
  scm_i_or, scm_i_set, scm_i_atapply, scm_i_atcall_cc;
static char s_quasiquote[] = "quasiquote";
static char s_delay[] = "delay";
static char s_undefine[] = "undefine";
#ifdef DEBUG_EXTENSIONS
SCM scm_i_enter_frame, scm_i_apply_frame, scm_i_exit_frame;
SCM scm_i_trace;
#endif

#define ASRTSYNTAX(cond_, msg_) if(!(cond_))scm_wta(xorig, (msg_), what);



static void  bodycheck SCM_P ((SCM xorig, SCM *bodyloc, char *what));

static void 
bodycheck (xorig, bodyloc, what)
     SCM xorig;
     SCM *bodyloc;
     char *what;
{
  ASRTSYNTAX (scm_ilength (*bodyloc) >= 1, s_expression);
}



SCM 
scm_m_quote (xorig, env)
     SCM xorig;
     SCM env;
{
  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, xorig, s_expression, "quote");
  return scm_cons (SCM_IM_QUOTE, SCM_CDR (xorig));
}



SCM 
scm_m_begin (xorig, env)
     SCM xorig;
     SCM env;
{
  ASSYNT (scm_ilength (SCM_CDR (xorig)) >= 1, xorig, s_expression, "begin");
  return scm_cons (SCM_IM_BEGIN, SCM_CDR (xorig));
}



SCM 
scm_m_if (xorig, env)
     SCM xorig;
     SCM env;
{
  int len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 2 && len <= 3, xorig, s_expression, "if");
  return scm_cons (SCM_IM_IF, SCM_CDR (xorig));
}



SCM 
scm_m_set (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM x = SCM_CDR (xorig);
  ASSYNT (2 == scm_ilength (x), xorig, s_expression, "set!");
  ASSYNT (SCM_NIMP (SCM_CAR (x)) && SCM_SYMBOLP (SCM_CAR (x)),
	  xorig, s_variable, "set!");
  return scm_cons (SCM_IM_SET, x);
}


#if 0

SCM 
scm_m_vref (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM x = SCM_CDR (xorig);
  ASSYNT (1 == scm_ilength (x), xorig, s_expression, s_vref);
  if (SCM_NIMP(x) && UDSCM_VARIABLEP (SCM_CAR (x)))
    {
      /* scm_everr (SCM_UNDEFINED, env,..., "global variable reference") */
      scm_misc_error (NULL,
		      "Bad variable: %S",
		      scm_listify (SCM_CAR (SCM_CDR (x)), SCM_UNDEFINED));
    }
  ASSYNT (SCM_NIMP(x) && DEFSCM_VARIABLEP (SCM_CAR (x)),
	  xorig, s_variable, s_vref);
  return 
  return scm_cons (IM_VREF, x);
}



SCM 
scm_m_vset (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM x = SCM_CDR (xorig);
  ASSYNT (3 == scm_ilength (x), xorig, s_expression, s_vset);
  ASSYNT ((   DEFSCM_VARIABLEP (SCM_CAR (x))
	   || UDSCM_VARIABLEP (SCM_CAR (x))),
	  xorig, s_variable, s_vset);
  return scm_cons (IM_VSET, x);
}
#endif 



SCM 
scm_m_and (xorig, env)
     SCM xorig;
     SCM env;
{
  int len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 0, xorig, s_test, "and");
  if (len >= 1)
    return scm_cons (SCM_IM_AND, SCM_CDR (xorig));
  else
    return SCM_BOOL_T;
}



SCM 
scm_m_or (xorig, env)
     SCM xorig;
     SCM env;
{
  int len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 0, xorig, s_test, "or");
  if (len >= 1)
    return scm_cons (SCM_IM_OR, SCM_CDR (xorig));
  else
    return SCM_BOOL_F;
}



SCM 
scm_m_case (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM proc, x = SCM_CDR (xorig);
  ASSYNT (scm_ilength (x) >= 2, xorig, s_clauses, "case");
  while (SCM_NIMP (x = SCM_CDR (x)))
    {
      proc = SCM_CAR (x);
      ASSYNT (scm_ilength (proc) >= 2, xorig, s_clauses, "case");
      ASSYNT (scm_ilength (SCM_CAR (proc)) >= 0 || scm_i_else == SCM_CAR (proc),
	      xorig, s_clauses, "case");
    }
  return scm_cons (SCM_IM_CASE, SCM_CDR (xorig));
}



SCM 
scm_m_cond (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM arg1, x = SCM_CDR (xorig);
  int len = scm_ilength (x);
  ASSYNT (len >= 1, xorig, s_clauses, "cond");
  while (SCM_NIMP (x))
    {
      arg1 = SCM_CAR (x);
      len = scm_ilength (arg1);
      ASSYNT (len >= 1, xorig, s_clauses, "cond");
      if (scm_i_else == SCM_CAR (arg1))
	{
	  ASSYNT (SCM_NULLP (SCM_CDR (x)) && len >= 2, xorig, "bad ELSE clause", "cond");
	  SCM_SETCAR (arg1, SCM_BOOL_T);
	}
      if (len >= 2 && scm_i_arrow == SCM_CAR (SCM_CDR (arg1)))
	ASSYNT (3 == len && SCM_NIMP (SCM_CAR (SCM_CDR (SCM_CDR (arg1)))),
		xorig, "bad recipient", "cond");
      x = SCM_CDR (x);
    }
  return scm_cons (SCM_IM_COND, SCM_CDR (xorig));
}



SCM 
scm_m_lambda (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM proc, x = SCM_CDR (xorig);
  if (scm_ilength (x) < 2)
    goto badforms;
  proc = SCM_CAR (x);
  if (SCM_NULLP (proc))
    goto memlambda;
  if (SCM_IMP (proc))
    goto badforms;
  if (SCM_SYMBOLP (proc))
    goto memlambda;
  if (SCM_NCONSP (proc))
    goto badforms;
  while (SCM_NIMP (proc))
    {
      if (SCM_NCONSP (proc))
	{
	  if (!SCM_SYMBOLP (proc))
	    goto badforms;
	  else
	    goto memlambda;
	}
      if (!(SCM_NIMP (SCM_CAR (proc)) && SCM_SYMBOLP (SCM_CAR (proc))))
	goto badforms;
      proc = SCM_CDR (proc);
    }
  if SCM_NNULLP
    (proc)
  badforms:scm_wta (xorig, s_formals, "lambda");
memlambda:
  bodycheck (xorig, SCM_CDRLOC (x), "lambda");
  return scm_cons (SCM_IM_LAMBDA, SCM_CDR (xorig));
}



SCM 
scm_m_letstar (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM x = SCM_CDR (xorig), arg1, proc, vars = SCM_EOL, *varloc = &vars;
  int len = scm_ilength (x);
  ASSYNT (len >= 2, xorig, s_body, "let*");
  proc = SCM_CAR (x);
  ASSYNT (scm_ilength (proc) >= 0, xorig, s_bindings, "let*");
  while SCM_NIMP (proc)
    {
      arg1 = SCM_CAR (proc);
      ASSYNT (2 == scm_ilength (arg1), xorig, s_bindings, "let*");
      ASSYNT (SCM_NIMP (SCM_CAR (arg1)) && SCM_SYMBOLP (SCM_CAR (arg1)), xorig, s_variable, "let*");
      *varloc = scm_cons2 (SCM_CAR (arg1), SCM_CAR (SCM_CDR (arg1)), SCM_EOL);
      varloc = SCM_CDRLOC (SCM_CDR (*varloc));
      proc = SCM_CDR (proc);
    }
  x = scm_cons (vars, SCM_CDR (x));
  bodycheck (xorig, SCM_CDRLOC (x), "let*");
  return scm_cons (SCM_IM_LETSTAR, x);
}

/* DO gets the most radically altered syntax
   (do ((<var1> <init1> <step1>)
   (<var2> <init2>)
   ... )
   (<test> <return>)
   <body>)
   ;; becomes
   (do_mem (varn ... var2 var1)
   (<init1> <init2> ... <initn>)
   (<test> <return>)
   (<body>)
   <step1> <step2> ... <stepn>) ;; missing steps replaced by var
   */



SCM 
scm_m_do (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM x = SCM_CDR (xorig), arg1, proc;
  SCM vars = SCM_EOL, inits = SCM_EOL, steps = SCM_EOL;
  SCM *initloc = &inits, *steploc = &steps;
  int len = scm_ilength (x);
  ASSYNT (len >= 2, xorig, s_test, "do");
  proc = SCM_CAR (x);
  ASSYNT (scm_ilength (proc) >= 0, xorig, s_bindings, "do");
  while SCM_NIMP
    (proc)
    {
      arg1 = SCM_CAR (proc);
      len = scm_ilength (arg1);
      ASSYNT (2 == len || 3 == len, xorig, s_bindings, "do");
      ASSYNT (SCM_NIMP (SCM_CAR (arg1)) && SCM_SYMBOLP (SCM_CAR (arg1)), xorig, s_variable, "do");
      /* vars reversed here, inits and steps reversed at evaluation */
      vars = scm_cons (SCM_CAR (arg1), vars);	/* variable */
      arg1 = SCM_CDR (arg1);
      *initloc = scm_cons (SCM_CAR (arg1), SCM_EOL);	/* init */
      initloc = SCM_CDRLOC (*initloc);
      arg1 = SCM_CDR (arg1);
      *steploc = scm_cons (SCM_IMP (arg1) ? SCM_CAR (vars) : SCM_CAR (arg1), SCM_EOL);	/* step */
      steploc = SCM_CDRLOC (*steploc);
      proc = SCM_CDR (proc);
    }
  x = SCM_CDR (x);
  ASSYNT (scm_ilength (SCM_CAR (x)) >= 1, xorig, s_test, "do");
  x = scm_cons2 (SCM_CAR (x), SCM_CDR (x), steps);
  x = scm_cons2 (vars, inits, x);
  bodycheck (xorig, SCM_CARLOC (SCM_CDR (SCM_CDR (x))), "do");
  return scm_cons (SCM_IM_DO, x);
}

/* evalcar is small version of inline EVALCAR when we don't care about
 * speed
 */
#define evalcar scm_eval_car


static SCM  iqq SCM_P ((SCM form, SCM env, int depth));

static SCM 
iqq (form, env, depth)
     SCM form;
     SCM env;
     int depth;
{
  SCM tmp;
  int edepth = depth;
  if SCM_IMP
    (form) return form;
  if (SCM_VECTORP (form))
    {
      long i = SCM_LENGTH (form);
      SCM *data = SCM_VELTS (form);
      tmp = SCM_EOL;
      for (; --i >= 0;)
	tmp = scm_cons (data[i], tmp);
      return scm_vector (iqq (tmp, env, depth));
    }
  if SCM_NCONSP
    (form) return form;
  tmp = SCM_CAR (form);
  if (scm_i_quasiquote == tmp)
    {
      depth++;
      goto label;
    }
  if (scm_i_unquote == tmp)
    {
      --depth;
    label:
      form = SCM_CDR (form);
      SCM_ASSERT (SCM_NIMP (form) && SCM_ECONSP (form) && SCM_NULLP (SCM_CDR (form)),
	      form, SCM_ARG1, s_quasiquote);
      if (0 == depth)
	return evalcar (form, env);
      return scm_cons2 (tmp, iqq (SCM_CAR (form), env, depth), SCM_EOL);
    }
  if (SCM_NIMP (tmp) && (scm_i_uq_splicing == SCM_CAR (tmp)))
    {
      tmp = SCM_CDR (tmp);
      if (0 == --edepth)
	return scm_append (scm_cons2 (evalcar (tmp, env), iqq (SCM_CDR (form), env, depth), SCM_EOL));
    }
  return scm_cons (iqq (SCM_CAR (form), env, edepth), iqq (SCM_CDR (form), env, depth));
}

/* Here are acros which return values rather than code. */


SCM 
scm_m_quasiquote (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM x = SCM_CDR (xorig);
  ASSYNT (scm_ilength (x) == 1, xorig, s_expression, s_quasiquote);
  return iqq (SCM_CAR (x), env, 1);
}


SCM 
scm_m_delay (xorig, env)
     SCM xorig;
     SCM env;
{
  ASSYNT (scm_ilength (xorig) == 2, xorig, s_expression, s_delay);
  xorig = SCM_CDR (xorig);
  return scm_makprom (scm_closure (scm_cons2 (SCM_EOL, SCM_CAR (xorig), SCM_CDR (xorig)),
				   env));
}


static SCM env_top_level SCM_P ((SCM env));

static SCM
env_top_level (env)
     SCM env;
{
  while (SCM_NIMP(env))
    {
      if (SCM_BOOL_T == scm_procedure_p (SCM_CAR(env)))
	return SCM_CAR(env);
      env = SCM_CDR (env);
    }
  return SCM_BOOL_F;
}


SCM 
scm_m_define (x, env)
     SCM x;
     SCM env;
{
  SCM proc, arg1 = x;
  x = SCM_CDR (x);
  /*  ASSYNT(SCM_NULLP(env), x, "bad placement", s_define);*/
  ASSYNT (scm_ilength (x) >= 2, arg1, s_expression, "define");
  proc = SCM_CAR (x);
  x = SCM_CDR (x);
  while (SCM_NIMP (proc) && SCM_CONSP (proc))
    {				/* nested define syntax */
      x = scm_cons (scm_cons2 (scm_i_lambda, SCM_CDR (proc), x), SCM_EOL);
      proc = SCM_CAR (proc);
    }
  ASSYNT (SCM_NIMP (proc) && SCM_SYMBOLP (proc), arg1, s_variable, "define");
  ASSYNT (1 == scm_ilength (x), arg1, s_expression, "define");
  if (SCM_TOP_LEVEL (env))
    {
      x = evalcar (x, env);
#ifdef DEBUG_EXTENSIONS
      if (SCM_REC_PROCNAMES_P && SCM_NIMP (x))
	{
	  arg1 = x;
	proc:
	  if (SCM_CLOSUREP (arg1)
	      /* Only the first definition determines the name. */
	      && scm_procedure_property (arg1, scm_i_name) == SCM_BOOL_F)
	    scm_set_procedure_property_x (arg1, scm_i_name, proc);
	  else if (SCM_TYP16 (arg1) == scm_tc16_macro
		   && SCM_CDR (arg1) != arg1)
	    {
	      arg1 = SCM_CDR (arg1);
	      goto proc;
	    }
	}
#endif
      arg1 = scm_sym2vcell (proc, env_top_level (env), SCM_BOOL_T);
#if 0
#ifndef RECKLESS
      if (SCM_NIMP (SCM_CDR (arg1)) && ((SCM) SCM_SNAME (SCM_CDR (arg1)) == proc)
	  && (SCM_CDR (arg1) != x))
	scm_warn ("redefining built-in ", SCM_CHARS (proc));
      else
#endif
      if (5 <= scm_verbose && SCM_UNDEFINED != SCM_CDR (arg1))
	scm_warn ("redefining ", SCM_CHARS (proc));
#endif
      SCM_SETCDR (arg1, x);
#ifdef SICP
      return scm_cons2 (scm_i_quote, SCM_CAR (arg1), SCM_EOL);
#else
      return SCM_UNSPECIFIED;
#endif
    }
  return scm_cons2 (SCM_IM_DEFINE, proc, x);
}

SCM
scm_m_undefine (x, env)
     SCM x, env;
{
  SCM arg1 = x;
  x = SCM_CDR (x);
  ASSYNT (SCM_TOP_LEVEL (env), arg1, "bad placement ", s_undefine);
  ASSYNT (SCM_NIMP (x) && SCM_CONSP (x) && SCM_CDR (x) == SCM_EOL,
	  arg1, s_expression, s_undefine);
  x = SCM_CAR (x);
  ASSYNT (SCM_NIMP (x) && SCM_SYMBOLP (x), arg1, s_variable, s_undefine);
  arg1 = scm_sym2vcell (x, env_top_level (env), SCM_BOOL_F);
  ASSYNT (SCM_NFALSEP (arg1) && !SCM_UNBNDP (SCM_CDR (arg1)),
	  x, "variable already unbound ", s_undefine);
#if 0
#ifndef RECKLESS
  if (SCM_NIMP (SCM_CDR (arg1)) && ((SCM) SCM_SNAME (SCM_CDR (arg1)) == x))
    scm_warn ("undefining built-in ", SCM_CHARS (x));
  else
#endif
    if (5 <= scm_verbose && SCM_UNDEFINED != SCM_CDR (arg1))
      scm_warn ("redefining ", SCM_CHARS (x));
#endif
  SCM_SETCDR (arg1, SCM_UNDEFINED);
#ifdef SICP
  return SCM_CAR (arg1);
#else
  return SCM_UNSPECIFIED;
#endif
}

/* end of acros */


SCM 
scm_m_letrec (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM cdrx = SCM_CDR (xorig);	/* locally mutable version of form */
  char *what = SCM_CHARS (SCM_CAR (xorig));
  SCM x = cdrx, proc, arg1;	/* structure traversers */
  SCM vars = SCM_EOL, inits = SCM_EOL, *initloc = &inits;

  ASRTSYNTAX (scm_ilength (x) >= 2, s_body);
  proc = SCM_CAR (x);
  if SCM_NULLP
    (proc) return scm_m_letstar (xorig, env);	/* null binding, let* faster */
  ASRTSYNTAX (scm_ilength (proc) >= 1, s_bindings);
  do
    {
      /* vars scm_list reversed here, inits reversed at evaluation */
      arg1 = SCM_CAR (proc);
      ASRTSYNTAX (2 == scm_ilength (arg1), s_bindings);
      ASRTSYNTAX (SCM_NIMP (SCM_CAR (arg1)) && SCM_SYMBOLP (SCM_CAR (arg1)), s_variable);
      vars = scm_cons (SCM_CAR (arg1), vars);
      *initloc = scm_cons (SCM_CAR (SCM_CDR (arg1)), SCM_EOL);
      initloc = SCM_CDRLOC (*initloc);
    }
  while SCM_NIMP
  (proc = SCM_CDR (proc));
  cdrx = scm_cons2 (vars, inits, SCM_CDR (x));
  bodycheck (xorig, SCM_CDRLOC (SCM_CDR (cdrx)), what);
  return scm_cons (SCM_IM_LETREC, cdrx);
}


SCM 
scm_m_let (xorig, env)
     SCM xorig;
     SCM env;
{
  SCM cdrx = SCM_CDR (xorig);	/* locally mutable version of form */
  SCM x = cdrx, proc, arg1, name;	/* structure traversers */
  SCM vars = SCM_EOL, inits = SCM_EOL, *varloc = &vars, *initloc = &inits;

  ASSYNT (scm_ilength (x) >= 2, xorig, s_body, "let");
  proc = SCM_CAR (x);
  if (SCM_NULLP (proc)
      || (SCM_NIMP (proc) && SCM_CONSP (proc)
	  && SCM_NIMP (SCM_CAR (proc)) && SCM_CONSP (SCM_CAR (proc)) && SCM_NULLP (SCM_CDR (proc))))
    return scm_m_letstar (xorig, env);	/* null or single binding, let* is faster */
  ASSYNT (SCM_NIMP (proc), xorig, s_bindings, "let");
  if (SCM_CONSP (proc))			/* plain let, proc is <bindings> */
      return scm_cons (SCM_IM_LET, SCM_CDR (scm_m_letrec (xorig, env)));
  if (!SCM_SYMBOLP (proc))
    scm_wta (xorig, s_bindings, "let");	/* bad let */
  name = proc;			/* named let, build equiv letrec */
  x = SCM_CDR (x);
  ASSYNT (scm_ilength (x) >= 2, xorig, s_body, "let");
  proc = SCM_CAR (x);		/* bindings scm_list */
  ASSYNT (scm_ilength (proc) >= 0, xorig, s_bindings, "let");
  while SCM_NIMP
    (proc)
    {				/* vars and inits both in order */
      arg1 = SCM_CAR (proc);
      ASSYNT (2 == scm_ilength (arg1), xorig, s_bindings, "let");
      ASSYNT (SCM_NIMP (SCM_CAR (arg1)) && SCM_SYMBOLP (SCM_CAR (arg1)), xorig, s_variable, "let");
      *varloc = scm_cons (SCM_CAR (arg1), SCM_EOL);
      varloc = SCM_CDRLOC (*varloc);
      *initloc = scm_cons (SCM_CAR (SCM_CDR (arg1)), SCM_EOL);
      initloc = SCM_CDRLOC (*initloc);
      proc = SCM_CDR (proc);
    }
  return
    scm_m_letrec (scm_cons2 (scm_i_let,
			     scm_cons (scm_cons2 (name, scm_cons2 (scm_i_lambda, vars, SCM_CDR (x)), SCM_EOL), SCM_EOL),
			     scm_acons (name, inits, SCM_EOL)), 	/* body */
		  env);
}



SCM 
scm_m_apply (xorig, env)
     SCM xorig;
     SCM env;
{
  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 2, xorig, s_expression, "@apply");
  return scm_cons (SCM_IM_APPLY, SCM_CDR (xorig));
}

#define s_atcall_cc (SCM_ISYMCHARS(SCM_IM_CONT)+1)


SCM 
scm_m_cont (xorig, env)
     SCM xorig;
     SCM env;
{
  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, xorig, s_expression, "@call-with-current-continuation");
  return scm_cons (SCM_IM_CONT, SCM_CDR (xorig));
}

/* scm_unmemocopy takes a memoized expression together with its
 * environment and rewrites it to its original form.  Thus, it is the
 * inversion of the rewrite rules above.  The procedure is not
 * optimized for speed.  It's used in scm_iprin1 when printing the
 * code of a closure, in scm_procedure_source, in display_frame when
 * generating the source for a stackframe in a backtrace, and in
 * display_expression.
 */

static SCM unmemocopy SCM_P ((SCM x, SCM env));

static SCM
unmemocopy (x, env)
     SCM x;
     SCM env;
{
  SCM ls, z;
#ifdef DEBUG_EXTENSIONS
  SCM p;
#endif
  if (SCM_NCELLP (x) || SCM_NECONSP (x))
    return x;
#ifdef DEBUG_EXTENSIONS
  p = scm_whash_lookup (scm_source_whash, x);
#endif
  switch (SCM_TYP7 (x))
    {
    case (127 & SCM_IM_AND):
      ls = z = scm_cons (scm_i_and, SCM_UNSPECIFIED);
      break;
    case (127 & SCM_IM_BEGIN):
      ls = z = scm_cons (scm_i_begin, SCM_UNSPECIFIED);
      break;
    case (127 & SCM_IM_CASE):
      ls = z = scm_cons (scm_i_case, SCM_UNSPECIFIED);
      break;
    case (127 & SCM_IM_COND):
      ls = z = scm_cons (scm_i_cond, SCM_UNSPECIFIED);
      break;
    case (127 & SCM_IM_DO):
      ls = scm_cons (scm_i_do, SCM_UNSPECIFIED);
      goto transform;
    case (127 & SCM_IM_IF):
      ls = z = scm_cons (scm_i_if, SCM_UNSPECIFIED);
      break;
    case (127 & SCM_IM_LET):
      ls = scm_cons (scm_i_let, SCM_UNSPECIFIED);
      goto transform;
    case (127 & SCM_IM_LETREC):
      {
	SCM f, v, e, s;
	ls = scm_cons (scm_i_letrec, SCM_UNSPECIFIED);
      transform:
	x = SCM_CDR (x);
	f = v = SCM_CAR (x);
	x = SCM_CDR (x);
	z = EXTEND_ENV (f, SCM_EOL, env);
	e = scm_reverse (unmemocopy (SCM_CAR (x),
				     SCM_CAR (ls) == scm_i_letrec ? z : env));
	env = z;
	s = SCM_CAR (ls) == scm_i_do
	    ? scm_reverse (unmemocopy (SCM_CDR (SCM_CDR (SCM_CDR (x))), env))
	    : f;
	z = SCM_EOL;
	do
	  {
	    z = scm_acons (SCM_CAR (v),
			   scm_cons (SCM_CAR (e),
				     SCM_CAR (s) == SCM_CAR (v)
				     ? SCM_EOL
				     : scm_cons (SCM_CAR (s), SCM_EOL)),
			   z);
	    v = SCM_CDR (v);
	    e = SCM_CDR (e);
	    s = SCM_CDR (s);
	  }
	while SCM_NIMP (v);
	z = scm_cons (z, SCM_UNSPECIFIED);
	SCM_SETCDR (ls, z);
	if (SCM_CAR (ls) == scm_i_do)
	  {
	    x = SCM_CDR (x);
	    SCM_SETCDR (z, scm_cons (unmemocopy (SCM_CAR (x), env),
				     SCM_UNSPECIFIED));
	    z = SCM_CDR (z);
	    x = (SCM) (SCM_CARLOC (SCM_CDR (x)) - 1);
	  }
	break;
      }
    case (127 & SCM_IM_LETSTAR):
      {
	SCM b, y;
	x = SCM_CDR (x);
	b = SCM_CAR (x);
	y = SCM_EOL;
	if SCM_IMP (b)
	  {
	    env = EXTEND_ENV (SCM_EOL, SCM_EOL, env);
	    goto letstar;
	  }
	y = z = scm_acons (SCM_CAR (b),
			   unmemocar (
	scm_cons (unmemocopy (SCM_CAR (SCM_CDR (b)), env), SCM_EOL), env),
			   SCM_UNSPECIFIED);
	env = EXTEND_ENV (SCM_CAR (b), SCM_BOOL_F, env);
	b = SCM_CDR (SCM_CDR (b));
	if (SCM_IMP (b))
	  {
	    SCM_SETCDR (y, SCM_EOL);
	    ls = scm_cons (scm_i_let, z = scm_cons (y, SCM_UNSPECIFIED));
	    break;
	  }
	do
	  {
	    SCM_SETCDR (z, scm_acons (SCM_CAR (b),
				      unmemocar (
	    scm_cons (unmemocopy (SCM_CAR (SCM_CDR (b)), env), SCM_EOL), env),
				      SCM_UNSPECIFIED));
	    z = SCM_CDR (z);
	    env = EXTEND_ENV (SCM_CAR (b), SCM_BOOL_F, env);
	    b = SCM_CDR (SCM_CDR (b));
	  }
	while SCM_NIMP (b);
	SCM_SETCDR (z, SCM_EOL);
      letstar:
	ls = scm_cons (scm_i_letstar, z = scm_cons (y, SCM_UNSPECIFIED));
	break;
      }
    case (127 & SCM_IM_OR):
      ls = z = scm_cons (scm_i_or, SCM_UNSPECIFIED);
      break;
    case (127 & SCM_IM_LAMBDA):
      x = SCM_CDR (x);
      ls = scm_cons (scm_i_lambda,
		     z = scm_cons (SCM_CAR (x), SCM_UNSPECIFIED));
      env = EXTEND_ENV (SCM_CAR (x), SCM_EOL, env);
      break;
    case (127 & SCM_IM_QUOTE):
      ls = z = scm_cons (scm_i_quote, SCM_UNSPECIFIED);
      break;
    case (127 & SCM_IM_SET):
      ls = z = scm_cons (scm_i_set, SCM_UNSPECIFIED);
      break;
    case (127 & SCM_IM_DEFINE):
      {
	SCM n;
	x = SCM_CDR (x);
	ls = scm_cons (scm_i_define,
		       z = scm_cons (n = SCM_CAR (x), SCM_UNSPECIFIED));
	if (SCM_NNULLP (env))
	  SCM_SETCAR (SCM_CAR (env), scm_cons (n, SCM_CAR (SCM_CAR (env))));
	break;
      }
    case (127 & SCM_MAKISYM (0)):
      z = SCM_CAR (x);
      if (!SCM_ISYMP (z))
	goto unmemo;
      switch SCM_ISYMNUM (z)
	{
	case (SCM_ISYMNUM (SCM_IM_APPLY)):
	  ls = z = scm_cons (scm_i_atapply, SCM_UNSPECIFIED);
	  goto loop;
	case (SCM_ISYMNUM (SCM_IM_CONT)):
	  ls = z = scm_cons (scm_i_atcall_cc, SCM_UNSPECIFIED);
	  goto loop;
	default:
	  /* appease the Sun compiler god: */ ;
	}
    unmemo:
    default:
      ls = z = unmemocar (scm_cons (unmemocopy (SCM_CAR (x), env),
				    SCM_UNSPECIFIED),
			  env);
    }
loop:
  while (SCM_CELLP (x = SCM_CDR (x)) && SCM_ECONSP (x))
    {
      SCM_SETCDR (z, unmemocar (scm_cons (unmemocopy (SCM_CAR (x), env),
					  SCM_UNSPECIFIED),
				env));
      z = SCM_CDR (z);
    }
  SCM_SETCDR (z, x);
#ifdef DEBUG_EXTENSIONS
  if (SCM_NFALSEP (p))
    scm_whash_insert (scm_source_whash, ls, p);
#endif
  return ls;
}


SCM
scm_unmemocopy (x, env)
     SCM x;
     SCM env;
{
  if (SCM_NNULLP (env))
    /* Make a copy of the lowest frame to protect it from
       modifications by SCM_IM_DEFINE */
    return unmemocopy (x, scm_cons (SCM_CAR (env), SCM_CDR (env)));
  else
    return unmemocopy (x, env);
}

#ifndef RECKLESS

int 
scm_badargsp (formals, args)
     SCM formals;
     SCM args;
{
  while SCM_NIMP
    (formals)
    {
      if SCM_NCONSP
	(formals) return 0;
      if SCM_IMP
	(args) return 1;
      formals = SCM_CDR (formals);
      args = SCM_CDR (args);
    }
  return SCM_NNULLP (args) ? 1 : 0;
}
#endif



long scm_tc16_macro;


SCM 
scm_eval_args (l, env, proc)
     SCM l;
     SCM env;
     SCM proc;
{
  SCM results = SCM_EOL, *lloc = &results, res;
  while (SCM_NIMP (l))
    {
#ifdef CAUTIOUS
      if (SCM_IMP (l))
	goto wrongnumargs;
      else if (SCM_CONSP (l))
	{
	  if (SCM_IMP (SCM_CAR (l)))
	    res = EVALIM (SCM_CAR (l), env);
	  else
	    res = EVALCELLCAR (l, env);
	}
      else if (SCM_TYP3 (l) == 1)
	{
	  if ((res = SCM_GLOC_VAL (SCM_CAR (l))) == 0)
	    res = SCM_CAR (l); /* struct planted in code */
	}
      else
	goto wrongnumargs;
#else
      res = EVALCAR (l, env);
#endif
      *lloc = scm_cons (res, SCM_EOL);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
#ifdef CAUTIOUS
  if (SCM_NNULLP (l))
    {
    wrongnumargs:
      scm_wrong_num_args (proc);
    }
#endif
  return results;
}


#endif /* !DEVAL */


/* SECTION: This code is specific for the debugging support.  One
 * branch is read when DEVAL isn't defined, the other when DEVAL is
 * defined.
 */

#ifndef DEVAL

#define SCM_APPLY scm_apply
#define PREP_APPLY(proc, args)
#define ENTER_APPLY
#define RETURN(x) return x;
#ifdef STACK_CHECKING
#ifndef NO_CEVAL_STACK_CHECKING
#define EVAL_STACK_CHECKING
#endif
#endif

#else /* !DEVAL */

#undef SCM_CEVAL
#define SCM_CEVAL scm_deval	/* Substitute all uses of scm_ceval */
#undef SCM_APPLY
#define SCM_APPLY scm_dapply
#undef PREP_APPLY
#define PREP_APPLY(p, l) \
{ ++debug.info; debug.info->a.proc = p; debug.info->a.args = l; }
#undef ENTER_APPLY
#define ENTER_APPLY \
{\
  SCM_SET_ARGSREADY (debug);\
  if (CHECK_APPLY && SCM_TRAPS_P)\
    if (SCM_APPLY_FRAME_P || (SCM_TRACE_P && PROCTRACEP (proc)))\
      {\
	SCM tmp, tail = SCM_TRACED_FRAME_P (debug) ? SCM_BOOL_T : SCM_BOOL_F;\
	SCM_SET_TRACED_FRAME (debug); \
	if (SCM_CHEAPTRAPS_P)\
	  {\
	    tmp = scm_make_debugobj (&debug);\
	    scm_ithrow (scm_i_apply_frame, scm_cons2 (tmp, tail, SCM_EOL), 0);\
 	  }\
	else\
	  {\
	    scm_make_cont (&tmp);\
	    if (!setjmp (SCM_JMPBUF (tmp)))\
	      scm_ithrow (scm_i_apply_frame, scm_cons2 (tmp, tail, SCM_EOL), 0);\
	  }\
      }\
}
#undef RETURN
#define RETURN(e) {proc = (e); goto exit;}
#ifdef STACK_CHECKING
#ifndef EVAL_STACK_CHECKING
#define EVAL_STACK_CHECKING
#endif
#endif

/* scm_ceval_ptr points to the currently selected evaluator.
 * *fixme*: Although efficiency is important here, this state variable
 * should probably not be a global.  It should be related to the
 * current repl.
 */


SCM (*scm_ceval_ptr) SCM_P ((SCM x, SCM env));

/* scm_last_debug_frame contains a pointer to the last debugging
 * information stack frame.  It is accessed very often from the
 * debugging evaluator, so it should probably not be indirectly
 * addressed.  Better to save and restore it from the current root at
 * any stack swaps.
 */

#ifndef USE_THREADS
scm_debug_frame *scm_last_debug_frame;
#endif

/* scm_debug_eframe_size is the number of slots available for pseudo
 * stack frames at each real stack frame.
 */

int scm_debug_eframe_size;

int scm_debug_mode, scm_check_entry_p, scm_check_apply_p, scm_check_exit_p;

int scm_eval_stack;

scm_option scm_eval_opts[] = {
  { SCM_OPTION_INTEGER, "stack", 22000, "Size of thread stacks (in machine words)." }
};

scm_option scm_debug_opts[] = {
  { SCM_OPTION_BOOLEAN, "cheap", 1,
    "*Flyweight representation of the stack at traps." },
  { SCM_OPTION_BOOLEAN, "breakpoints", 0, "*Check for breakpoints." },
  { SCM_OPTION_BOOLEAN, "trace", 0, "*Trace mode." },
  { SCM_OPTION_BOOLEAN, "procnames", 1,
    "Record procedure names at definition." },
  { SCM_OPTION_BOOLEAN, "backwards", 0,
    "Display backtrace in anti-chronological order." },
  { SCM_OPTION_INTEGER, "indent", 10, "Maximal indentation in backtrace." },
  { SCM_OPTION_INTEGER, "frames", 3,
    "Maximum number of tail-recursive frames in backtrace." },
  { SCM_OPTION_INTEGER, "maxdepth", 1000,
    "Maximal number of stored backtrace frames." },
  { SCM_OPTION_INTEGER, "depth", 20, "Maximal length of printed backtrace." },
  { SCM_OPTION_BOOLEAN, "backtrace", 0, "Show backtrace on error." },
  { SCM_OPTION_BOOLEAN, "debug", 0, "Use the debugging evaluator." },
  { SCM_OPTION_INTEGER, "stack", 20000, "Stack size limit (measured in words; 0 = no check)." }
};

scm_option scm_evaluator_trap_table[] = {
  { SCM_OPTION_BOOLEAN, "traps", 0, "Enable evaluator traps." },
  { SCM_OPTION_BOOLEAN, "enter-frame", 0, "Trap when eval enters new frame." },
  { SCM_OPTION_BOOLEAN, "apply-frame", 0, "Trap when entering apply." },
  { SCM_OPTION_BOOLEAN, "exit-frame", 0, "Trap when exiting eval or apply." }
};

SCM_PROC (s_eval_options_interface, "eval-options-interface", 0, 1, 0, scm_eval_options_interface);

SCM
scm_eval_options_interface (SCM setting)
{
  SCM ans;
  SCM_DEFER_INTS;
  ans = scm_options (setting,
		     scm_eval_opts,
		     SCM_N_EVAL_OPTIONS,
		     s_eval_options_interface);
  scm_eval_stack = SCM_EVAL_STACK * sizeof (void *);
  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_evaluator_traps, "evaluator-traps-interface", 0, 1, 0, scm_evaluator_traps);

SCM
scm_evaluator_traps (setting)
     SCM setting;
{
  SCM ans;
  SCM_DEFER_INTS;
  ans = scm_options (setting,
		     scm_evaluator_trap_table,
		     SCM_N_EVALUATOR_TRAPS,
		     s_evaluator_traps);
  SCM_RESET_DEBUG_MODE;
  SCM_ALLOW_INTS
  return ans;
}

SCM
scm_deval_args (l, env, proc, lloc)
     SCM l, env, proc, *lloc;
{
  SCM *results = lloc, res;
  while (SCM_NIMP (l))
    {
#ifdef CAUTIOUS
      if (SCM_IMP (l))
	goto wrongnumargs;
      else if (SCM_CONSP (l))
	{
	  if (SCM_IMP (SCM_CAR (l)))
	    res = EVALIM (SCM_CAR (l), env);
	  else
	    res = EVALCELLCAR (l, env);
	}
      else if (SCM_TYP3 (l) == 1)
	{
	  if ((res = SCM_GLOC_VAL (SCM_CAR (l))) == 0)
	    res = SCM_CAR (l); /* struct planted in code */
	}
      else
	goto wrongnumargs;
#else
      res = EVALCAR (l, env);
#endif
      *lloc = scm_cons (res, SCM_EOL);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
#ifdef CAUTIOUS
  if (SCM_NNULLP (l))
    {
    wrongnumargs:
      scm_wrong_num_args (proc);
    }
#endif
  return *results;
}

#endif /* !DEVAL */


/* SECTION: Some local definitions for the evaluator.
 */

#ifndef DEVAL
#ifdef SCM_FLOATS
#define CHECK_EQVISH(A,B) 	(((A) == (B)) || (SCM_NFALSEP (scm_eqv_p ((A), (B)))))
#else
#define CHECK_EQVISH(A,B) 	((A) == (B))
#endif
#endif /* DEVAL */

#define BUILTIN_RPASUBR /* Handle rpsubrs and asubrs without calling apply */

/* SECTION: This is the evaluator.  Like any real monster, it has
 * three heads.  This code is compiled twice.
 */

#if 0

SCM 
scm_ceval (x, env)
     SCM x;
     SCM env;
{}
#endif
#if 0

SCM 
scm_deval (x, env)
     SCM x;
     SCM env;
{}
#endif

SCM 
SCM_CEVAL (x, env)
     SCM x;
     SCM env;
{
  union
    {
      SCM *lloc;
      SCM arg1;
   } t;
  SCM proc, arg2;
#ifdef DEVAL
  scm_debug_frame debug;
  scm_debug_info *debug_info_end;
  debug.prev = scm_last_debug_frame;
  debug.status = scm_debug_eframe_size;
  debug.vect = (scm_debug_info *) alloca (scm_debug_eframe_size
					  * sizeof (debug.vect[0]));
  debug.info = debug.vect;
  debug_info_end = debug.vect + scm_debug_eframe_size;
  scm_last_debug_frame = &debug;
#endif
#ifdef EVAL_STACK_CHECKING
  if (SCM_STACK_OVERFLOW_P ((SCM_STACKITEM *) &proc)
      && scm_stack_checking_enabled_p)
    {
#ifdef DEVAL
      debug.info->e.exp = x;
      debug.info->e.env = env;
#endif
      scm_report_stack_overflow ();
    }
#endif
#ifdef DEVAL
  goto start;
#endif
loopnoap:
  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
loop:
#ifdef DEVAL
  SCM_CLEAR_ARGSREADY (debug);
  if (SCM_OVERFLOWP (debug))
    --debug.info;
  else if (++debug.info >= debug_info_end)
    {
      SCM_SET_OVERFLOW (debug);
      debug.info -= 2;
    }
start:
  debug.info->e.exp = x;
  debug.info->e.env = env;
  if (CHECK_ENTRY && SCM_TRAPS_P)
    if (SCM_ENTER_FRAME_P || (SCM_BREAKPOINTS_P && SRCBRKP (x)))
      {
	SCM tail = SCM_TAILRECP (debug) ? SCM_BOOL_T : SCM_BOOL_F;
	SCM_SET_TAILREC (debug);
	if (SCM_CHEAPTRAPS_P)
	  t.arg1 = scm_make_debugobj (&debug);
	else
	  {
	    scm_make_cont (&t.arg1);
	    if (setjmp (SCM_JMPBUF (t.arg1)))
	      {
		x = SCM_THROW_VALUE (t.arg1);
		if (SCM_IMP (x))
		  {
		    RETURN (x);
		  }
		else
		  /* This gives the possibility for the debugger to
		     modify the source expression before evaluation. */
		  goto dispatch;
	      }
	  }
	scm_ithrow (scm_i_enter_frame,
		    scm_cons2 (t.arg1, tail,
			       scm_cons (scm_unmemocopy (x, env), SCM_EOL)),
		    0);
      }
#endif
#if defined (USE_THREADS) || defined (DEVAL)
dispatch:
#endif
  SCM_TICK;
  switch (SCM_TYP7 (x))
    {
    case scm_tcs_symbols:
      /* Only happens when called at top level.
       */
      x = scm_cons (x, SCM_UNDEFINED);
      goto retval;

    case (127 & SCM_IM_AND):
      x = SCM_CDR (x);
      t.arg1 = x;
      while (SCM_NNULLP (t.arg1 = SCM_CDR (t.arg1)))
	if (SCM_FALSEP (EVALCAR (x, env)))
	  {
	    RETURN (SCM_BOOL_F);
	  }
	else
	  x = t.arg1;
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;

    case (127 & SCM_IM_BEGIN):
    cdrxnoap:
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
    cdrxbegin:
      x = SCM_CDR (x);

    begin:
      t.arg1 = x;
      while (SCM_NNULLP (t.arg1 = SCM_CDR (t.arg1)))
	{
	  SIDEVAL (SCM_CAR (x), env);
	  x = t.arg1;
	}

    carloop:			/* scm_eval car of last form in list */
      if (SCM_NCELLP (SCM_CAR (x)))
	{
	  x = SCM_CAR (x);
	  RETURN (SCM_IMP (x) ? EVALIM (x, env) : SCM_GLOC_VAL (x))
	}

      if (SCM_SYMBOLP (SCM_CAR (x)))
	{
	retval:
	  RETURN (*scm_lookupcar (x, env))
	}

      x = SCM_CAR (x);
      goto loop;		/* tail recurse */


    case (127 & SCM_IM_CASE):
      x = SCM_CDR (x);
      t.arg1 = EVALCAR (x, env);
      while (SCM_NIMP (x = SCM_CDR (x)))
	{
	  proc = SCM_CAR (x);
	  if (scm_i_else == SCM_CAR (proc))
	    {
	      x = SCM_CDR (proc);
	      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	      goto begin;
	    }
	  proc = SCM_CAR (proc);
	  while (SCM_NIMP (proc))
	    {
	      if (CHECK_EQVISH (SCM_CAR (proc), t.arg1))
		{
		  x = SCM_CDR (SCM_CAR (x));
		  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		  goto begin;
		}
	      proc = SCM_CDR (proc);
	    }
	}
      RETURN (SCM_UNSPECIFIED)


    case (127 & SCM_IM_COND):
      while (SCM_NIMP (x = SCM_CDR (x)))
	{
	  proc = SCM_CAR (x);
	  t.arg1 = EVALCAR (proc, env);
	  if (SCM_NFALSEP (t.arg1))
	    {
	      x = SCM_CDR (proc);
	      if SCM_NULLP (x)
		{
		  RETURN (t.arg1)
		}
	      if (scm_i_arrow != SCM_CAR (x))
		{
		  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		  goto begin;
		}
	      proc = SCM_CDR (x);
	      proc = EVALCAR (proc, env);
	      SCM_ASRTGO (SCM_NIMP (proc), badfun);
	      PREP_APPLY (proc, scm_cons (t.arg1, SCM_EOL));
	      ENTER_APPLY;
	      goto evap1;
	    }
	}
      RETURN (SCM_UNSPECIFIED)


    case (127 & SCM_IM_DO):
      x = SCM_CDR (x);
      proc = SCM_CAR (SCM_CDR (x)); /* inits */
      t.arg1 = SCM_EOL;		/* values */
      while (SCM_NIMP (proc))
	{
	  t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1);
	  proc = SCM_CDR (proc);
	}
      env = EXTEND_ENV (SCM_CAR (x), t.arg1, env);
      x = SCM_CDR (SCM_CDR (x));
      while (proc = SCM_CAR (x), SCM_FALSEP (EVALCAR (proc, env)))
	{
	  for (proc = SCM_CAR (SCM_CDR (x)); SCM_NIMP (proc); proc = SCM_CDR (proc))
	    {
	      t.arg1 = SCM_CAR (proc); /* body */
	      SIDEVAL (t.arg1, env);
	    }
	  for (t.arg1 = SCM_EOL, proc = SCM_CDR (SCM_CDR (x)); SCM_NIMP (proc); proc = SCM_CDR (proc))
	    t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1); /* steps */
	  env = EXTEND_ENV (SCM_CAR (SCM_CAR (env)), t.arg1, SCM_CDR (env));
	}
      x = SCM_CDR (proc);
      if (SCM_NULLP (x))
	RETURN (SCM_UNSPECIFIED);
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto begin;


    case (127 & SCM_IM_IF):
      x = SCM_CDR (x);
      if (SCM_NFALSEP (EVALCAR (x, env)))
	x = SCM_CDR (x);
      else if (SCM_IMP (x = SCM_CDR (SCM_CDR (x))))
	{
	  RETURN (SCM_UNSPECIFIED);
	}
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;


    case (127 & SCM_IM_LET):
      x = SCM_CDR (x);
      proc = SCM_CAR (SCM_CDR (x));
      t.arg1 = SCM_EOL;
      do
	{
	  t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1);
	}
      while (SCM_NIMP (proc = SCM_CDR (proc)));
      env = EXTEND_ENV (SCM_CAR (x), t.arg1, env);
      x = SCM_CDR (x);
      goto cdrxnoap;


    case (127 & SCM_IM_LETREC):
      x = SCM_CDR (x);
      env = EXTEND_ENV (SCM_CAR (x), scm_undefineds, env);
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      t.arg1 = SCM_EOL;
      do
	{
	  t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1);
	}
      while (SCM_NIMP (proc = SCM_CDR (proc)));
      SCM_SETCDR (SCM_CAR (env), t.arg1);
      goto cdrxnoap;


    case (127 & SCM_IM_LETSTAR):
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      if (SCM_IMP (proc))
	{
	  env = EXTEND_ENV (SCM_EOL, SCM_EOL, env);
	  goto cdrxnoap;
	}
      do
	{
	  t.arg1 = SCM_CAR (proc);
	  proc = SCM_CDR (proc);
	  env = EXTEND_ENV (t.arg1, EVALCAR (proc, env), env);
	}
      while (SCM_NIMP (proc = SCM_CDR (proc)));
      goto cdrxnoap;

    case (127 & SCM_IM_OR):
      x = SCM_CDR (x);
      t.arg1 = x;
      while (SCM_NNULLP (t.arg1 = SCM_CDR (t.arg1)))
	{
	  x = EVALCAR (x, env);
	  if (SCM_NFALSEP (x))
	    {
	      RETURN (x);
	    }
	  x = t.arg1;
	}
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;


    case (127 & SCM_IM_LAMBDA):
      RETURN (scm_closure (SCM_CDR (x), env));


    case (127 & SCM_IM_QUOTE):
      RETURN (SCM_CAR (SCM_CDR (x)));


    case (127 & SCM_IM_SET):
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      switch (7 & (int) proc)
	{
	case 0:
	  t.lloc = scm_lookupcar (x, env);
	  break;
	case 1:
	  t.lloc = SCM_GLOC_VAL_LOC (proc);
	  break;
#ifdef MEMOIZE_LOCALS
	case 4:
	  t.lloc = scm_ilookup (proc, env);
	  break;
#endif
	}
      x = SCM_CDR (x);
      *t.lloc = EVALCAR (x, env);
#ifdef SICP
      RETURN (*t.lloc);
#else
      RETURN (SCM_UNSPECIFIED);
#endif


    case (127 & SCM_IM_DEFINE):	/* only for internal defines */
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      x = SCM_CDR (x);
      x = evalcar (x, env);
#ifdef DEBUG_EXTENSIONS
      if (SCM_REC_PROCNAMES_P && SCM_NIMP (x))
	{
	  t.arg1 = x;
	proc:
	  if (SCM_CLOSUREP (t.arg1)
	      /* Only the first definition determines the name. */
	      && (scm_procedure_property (t.arg1, scm_i_inner_name)
		  == SCM_BOOL_F))
	    scm_set_procedure_property_x (t.arg1, scm_i_inner_name, proc);
	  else if (SCM_TYP16 (t.arg1) == scm_tc16_macro
		   && SCM_CDR (t.arg1) != t.arg1)
	    {
	      t.arg1 = SCM_CDR (t.arg1);
	      goto proc;
	    }
	}
#endif
      env = SCM_CAR (env);
      SCM_DEFER_INTS;
      SCM_SETCAR (env, scm_cons (proc, SCM_CAR (env)));
      SCM_SETCDR (env, scm_cons (x, SCM_CDR (env)));
      SCM_ALLOW_INTS;
      RETURN (SCM_UNSPECIFIED);


      /* new syntactic forms go here. */
    case (127 & SCM_MAKISYM (0)):
      proc = SCM_CAR (x);
      SCM_ASRTGO (SCM_ISYMP (proc), badfun);
      switch SCM_ISYMNUM (proc)
	{
#if 0
	case (SCM_ISYMNUM (IM_VREF)):
	  {
	    SCM var;
	    var = SCM_CAR (SCM_CDR (x));
	    RETURN (SCM_CDR(var));
	  }
	case (SCM_ISYMNUM (IM_VSET)):
	  SCM_CDR (SCM_CAR ( SCM_CDR (x))) = EVALCAR( SCM_CDR ( SCM_CDR (x)), env);
	  SCM_CAR (SCM_CAR ( SCM_CDR (x))) = scm_tc16_variable;
	  RETURN (SCM_UNSPECIFIED)
#endif

	case (SCM_ISYMNUM (SCM_IM_APPLY)):
	  proc = SCM_CDR (x);
	  proc = EVALCAR (proc, env);
	  SCM_ASRTGO (SCM_NIMP (proc), badfun);
	  if (SCM_CLOSUREP (proc))
	    {
	      SCM argl, tl;
	      PREP_APPLY (proc, SCM_EOL);
	      t.arg1 = SCM_CDR (SCM_CDR (x));
	      t.arg1 = EVALCAR (t.arg1, env);
#ifdef DEVAL
	      debug.info->a.args = t.arg1;
#endif
#ifndef RECKLESS
	      if (scm_badargsp (SCM_CAR (SCM_CODE (proc)), t.arg1))
		goto wrongnumargs;
#endif
	      /* Copy argument list */
	      if (SCM_IMP (t.arg1))
		argl = t.arg1;
	      else
		{
		  argl = tl = scm_cons (SCM_CAR (t.arg1), SCM_UNSPECIFIED);
		  while (SCM_NIMP (t.arg1 = SCM_CDR (t.arg1))
			 && SCM_CONSP (t.arg1))
		    {
		      SCM_SETCDR (tl, scm_cons (SCM_CAR (t.arg1),
						SCM_UNSPECIFIED));
		      tl = SCM_CDR (tl);
		    }
		  SCM_SETCDR (tl, t.arg1);
		}
	      
	      env = EXTEND_ENV (SCM_CAR (SCM_CODE (proc)), argl, SCM_ENV (proc));
	      x = SCM_CODE (proc);
	      goto cdrxbegin;
	    }
	  proc = scm_i_apply;
	  goto evapply;

	case (SCM_ISYMNUM (SCM_IM_CONT)):
	  scm_make_cont (&t.arg1);
	  if (setjmp (SCM_JMPBUF (t.arg1)))
	    {
	      SCM val;
	      val = SCM_THROW_VALUE (t.arg1);
	      RETURN (val);
	    }
	  proc = SCM_CDR (x);
	  proc = evalcar (proc, env);
	  SCM_ASRTGO (SCM_NIMP (proc), badfun);
	  PREP_APPLY (proc, scm_cons (t.arg1, SCM_EOL));
	  ENTER_APPLY;
	  goto evap1;

	default:
	  goto badfun;
	}

    default:
      proc = x;
    badfun:
      /* scm_everr (x, env,...) */
      scm_misc_error (NULL,
		      "Wrong type to apply: %S",
		      scm_listify (proc, SCM_UNDEFINED));
    case scm_tc7_vector:
    case scm_tc7_wvect:
    case scm_tc7_bvect:
    case scm_tc7_byvect:
    case scm_tc7_svect:
    case scm_tc7_ivect:
    case scm_tc7_uvect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
#ifdef LONGLONGS
    case scm_tc7_llvect:
#endif
    case scm_tc7_string:
    case scm_tc7_substring:
    case scm_tc7_smob:
    case scm_tcs_closures:
    case scm_tcs_subrs:
      RETURN (x);

#ifdef MEMOIZE_LOCALS
    case (127 & SCM_ILOC00):
      proc = *scm_ilookup (SCM_CAR (x), env);
      SCM_ASRTGO (SCM_NIMP (proc), badfun);
#ifndef RECKLESS
#ifdef CAUTIOUS
      goto checkargs;
#endif
#endif
      break;
#endif /* ifdef MEMOIZE_LOCALS */


    case scm_tcs_cons_gloc:
      proc = SCM_GLOC_VAL (SCM_CAR (x));
      SCM_ASRTGO (SCM_NIMP (proc), badfun);
#ifndef RECKLESS
#ifdef CAUTIOUS
      goto checkargs;
#endif
#endif
      break;


    case scm_tcs_cons_nimcar:
      if (SCM_SYMBOLP (SCM_CAR (x)))
	{
#ifdef USE_THREADS
	  t.lloc = scm_lookupcar1 (x, env);
	  if (t.lloc == NULL)
	    {
	      /* we have lost the race, start again. */
	      goto dispatch;
	    }
	  proc = *t.lloc;
#else
	  proc = *scm_lookupcar (x, env);
#endif

	  if (SCM_IMP (proc))
	    {
	      unmemocar (x, env);
	      goto badfun;
	    }
	  if (scm_tc16_macro == SCM_TYP16 (proc))
	    {
	      unmemocar (x, env);

	    handle_a_macro:
	      t.arg1 = SCM_APPLY (SCM_CDR (proc), x,
				  scm_cons (env, scm_listofnull));

	      switch ((int) (SCM_CAR (proc) >> 16))
		{
		case 2:
		  if (scm_ilength (t.arg1) <= 0)
		    t.arg1 = scm_cons2 (SCM_IM_BEGIN, t.arg1, SCM_EOL);
#ifdef DEVAL
		  if (!SCM_CLOSUREP (SCM_CDR (proc)))
		    {

#if 0 /* Top-level defines doesn't very often occur in backtraces */
		      if (scm_m_define == SCM_SUBRF (SCM_CDR (proc)) && SCM_TOP_LEVEL (env))
			/* Prevent memoizing result of define macro */
			{
			  debug.info->e.exp = scm_cons (SCM_CAR (x), SCM_CDR (x));
			  scm_set_source_properties_x (debug.info->e.exp,
						       scm_source_properties (x));
			}
#endif
		      SCM_DEFER_INTS;
		      SCM_SETCAR (x, SCM_CAR (t.arg1));
		      SCM_SETCDR (x, SCM_CDR (t.arg1));
		      SCM_ALLOW_INTS;
		      goto dispatch;
		    }
		  /* Prevent memoizing of debug info expression. */
		  debug.info->e.exp = scm_cons (SCM_CAR (x), SCM_CDR (x));
		  scm_set_source_properties_x (debug.info->e.exp,
					       scm_source_properties (x));
#endif
		  SCM_DEFER_INTS;
		  SCM_SETCAR (x, SCM_CAR (t.arg1));
		  SCM_SETCDR (x, SCM_CDR (t.arg1));
		  SCM_ALLOW_INTS;
		  goto loopnoap;
		case 1:
		  if (SCM_NIMP (x = t.arg1))
		    goto loopnoap;
		case 0:
		  RETURN (t.arg1);
		}
	    }
	}
      else
	proc = SCM_CEVAL (SCM_CAR (x), env);
      SCM_ASRTGO (SCM_NIMP (proc), badfun);
#ifndef RECKLESS
#ifdef CAUTIOUS
    checkargs:
#endif
      if (SCM_CLOSUREP (proc))
	{
	  arg2 = SCM_CAR (SCM_CODE (proc));
	  t.arg1 = SCM_CDR (x);
	  while (SCM_NIMP (arg2))
	    {
	      if (SCM_NCONSP (arg2))
		goto evapply;
	      if (SCM_IMP (t.arg1))
		goto umwrongnumargs;
	      arg2 = SCM_CDR (arg2);
	      t.arg1 = SCM_CDR (t.arg1);
	    }
	  if (SCM_NNULLP (t.arg1))
	    goto umwrongnumargs;
	}
      else if (scm_tc16_macro == SCM_TYP16 (proc))
	goto handle_a_macro;
#endif
    }


evapply:
  PREP_APPLY (proc, SCM_EOL);
  if (SCM_NULLP (SCM_CDR (x))) {
    ENTER_APPLY;
    switch (SCM_TYP7 (proc))
      {				/* no arguments given */
      case scm_tc7_subr_0:
	RETURN (SCM_SUBRF (proc) ());
      case scm_tc7_subr_1o:
	RETURN (SCM_SUBRF (proc) (SCM_UNDEFINED));
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (SCM_EOL));
      case scm_tc7_rpsubr:
	RETURN (SCM_BOOL_T);
      case scm_tc7_asubr:
	RETURN (SCM_SUBRF (proc) (SCM_UNDEFINED, SCM_UNDEFINED));
#ifdef CCLO
      case scm_tc7_cclo:
	t.arg1 = proc;
	proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	debug.info->a.proc = proc;
	debug.info->a.args = scm_cons (t.arg1, SCM_EOL);
#endif
	goto evap1;
#endif
      case scm_tcs_closures:
	x = SCM_CODE (proc);
	env = EXTEND_ENV (SCM_CAR (x), SCM_EOL, SCM_ENV (proc));
	goto cdrxbegin;
      case scm_tcs_cons_gloc:
	if (SCM_I_OPERATORP (proc))
	  {
	    x = (SCM_I_ENTITYP (proc)
		 ? SCM_ENTITY_PROC_0 (proc)
		 : SCM_OPERATOR_PROC_0 (proc));
	    if (SCM_NIMP (x))
	      {
		if (SCM_TYP7 (x) == scm_tc7_subr_1)
		  RETURN (SCM_SUBRF (x) (proc))
		else if (SCM_CLOSUREP (x))
		  {
		    t.arg1 = proc;
		    proc = x;
#ifdef DEVAL
		    debug.info->a.args = scm_cons (t.arg1, SCM_EOL);
		    debug.info->a.proc = proc;
#endif
		    goto clos1;
		  }
	      }
	    /* Fall through. */
	  }
      case scm_tc7_contin:
      case scm_tc7_subr_1:
      case scm_tc7_subr_2:
      case scm_tc7_subr_2o:
      case scm_tc7_cxr:
      case scm_tc7_subr_3:
      case scm_tc7_lsubr_2:
      umwrongnumargs:
	unmemocar (x, env);
      wrongnumargs:
	/* scm_everr (x, env,...)  */
	scm_wrong_num_args (proc);
      default:
	/* handle macros here */
	goto badfun;
      }
  }

  /* must handle macros by here */
  x = SCM_CDR (x);
#ifdef CAUTIOUS
  if (SCM_IMP (x))
    goto wrongnumargs;
  else if (SCM_CONSP (x))
    {
      if (SCM_IMP (SCM_CAR (x)))
	t.arg1 = EVALIM (SCM_CAR (x), env);
      else
	t.arg1 = EVALCELLCAR (x, env);
    }
  else if (SCM_TYP3 (x) == 1)
    {
      if ((t.arg1 = SCM_GLOC_VAL (SCM_CAR (x))) == 0)
	t.arg1 = SCM_CAR (x); /* struct planted in code */
    }
  else
    goto wrongnumargs;
#else
  t.arg1 = EVALCAR (x, env);
#endif
#ifdef DEVAL
  debug.info->a.args = scm_cons (t.arg1, SCM_EOL);
#endif
  x = SCM_CDR (x);
  if (SCM_NULLP (x))
    {
      ENTER_APPLY;
    evap1:
      switch (SCM_TYP7 (proc))
	{				/* have one argument in t.arg1 */
	case scm_tc7_subr_2o:
	  RETURN (SCM_SUBRF (proc) (t.arg1, SCM_UNDEFINED));
	case scm_tc7_subr_1:
	case scm_tc7_subr_1o:
	  RETURN (SCM_SUBRF (proc) (t.arg1));
	case scm_tc7_cxr:
#ifdef SCM_FLOATS
	  if (SCM_SUBRF (proc))
	    {
	      if (SCM_INUMP (t.arg1))
		{
		  RETURN (scm_makdbl (SCM_DSUBRF (proc) ((double) SCM_INUM (t.arg1)),
				      0.0));
		}
	      SCM_ASRTGO (SCM_NIMP (t.arg1), floerr);
	      if (SCM_REALP (t.arg1))
		{
		  RETURN (scm_makdbl (SCM_DSUBRF (proc) (SCM_REALPART (t.arg1)), 0.0));
		}
#ifdef SCM_BIGDIG
	      if (SCM_BIGP (t.arg1))
		{
		  RETURN (scm_makdbl (SCM_DSUBRF (proc) (scm_big2dbl (t.arg1)), 0.0));
		}
#endif
	    floerr:
	      scm_wta (t.arg1, (char *) SCM_ARG1, SCM_CHARS (SCM_SNAME (proc)));
	    }
#endif
	  proc = (SCM) SCM_SNAME (proc);
	  {
	    char *chrs = SCM_CHARS (proc) + SCM_LENGTH (proc) - 1;
	    while ('c' != *--chrs)
	      {
		SCM_ASSERT (SCM_NIMP (t.arg1) && SCM_CONSP (t.arg1),
			    t.arg1, SCM_ARG1, SCM_CHARS (proc));
		t.arg1 = ('a' == *chrs) ? SCM_CAR (t.arg1) : SCM_CDR (t.arg1);
	      }
	    RETURN (t.arg1);
	  }
	case scm_tc7_rpsubr:
	  RETURN (SCM_BOOL_T);
	case scm_tc7_asubr:
	  RETURN (SCM_SUBRF (proc) (t.arg1, SCM_UNDEFINED));
	case scm_tc7_lsubr:
#ifdef DEVAL
	  RETURN (SCM_SUBRF (proc) (debug.info->a.args))
#else
	  RETURN (SCM_SUBRF (proc) (scm_cons (t.arg1, SCM_EOL)));
#endif
#ifdef CCLO
	case scm_tc7_cclo:
	  arg2 = t.arg1;
	  t.arg1 = proc;
	  proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	  debug.info->a.args = scm_cons (t.arg1, debug.info->a.args);
	  debug.info->a.proc = proc;
#endif
	  goto evap2;
#endif
	case scm_tcs_closures:
	clos1:
	  x = SCM_CODE (proc);
#ifdef DEVAL
	  env = EXTEND_ENV (SCM_CAR (x), debug.info->a.args, SCM_ENV (proc));
#else
	  env = EXTEND_ENV (SCM_CAR (x), scm_cons (t.arg1, SCM_EOL), SCM_ENV (proc));
#endif
	  goto cdrxbegin;
	case scm_tc7_contin:
	  scm_call_continuation (proc, t.arg1);
	case scm_tcs_cons_gloc:
	  if (SCM_I_OPERATORP (proc))
	    {
	      x = (SCM_I_ENTITYP (proc)
		   ? SCM_ENTITY_PROC_1 (proc)
		   : SCM_OPERATOR_PROC_1 (proc));
	      if (SCM_NIMP (x))
		{
		  if (SCM_TYP7 (x) == scm_tc7_subr_2)
		    RETURN (SCM_SUBRF (x) (proc, t.arg1))
		  else if (SCM_CLOSUREP (x))
		    {
		      arg2 = t.arg1;
		      t.arg1 = proc;
		      proc = x;
#ifdef DEVAL
		      debug.info->a.args = scm_cons (t.arg1,
						     debug.info->a.args);
		      debug.info->a.proc = proc;
#endif
		      goto clos2;
		    }
		}
	      /* Fall through. */
	    }
	case scm_tc7_subr_2:
	case scm_tc7_subr_0:
	case scm_tc7_subr_3:
	case scm_tc7_lsubr_2:
	  goto wrongnumargs;
	default:
	  goto badfun;
	}
    }
#ifdef CAUTIOUS
  if (SCM_IMP (x))
    goto wrongnumargs;
  else if (SCM_CONSP (x))
    {
      if (SCM_IMP (SCM_CAR (x)))
	arg2 = EVALIM (SCM_CAR (x), env);
      else
	arg2 = EVALCELLCAR (x, env);
    }
  else if (SCM_TYP3 (x) == 1)
    {
      if ((arg2 = SCM_GLOC_VAL (SCM_CAR (x))) == 0)
	arg2 = SCM_CAR (x); /* struct planted in code */
    }
  else
    goto wrongnumargs;
#else
  arg2 = EVALCAR (x, env);
#endif
  {				/* have two or more arguments */
#ifdef DEVAL
    debug.info->a.args = scm_cons2 (t.arg1, arg2, SCM_EOL);
#endif
    x = SCM_CDR (x);
    if (SCM_NULLP (x)) {
      ENTER_APPLY;
#ifdef CCLO
    evap2:
#endif
      switch (SCM_TYP7 (proc))
	{			/* have two arguments */
	case scm_tc7_subr_2:
	case scm_tc7_subr_2o:
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2));
	case scm_tc7_lsubr:
#ifdef DEVAL
	  RETURN (SCM_SUBRF (proc) (debug.info->a.args))
#else
	  RETURN (SCM_SUBRF (proc) (scm_cons2 (t.arg1, arg2, SCM_EOL)));
#endif
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2, SCM_EOL));
	case scm_tc7_rpsubr:
	case scm_tc7_asubr:
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2));
#ifdef CCLO
	cclon:
	case scm_tc7_cclo:
#ifdef DEVAL
	  RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc), proc,
			     scm_cons (debug.info->a.args, SCM_EOL)));
#else
	  RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc), proc,
			     scm_cons2 (t.arg1, arg2,
					scm_cons (scm_eval_args (x, env, proc),
						  SCM_EOL))));
#endif
	  /*    case scm_tc7_cclo:
		x = scm_cons(arg2, scm_eval_args(x, env));
		arg2 = t.arg1;
		t.arg1 = proc;
		proc = SCM_CCLO_SUBR(proc);
		goto evap3; */
#endif
	case scm_tcs_cons_gloc:
	  if (SCM_I_OPERATORP (proc))
	    {
	      x = (SCM_I_ENTITYP (proc)
		   ? SCM_ENTITY_PROC_2 (proc)
		   : SCM_OPERATOR_PROC_2 (proc));
	      if (SCM_NIMP (x))
		{
		  if (SCM_TYP7 (x) == scm_tc7_subr_3)
		    RETURN (SCM_SUBRF (x) (proc, t.arg1, arg2))
		  else if (SCM_CLOSUREP (x))
		    {
#ifdef DEVAL
		      SCM_SET_ARGSREADY (debug);
		      debug.info->a.args = scm_cons (proc,
						     debug.info->a.args);
		      debug.info->a.proc = x;
#endif
		      env = EXTEND_ENV (SCM_CAR (SCM_CODE (x)),
					scm_cons2 (proc, t.arg1,
						   scm_cons (arg2, SCM_EOL)),
					SCM_ENV (x));
		      x = SCM_CODE (x);
		      goto cdrxbegin;
		    }
		}
	      /* Fall through. */
	    }
	case scm_tc7_subr_0:
	case scm_tc7_cxr:
	case scm_tc7_subr_1o:
	case scm_tc7_subr_1:
	case scm_tc7_subr_3:
	case scm_tc7_contin:
	  goto wrongnumargs;
	default:
	  goto badfun;
	case scm_tcs_closures:
	clos2:
#ifdef DEVAL
	  env = EXTEND_ENV (SCM_CAR (SCM_CODE (proc)),
			    debug.info->a.args,
			    SCM_ENV (proc));
#else
	  env = EXTEND_ENV (SCM_CAR (SCM_CODE (proc)),
			    scm_cons2 (t.arg1, arg2, SCM_EOL), SCM_ENV (proc));
#endif
	  x = SCM_CODE (proc);
	  goto cdrxbegin;
	}
    }
#ifdef CAUTIOUS
    if (SCM_IMP (x) || SCM_NECONSP (x))
      goto wrongnumargs;
#endif
#ifdef DEVAL
    debug.info->a.args = scm_cons2 (t.arg1, arg2,
      scm_deval_args (x, env, proc,
		      SCM_CDRLOC (SCM_CDR (debug.info->a.args))));
#endif
    ENTER_APPLY;
    switch (SCM_TYP7 (proc))
      {			/* have 3 or more arguments */
#ifdef DEVAL
      case scm_tc7_subr_3:
	SCM_ASRTGO (SCM_NULLP (SCM_CDR (x)), wrongnumargs);
	RETURN (SCM_SUBRF (proc) (t.arg1, arg2,
				  SCM_CADDR (debug.info->a.args)));
      case scm_tc7_asubr:
#ifdef BUILTIN_RPASUBR
	t.arg1 = SCM_SUBRF(proc)(t.arg1, arg2);
	arg2 = SCM_CDR (SCM_CDR (debug.info->a.args));
	do
	  {
	    t.arg1 = SCM_SUBRF(proc)(t.arg1, SCM_CAR (arg2));
	    arg2 = SCM_CDR (arg2);
	  }
	while (SCM_NIMP (arg2));
	RETURN (t.arg1)
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_rpsubr:
#ifdef BUILTIN_RPASUBR
	if (SCM_FALSEP (SCM_SUBRF (proc) (t.arg1, arg2)))
	  RETURN (SCM_BOOL_F)
	t.arg1 = SCM_CDR (SCM_CDR (debug.info->a.args));
	do
	  {
	    if (SCM_FALSEP (SCM_SUBRF (proc) (arg2, SCM_CAR (t.arg1))))
	      RETURN (SCM_BOOL_F)
		arg2 = SCM_CAR (t.arg1);
	    t.arg1 = SCM_CDR (t.arg1);
	  }
	while (SCM_NIMP (t.arg1));
	RETURN (SCM_BOOL_T)
#else /* BUILTIN_RPASUBR */
	RETURN (SCM_APPLY (proc, t.arg1,
			   scm_acons (arg2,
				      SCM_CDR (SCM_CDR (debug.info->a.args)),
				      SCM_EOL)))
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_lsubr_2:
	RETURN (SCM_SUBRF (proc) (t.arg1, arg2,
				  SCM_CDR (SCM_CDR (debug.info->a.args))))
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (debug.info->a.args))
#ifdef CCLO
      case scm_tc7_cclo:
	goto cclon;
#endif
      case scm_tcs_closures:
	SCM_SET_ARGSREADY (debug);
	env = EXTEND_ENV (SCM_CAR (SCM_CODE (proc)),
			      debug.info->a.args,
			      SCM_ENV (proc));
	x = SCM_CODE (proc);
	goto cdrxbegin;
#else /* DEVAL */
      case scm_tc7_subr_3:
	SCM_ASRTGO (SCM_NULLP (SCM_CDR (x)), wrongnumargs);
	RETURN (SCM_SUBRF (proc) (t.arg1, arg2, EVALCAR (x, env)));
      case scm_tc7_asubr:
#ifdef BUILTIN_RPASUBR
	t.arg1 = SCM_SUBRF (proc) (t.arg1, arg2);
	do
	  {
	    t.arg1 = SCM_SUBRF(proc)(t.arg1, EVALCAR(x, env));
	    x = SCM_CDR(x);
	  }
	while (SCM_NIMP (x));
	RETURN (t.arg1)
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_rpsubr:
#ifdef BUILTIN_RPASUBR
	if (SCM_FALSEP (SCM_SUBRF (proc) (t.arg1, arg2)))
	  RETURN (SCM_BOOL_F)
	do
	  {
	    t.arg1 = EVALCAR (x, env);
	    if (SCM_FALSEP (SCM_SUBRF (proc) (arg2, t.arg1)))
	      RETURN (SCM_BOOL_F)
		arg2 = t.arg1;
	    x = SCM_CDR (x);
	  }
	while (SCM_NIMP (x));
	RETURN (SCM_BOOL_T)
#else /* BUILTIN_RPASUBR */
	RETURN (SCM_APPLY (proc, t.arg1,
			   scm_acons (arg2,
				      scm_eval_args (x, env, proc),
				      SCM_EOL)));
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_lsubr_2:
	RETURN (SCM_SUBRF (proc) (t.arg1, arg2, scm_eval_args (x, env, proc)));
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (scm_cons2 (t.arg1,
					     arg2,
					     scm_eval_args (x, env, proc))));
#ifdef CCLO
      case scm_tc7_cclo:
	goto cclon;
#endif
      case scm_tcs_closures:
#ifdef DEVAL
	SCM_SET_ARGSREADY (debug);
#endif
	env = EXTEND_ENV (SCM_CAR (SCM_CODE (proc)),
			      scm_cons2 (t.arg1,
					 arg2,
					 scm_eval_args (x, env, proc)),
			      SCM_ENV (proc));
	x = SCM_CODE (proc);
	goto cdrxbegin;
#endif /* DEVAL */
      case scm_tcs_cons_gloc:
	if (SCM_I_OPERATORP (proc))
	  {
	    SCM p = (SCM_I_ENTITYP (proc)
		     ? SCM_ENTITY_PROC_3 (proc)
		     : SCM_OPERATOR_PROC_3 (proc));
	    if (SCM_NIMP (p))
	      {
		if (SCM_TYP7 (p) == scm_tc7_lsubr_2)
#ifdef DEVAL
		  RETURN (SCM_SUBRF (p) (proc, t.arg1,
					 scm_cons (arg2, SCM_CDDR (debug.info->a.args))))
#else
		  RETURN (SCM_SUBRF (p) (proc, t.arg1,
					 scm_cons (arg2,
						   scm_eval_args (x, env, proc))))
#endif
		else if (SCM_CLOSUREP (p))
		  {
#ifdef DEVAL
		    SCM_SET_ARGSREADY (debug);
		    debug.info->a.args = scm_cons (proc, debug.info->a.args);
		    debug.info->a.proc = p;
		    env = EXTEND_ENV (SCM_CAR (SCM_CODE (p)),
				      scm_cons2 (proc, t.arg1,
						 scm_cons (arg2,
							   SCM_CDDDR (debug.info->a.args))),
				      SCM_ENV (p));
#else
		    env = EXTEND_ENV (SCM_CAR (SCM_CODE (p)),
				      scm_cons2 (proc, t.arg1,
						 scm_cons (arg2,
							   scm_eval_args (x, env, proc))),
				      SCM_ENV (p));
#endif
		    x = SCM_CODE (p);
		    goto cdrxbegin;
		  }
	      }
	    /* Fall through. */
	  }
      case scm_tc7_subr_2:
      case scm_tc7_subr_1o:
      case scm_tc7_subr_2o:
      case scm_tc7_subr_0:
      case scm_tc7_cxr:
      case scm_tc7_subr_1:
      case scm_tc7_contin:
	goto wrongnumargs;
      default:
	goto badfun;
      }
  }
#ifdef DEVAL
exit:
  if (CHECK_EXIT && SCM_TRAPS_P)
    if (SCM_EXIT_FRAME_P || (SCM_TRACE_P && SCM_TRACED_FRAME_P (debug)))
      {
	SCM_CLEAR_TRACED_FRAME (debug);
	if (SCM_CHEAPTRAPS_P)
	  t.arg1 = scm_make_debugobj (&debug);
	else
	  {
	    scm_make_cont (&t.arg1);
	    if (setjmp (SCM_JMPBUF (t.arg1)))
	      {
		proc = SCM_THROW_VALUE (t.arg1);
		goto ret;
	      }
	  }
	scm_ithrow (scm_i_exit_frame, scm_cons2 (t.arg1, proc, SCM_EOL), 0);
      }
ret:
  scm_last_debug_frame = debug.prev;
  return proc;
#endif
}


/* SECTION: This code is compiled once.
 */

#ifndef DEVAL

SCM_PROC(s_procedure_documentation, "procedure-documentation", 1, 0, 0, scm_procedure_documentation);

SCM 
scm_procedure_documentation (proc)
     SCM proc;
{
  SCM code;
  SCM_ASSERT (SCM_BOOL_T == scm_procedure_p (proc) && SCM_NIMP (proc) && SCM_TYP7 (proc) != scm_tc7_contin,
	  proc, SCM_ARG1, s_procedure_documentation);
  switch (SCM_TYP7 (proc))
    {
    case scm_tcs_closures:
      code = SCM_CDR (SCM_CODE (proc));
      if (SCM_IMP (SCM_CDR (code)))
	return SCM_BOOL_F;
      code = SCM_CAR (code);
      if (SCM_IMP (code))
	return SCM_BOOL_F;
      if (SCM_STRINGP (code))
	return code;
    default:
      return SCM_BOOL_F;
/*
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
*/
    }
}

/* This code processes the arguments to apply:

   (apply PROC ARG1 ... ARGS)

   Given a list (ARG1 ... ARGS), this function conses the ARG1
   ... arguments onto the front of ARGS, and returns the resulting
   list.  Note that ARGS is a list; thus, the argument to this
   function is a list whose last element is a list.

   Apply calls this function, and applies PROC to the elements of the
   result.  apply:nconc2last takes care of building the list of
   arguments, given (ARG1 ... ARGS).

   Rather than do new consing, apply:nconc2last destroys its argument.
   On that topic, this code came into my care with the following
   beautifully cryptic comment on that topic: "This will only screw
   you if you do (scm_apply scm_apply '( ... ))"  If you know what
   they're referring to, send me a patch to this comment.  */

SCM_PROC(s_nconc2last, "apply:nconc2last", 1, 0, 0, scm_nconc2last);

SCM 
scm_nconc2last (lst)
     SCM lst;
{
  SCM *lloc;
  SCM_ASSERT (scm_ilength (lst) > 0, lst, SCM_ARG1, s_nconc2last);
  lloc = &lst;
  while (SCM_NNULLP (SCM_CDR (*lloc)))
    lloc = SCM_CDRLOC (*lloc);
  SCM_ASSERT (scm_ilength (SCM_CAR (*lloc)) >= 0, lst, SCM_ARG1, s_nconc2last);
  *lloc = SCM_CAR (*lloc);
  return lst;
}

#endif /* !DEVAL */


/* SECTION: When DEVAL is defined this code yields scm_dapply.
 * It is compiled twice.
 */

#if 0

SCM 
scm_apply (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
{}
#endif

#if 0

SCM 
scm_dapply (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
{}
#endif


/* Apply a function to a list of arguments.

   This function is exported to the Scheme level as taking two
   required arguments and a tail argument, as if it were:
	(lambda (proc arg1 . args) ...)
   Thus, if you just have a list of arguments to pass to a procedure,
   pass the list as ARG1, and '() for ARGS.  If you have some fixed
   args, pass the first as ARG1, then cons any remaining fixed args
   onto the front of your argument list, and pass that as ARGS.  */

SCM 
SCM_APPLY (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
{
#ifdef DEBUG_EXTENSIONS
#ifdef DEVAL
  scm_debug_frame debug;
  scm_debug_info debug_vect_body;
  debug.prev = scm_last_debug_frame;
  debug.status = SCM_APPLYFRAME;
  debug.vect = &debug_vect_body;
  debug.vect[0].a.proc = proc;
  debug.vect[0].a.args = SCM_EOL;
  scm_last_debug_frame = &debug;
#else
  if (SCM_DEBUGGINGP)
    return scm_dapply (proc, arg1, args);
#endif
#endif

  SCM_ASRTGO (SCM_NIMP (proc), badproc);

  /* If ARGS is the empty list, then we're calling apply with only two
     arguments --- ARG1 is the list of arguments for PROC.  Whatever
     the case, futz with things so that ARG1 is the first argument to
     give to PROC (or SCM_UNDEFINED if no args), and ARGS contains the
     rest.

     Setting the debug apply frame args this way is pretty messy.
     Perhaps we should store arg1 and args directly in the frame as
     received, and let scm_frame_arguments unpack them, because that's
     a relatively rare operation.  This works for now; if the Guile
     developer archives are still around, see Mikael's post of
     11-Apr-97.  */
  if (SCM_NULLP (args))
    {
      if (SCM_NULLP (arg1))
	{
	  arg1 = SCM_UNDEFINED;
#ifdef DEVAL
	  debug.vect[0].a.args = SCM_EOL;
#endif
	}
      else
	{
#ifdef DEVAL
	  debug.vect[0].a.args = arg1;
#endif
	  args = SCM_CDR (arg1);
	  arg1 = SCM_CAR (arg1);
	}
    }
  else
    {
      /* SCM_ASRTGO(SCM_NIMP(args) && SCM_CONSP(args), wrongnumargs); */
      args = scm_nconc2last (args);
#ifdef DEVAL
      debug.vect[0].a.args = scm_cons (arg1, args);
#endif
    }
#ifdef DEVAL
  if (SCM_ENTER_FRAME_P && SCM_TRAPS_P)
    {
      SCM tmp;
      if (SCM_CHEAPTRAPS_P)
	tmp = scm_make_debugobj (&debug);
      else
	{
	  scm_make_cont (&tmp);
	  if (setjmp (SCM_JMPBUF (tmp)))
	    goto entap;
	}
      scm_ithrow (scm_i_enter_frame, scm_cons (tmp, SCM_EOL), 0);
    }
entap:
  ENTER_APPLY;
#endif
#ifdef CCLO
tail:
#endif
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2o:
      args = SCM_NULLP (args) ? SCM_UNDEFINED : SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args))
    case scm_tc7_subr_2:
      SCM_ASRTGO (SCM_NNULLP (args) && SCM_NULLP (SCM_CDR (args)),
		  wrongnumargs);
      args = SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args))
    case scm_tc7_subr_0:
      SCM_ASRTGO (SCM_UNBNDP (arg1), wrongnumargs);
      RETURN (SCM_SUBRF (proc) ())
    case scm_tc7_subr_1:
    case scm_tc7_subr_1o:
      SCM_ASRTGO (SCM_NULLP (args), wrongnumargs);
      RETURN (SCM_SUBRF (proc) (arg1))
    case scm_tc7_cxr:
      SCM_ASRTGO (SCM_NULLP (args), wrongnumargs);
#ifdef SCM_FLOATS
      if (SCM_SUBRF (proc))
	{
	  if (SCM_INUMP (arg1))
	    {
	      RETURN (scm_makdbl (SCM_DSUBRF (proc) ((double) SCM_INUM (arg1)), 0.0));
	    }
	  SCM_ASRTGO (SCM_NIMP (arg1), floerr);
	  if (SCM_REALP (arg1))
	    {
	      RETURN (scm_makdbl (SCM_DSUBRF (proc) (SCM_REALPART (arg1)), 0.0));
	    }
#ifdef SCM_BIGDIG
	  if SCM_BIGP
	    (arg1)
	      RETURN (scm_makdbl (SCM_DSUBRF (proc) (scm_big2dbl (arg1)), 0.0))
#endif
	floerr:
	  scm_wta (arg1, (char *) SCM_ARG1, SCM_CHARS (SCM_SNAME (proc)));
	}
#endif
      proc = (SCM) SCM_SNAME (proc);
      {
	char *chrs = SCM_CHARS (proc) + SCM_LENGTH (proc) - 1;
	while ('c' != *--chrs)
	  {
	    SCM_ASSERT (SCM_NIMP (arg1) && SCM_CONSP (arg1),
		    arg1, SCM_ARG1, SCM_CHARS (proc));
	    arg1 = ('a' == *chrs) ? SCM_CAR (arg1) : SCM_CDR (arg1);
	  }
	RETURN (arg1)
      }
    case scm_tc7_subr_3:
      RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CAR (SCM_CDR (args))))
    case scm_tc7_lsubr:
#ifdef DEVAL
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : debug.vect[0].a.args))
#else
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args)))
#endif
    case scm_tc7_lsubr_2:
      SCM_ASRTGO (SCM_NIMP (args) && SCM_CONSP (args), wrongnumargs);
      RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CDR (args)))
    case scm_tc7_asubr:
      if (SCM_NULLP (args))
	RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED))
      while (SCM_NIMP (args))
	{
	  SCM_ASSERT (SCM_CONSP (args), args, SCM_ARG2, "apply");
	  arg1 = SCM_SUBRF (proc) (arg1, SCM_CAR (args));
	  args = SCM_CDR (args);
	}
      RETURN (arg1);
    case scm_tc7_rpsubr:
      if (SCM_NULLP (args))
	RETURN (SCM_BOOL_T);
      while (SCM_NIMP (args))
	{
	  SCM_ASSERT (SCM_CONSP (args), args, SCM_ARG2, "apply");
	  if (SCM_FALSEP (SCM_SUBRF (proc) (arg1, SCM_CAR (args))))
	    RETURN (SCM_BOOL_F);
	  arg1 = SCM_CAR (args);
	  args = SCM_CDR (args);
	}
      RETURN (SCM_BOOL_T);
    case scm_tcs_closures:
#ifdef DEVAL
      arg1 = (SCM_UNBNDP (arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
      arg1 = (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
#ifndef RECKLESS
      if (scm_badargsp (SCM_CAR (SCM_CODE (proc)), arg1))
	goto wrongnumargs;
#endif
      
      /* Copy argument list */
      if (SCM_IMP (arg1))
	args = arg1;
      else
	{
	  SCM tl = args = scm_cons (SCM_CAR (arg1), SCM_UNSPECIFIED);
	  while (SCM_NIMP (arg1 = SCM_CDR (arg1))
		 && SCM_CONSP (arg1))
	    {
	      SCM_SETCDR (tl, scm_cons (SCM_CAR (arg1),
					SCM_UNSPECIFIED));
	      tl = SCM_CDR (tl);
	    }
	  SCM_SETCDR (tl, arg1);
	}
      
      args = EXTEND_ENV (SCM_CAR (SCM_CODE (proc)), args, SCM_ENV (proc));
      proc = SCM_CODE (proc);
      while (SCM_NNULLP (proc = SCM_CDR (proc)))
	arg1 = EVALCAR (proc, args);
      RETURN (arg1);
    case scm_tc7_contin:
      SCM_ASRTGO (SCM_NULLP (args), wrongnumargs);
      scm_call_continuation (proc, arg1);
#ifdef CCLO
    case scm_tc7_cclo:
#ifdef DEVAL
      args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
      arg1 = proc;
      proc = SCM_CCLO_SUBR (proc);
      debug.vect[0].a.proc = proc;
      debug.vect[0].a.args = scm_cons (arg1, args);
#else
      args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
      arg1 = proc;
      proc = SCM_CCLO_SUBR (proc);
#endif
      goto tail;
#endif
    case scm_tcs_cons_gloc:
      if (SCM_I_OPERATORP (proc))
	{
#ifdef DEVAL
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
	  arg1 = proc;
	  proc = (SCM_NULLP (args)
		  ? (SCM_I_ENTITYP (proc)
		     ? SCM_ENTITY_PROC_0 (proc)
		     : SCM_OPERATOR_PROC_0 (proc))
		  : SCM_NULLP (SCM_CDR (args))
		  ? (SCM_I_ENTITYP (proc)
		     ? SCM_ENTITY_PROC_1 (proc)
		     : SCM_OPERATOR_PROC_1 (proc))
		  : SCM_NULLP (SCM_CDDR (args))
		  ? (SCM_I_ENTITYP (proc)
		     ? SCM_ENTITY_PROC_2 (proc)
		     : SCM_OPERATOR_PROC_2 (proc))
		  : (SCM_I_ENTITYP (proc)
		     ? SCM_ENTITY_PROC_3 (proc)
		     : SCM_OPERATOR_PROC_3 (proc)));
#ifdef DEVAL
	  debug.vect[0].a.proc = proc;
	  debug.vect[0].a.args = scm_cons (arg1, args);
#endif
	  goto tail;
	}
    wrongnumargs:
      scm_wrong_num_args (proc);
    default:
    badproc:
      scm_wta (proc, (char *) SCM_ARG1, "apply");
      RETURN (arg1);
    }
#ifdef DEVAL
exit:
  if (CHECK_EXIT && SCM_TRAPS_P)
    if (SCM_EXIT_FRAME_P || (SCM_TRACE_P && SCM_TRACED_FRAME_P (debug)))
      {
	SCM_CLEAR_TRACED_FRAME (debug);
	if (SCM_CHEAPTRAPS_P)
	  arg1 = scm_make_debugobj (&debug);
	else
	  {
	    scm_make_cont (&arg1);
	    if (setjmp (SCM_JMPBUF (arg1)))
	      {
		proc = SCM_THROW_VALUE (arg1);
		goto ret;
	      }
	  }
	scm_ithrow (scm_i_exit_frame, scm_cons2 (arg1, proc, SCM_EOL), 0);
      }
ret:
  scm_last_debug_frame = debug.prev;
  return proc;
#endif
}


/* SECTION: The rest of this file is only read once.
 */

#ifndef DEVAL

SCM_PROC(s_map, "map", 2, 0, 1, scm_map);

SCM 
scm_map (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
{
  long i;
  SCM res = SCM_EOL;
  SCM *pres = &res;
  SCM *ve = &args;		/* Keep args from being optimized away. */

  if (SCM_NULLP (arg1))
    return res;
  SCM_ASSERT (SCM_NIMP (arg1), arg1, SCM_ARG2, s_map);
  if (SCM_NULLP (args))
    {
      while (SCM_NIMP (arg1))
	{
	  SCM_ASSERT (SCM_CONSP (arg1), arg1, SCM_ARG2, s_map);
	  *pres = scm_cons (scm_apply (proc, SCM_CAR (arg1), scm_listofnull), SCM_EOL);
	  pres = SCM_CDRLOC (*pres);
	  arg1 = SCM_CDR (arg1);
	}
      return res;
    }
  args = scm_vector (scm_cons (arg1, args));
  ve = SCM_VELTS (args);
#ifndef RECKLESS
  for (i = SCM_LENGTH (args) - 1; i >= 0; i--)
    SCM_ASSERT (SCM_NIMP (ve[i]) && SCM_CONSP (ve[i]), args, SCM_ARG2, s_map);
#endif
  while (1)
    {
      arg1 = SCM_EOL;
      for (i = SCM_LENGTH (args) - 1; i >= 0; i--)
	{
	  if SCM_IMP
	    (ve[i]) return res;
	  arg1 = scm_cons (SCM_CAR (ve[i]), arg1);
	  ve[i] = SCM_CDR (ve[i]);
	}
      *pres = scm_cons (scm_apply (proc, arg1, SCM_EOL), SCM_EOL);
      pres = SCM_CDRLOC (*pres);
    }
}


SCM_PROC(s_for_each, "for-each", 2, 0, 1, scm_for_each);

SCM 
scm_for_each (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
{
  SCM *ve = &args;		/* Keep args from being optimized away. */
  long i;
  if SCM_NULLP (arg1)
    return SCM_UNSPECIFIED;
  SCM_ASSERT (SCM_NIMP (arg1), arg1, SCM_ARG2, s_for_each);
  if SCM_NULLP (args)
    {
      while SCM_NIMP (arg1)
	{
	  SCM_ASSERT (SCM_CONSP (arg1), arg1, SCM_ARG2, s_for_each);
	  scm_apply (proc, SCM_CAR (arg1), scm_listofnull);
	  arg1 = SCM_CDR (arg1);
	}
      return SCM_UNSPECIFIED;
    }
  args = scm_vector (scm_cons (arg1, args));
  ve = SCM_VELTS (args);
#ifndef RECKLESS
  for (i = SCM_LENGTH (args) - 1; i >= 0; i--)
    SCM_ASSERT (SCM_NIMP (ve[i]) && SCM_CONSP (ve[i]), args, SCM_ARG2, s_for_each);
#endif
  while (1)
    {
      arg1 = SCM_EOL;
      for (i = SCM_LENGTH (args) - 1; i >= 0; i--)
	{
	  if SCM_IMP
	    (ve[i]) return SCM_UNSPECIFIED;
	  arg1 = scm_cons (SCM_CAR (ve[i]), arg1);
	  ve[i] = SCM_CDR (ve[i]);
	}
      scm_apply (proc, arg1, SCM_EOL);
    }
}



SCM 
scm_closure (code, env)
     SCM code;
     SCM env;
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_SETCODE (z, code);
  SCM_SETENV (z, env);
  return z;
}


long scm_tc16_promise;

SCM 
scm_makprom (code)
     SCM code;
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_ENTER_A_SECTION;
  SCM_SETCDR (z, code);
  SCM_SETCAR (z, scm_tc16_promise);
  SCM_EXIT_A_SECTION;
  return z;
}



static int  prinprom SCM_P ((SCM exp, SCM port, scm_print_state *pstate));

static int 
prinprom (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<promise ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (SCM_CDR (exp), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return !0;
}


SCM_PROC(s_makacro, "procedure->syntax", 1, 0, 0, scm_makacro);

SCM 
scm_makacro (code)
     SCM code;
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_SETCDR (z, code);
  SCM_SETCAR (z, scm_tc16_macro);
  return z;
}


SCM_PROC(s_makmacro, "procedure->macro", 1, 0, 0, scm_makmacro);

SCM 
scm_makmacro (code)
     SCM code;
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_SETCDR (z, code);
  SCM_SETCAR (z, scm_tc16_macro | (1L << 16));
  return z;
}


SCM_PROC(s_makmmacro, "procedure->memoizing-macro", 1, 0, 0, scm_makmmacro);

SCM 
scm_makmmacro (code)
     SCM code;
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_SETCDR (z, code);
  SCM_SETCAR (z, scm_tc16_macro | (2L << 16));
  return z;
}


SCM_PROC (s_macro_p, "macro?", 1, 0, 0, scm_macro_p);

SCM
scm_macro_p (obj)
     SCM obj;
{
  return (SCM_NIMP (obj) && SCM_TYP16 (obj) == scm_tc16_macro
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_SYMBOL (scm_sym_syntax, "syntax");
SCM_SYMBOL (scm_sym_macro, "macro");
SCM_SYMBOL (scm_sym_mmacro, "macro!");

SCM_PROC (s_macro_type, "macro-type", 1, 0, 0, scm_macro_type);

SCM
scm_macro_type (m)
     SCM m;
{
  if (!(SCM_NIMP (m) && SCM_TYP16 (m) == scm_tc16_macro))
    return SCM_BOOL_F;
  switch ((int) (SCM_CAR (m) >> 16))
    {
    case 0: return scm_sym_syntax;
    case 1: return scm_sym_macro;
    case 2: return scm_sym_mmacro;
    default: scm_wrong_type_arg (s_macro_type, 1, m);
    }
}


SCM_PROC (s_macro_name, "macro-name", 1, 0, 0, scm_macro_name);

SCM
scm_macro_name (m)
     SCM m;
{
  SCM_ASSERT (SCM_NIMP (m) && SCM_TYP16 (m) == scm_tc16_macro,
	      m,
	      SCM_ARG1,
	      s_macro_name);
  return scm_procedure_name (SCM_CDR (m));
}


SCM_PROC (s_macro_transformer, "macro-transformer", 1, 0, 0, scm_macro_transformer);

SCM
scm_macro_transformer (m)
     SCM m;
{
  SCM_ASSERT (SCM_NIMP (m) && SCM_TYP16 (m) == scm_tc16_macro,
	      m,
	      SCM_ARG1,
	      s_macro_transformer);
  return SCM_CLOSUREP (SCM_CDR (m)) ? SCM_CDR (m) : SCM_BOOL_F;
}



SCM_PROC(s_force, "force", 1, 0, 0, scm_force);

SCM 
scm_force (x)
     SCM x;
{
  SCM_ASSERT (SCM_NIMP(x) && SCM_TYP16 (x) == scm_tc16_promise,
	      x, SCM_ARG1, s_force);
  if (!((1L << 16) & SCM_CAR (x)))
    {
      SCM ans = scm_apply (SCM_CDR (x), SCM_EOL, SCM_EOL);
      if (!((1L << 16) & SCM_CAR (x)))
	{
	  SCM_DEFER_INTS;
	  SCM_SETCDR (x, ans);
	  SCM_SETOR_CAR (x, (1L << 16));
	  SCM_ALLOW_INTS;
	}
    }
  return SCM_CDR (x);
}

SCM_PROC (s_promise_p, "promise?", 1, 0, 0, scm_promise_p);

SCM
scm_promise_p (x)
     SCM x;
{
  return ((SCM_NIMP (x) && (SCM_TYP16 (x) == scm_tc16_promise))
	   ? SCM_BOOL_T
	   : SCM_BOOL_F);
}

SCM_PROC(s_copy_tree, "copy-tree", 1, 0, 0, scm_copy_tree);

SCM 
scm_copy_tree (obj)
     SCM obj;
{
  SCM ans, tl;
  if SCM_IMP
    (obj) return obj;
  if (SCM_VECTORP (obj))
    {
      scm_sizet i = SCM_LENGTH (obj);
      ans = scm_make_vector (SCM_MAKINUM (i), SCM_UNSPECIFIED);
      while (i--)
	SCM_VELTS (ans)[i] = scm_copy_tree (SCM_VELTS (obj)[i]);
      return ans;
    }
  if SCM_NCONSP (obj)
    return obj;
/*  return scm_cons(scm_copy_tree(SCM_CAR(obj)), scm_copy_tree(SCM_CDR(obj))); */
  ans = tl = scm_cons (scm_copy_tree (SCM_CAR (obj)), SCM_UNSPECIFIED);
  while (SCM_NIMP (obj = SCM_CDR (obj)) && SCM_CONSP (obj))
    {
      SCM_SETCDR (tl, scm_cons (scm_copy_tree (SCM_CAR (obj)),
				SCM_UNSPECIFIED));
      tl = SCM_CDR (tl);
    }
  SCM_SETCDR (tl, obj);
  return ans;
}


SCM 
scm_eval_3 (obj, copyp, env)
     SCM obj;
     int copyp;
     SCM env;
{
  if (SCM_NIMP (SCM_CDR (scm_system_transformer)))
    obj = scm_apply (SCM_CDR (scm_system_transformer), obj, scm_listofnull);
  else if (copyp)
    obj = scm_copy_tree (obj);
  return XEVAL (obj, env);
}


SCM
scm_top_level_env (thunk)
     SCM thunk;
{
  if (SCM_IMP(thunk))
    return SCM_EOL;
  else
    return scm_cons(thunk, (SCM)SCM_EOL);
}

SCM_PROC(s_eval2, "eval2", 2, 0, 0, scm_eval2);

SCM
scm_eval2 (obj, env_thunk)
     SCM obj;
     SCM env_thunk;
{
  return scm_eval_3 (obj, 1, scm_top_level_env(env_thunk));
}

SCM_PROC(s_eval, "eval", 1, 0, 0, scm_eval);

SCM
scm_eval (obj)
     SCM obj;
{
  return
    scm_eval_3(obj, 1, scm_top_level_env(SCM_CDR(scm_top_level_lookup_closure_var)));
}

/* SCM_PROC(s_eval_x, "eval!", 1, 0, 0, scm_eval_x); */

SCM
scm_eval_x (obj)
     SCM obj;
{
  return
    scm_eval_3(obj,
	       0,
	       scm_top_level_env (SCM_CDR (scm_top_level_lookup_closure_var)));
}

SCM_PROC (s_definedp, "defined?", 1, 0, 0, scm_definedp);

SCM 
scm_definedp (sym)
     SCM sym;
{
  SCM vcell;

  if (SCM_ISYMP (sym))
    return SCM_BOOL_T;

  SCM_ASSERT (SCM_NIMP (sym) && SCM_SYMBOLP (sym), sym, SCM_ARG1, s_definedp);

  vcell = scm_sym2vcell(sym,
			SCM_CDR (scm_top_level_lookup_closure_var),
			SCM_BOOL_F);
  return (vcell == SCM_BOOL_F || SCM_UNBNDP(SCM_CDR(vcell))) ? 
      SCM_BOOL_F : SCM_BOOL_T;
}

static scm_smobfuns promsmob = {scm_markcdr, scm_free0, prinprom};

static scm_smobfuns macrosmob = {scm_markcdr, scm_free0};

SCM 
scm_make_synt (name, macroizer, fcn)
     char *name;
     SCM (*macroizer) ();
     SCM (*fcn) ();
{
  SCM symcell = scm_sysintern (name, SCM_UNDEFINED);
  long tmp = ((((SCM_CELLPTR) (SCM_CAR (symcell))) - scm_heap_org) << 8);
  register SCM z;
  if ((tmp >> 8) != ((SCM_CELLPTR) (SCM_CAR (symcell)) - scm_heap_org))
    tmp = 0;
  SCM_NEWCELL (z);
  SCM_SUBRF (z) = fcn;
  SCM_SETCAR (z, tmp + scm_tc7_subr_2);
  SCM_SETCDR (symcell, macroizer (z));
  return SCM_CAR (symcell);
}


/* At this point, scm_deval and scm_dapply are generated.
 */

#ifdef DEBUG_EXTENSIONS
# define DEVAL
# include "eval.c"
#endif



void 
scm_init_eval ()
{
  scm_init_opts (scm_evaluator_traps,
		 scm_evaluator_trap_table,
		 SCM_N_EVALUATOR_TRAPS);
  scm_init_opts (scm_eval_options_interface,
		 scm_eval_opts,
		 SCM_N_EVAL_OPTIONS);
  
  scm_tc16_promise = scm_newsmob (&promsmob);
  scm_tc16_macro = scm_newsmob (&macrosmob);
  scm_i_apply = scm_make_subr ("apply", scm_tc7_lsubr_2, scm_apply);
  scm_system_transformer = scm_sysintern ("scm:eval-transformer", SCM_UNDEFINED);
  scm_i_dot = SCM_CAR (scm_sysintern (".", SCM_UNDEFINED));
  scm_i_arrow = SCM_CAR (scm_sysintern ("=>", SCM_UNDEFINED));
  scm_i_else = SCM_CAR (scm_sysintern ("else", SCM_UNDEFINED));
  scm_i_unquote = SCM_CAR (scm_sysintern ("unquote", SCM_UNDEFINED));
  scm_i_uq_splicing = SCM_CAR (scm_sysintern ("unquote-splicing", SCM_UNDEFINED));

  /* acros */
  scm_i_quasiquote = scm_make_synt (s_quasiquote, scm_makacro, scm_m_quasiquote);
  scm_make_synt (s_undefine, scm_makacro, scm_m_undefine);
  scm_make_synt (s_delay, scm_makacro, scm_m_delay);
  /* end of acros */

  scm_top_level_lookup_closure_var =
    scm_sysintern("*top-level-lookup-closure*", SCM_BOOL_F);
  scm_can_use_top_level_lookup_closure_var = 1;

  scm_i_and = scm_make_synt ("and", scm_makmmacro, scm_m_and);
  scm_i_begin = scm_make_synt ("begin", scm_makmmacro, scm_m_begin);
  scm_i_case = scm_make_synt ("case", scm_makmmacro, scm_m_case);
  scm_i_cond = scm_make_synt ("cond", scm_makmmacro, scm_m_cond);
  scm_i_define = scm_make_synt ("define", scm_makmmacro, scm_m_define);
  scm_i_do = scm_make_synt ("do", scm_makmmacro, scm_m_do);
  scm_i_if = scm_make_synt ("if", scm_makmmacro, scm_m_if);
  scm_i_lambda = scm_make_synt ("lambda", scm_makmmacro, scm_m_lambda);
  scm_i_let = scm_make_synt ("let", scm_makmmacro, scm_m_let);
  scm_i_letrec = scm_make_synt ("letrec", scm_makmmacro, scm_m_letrec);
  scm_i_letstar = scm_make_synt ("let*", scm_makmmacro, scm_m_letstar);
  scm_i_or = scm_make_synt ("or", scm_makmmacro, scm_m_or);
  scm_i_quote = scm_make_synt ("quote", scm_makmmacro, scm_m_quote);
  scm_i_set = scm_make_synt ("set!", scm_makmmacro, scm_m_set);
  scm_i_atapply = scm_make_synt ("@apply", scm_makmmacro, scm_m_apply);
  scm_i_atcall_cc = scm_make_synt ("@call-with-current-continuation",
				   scm_makmmacro, scm_m_cont);

#ifdef DEBUG_EXTENSIONS
  scm_i_enter_frame = SCM_CAR (scm_sysintern ("enter-frame", SCM_UNDEFINED));
  scm_i_apply_frame = SCM_CAR (scm_sysintern ("apply-frame", SCM_UNDEFINED));
  scm_i_exit_frame = SCM_CAR (scm_sysintern ("exit-frame", SCM_UNDEFINED));
  scm_i_trace = SCM_CAR (scm_sysintern ("trace", SCM_UNDEFINED));
#endif

#include "eval.x"

  scm_add_feature ("delay");
}

#endif /* !DEVAL */
