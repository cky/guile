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


/* This file is read twice in order to produce a second debugging
 * version of scm_ceval called scm_deval.  scm_deval is produced when
 * we define the preprocessor macro DEVAL.
 */

#ifndef DEVAL

#include <stdio.h>
#include "_scm.h"



#define EVALCELLCAR(x, env) (SCM_SYMBOLP (SCM_CAR(x)) \
			     ? *scm_lookupcar(x, env) \
			     : SCM_CEVAL(SCM_CAR(x), env))

#ifdef MEMOIZE_LOCALS
#define EVALIMP(x, env) (SCM_ILOCP(x)?*scm_ilookup((x), env):x)
#else
#define EVALIMP(x, env) x
#endif
#define EVALCAR(x, env) (SCM_NCELLP(SCM_CAR(x))\
			? (SCM_IMP(SCM_CAR(x)) \
			   ? EVALIMP(SCM_CAR(x), env) \
			   : SCM_GLOC_VAL(SCM_CAR(x))) \
			: EVALCELLCAR(x, env))
#ifdef DEBUG_EXTENSIONS
#define XEVALCAR(x, env) (SCM_NCELLP(SCM_CAR(x)) \
			  ? (SCM_IMP(SCM_CAR(x)) \
			     ? EVALIMP(SCM_CAR(x), env) \
			     : SCM_GLOC_VAL(SCM_CAR(x))) \
			  : (SCM_SYMBOLP(SCM_CAR(x)) \
			     ? *scm_lookupcar(x, env) \
			     : (*scm_ceval_ptr) (SCM_CAR(x), env)))
#else
#define XEVALCAR(x, env) EVALCAR(x, env)
#endif

#define EXTEND_SCM_ENV SCM_EXTEND_SCM_ENV

#ifdef MEMOIZE_LOCALS
#ifdef __STDC__
SCM *
scm_ilookup (SCM iloc, SCM env)
#else
SCM *
scm_ilookup (iloc, env)
     SCM iloc;
     SCM env;
#endif
{
  register int ir = SCM_IFRAME (iloc);
  register SCM er = env;
  for (; 0 != ir; --ir)
    er = SCM_CDR (er);
  er = SCM_CAR (er);
  for (ir = SCM_IDIST (iloc); 0 != ir; --ir)
    er = SCM_CDR (er);
  if (SCM_ICDRP (iloc))
    return &SCM_CDR (er);
  return &SCM_CAR (SCM_CDR (er));
}
#endif

#ifdef __STDC__
SCM *
scm_lookupcar (SCM vloc, SCM genv)
#else
SCM *
scm_lookupcar (vloc, genv)
     SCM vloc;
     SCM genv;
#endif
{
  SCM env = genv;
  register SCM *al, fl, var = SCM_CAR (vloc);
#ifdef MEMOIZE_LOCALS
  register SCM iloc = SCM_ILOC00;
#endif
  for (; SCM_NIMP (env); env = SCM_CDR (env))
    {
      if (SCM_BOOL_T == scm_procedure_p (SCM_CAR (env)))
	break;
      al = &SCM_CAR (env);
      for (fl = SCM_CAR (*al); SCM_NIMP (fl); fl = SCM_CDR (fl))
	{
	  if (SCM_NCONSP (fl))
	      if (fl == var)
	      {
#ifdef MEMOIZE_LOCALS
		SCM_CAR (vloc) = iloc + SCM_ICDR;
#endif
		return &SCM_CDR (*al);
	      }
	    else
	      break;
	  al = &SCM_CDR (*al);
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
	      SCM_CAR (vloc) = iloc;
#endif
	      return &SCM_CAR (*al);
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
      scm_everr (vloc, genv, var,
		 (SCM_NULLP (env)
		  ? "unbound variable: "
		  : "damaged environment"),
		 "");
    }
#endif
  SCM_CAR (vloc) = var + 1;
  /* Except wait...what if the var is not a vcell,
   * but syntax or something....
   */
  return &SCM_CDR (var);
}

#define unmemocar scm_unmemocar
#ifdef __STDC__
SCM 
scm_unmemocar (SCM form, SCM env)
#else
SCM 
scm_unmemocar (form, env)
     SCM form;
     SCM env;
#endif
{
  register int ir;
  SCM c;

  if (SCM_IMP (form))
    return form;
  c = SCM_CAR (form);
  if (1 == (c & 7))
    SCM_CAR (form) = SCM_CAR (c - 1);
#ifdef MEMOIZE_LOCALS
  else if (SCM_ILOCP (c))
    {
      for (ir = SCM_IFRAME (c); ir != 0; --ir)
	env = SCM_CDR (env);
      env = SCM_CAR (SCM_CAR (env));
      for (ir = SCM_IDIST (c); ir != 0; --ir)
	env = SCM_CDR (env);
      SCM_CAR (form) = SCM_ICDRP (c) ? env : SCM_CAR (env);
    }
#endif
  return form;
}

#ifdef __STDC__
SCM
scm_eval_car (SCM pair, SCM env)
#else
SCM
scm_eval_car (pair, env)
     SCM pair;
     SCM env;
#endif
{
  return EVALCAR (pair, env);
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
SCM scm_i_name;
#ifdef DEBUG_EXTENSIONS
static SCM enter_frame_sym, exit_frame_sym;
#endif
static char s_quasiquote[] = "quasiquote";
static char s_delay[] = "delay";

#define ASRTSYNTAX(cond_, msg_) if(!(cond_))scm_wta(xorig, (msg_), what);


#ifdef __STDC__
static void 
bodycheck (SCM xorig, SCM *bodyloc, char *what)
#else
static void 
bodycheck (xorig, bodyloc, what)
     SCM xorig;
     SCM *bodyloc;
     char *what;
#endif
{
  ASRTSYNTAX (scm_ilength (*bodyloc) >= 1, s_expression);
}


#ifdef __STDC__
SCM 
scm_m_quote (SCM xorig, SCM env)
#else
SCM 
scm_m_quote (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, xorig, s_expression, "quote");
  return scm_cons (SCM_IM_QUOTE, SCM_CDR (xorig));
}


#ifdef __STDC__
SCM 
scm_m_begin (SCM xorig, SCM env)
#else
SCM 
scm_m_begin (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  ASSYNT (scm_ilength (SCM_CDR (xorig)) >= 1, xorig, s_expression, "begin");
  return scm_cons (SCM_IM_BEGIN, SCM_CDR (xorig));
}


#ifdef __STDC__
SCM 
scm_m_if (SCM xorig, SCM env)
#else
SCM 
scm_m_if (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  int len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 2 && len <= 3, xorig, s_expression, "if");
  return scm_cons (SCM_IM_IF, SCM_CDR (xorig));
}


#ifdef __STDC__
SCM 
scm_m_set (SCM xorig, SCM env)
#else
SCM 
scm_m_set (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  SCM x;
  int len;

  x = SCM_CDR (xorig);
  len = scm_ilength (x);
  ASSYNT ((len > 0) && !(len & 1), xorig, s_expression, "set!");

  {
    SCM y;
    y = x;
    while (len)
      {
	ASSYNT (SCM_NIMP (SCM_CAR (y)) && SCM_SYMBOLP (SCM_CAR (y)),
		xorig, s_variable, "set!");
	y = SCM_CDR (SCM_CDR (x));
	len -= 2;
      }
  }
  return scm_cons (SCM_IM_SET, x);
}


#if 0
#ifdef __STDC__
SCM 
scm_m_vref (SCM xorig, SCM env)
#else
SCM 
scm_m_vref (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  SCM x = SCM_CDR (xorig);
  ASSYNT (1 == scm_ilength (x), xorig, s_expression, s_vref);
  if (SCM_NIMP(x) && UDSCM_VARIABLEP (SCM_CAR (x)))
    {
      scm_everr (SCM_UNDEFINED, env, SCM_CAR(SCM_CDR(x)), s_variable,
		 "global variable reference");
    }
  ASSYNT (SCM_NIMP(x) && DEFSCM_VARIABLEP (SCM_CAR (x)),
	  xorig, s_variable, s_vref);
  return 
  return scm_cons (IM_VREF, x);
}


#ifdef __STDC__
SCM 
scm_m_vset (SCM xorig, SCM env)
#else
SCM 
scm_m_vset (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  SCM x = SCM_CDR (xorig);
  ASSYNT (3 == scm_ilength (x), xorig, s_expression, s_vset);
  ASSYNT ((   DEFSCM_VARIABLEP (SCM_CAR (x))
	   || UDSCM_VARIABLEP (SCM_CAR (x))),
	  xorig, s_variable, s_vset);
  return scm_cons (IM_VSET, x);
}
#endif 


#ifdef __STDC__
SCM 
scm_m_and (SCM xorig, SCM env)
#else
SCM 
scm_m_and (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  int len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 0, xorig, s_test, "and");
  if (len >= 1)
    return scm_cons (SCM_IM_AND, SCM_CDR (xorig));
  else
    return SCM_BOOL_T;
}


#ifdef __STDC__
SCM 
scm_m_or (SCM xorig, SCM env)
#else
SCM 
scm_m_or (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  int len = scm_ilength (SCM_CDR (xorig));
  ASSYNT (len >= 0, xorig, s_test, "or");
  if (len >= 1)
    return scm_cons (SCM_IM_OR, SCM_CDR (xorig));
  else
    return SCM_BOOL_F;
}


#ifdef __STDC__
SCM 
scm_m_case (SCM xorig, SCM env)
#else
SCM 
scm_m_case (xorig, env)
     SCM xorig;
     SCM env;
#endif
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


#ifdef __STDC__
SCM 
scm_m_cond (SCM xorig, SCM env)
#else
SCM 
scm_m_cond (xorig, env)
     SCM xorig;
     SCM env;
#endif
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
	  SCM_CAR (arg1) = SCM_BOOL_T;
	}
      if (len >= 2 && scm_i_arrow == SCM_CAR (SCM_CDR (arg1)))
	ASSYNT (3 == len && SCM_NIMP (SCM_CAR (SCM_CDR (SCM_CDR (arg1)))),
		xorig, "bad recipient", "cond");
      x = SCM_CDR (x);
    }
  return scm_cons (SCM_IM_COND, SCM_CDR (xorig));
}


#ifdef __STDC__
SCM 
scm_m_lambda (SCM xorig, SCM env)
#else
SCM 
scm_m_lambda (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  SCM proc, x = SCM_CDR (xorig);
  if (scm_ilength (x) < 2)
    goto badforms;
  proc = SCM_CAR (x);
  if SCM_NULLP
    (proc) goto memlambda;
  if SCM_IMP
    (proc) goto badforms;
  if SCM_SYMBOLP
    (proc) goto memlambda;
  if SCM_NCONSP
    (proc) goto badforms;
  while SCM_NIMP
    (proc)
    {
      if SCM_NCONSP
	(proc)
	  if (!SCM_SYMBOLP (proc))
	  goto badforms;
	else
	  goto memlambda;
      if (!(SCM_NIMP (SCM_CAR (proc)) && SCM_SYMBOLP (SCM_CAR (proc))))
	goto badforms;
      proc = SCM_CDR (proc);
    }
  if SCM_NNULLP
    (proc)
  badforms:scm_wta (xorig, s_formals, "lambda");
memlambda:
  bodycheck (xorig, &SCM_CDR (x), "lambda");
  return scm_cons (SCM_IM_LAMBDA, SCM_CDR (xorig));
}


#ifdef __STDC__
SCM 
scm_m_letstar (SCM xorig, SCM env)
#else
SCM 
scm_m_letstar (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  SCM x = SCM_CDR (xorig), arg1, proc, vars = SCM_EOL, *varloc = &vars;
  int len = scm_ilength (x);
  ASSYNT (len >= 2, xorig, s_body, "let*");
  proc = SCM_CAR (x);
  ASSYNT (scm_ilength (proc) >= 0, xorig, s_bindings, "let*");
  while SCM_NIMP
    (proc)
    {
      arg1 = SCM_CAR (proc);
      ASSYNT (2 == scm_ilength (arg1), xorig, s_bindings, "let*");
      ASSYNT (SCM_NIMP (SCM_CAR (arg1)) && SCM_SYMBOLP (SCM_CAR (arg1)), xorig, s_variable, "let*");
      *varloc = scm_cons2 (SCM_CAR (arg1), SCM_CAR (SCM_CDR (arg1)), SCM_EOL);
      varloc = &SCM_CDR (SCM_CDR (*varloc));
      proc = SCM_CDR (proc);
    }
  x = scm_cons (vars, SCM_CDR (x));
  bodycheck (xorig, &SCM_CDR (x), "let*");
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


#ifdef __STDC__
SCM 
scm_m_do (SCM xorig, SCM env)
#else
SCM 
scm_m_do (xorig, env)
     SCM xorig;
     SCM env;
#endif
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
      initloc = &SCM_CDR (*initloc);
      arg1 = SCM_CDR (arg1);
      *steploc = scm_cons (SCM_IMP (arg1) ? SCM_CAR (vars) : SCM_CAR (arg1), SCM_EOL);	/* step */
      steploc = &SCM_CDR (*steploc);
      proc = SCM_CDR (proc);
    }
  x = SCM_CDR (x);
  ASSYNT (scm_ilength (SCM_CAR (x)) >= 1, xorig, s_test, "do");
  x = scm_cons2 (SCM_CAR (x), SCM_CDR (x), steps);
  x = scm_cons2 (vars, inits, x);
  bodycheck (xorig, &SCM_CAR (SCM_CDR (SCM_CDR (x))), "do");
  return scm_cons (SCM_IM_DO, x);
}

/* evalcar is small version of inline EVALCAR when we don't care about speed */
#ifdef __STDC__
static SCM 
evalcar (SCM x, SCM env)
#else
static SCM 
evalcar (x, env)
     SCM x;
     SCM env;
#endif
{
  return XEVALCAR (x, env);
}

#ifdef __STDC__
static SCM 
iqq (SCM form, SCM env, int depth)
#else
static SCM 
iqq (form, env, depth)
     SCM form;
     SCM env;
     int depth;
#endif
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
      /* !!! might need a check here to be sure that form isn't a struct. */
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

#ifdef __STDC__
SCM 
scm_m_quasiquote (SCM xorig, SCM env)
#else
SCM 
scm_m_quasiquote (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  SCM x = SCM_CDR (xorig);
  ASSYNT (scm_ilength (x) == 1, xorig, s_expression, s_quasiquote);
  return iqq (SCM_CAR (x), env, 1);
}

#ifdef __STDC__
SCM 
scm_m_delay (SCM xorig, SCM env)
#else
SCM 
scm_m_delay (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  ASSYNT (scm_ilength (xorig) == 2, xorig, s_expression, s_delay);
  xorig = SCM_CDR (xorig);
  return scm_makprom (scm_closure (scm_cons2 (SCM_EOL, SCM_CAR (xorig), SCM_CDR (xorig)),
				   env));
}

#ifdef __STDC__
static SCM
env_top_level (SCM env)
#else
static SCM
env_top_level (env)
     SCM env;
#endif
{
  while (SCM_NIMP(env))
    {
      if (SCM_BOOL_T == scm_procedure_p (SCM_CAR(env)))
	return SCM_CAR(env);
      env = SCM_CDR (env);
    }
  return SCM_BOOL_F;
}

#ifdef __STDC__
SCM 
scm_m_define (SCM x, SCM env)
#else
SCM 
scm_m_define (x, env)
     SCM x;
     SCM env;
#endif
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
#ifdef DEBUG_EXTENSIONS
      if (RECORD_PROCNAMES && SCM_NIMP (x) && SCM_CLOSUREP (x))
	scm_set_procedure_property_x (x, scm_i_name, proc);
#endif
      SCM_CDR (arg1) = x;
#ifdef SICP
      return scm_cons2 (scm_i_quote, SCM_CAR (arg1), SCM_EOL);
#else
      return SCM_UNSPECIFIED;
#endif
    }
  return scm_cons2 (SCM_IM_DEFINE, proc, x);
}
/* end of acros */

#ifdef __STDC__
SCM 
scm_m_letrec (SCM xorig, SCM env)
#else
SCM 
scm_m_letrec (xorig, env)
     SCM xorig;
     SCM env;
#endif
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
      initloc = &SCM_CDR (*initloc);
    }
  while SCM_NIMP
  (proc = SCM_CDR (proc));
  cdrx = scm_cons2 (vars, inits, SCM_CDR (x));
  bodycheck (xorig, &SCM_CDR (SCM_CDR (cdrx)), what);
  return scm_cons (SCM_IM_LETREC, cdrx);
}

#ifdef __STDC__
SCM 
scm_m_let (SCM xorig, SCM env)
#else
SCM 
scm_m_let (xorig, env)
     SCM xorig;
     SCM env;
#endif
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
      varloc = &SCM_CDR (*varloc);
      *initloc = scm_cons (SCM_CAR (SCM_CDR (arg1)), SCM_EOL);
      initloc = &SCM_CDR (*initloc);
      proc = SCM_CDR (proc);
    }
  return
    scm_m_letrec (scm_cons2 (scm_i_let,
			     scm_cons (scm_cons2 (name, scm_cons2 (scm_i_lambda, vars, SCM_CDR (x)), SCM_EOL), SCM_EOL),
			     scm_acons (name, inits, SCM_EOL)), 	/* body */
		  env);
}


#ifdef __STDC__
SCM 
scm_m_apply (SCM xorig, SCM env)
#else
SCM 
scm_m_apply (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 2, xorig, s_expression, "@apply");
  return scm_cons (SCM_IM_APPLY, SCM_CDR (xorig));
}

#define s_atcall_cc (SCM_ISYMSCM_CHARS(SCM_IM_CONT)+1)

#ifdef __STDC__
SCM 
scm_m_cont (SCM xorig, SCM env)
#else
SCM 
scm_m_cont (xorig, env)
     SCM xorig;
     SCM env;
#endif
{
  ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, xorig, s_expression, "@call-with-current-continuation");
  return scm_cons (SCM_IM_CONT, SCM_CDR (xorig));
}

#ifndef RECKLESS
#ifdef __STDC__
int 
scm_badargsp (SCM formals, SCM args)
#else
int 
scm_badargsp (formals, args)
     SCM formals;
     SCM args;
#endif
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

#endif /* !DEVAL */

#ifdef DEVAL
#undef SCM_EVAL_ARGS
#define SCM_EVAL_ARGS scm_deval_args
#undef SCM_CEVAL
#define SCM_CEVAL scm_deval	/* Substitute all uses of scm_ceval */
#undef SCM_APPLY
#define SCM_APPLY scm_dapply
#undef RETURN
#define RETURN(e) {proc = (e); goto exit;}
#else
#define SCM_EVAL_ARGS scm_eval_args
#define RETURN(x) return x;
#endif

SCM 
SCM_EVAL_ARGS (l, env)
     SCM l, env;
{
  SCM res = SCM_EOL, *lloc = &res;
  while (SCM_NIMP (l))
    {
      *lloc = scm_cons (EVALCAR (l, env), SCM_EOL);
      lloc = &SCM_CDR (*lloc);
      l = SCM_CDR (l);
    }
  return res;
}

#if 0
#ifdef __STDC__
SCM 
scm_ceval (SCM x, SCM env)
#else
SCM 
scm_ceval (x, env)
     SCM x;
     SCM env;
#endif
{}
#endif
#if 0
#ifdef __STDC__
SCM 
scm_deval (SCM x, SCM env)
#else
SCM 
scm_deval (x, env)
     SCM x;
     SCM env;
#endif
{}
#endif

#ifdef SCM_FLOATS
#define CHECK_EQVISH(A,B) 	(((A) == (B)) || (SCM_NFALSEP (scm_eqv_p ((A), (B)))))
#else
#define CHECK_EQVISH(A,B) 	((A) == (B))
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
  SCM proc;
  SCM arg2;

  SCM_CHECK_STACK;

 loop:
  SCM_ASYNC_TICK;

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
      goto carloop;

    case (127 & SCM_IM_BEGIN):

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
	  RETURN (SCM_IMP (x) ? EVALIMP (x, env) : SCM_GLOC_VAL (x));
	}

      if (SCM_SYMBOLP (SCM_CAR (x)))
	{
	retval:
	  RETURN (*scm_lookupcar (x, env));
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
	      goto begin;
	    }
	  proc = SCM_CAR (proc);
	  while (SCM_NIMP (proc))
	    {
	      if (CHECK_EQVISH (SCM_CAR (proc), t.arg1))
		{
		  x = SCM_CDR (SCM_CAR (x));
		  goto begin;
		}
	      proc = SCM_CDR (proc);
	    }
	}
      RETURN (SCM_UNSPECIFIED);


    case (127 & SCM_IM_COND):
      while (SCM_NIMP (x = SCM_CDR (x)))
	{
	  proc = SCM_CAR (x);
	  t.arg1 = EVALCAR (proc, env);
	  if (SCM_NFALSEP (t.arg1))
	    {
	      x = SCM_CDR (proc);
	      if (SCM_NULLP (x))
		{
		  RETURN (t.arg1);
		}
	      if (scm_i_arrow != SCM_CAR (x))
		goto begin;
	      proc = SCM_CDR (x);
	      proc = EVALCAR (proc, env);
	      SCM_ASRTGO (SCM_NIMP (proc), badfun);
	      goto evap1;
	    }
	}
      RETURN (SCM_UNSPECIFIED);


    case (127 & SCM_IM_DO):
      x = SCM_CDR (x);
      proc = SCM_CAR (SCM_CDR (x)); /* inits */
      t.arg1 = SCM_EOL;		/* values */
      while (SCM_NIMP (proc))
	{
	  t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1);
	  proc = SCM_CDR (proc);
	}
      env = EXTEND_SCM_ENV (SCM_CAR (x), t.arg1, env);
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
	  env = EXTEND_SCM_ENV (SCM_CAR (SCM_CAR (env)), t.arg1, SCM_CDR (env));
	}
      x = SCM_CDR (proc);
      if (SCM_NULLP (x))
	{
	  RETURN (SCM_UNSPECIFIED);
	}
      goto begin;


    case (127 & SCM_IM_IF):
      x = SCM_CDR (x);
      if (SCM_NFALSEP (EVALCAR (x, env)))
	x = SCM_CDR (x);
      else if (SCM_IMP (x = SCM_CDR (SCM_CDR (x))))
	{
	  RETURN (SCM_UNSPECIFIED);
	}
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
      env = EXTEND_SCM_ENV (SCM_CAR (x), t.arg1, env);
      x = SCM_CDR (x);
      goto cdrxbegin;


    case (127 & SCM_IM_LETREC):
      x = SCM_CDR (x);
      env = EXTEND_SCM_ENV (SCM_CAR (x), scm_undefineds, env);
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      t.arg1 = SCM_EOL;
      do
	{
	  t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1);
	}
      while (SCM_NIMP (proc = SCM_CDR (proc)));
      SCM_CDR (SCM_CAR (env)) = t.arg1;
      goto cdrxbegin;


    case (127 & SCM_IM_LETSTAR):
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      if (SCM_IMP (proc))
	{
	  env = EXTEND_SCM_ENV (SCM_EOL, SCM_EOL, env);
	  goto cdrxbegin;
	}
      do
	{
	  t.arg1 = SCM_CAR (proc);
	  proc = SCM_CDR (proc);
	  env = EXTEND_SCM_ENV (t.arg1, EVALCAR (proc, env), env);
	}
      while (SCM_NIMP (proc = SCM_CDR (proc)));
      goto cdrxbegin;

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
      goto carloop;


    case (127 & SCM_IM_LAMBDA):
      RETURN (scm_closure (SCM_CDR (x), env));


    case (127 & SCM_IM_QUOTE):
      RETURN (SCM_CAR (SCM_CDR (x)));


    case (127 & SCM_IM_SET):
    set_some_more:
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      switch (7 & (int)proc)
	{
	case 0:
	  t.lloc = scm_lookupcar (x, env);
	  break;
	case 1:
	  t.lloc = &SCM_GLOC_VAL (proc);
	  break;
#ifdef MEMOIZE_LOCALS
	case 4:
	  t.lloc = scm_ilookup (proc, env);
	  break;
#endif
	}
      x = SCM_CDR (x);
      *t.lloc = EVALCAR (x, env);
      if (!SCM_NULLP (SCM_CDR(x)))
	goto set_some_more;
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
      env = SCM_CAR (env);
      SCM_DEFER_INTS;
      SCM_CAR (env) = scm_cons (proc, SCM_CAR (env));
      SCM_CDR (env) = scm_cons (x, SCM_CDR (env));
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
	  RETURN (SCM_UNSPECIFIED);
#endif

	case (SCM_ISYMNUM (SCM_IM_APPLY)):
	  proc = SCM_CDR (x);
	  proc = EVALCAR (proc, env);
	  SCM_ASRTGO (SCM_NIMP (proc), badfun);
	  if (SCM_CLOSUREP (proc))
	    {
	      t.arg1 = SCM_CDR (SCM_CDR (x));
	      t.arg1 = EVALCAR (t.arg1, env);
#ifndef RECKLESS
	      if (scm_badargsp (SCM_CAR (SCM_CODE (proc)), t.arg1))
		goto wrongnumargs;
#endif
	      env = EXTEND_SCM_ENV (SCM_CAR (SCM_CODE (proc)), t.arg1, SCM_ENV (proc));
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
	      RETURN (val);;
	    }
	  proc = SCM_CDR (x);
	  proc = evalcar (proc, env);
	  SCM_ASRTGO (SCM_NIMP (proc), badfun);
	  goto evap1;

	default:
	  goto badfun;
	}

    default:
      proc = x;
    badfun:
      scm_everr (x, env, proc, "Wrong type to apply: ", "");

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
    case scm_tc7_mb_string:
    case scm_tc7_substring:
    case scm_tc7_mb_substring:
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
	  proc = *scm_lookupcar (x, env);
	  if (SCM_IMP (proc))
	    {
	      unmemocar (x, env);
	      goto badfun;
	    }
	  if (scm_tc16_macro == SCM_TYP16 (proc))
	    {
	      unmemocar (x, env);

	    handle_a_macro:
	      t.arg1 = SCM_APPLY (SCM_CDR (proc), x, scm_cons (env, scm_listofnull));
	      switch ((int) (SCM_CAR (proc) >> 16))
		{
		case 2:
		  if (scm_ilength (t.arg1) <= 0)
		    t.arg1 = scm_cons2 (SCM_IM_BEGIN, t.arg1, SCM_EOL);
		  SCM_DEFER_INTS;
		  SCM_CAR (x) = SCM_CAR (t.arg1);
		  SCM_CDR (x) = SCM_CDR (t.arg1);
		  SCM_ALLOW_INTS;
		  goto loop;
		case 1:
		  if (SCM_NIMP (x = t.arg1))
		    goto loop;
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
  if (SCM_NULLP (SCM_CDR (x)))
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
      case scm_tc7_cclo:
	t.arg1 = proc;
	proc = SCM_CCLO_SUBR (proc);
	goto evap1;
      case scm_tcs_closures:
	x = SCM_CODE (proc);
	env = EXTEND_SCM_ENV (SCM_CAR (x), SCM_EOL, SCM_ENV (proc));
	goto cdrxbegin;
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
	scm_everr (x, env, proc, (char *) SCM_WNA, "");
      default:
	/* handle macros here */
	goto badfun;
      }


  /* must handle macros by here */
  x = SCM_CDR (x);
#ifdef CAUTIOUS
  if (SCM_IMP (x))
    goto wrongnumargs;
#endif
  t.arg1 = EVALCAR (x, env);
  x = SCM_CDR (x);
  if (SCM_NULLP (x))
    {
    evap1:
      switch (SCM_TYP7 (proc))
	{			/* have one argument in t.arg1 */
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
	  RETURN (SCM_SUBRF (proc) (dbg_info.args));
#else
	  RETURN (SCM_SUBRF (proc) (scm_cons (t.arg1, SCM_EOL)));
#endif
	case scm_tc7_cclo:
	  arg2 = t.arg1;
	  t.arg1 = proc;
	  proc = SCM_CCLO_SUBR (proc);
	  goto evap2;
	case scm_tcs_closures:
	  x = SCM_CODE (proc);
#ifdef DEVAL
	  env = EXTEND_SCM_ENV (SCM_CAR (x), dbg_info.args, SCM_ENV (proc));
#else
	  env = EXTEND_SCM_ENV (SCM_CAR (x), scm_cons (t.arg1, SCM_EOL), SCM_ENV (proc));
#endif
	  goto cdrxbegin;
	case scm_tc7_contin:
	  scm_call_continuation (proc, t.arg1);
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
#endif
  {				/* have two or more arguments */
    arg2 = EVALCAR (x, env);
    x = SCM_CDR (x);
    if (SCM_NULLP (x)) {
#ifdef CCLO
    evap2:
#endif
#ifdef DEVAL
      dbg_info.args = scm_cons2 (t.arg1, arg2, SCM_EOL);
#endif
      switch SCM_TYP7
	(proc)
	  {			/* have two arguments */
	  case scm_tc7_subr_2:
	  case scm_tc7_subr_2o:
	    RETURN (SCM_SUBRF (proc) (t.arg1, arg2));
	  case scm_tc7_lsubr:
#ifdef DEVAL
	    RETURN (SCM_SUBRF (proc) (dbg_info.args));
#else
	    RETURN (SCM_SUBRF (proc) (scm_cons2 (t.arg1, arg2, SCM_EOL)));
#endif
	  case scm_tc7_lsubr_2:
	    RETURN (SCM_SUBRF (proc) (t.arg1, arg2, SCM_EOL));
	  case scm_tc7_rpsubr:
	  case scm_tc7_asubr:
	    RETURN (SCM_SUBRF (proc) (t.arg1, arg2));
#ifdef CCLO
	  cclon: case scm_tc7_cclo:
	    RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc), proc,
			       scm_cons2 (t.arg1, arg2, scm_cons (SCM_EVAL_ARGS (x, env), SCM_EOL))));
	    /*    case scm_tc7_cclo:
		  x = scm_cons(arg2, scm_eval_args(x, env));
		  arg2 = t.arg1;
		  t.arg1 = proc;
		  proc = SCM_CCLO_SUBR(proc);
		  goto evap3; */
#endif
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
#ifdef DEVAL
	    env = EXTEND_SCM_ENV (SCM_CAR (SCM_CODE (proc)), dbg_info.args, SCM_ENV (proc));
#else
	    env = EXTEND_SCM_ENV (SCM_CAR (SCM_CODE (proc)), scm_cons2 (t.arg1, arg2, SCM_EOL), SCM_ENV (proc));
#endif
	    x = SCM_CODE (proc);
	    goto cdrxbegin;
	  }
    }
#ifdef DEVAL
    dbg_info.args = scm_cons2 (t.arg1, arg2, scm_deval_args (x, env));
#endif
    switch SCM_TYP7
      (proc)
	{			/* have 3 or more arguments */
#ifdef DEVAL
	case scm_tc7_subr_3:
	  SCM_ASRTGO (SCM_NULLP (SCM_CDR (x)), wrongnumargs);
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2, SCM_CAR (SCM_CDR (SCM_CDR (dbg_info.args)))));
	case scm_tc7_asubr:
	  /*      t.arg1 = SCM_SUBRF(proc)(t.arg1, arg2);
		  while SCM_NIMP(x) {
		  t.arg1 = SCM_SUBRF(proc)(t.arg1, EVALCAR(x, env));
		  x = SCM_CDR(x);
		  }
		  RETURN (t.arg1) */
	case scm_tc7_rpsubr:
	  RETURN (SCM_APPLY (proc, t.arg1, scm_acons (arg2, SCM_CDR (SCM_CDR (dbg_info.args)), SCM_EOL)));
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2, SCM_CDR (SCM_CDR (dbg_info.args))));
	case scm_tc7_lsubr:
	  RETURN (SCM_SUBRF (proc) (dbg_info.args));
#ifdef CCLO
	case scm_tc7_cclo:
	  goto cclon;
#endif
	case scm_tcs_closures:
	  env = EXTEND_SCM_ENV (SCM_CAR (SCM_CODE (proc)),
				dbg_info.args,
				SCM_ENV (proc));
	  x = SCM_CODE (proc);
	  goto cdrxbegin;
#else
	case scm_tc7_subr_3:
	  SCM_ASRTGO (SCM_NULLP (SCM_CDR (x)), wrongnumargs);
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2, EVALCAR (x, env)));
	case scm_tc7_asubr:
	  /*      t.arg1 = SCM_SUBRF(proc)(t.arg1, arg2);
		  while SCM_NIMP(x) {
		  t.arg1 = SCM_SUBRF(proc)(t.arg1, EVALCAR(x, env));
		  x = SCM_CDR(x);
		  }
		  RETURN (t.arg1) */
	case scm_tc7_rpsubr:
	  RETURN (SCM_APPLY (proc, t.arg1, scm_acons (arg2, scm_eval_args (x, env), SCM_EOL)));
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2, scm_eval_args (x, env)));
	case scm_tc7_lsubr:
	  RETURN (SCM_SUBRF (proc) (scm_cons2 (t.arg1, arg2, scm_eval_args (x, env))));
#ifdef CCLO
	case scm_tc7_cclo:
	  goto cclon;
#endif
	case scm_tcs_closures:
	  env = EXTEND_SCM_ENV (SCM_CAR (SCM_CODE (proc)),
				scm_cons2 (t.arg1, arg2, scm_eval_args (x, env)),
				SCM_ENV (proc));
	  x = SCM_CODE (proc);
	  goto cdrxbegin;
#endif /* DEVAL */
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
  if (CHECK_SCM_EXIT)
    {
      /* if (SINGLE_STEP) ... but this is always fulfilled. */
      SINGLE_STEP = 0;
      scm_make_cont (&t.arg1);
      if (setjmp (SCM_JMPBUF (t.arg1)))
	{
	  proc = SCM_THROW_VALUE(t.arg1);
	  goto ret;
	}
      scm_ithrow (exit_frame_sym, proc, 0);
    }
 ret:
  last_debug_info_frame = dbg_info.prev;
  return proc;
#endif
}

#ifndef DEVAL

SCM_PROC(s_procedure_documentation, "procedure-documentation", 1, 0, 0, scm_procedure_documentation);
#ifdef __STDC__
SCM 
scm_procedure_documentation (SCM proc)
#else
SCM 
scm_procedure_documentation (proc)
     SCM proc;
#endif
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

/* This code is for scm_apply. it is destructive on multiple args.
 * This will only screw you if you do (scm_apply scm_apply '( ... )) 
 */
SCM_PROC(s_nconc2last, "apply:nconc2last", 1, 0, 0, scm_nconc2last);
#ifdef __STDC__
SCM 
scm_nconc2last (SCM lst)
#else
SCM 
scm_nconc2last (lst)
     SCM lst;
#endif
{
  SCM *lloc;
  if (SCM_EOL == lst)
    return lst;
  SCM_ASSERT (SCM_NIMP (lst) && SCM_CONSP (lst), lst, SCM_ARG1, s_nconc2last);
  lloc = &lst;
  while (SCM_NNULLP (SCM_CDR (*lloc)))
    {
      lloc = &SCM_CDR (*lloc);
      SCM_ASSERT (SCM_NIMP (*lloc) && SCM_CONSP (*lloc), lst, SCM_ARG1, s_nconc2last);
    }
  *lloc = SCM_CAR (*lloc);
  return lst;
}

#endif /* !DEVAL */

#if 0
#ifdef __STDC__
SCM 
scm_apply (SCM proc, SCM arg1, SCM args)
#else
SCM 
scm_apply (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
#endif
{}
#endif

#if 0
#ifdef __STDC__
SCM 
scm_dapply (SCM proc, SCM arg1, SCM args)
#else
SCM 
scm_dapply (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
#endif
{}
#endif



#ifdef __STDC__
SCM 
SCM_APPLY (SCM proc, SCM arg1, SCM args)
#else
SCM 
SCM_APPLY (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
#endif
{
#ifdef DEBUG_EXTENSIONS
#ifdef DEVAL
  debug_info dbg_info;
  dbg_info.prev = last_debug_info_frame;
  dbg_info.exp = SCM_UNDEFINED;
  dbg_info.proc = proc;
  dbg_info.args = SCM_UNDEFINED;
  last_debug_info_frame = &dbg_info;
#else
  if (DEBUGGINGP)
    return scm_dapply (proc, arg1, args);
#endif
#endif

  SCM_ASRTGO (SCM_NIMP (proc), badproc);
  if (SCM_NULLP (args))
    {
      if (SCM_NULLP (arg1))
	arg1 = SCM_UNDEFINED;
      else
	{
	  args = SCM_CDR (arg1);
	  arg1 = SCM_CAR (arg1);
	}
    }
  else
    {
      /*		SCM_ASRTGO(SCM_NIMP(args) && SCM_CONSP(args), wrongnumargs); */
      args = scm_nconc2last (args);
    }
#ifdef CCLO
 tail:
#endif
#ifdef DEVAL
  dbg_info.args = scm_cons (arg1, args);
#endif
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2o:
      args = SCM_NULLP (args) ? SCM_UNDEFINED : SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args))
    case scm_tc7_subr_2:
      SCM_ASRTGO (SCM_NULLP (SCM_CDR (args)), wrongnumargs);
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
	  if SCM_INUMP
	    (arg1)
	      RETURN (scm_makdbl (SCM_DSUBRF (proc) ((double) SCM_INUM (arg1)), 0.0))
	  SCM_ASRTGO (SCM_NIMP (arg1), floerr);
	  if SCM_REALP
	    (arg1)
	      RETURN (scm_makdbl (SCM_DSUBRF (proc) (SCM_REALPART (arg1)), 0.0))
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
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : dbg_info.args))
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
      arg1 = (SCM_UNBNDP (arg1) ? SCM_EOL : dbg_info.args);
#else
      arg1 = (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
#ifndef RECKLESS
      if (scm_badargsp (SCM_CAR (SCM_CODE (proc)), arg1))
	goto wrongnumargs;
#endif
      args = EXTEND_SCM_ENV (SCM_CAR (SCM_CODE (proc)), arg1, SCM_ENV (proc));
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
      args = (SCM_UNBNDP(arg1) ? SCM_EOL : dbg_info.args);
#else
      args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
      arg1 = proc;
      proc = SCM_CCLO_SUBR (proc);
      goto tail;
#endif
    wrongnumargs:
      scm_wta (proc, (char *) SCM_WNA, "apply");
    default:
    badproc:
      scm_wta (proc, (char *) SCM_ARG1, "apply");
      RETURN (arg1);
    }
#ifdef DEVAL
 exit:
  if (CHECK_SCM_EXIT)
    {
      /* if (SINGLE_STEP) ... but this is always fulfilled. */
      SINGLE_STEP = 0;
      scm_make_cont (&arg1);
      if (setjmp (SCM_JMPBUF (arg1)))
	{
	  proc = SCM_THROW_VALUE(arg1);
	  goto ret;
	}
      scm_ithrow (exit_frame_sym, proc, 0);
    }
 ret:
  last_debug_info_frame = dbg_info.prev;
  return proc;
#endif
}

#ifndef DEVAL

SCM_PROC(s_map, "map", 2, 0, 1, scm_map);
#ifdef __STDC__
SCM 
scm_map (SCM proc, SCM arg1, SCM args)
#else
SCM 
scm_map (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
#endif
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
	  pres = &SCM_CDR (*pres);
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
      pres = &SCM_CDR (*pres);
    }
}


SCM_PROC(s_for_each, "for-each", 2, 0, 1, scm_for_each);
#ifdef __STDC__
SCM 
scm_for_each (SCM proc, SCM arg1, SCM args)
#else
SCM 
scm_for_each (proc, arg1, args)
     SCM proc;
     SCM arg1;
     SCM args;
#endif
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


#ifdef __STDC__
SCM 
scm_closure (SCM code, SCM env)
#else
SCM 
scm_closure (code, env)
     SCM code;
     SCM env;
#endif
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_SETCODE (z, code);
  SCM_ENV (z) = env;
  return z;
}


long scm_tc16_promise;
#ifdef __STDC__
SCM 
scm_makprom (SCM code)
#else
SCM 
scm_makprom (code)
     SCM code;
#endif
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_CDR (z) = code;
  SCM_CAR (z) = scm_tc16_promise;
  return z;
}


#ifdef __STDC__
static int 
prinprom (SCM exp, SCM port, int writing)
#else
static int 
prinprom (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
  scm_gen_puts (scm_regular_string, "#<promise ", port);
  scm_iprin1 (SCM_CDR (exp), port, writing);
  scm_gen_putc ('>', port);
  return !0;
}


SCM_PROC(s_makacro, "procedure->syntax", 1, 0, 0, scm_makacro);
#ifdef __STDC__
SCM 
scm_makacro (SCM code)
#else
SCM 
scm_makacro (code)
     SCM code;
#endif
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_CDR (z) = code;
  SCM_CAR (z) = scm_tc16_macro;
  return z;
}


SCM_PROC(s_makmacro, "procedure->macro", 1, 0, 0, scm_makmacro);
#ifdef __STDC__
SCM 
scm_makmacro (SCM code)
#else
SCM 
scm_makmacro (code)
     SCM code;
#endif
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_CDR (z) = code;
  SCM_CAR (z) = scm_tc16_macro | (1L << 16);
  return z;
}


SCM_PROC(s_makmmacro, "procedure->memoizing-macro", 1, 0, 0, scm_makmmacro);
#ifdef __STDC__
SCM 
scm_makmmacro (SCM code)
#else
SCM 
scm_makmmacro (code)
     SCM code;
#endif
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_CDR (z) = code;
  SCM_CAR (z) = scm_tc16_macro | (2L << 16);
  return z;
}


#ifdef __STDC__
static int 
prinmacro (SCM exp, SCM port, int writing)
#else
static int 
prinmacro (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
  if (SCM_CAR (exp) & (3L << 16))
    scm_gen_puts (scm_regular_string, "#<macro", port);
  else
    scm_gen_puts (scm_regular_string, "#<syntax", port);
  if (SCM_CAR (exp) & (2L << 16))
    scm_gen_putc ('!', port);
  scm_gen_putc (' ', port);
  scm_iprin1 (SCM_CDR (exp), port, writing);
  scm_gen_putc ('>', port);
  return !0;
}

SCM_PROC(s_force, "force", 1, 0, 0, scm_force);
#ifdef __STDC__
SCM 
scm_force (SCM x)
#else
SCM 
scm_force (x)
     SCM x;
#endif
{
  SCM_ASSERT ((SCM_TYP16 (x) == scm_tc16_promise), x, SCM_ARG1, s_force);
  if (!((1L << 16) & SCM_CAR (x)))
    {
      SCM ans = scm_apply (SCM_CDR (x), SCM_EOL, SCM_EOL);
      if (!((1L << 16) & SCM_CAR (x)))
	{
	  SCM_DEFER_INTS;
	  SCM_CDR (x) = ans;
	  SCM_CAR (x) |= (1L << 16);
	  SCM_ALLOW_INTS;
	}
    }
  return SCM_CDR (x);
}

SCM_PROC (s_promise_p, "promise?", 1, 0, 0, scm_promise_p);
#ifdef __STDC__
SCM
scm_promise_p (SCM x)
#else
SCM
scm_promise_p (x)
     SCM x;
#endif
{
  return ((SCM_NIMP (x) && (SCM_TYP16 (x) == scm_tc16_promise))
	   ? SCM_BOOL_T
	   : SCM_BOOL_F);
}

SCM_PROC(s_copy_tree, "copy-tree", 1, 0, 0, scm_copy_tree);
#ifdef __STDC__
SCM 
scm_copy_tree (SCM obj)
#else
SCM 
scm_copy_tree (obj)
     SCM obj;
#endif
{
  SCM ans, tl;
  if SCM_IMP
    (obj) return obj;
  if (SCM_VECTORP (obj))
    {
      scm_sizet i = SCM_LENGTH (obj);
      ans = scm_make_vector (SCM_MAKINUM (i), SCM_UNSPECIFIED, SCM_UNDEFINED);
      while (i--)
	SCM_VELTS (ans)[i] = scm_copy_tree (SCM_VELTS (obj)[i]);
      return ans;
    }
  if SCM_NCONSP (obj)
    return obj;
/*  return scm_cons(scm_copy_tree(SCM_CAR(obj)), scm_copy_tree(SCM_CDR(obj))); */
  ans = tl = scm_cons (scm_copy_tree (SCM_CAR (obj)), SCM_UNSPECIFIED);
  while (SCM_NIMP (obj = SCM_CDR (obj)) && SCM_CONSP (obj))
    tl = (SCM_CDR (tl) = scm_cons (scm_copy_tree (SCM_CAR (obj)), SCM_UNSPECIFIED));
  SCM_CDR (tl) = obj;
  return ans;
}

#ifdef __STDC__
SCM 
scm_eval_3 (SCM obj, int copyp, SCM env)
#else
SCM 
scm_eval_3 (obj, copyp, env)
     SCM obj;
     int copyp;
     SCM env;
#endif
{
  if (SCM_NIMP (SCM_CDR (scm_system_transformer)))
    obj = scm_apply (SCM_CDR (scm_system_transformer), obj, scm_listofnull);
  else if (copyp)
    obj = scm_copy_tree (obj);
  return EVAL (obj, env);
}

#ifdef __STDC__
SCM
scm_top_level_env (SCM thunk)
#else
SCM
scm_top_level_env (thunk)
     SCM thunk;
#endif
{
  if (SCM_IMP(thunk))
    return SCM_EOL;
  else
    return scm_cons(thunk, (SCM)SCM_EOL);
}

SCM_PROC(s_eval2, "eval2", 2, 0, 0, scm_eval2);
#ifdef __STDC__
SCM
scm_eval2 (SCM obj, SCM env_thunk)
#else
SCM
scm_eval2 (obj, env_thunk)
     SCM obj;
     SCM env_thunk;
#endif
{
  return scm_eval_3 (obj, 1, scm_top_level_env(env_thunk));
}

SCM_PROC(s_eval, "eval", 1, 0, 0, scm_eval);
#ifdef __STDC__
SCM
scm_eval (SCM obj)
#else
SCM
scm_eval (obj)
     SCM obj;
#endif
{
  return
    scm_eval_3(obj, 1, scm_top_level_env(SCM_CDR(scm_top_level_lookup_thunk_var)));
}

SCM_PROC(s_eval_x, "eval!", 1, 0, 0, scm_eval_x);
#ifdef __STDC__
SCM
scm_eval_x (SCM obj)
#else
SCM
scm_eval_x (obj)
     SCM obj;
#endif
{
  return
    scm_eval_3(obj,
	       0,
	       scm_top_level_env (SCM_CDR (scm_top_level_lookup_thunk_var)));
}

SCM_PROC (s_macro_eval_x, "macro-eval!", 2, 0, 0, scm_macro_eval_x);
#ifdef __STDC__
SCM
scm_macro_eval_x (SCM exp, SCM env)
#else
SCM
scm_macro_eval_x (exp, env)
     SCM exp;
     SCM env;
#endif
{
  return scm_eval_3 (exp, 0, env);
}

#ifdef __STDC__
SCM 
scm_definedp (SCM x, SCM env)
#else
SCM 
scm_definedp (x, env)
     SCM x;
     SCM env;
#endif
{
  SCM proc = SCM_CAR (x = SCM_CDR (x));
  if (SCM_ISYMP (proc))
    return SCM_BOOL_T;
  else if(SCM_IMP(proc) || !SCM_SYMBOLP(proc))
    return SCM_BOOL_F;
  else
    {
      SCM vcell = scm_sym2vcell(proc, env_top_level(env), SCM_BOOL_F);
      return (vcell == SCM_BOOL_F || SCM_UNBNDP(SCM_CDR(vcell))) ? SCM_BOOL_F : SCM_BOOL_T;
    }
}

static scm_smobfuns promsmob =
{scm_markcdr, scm_free0, prinprom};

static scm_smobfuns macrosmob =
{scm_markcdr, scm_free0, prinmacro};

#ifdef __STDC__
SCM 
scm_make_synt (char *name, SCM (*macroizer) (), SCM (*fcn) ())
#else
SCM 
scm_make_synt (name, macroizer, fcn)
     char *name;
     SCM (*macroizer) ();
     SCM (*fcn) ();
#endif
{
  SCM symcell = scm_sysintern (name, SCM_UNDEFINED);
  long tmp = ((((SCM_CELLPTR) (SCM_CAR (symcell))) - scm_heap_org) << 8);
  register SCM z;
  if ((tmp >> 8) != ((SCM_CELLPTR) (SCM_CAR (symcell)) - scm_heap_org))
    tmp = 0;
  SCM_NEWCELL (z);
  SCM_SUBRF (z) = fcn;
  SCM_CAR (z) = tmp + scm_tc7_subr_2;
  SCM_CDR (symcell) = macroizer (z);
  return SCM_CAR (symcell);
}

#ifdef DEBUG_EXTENSIONS
# ifndef DEVAL
#  define DEVAL
#  include "eval.c"
# endif
#endif


#ifdef __STDC__
void 
scm_init_eval (void)
#else
void 
scm_init_eval ()
#endif
{
#ifdef DEBUG_EXTENSIONS
  enter_frame_sym = SCM_CAR (scm_sysintern ("enter-frame", SCM_UNDEFINED));
  exit_frame_sym = SCM_CAR (scm_sysintern ("exit-frame", SCM_UNDEFINED));
#endif
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
  scm_make_synt ("define", scm_makmmacro, scm_m_define);
  scm_make_synt (s_delay, scm_makacro, scm_m_delay);
  /* end of acros */

  scm_top_level_lookup_thunk_var =
    scm_sysintern("*top-level-lookup-thunk*", SCM_BOOL_F);

  scm_make_synt ("and", scm_makmmacro, scm_m_and);
  scm_make_synt ("begin", scm_makmmacro, scm_m_begin);
  scm_make_synt ("case", scm_makmmacro, scm_m_case);
  scm_make_synt ("cond", scm_makmmacro, scm_m_cond);
  scm_make_synt ("do", scm_makmmacro, scm_m_do);
  scm_make_synt ("if", scm_makmmacro, scm_m_if);
  scm_i_lambda = scm_make_synt ("lambda", scm_makmmacro, scm_m_lambda);
  scm_i_let = scm_make_synt ("let", scm_makmmacro, scm_m_let);
  scm_make_synt ("letrec", scm_makmmacro, scm_m_letrec);
  scm_make_synt ("let*", scm_makmmacro, scm_m_letstar);
  scm_make_synt ("or", scm_makmmacro, scm_m_or);
  scm_i_quote = scm_make_synt ("quote", scm_makmmacro, scm_m_quote);
  scm_make_synt ("set!", scm_makmmacro, scm_m_set);
  scm_make_synt ("@apply", scm_makmmacro, scm_m_apply);
  scm_make_synt ("@call-with-current-continuation", scm_makmmacro, scm_m_cont);
  scm_make_synt ("defined?", scm_makmmacro, scm_definedp);
  scm_i_name = SCM_CAR (scm_sysintern ("name", SCM_UNDEFINED));
  scm_permanent_object (scm_i_name);
#include "eval.x"
}
#endif /* !DEVAL */

