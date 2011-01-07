/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011
 * Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/__scm.h"
#include "libguile/_scm.h"
#include "libguile/continuations.h"
#include "libguile/eq.h"
#include "libguile/expand.h"
#include "libguile/list.h"
#include "libguile/macros.h"
#include "libguile/memoize.h"
#include "libguile/modules.h"
#include "libguile/srcprop.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/strings.h"
#include "libguile/throw.h"
#include "libguile/validate.h"





#define CAR(x)   SCM_CAR(x)
#define CDR(x)   SCM_CDR(x)
#define CAAR(x)  SCM_CAAR(x)
#define CADR(x)  SCM_CADR(x)
#define CDAR(x)  SCM_CDAR(x)
#define CDDR(x)  SCM_CDDR(x)
#define CADDR(x) SCM_CADDR(x)
#define CDDDR(x) SCM_CDDDR(x)
#define CADDDR(x) SCM_CADDDR(x)


SCM_SYMBOL (sym_case_lambda_star, "case-lambda*");




/* {Evaluator memoized expressions}
 */

scm_t_bits scm_tc16_memoized;

#define MAKMEMO(n, args) 	(scm_cell (scm_tc16_memoized | ((n) << 16), (scm_t_bits)(args)))

#define MAKMEMO_BEGIN(exps) \
  MAKMEMO (SCM_M_BEGIN, exps)
#define MAKMEMO_IF(test, then, else_) \
  MAKMEMO (SCM_M_IF, scm_cons (test, scm_cons (then, else_)))
#define FIXED_ARITY(nreq) \
  scm_list_1 (SCM_I_MAKINUM (nreq))
#define REST_ARITY(nreq, rest) \
  scm_list_2 (SCM_I_MAKINUM (nreq), rest)
#define FULL_ARITY(nreq, rest, nopt, kw, inits, alt) \
  scm_list_n (SCM_I_MAKINUM (nreq), rest, SCM_I_MAKINUM (nopt), kw, inits, \
              alt, SCM_UNDEFINED)
#define MAKMEMO_LAMBDA(body, arity) \
  MAKMEMO (SCM_M_LAMBDA, (scm_cons (body, arity)))
#define MAKMEMO_LET(inits, body) \
  MAKMEMO (SCM_M_LET, scm_cons (inits, body))
#define MAKMEMO_QUOTE(exp) \
  MAKMEMO (SCM_M_QUOTE, exp)
#define MAKMEMO_DEFINE(var, val) \
  MAKMEMO (SCM_M_DEFINE, scm_cons (var, val))
#define MAKMEMO_DYNWIND(in, expr, out) \
  MAKMEMO (SCM_M_DYNWIND, scm_cons (in, scm_cons (expr, out)))
#define MAKMEMO_WITH_FLUIDS(fluids, vals, expr) \
  MAKMEMO (SCM_M_WITH_FLUIDS, scm_cons (fluids, scm_cons (vals, expr)))
#define MAKMEMO_APPLY(proc, args)\
  MAKMEMO (SCM_M_APPLY, scm_list_2 (proc, args))
#define MAKMEMO_CONT(proc) \
  MAKMEMO (SCM_M_CONT, proc)
#define MAKMEMO_CALL_WITH_VALUES(prod, cons) \
  MAKMEMO (SCM_M_CALL_WITH_VALUES, scm_cons (prod, cons))
#define MAKMEMO_CALL(proc, nargs, args) \
  MAKMEMO (SCM_M_CALL, scm_cons (proc, scm_cons (SCM_I_MAKINUM (nargs), args)))
#define MAKMEMO_LEX_REF(n) \
  MAKMEMO (SCM_M_LEXICAL_REF, SCM_I_MAKINUM (n))
#define MAKMEMO_LEX_SET(n, val) \
  MAKMEMO (SCM_M_LEXICAL_SET, scm_cons (SCM_I_MAKINUM (n), val))
#define MAKMEMO_TOP_REF(var) \
  MAKMEMO (SCM_M_TOPLEVEL_REF, var)
#define MAKMEMO_TOP_SET(var, val) \
  MAKMEMO (SCM_M_TOPLEVEL_SET, scm_cons (var, val))
#define MAKMEMO_MOD_REF(mod, var, public) \
  MAKMEMO (SCM_M_MODULE_REF, scm_cons (mod, scm_cons (var, public)))
#define MAKMEMO_MOD_SET(val, mod, var, public) \
  MAKMEMO (SCM_M_MODULE_SET, scm_cons (val, scm_cons (mod, scm_cons (var, public))))
#define MAKMEMO_PROMPT(tag, exp, handler) \
  MAKMEMO (SCM_M_PROMPT, scm_cons (tag, scm_cons (exp, handler)))


/* Primitives for the evaluator */
scm_t_bits scm_tc16_memoizer;
#define SCM_MEMOIZER_P(x) (SCM_SMOB_PREDICATE (scm_tc16_memoizer, (x)))
#define SCM_MEMOIZER(M) (SCM_SMOB_OBJECT_1 (M))



/* This table must agree with the list of M_ constants in memoize.h */
static const char *const memoized_tags[] =
{
  "begin",
  "if",
  "lambda",
  "let",
  "quote",
  "define",
  "dynwind",
  "with-fluids",
  "apply",
  "call/cc",
  "call-with-values",
  "call",
  "lexical-ref",
  "lexical-set!",
  "toplevel-ref",
  "toplevel-set!",
  "module-ref",
  "module-set!",
  "prompt",
};

static int
scm_print_memoized (SCM memoized, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<memoized ", port);
  scm_write (scm_unmemoize_expression (memoized), port);
  scm_puts (">", port);
  return 1;
}





static int
lookup (SCM x, SCM env)
{
  int i = 0;
  for (; scm_is_pair (env); env = CDR (env), i++)
    if (scm_is_eq (x, CAR (env)))
      return i; /* bound */
  abort ();
}


/* Abbreviate SCM_EXPANDED_REF. Copied because I'm not sure about symbol pasting */
#define REF(x,type,field) \
  (scm_struct_ref (x, SCM_I_MAKINUM (SCM_EXPANDED_##type##_##field)))

static SCM list_of_guile = SCM_BOOL_F;

static SCM memoize (SCM exp, SCM env);

static SCM
memoize_exps (SCM exps, SCM env)
{
  SCM ret;
  for (ret = SCM_EOL; scm_is_pair (exps); exps = CDR (exps))
    ret = scm_cons (memoize (CAR (exps), env), ret);
  return scm_reverse_x (ret, SCM_UNDEFINED);
}
  
static SCM
memoize (SCM exp, SCM env)
{
  if (!SCM_EXPANDED_P (exp))
    abort ();

  switch (SCM_EXPANDED_TYPE (exp))
    {
    case SCM_EXPANDED_VOID:
      return MAKMEMO_QUOTE (SCM_UNSPECIFIED);
      
    case SCM_EXPANDED_CONST:
      return MAKMEMO_QUOTE (REF (exp, CONST, EXP));

    case SCM_EXPANDED_PRIMITIVE_REF:
      if (scm_is_eq (scm_current_module (), scm_the_root_module ()))
        return MAKMEMO_TOP_REF (REF (exp, PRIMITIVE_REF, NAME));
      else
        return MAKMEMO_MOD_REF (list_of_guile, REF (exp, PRIMITIVE_REF, NAME),
                                SCM_BOOL_F);
                                
    case SCM_EXPANDED_LEXICAL_REF:
      return MAKMEMO_LEX_REF (lookup (REF (exp, LEXICAL_REF, GENSYM), env));

    case SCM_EXPANDED_LEXICAL_SET:
      return MAKMEMO_LEX_SET (lookup (REF (exp, LEXICAL_SET, GENSYM), env),
                              memoize (REF (exp, LEXICAL_SET, EXP), env));

    case SCM_EXPANDED_MODULE_REF:
      return MAKMEMO_MOD_REF (REF (exp, MODULE_REF, MOD),
                              REF (exp, MODULE_REF, NAME),
                              REF (exp, MODULE_REF, PUBLIC));

    case SCM_EXPANDED_MODULE_SET:
      return MAKMEMO_MOD_SET (memoize (REF (exp, MODULE_SET, EXP), env),
                              REF (exp, MODULE_SET, MOD),
                              REF (exp, MODULE_SET, NAME),
                              REF (exp, MODULE_SET, PUBLIC));

    case SCM_EXPANDED_TOPLEVEL_REF:
      return MAKMEMO_TOP_REF (REF (exp, TOPLEVEL_REF, NAME));

    case SCM_EXPANDED_TOPLEVEL_SET:
      return MAKMEMO_TOP_SET (REF (exp, TOPLEVEL_SET, NAME),
                              memoize (REF (exp, TOPLEVEL_SET, EXP), env));

    case SCM_EXPANDED_TOPLEVEL_DEFINE:
      return MAKMEMO_DEFINE (REF (exp, TOPLEVEL_DEFINE, NAME),
                             memoize (REF (exp, TOPLEVEL_DEFINE, EXP), env));

    case SCM_EXPANDED_CONDITIONAL:
      return MAKMEMO_IF (memoize (REF (exp, CONDITIONAL, TEST), env),
                         memoize (REF (exp, CONDITIONAL, CONSEQUENT), env),
                         memoize (REF (exp, CONDITIONAL, ALTERNATE), env));

    case SCM_EXPANDED_APPLICATION:
      {
        SCM proc, args;

        proc = REF (exp, APPLICATION, PROC);
        args = memoize_exps (REF (exp, APPLICATION, ARGS), env);

        if (SCM_EXPANDED_TYPE (proc) == SCM_EXPANDED_TOPLEVEL_REF)
          {
            SCM var = scm_module_variable (scm_current_module (),
                                           REF (proc, TOPLEVEL_REF, NAME));
            if (SCM_VARIABLEP (var))
              {
                SCM val = SCM_VARIABLE_REF (var);
                if (SCM_MEMOIZER_P (val))
                  return scm_apply (SCM_SMOB_OBJECT_1 (val), args, SCM_EOL);
              }
          }
        /* otherwise we all fall down here */
        return MAKMEMO_CALL (memoize (proc, env), scm_ilength (args), args);
      }

    case SCM_EXPANDED_SEQUENCE:
      return MAKMEMO_BEGIN (memoize_exps (REF (exp, SEQUENCE, EXPS), env));

    case SCM_EXPANDED_LAMBDA:
      /* The body will be a lambda-case. */
      return memoize (REF (exp, LAMBDA, BODY), env);

    case SCM_EXPANDED_LAMBDA_CASE:
      {
        SCM req, rest, opt, kw, inits, vars, body, alt;
        SCM walk, minits, arity, new_env;
        int nreq, nopt, ntotal;

        req = REF (exp, LAMBDA_CASE, REQ);
        rest = REF (exp, LAMBDA_CASE, REST);
        opt = REF (exp, LAMBDA_CASE, OPT);
        kw = REF (exp, LAMBDA_CASE, KW);
        inits = REF (exp, LAMBDA_CASE, INITS);
        vars = REF (exp, LAMBDA_CASE, GENSYMS);
        body = REF (exp, LAMBDA_CASE, BODY);
        alt = REF (exp, LAMBDA_CASE, ALTERNATE);

        nreq = scm_ilength (req);
        nopt = scm_is_pair (opt) ? scm_ilength (opt) : 0;
        ntotal = scm_ilength (vars);

        /* The vars are the gensyms, according to the divine plan. But we need
           to memoize the inits within their appropriate environment,
           complicating things. */
        new_env = env;
        for (walk = req; scm_is_pair (walk);
             walk = CDR (walk), vars = CDR (vars))
          new_env = scm_cons (CAR (vars), new_env);

        minits = SCM_EOL;
        for (walk = opt; scm_is_pair (walk);
             walk = CDR (walk), vars = CDR (vars), inits = CDR (inits))
          {
            minits = scm_cons (memoize (CAR (inits), new_env), minits);
            new_env = scm_cons (CAR (vars), new_env);
          }

        if (scm_is_true (rest))
          {
            new_env = scm_cons (CAR (vars), new_env);
            vars = CDR (vars);
          }

        for (; scm_is_pair (inits); vars = CDR (vars), inits = CDR (inits))
          {
            minits = scm_cons (memoize (CAR (inits), new_env), minits);
            new_env = scm_cons (CAR (vars), new_env);
          }
        if (!scm_is_null (vars))
          abort ();

        minits = scm_reverse_x (minits, SCM_UNDEFINED);

        if (scm_is_true (kw))
          {
            /* (aok? (kw name sym) ...) -> (aok? (kw . index) ...) */
            SCM aok = CAR (kw), indices = SCM_EOL;
            for (kw = CDR (kw); scm_is_pair (kw); kw = CDR (kw))
              {
                SCM k;
                int idx;

                k = CAR (CAR (kw));
                idx = ntotal - 1 - lookup (CADDR (CAR (kw)), new_env);
                indices = scm_acons (k, SCM_I_MAKINUM (idx), indices);
              }
            kw = scm_cons (aok, scm_reverse_x (indices, SCM_UNDEFINED));
          }

        if (scm_is_false (alt) && scm_is_false (kw) && scm_is_false (opt))
          {
            if (scm_is_false (rest))
              arity = FIXED_ARITY (nreq);
            else
              arity = REST_ARITY (nreq, SCM_BOOL_T);
          }
        else if (scm_is_true (alt))
          arity = FULL_ARITY (nreq, rest, nopt, kw, minits,
                              SCM_MEMOIZED_ARGS (memoize (alt, env)));
        else
          arity = FULL_ARITY (nreq, rest, nopt, kw, minits, SCM_BOOL_F);

        return MAKMEMO_LAMBDA (memoize (body, new_env), arity);
      }

    case SCM_EXPANDED_LET:
      {
        SCM vars, exps, body, inits, new_env;
        
        vars = REF (exp, LET, GENSYMS);
        exps = REF (exp, LET, VALS);
        body = REF (exp, LET, BODY);
        
        inits = SCM_EOL;
        new_env = env;
        for (; scm_is_pair (vars); vars = CDR (vars), exps = CDR (exps))
          {
            new_env = scm_cons (CAR (vars), new_env);
            inits = scm_cons (memoize (CAR (exps), env), inits);
          }

        return MAKMEMO_LET (scm_reverse_x (inits, SCM_UNDEFINED),
                            memoize (body, new_env));
      }

    case SCM_EXPANDED_LETREC:
      {
        SCM vars, exps, body, undefs, new_env;
        int i, nvars, in_order_p;
        
        vars = REF (exp, LETREC, GENSYMS);
        exps = REF (exp, LETREC, VALS);
        body = REF (exp, LETREC, BODY);
        in_order_p = scm_is_true (REF (exp, LETREC, IN_ORDER_P));
        nvars = i = scm_ilength (vars);
        undefs = SCM_EOL;
        new_env = env;

        for (; scm_is_pair (vars); vars = CDR (vars))
          {
            new_env = scm_cons (CAR (vars), new_env);
            undefs = scm_cons (MAKMEMO_QUOTE (SCM_UNDEFINED), undefs);
          }

        if (in_order_p)
          {
            SCM body_exps = SCM_EOL;
            for (; scm_is_pair (exps); exps = CDR (exps), i--)
              body_exps = scm_cons (MAKMEMO_LEX_SET (i-1,
                                                     memoize (CAR (exps), new_env)),
                                    body_exps);
            body_exps = scm_cons (memoize (body, new_env), body_exps);
            body_exps = scm_reverse_x (body_exps, SCM_UNDEFINED);
            return MAKMEMO_LET (undefs, MAKMEMO_BEGIN (body_exps));
          }
        else
          {
            SCM sets = SCM_EOL, inits = SCM_EOL;
            for (; scm_is_pair (exps); exps = CDR (exps), i--)
              {
                sets = scm_cons (MAKMEMO_LEX_SET ((i-1) + nvars,
                                                  MAKMEMO_LEX_REF (i-1)),
                                 sets);
                inits = scm_cons (memoize (CAR (exps), new_env), inits);
              }
            inits = scm_reverse_x (inits, SCM_UNDEFINED);
            return MAKMEMO_LET
              (undefs,
               MAKMEMO_BEGIN (scm_list_2 (MAKMEMO_LET (inits, MAKMEMO_BEGIN (sets)),
                                          memoize (body, new_env))));
          }
      }

    case SCM_EXPANDED_DYNLET:
      return MAKMEMO_WITH_FLUIDS (memoize_exps (REF (exp, DYNLET, FLUIDS), env),
                                  memoize_exps (REF (exp, DYNLET, VALS), env),
                                  memoize (REF (exp, DYNLET, BODY), env));

    default:
      abort ();
    }
}




SCM_DEFINE (scm_memoize_expression, "memoize-expression", 1, 0, 0, 
            (SCM exp),
	    "Memoize the expression @var{exp}.")
#define FUNC_NAME s_scm_memoize_expression
{
  SCM_ASSERT_TYPE (SCM_EXPANDED_P (exp), exp, 1, FUNC_NAME, "expanded");
  return memoize (exp, scm_current_module ());
}
#undef FUNC_NAME




#define SCM_MAKE_MEMOIZER(STR, MEMOIZER, N)                             \
  (scm_cell (scm_tc16_memoizer,                                         \
             (scm_t_bits)(scm_c_make_gsubr (STR, N, 0, 0, MEMOIZER))))
#define SCM_DEFINE_MEMOIZER(STR, MEMOIZER, N)                           \
SCM_SNARF_INIT(scm_c_define (STR, SCM_MAKE_MEMOIZER (STR, MEMOIZER, N)))

#define SCM_MAKE_REST_MEMOIZER(STR, MEMOIZER, N)                        \
  (scm_cell (scm_tc16_memoizer,                                         \
             (scm_t_bits)(scm_c_make_gsubr (STR, N, 0, 1, MEMOIZER))))
#define SCM_DEFINE_REST_MEMOIZER(STR, MEMOIZER, N)                      \
SCM_SNARF_INIT(scm_c_define (STR, SCM_MAKE_REST_MEMOIZER (STR, MEMOIZER, N)))

static SCM m_apply (SCM proc, SCM arg, SCM rest);
static SCM m_call_cc (SCM proc);
static SCM m_call_values (SCM prod, SCM cons);
static SCM m_dynamic_wind (SCM pre, SCM exp, SCM post);
static SCM m_prompt (SCM tag, SCM exp, SCM handler);

SCM_DEFINE_REST_MEMOIZER ("@apply", m_apply, 2);
SCM_DEFINE_MEMOIZER ("@call-with-current-continuation", m_call_cc, 1);
SCM_DEFINE_MEMOIZER ("@call-with-values", m_call_values, 2);
SCM_DEFINE_MEMOIZER ("@dynamic-wind", m_dynamic_wind, 3);
SCM_DEFINE_MEMOIZER ("@prompt", m_prompt, 3);




static SCM m_apply (SCM proc, SCM arg, SCM rest)
#define FUNC_NAME "@apply"
{
  long len;
  
  SCM_VALIDATE_MEMOIZED (1, proc);
  SCM_VALIDATE_MEMOIZED (2, arg);
  len = scm_ilength (rest);
  if (len < 0)
    abort ();
  else if (len == 0)
    return MAKMEMO_APPLY (proc, arg);
  else
    {
      SCM tail;

      rest = scm_reverse (rest);
      tail = scm_car (rest);
      rest = scm_cdr (rest);
      len--;
      
      while (scm_is_pair (rest))
        {
          tail = MAKMEMO_CALL (MAKMEMO_MOD_REF (scm_list_1 (scm_from_latin1_symbol ("guile")),
                                                scm_from_latin1_symbol ("cons"),
                                                SCM_BOOL_F),
                               2,
                               scm_list_2 (scm_car (rest), tail));
          rest = scm_cdr (rest);
        }
      return MAKMEMO_APPLY (proc, tail);
    }
}
#undef FUNC_NAME

static SCM m_call_cc (SCM proc)
#define FUNC_NAME "@call-with-current-continuation"
{
  SCM_VALIDATE_MEMOIZED (1, proc);
  return MAKMEMO_CONT (proc);
}
#undef FUNC_NAME

static SCM m_call_values (SCM prod, SCM cons)
#define FUNC_NAME "@call-with-values"
{
  SCM_VALIDATE_MEMOIZED (1, prod);
  SCM_VALIDATE_MEMOIZED (2, cons);
  return MAKMEMO_CALL_WITH_VALUES (prod, cons);
}
#undef FUNC_NAME

static SCM m_dynamic_wind (SCM in, SCM expr, SCM out)
#define FUNC_NAME "memoize-dynwind"
{
  SCM_VALIDATE_MEMOIZED (1, in);
  SCM_VALIDATE_MEMOIZED (2, expr);
  SCM_VALIDATE_MEMOIZED (3, out);
  return MAKMEMO_DYNWIND (in, expr, out);
}
#undef FUNC_NAME

static SCM m_prompt (SCM tag, SCM exp, SCM handler)
#define FUNC_NAME "@prompt"
{
  SCM_VALIDATE_MEMOIZED (1, tag);
  SCM_VALIDATE_MEMOIZED (2, exp);
  SCM_VALIDATE_MEMOIZED (3, handler);
  return MAKMEMO_PROMPT (tag, exp, handler);
}
#undef FUNC_NAME




SCM_SYMBOL (sym_placeholder, "_");

static SCM unmemoize (SCM expr);

static SCM
unmemoize_exprs (SCM exprs)
{
  SCM ret, tail;
  if (scm_is_null (exprs))
    return SCM_EOL;
  ret = scm_list_1 (unmemoize (CAR (exprs)));
  tail = ret;
  for (exprs = CDR (exprs); !scm_is_null (exprs); exprs = CDR (exprs))
    {
      SCM_SETCDR (tail, scm_list_1 (unmemoize (CAR (exprs))));
      tail = CDR (tail);
    }
  return ret;
}

static SCM
unmemoize_bindings (SCM inits)
{
  SCM ret, tail;
  if (scm_is_null (inits))
    return SCM_EOL;
  ret = scm_list_1 (scm_list_2 (sym_placeholder, unmemoize (CAR (inits))));
  tail = ret;
  for (inits = CDR (inits); !scm_is_null (inits); inits = CDR (inits))
    {
      SCM_SETCDR (tail, scm_list_1 (scm_list_2 (sym_placeholder,
                                                unmemoize (CAR (inits)))));
      tail = CDR (tail);
    }
  return ret;
}

static SCM
unmemoize_lexical (SCM n)
{
  char buf[16];
  buf[15] = 0;
  snprintf (buf, 15, "<%u>", scm_to_uint32 (n));
  return scm_from_locale_symbol (buf);
}

static SCM
unmemoize (const SCM expr)
{
  SCM args;
  
  if (!SCM_MEMOIZED_P (expr))
    abort ();

  args = SCM_MEMOIZED_ARGS (expr);
  switch (SCM_MEMOIZED_TAG (expr))
    {
    case SCM_M_APPLY:
      return scm_cons (scm_sym_atapply, unmemoize_exprs (args));
    case SCM_M_BEGIN:
      return scm_cons (scm_sym_begin, unmemoize_exprs (args));
    case SCM_M_CALL:
      return scm_cons (unmemoize (CAR (args)), unmemoize_exprs (CDDR (args)));
    case SCM_M_CONT:
      return scm_list_2 (scm_sym_atcall_cc, unmemoize (args));
    case SCM_M_CALL_WITH_VALUES:
      return scm_list_3 (scm_sym_at_call_with_values,
                         unmemoize (CAR (args)), unmemoize (CDR (args)));
    case SCM_M_DEFINE:
      return scm_list_3 (scm_sym_define, CAR (args), unmemoize (CDR (args)));
    case SCM_M_DYNWIND:
      return scm_list_4 (scm_sym_at_dynamic_wind,
                         unmemoize (CAR (args)),
                         unmemoize (CADR (args)),
                         unmemoize (CDDR (args)));
    case SCM_M_WITH_FLUIDS:
      {
        SCM binds = SCM_EOL, fluids, vals;
        for (fluids = CAR (args), vals = CADR (args); scm_is_pair (fluids);
             fluids = CDR (fluids), vals = CDR (vals))
          binds = scm_cons (scm_list_2 (unmemoize (CAR (fluids)),
                                        unmemoize (CAR (vals))),
                            binds);
        return scm_list_3 (scm_sym_with_fluids,
                           scm_reverse_x (binds, SCM_UNDEFINED),
                           unmemoize (CDDR (args)));
      }
    case SCM_M_IF:
      return scm_list_4 (scm_sym_if, unmemoize (scm_car (args)),
                         unmemoize (scm_cadr (args)), unmemoize (scm_cddr (args)));
    case SCM_M_LAMBDA:
      if (scm_is_null (CDDR (args)))
        return scm_list_3 (scm_sym_lambda,
                           scm_make_list (CADR (args), sym_placeholder),
                           unmemoize (CAR (args)));
      else if (scm_is_null (CDDDR (args)))
        {
          SCM formals = scm_make_list (CADR (args), sym_placeholder);
          return scm_list_3 (scm_sym_lambda,
                             scm_is_true (CADDR (args))
                             ? scm_cons_star (sym_placeholder, formals)
                             : formals,
                             unmemoize (CAR (args)));
        }
      else
        {
          SCM body = CAR (args), spec = CDR (args), alt, tail;
          
          alt = CADDR (CDDDR (spec));
          if (scm_is_true (alt))
            tail = CDR (unmemoize (alt));
          else
            tail = SCM_EOL;
          
          return scm_cons
            (sym_case_lambda_star,
             scm_cons (scm_list_2 (scm_list_5 (CAR (spec),
                                               CADR (spec),
                                               CADDR (spec),
                                               CADDDR (spec),
                                               unmemoize_exprs (CADR (CDDDR (spec)))),
                                   unmemoize (body)),
                       tail));
        }
    case SCM_M_LET:
      return scm_list_3 (scm_sym_let,
                         unmemoize_bindings (CAR (args)),
                         unmemoize (CDR (args)));
    case SCM_M_QUOTE:
      return scm_list_2 (scm_sym_quote, args);
    case SCM_M_LEXICAL_REF:
      return unmemoize_lexical (args);
    case SCM_M_LEXICAL_SET:
      return scm_list_3 (scm_sym_set_x, unmemoize_lexical (CAR (args)),
                         unmemoize (CDR (args)));
    case SCM_M_TOPLEVEL_REF:
      return args;
    case SCM_M_TOPLEVEL_SET:
      return scm_list_3 (scm_sym_set_x, CAR (args), unmemoize (CDR (args)));
    case SCM_M_MODULE_REF:
      return SCM_VARIABLEP (args) ? args
        : scm_list_3 (scm_is_true (CDDR (args)) ? scm_sym_at : scm_sym_atat,
                      scm_i_finite_list_copy (CAR (args)),
                      CADR (args));
    case SCM_M_MODULE_SET:
      return scm_list_3 (scm_sym_set_x,
                         SCM_VARIABLEP (CDR (args)) ? CDR (args)
                         : scm_list_3 (scm_is_true (CDDDR (args))
                                       ? scm_sym_at : scm_sym_atat,
                                       scm_i_finite_list_copy (CADR (args)),
                                       CADDR (args)),
                         unmemoize (CAR (args)));
    case SCM_M_PROMPT:
      return scm_list_4 (scm_sym_at_prompt,
                         unmemoize (CAR (args)),
                         unmemoize (CADR (args)),
                         unmemoize (CDDR (args)));
    default:
      abort ();
    }
}




SCM_DEFINE (scm_memoized_p, "memoized?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is memoized.")
#define FUNC_NAME s_scm_memoized_p
{
  return scm_from_bool (SCM_MEMOIZED_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_unmemoize_expression, "unmemoize-expression", 1, 0, 0, 
            (SCM m),
	    "Unmemoize the memoized expression @var{m}.")
#define FUNC_NAME s_scm_unmemoize_expression
{
  SCM_VALIDATE_MEMOIZED (1, m);
  return unmemoize (m);
}
#undef FUNC_NAME

SCM_DEFINE (scm_memoized_expression_typecode, "memoized-expression-typecode", 1, 0, 0, 
            (SCM m),
	    "Return the typecode from the memoized expression @var{m}.")
#define FUNC_NAME s_scm_memoized_expression_typecode
{
  SCM_VALIDATE_MEMOIZED (1, m);

  /* The tag is a 16-bit integer so it fits in an inum.  */
  return SCM_I_MAKINUM (SCM_MEMOIZED_TAG (m));
}
#undef FUNC_NAME

SCM_DEFINE (scm_memoized_expression_data, "memoized-expression-data", 1, 0, 0, 
            (SCM m),
	    "Return the data from the memoized expression @var{m}.")
#define FUNC_NAME s_scm_memoized_expression_data
{
  SCM_VALIDATE_MEMOIZED (1, m);
  return SCM_MEMOIZED_ARGS (m);
}
#undef FUNC_NAME

SCM_DEFINE (scm_memoized_typecode, "memoized-typecode", 1, 0, 0, 
            (SCM sym),
	    "Return the memoized typecode corresponding to the symbol @var{sym}.")
#define FUNC_NAME s_scm_memoized_typecode
{
  int i;

  SCM_VALIDATE_SYMBOL (1, sym);

  for (i = 0; i < sizeof(memoized_tags)/sizeof(const char*); i++)
    if (strcmp (scm_i_symbol_chars (sym), memoized_tags[i]) == 0)
      return scm_from_int32 (i);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_SYMBOL (scm_unbound_variable_key, "unbound-variable");
static void error_unbound_variable (SCM symbol) SCM_NORETURN;
static void error_unbound_variable (SCM symbol)
{
  scm_error (scm_unbound_variable_key, NULL, "Unbound variable: ~S",
	     scm_list_1 (symbol), SCM_BOOL_F);
}

SCM_DEFINE (scm_memoize_variable_access_x, "memoize-variable-access!", 2, 0, 0, 
            (SCM m, SCM mod),
	    "Look up and cache the variable that @var{m} will access, returning the variable.")
#define FUNC_NAME s_scm_memoize_variable_access_x
{
  SCM mx;
  SCM_VALIDATE_MEMOIZED (1, m);
  mx = SCM_MEMOIZED_ARGS (m);
  switch (SCM_MEMOIZED_TAG (m))
    {
    case SCM_M_TOPLEVEL_REF:
      if (SCM_VARIABLEP (mx))
        return mx;
      else
        {
          SCM var = scm_module_variable (mod, mx);
          if (scm_is_false (var) || scm_is_false (scm_variable_bound_p (var)))
            error_unbound_variable (mx);
          SCM_SET_SMOB_OBJECT (m, var);
          return var;
        }

    case SCM_M_TOPLEVEL_SET:
      {
        SCM var = CAR (mx);
        if (SCM_VARIABLEP (var))
          return var;
        else
          {
            var = scm_module_variable (mod, var);
            if (scm_is_false (var))
              error_unbound_variable (CAR (mx));
            SCM_SETCAR (mx, var);
            return var;
          }
      }

    case SCM_M_MODULE_REF:
      if (SCM_VARIABLEP (mx))
        return mx;
      else
        {
          SCM var;
          mod = scm_resolve_module (CAR (mx));
          if (scm_is_true (CDDR (mx)))
            mod = scm_module_public_interface (mod);
          var = scm_module_lookup (mod, CADR (mx));
          if (scm_is_false (scm_variable_bound_p (var)))
            error_unbound_variable (CADR (mx));
          SCM_SET_SMOB_OBJECT (m, var);
          return var;
        }

    case SCM_M_MODULE_SET:
      /* FIXME: not quite threadsafe */
      if (SCM_VARIABLEP (CDR (mx)))
        return CDR (mx);
      else
        {
          SCM var;
          mod = scm_resolve_module (CADR (mx));
          if (scm_is_true (CDDDR (mx)))
            mod = scm_module_public_interface (mod);
          var = scm_module_lookup (mod, CADDR (mx));
          SCM_SETCDR (mx, var);
          return var;
        }

    default:
      scm_wrong_type_arg (FUNC_NAME, 1, m);
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME




void
scm_init_memoize ()
{
  scm_tc16_memoized = scm_make_smob_type ("%memoized", 0);
  scm_set_smob_mark (scm_tc16_memoized, scm_markcdr);
  scm_set_smob_print (scm_tc16_memoized, scm_print_memoized);

  scm_tc16_memoizer = scm_make_smob_type ("memoizer", 0);

#include "libguile/memoize.x"

  list_of_guile = scm_list_1 (scm_from_latin1_symbol ("guile"));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
