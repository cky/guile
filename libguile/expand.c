/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2012
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
#include "libguile/list.h"
#include "libguile/macros.h"
#include "libguile/expand.h"
#include "libguile/modules.h"
#include "libguile/srcprop.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/strings.h"
#include "libguile/throw.h"
#include "libguile/validate.h"





SCM scm_exp_vtable_vtable;
static SCM exp_vtables[SCM_NUM_EXPANDED_TYPES];
static size_t exp_nfields[SCM_NUM_EXPANDED_TYPES];
static const char* exp_names[SCM_NUM_EXPANDED_TYPES];
static const char** exp_field_names[SCM_NUM_EXPANDED_TYPES];


/* The trailing underscores on these first to are to avoid spurious
   conflicts with macros defined on MinGW.  */

#define VOID_(src) \
  SCM_MAKE_EXPANDED_VOID(src)
#define CONST_(src, exp) \
  SCM_MAKE_EXPANDED_CONST(src, exp)
#define PRIMITIVE_REF_TYPE(src, name) \
  SCM_MAKE_EXPANDED_PRIMITIVE_REF_TYPE(src, name)
#define LEXICAL_REF(src, name, gensym) \
  SCM_MAKE_EXPANDED_LEXICAL_REF(src, name, gensym)
#define LEXICAL_SET(src, name, gensym, exp) \
  SCM_MAKE_EXPANDED_LEXICAL_SET(src, name, gensym, exp)
#define MODULE_REF(src, mod, name, public) \
  SCM_MAKE_EXPANDED_MODULE_REF(src, mod, name, public)
#define MODULE_SET(src, mod, name, public, exp) \
  SCM_MAKE_EXPANDED_MODULE_SET(src, mod, name, public, exp)
#define TOPLEVEL_REF(src, name) \
  SCM_MAKE_EXPANDED_TOPLEVEL_REF(src, name)
#define TOPLEVEL_SET(src, name, exp) \
  SCM_MAKE_EXPANDED_TOPLEVEL_SET(src, name, exp)
#define TOPLEVEL_DEFINE(src, name, exp) \
  SCM_MAKE_EXPANDED_TOPLEVEL_DEFINE(src, name, exp)
#define CONDITIONAL(src, test, consequent, alternate) \
  SCM_MAKE_EXPANDED_CONDITIONAL(src, test, consequent, alternate)
#define APPLICATION(src, proc, exps) \
  SCM_MAKE_EXPANDED_APPLICATION(src, proc, exps)
#define SEQUENCE(src, exps) \
  SCM_MAKE_EXPANDED_SEQUENCE(src, exps)
#define LAMBDA(src, meta, body) \
  SCM_MAKE_EXPANDED_LAMBDA(src, meta, body)
#define LAMBDA_CASE(src, req, opt, rest, kw, inits, gensyms, body, alternate) \
  SCM_MAKE_EXPANDED_LAMBDA_CASE(src, req, opt, rest, kw, inits, gensyms, body, alternate)
#define LET(src, names, gensyms, vals, body) \
  SCM_MAKE_EXPANDED_LET(src, names, gensyms, vals, body)
#define LETREC(src, in_order_p, names, gensyms, vals, body) \
  SCM_MAKE_EXPANDED_LETREC(src, in_order_p, names, gensyms, vals, body)
#define DYNLET(src, fluids, vals, body) \
  SCM_MAKE_EXPANDED_DYNLET(src, fluids, vals, body)

#define CAR(x)   SCM_CAR(x)
#define CDR(x)   SCM_CDR(x)
#define CAAR(x)  SCM_CAAR(x)
#define CADR(x)  SCM_CADR(x)
#define CDAR(x)  SCM_CDAR(x)
#define CDDR(x)  SCM_CDDR(x)
#define CADDR(x) SCM_CADDR(x)
#define CDDDR(x) SCM_CDDDR(x)
#define CADDDR(x) SCM_CADDDR(x)


static const char s_bad_expression[] = "Bad expression";
static const char s_expression[] = "Missing or extra expression in";
static const char s_missing_expression[] = "Missing expression in";
static const char s_extra_expression[] = "Extra expression in";
static const char s_empty_combination[] = "Illegal empty combination";
static const char s_missing_body_expression[] = "Missing body expression in";
static const char s_mixed_body_forms[] = "Mixed definitions and expressions in";
static const char s_bad_define[] = "Bad define placement";
static const char s_missing_clauses[] = "Missing clauses";
static const char s_misplaced_else_clause[] = "Misplaced else clause";
static const char s_bad_case_clause[] = "Bad case clause";
static const char s_bad_case_labels[] = "Bad case labels";
static const char s_duplicate_case_label[] = "Duplicate case label";
static const char s_bad_cond_clause[] = "Bad cond clause";
static const char s_missing_recipient[] = "Missing recipient in";
static const char s_bad_variable[] = "Bad variable";
static const char s_bad_bindings[] = "Bad bindings";
static const char s_bad_binding[] = "Bad binding";
static const char s_duplicate_binding[] = "Duplicate binding";
static const char s_bad_exit_clause[] = "Bad exit clause";
static const char s_bad_formals[] = "Bad formals";
static const char s_bad_formal[] = "Bad formal";
static const char s_duplicate_formal[] = "Duplicate formal";
static const char s_splicing[] = "Non-list result for unquote-splicing";
static const char s_bad_slot_number[] = "Bad slot number";

static void syntax_error (const char* const, const SCM, const SCM) SCM_NORETURN;

SCM_SYMBOL (syntax_error_key, "syntax-error");

/* Shortcut macros to simplify syntax error handling. */
#define ASSERT_SYNTAX(cond, message, form)		\
  { if (SCM_UNLIKELY (!(cond)))			\
      syntax_error (message, form, SCM_UNDEFINED); }
#define ASSERT_SYNTAX_2(cond, message, form, expr)	\
  { if (SCM_UNLIKELY (!(cond)))			\
      syntax_error (message, form, expr); }




/* Primitive syntax.  */

#define SCM_SYNTAX(STR, CFN)  \
SCM_SNARF_HERE(static SCM CFN (SCM xorig, SCM env))                     \
SCM_SNARF_INIT(scm_c_define (STR, scm_i_make_primitive_macro (STR, CFN)))


/* True primitive syntax */
SCM_SYNTAX ("@", expand_at);
SCM_SYNTAX ("@@", expand_atat);
SCM_SYNTAX ("begin", expand_begin);
SCM_SYNTAX ("define", expand_define);
SCM_SYNTAX ("with-fluids", expand_with_fluids);
SCM_SYNTAX ("eval-when", expand_eval_when);
SCM_SYNTAX ("if", expand_if);
SCM_SYNTAX ("lambda", expand_lambda);
SCM_SYNTAX ("let", expand_let);
SCM_SYNTAX ("quote", expand_quote);
SCM_SYNTAX ("set!", expand_set_x);

/* Convenient syntax during boot, expands to primitive syntax. Replaced after
   psyntax boots. */
SCM_SYNTAX ("and", expand_and);
SCM_SYNTAX ("cond", expand_cond);
SCM_SYNTAX ("letrec", expand_letrec);
SCM_SYNTAX ("letrec*", expand_letrec_star);
SCM_SYNTAX ("let*", expand_letstar);
SCM_SYNTAX ("or", expand_or);
SCM_SYNTAX ("lambda*", expand_lambda_star);
SCM_SYNTAX ("case-lambda", expand_case_lambda);
SCM_SYNTAX ("case-lambda*", expand_case_lambda_star);


SCM_GLOBAL_SYMBOL (scm_sym_apply, "apply");
SCM_GLOBAL_SYMBOL (scm_sym_arrow, "=>");
SCM_GLOBAL_SYMBOL (scm_sym_at, "@");
SCM_GLOBAL_SYMBOL (scm_sym_atat, "@@");
SCM_GLOBAL_SYMBOL (scm_sym_at_call_with_values, "@call-with-values");
SCM_GLOBAL_SYMBOL (scm_sym_atapply, "@apply");
SCM_GLOBAL_SYMBOL (scm_sym_atcall_cc, "@call-with-current-continuation");
SCM_GLOBAL_SYMBOL (scm_sym_begin, "begin");
SCM_GLOBAL_SYMBOL (scm_sym_case, "case");
SCM_GLOBAL_SYMBOL (scm_sym_cond, "cond");
SCM_GLOBAL_SYMBOL (scm_sym_define, "define");
SCM_GLOBAL_SYMBOL (scm_sym_at_dynamic_wind, "@dynamic-wind");
SCM_GLOBAL_SYMBOL (scm_sym_with_fluids, "with-fluids");
SCM_GLOBAL_SYMBOL (scm_sym_else, "else");
SCM_GLOBAL_SYMBOL (scm_sym_eval_when, "eval-when");
SCM_GLOBAL_SYMBOL (scm_sym_if, "if");
SCM_GLOBAL_SYMBOL (scm_sym_lambda, "lambda");
SCM_GLOBAL_SYMBOL (scm_sym_let, "let");
SCM_GLOBAL_SYMBOL (scm_sym_letrec, "letrec");
SCM_GLOBAL_SYMBOL (scm_sym_letstar, "let*");
SCM_GLOBAL_SYMBOL (scm_sym_or, "or");
SCM_GLOBAL_SYMBOL (scm_sym_at_prompt, "@prompt");
SCM_GLOBAL_SYMBOL (scm_sym_quote, "quote");
SCM_GLOBAL_SYMBOL (scm_sym_set_x, "set!");
SCM_SYMBOL (sym_lambda_star, "lambda*");
SCM_SYMBOL (sym_eval, "eval");
SCM_SYMBOL (sym_load, "load");

SCM_GLOBAL_SYMBOL (scm_sym_unquote, "unquote");
SCM_GLOBAL_SYMBOL (scm_sym_quasiquote, "quasiquote");
SCM_GLOBAL_SYMBOL (scm_sym_uq_splicing, "unquote-splicing");

SCM_KEYWORD (kw_allow_other_keys, "allow-other-keys");
SCM_KEYWORD (kw_optional, "optional");
SCM_KEYWORD (kw_key, "key");
SCM_KEYWORD (kw_rest, "rest");





/* Signal a syntax error.  We distinguish between the form that caused the
 * error and the enclosing expression.  The error message will print out as
 * shown in the following pattern.  The file name and line number are only
 * given when they can be determined from the erroneous form or from the
 * enclosing expression.
 *
 * <filename>: In procedure memoization:
 * <filename>: In file <name>, line <nr>: <error-message> in <expression>.  */

static void 
syntax_error (const char* const msg, const SCM form, const SCM expr)
{
  SCM msg_string = scm_from_locale_string (msg);
  SCM filename = SCM_BOOL_F;
  SCM linenr = SCM_BOOL_F;
  const char *format;
  SCM args;

  if (scm_is_pair (form))
    {
      filename = scm_source_property (form, scm_sym_filename);
      linenr = scm_source_property (form, scm_sym_line);
    }

  if (scm_is_false (filename) && scm_is_false (linenr) && scm_is_pair (expr))
    {
      filename = scm_source_property (expr, scm_sym_filename);
      linenr = scm_source_property (expr, scm_sym_line);
    }

  if (!SCM_UNBNDP (expr))
    {
      if (scm_is_true (filename))
	{
	  format = "In file ~S, line ~S: ~A ~S in expression ~S.";
	  args = scm_list_5 (filename, linenr, msg_string, form, expr);
	}
      else if (scm_is_true (linenr))
	{
	  format = "In line ~S: ~A ~S in expression ~S.";
	  args = scm_list_4 (linenr, msg_string, form, expr);
	}
      else
	{
	  format = "~A ~S in expression ~S.";
	  args = scm_list_3 (msg_string, form, expr);
	}
    }
  else
    {
      if (scm_is_true (filename))
	{
	  format = "In file ~S, line ~S: ~A ~S.";
	  args = scm_list_4 (filename, linenr, msg_string, form);
	}
      else if (scm_is_true (linenr))
	{
	  format = "In line ~S: ~A ~S.";
	  args = scm_list_3 (linenr, msg_string, form);
	}
      else
	{
	  format = "~A ~S.";
	  args = scm_list_2 (msg_string, form);
	}
    }

  scm_error (syntax_error_key, "memoization", format, args, SCM_BOOL_F);
}





static int
expand_env_var_is_free (SCM env, SCM x)
{
  for (; scm_is_pair (env); env = CDR (env))
    if (scm_is_eq (x, CAAR (env)))
      return 0; /* bound */
  return 1; /* free */
}

static SCM
expand_env_ref_macro (SCM env, SCM x)
{
  SCM var;
  if (!expand_env_var_is_free (env, x))
    return SCM_BOOL_F; /* lexical */

  var = scm_module_variable (scm_current_module (), x);
  if (scm_is_true (var) && scm_is_true (scm_variable_bound_p (var))
      && scm_is_true (scm_macro_p (scm_variable_ref (var))))
    return scm_variable_ref (var);
  else
    return SCM_BOOL_F; /* anything else */
}

static SCM
expand_env_lexical_gensym (SCM env, SCM name)
{
  for (; scm_is_pair (env); env = CDR (env))
    if (scm_is_eq (name, CAAR (env)))
      return CDAR (env); /* bound */
  return SCM_BOOL_F; /* free */
}

static SCM
expand_env_extend (SCM env, SCM names, SCM vars)
{
  while (scm_is_pair (names))
    {
      env = scm_acons (CAR (names), CAR (vars), env);
      names = CDR (names);
      vars = CDR (vars);
    }
  return env;
}

static SCM
expand (SCM exp, SCM env)
{
  if (scm_is_pair (exp))
    {
      SCM car;
      scm_t_macro_primitive trans = NULL;
      SCM macro = SCM_BOOL_F;
      
      car = CAR (exp);
      if (scm_is_symbol (car))
        macro = expand_env_ref_macro (env, car);
      
      if (scm_is_true (macro))
        trans = scm_i_macro_primitive (macro);

      if (trans)
        return trans (exp, env);
      else
        {
          SCM arg_exps = SCM_EOL;
          SCM args = SCM_EOL;
          SCM proc = CAR (exp);
          
          for (arg_exps = CDR (exp); scm_is_pair (arg_exps);
               arg_exps = CDR (arg_exps))
            args = scm_cons (expand (CAR (arg_exps), env), args);
          if (scm_is_null (arg_exps))
            return APPLICATION (scm_source_properties (exp),
                                expand (proc, env),
                                scm_reverse_x (args, SCM_UNDEFINED));
          else
            syntax_error ("expected a proper list", exp, SCM_UNDEFINED);
        }
    }
  else if (scm_is_symbol (exp))
    {
      SCM gensym = expand_env_lexical_gensym (env, exp);
      if (scm_is_true (gensym))
        return LEXICAL_REF (SCM_BOOL_F, exp, gensym);
      else
        return TOPLEVEL_REF (SCM_BOOL_F, exp);
    }
  else
    return CONST_ (SCM_BOOL_F, exp);
}

static SCM
expand_exprs (SCM forms, const SCM env)
{
  SCM ret = SCM_EOL;

  for (; !scm_is_null (forms); forms = CDR (forms))
    ret = scm_cons (expand (CAR (forms), env), ret);
  return scm_reverse_x (ret, SCM_UNDEFINED);
}

static SCM
expand_sequence (const SCM forms, const SCM env)
{
  ASSERT_SYNTAX (scm_ilength (forms) >= 1, s_bad_expression,
                 scm_cons (scm_sym_begin, forms));
  if (scm_is_null (CDR (forms)))
    return expand (CAR (forms), env);
  else
    return SEQUENCE (SCM_BOOL_F, expand_exprs (forms, env));
}





static SCM
expand_at (SCM expr, SCM env SCM_UNUSED)
{
  ASSERT_SYNTAX (scm_ilength (expr) == 3, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (CADR (expr)) > 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_is_symbol (CADDR (expr)), s_bad_expression, expr);

  return MODULE_REF (scm_source_properties (expr),
                     CADR (expr), CADDR (expr), SCM_BOOL_T);
}

static SCM
expand_atat (SCM expr, SCM env SCM_UNUSED)
{
  ASSERT_SYNTAX (scm_ilength (expr) == 3, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (CADR (expr)) > 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_is_symbol (CADDR (expr)), s_bad_expression, expr);

  return MODULE_REF (scm_source_properties (expr),
                     CADR (expr), CADDR (expr), SCM_BOOL_F);
}

static SCM
expand_and (SCM expr, SCM env)
{
  const SCM cdr_expr = CDR (expr);

  if (scm_is_null (cdr_expr))
    return CONST_ (SCM_BOOL_F, SCM_BOOL_T);

  ASSERT_SYNTAX (scm_is_pair (cdr_expr), s_bad_expression, expr);

  if (scm_is_null (CDR (cdr_expr)))
    return expand (CAR (cdr_expr), env);
  else
    return CONDITIONAL (scm_source_properties (expr),
                        expand (CAR (cdr_expr), env),
                        expand_and (cdr_expr, env),
                        CONST_ (SCM_BOOL_F, SCM_BOOL_F));
}

static SCM
expand_begin (SCM expr, SCM env)
{
  const SCM cdr_expr = CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 1, s_bad_expression, expr);
  return expand_sequence (cdr_expr, env);
}

static SCM
expand_cond_clauses (SCM clause, SCM rest, int elp, int alp, SCM env)
{
  SCM test;
  const long length = scm_ilength (clause);
  ASSERT_SYNTAX (length >= 1, s_bad_cond_clause, clause);

  test = CAR (clause);
  if (scm_is_eq (test, scm_sym_else) && elp)
    {
      const int last_clause_p = scm_is_null (rest);
      ASSERT_SYNTAX (length >= 2, s_bad_cond_clause, clause);
      ASSERT_SYNTAX (last_clause_p, s_misplaced_else_clause, clause);
      return expand_sequence (CDR (clause), env);
    }

  if (scm_is_null (rest))
    rest = VOID_ (SCM_BOOL_F);
  else
    rest = expand_cond_clauses (CAR (rest), CDR (rest), elp, alp, env);

  if (length >= 2
      && scm_is_eq (CADR (clause), scm_sym_arrow)
      && alp)
    {
      SCM tmp = scm_gensym (scm_from_locale_string ("cond "));
      SCM new_env = scm_acons (tmp, tmp, env);
      ASSERT_SYNTAX (length > 2, s_missing_recipient, clause);
      ASSERT_SYNTAX (length == 3, s_extra_expression, clause);
      return LET (SCM_BOOL_F,
                  scm_list_1 (tmp),
                  scm_list_1 (tmp),
                  scm_list_1 (expand (test, env)),
                  CONDITIONAL (SCM_BOOL_F,
                               LEXICAL_REF (SCM_BOOL_F, tmp, tmp),
                               APPLICATION (SCM_BOOL_F,
                                            expand (CADDR (clause), new_env),
                                            scm_list_1 (LEXICAL_REF (SCM_BOOL_F,
                                                                     tmp, tmp))),
                               rest));
    }
  /* FIXME length == 1 case */
  else
    return CONDITIONAL (SCM_BOOL_F,
                        expand (test, env),
                        expand_sequence (CDR (clause), env),
                        rest);
}
  
static SCM
expand_cond (SCM expr, SCM env)
{
  const int else_literal_p = expand_env_var_is_free (env, scm_sym_else);
  const int arrow_literal_p = expand_env_var_is_free (env, scm_sym_arrow);
  const SCM clauses = CDR (expr);

  ASSERT_SYNTAX (scm_ilength (clauses) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (clauses) >= 1, s_missing_clauses, expr);

  return expand_cond_clauses (CAR (clauses), CDR (clauses),
                              else_literal_p, arrow_literal_p, env);
}

/* lone forward decl */
static SCM expand_lambda (SCM expr, SCM env);

/* According to Section 5.2.1 of R5RS we first have to make sure that the
   variable is bound, and then perform the `(set! variable expression)'
   operation.  However, EXPRESSION _can_ be evaluated before VARIABLE is
   bound.  This means that EXPRESSION won't necessarily be able to assign
   values to VARIABLE as in `(define foo (begin (set! foo 1) (+ foo 1)))'.  */
static SCM
expand_define (SCM expr, SCM env)
{
  const SCM cdr_expr = CDR (expr);
  SCM body;
  SCM variable;

  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 2, s_missing_expression, expr);
  ASSERT_SYNTAX (!scm_is_pair (env), s_bad_define, expr);

  body = CDR (cdr_expr);
  variable = CAR (cdr_expr);

  if (scm_is_pair (variable))
    {
      ASSERT_SYNTAX_2 (scm_is_symbol (CAR (variable)), s_bad_variable, variable, expr);
      return TOPLEVEL_DEFINE
        (scm_source_properties (expr),
         CAR (variable),
         expand_lambda (scm_cons (scm_sym_lambda, scm_cons (CDR (variable), body)),
                        env));
    }
  ASSERT_SYNTAX_2 (scm_is_symbol (variable), s_bad_variable, variable, expr);
  ASSERT_SYNTAX (scm_ilength (body) == 1, s_expression, expr);
  return TOPLEVEL_DEFINE (scm_source_properties (expr), variable,
                          expand (CAR (body), env));
}

static SCM
expand_with_fluids (SCM expr, SCM env)
{
  SCM binds, fluids, vals;
  ASSERT_SYNTAX (scm_ilength (expr) >= 3, s_bad_expression, expr);
  binds = CADR (expr);
  ASSERT_SYNTAX_2 (scm_ilength (binds) >= 0, s_bad_bindings, binds, expr);
  for (fluids = SCM_EOL, vals = SCM_EOL;
       scm_is_pair (binds);
       binds = CDR (binds))
    {
      SCM binding = CAR (binds);
      ASSERT_SYNTAX_2 (scm_ilength (CAR (binds)) == 2, s_bad_binding,
                       binding, expr);
      fluids = scm_cons (expand (CAR (binding), env), fluids);
      vals = scm_cons (expand (CADR (binding), env), vals);
    }

  return DYNLET (scm_source_properties (expr),
                 scm_reverse_x (fluids, SCM_UNDEFINED),
                 scm_reverse_x (vals, SCM_UNDEFINED),
                 expand_sequence (CDDR (expr), env));
}

static SCM
expand_eval_when (SCM expr, SCM env)
{
  ASSERT_SYNTAX (scm_ilength (expr) >= 3, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (CADR (expr)) > 0, s_bad_expression, expr);

  if (scm_is_true (scm_memq (sym_eval, CADR (expr)))
      || scm_is_true (scm_memq (sym_load, CADR (expr))))
    return expand_sequence (CDDR (expr), env);
  else
    return VOID_ (scm_source_properties (expr));
}

static SCM
expand_if (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = CDR (expr);
  const long length = scm_ilength (cdr_expr);
  ASSERT_SYNTAX (length == 2 || length == 3, s_expression, expr);
  return CONDITIONAL (scm_source_properties (expr),
                      expand (CADR (expr), env),
                      expand (CADDR (expr), env),
                      ((length == 3)
                       ? expand (CADDDR (expr), env)
                       : VOID_ (SCM_BOOL_F)));
}

/* A helper function for expand_lambda to support checking for duplicate
 * formal arguments: Return true if OBJ is `eq?' to one of the elements of
 * LIST or to the CDR of the last cons.  Therefore, LIST may have any of the
 * forms that a formal argument can have:
 *   <rest>, (<arg1> ...), (<arg1> ...  .  <rest>) */
static int
c_improper_memq (SCM obj, SCM list)
{
  for (; scm_is_pair (list); list = CDR (list))
    {
      if (scm_is_eq (CAR (list), obj))
        return 1;
    }
  return scm_is_eq (list, obj);
}

static SCM
expand_lambda_case (SCM clause, SCM alternate, SCM env)
{
  SCM formals;
  SCM rest;
  SCM req = SCM_EOL;
  SCM vars = SCM_EOL;
  SCM body;
  int nreq = 0;

  ASSERT_SYNTAX (scm_is_pair (clause) && scm_is_pair (CDR (clause)),
                 s_bad_expression, scm_cons (scm_sym_lambda, clause));

  /* Before iterating the list of formal arguments, make sure the formals
   * actually are given as either a symbol or a non-cyclic list.  */
  formals = CAR (clause);
  if (scm_is_pair (formals))
    {
      /* Dirk:FIXME:: We should check for a cyclic list of formals, and if
       * detected, report a 'Bad formals' error.  */
    }
  else
    ASSERT_SYNTAX_2 (scm_is_symbol (formals) || scm_is_null (formals),
                     s_bad_formals, formals, scm_cons (scm_sym_lambda, clause));

  /* Now iterate the list of formal arguments to check if all formals are
   * symbols, and that there are no duplicates.  */
  while (scm_is_pair (formals))
    {
      const SCM formal = CAR (formals);
      formals = CDR (formals);
      ASSERT_SYNTAX_2 (scm_is_symbol (formal), s_bad_formal, formal,
                       scm_cons (scm_sym_lambda, clause));
      ASSERT_SYNTAX_2 (!c_improper_memq (formal, formals), s_duplicate_formal,
                       formal, scm_cons (scm_sym_lambda, clause));
      nreq++;
      req = scm_cons (formal, req);
      vars = scm_cons (scm_gensym (SCM_UNDEFINED), vars);
      env = scm_acons (formal, CAR (vars), env);
    }

  ASSERT_SYNTAX_2 (scm_is_null (formals) || scm_is_symbol (formals),
                   s_bad_formal, formals, scm_cons (scm_sym_lambda, clause));
  if (scm_is_symbol (formals))
    {
      rest = formals;
      vars = scm_cons (scm_gensym (SCM_UNDEFINED), vars);
      env = scm_acons (rest, CAR (vars), env);
    }
  else
    rest = SCM_BOOL_F;
  
  body = expand_sequence (CDR (clause), env);
  req = scm_reverse_x (req, SCM_UNDEFINED);
  vars = scm_reverse_x (vars, SCM_UNDEFINED);

  if (scm_is_true (alternate) && !(SCM_EXPANDED_P (alternate) && SCM_EXPANDED_TYPE (alternate) == SCM_EXPANDED_LAMBDA_CASE))
    abort ();
    
  return LAMBDA_CASE (SCM_BOOL_F, req, SCM_BOOL_F, rest, SCM_BOOL_F,
                      SCM_EOL, vars, body, alternate);
}

static SCM
expand_lambda (SCM expr, SCM env)
{
  return LAMBDA (scm_source_properties (expr),
                 SCM_EOL,
                 expand_lambda_case (CDR (expr), SCM_BOOL_F, env));
}

static SCM
expand_lambda_star_case (SCM clause, SCM alternate, SCM env)
{
  SCM req, opt, kw, allow_other_keys, rest, formals, vars, body, tmp;
  SCM inits;
  int nreq, nopt;

  const long length = scm_ilength (clause);
  ASSERT_SYNTAX (length >= 1, s_bad_expression,
                 scm_cons (sym_lambda_star, clause));
  ASSERT_SYNTAX (length >= 2, s_missing_expression,
                 scm_cons (sym_lambda_star, clause));

  formals = CAR (clause);
  body = CDR (clause);

  nreq = nopt = 0;
  req = opt = kw = SCM_EOL;
  rest = allow_other_keys = SCM_BOOL_F;

  while (scm_is_pair (formals) && scm_is_symbol (CAR (formals)))
    {
      nreq++;
      req = scm_cons (CAR (formals), req);
      formals = scm_cdr (formals);
    }

  if (scm_is_pair (formals) && scm_is_eq (CAR (formals), kw_optional))
    {
      formals = CDR (formals);
      while (scm_is_pair (formals)
             && (scm_is_symbol (CAR (formals)) || scm_is_pair (CAR (formals))))
        {
          nopt++;
          opt = scm_cons (CAR (formals), opt);
          formals = scm_cdr (formals);
        }
    }
  
  if (scm_is_pair (formals) && scm_is_eq (CAR (formals), kw_key))
    {
      formals = CDR (formals);
      while (scm_is_pair (formals)
             && (scm_is_symbol (CAR (formals)) || scm_is_pair (CAR (formals))))
        {
          kw = scm_cons (CAR (formals), kw);
          formals = scm_cdr (formals);
        }
    }
  
  if (scm_is_pair (formals) && scm_is_eq (CAR (formals), kw_allow_other_keys))
    {
      formals = CDR (formals);
      allow_other_keys = SCM_BOOL_T;
    }
  
  if (scm_is_pair (formals) && scm_is_eq (CAR (formals), kw_rest))
    {
      ASSERT_SYNTAX (scm_ilength (formals) == 2, s_bad_formals,
                     CAR (clause));
      rest = CADR (formals);
    }
  else if (scm_is_symbol (formals))
    rest = formals;
  else
    {
      ASSERT_SYNTAX (scm_is_null (formals), s_bad_formals, CAR (clause));
      rest = SCM_BOOL_F;
    }
  
  /* Now, iterate through them a second time, building up an expansion-time
     environment, checking, expanding and canonicalizing the opt/kw init forms,
     and eventually memoizing the body as well. Note that the rest argument, if
     any, is expanded before keyword args, thus necessitating the second
     pass.

     Also note that the specific environment during expansion of init
     expressions here needs to coincide with the environment when psyntax
     expands. A lot of effort for something that is only used in the bootstrap
     expandr, you say? Yes. Yes it is.
  */

  vars = SCM_EOL;
  req = scm_reverse_x (req, SCM_EOL);
  for (tmp = req; scm_is_pair (tmp); tmp = scm_cdr (tmp))
    {
      vars = scm_cons (scm_gensym (SCM_UNDEFINED), vars);
      env = scm_acons (CAR (tmp), CAR (vars), env);
    }
  
  /* Build up opt inits and env */
  inits = SCM_EOL;
  opt = scm_reverse_x (opt, SCM_EOL);
  for (tmp = opt; scm_is_pair (tmp); tmp = scm_cdr (tmp))
    {
      SCM x = CAR (tmp);
      vars = scm_cons (scm_gensym (SCM_UNDEFINED), vars);
      env = scm_acons (x, CAR (vars), env);
      if (scm_is_symbol (x))
        inits = scm_cons (CONST_ (SCM_BOOL_F, SCM_BOOL_F), inits);
      else
        {
          ASSERT_SYNTAX (scm_ilength (x) == 2 && scm_is_symbol (CAR (x)),
                         s_bad_formals, CAR (clause));
          inits = scm_cons (expand (CADR (x), env), inits);
        }
      env = scm_acons (scm_is_symbol (x) ? x : CAR (x), CAR (vars), env);
    }
  if (scm_is_null (opt))
    opt = SCM_BOOL_F;
      
  /* Process rest before keyword args */
  if (scm_is_true (rest))
    {
      vars = scm_cons (scm_gensym (SCM_UNDEFINED), vars);
      env = scm_acons (rest, CAR (vars), env);
    }

  /* Build up kw inits, env, and kw-canon list */
  if (scm_is_null (kw))
    kw = SCM_BOOL_F;
  else
    {
      SCM kw_canon = SCM_EOL;
      kw = scm_reverse_x (kw, SCM_UNDEFINED);
      for (tmp = kw; scm_is_pair (tmp); tmp = scm_cdr (tmp))
        {
          SCM x, sym, k, init;
          x = CAR (tmp);
          if (scm_is_symbol (x))
            {
              sym = x;
              init = SCM_BOOL_F;
              k = scm_symbol_to_keyword (sym);
            }
          else if (scm_ilength (x) == 2 && scm_is_symbol (CAR (x)))
            {
              sym = CAR (x);
              init = CADR (x);
              k = scm_symbol_to_keyword (sym);
            }
          else if (scm_ilength (x) == 3 && scm_is_symbol (CAR (x))
                   && scm_is_keyword (CADDR (x)))
            {
              sym = CAR (x);
              init = CADR (x);
              k = CADDR (x);
            }
          else
            syntax_error (s_bad_formals, CAR (clause), SCM_UNDEFINED);

          inits = scm_cons (expand (init, env), inits);
          vars = scm_cons (scm_gensym (SCM_UNDEFINED), vars);
          kw_canon = scm_cons (scm_list_3 (k, sym, CAR (vars)), kw_canon);
          env = scm_acons (sym, CAR (vars), env);
        }
      kw_canon = scm_reverse_x (kw_canon, SCM_UNDEFINED);
      kw = scm_cons (allow_other_keys, kw_canon);
    }

  /* We should check for no duplicates, but given that psyntax does this
     already, we can punt on it here... */

  vars = scm_reverse_x (vars, SCM_UNDEFINED);
  inits = scm_reverse_x (inits, SCM_UNDEFINED);
  body = expand_sequence (body, env);

  return LAMBDA_CASE (SCM_BOOL_F, req, opt, rest, kw, inits, vars, body,
                      alternate);
}

static SCM
expand_lambda_star (SCM expr, SCM env)
{
  return LAMBDA (scm_source_properties (expr),
                 SCM_EOL,
                 expand_lambda_star_case (CDR (expr), SCM_BOOL_F, env));
}

static SCM
expand_case_lambda_clauses (SCM expr, SCM rest, SCM env)
{
  SCM alt;

  if (scm_is_pair (rest))
    alt = expand_case_lambda_clauses (CAR (rest), CDR (rest), env);
  else
    alt = SCM_BOOL_F;
  
  return expand_lambda_case (expr, alt, env);
}

static SCM
expand_case_lambda (SCM expr, SCM env)
{
  ASSERT_SYNTAX (scm_is_pair (CDR (expr)), s_missing_expression, expr);

  return LAMBDA (scm_source_properties (expr),
                 SCM_EOL,
                 expand_case_lambda_clauses (CADR (expr), CDDR (expr), env));
}

static SCM
expand_case_lambda_star_clauses (SCM expr, SCM rest, SCM env)
{
  SCM alt;

  if (scm_is_pair (rest))
    alt = expand_case_lambda_star_clauses (CAR (rest), CDR (rest), env);
  else
    alt = SCM_BOOL_F;
  
  return expand_lambda_star_case (expr, alt, env);
}

static SCM
expand_case_lambda_star (SCM expr, SCM env)
{
  ASSERT_SYNTAX (scm_is_pair (CDR (expr)), s_missing_expression, expr);

  return LAMBDA (scm_source_properties (expr),
                 SCM_EOL,
                 expand_case_lambda_star_clauses (CADR (expr), CDDR (expr), env));
}

/* Check if the format of the bindings is ((<symbol> <init-form>) ...).  */
static void
check_bindings (const SCM bindings, const SCM expr)
{
  SCM binding_idx;

  ASSERT_SYNTAX_2 (scm_ilength (bindings) >= 0,
                   s_bad_bindings, bindings, expr);

  binding_idx = bindings;
  for (; !scm_is_null (binding_idx); binding_idx = CDR (binding_idx))
    {
      SCM name;         /* const */

      const SCM binding = CAR (binding_idx);
      ASSERT_SYNTAX_2 (scm_ilength (binding) == 2,
                       s_bad_binding, binding, expr);

      name = CAR (binding);
      ASSERT_SYNTAX_2 (scm_is_symbol (name), s_bad_variable, name, expr);
    }
}

/* The bindings, which must have the format ((v1 i1) (v2 i2) ... (vn in)), are
 * transformed to the lists (vn .. v2 v1) and (i1 i2 ... in). If a duplicate
 * variable name is detected, an error is signalled. */
static void
transform_bindings (const SCM bindings, const SCM expr,
                    SCM *const names, SCM *const vars, SCM *const initptr)
{
  SCM rnames = SCM_EOL;
  SCM rvars = SCM_EOL;
  SCM rinits = SCM_EOL;
  SCM binding_idx = bindings;
  for (; !scm_is_null (binding_idx); binding_idx = CDR (binding_idx))
    {
      const SCM binding = CAR (binding_idx);
      const SCM CDR_binding = CDR (binding);
      const SCM name = CAR (binding);
      ASSERT_SYNTAX_2 (scm_is_false (scm_c_memq (name, rnames)),
                       s_duplicate_binding, name, expr);
      rnames = scm_cons (name, rnames);
      rvars = scm_cons (scm_gensym (SCM_UNDEFINED), rvars);
      rinits = scm_cons (CAR (CDR_binding), rinits);
    }
  *names = scm_reverse_x (rnames, SCM_UNDEFINED);
  *vars = scm_reverse_x (rvars, SCM_UNDEFINED);
  *initptr = scm_reverse_x (rinits, SCM_UNDEFINED);
}

/* FIXME: Remove named let in this boot expander. */
static SCM
expand_named_let (const SCM expr, SCM env)
{
  SCM var_names, var_syms, inits;
  SCM inner_env;
  SCM name_sym;

  const SCM cdr_expr = CDR (expr);
  const SCM name = CAR (cdr_expr);
  const SCM cddr_expr = CDR (cdr_expr);
  const SCM bindings = CAR (cddr_expr);
  check_bindings (bindings, expr);

  transform_bindings (bindings, expr, &var_names, &var_syms, &inits);
  name_sym = scm_gensym (SCM_UNDEFINED);
  inner_env = scm_acons (name, name_sym, env);
  inner_env = expand_env_extend (inner_env, var_names, var_syms);

  return LETREC
    (scm_source_properties (expr), SCM_BOOL_F,
     scm_list_1 (name), scm_list_1 (name_sym),
     scm_list_1 (LAMBDA (SCM_BOOL_F,
                         SCM_EOL,
                         LAMBDA_CASE (SCM_BOOL_F, var_names, SCM_BOOL_F, SCM_BOOL_F,
                                      SCM_BOOL_F, SCM_BOOL_F, var_syms,
                                      expand_sequence (CDDDR (expr), inner_env),
                                      SCM_BOOL_F))),
     APPLICATION (SCM_BOOL_F,
                  LEXICAL_REF (SCM_BOOL_F, name, name_sym),
                  expand_exprs (inits, env)));
}

static SCM
expand_let (SCM expr, SCM env)
{
  SCM bindings;

  const SCM cdr_expr = CDR (expr);
  const long length = scm_ilength (cdr_expr);
  ASSERT_SYNTAX (length >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (length >= 2, s_missing_expression, expr);

  bindings = CAR (cdr_expr);
  if (scm_is_symbol (bindings))
    {
      ASSERT_SYNTAX (length >= 3, s_missing_expression, expr);
      return expand_named_let (expr, env);
    }

  check_bindings (bindings, expr);
  if (scm_is_null (bindings))
    return expand_sequence (CDDR (expr), env);
  else
    {
      SCM var_names, var_syms, inits;
      transform_bindings (bindings, expr, &var_names, &var_syms, &inits);
      return LET (SCM_BOOL_F,
                  var_names, var_syms, expand_exprs (inits, env),
                  expand_sequence (CDDR (expr),
                                   expand_env_extend (env, var_names,
                                                      var_syms)));
    }
}

static SCM
expand_letrec_helper (SCM expr, SCM env, SCM in_order_p)
{
  SCM bindings;

  const SCM cdr_expr = CDR (expr);
  const long length = scm_ilength (cdr_expr);
  ASSERT_SYNTAX (length >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (length >= 2, s_missing_expression, expr);

  bindings = CAR (cdr_expr);
  check_bindings (bindings, expr);
  if (scm_is_null (bindings))
    return expand_sequence (CDDR (expr), env);
  else
    {
      SCM var_names, var_syms, inits;
      transform_bindings (bindings, expr, &var_names, &var_syms, &inits);
      env = expand_env_extend (env, var_names, var_syms);
      return LETREC (SCM_BOOL_F, in_order_p,
                     var_names, var_syms, expand_exprs (inits, env),
                     expand_sequence (CDDR (expr), env));
    }
}

static SCM
expand_letrec (SCM expr, SCM env)
{
  return expand_letrec_helper (expr, env, SCM_BOOL_F);
}

static SCM
expand_letrec_star (SCM expr, SCM env)
{
  return expand_letrec_helper (expr, env, SCM_BOOL_T);
}

static SCM
expand_letstar_clause (SCM bindings, SCM body, SCM env SCM_UNUSED)
{
  if (scm_is_null (bindings))
    return expand_sequence (body, env);
  else
    {
      SCM bind, name, sym, init;

      ASSERT_SYNTAX (scm_is_pair (bindings), s_bad_expression, bindings);
      bind = CAR (bindings);
      ASSERT_SYNTAX (scm_ilength (bind) == 2, s_bad_binding, bind);
      name = CAR (bind);
      sym = scm_gensym (SCM_UNDEFINED);
      init = CADR (bind);
      
      return LET (SCM_BOOL_F, scm_list_1 (name), scm_list_1 (sym),
                  scm_list_1 (expand (init, env)),
                  expand_letstar_clause (CDR (bindings), body,
                                         scm_acons (name, sym, env)));
    }
}

static SCM
expand_letstar (SCM expr, SCM env SCM_UNUSED)
{
  const SCM cdr_expr = CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 2, s_missing_expression, expr);

  return expand_letstar_clause (CADR (expr), CDDR (expr), env);
}

static SCM
expand_or (SCM expr, SCM env SCM_UNUSED)
{
  SCM tail = CDR (expr);
  const long length = scm_ilength (tail);

  ASSERT_SYNTAX (length >= 0, s_bad_expression, expr);

  if (scm_is_null (CDR (expr)))
    return CONST_ (SCM_BOOL_F, SCM_BOOL_F);
  else
    {
      SCM tmp = scm_gensym (SCM_UNDEFINED);
      return LET (SCM_BOOL_F,
                  scm_list_1 (tmp), scm_list_1 (tmp),
                  scm_list_1 (expand (CADR (expr), env)),
                  CONDITIONAL (SCM_BOOL_F,
                               LEXICAL_REF (SCM_BOOL_F, tmp, tmp),
                               LEXICAL_REF (SCM_BOOL_F, tmp, tmp),
                               expand_or (CDR (expr),
                                          scm_acons (tmp, tmp, env))));
    }
}

static SCM
expand_quote (SCM expr, SCM env SCM_UNUSED)
{
  SCM quotee;

  const SCM cdr_expr = CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 1, s_expression, expr);
  quotee = CAR (cdr_expr);
  return CONST_ (scm_source_properties (expr), quotee);
}

static SCM
expand_set_x (SCM expr, SCM env)
{
  SCM variable;
  SCM vmem;

  const SCM cdr_expr = CDR (expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) >= 0, s_bad_expression, expr);
  ASSERT_SYNTAX (scm_ilength (cdr_expr) == 2, s_expression, expr);
  variable = CAR (cdr_expr);
  vmem = expand (variable, env);
  
  switch (SCM_EXPANDED_TYPE (vmem))
    {
    case SCM_EXPANDED_LEXICAL_REF:
      return LEXICAL_SET (scm_source_properties (expr),
                          SCM_EXPANDED_REF (vmem, LEXICAL_REF, NAME),
                          SCM_EXPANDED_REF (vmem, LEXICAL_REF, GENSYM),
                          expand (CADDR (expr), env));
    case SCM_EXPANDED_TOPLEVEL_REF:
      return TOPLEVEL_SET (scm_source_properties (expr),
                           SCM_EXPANDED_REF (vmem, TOPLEVEL_REF, NAME),
                           expand (CADDR (expr), env));
    case SCM_EXPANDED_MODULE_REF:
      return MODULE_SET (scm_source_properties (expr),
                         SCM_EXPANDED_REF (vmem, MODULE_REF, MOD),
                         SCM_EXPANDED_REF (vmem, MODULE_REF, NAME),
                         SCM_EXPANDED_REF (vmem, MODULE_REF, PUBLIC),
                         expand (CADDR (expr), env));
    default:
      syntax_error (s_bad_variable, variable, expr);
    }
}




/* This is the boot expander. It is later replaced with psyntax's sc-expand. */
SCM_DEFINE (scm_macroexpand, "macroexpand", 1, 0, 0, 
            (SCM exp),
	    "Expand the expression @var{exp}.")
#define FUNC_NAME s_scm_macroexpand
{
  return expand (exp, scm_current_module ());
}
#undef FUNC_NAME

SCM_DEFINE (scm_macroexpanded_p, "macroexpanded?", 1, 0, 0, 
            (SCM exp),
	    "Return @code{#t} if @var{exp} is an expanded expression.")
#define FUNC_NAME s_scm_macroexpanded_p
{
  return scm_from_bool (SCM_EXPANDED_P (exp));
}
#undef FUNC_NAME


 

#define DEFINE_NAMES(type)                                              \
  {                                                                     \
    static const char *fields[] = SCM_EXPANDED_##type##_FIELD_NAMES;    \
    exp_field_names[SCM_EXPANDED_##type] = fields;                      \
    exp_names[SCM_EXPANDED_##type] = SCM_EXPANDED_##type##_TYPE_NAME;   \
    exp_nfields[SCM_EXPANDED_##type] = SCM_NUM_EXPANDED_##type##_FIELDS; \
  }

static SCM
make_exp_vtable (size_t n)
{
  SCM layout, printer, name, code, fields;
  
  layout = scm_string_to_symbol
    (scm_string_append (scm_make_list (scm_from_size_t (exp_nfields[n]),
                                       scm_from_locale_string ("pw"))));
  printer = SCM_BOOL_F;
  name = scm_from_locale_symbol (exp_names[n]);
  code = scm_from_size_t (n);
  fields = SCM_EOL;
  {
    size_t m = exp_nfields[n];
    while (m--)
      fields = scm_cons (scm_from_locale_symbol (exp_field_names[n][m]), fields);
  }

  return scm_c_make_struct (scm_exp_vtable_vtable, 0, 5,
                            SCM_UNPACK (layout), SCM_UNPACK (printer), SCM_UNPACK (name),
                            SCM_UNPACK (code), SCM_UNPACK (fields));
}

void
scm_init_expand ()
{
  size_t n;
  SCM exp_vtable_list = SCM_EOL;

  DEFINE_NAMES (VOID);
  DEFINE_NAMES (CONST);
  DEFINE_NAMES (PRIMITIVE_REF);
  DEFINE_NAMES (LEXICAL_REF);
  DEFINE_NAMES (LEXICAL_SET);
  DEFINE_NAMES (MODULE_REF);
  DEFINE_NAMES (MODULE_SET);
  DEFINE_NAMES (TOPLEVEL_REF);
  DEFINE_NAMES (TOPLEVEL_SET);
  DEFINE_NAMES (TOPLEVEL_DEFINE);
  DEFINE_NAMES (CONDITIONAL);
  DEFINE_NAMES (APPLICATION);
  DEFINE_NAMES (SEQUENCE);
  DEFINE_NAMES (LAMBDA);
  DEFINE_NAMES (LAMBDA_CASE);
  DEFINE_NAMES (LET);
  DEFINE_NAMES (LETREC);
  DEFINE_NAMES (DYNLET);

  scm_exp_vtable_vtable =
    scm_make_vtable (scm_from_locale_string (SCM_VTABLE_BASE_LAYOUT "pwuwpw"),
                     SCM_BOOL_F);

  for (n = 0; n < SCM_NUM_EXPANDED_TYPES; n++)
    exp_vtables[n] = make_exp_vtable (n);
  
  /* Now walk back down, consing in reverse. */
  while (n--)
    exp_vtable_list = scm_cons (exp_vtables[n], exp_vtable_list);

  scm_c_define ("%expanded-vtables", scm_vector (exp_vtable_list));
  
#include "libguile/expand.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
