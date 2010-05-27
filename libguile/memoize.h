/* classes: h_files */

#ifndef SCM_MEMOIZE_H
#define SCM_MEMOIZE_H

/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002,2004,2008,2009,2010
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



#include "libguile/__scm.h"




SCM_API SCM scm_sym_and;
SCM_API SCM scm_sym_begin;
SCM_API SCM scm_sym_case;
SCM_API SCM scm_sym_cond;
SCM_API SCM scm_sym_define;
SCM_API SCM scm_sym_do;
SCM_API SCM scm_sym_if;
SCM_API SCM scm_sym_lambda;
SCM_API SCM scm_sym_let;
SCM_API SCM scm_sym_letstar;
SCM_API SCM scm_sym_letrec;
SCM_API SCM scm_sym_quote;
SCM_API SCM scm_sym_quasiquote;
SCM_API SCM scm_sym_unquote;
SCM_API SCM scm_sym_uq_splicing;
SCM_API SCM scm_sym_with_fluids;

SCM_API SCM scm_sym_at;
SCM_API SCM scm_sym_atat;
SCM_API SCM scm_sym_atapply;
SCM_API SCM scm_sym_atcall_cc;
SCM_API SCM scm_sym_at_call_with_values;
SCM_API SCM scm_sym_at_prompt;
SCM_API SCM scm_sym_delay;
SCM_API SCM scm_sym_at_dynamic_wind;
SCM_API SCM scm_sym_eval_when;
SCM_API SCM scm_sym_arrow;
SCM_API SCM scm_sym_else;
SCM_API SCM scm_sym_apply;
SCM_API SCM scm_sym_set_x;
SCM_API SCM scm_sym_args;

/* {Memoized Source}
 */

SCM_INTERNAL scm_t_bits scm_tc16_memoized;

#define SCM_MEMOIZED_P(x) 	(SCM_SMOB_PREDICATE (scm_tc16_memoized, (x)))
#define SCM_MEMOIZED_TAG(x) 	(SCM_SMOB_FLAGS (x))
#define SCM_MEMOIZED_ARGS(x) 	(SCM_SMOB_OBJECT (x))

enum
  {
    SCM_M_BEGIN,
    SCM_M_IF,
    SCM_M_LAMBDA,
    SCM_M_LET,
    SCM_M_QUOTE,
    SCM_M_DEFINE,
    SCM_M_DYNWIND,
    SCM_M_WITH_FLUIDS,
    SCM_M_APPLY,
    SCM_M_CONT,
    SCM_M_CALL_WITH_VALUES,
    SCM_M_CALL,
    SCM_M_LEXICAL_REF,
    SCM_M_LEXICAL_SET,
    SCM_M_TOPLEVEL_REF,
    SCM_M_TOPLEVEL_SET,
    SCM_M_MODULE_REF,
    SCM_M_MODULE_SET,
    SCM_M_PROMPT
  };




SCM_INTERNAL SCM scm_memoize_expression (SCM exp);
SCM_INTERNAL SCM scm_unmemoize_expression (SCM memoized);
SCM_INTERNAL SCM scm_memoized_expression_typecode (SCM memoized);
SCM_INTERNAL SCM scm_memoized_expression_data (SCM memoized);
SCM_INTERNAL SCM scm_memoized_typecode (SCM sym);
SCM_INTERNAL SCM scm_memoize_variable_access_x (SCM memoized, SCM module);
SCM_API SCM scm_memoized_p (SCM obj);

SCM_INTERNAL void scm_init_memoize (void);


#endif  /* SCM_MEMOIZE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
