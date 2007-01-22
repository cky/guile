#undef RETURN
#undef ENTER_APPLY
#undef PREP_APPLY
#undef CEVAL
#undef SCM_APPLY
#undef EVAL_DEBUGGING_P


#ifdef DEVAL

/*
  This code is specific for the debugging support.
 */

#define EVAL_DEBUGGING_P 1
#define CEVAL deval	/* Substitute all uses of ceval */
#define SCM_APPLY scm_dapply
#define PREP_APPLY(p, l)						\
{ ++debug.info; debug.info->a.proc = p; debug.info->a.args = l; }

#define ENTER_APPLY \
do { \
  SCM_SET_ARGSREADY (debug);\
  if (scm_check_apply_p && SCM_TRAPS_P)\
    if (SCM_APPLY_FRAME_P || (SCM_TRACE_P && SCM_PROCTRACEP (proc)))\
      {\
	SCM tmp, tail = scm_from_bool(SCM_TRACED_FRAME_P (debug)); \
	SCM_SET_TRACED_FRAME (debug); \
	SCM_TRAPS_P = 0;\
        tmp = scm_make_debugobj (&debug);\
	scm_call_3 (SCM_APPLY_FRAME_HDLR, scm_sym_apply_frame, tmp, tail);\
	SCM_TRAPS_P = 1;\
      }\
} while (0)

#define RETURN(e) do { proc = (e); goto exit; } while (0)

#ifdef STACK_CHECKING
# ifndef EVAL_STACK_CHECKING
# define EVAL_STACK_CHECKING
# endif /* EVAL_STACK_CHECKING */
#endif /* STACK_CHECKING */




static SCM
deval_args (SCM l, SCM env, SCM proc, SCM *lloc)
{
  SCM *results = lloc;
  while (scm_is_pair (l))
    {
      const SCM res = SCM_I_XEVALCAR (l, env, 1);

      *lloc = scm_list_1 (res);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
  if (!scm_is_null (l))
    scm_wrong_num_args (proc);
  return *results;
}


#else /* DEVAL */

/*
  Code is specific to debugging-less support.
 */


#define CEVAL ceval
#define SCM_APPLY scm_apply
#define PREP_APPLY(proc, args)
#define ENTER_APPLY
#define RETURN(x) do { return x; } while (0)
#define EVAL_DEBUGGING_P 0

#ifdef STACK_CHECKING
# ifndef NO_CEVAL_STACK_CHECKING
# define EVAL_STACK_CHECKING
# endif
#endif




static void
ceval_letrec_inits (SCM env, SCM init_forms, SCM **init_values_eol)
{
  SCM argv[10];
  int i = 0, imax = sizeof (argv) / sizeof (SCM);

  while (!scm_is_null (init_forms))
    {
      if (imax == i)
	{
	  ceval_letrec_inits (env, init_forms, init_values_eol);
	  break;
	}
      argv[i++] = SCM_I_XEVALCAR (init_forms, env, 0);
      init_forms = SCM_CDR (init_forms);
    }
 
  for (i--; i >= 0; i--)
    {
      **init_values_eol = scm_list_1 (argv[i]);
      *init_values_eol = SCM_CDRLOC (**init_values_eol);
    }
}

static SCM 
scm_ceval_args (SCM l, SCM env, SCM proc)
{
  SCM results = SCM_EOL, *lloc = &results, res;
  while (scm_is_pair (l))
    {
      res = EVALCAR (l, env);

      *lloc = scm_list_1 (res);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
  if (!scm_is_null (l))
    scm_wrong_num_args (proc);
  return results;
}


SCM 
scm_eval_args (SCM l, SCM env, SCM proc)
{
  return scm_ceval_args (l, env, proc);
}



#endif




#define EVAL(x, env) SCM_I_XEVAL(x, env, EVAL_DEBUGGING_P)
#define EVALCAR(x, env) SCM_I_XEVALCAR(x, env, EVAL_DEBUGGING_P)



/* Update the toplevel environment frame ENV so that it refers to the
 * current module.  */
#define UPDATE_TOPLEVEL_ENV(env) \
  do { \
    SCM p = scm_current_module_lookup_closure (); \
    if (p != SCM_CAR (env)) \
      env = scm_top_level_env (p); \
  } while (0)


#define SCM_VALIDATE_NON_EMPTY_COMBINATION(x) \
  ASSERT_SYNTAX (!scm_is_eq ((x), SCM_EOL), s_empty_combination, x)


/* This is the evaluator.  Like any real monster, it has three heads:
 *
 * ceval is the non-debugging evaluator, deval is the debugging version.  Both
 * are implemented using a common code base, using the following mechanism:
 * CEVAL is a macro, which is either defined to ceval or deval.  Thus, there
 * is no function CEVAL, but the code for CEVAL actually compiles to either
 * ceval or deval.  When CEVAL is defined to ceval, it is known that the macro
 * DEVAL is not defined.  When CEVAL is defined to deval, then the macro DEVAL
 * is known to be defined.  Thus, in CEVAL parts for the debugging evaluator
 * are enclosed within #ifdef DEVAL ... #endif.
 *
 * All three (ceval, deval and their common implementation CEVAL) take two
 * input parameters, x and env: x is a single expression to be evalutated.
 * env is the environment in which bindings are searched.
 *
 * x is known to be a pair.  Since x is a single expression, it is necessarily
 * in a tail position.  If x is just a call to another function like in the
 * expression (foo exp1 exp2 ...), the realization of that call therefore
 * _must_not_ increase stack usage (the evaluation of exp1, exp2 etc.,
 * however, may do so).  This is realized by making extensive use of 'goto'
 * statements within the evaluator: The gotos replace recursive calls to
 * CEVAL, thus re-using the same stack frame that CEVAL was already using.
 * If, however, x represents some form that requires to evaluate a sequence of
 * expressions like (begin exp1 exp2 ...), then recursive calls to CEVAL are
 * performed for all but the last expression of that sequence.  */

static SCM
CEVAL (SCM x, SCM env)
{
  SCM proc, arg1;
#ifdef DEVAL
  scm_t_debug_frame debug;
  scm_t_debug_info *debug_info_end;
  debug.prev = scm_i_last_debug_frame ();
  debug.status = 0;
  /*
   * The debug.vect contains twice as much scm_t_debug_info frames as the
   * user has specified with (debug-set! frames <n>).
   *
   * Even frames are eval frames, odd frames are apply frames.
   */
  debug.vect = (scm_t_debug_info *) alloca (scm_debug_eframe_size
					    * sizeof (scm_t_debug_info));
  debug.info = debug.vect;
  debug_info_end = debug.vect + scm_debug_eframe_size;
  scm_i_set_last_debug_frame (&debug);
#endif
#ifdef EVAL_STACK_CHECKING
  if (scm_stack_checking_enabled_p && SCM_STACK_OVERFLOW_P (&proc))
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

loop:
#ifdef DEVAL
  SCM_CLEAR_ARGSREADY (debug);
  if (SCM_OVERFLOWP (debug))
    --debug.info;
  /*
   * In theory, this should be the only place where it is necessary to
   * check for space in debug.vect since both eval frames and
   * available space are even.
   *
   * For this to be the case, however, it is necessary that primitive
   * special forms which jump back to `loop', `begin' or some similar
   * label call PREP_APPLY.
   */
  else if (++debug.info >= debug_info_end)
    {
      SCM_SET_OVERFLOW (debug);
      debug.info -= 2;
    }

start:
  debug.info->e.exp = x;
  debug.info->e.env = env;
  if (scm_check_entry_p && SCM_TRAPS_P)
    {
      if (SCM_ENTER_FRAME_P
	  || (SCM_BREAKPOINTS_P && scm_c_source_property_breakpoint_p (x)))
	{
	  SCM stackrep;
	  SCM tail = scm_from_bool (SCM_TAILRECP (debug));
	  SCM_SET_TAILREC (debug);
	  stackrep = scm_make_debugobj (&debug);
	  SCM_TRAPS_P = 0;
	  stackrep = scm_call_4 (SCM_ENTER_FRAME_HDLR,
				 scm_sym_enter_frame,
				 stackrep,
				 tail,
				 unmemoize_expression (x, env));
	  SCM_TRAPS_P = 1;
	  if (scm_is_pair (stackrep) &&
	      scm_is_eq (SCM_CAR (stackrep), sym_instead))
	    {
	      /* This gives the possibility for the debugger to modify
		 the source expression before evaluation. */
	      x = SCM_CDR (stackrep);
	      if (SCM_IMP (x))
		RETURN (x);
	    }
	}
    }
#endif
dispatch:
  SCM_TICK;
  if (SCM_ISYMP (SCM_CAR (x)))
    {
      switch (ISYMNUM (SCM_CAR (x)))
        {
        case (ISYMNUM (SCM_IM_AND)):
          x = SCM_CDR (x);
          while (!scm_is_null (SCM_CDR (x)))
            {
              SCM test_result = EVALCAR (x, env);
              if (scm_is_false (test_result) || SCM_NILP (test_result))
                RETURN (SCM_BOOL_F);
              else
                x = SCM_CDR (x);
            }
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto carloop;

        case (ISYMNUM (SCM_IM_BEGIN)):
          x = SCM_CDR (x);
          if (scm_is_null (x))
            RETURN (SCM_UNSPECIFIED);

          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);

        begin:
          /* If we are on toplevel with a lookup closure, we need to sync
             with the current module. */
          if (scm_is_pair (env) && !scm_is_pair (SCM_CAR (env)))
            {
              UPDATE_TOPLEVEL_ENV (env);
              while (!scm_is_null (SCM_CDR (x)))
                {
                  EVALCAR (x, env);
                  UPDATE_TOPLEVEL_ENV (env);
                  x = SCM_CDR (x);
                }
              goto carloop;
            }
          else
            goto nontoplevel_begin;

        nontoplevel_begin:
          while (!scm_is_null (SCM_CDR (x)))
            {
              const SCM form = SCM_CAR (x);
              if (SCM_IMP (form))
                {
                  if (SCM_ISYMP (form))
                    {
		      scm_dynwind_begin (0);
		      scm_i_dynwind_pthread_mutex_lock (&source_mutex);
                      /* check for race condition */
                      if (SCM_ISYMP (SCM_CAR (x)))
                        m_expand_body (x, env);
		      scm_dynwind_end ();
                      goto nontoplevel_begin;
                    }
                  else
                    SCM_VALIDATE_NON_EMPTY_COMBINATION (form);
                }
              else
                (void) EVAL (form, env);
              x = SCM_CDR (x);
            }

        carloop:
          {
            /* scm_eval last form in list */
            const SCM last_form = SCM_CAR (x);

            if (scm_is_pair (last_form))
              {
                /* This is by far the most frequent case. */
                x = last_form;
                goto loop;		/* tail recurse */
              }
            else if (SCM_IMP (last_form))
              RETURN (SCM_I_EVALIM (last_form, env));
            else if (SCM_VARIABLEP (last_form))
              RETURN (SCM_VARIABLE_REF (last_form));
            else if (scm_is_symbol (last_form))
	      RETURN (*scm_lookupcar (x, env, 1));
            else
              RETURN (last_form);
          }


        case (ISYMNUM (SCM_IM_CASE)):
          x = SCM_CDR (x);
          {
            const SCM key = EVALCAR (x, env);
            x = SCM_CDR (x);
            while (!scm_is_null (x))
              {
                const SCM clause = SCM_CAR (x);
                SCM labels = SCM_CAR (clause);
                if (scm_is_eq (labels, SCM_IM_ELSE))
                  {
                    x = SCM_CDR (clause);
                    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
                    goto begin;
                  }
                while (!scm_is_null (labels))
                  {
                    const SCM label = SCM_CAR (labels);
                    if (scm_is_eq (label, key)
                        || scm_is_true (scm_eqv_p (label, key)))
                      {
                        x = SCM_CDR (clause);
                        PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
                        goto begin;
                      }
                    labels = SCM_CDR (labels);
                  }
                x = SCM_CDR (x);
              }
          }
          RETURN (SCM_UNSPECIFIED);


        case (ISYMNUM (SCM_IM_COND)):
          x = SCM_CDR (x);
          while (!scm_is_null (x))
            {
              const SCM clause = SCM_CAR (x);
              if (scm_is_eq (SCM_CAR (clause), SCM_IM_ELSE))
                {
                  x = SCM_CDR (clause);
                  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
                  goto begin;
                }
              else
                {
                  arg1 = EVALCAR (clause, env);
		  /* SRFI 61 extended cond */
		  if (!scm_is_null (SCM_CDR (clause))
		      && !scm_is_null (SCM_CDDR (clause))
		      && scm_is_eq (SCM_CADDR (clause), SCM_IM_ARROW))
		    {
		      SCM xx, guard_result;
		      if (SCM_VALUESP (arg1))
			arg1 = scm_struct_ref (arg1, SCM_INUM0);
		      else
			arg1 = scm_list_1 (arg1);
		      xx = SCM_CDR (clause);
		      proc = EVALCAR (xx, env);
		      guard_result = SCM_APPLY (proc, arg1, SCM_EOL);
		      if (scm_is_true (guard_result)
			  && !SCM_NILP (guard_result))
			{
			  proc = SCM_CDDR (xx);
			  proc = EVALCAR (proc, env);
			  PREP_APPLY (proc, arg1);
			  goto apply_proc;
			}
		    }
                  else if (scm_is_true (arg1) && !SCM_NILP (arg1))
                    {
                      x = SCM_CDR (clause);
                      if (scm_is_null (x))
                        RETURN (arg1);
                      else if (!scm_is_eq (SCM_CAR (x), SCM_IM_ARROW))
                        {
                          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
                          goto begin;
                        }
                      else
                        {
                          proc = SCM_CDR (x);
                          proc = EVALCAR (proc, env);
                          PREP_APPLY (proc, scm_list_1 (arg1));
                          ENTER_APPLY;
                          goto evap1;
                        }
                    }
                  x = SCM_CDR (x);
                }
            }
          RETURN (SCM_UNSPECIFIED);


        case (ISYMNUM (SCM_IM_DO)):
          x = SCM_CDR (x);
          {
            /* Compute the initialization values and the initial environment.  */
            SCM init_forms = SCM_CAR (x);
            SCM init_values = SCM_EOL;
            while (!scm_is_null (init_forms))
              {
                init_values = scm_cons (EVALCAR (init_forms, env), init_values);
                init_forms = SCM_CDR (init_forms);
              }
            x = SCM_CDR (x);
            env = SCM_EXTEND_ENV (SCM_CAR (x), init_values, env);
          }
          x = SCM_CDR (x);
          {
            SCM test_form = SCM_CAR (x);
            SCM body_forms = SCM_CADR (x);
            SCM step_forms = SCM_CDDR (x);

            SCM test_result = EVALCAR (test_form, env);

            while (scm_is_false (test_result) || SCM_NILP (test_result))
              {
                {
                  /* Evaluate body forms.  */
                  SCM temp_forms;
                  for (temp_forms = body_forms;
                       !scm_is_null (temp_forms);
                       temp_forms = SCM_CDR (temp_forms))
                    {
                      SCM form = SCM_CAR (temp_forms);
                      /* Dirk:FIXME: We only need to eval forms that may have
                       * a side effect here.  This is only true for forms that
                       * start with a pair.  All others are just constants.
                       * Since with the current memoizer 'form' may hold a
                       * constant, we call EVAL here to handle the constant
                       * cases.  In the long run it would make sense to have
                       * the macro transformer of 'do' eliminate all forms
                       * that have no sideeffect.  Then instead of EVAL we
                       * could call CEVAL directly here.  */
                      (void) EVAL (form, env);
                    }
                }

                {
                  /* Evaluate the step expressions.  */
                  SCM temp_forms;
                  SCM step_values = SCM_EOL;
                  for (temp_forms = step_forms;
                       !scm_is_null (temp_forms);
                       temp_forms = SCM_CDR (temp_forms))
                    {
                      const SCM value = EVALCAR (temp_forms, env);
                      step_values = scm_cons (value, step_values);
                    }
                  env = SCM_EXTEND_ENV (SCM_CAAR (env),
                                        step_values,
                                        SCM_CDR (env));
                }

                test_result = EVALCAR (test_form, env);
              }
          }
          x = SCM_CDAR (x);
          if (scm_is_null (x))
            RETURN (SCM_UNSPECIFIED);
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto nontoplevel_begin;


        case (ISYMNUM (SCM_IM_IF)):
          x = SCM_CDR (x);
          {
            SCM test_result = EVALCAR (x, env);
            x = SCM_CDR (x);  /* then expression */
            if (scm_is_false (test_result) || SCM_NILP (test_result))
              {
                x = SCM_CDR (x);  /* else expression */
                if (scm_is_null (x))
                  RETURN (SCM_UNSPECIFIED);
              }
          }
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto carloop;


        case (ISYMNUM (SCM_IM_LET)):
          x = SCM_CDR (x);
          {
            SCM init_forms = SCM_CADR (x);
            SCM init_values = SCM_EOL;
            do
              {
                init_values = scm_cons (EVALCAR (init_forms, env), init_values);
                init_forms = SCM_CDR (init_forms);
              }
            while (!scm_is_null (init_forms));
            env = SCM_EXTEND_ENV (SCM_CAR (x), init_values, env);
          }
          x = SCM_CDDR (x);
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto nontoplevel_begin;


        case (ISYMNUM (SCM_IM_LETREC)):
          x = SCM_CDR (x);
          env = SCM_EXTEND_ENV (SCM_CAR (x), undefineds, env);
          x = SCM_CDR (x);
          {
            SCM init_forms = SCM_CAR (x);
	    SCM init_values = scm_list_1 (SCM_BOOL_T);
	    SCM *init_values_eol = SCM_CDRLOC (init_values);
	    ceval_letrec_inits (env, init_forms, &init_values_eol);
            SCM_SETCDR (SCM_CAR (env), SCM_CDR (init_values));
          }
          x = SCM_CDR (x);
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto nontoplevel_begin;


        case (ISYMNUM (SCM_IM_LETSTAR)):
          x = SCM_CDR (x);
          {
            SCM bindings = SCM_CAR (x);
            if (!scm_is_null (bindings))
              {
                do
                  {
                    SCM name = SCM_CAR (bindings);
                    SCM init = SCM_CDR (bindings);
                    env = SCM_EXTEND_ENV (name, EVALCAR (init, env), env);
                    bindings = SCM_CDR (init);
                  }
                while (!scm_is_null (bindings));
              }
          }
          x = SCM_CDR (x);
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto nontoplevel_begin;


        case (ISYMNUM (SCM_IM_OR)):
          x = SCM_CDR (x);
          while (!scm_is_null (SCM_CDR (x)))
            {
              SCM val = EVALCAR (x, env);
              if (scm_is_true (val) && !SCM_NILP (val))
                RETURN (val);
              else
                x = SCM_CDR (x);
            }
          PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
          goto carloop;


        case (ISYMNUM (SCM_IM_LAMBDA)):
          RETURN (scm_closure (SCM_CDR (x), env));


        case (ISYMNUM (SCM_IM_QUOTE)):
          RETURN (SCM_CDR (x));


        case (ISYMNUM (SCM_IM_SET_X)):
          x = SCM_CDR (x);
          {
            SCM *location;
            SCM variable = SCM_CAR (x);
            if (SCM_ILOCP (variable))
              location = scm_ilookup (variable, env);
            else if (SCM_VARIABLEP (variable))
              location = SCM_VARIABLE_LOC (variable);
            else
              {
                /* (scm_is_symbol (variable)) is known to be true */
                variable = lazy_memoize_variable (variable, env);
                SCM_SETCAR (x, variable);
                location = SCM_VARIABLE_LOC (variable);
              }
            x = SCM_CDR (x);
            *location = EVALCAR (x, env);
          }
          RETURN (SCM_UNSPECIFIED);


	case (ISYMNUM (SCM_IM_APPLY)):
          /* Evaluate the procedure to be applied.  */
	  x = SCM_CDR (x);
	  proc = EVALCAR (x, env);
          PREP_APPLY (proc, SCM_EOL);

          /* Evaluate the argument holding the list of arguments */
          x = SCM_CDR (x);
          arg1 = EVALCAR (x, env);

        apply_proc:
          /* Go here to tail-apply a procedure.  PROC is the procedure and
           * ARG1 is the list of arguments. PREP_APPLY must have been called
           * before jumping to apply_proc.  */
	  if (SCM_CLOSUREP (proc))
	    {
              SCM formals = SCM_CLOSURE_FORMALS (proc);
#ifdef DEVAL
              debug.info->a.args = arg1;
#endif
              if (scm_badargsp (formals, arg1))
                scm_wrong_num_args (proc);
              ENTER_APPLY;
              /* Copy argument list */
              if (SCM_NULL_OR_NIL_P (arg1))
                env = SCM_EXTEND_ENV (formals, SCM_EOL, SCM_ENV (proc));
              else
                {
                  SCM args = scm_list_1 (SCM_CAR (arg1));
                  SCM tail = args;
                  arg1 = SCM_CDR (arg1);
                  while (!SCM_NULL_OR_NIL_P (arg1))
                    {
                      SCM new_tail = scm_list_1 (SCM_CAR (arg1));
                      SCM_SETCDR (tail, new_tail);
                      tail = new_tail;
                      arg1 = SCM_CDR (arg1);
                    }
                  env = SCM_EXTEND_ENV (formals, args, SCM_ENV (proc));
                }

              x = SCM_CLOSURE_BODY (proc);
              goto nontoplevel_begin;
	    }
	  else
	    {
              ENTER_APPLY;
              RETURN (SCM_APPLY (proc, arg1, SCM_EOL));
	    }


	case (ISYMNUM (SCM_IM_CONT)):
	  {
	    int first;
	    SCM val = scm_make_continuation (&first);

	    if (!first)
	      RETURN (val);
	    else
	      {
		arg1 = val;
		proc = SCM_CDR (x);
		proc = EVALCAR (proc, env);
		PREP_APPLY (proc, scm_list_1 (arg1));
		ENTER_APPLY;
		goto evap1;
	      }
	  }


	case (ISYMNUM (SCM_IM_DELAY)):
	  RETURN (scm_makprom (scm_closure (SCM_CDR (x), env)));

#if 0
	  /* See futures.h for a comment why futures are not enabled.
	   */
	case (ISYMNUM (SCM_IM_FUTURE)):
	  RETURN (scm_i_make_future (scm_closure (SCM_CDR (x), env)));
#endif

	  /* PLACEHOLDER for case (ISYMNUM (SCM_IM_DISPATCH)): The following
	     code (type_dispatch) is intended to be the tail of the case
	     clause for the internal macro SCM_IM_DISPATCH.  Please don't
	     remove it from this location without discussing it with Mikael
	     <djurfeldt@nada.kth.se>  */
	  
	  /* The type dispatch code is duplicated below
	   * (c.f. objects.c:scm_mcache_compute_cmethod) since that
	   * cuts down execution time for type dispatch to 50%.  */
	type_dispatch: /* inputs: x, arg1 */
	  /* Type dispatch means to determine from the types of the function
	   * arguments (i. e. the 'signature' of the call), which method from
	   * a generic function is to be called.  This process of selecting
	   * the right method takes some time.  To speed it up, guile uses
	   * caching:  Together with the macro call to dispatch the signatures
	   * of some previous calls to that generic function from the same
	   * place are stored (in the code!) in a cache that we call the
	   * 'method cache'.  This is done since it is likely, that
	   * consecutive calls to dispatch from that position in the code will
	   * have the same signature.  Thus, the type dispatch works as
	   * follows: First, determine a hash value from the signature of the
	   * actual arguments.  Second, use this hash value as an index to
	   * find that same signature in the method cache stored at this
	   * position in the code.  If found, you have also found the 
	   * corresponding method that belongs to that signature.  If the
	   * signature is not found in the method cache, you have to perform a
	   * full search over all signatures stored with the generic
	   * function.  */
	{
	    unsigned long int specializers;
	    unsigned long int hash_value;
	    unsigned long int cache_end_pos;
	    unsigned long int mask;
	    SCM method_cache;

	    {
	      SCM z = SCM_CDDR (x);
	      SCM tmp = SCM_CADR (z);
	      specializers = scm_to_ulong (SCM_CAR (z));

	      /* Compute a hash value for searching the method cache.  There
	       * are two variants for computing the hash value, a (rather)
	       * complicated one, and a simple one.  For the complicated one
	       * explained below, tmp holds a number that is used in the
	       * computation.  */
	      if (scm_is_simple_vector (tmp))
		{
		  /* This method of determining the hash value is much
		   * simpler:  Set the hash value to zero and just perform a
		   * linear search through the method cache.  */
		  method_cache = tmp;
		  mask = (unsigned long int) ((long) -1);
		  hash_value = 0;
		  cache_end_pos = SCM_SIMPLE_VECTOR_LENGTH (method_cache);
		}
	      else
		{
		  /* Use the signature of the actual arguments to determine
		   * the hash value.  This is done as follows:  Each class has
		   * an array of random numbers, that are determined when the
		   * class is created.  The integer 'hashset' is an index into
		   * that array of random numbers.  Now, from all classes that
		   * are part of the signature of the actual arguments, the
		   * random numbers at index 'hashset' are taken and summed
		   * up, giving the hash value.  The value of 'hashset' is
		   * stored at the call to dispatch.  This allows to have
		   * different 'formulas' for calculating the hash value at
		   * different places where dispatch is called.  This allows
		   * to optimize the hash formula at every individual place
		   * where dispatch is called, such that hopefully the hash
		   * value that is computed will directly point to the right
		   * method in the method cache.  */
		  unsigned long int hashset = scm_to_ulong (tmp);
		  unsigned long int counter = specializers + 1;
		  SCM tmp_arg = arg1;
		  hash_value = 0;
		  while (!scm_is_null (tmp_arg) && counter != 0)
		    {
		      SCM class = scm_class_of (SCM_CAR (tmp_arg));
		      hash_value += SCM_INSTANCE_HASH (class, hashset);
		      tmp_arg = SCM_CDR (tmp_arg);
		      counter--;
		    }
		  z = SCM_CDDR (z);
		  method_cache = SCM_CADR (z);
		  mask = scm_to_ulong (SCM_CAR (z));
		  hash_value &= mask;
		  cache_end_pos = hash_value;
		}
	    }

	    {
	      /* Search the method cache for a method with a matching
	       * signature.  Start the search at position 'hash_value'.  The
	       * hashing implementation uses linear probing for conflict
	       * resolution, that is, if the signature in question is not
	       * found at the starting index in the hash table, the next table
	       * entry is tried, and so on, until in the worst case the whole
	       * cache has been searched, but still the signature has not been
	       * found.  */
	      SCM z;
	      do
		{
		  SCM args = arg1; /* list of arguments */
		  z = SCM_SIMPLE_VECTOR_REF (method_cache, hash_value);
		  while (!scm_is_null (args))
		    {
		      /* More arguments than specifiers => CLASS != ENV */
		      SCM class_of_arg = scm_class_of (SCM_CAR (args));
		      if (!scm_is_eq (class_of_arg, SCM_CAR (z)))
			goto next_method;
		      args = SCM_CDR (args);
		      z = SCM_CDR (z);
		    }
		  /* Fewer arguments than specifiers => CAR != ENV */
		  if (scm_is_null (SCM_CAR (z)) || scm_is_pair (SCM_CAR (z)))
		    goto apply_cmethod;
		next_method:
		  hash_value = (hash_value + 1) & mask;
		} while (hash_value != cache_end_pos);

	      /* No appropriate method was found in the cache.  */
	      z = scm_memoize_method (x, arg1);

	    apply_cmethod: /* inputs: z, arg1 */
	      {
		SCM formals = SCM_CMETHOD_FORMALS (z);
		env = SCM_EXTEND_ENV (formals, arg1, SCM_CMETHOD_ENV (z));
		x = SCM_CMETHOD_BODY (z);
		goto nontoplevel_begin;
	      }
	    }
	  }


	case (ISYMNUM (SCM_IM_SLOT_REF)):
	  x = SCM_CDR (x);
	  {
	    SCM instance = EVALCAR (x, env);
	    unsigned long int slot = SCM_I_INUM (SCM_CDR (x));
	    RETURN (SCM_PACK (SCM_STRUCT_DATA (instance) [slot]));
	  }


	case (ISYMNUM (SCM_IM_SLOT_SET_X)):
	  x = SCM_CDR (x);
	  {
	    SCM instance = EVALCAR (x, env);
	    unsigned long int slot = SCM_I_INUM (SCM_CADR (x));
	    SCM value = EVALCAR (SCM_CDDR (x), env);
	    SCM_STRUCT_DATA (instance) [slot] = SCM_UNPACK (value);
	    RETURN (SCM_UNSPECIFIED);
	  }


#if SCM_ENABLE_ELISP
	  
	case (ISYMNUM (SCM_IM_NIL_COND)):
	  {
	    SCM test_form = SCM_CDR (x);
	    x = SCM_CDR (test_form);
	    while (!SCM_NULL_OR_NIL_P (x))
	      {
		SCM test_result = EVALCAR (test_form, env);
		if (!(scm_is_false (test_result)
		      || SCM_NULL_OR_NIL_P (test_result)))
		  {
		    if (scm_is_eq (SCM_CAR (x), SCM_UNSPECIFIED))
		      RETURN (test_result);
		    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		    goto carloop;
		  }
		else
		  {
		    test_form = SCM_CDR (x);
		    x = SCM_CDR (test_form);
		  }
	      }
	    x = test_form;
	    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	    goto carloop;
	  }

#endif /* SCM_ENABLE_ELISP */

	case (ISYMNUM (SCM_IM_BIND)):
	  {
	    SCM vars, exps, vals;

	    x = SCM_CDR (x);
	    vars = SCM_CAAR (x);
	    exps = SCM_CDAR (x);
	    vals = SCM_EOL;
	    while (!scm_is_null (exps))
	      {
		vals = scm_cons (EVALCAR (exps, env), vals);
		exps = SCM_CDR (exps);
	      }
	    
	    scm_swap_bindings (vars, vals);
	    scm_i_set_dynwinds (scm_acons (vars, vals, scm_i_dynwinds ()));

	    /* Ignore all but the last evaluation result.  */
	    for (x = SCM_CDR (x); !scm_is_null (SCM_CDR (x)); x = SCM_CDR (x))
	      {
		if (scm_is_pair (SCM_CAR (x)))
		  CEVAL (SCM_CAR (x), env);
	      }
	    proc = EVALCAR (x, env);
	  
	    scm_i_set_dynwinds (SCM_CDR (scm_i_dynwinds ()));
	    scm_swap_bindings (vars, vals);

	    RETURN (proc);
	  }


	case (ISYMNUM (SCM_IM_CALL_WITH_VALUES)):
	  {
            SCM producer;

	    x = SCM_CDR (x);
	    producer = EVALCAR (x, env);
	    x = SCM_CDR (x);
	    proc = EVALCAR (x, env);  /* proc is the consumer. */
	    arg1 = SCM_APPLY (producer, SCM_EOL, SCM_EOL);
	    if (SCM_VALUESP (arg1))
              {
                /* The list of arguments is not copied.  Rather, it is assumed
                 * that this has been done by the 'values' procedure.  */
                arg1 = scm_struct_ref (arg1, SCM_INUM0);
              }
	    else
              {
                arg1 = scm_list_1 (arg1);
              }
            PREP_APPLY (proc, arg1);
            goto apply_proc;
	  }


	default:
	  break;
	}
    }
  else
    {
      if (SCM_VARIABLEP (SCM_CAR (x)))
        proc = SCM_VARIABLE_REF (SCM_CAR (x));
      else if (SCM_ILOCP (SCM_CAR (x)))
        proc = *scm_ilookup (SCM_CAR (x), env);
      else if (scm_is_pair (SCM_CAR (x)))
	proc = CEVAL (SCM_CAR (x), env);
      else if (scm_is_symbol (SCM_CAR (x)))
	{
	  SCM orig_sym = SCM_CAR (x);
	  {
	    SCM *location = scm_lookupcar1 (x, env, 1);
	    if (location == NULL)
	      {
		/* we have lost the race, start again. */
		goto dispatch;
	      }
	    proc = *location;
#ifdef DEVAL
	    if (scm_check_memoize_p && SCM_TRAPS_P)
	      {
		SCM_CLEAR_TRACED_FRAME (debug);
		SCM arg1 = scm_make_debugobj (&debug);
		SCM retval = SCM_BOOL_T;
		SCM_TRAPS_P = 0;
		retval = scm_call_4 (SCM_MEMOIZE_HDLR,
				     scm_sym_memoize_symbol,
				     arg1, x, env);

		/*
		  do something with retval? 
		 */
		SCM_TRAPS_P = 1;
	      }
#endif
	  }

	  if (SCM_MACROP (proc))
	    {
	      SCM_SETCAR (x, orig_sym);  /* Undo memoizing effect of
					    lookupcar */
	    handle_a_macro: /* inputs: x, env, proc */
#ifdef DEVAL
	      /* Set a flag during macro expansion so that macro
		 application frames can be deleted from the backtrace. */
	      SCM_SET_MACROEXP (debug);
#endif
	      arg1 = SCM_APPLY (SCM_MACRO_CODE (proc), x,
                                scm_cons (env, scm_listofnull));
#ifdef DEVAL
	      SCM_CLEAR_MACROEXP (debug);
#endif
	      switch (SCM_MACRO_TYPE (proc))
		{
		case 3:
		case 2:
		  if (!scm_is_pair (arg1))
		    arg1 = scm_list_2 (SCM_IM_BEGIN, arg1);

                  assert (!scm_is_eq (x, SCM_CAR (arg1))
                          && !scm_is_eq (x, SCM_CDR (arg1)));

#ifdef DEVAL
		  if (!SCM_CLOSUREP (SCM_MACRO_CODE (proc)))
		    {
		      SCM_CRITICAL_SECTION_START;
		      SCM_SETCAR (x, SCM_CAR (arg1));
		      SCM_SETCDR (x, SCM_CDR (arg1));
		      SCM_CRITICAL_SECTION_END;
		      goto dispatch;
		    }
		  /* Prevent memoizing of debug info expression. */
		  debug.info->e.exp = scm_cons_source (debug.info->e.exp,
						       SCM_CAR (x),
						       SCM_CDR (x));
#endif
		  SCM_CRITICAL_SECTION_START;
		  SCM_SETCAR (x, SCM_CAR (arg1));
		  SCM_SETCDR (x, SCM_CDR (arg1));
		  SCM_CRITICAL_SECTION_END;
		  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		  goto loop;
#if SCM_ENABLE_DEPRECATED == 1
		case 1:
		  x = arg1;
		  if (SCM_NIMP (x))
		    {
		      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		      goto loop;
		    }
		  else
		    RETURN (arg1);
#endif
		case 0:
		  RETURN (arg1);
		}
	    }
	}
      else
	proc = SCM_CAR (x);

      if (SCM_MACROP (proc))
	goto handle_a_macro;
    }


  /* When reaching this part of the code, the following is granted: Variable x
   * holds the first pair of an expression of the form (<function> arg ...).
   * Variable proc holds the object that resulted from the evaluation of
   * <function>.  In the following, the arguments (if any) will be evaluated,
   * and proc will be applied to them.  If proc does not really hold a
   * function object, this will be signalled as an error on the scheme
   * level.  If the number of arguments does not match the number of arguments
   * that are allowed to be passed to proc, also an error on the scheme level
   * will be signalled.  */

  PREP_APPLY (proc, SCM_EOL);
  if (scm_is_null (SCM_CDR (x))) {
    ENTER_APPLY;
  evap0:
    SCM_ASRTGO (!SCM_IMP (proc), badfun);
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
      case scm_tc7_smob:
	if (!SCM_SMOB_APPLICABLE_P (proc))
	  goto badfun;
	RETURN (SCM_SMOB_APPLY_0 (proc));
      case scm_tc7_cclo:
	arg1 = proc;
	proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	debug.info->a.proc = proc;
	debug.info->a.args = scm_list_1 (arg1);
#endif
	goto evap1;
      case scm_tc7_pws:
	proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	debug.info->a.proc = proc;
#endif
	if (!SCM_CLOSUREP (proc))
	  goto evap0;
        /* fallthrough */
      case scm_tcs_closures:
        {
          const SCM formals = SCM_CLOSURE_FORMALS (proc);
          if (scm_is_pair (formals))
            goto wrongnumargs;
          x = SCM_CLOSURE_BODY (proc);
          env = SCM_EXTEND_ENV (formals, SCM_EOL, SCM_ENV (proc));
          goto nontoplevel_begin;
        }
      case scm_tcs_struct:
	if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	  {
	    x = SCM_ENTITY_PROCEDURE (proc);
	    arg1 = SCM_EOL;
	    goto type_dispatch;
	  }
	else if (SCM_I_OPERATORP (proc))
	  {
	    arg1 = proc;
	    proc = (SCM_I_ENTITYP (proc)
		    ? SCM_ENTITY_PROCEDURE (proc)
		    : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	    debug.info->a.proc = proc;
	    debug.info->a.args = scm_list_1 (arg1);
#endif
            goto evap1;
	  }
        else
          goto badfun;
      case scm_tc7_subr_1:
      case scm_tc7_subr_2:
      case scm_tc7_subr_2o:
      case scm_tc7_dsubr:
      case scm_tc7_cxr:
      case scm_tc7_subr_3:
      case scm_tc7_lsubr_2:
      wrongnumargs:
	scm_wrong_num_args (proc);
      default:
      badfun:
        scm_misc_error (NULL, "Wrong type to apply: ~S", scm_list_1 (proc));
      }
  }

  /* must handle macros by here */
  x = SCM_CDR (x);
  if (scm_is_pair (x))
    arg1 = EVALCAR (x, env);
  else
    scm_wrong_num_args (proc);
#ifdef DEVAL
  debug.info->a.args = scm_list_1 (arg1);
#endif
  x = SCM_CDR (x);
  {
    SCM arg2;
    if (scm_is_null (x))
      {
	ENTER_APPLY;
      evap1: /* inputs: proc, arg1 */
        SCM_ASRTGO (!SCM_IMP (proc), badfun);
	switch (SCM_TYP7 (proc))
	  {				/* have one argument in arg1 */
	  case scm_tc7_subr_2o:
	    RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
	  case scm_tc7_subr_1:
	  case scm_tc7_subr_1o:
	    RETURN (SCM_SUBRF (proc) (arg1));
	  case scm_tc7_dsubr:
            if (SCM_I_INUMP (arg1))
              {
                RETURN (scm_from_double (SCM_DSUBRF (proc) ((double) SCM_I_INUM (arg1))));
              }
            else if (SCM_REALP (arg1))
              {
                RETURN (scm_from_double (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
              }
            else if (SCM_BIGP (arg1))
              {
                RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_big2dbl (arg1))));
              }
	    else if (SCM_FRACTIONP (arg1))
	      {
                RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_fraction2double (arg1))));
	      }
	    SCM_WTA_DISPATCH_1 (*SCM_SUBR_GENERIC (proc), arg1,
                                SCM_ARG1,
				scm_i_symbol_chars (SCM_SNAME (proc)));
	  case scm_tc7_cxr:
	    RETURN (scm_i_chase_pairs (arg1, (scm_t_bits) SCM_SUBRF (proc)));
	  case scm_tc7_rpsubr:
	    RETURN (SCM_BOOL_T);
	  case scm_tc7_asubr:
	    RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
	  case scm_tc7_lsubr:
#ifdef DEVAL
	    RETURN (SCM_SUBRF (proc) (debug.info->a.args));
#else
	    RETURN (SCM_SUBRF (proc) (scm_list_1 (arg1)));
#endif
	  case scm_tc7_smob:
	    if (!SCM_SMOB_APPLICABLE_P (proc))
	      goto badfun;
	    RETURN (SCM_SMOB_APPLY_1 (proc, arg1));
	  case scm_tc7_cclo:
	    arg2 = arg1;
	    arg1 = proc;
	    proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	    debug.info->a.args = scm_cons (arg1, debug.info->a.args);
	    debug.info->a.proc = proc;
#endif
	    goto evap2;
	  case scm_tc7_pws:
	    proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	    debug.info->a.proc = proc;
#endif
	    if (!SCM_CLOSUREP (proc))
	      goto evap1;
            /* fallthrough */
	  case scm_tcs_closures:
            {
              /* clos1: */
              const SCM formals = SCM_CLOSURE_FORMALS (proc);
              if (scm_is_null (formals)
                  || (scm_is_pair (formals) && scm_is_pair (SCM_CDR (formals))))
                goto wrongnumargs;
              x = SCM_CLOSURE_BODY (proc);
#ifdef DEVAL
              env = SCM_EXTEND_ENV (formals,
                                    debug.info->a.args,
                                    SCM_ENV (proc));
#else
              env = SCM_EXTEND_ENV (formals,
                                    scm_list_1 (arg1),
                                    SCM_ENV (proc));
#endif
              goto nontoplevel_begin;
            }
	  case scm_tcs_struct:
	    if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	      {
		x = SCM_ENTITY_PROCEDURE (proc);
#ifdef DEVAL
		arg1 = debug.info->a.args;
#else
		arg1 = scm_list_1 (arg1);
#endif
		goto type_dispatch;
	      }
	    else if (SCM_I_OPERATORP (proc))
	      {
		arg2 = arg1;
		arg1 = proc;
		proc = (SCM_I_ENTITYP (proc)
			? SCM_ENTITY_PROCEDURE (proc)
			: SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
		debug.info->a.args = scm_cons (arg1, debug.info->a.args);
		debug.info->a.proc = proc;
#endif
                goto evap2;
	      }
            else
              goto badfun;
	  case scm_tc7_subr_2:
	  case scm_tc7_subr_0:
	  case scm_tc7_subr_3:
	  case scm_tc7_lsubr_2:
	    scm_wrong_num_args (proc);
	  default:
	    goto badfun;
	  }
      }
    if (scm_is_pair (x))
      arg2 = EVALCAR (x, env);
    else
      scm_wrong_num_args (proc);

    {				/* have two or more arguments */
#ifdef DEVAL
      debug.info->a.args = scm_list_2 (arg1, arg2);
#endif
      x = SCM_CDR (x);
      if (scm_is_null (x)) {
	ENTER_APPLY;
      evap2:
        SCM_ASRTGO (!SCM_IMP (proc), badfun);
	switch (SCM_TYP7 (proc))
	  {			/* have two arguments */
	  case scm_tc7_subr_2:
	  case scm_tc7_subr_2o:
	    RETURN (SCM_SUBRF (proc) (arg1, arg2));
	  case scm_tc7_lsubr:
#ifdef DEVAL
	    RETURN (SCM_SUBRF (proc) (debug.info->a.args));
#else
	    RETURN (SCM_SUBRF (proc) (scm_list_2 (arg1, arg2)));
#endif
	  case scm_tc7_lsubr_2:
	    RETURN (SCM_SUBRF (proc) (arg1, arg2, SCM_EOL));
	  case scm_tc7_rpsubr:
	  case scm_tc7_asubr:
	    RETURN (SCM_SUBRF (proc) (arg1, arg2));
	  case scm_tc7_smob:
	    if (!SCM_SMOB_APPLICABLE_P (proc))
	      goto badfun;
	    RETURN (SCM_SMOB_APPLY_2 (proc, arg1, arg2));
	  cclon:
	  case scm_tc7_cclo:
#ifdef DEVAL
	    RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc),
			       scm_cons (proc, debug.info->a.args),
			       SCM_EOL));
#else
	    RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc),
			       scm_cons2 (proc, arg1,
					  scm_cons (arg2,
						    scm_ceval_args (x,
								   env,
								   proc))),
			       SCM_EOL));
#endif
	  case scm_tcs_struct:
	    if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	      {
		x = SCM_ENTITY_PROCEDURE (proc);
#ifdef DEVAL
		arg1 = debug.info->a.args;
#else
		arg1 = scm_list_2 (arg1, arg2);
#endif
		goto type_dispatch;
	      }
	    else if (SCM_I_OPERATORP (proc))
	      {
	      operatorn:
#ifdef DEVAL
		RETURN (SCM_APPLY (SCM_I_ENTITYP (proc)
				   ? SCM_ENTITY_PROCEDURE (proc)
				   : SCM_OPERATOR_PROCEDURE (proc),
				   scm_cons (proc, debug.info->a.args),
				   SCM_EOL));
#else
		RETURN (SCM_APPLY (SCM_I_ENTITYP (proc)
				   ? SCM_ENTITY_PROCEDURE (proc)
				   : SCM_OPERATOR_PROCEDURE (proc),
				   scm_cons2 (proc, arg1,
					      scm_cons (arg2,
							scm_ceval_args (x,
								       env,
								       proc))),
				   SCM_EOL));
#endif
	      }
            else
              goto badfun;
	  case scm_tc7_subr_0:
	  case scm_tc7_dsubr:
	  case scm_tc7_cxr:
	  case scm_tc7_subr_1o:
	  case scm_tc7_subr_1:
	  case scm_tc7_subr_3:
	    scm_wrong_num_args (proc);
	  default:
	    goto badfun;
	  case scm_tc7_pws:
	    proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	    debug.info->a.proc = proc;
#endif
	    if (!SCM_CLOSUREP (proc))
	      goto evap2;
            /* fallthrough */
	  case scm_tcs_closures:
            {
              /* clos2: */
              const SCM formals = SCM_CLOSURE_FORMALS (proc);
              if (scm_is_null (formals)
                  || (scm_is_pair (formals)
                      && (scm_is_null (SCM_CDR (formals))
                          || (scm_is_pair (SCM_CDR (formals))
                              && scm_is_pair (SCM_CDDR (formals))))))
                goto wrongnumargs;
#ifdef DEVAL
              env = SCM_EXTEND_ENV (formals,
                                    debug.info->a.args,
                                    SCM_ENV (proc));
#else
              env = SCM_EXTEND_ENV (formals,
                                    scm_list_2 (arg1, arg2),
                                    SCM_ENV (proc));
#endif
              x = SCM_CLOSURE_BODY (proc);
              goto nontoplevel_begin;
            }
	  }
      }
      if (!scm_is_pair (x))
	scm_wrong_num_args (proc);
#ifdef DEVAL
      debug.info->a.args = scm_cons2 (arg1, arg2,
				      deval_args (x, env, proc,
						  SCM_CDRLOC (SCM_CDR (debug.info->a.args))));
#endif
      ENTER_APPLY;
    evap3:
      SCM_ASRTGO (!SCM_IMP (proc), badfun);
      switch (SCM_TYP7 (proc))
	{			/* have 3 or more arguments */
#ifdef DEVAL
	case scm_tc7_subr_3:
	  if (!scm_is_null (SCM_CDR (x)))
	    scm_wrong_num_args (proc);
	  else
	    RETURN (SCM_SUBRF (proc) (arg1, arg2,
				      SCM_CADDR (debug.info->a.args)));
	case scm_tc7_asubr:
	  arg1 = SCM_SUBRF(proc)(arg1, arg2);
	  arg2 = SCM_CDDR (debug.info->a.args);
	  do
	    {
	      arg1 = SCM_SUBRF(proc)(arg1, SCM_CAR (arg2));
	      arg2 = SCM_CDR (arg2);
	    }
	  while (SCM_NIMP (arg2));
	  RETURN (arg1);
	case scm_tc7_rpsubr:
	  if (scm_is_false (SCM_SUBRF (proc) (arg1, arg2)))
	    RETURN (SCM_BOOL_F);
	  arg1 = SCM_CDDR (debug.info->a.args);
	  do
	    {
	      if (scm_is_false (SCM_SUBRF (proc) (arg2, SCM_CAR (arg1))))
		RETURN (SCM_BOOL_F);
	      arg2 = SCM_CAR (arg1);
	      arg1 = SCM_CDR (arg1);
	    }
	  while (SCM_NIMP (arg1));
	  RETURN (SCM_BOOL_T);
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (arg1, arg2,
				    SCM_CDDR (debug.info->a.args)));
	case scm_tc7_lsubr:
	  RETURN (SCM_SUBRF (proc) (debug.info->a.args));
	case scm_tc7_smob:
	  if (!SCM_SMOB_APPLICABLE_P (proc))
	    goto badfun;
	  RETURN (SCM_SMOB_APPLY_3 (proc, arg1, arg2,
				    SCM_CDDR (debug.info->a.args)));
	case scm_tc7_cclo:
	  goto cclon;
	case scm_tc7_pws:
	  proc = SCM_PROCEDURE (proc);
	  debug.info->a.proc = proc;
	  if (!SCM_CLOSUREP (proc))
	    goto evap3;
          /* fallthrough */
	case scm_tcs_closures:
          {
            const SCM formals = SCM_CLOSURE_FORMALS (proc);
            if (scm_is_null (formals)
                || (scm_is_pair (formals)
                    && (scm_is_null (SCM_CDR (formals))
                        || (scm_is_pair (SCM_CDR (formals))
                            && scm_badargsp (SCM_CDDR (formals), x)))))
              goto wrongnumargs;
            SCM_SET_ARGSREADY (debug);
            env = SCM_EXTEND_ENV (formals,
                                  debug.info->a.args,
                                  SCM_ENV (proc));
            x = SCM_CLOSURE_BODY (proc);
            goto nontoplevel_begin;
          }
#else /* DEVAL */
	case scm_tc7_subr_3:
	  if (!scm_is_null (SCM_CDR (x)))
	    scm_wrong_num_args (proc);
	  else
	    RETURN (SCM_SUBRF (proc) (arg1, arg2, EVALCAR (x, env)));
	case scm_tc7_asubr:
	  arg1 = SCM_SUBRF (proc) (arg1, arg2);
	  do
	    {
	      arg1 = SCM_SUBRF(proc)(arg1, EVALCAR(x, env));
	      x = SCM_CDR(x);
	    }
	  while (!scm_is_null (x));
	  RETURN (arg1);
	case scm_tc7_rpsubr:
	  if (scm_is_false (SCM_SUBRF (proc) (arg1, arg2)))
	    RETURN (SCM_BOOL_F);
	  do
	    {
	      arg1 = EVALCAR (x, env);
	      if (scm_is_false (SCM_SUBRF (proc) (arg2, arg1)))
		RETURN (SCM_BOOL_F);
	      arg2 = arg1;
	      x = SCM_CDR (x);
	    }
	  while (!scm_is_null (x));
	  RETURN (SCM_BOOL_T);
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (arg1, arg2, scm_ceval_args (x, env, proc)));
	case scm_tc7_lsubr:
	  RETURN (SCM_SUBRF (proc) (scm_cons2 (arg1,
					       arg2,
					       scm_ceval_args (x, env, proc))));
	case scm_tc7_smob:
	  if (!SCM_SMOB_APPLICABLE_P (proc))
	    goto badfun;
	  RETURN (SCM_SMOB_APPLY_3 (proc, arg1, arg2,
				    scm_ceval_args (x, env, proc)));
	case scm_tc7_cclo:
	  goto cclon;
	case scm_tc7_pws:
	  proc = SCM_PROCEDURE (proc);
	  if (!SCM_CLOSUREP (proc))
	    goto evap3;
          /* fallthrough */
	case scm_tcs_closures:
	  {
	    const SCM formals = SCM_CLOSURE_FORMALS (proc);
	    if (scm_is_null (formals)
		|| (scm_is_pair (formals)
		    && (scm_is_null (SCM_CDR (formals))
			|| (scm_is_pair (SCM_CDR (formals))
			    && scm_badargsp (SCM_CDDR (formals), x)))))
	      goto wrongnumargs;
            env = SCM_EXTEND_ENV (formals,
                                  scm_cons2 (arg1,
                                             arg2,
                                             scm_ceval_args (x, env, proc)),
                                  SCM_ENV (proc));
            x = SCM_CLOSURE_BODY (proc);
            goto nontoplevel_begin;
	  }
#endif /* DEVAL */
	case scm_tcs_struct:
	  if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	    {
#ifdef DEVAL
	      arg1 = debug.info->a.args;
#else
	      arg1 = scm_cons2 (arg1, arg2, scm_ceval_args (x, env, proc));
#endif
	      x = SCM_ENTITY_PROCEDURE (proc);
	      goto type_dispatch;
	    }
	  else if (SCM_I_OPERATORP (proc))
	    goto operatorn;
	  else
	    goto badfun;
	case scm_tc7_subr_2:
	case scm_tc7_subr_1o:
	case scm_tc7_subr_2o:
	case scm_tc7_subr_0:
	case scm_tc7_dsubr:
	case scm_tc7_cxr:
	case scm_tc7_subr_1:
	  scm_wrong_num_args (proc);
	default:
	  goto badfun;
	}
    }
  }
#ifdef DEVAL
exit:
  if (scm_check_exit_p && SCM_TRAPS_P)
    if (SCM_EXIT_FRAME_P || (SCM_TRACE_P && SCM_TRACED_FRAME_P (debug)))
      {
	SCM_CLEAR_TRACED_FRAME (debug);
	arg1 = scm_make_debugobj (&debug);
	SCM_TRAPS_P = 0;
	arg1 = scm_call_3 (SCM_EXIT_FRAME_HDLR, scm_sym_exit_frame, arg1, proc);
	SCM_TRAPS_P = 1;
	if (scm_is_pair (arg1) && scm_is_eq (SCM_CAR (arg1), sym_instead))
	  proc = SCM_CDR (arg1);
      }
  scm_i_set_last_debug_frame (debug.prev);
  return proc;
#endif
}




/* Apply a function to a list of arguments.

   This function is exported to the Scheme level as taking two
   required arguments and a tail argument, as if it were:
	(lambda (proc arg1 . args) ...)
   Thus, if you just have a list of arguments to pass to a procedure,
   pass the list as ARG1, and '() for ARGS.  If you have some fixed
   args, pass the first as ARG1, then cons any remaining fixed args
   onto the front of your argument list, and pass that as ARGS.  */

SCM 
SCM_APPLY (SCM proc, SCM arg1, SCM args)
{
#ifdef DEVAL
  scm_t_debug_frame debug;
  scm_t_debug_info debug_vect_body;
  debug.prev = scm_i_last_debug_frame ();
  debug.status = SCM_APPLYFRAME;
  debug.vect = &debug_vect_body;
  debug.vect[0].a.proc = proc;
  debug.vect[0].a.args = SCM_EOL;
  scm_i_set_last_debug_frame (&debug);
#else
  if (scm_debug_mode_p)
    return scm_dapply (proc, arg1, args);
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
  if (scm_is_null (args))
    {
      if (scm_is_null (arg1))
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
      args = scm_nconc2last (args);
#ifdef DEVAL
      debug.vect[0].a.args = scm_cons (arg1, args);
#endif
    }
#ifdef DEVAL
  if (SCM_ENTER_FRAME_P && SCM_TRAPS_P)
    {
      SCM tmp = scm_make_debugobj (&debug);
      SCM_TRAPS_P = 0;
      scm_call_2 (SCM_ENTER_FRAME_HDLR, scm_sym_enter_frame, tmp);
      SCM_TRAPS_P = 1;
    }
  ENTER_APPLY;
#endif
tail:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2o:
      if (SCM_UNBNDP (arg1))
	scm_wrong_num_args (proc);
      if (scm_is_null (args))
        args = SCM_UNDEFINED;
      else
        {
          if (! scm_is_null (SCM_CDR (args)))
            scm_wrong_num_args (proc);
          args = SCM_CAR (args);
        }
      RETURN (SCM_SUBRF (proc) (arg1, args));
    case scm_tc7_subr_2:
      if (scm_is_null (args) || !scm_is_null (SCM_CDR (args)))
	scm_wrong_num_args (proc);
      args = SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args));
    case scm_tc7_subr_0:
      if (!SCM_UNBNDP (arg1))
	scm_wrong_num_args (proc);
      else
	RETURN (SCM_SUBRF (proc) ());
    case scm_tc7_subr_1:
      if (SCM_UNBNDP (arg1))
	scm_wrong_num_args (proc);
    case scm_tc7_subr_1o:
      if (!scm_is_null (args))
	scm_wrong_num_args (proc);
      else
	RETURN (SCM_SUBRF (proc) (arg1));
    case scm_tc7_dsubr:
      if (SCM_UNBNDP (arg1) || !scm_is_null (args))
	scm_wrong_num_args (proc);
      if (SCM_I_INUMP (arg1))
        {
          RETURN (scm_from_double (SCM_DSUBRF (proc) ((double) SCM_I_INUM (arg1))));
        }
      else if (SCM_REALP (arg1))
        {
          RETURN (scm_from_double (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
        }
      else if (SCM_BIGP (arg1))
	{
	  RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_big2dbl (arg1))));
	}
      else if (SCM_FRACTIONP (arg1))
	{
	  RETURN (scm_from_double (SCM_DSUBRF (proc) (scm_i_fraction2double (arg1))));
	}
      SCM_WTA_DISPATCH_1 (*SCM_SUBR_GENERIC (proc), arg1,
                          SCM_ARG1, scm_i_symbol_chars (SCM_SNAME (proc)));
    case scm_tc7_cxr:
      if (SCM_UNBNDP (arg1) || !scm_is_null (args))
	scm_wrong_num_args (proc);
      RETURN (scm_i_chase_pairs (arg1, (scm_t_bits) SCM_SUBRF (proc)));
    case scm_tc7_subr_3:
      if (scm_is_null (args)
	  || scm_is_null (SCM_CDR (args))
	  || !scm_is_null (SCM_CDDR (args)))
	scm_wrong_num_args (proc);
      else
	RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CADR (args)));
    case scm_tc7_lsubr:
#ifdef DEVAL
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : debug.vect[0].a.args));
#else
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args)));
#endif
    case scm_tc7_lsubr_2:
      if (!scm_is_pair (args))
	scm_wrong_num_args (proc);
      else
	RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CDR (args)));
    case scm_tc7_asubr:
      if (scm_is_null (args))
	RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
      while (SCM_NIMP (args))
	{
	  SCM_ASSERT (scm_is_pair (args), args, SCM_ARG2, "apply");
	  arg1 = SCM_SUBRF (proc) (arg1, SCM_CAR (args));
	  args = SCM_CDR (args);
	}
      RETURN (arg1);
    case scm_tc7_rpsubr:
      if (scm_is_null (args))
	RETURN (SCM_BOOL_T);
      while (SCM_NIMP (args))
	{
	  SCM_ASSERT (scm_is_pair (args), args, SCM_ARG2, "apply");
	  if (scm_is_false (SCM_SUBRF (proc) (arg1, SCM_CAR (args))))
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
      if (scm_badargsp (SCM_CLOSURE_FORMALS (proc), arg1))
	scm_wrong_num_args (proc);
      
      /* Copy argument list */
      if (SCM_IMP (arg1))
	args = arg1;
      else
	{
	  SCM tl = args = scm_cons (SCM_CAR (arg1), SCM_UNSPECIFIED);
	  for (arg1 = SCM_CDR (arg1); scm_is_pair (arg1); arg1 = SCM_CDR (arg1))
	    {
	      SCM_SETCDR (tl, scm_cons (SCM_CAR (arg1), SCM_UNSPECIFIED));
	      tl = SCM_CDR (tl);
	    }
	  SCM_SETCDR (tl, arg1);
	}
      
      args = SCM_EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
                             args,
                             SCM_ENV (proc));
      proc = SCM_CLOSURE_BODY (proc);
    again:
      arg1 = SCM_CDR (proc);
      while (!scm_is_null (arg1))
	{
	  if (SCM_IMP (SCM_CAR (proc)))
	    {
	      if (SCM_ISYMP (SCM_CAR (proc)))
		{
		  scm_dynwind_begin (0);
		  scm_i_dynwind_pthread_mutex_lock (&source_mutex);
		  /* check for race condition */
		  if (SCM_ISYMP (SCM_CAR (proc)))
		    m_expand_body (proc, args);
		  scm_dynwind_end ();
		  goto again;
		}
	      else
		SCM_VALIDATE_NON_EMPTY_COMBINATION (SCM_CAR (proc));
	    }
	  else
	    (void) EVAL (SCM_CAR (proc), args);
	  proc = arg1;
          arg1 = SCM_CDR (proc);
	}
      RETURN (EVALCAR (proc, args));
    case scm_tc7_smob:
      if (!SCM_SMOB_APPLICABLE_P (proc))
	goto badproc;
      if (SCM_UNBNDP (arg1))
	RETURN (SCM_SMOB_APPLY_0 (proc));
      else if (scm_is_null (args))
	RETURN (SCM_SMOB_APPLY_1 (proc, arg1));
      else if (scm_is_null (SCM_CDR (args)))
	RETURN (SCM_SMOB_APPLY_2 (proc, arg1, SCM_CAR (args)));
      else
	RETURN (SCM_SMOB_APPLY_3 (proc, arg1, SCM_CAR (args), SCM_CDR (args)));
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
    case scm_tc7_pws:
      proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
      debug.vect[0].a.proc = proc;
#endif
      goto tail;
    case scm_tcs_struct:
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	{
#ifdef DEVAL
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
	  RETURN (scm_apply_generic (proc, args));
	}
      else if (SCM_I_OPERATORP (proc))
	{
	  /* operator */
#ifdef DEVAL
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
	  arg1 = proc;
	  proc = (SCM_I_ENTITYP (proc)
		  ? SCM_ENTITY_PROCEDURE (proc)
		  : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	  debug.vect[0].a.proc = proc;
	  debug.vect[0].a.args = scm_cons (arg1, args);
#endif
	  if (SCM_NIMP (proc))
	    goto tail;
	  else
	    goto badproc;
	}
      else
        goto badproc;
    default:
    badproc:
      scm_wrong_type_arg ("apply", SCM_ARG1, proc);
    }
#ifdef DEVAL
exit:
  if (scm_check_exit_p && SCM_TRAPS_P)
    if (SCM_EXIT_FRAME_P || (SCM_TRACE_P && SCM_TRACED_FRAME_P (debug)))
      {
	SCM_CLEAR_TRACED_FRAME (debug);
	arg1 = scm_make_debugobj (&debug);
	SCM_TRAPS_P = 0;
	arg1 = scm_call_3 (SCM_EXIT_FRAME_HDLR, scm_sym_exit_frame, arg1, proc);
	SCM_TRAPS_P = 1;
	if (scm_is_pair (arg1) && scm_is_eq (SCM_CAR (arg1), sym_instead))
	  proc = SCM_CDR (arg1);
      }
  scm_i_set_last_debug_frame (debug.prev);
  return proc;
#endif
}

