/* Copyright (C) 2001 Free Software Foundation, Inc.
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

#include <string.h>
#include "envs.h"

#define ENV_OBARRAY_SIZE 31


scm_bits_t scm_tc16_env;

SCM
scm_c_make_env (void)
{
  struct scm_env *p = scm_must_malloc (sizeof (struct scm_env),
				       "scm_c_make_env");
  p->identifier = SCM_BOOL_F;
  p->obarray    = scm_c_make_hash_table (ENV_OBARRAY_SIZE);
  SCM_RETURN_NEWSMOB (scm_tc16_env, p);
}

static SCM
env_mark (SCM obj)
{
  struct scm_env *p = SCM_ENV_DATA (obj);
  scm_gc_mark (p->identifier);
  return p->obarray;
}

static scm_sizet
env_free (SCM obj)
{
  scm_must_free (SCM_ENV_DATA (obj));
  return sizeof (struct scm_env);
}


/*
 * C interface
 */

static SCM env_table;
static SCM load_env;

SCM
scm_c_lookup_env (SCM identifier)
{
  /* Check if the env is already loaded */
  SCM vcell = scm_sym2ovcell_soft (identifier, env_table);

  /* If not, load the env */
  if (SCM_FALSEP (vcell))
    {
      SCM env = scm_apply (SCM_CDR (load_env),
			   SCM_LIST1 (identifier), SCM_EOL);
      if (!SCM_ENV_P (env))
	scm_misc_error ("scm_c_lookup_env",
			"Invalid env: ~S", SCM_LIST1 (env));
      scm_intern_symbol (env_table, identifier);
      vcell = scm_sym2ovcell_soft (identifier, env_table);
      SCM_SETCDR (vcell, env);
    }

  return SCM_CDR (vcell);
}

SCM
scm_c_env_vcell (SCM env, SCM name, int intern)
{
  SCM ob = SCM_ENV_OBARRAY (env);
  if (intern)
    scm_intern_symbol (ob, name);
  return scm_sym2ovcell_soft (name, ob);
}


/*
 * Scheme interface
 */

SCM_DEFINE (scm_make_env, "make-env", 0, 0, 0,
	    (),
	    "")
#define FUNC_NAME s_scm_make_env
{
  return scm_c_make_env ();
}
#undef FUNC_NAME

SCM_DEFINE (scm_env_p, "env?", 1, 0, 0,
	    (SCM x),
	    "")
#define FUNC_NAME s_scm_env_p
{
  return SCM_BOOL (SCM_ENV_P (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_env_identifier, "env-identifier", 1, 0, 0,
	    (SCM env),
	    "")
#define FUNC_NAME s_scm_env_identifier
{
  SCM_VALIDATE_ENV (1, env);
  return SCM_ENV_IDENTIFIER (env);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_env_identifier_x, "set-env-identifier!", 2, 0, 0,
	    (SCM env, SCM identifier),
	    "")
#define FUNC_NAME s_scm_set_env_identifier_x
{
  SCM_VALIDATE_ENV (1, env);
  SCM_VALIDATE_SYMBOL (2, identifier);
  SCM_ENV_IDENTIFIER (env) = identifier;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_env_bound_p, "env-bound?", 2, 0, 0,
	    (SCM env, SCM name),
	    "")
#define FUNC_NAME s_scm_env_bound_p
{
  SCM vcell;
  SCM_VALIDATE_ENV (1, env);
  SCM_VALIDATE_SYMBOL (2, name);
  vcell = scm_sym2ovcell_soft (name, SCM_ENV_OBARRAY (env));
  return SCM_BOOL (!SCM_FALSEP (vcell) && !SCM_UNBNDP (SCM_CDR (vcell)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_env_ref, "env-ref", 2, 0, 0,
	    (SCM env, SCM name),
	    "")
#define FUNC_NAME s_scm_env_ref
{
  SCM vcell;
  SCM_VALIDATE_ENV (1, env);
  SCM_VALIDATE_SYMBOL (2, name);
  vcell = scm_sym2ovcell_soft (name, SCM_ENV_OBARRAY (env));
  if (SCM_FALSEP (vcell) || SCM_UNBNDP (SCM_CDR (vcell)))
    SCM_MISC_ERROR ("Unbound variable in env: ~A, ~A",
		    SCM_LIST2 (env, name));
  return SCM_CDR (vcell);
}
#undef FUNC_NAME

SCM_DEFINE (scm_env_set_x, "env-set!", 3, 0, 0,
	    (SCM env, SCM name, SCM val),
	    "")
#define FUNC_NAME s_scm_env_set_x
{
  SCM vcell;
  SCM_VALIDATE_ENV (1, env);
  SCM_VALIDATE_SYMBOL (2, name);
  vcell = scm_sym2ovcell_soft (name, SCM_ENV_OBARRAY (env));
  if (SCM_FALSEP (vcell))
    SCM_MISC_ERROR ("Unbound variable in env: ~A, ~A",
		    SCM_LIST2 (env, name));
  SCM_SETCDR (vcell, val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_env_define, "env-define", 3, 0, 0,
	    (SCM env, SCM name, SCM val),
	    "")
#define FUNC_NAME s_scm_env_define
{
  SCM vcell;
  SCM_VALIDATE_ENV (1, env);
  SCM_VALIDATE_SYMBOL (2, name);
  vcell = scm_c_env_vcell (env, name, 1);
  SCM_SETCDR (vcell, val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void
scm_init_envs (void)
{
  SCM mod, old;

  scm_tc16_env = scm_make_smob_type ("env", 0);
  scm_set_smob_mark (scm_tc16_env, env_mark);
  scm_set_smob_free (scm_tc16_env, env_free);

  env_table = scm_permanent_object (scm_c_make_hash_table (51));

  mod = scm_resolve_module (scm_read_0str ("(system base module)"));
  old = scm_set_current_module (mod);

#ifndef SCM_MAGIC_SNARFER
#include "envs.x"
#endif

  load_env = scm_eval_closure_lookup (scm_standard_eval_closure (mod),
				      scm_str2symbol ("load-env"),
				      SCM_BOOL_T);
  load_env = SCM_VARVCELL (load_env);

  scm_set_current_module (old);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
