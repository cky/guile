/* Copyright (C) 2000 Free Software Foundation, Inc.
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

/* This file is included in vm_engine.c */

VM_DEFINE_INSTRUCTION (load_integer, "load-integer", -1, 0, 1)
{
  size_t len;

  FETCH_LENGTH (len);
  if (len <= 4)
    {
      long val = 0;
      while (len-- > 0)
	val = (val << 8) + FETCH ();
      PUSH (scm_long2num (val));
      NEXT;
    }
  else
    SCM_MISC_ERROR ("load-integer: not implemented yet", SCM_EOL);
}

VM_DEFINE_INSTRUCTION (load_number, "load-number", -1, 0, 1)
{
  size_t len;
  FETCH_LENGTH (len);
  PUSH (scm_istring2number (ip, len, 10));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_string, "load-string", -1, 0, 1)
{
  size_t len;
  FETCH_LENGTH (len);
  PUSH (scm_makfromstr (ip, len, 0));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_symbol, "load-symbol", -1, 0, 1)
{
  size_t len;
  FETCH_LENGTH (len);
  PUSH (scm_mem2symbol (ip, len));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_keyword, "load-keyword", -1, 0, 1)
{
  SCM sym;
  size_t len;
  FETCH_LENGTH (len);
  sym = scm_mem2symbol (ip, len);
  PUSH (scm_make_keyword_from_dash_symbol (sym));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_module, "load-module", -1, 0, 1)
{
  size_t len;
  FETCH_LENGTH (len);
  PUSH (scm_c_lookup_env (scm_mem2symbol (ip, len)));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_program, "load-program", -1, 0, 1)
{
  size_t len;
  SCM prog, x;

  FETCH_LENGTH (len);
  prog = scm_c_make_program (ip, len, program);
  ip += len;

  /* init object table */
  x = *sp;
  if (SCM_VECTORP (x))
    {
      SCM_PROGRAM_OBJS (prog) = x;
      DROP ();
      x = *sp;
    }

  /* init parameters */
  if (SCM_INUMP (x))
    {
      int i = SCM_INUM (x);
      if (-128 <= i && i <= 127)
	{
	  /* 8-bit representation */
	  SCM_PROGRAM_NARGS (prog) = (i >> 6) & 0x03;	/* 7-6 bits */
	  SCM_PROGRAM_NREST (prog) = (i >> 5) & 0x01;	/* 5 bit */
	  SCM_PROGRAM_NLOCS (prog) = (i >> 2) & 0x07;	/* 4-2 bits */
	  SCM_PROGRAM_NEXTS (prog) = i & 0x03;		/* 1-0 bits */
	}
      else
	{
	  /* 16-bit representation */
	  SCM_PROGRAM_NARGS (prog) = (i >> 12) & 0x07;	/* 15-12 bits */
	  SCM_PROGRAM_NREST (prog) = (i >> 11) & 0x01;	/* 11 bit */
	  SCM_PROGRAM_NLOCS (prog) = (i >> 4)  & 0x7f;	/* 10-4 bits */
	  SCM_PROGRAM_NEXTS (prog) = i & 0x07;		/* 3-0 bits */
	}
    }
  else
    {
      /* Other cases */
      SCM_PROGRAM_NARGS (prog) = SCM_INUM (sp[4]);
      SCM_PROGRAM_NREST (prog) = SCM_INUM (sp[3]);
      SCM_PROGRAM_NLOCS (prog) = SCM_INUM (sp[2]);
      SCM_PROGRAM_NEXTS (prog) = SCM_INUM (sp[1]);
      sp += 4;
    }

  *sp = prog;
  NEXT;
}

VM_DEFINE_INSTRUCTION (link, "link", 0, 2, 1)
{
  sp[1] = scm_c_env_vcell (sp[1], sp[0], 1);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (link_current_module, "link/current-module", 0, 1, 1)
{
  SCM mod = scm_current_module ();
  SCM var = scm_eval_closure_lookup (scm_standard_eval_closure (mod),
				     *sp, SCM_BOOL_F);
  if (SCM_FALSEP (var))
    /* Create a new variable if not defined yet */
    var = scm_eval_closure_lookup (scm_standard_eval_closure (mod),
				   *sp, SCM_BOOL_T);
  *sp = SCM_VARVCELL (var);
  NEXT;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
