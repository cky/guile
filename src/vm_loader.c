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

VM_DEFINE_INSTRUCTION (load_integer, "load-integer", -1)
{
  size_t len;
  FETCH_LENGTH (len);
  SCM_MISC_ERROR ("Not implemented yet", SCM_EOL);
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_symbol, "load-symbol", -1)
{
  size_t len;
  FETCH_LENGTH (len);
  PUSH (scm_mem2symbol (ip, len));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_string, "load-string", -1)
{
  size_t len;
  FETCH_LENGTH (len);
  PUSH (scm_makfromstr (ip, len, 0));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_keyword, "load-keyword", -1)
{
  SCM sym;
  size_t len;
  FETCH_LENGTH (len);
  sym = scm_mem2symbol (ip, len);
  PUSH (scm_make_keyword_from_dash_symbol (sym));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_module, "load-module", -1)
{
  size_t len;
  FETCH_LENGTH (len);
  PUSH (scm_c_lookup_env (scm_mem2symbol (ip, len)));
  ip += len;
  NEXT;
}

VM_DEFINE_INSTRUCTION (load_program, "load-program", -1)
{
  size_t len;
  SCM prog, x;

  FETCH_LENGTH (len);
  prog = scm_c_make_program (ip, len, program);

  x = sp[0];
  if (SCM_INUMP (x))
    {
      int i = SCM_INUM (x);
      SCM_PROGRAM_NARGS (prog) = i >> 5;	/* 6-5 bits */
      SCM_PROGRAM_NREST (prog) = (i >> 4) & 1;	/* 4 bit */
      SCM_PROGRAM_NLOCS (prog) = i & 15;	/* 3-0 bits */
    }
  else
    {
      SCM_PROGRAM_NARGS (prog) = SCM_INUM (sp[3]);
      SCM_PROGRAM_NREST (prog) = SCM_INUM (sp[2]);
      SCM_PROGRAM_NLOCS (prog) = SCM_INUM (sp[1]);
      if (SCM_VECTORP (x))
	SCM_PROGRAM_OBJS (prog) = x;
      sp += 3;
    }

  ip += len;
  *sp = prog;
  NEXT;
}

VM_DEFINE_INSTRUCTION (link, "link", 0)
{
  sp[1] = scm_c_env_vcell (sp[1], sp[0], 1);
  DROP ();
  NEXT;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
