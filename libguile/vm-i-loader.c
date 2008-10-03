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

/* This file is included in vm_engine.c */

VM_DEFINE_LOADER (load_integer, "load-integer")
{
  size_t len;

  FETCH_LENGTH (len);
  if (len <= 4)
    {
      long val = 0;
      while (len-- > 0)
	val = (val << 8) + FETCH ();
      SYNC_REGISTER ();
      PUSH (scm_from_ulong (val));
      NEXT;
    }
  else
    SCM_MISC_ERROR ("load-integer: not implemented yet", SCM_EOL);
}

VM_DEFINE_LOADER (load_number, "load-number")
{
  size_t len;

  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  PUSH (scm_string_to_number (scm_from_locale_stringn ((char *)ip, len),
			      SCM_UNDEFINED /* radix = 10 */));
  /* Was: scm_istring2number (ip, len, 10)); */
  ip += len;
  NEXT;
}

VM_DEFINE_LOADER (load_string, "load-string")
{
  size_t len;
  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  PUSH (scm_from_locale_stringn ((char *)ip, len));
  /* Was: scm_makfromstr (ip, len, 0) */
  ip += len;
  NEXT;
}

VM_DEFINE_LOADER (load_symbol, "load-symbol")
{
  size_t len;
  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  PUSH (scm_from_locale_symboln ((char *)ip, len));
  ip += len;
  NEXT;
}

VM_DEFINE_LOADER (load_keyword, "load-keyword")
{
  size_t len;
  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  PUSH (scm_from_locale_keywordn ((char *)ip, len));
  ip += len;
  NEXT;
}

VM_DEFINE_LOADER (load_program, "load-program")
{
  size_t len;
  SCM prog, x;
  struct scm_program *p;

  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  prog = scm_c_make_program (ip, len, program);
  p = SCM_PROGRAM_DATA (prog);
  ip += len;

  POP (x);

  /* init meta data */
  if (SCM_PROGRAM_P (x))
    {
      p->meta = x;
      POP (x);
    }

  /* init object table */
  if (scm_is_vector (x))
    {
#if 0
      if (scm_is_simple_vector (x))
	printf ("is_simple_vector!\n");
      else
	printf ("NOT is_simple_vector\n");
#endif
      p->objs = x;
      POP (x);
    }

  /* init parameters */
  /* NOTE: format defined in system/vm/assemble.scm */
  if (SCM_I_INUMP (x))
    {
      scm_t_uint16 s = (scm_t_uint16)SCM_I_INUM (x);
      /* 16-bit representation */
      p->nargs = (s >> 12) & 0x0f;	/* 15-12 bits */
      p->nrest = (s >> 11) & 0x01;	/*    11 bit  */
      p->nlocs = (s >> 4)  & 0x7f;	/* 10-04 bits */
      p->nexts = s & 0x0f;		/* 03-00 bits */
    }
  else
    {
      /* Other cases */
      /* x is #f, and already popped off */
      POP (x); p->nexts = scm_to_unsigned_integer (x, 0, 255);
      POP (x); p->nlocs = scm_to_unsigned_integer (x, 0, 255);
      POP (x); p->nrest = scm_to_unsigned_integer (x, 0, 1);
      POP (x); p->nargs = scm_to_unsigned_integer (x, 0, 255);
    }

  PUSH (prog);
  NEXT;
}

VM_DEFINE_INSTRUCTION (link_now, "link-now", 0, 1, 1)
{
  SCM what;
  POP (what);
  SYNC_REGISTER ();
  if (SCM_LIKELY (SCM_SYMBOLP (what)))
    {
      PUSH (scm_lookup (what)); /* might longjmp */
    }
  else
    {
      SCM mod;
      /* compilation of @ or @@
         `what' is a three-element list: (MODNAME SYM INTERFACE?)
         INTERFACE? is #t if we compiled @ or #f if we compiled @@
      */
      mod = scm_resolve_module (SCM_CAR (what));
      if (scm_is_true (SCM_CADDR (what)))
        mod = scm_module_public_interface (mod);
      if (SCM_FALSEP (mod))
        {
          err_args = SCM_LIST1 (SCM_CAR (what));
          goto vm_error_no_such_module;
        }
      /* might longjmp */
      PUSH (scm_module_lookup (mod, SCM_CADR (what)));
    }
      
  NEXT;
}

VM_DEFINE_LOADER (define, "define")
{
  SCM sym;
  size_t len;

  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  sym = scm_from_locale_symboln ((char *)ip, len);
  ip += len;

  SYNC_REGISTER ();
  PUSH (scm_sym2var (sym, scm_current_module_lookup_closure (), SCM_BOOL_T));
  NEXT;
}

VM_DEFINE_LOADER (late_bind, "late-bind")
{
  SCM sym;
  size_t len;

  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  sym = scm_from_locale_symboln ((char *)ip, len);
  ip += len;

  PUSH (sym);
  NEXT;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
