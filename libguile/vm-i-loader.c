/* Copyright (C) 2001,2008,2009 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */


/* This file is included in vm_engine.c */

VM_DEFINE_LOADER (60, load_integer, "load-integer")
{
  size_t len;

  FETCH_LENGTH (len);
  if (len <= 4)
    {
      int val = 0;
      while (len-- > 0)
	val = (val << 8) + FETCH ();
      SYNC_REGISTER ();
      PUSH (scm_from_int (val));
      NEXT;
    }
  else
    SCM_MISC_ERROR ("load-integer: not implemented yet", SCM_EOL);
}

VM_DEFINE_LOADER (61, load_number, "load-number")
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

VM_DEFINE_LOADER (62, load_string, "load-string")
{
  size_t len;
  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  PUSH (scm_from_locale_stringn ((char *)ip, len));
  /* Was: scm_makfromstr (ip, len, 0) */
  ip += len;
  NEXT;
}

VM_DEFINE_LOADER (63, load_symbol, "load-symbol")
{
  size_t len;
  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  PUSH (scm_from_locale_symboln ((char *)ip, len));
  ip += len;
  NEXT;
}

VM_DEFINE_LOADER (64, load_keyword, "load-keyword")
{
  size_t len;
  FETCH_LENGTH (len);
  SYNC_REGISTER ();
  PUSH (scm_from_locale_keywordn ((char *)ip, len));
  ip += len;
  NEXT;
}

VM_DEFINE_LOADER (65, load_program, "load-program")
{
  scm_t_uint32 len;
  SCM objs, objcode;

  POP (objs);
  SYNC_REGISTER ();

  if (scm_is_vector (objs) && scm_is_false (scm_c_vector_ref (objs, 0)))
    scm_c_vector_set_x (objs, 0, scm_current_module ());

  objcode = scm_c_make_objcode_slice (SCM_PROGRAM_OBJCODE (fp[-1]), ip);
  len = sizeof (struct scm_objcode) + SCM_OBJCODE_TOTAL_LEN (objcode);

  PUSH (scm_make_program (objcode, objs, SCM_EOL));

  ip += len;

  NEXT;
}

VM_DEFINE_INSTRUCTION (66, link_now, "link-now", 0, 1, 1)
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

VM_DEFINE_LOADER (67, define, "define")
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

/*
(defun renumber-ops ()
  "start from top of buffer and renumber 'VM_DEFINE_FOO (\n' sequences"
  (interactive "")
  (save-excursion
    (let ((counter 59)) (goto-char (point-min))
      (while (re-search-forward "^VM_DEFINE_[^ ]+ (\\([^,]+\\)," (point-max) t)
        (replace-match
         (number-to-string (setq counter (1+ counter)))
          t t nil 1)))))
*/

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
