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


/*
 * Predicates
 */

#define ARGS1(a1)	SCM a1 = sp[0];
#define ARGS2(a1,a2)	SCM a1 = sp[-1], a2 = sp[0]; sp--; NULLSTACK (1);
#define ARGS3(a1,a2,a3)	SCM a1 = sp[-2], a2 = sp[-1], a3 = sp[0]; sp -= 2; NULLSTACK (2);

#define RETURN(x)	do { *sp = x; NEXT; } while (0)

VM_DEFINE_FUNCTION (80, not, "not", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (SCM_FALSEP (x)));
}

VM_DEFINE_FUNCTION (81, not_not, "not-not", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (!SCM_FALSEP (x)));
}

VM_DEFINE_FUNCTION (82, eq, "eq?", 2)
{
  ARGS2 (x, y);
  RETURN (SCM_BOOL (SCM_EQ_P (x, y)));
}

VM_DEFINE_FUNCTION (83, not_eq, "not-eq?", 2)
{
  ARGS2 (x, y);
  RETURN (SCM_BOOL (!SCM_EQ_P (x, y)));
}

VM_DEFINE_FUNCTION (84, nullp, "null?", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (SCM_NULLP (x)));
}

VM_DEFINE_FUNCTION (85, not_nullp, "not-null?", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (!SCM_NULLP (x)));
}

VM_DEFINE_FUNCTION (86, eqv, "eqv?", 2)
{
  ARGS2 (x, y);
  if (SCM_EQ_P (x, y))
    RETURN (SCM_BOOL_T);
  if (SCM_IMP (x) || SCM_IMP (y))
    RETURN (SCM_BOOL_F);
  SYNC_REGISTER ();
  RETURN (scm_eqv_p (x, y));
}

VM_DEFINE_FUNCTION (87, equal, "equal?", 2)
{
  ARGS2 (x, y);
  if (SCM_EQ_P (x, y))
    RETURN (SCM_BOOL_T);
  if (SCM_IMP (x) || SCM_IMP (y))
    RETURN (SCM_BOOL_F);
  SYNC_REGISTER ();
  RETURN (scm_equal_p (x, y));
}

VM_DEFINE_FUNCTION (88, pairp, "pair?", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (SCM_CONSP (x)));
}

VM_DEFINE_FUNCTION (89, listp, "list?", 1)
{
  ARGS1 (x);
  RETURN (SCM_BOOL (scm_ilength (x) >= 0));
}


/*
 * Basic data
 */

VM_DEFINE_FUNCTION (90, cons, "cons", 2)
{
  ARGS2 (x, y);
  CONS (x, x, y);
  RETURN (x);
}

#define VM_VALIDATE_CONS(x)                     \
  if (SCM_UNLIKELY (!scm_is_pair (x)))          \
    { finish_args = x;                          \
      goto vm_error_not_a_pair;                 \
    }
  
VM_DEFINE_FUNCTION (91, car, "car", 1)
{
  ARGS1 (x);
  VM_VALIDATE_CONS (x);
  RETURN (SCM_CAR (x));
}

VM_DEFINE_FUNCTION (92, cdr, "cdr", 1)
{
  ARGS1 (x);
  VM_VALIDATE_CONS (x);
  RETURN (SCM_CDR (x));
}

VM_DEFINE_FUNCTION (93, set_car, "set-car!", 2)
{
  ARGS2 (x, y);
  VM_VALIDATE_CONS (x);
  SCM_SETCAR (x, y);
  RETURN (SCM_UNSPECIFIED);
}

VM_DEFINE_FUNCTION (94, set_cdr, "set-cdr!", 2)
{
  ARGS2 (x, y);
  VM_VALIDATE_CONS (x);
  SCM_SETCDR (x, y);
  RETURN (SCM_UNSPECIFIED);
}


/*
 * Numeric relational tests
 */

#undef REL
#define REL(crel,srel)						\
{								\
  ARGS2 (x, y);							\
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))			\
    RETURN (SCM_BOOL (SCM_I_INUM (x) crel SCM_I_INUM (y)));	\
  SYNC_REGISTER ();                                             \
  RETURN (srel (x, y));                                         \
}

VM_DEFINE_FUNCTION (95, ee, "ee?", 2)
{
  REL (==, scm_num_eq_p);
}

VM_DEFINE_FUNCTION (96, lt, "lt?", 2)
{
  REL (<, scm_less_p);
}

VM_DEFINE_FUNCTION (97, le, "le?", 2)
{
  REL (<=, scm_leq_p);
}

VM_DEFINE_FUNCTION (98, gt, "gt?", 2)
{
  REL (>, scm_gr_p);
}

VM_DEFINE_FUNCTION (99, ge, "ge?", 2)
{
  REL (>=, scm_geq_p);
}


/*
 * Numeric functions
 */

#undef FUNC2
#define FUNC2(CFUNC,SFUNC)				\
{							\
  ARGS2 (x, y);						\
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))		\
    {							\
      scm_t_bits n = SCM_I_INUM (x) CFUNC SCM_I_INUM (y);\
      if (SCM_FIXABLE (n))				\
	RETURN (SCM_I_MAKINUM (n));			\
    }							\
  SYNC_REGISTER ();					\
  RETURN (SFUNC (x, y));				\
}

VM_DEFINE_FUNCTION (100, add, "add", 2)
{
  FUNC2 (+, scm_sum);
}

VM_DEFINE_FUNCTION (101, sub, "sub", 2)
{
  FUNC2 (-, scm_difference);
}

VM_DEFINE_FUNCTION (102, mul, "mul", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_product (x, y));
}

VM_DEFINE_FUNCTION (103, div, "div", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_divide (x, y));
}

VM_DEFINE_FUNCTION (104, quo, "quo", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_quotient (x, y));
}

VM_DEFINE_FUNCTION (105, rem, "rem", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_remainder (x, y));
}

VM_DEFINE_FUNCTION (106, mod, "mod", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_modulo (x, y));
}


/*
 * GOOPS support
 */
VM_DEFINE_FUNCTION (107, slot_ref, "slot-ref", 2)
{
  size_t slot;
  ARGS2 (instance, idx);
  slot = SCM_I_INUM (idx);
  RETURN (SCM_PACK (SCM_STRUCT_DATA (instance) [slot]));
}

VM_DEFINE_FUNCTION (108, slot_set, "slot-set", 3)
{
  size_t slot;
  ARGS3 (instance, idx, val);
  slot = SCM_I_INUM (idx);
  SCM_STRUCT_DATA (instance) [slot] = SCM_UNPACK (val);
  RETURN (SCM_UNSPECIFIED);
}

/*
(defun renumber-ops ()
  "start from top of buffer and renumber 'VM_DEFINE_FOO (\n' sequences"
  (interactive "")
  (save-excursion
    (let ((counter 79)) (goto-char (point-min))
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
