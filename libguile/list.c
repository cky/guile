/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */

#include <stdio.h>
#include "_scm.h"
#include "eq.h"

#include "list.h"

#ifdef __STDC__
#include <stdarg.h>
#define var_start(x, y) va_start(x, y)
#else
#include <varargs.h>
#define var_start(x, y) va_start(x)
#endif




#ifdef __STDC__
SCM
scm_listify (SCM elt, ...)
#else
SCM
scm_listify (elt, va_alist)
     SCM elt;
     va_dcl

#endif
{
  va_list foo;
  SCM answer;
  SCM *pos;

  var_start (foo, elt);
  answer = SCM_EOL;
  pos = &answer;
  while (elt != SCM_UNDEFINED)
    {
      *pos = scm_cons (elt, SCM_EOL);
      pos = &SCM_CDR (*pos);
      elt = va_arg (foo, SCM);
    }
  return answer;
}


SCM_PROC(s_list, "list", 0, 0, 1, scm_list);
#ifdef __STDC__
SCM
scm_list(SCM objs)
#else
SCM
scm_list(objs)
     SCM objs;
#endif
{
  return objs;
}





SCM_PROC(s_null_p, "null?", 1, 0, 0, scm_null_p);
#ifdef __STDC__
SCM
scm_null_p(SCM x)
#else
SCM
scm_null_p(x)
     SCM x;
#endif
{
 return SCM_NULLP(x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_list_p, "list?", 1, 0, 0, scm_list_p);
#ifdef __STDC__
SCM
scm_list_p(SCM x)
#else
SCM
scm_list_p(x)
     SCM x;
#endif
{
  if (scm_ilength(x)<0)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}


#ifdef __STDC__
long
scm_ilength(SCM sx)
#else
long
scm_ilength(sx)
     SCM sx;
#endif
{
  register long i = 0;
  register SCM x = sx;
  do {
    if SCM_IMP(x) return SCM_NULLP(x) ? i : -1;
    if SCM_NCONSP(x) return -1;
    x = SCM_CDR(x);
    i++;
    if SCM_IMP(x) return SCM_NULLP(x) ? i : -1;
    if SCM_NCONSP(x) return -1;
    x = SCM_CDR(x);
    i++;
    sx = SCM_CDR(sx);
  }
  while (x != sx);
  return -1;
}

SCM_PROC(s_list_length, "list-length", 1, 0, 0, scm_list_length);
#ifdef __STDC__
SCM
scm_list_length(SCM x)
#else
SCM
scm_list_length(x)
     SCM x;
#endif
{
  int i;
  i = scm_ilength(x);
  SCM_ASSERT(i >= 0, x, SCM_ARG1, s_list_length);
  return SCM_MAKINUM (i);
}




SCM_PROC (s_list_append, "list-append", 0, 0, 1, scm_list_append);
#ifdef __STDC__
SCM
scm_list_append(SCM args)
#else
SCM
scm_list_append(args)
     SCM args;
#endif
{
  SCM res = SCM_EOL;
  SCM *lloc = &res, arg;
  if SCM_IMP(args) {
    SCM_ASSERT(SCM_NULLP(args), args, SCM_ARGn, s_list_append);
    return res;
  }
  SCM_ASSERT(SCM_CONSP(args), args, SCM_ARGn, s_list_append);
  while (1) {
    arg = SCM_CAR(args);
    args = SCM_CDR(args);
    if SCM_IMP(args) {
      *lloc = arg;
      SCM_ASSERT(SCM_NULLP(args), args, SCM_ARGn, s_list_append);
      return res;
    }
    SCM_ASSERT(SCM_CONSP(args), args, SCM_ARGn, s_list_append);
    for(;SCM_NIMP(arg);arg = SCM_CDR(arg)) {
      SCM_ASSERT(SCM_CONSP(arg), arg, SCM_ARGn, s_list_append);
      *lloc = scm_cons(SCM_CAR(arg), SCM_EOL);
      lloc = &SCM_CDR(*lloc);
    }
    SCM_ASSERT(SCM_NULLP(arg), arg, SCM_ARGn, s_list_append);
  }
}


SCM_PROC (s_list_append_x, "list-append!", 0, 0, 1, scm_list_append_x);
#ifdef __STDC__
SCM
scm_list_append_x(SCM args)
#else
SCM
scm_list_append_x(args)
     SCM args;
#endif
{
  SCM arg;
 tail:
  if SCM_NULLP(args) return SCM_EOL;
  arg = SCM_CAR(args);
  SCM_ASSERT(SCM_NULLP(arg) || (SCM_NIMP(arg) && SCM_CONSP(arg)), arg, SCM_ARG1, s_list_append_x);
  args = SCM_CDR(args);
  if SCM_NULLP(args) return arg;
  if SCM_NULLP(arg) goto tail;
  SCM_CDR(scm_last_pair(arg)) = scm_list_append_x(args);
  return arg;
}





SCM_PROC (s_list_reverse, "list-reverse", 1, 0, 0, scm_list_reverse);
#ifdef __STDC__
SCM
scm_list_reverse(SCM lst)
#else
SCM
scm_list_reverse(lst)
     SCM lst;
#endif
{
	SCM res = SCM_EOL;
	SCM p = lst;
	for(;SCM_NIMP(p);p = SCM_CDR(p)) {
		SCM_ASSERT(SCM_CONSP(p), lst, SCM_ARG1, s_list_reverse);
		res = scm_cons(SCM_CAR(p), res);
	}
	SCM_ASSERT(SCM_NULLP(p), lst, SCM_ARG1, s_list_reverse);
	return res;
}

SCM_PROC (s_list_reverse_x, "list-reverse!", 1, 1, 0, scm_list_reverse_x);
#ifdef __STDC__
SCM
scm_list_reverse_x (SCM lst, SCM newtail)
#else
SCM
scm_list_reverse_x (lst, newtail)
     SCM lst;
     SCM newtail;
#endif
{
  SCM old_tail;
  if (newtail == SCM_UNDEFINED)
    newtail = SCM_EOL;

 loop:
  if (!(SCM_NIMP (lst) && SCM_CONSP (lst)))
    return lst;

  old_tail = SCM_CDR (lst);
  SCM_SETCDR (lst, newtail);
  if (SCM_NULLP (old_tail))
    return lst;

  newtail = lst;
  lst = old_tail;
  goto loop;
}





SCM_PROC(s_list_ref, "list-ref", 2, 0, 0, scm_list_ref);
#ifdef __STDC__
SCM
scm_list_ref(SCM lst, SCM k)
#else
SCM
scm_list_ref(lst, k)
     SCM lst;
     SCM k;
#endif
{
	register long i;
	SCM_ASSERT(SCM_INUMP(k), k, SCM_ARG2, s_list_ref);
	i = SCM_INUM(k);
	SCM_ASSERT(i >= 0, k, SCM_ARG2, s_list_ref);
	while (i-- > 0) {
		SCM_ASRTGO(SCM_NIMP(lst) && SCM_CONSP(lst), erout);
		lst = SCM_CDR(lst);
	}
erout:	SCM_ASSERT(SCM_NIMP(lst) && SCM_CONSP(lst),
	       SCM_NULLP(lst)?k:lst, SCM_NULLP(lst)?SCM_OUTOFRANGE:SCM_ARG1, s_list_ref);
	return SCM_CAR(lst);
}

SCM_PROC(s_list_set_x, "list-set!", 3, 0, 0, scm_list_set_x);
#ifdef __STDC__
SCM
scm_list_set_x(SCM lst, SCM k, SCM val)
#else
SCM
scm_list_set_x(lst, k, val)
     SCM lst;
     SCM k;
     SCM val;
#endif
{
	register long i;
	SCM_ASSERT(SCM_INUMP(k), k, SCM_ARG2, s_list_set_x);
	i = SCM_INUM(k);
	SCM_ASSERT(i >= 0, k, SCM_ARG2, s_list_set_x);
	while (i-- > 0) {
		SCM_ASRTGO(SCM_NIMP(lst) && SCM_CONSP(lst), erout);
		lst = SCM_CDR(lst);
	}
erout:	SCM_ASSERT(SCM_NIMP(lst) && SCM_CONSP(lst),
	       SCM_NULLP(lst)?k:lst, SCM_NULLP(lst)?SCM_OUTOFRANGE:SCM_ARG1, s_list_set_x);
	SCM_CAR (lst) = val;
	return val;
}



SCM_PROC(s_list_cdr_set_x, "list-cdr-set!", 3, 0, 0, scm_list_cdr_set_x);
#ifdef __STDC__
SCM
scm_list_cdr_set_x(SCM lst, SCM k, SCM val)
#else
SCM
scm_list_cdr_set_x(lst, k, val)
     SCM lst;
     SCM k;
     SCM val;
#endif
{
	register long i;
	SCM_ASSERT(SCM_INUMP(k), k, SCM_ARG2, s_list_cdr_set_x);
	i = SCM_INUM(k);
	SCM_ASSERT(i >= 0, k, SCM_ARG2, s_list_cdr_set_x);
	while (i-- > 0) {
		SCM_ASRTGO(SCM_NIMP(lst) && SCM_CONSP(lst), erout);
		lst = SCM_CDR(lst);
	}
erout:	SCM_ASSERT(SCM_NIMP(lst) && SCM_CONSP(lst),
	       SCM_NULLP(lst)?k:lst, SCM_NULLP(lst)?SCM_OUTOFRANGE:SCM_ARG1, s_list_cdr_set_x);
	SCM_SETCDR (lst, val);
	return val;
}






SCM_PROC(s_last_pair, "last-pair", 1, 0, 0, scm_last_pair);
#ifdef __STDC__
SCM
scm_last_pair(SCM sx)
#else
SCM
scm_last_pair(sx)
     SCM sx;
#endif
{
  register SCM res = sx;
  register SCM x;

  if (SCM_NULLP (sx))
    return SCM_EOL;

  SCM_ASSERT(SCM_NIMP(res) && SCM_CONSP(res), res, SCM_ARG1, s_last_pair);
  while (!0) {
    x = SCM_CDR(res);
    if (SCM_IMP(x) || SCM_NCONSP(x)) return res;
    res = x;
    x = SCM_CDR(res);
    if (SCM_IMP(x) || SCM_NCONSP(x)) return res;
    res = x;
    sx = SCM_CDR(sx);
    SCM_ASSERT(x != sx, sx, SCM_ARG1, s_last_pair);
  }
}

SCM_PROC(s_list_cdr_ref, "list-cdr-ref", 2, 0, 0, scm_list_tail);
SCM_PROC(s_list_tail, "list-tail", 2, 0, 0, scm_list_tail);
#ifdef __STDC__
SCM
scm_list_tail(SCM lst, SCM k)
#else
SCM
scm_list_tail(lst, k)
     SCM lst;
     SCM k;
#endif
{
  register long i;
  SCM_ASSERT(SCM_INUMP(k), k, SCM_ARG2, s_list_tail);
  i = SCM_INUM(k);
  while (i-- > 0) {
    SCM_ASSERT(SCM_NIMP(lst) && SCM_CONSP(lst), lst, SCM_ARG1, s_list_tail);
    lst = SCM_CDR(lst);
  }
  return lst;
}


SCM_PROC(s_list_head, "list-head", 2, 0, 0, scm_list_head);
#ifdef __STDC__
SCM
scm_list_head(SCM lst, SCM k)
#else
SCM
scm_list_head(lst, k)
     SCM lst;
     SCM k;
#endif
{
  SCM answer;
  SCM * pos;
  register long i;

  SCM_ASSERT(SCM_INUMP(k), k, SCM_ARG2, s_list_head);
  answer = SCM_EOL;
  pos = &answer;
  i = SCM_INUM(k);
  while (i-- > 0)
    {
      SCM_ASSERT(SCM_NIMP(lst) && SCM_CONSP(lst), lst, SCM_ARG1, s_list_head);
      *pos = scm_cons (SCM_CAR (lst), SCM_EOL);
      pos = &SCM_CDR (*pos);
      lst = SCM_CDR(lst);
    }
  return answer;
}




#ifdef __STDC__
static void
sloppy_mem_check (SCM obj, char * where, char * why)
#else
static void
sloppy_mem_check (obj, where, why)
     SCM obj;
     char * where;
     char * why;
#endif
{
  SCM_ASSERT ((scm_ilength (obj) >= 0), obj, where, why);
}


SCM_PROC (s_sloppy_memq, "sloppy-memq", 2, 0, 0, scm_sloppy_memq);
#ifdef __STDC__
SCM
scm_sloppy_memq(SCM x, SCM lst)
#else
SCM
scm_sloppy_memq(x, lst)
     SCM x;
     SCM lst;
#endif
{
  for(;  SCM_NIMP(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_CAR(lst)==x)
	return lst;
    }
  return lst;
}


SCM_PROC (s_sloppy_memv, "sloppy-memv", 2, 0, 0, scm_sloppy_memv);
#ifdef __STDC__
SCM
scm_sloppy_memv(SCM x, SCM lst)
#else
SCM
scm_sloppy_memv(x, lst)
     SCM x;
     SCM lst;
#endif
{
  for(;  SCM_NIMP(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR(lst), x))
	return lst;
    }
  return lst;
}


SCM_PROC (s_sloppy_member, "sloppy-member", 2, 0, 0, scm_sloppy_member);
#ifdef __STDC__
SCM
scm_sloppy_member (SCM x, SCM lst)
#else
SCM
scm_sloppy_member (x, lst)
     SCM x;
     SCM lst;
#endif
{
  for(;  SCM_NIMP(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_BOOL_F != scm_equal_p (SCM_CAR(lst), x))
	return lst;
    }
  return lst;
}



SCM_PROC(s_memq, "memq", 2, 0, 0, scm_memq);
#ifdef __STDC__
SCM
scm_memq(SCM x, SCM lst)
#else
SCM
scm_memq(x, lst)
     SCM x;
     SCM lst;
#endif
{
  SCM answer;
  answer = scm_sloppy_memq (x, lst);
  sloppy_mem_check (answer, (char *)SCM_ARG2, s_memq);
  return answer;
}



SCM_PROC(s_memv, "memv", 2, 0, 0, scm_memv);
#ifdef __STDC__
SCM
scm_memv(SCM x, SCM lst)
#else
SCM
scm_memv(x, lst)
     SCM x;
     SCM lst;
#endif
{
  SCM answer;
  answer = scm_sloppy_memv (x, lst);
  sloppy_mem_check (answer, (char *)SCM_ARG2, s_memv);
  return answer;
}


SCM_PROC(s_member, "member", 2, 0, 0, scm_member);
#ifdef __STDC__
SCM
scm_member(SCM x, SCM lst)
#else
SCM
scm_member(x, lst)
     SCM x;
     SCM lst;
#endif
{
  SCM answer;
  answer = scm_sloppy_member (x, lst);
  sloppy_mem_check (answer, (char *)SCM_ARG2, s_member);
  return answer;
}




SCM_PROC(s_delq_x, "delq!", 2, 0, 0, scm_delq_x);
#ifdef __STDC__
SCM
scm_delq_x (SCM item, SCM lst)
#else
SCM
scm_delq_x (item, lst)
     SCM item;
     SCM lst;
#endif
{
  SCM start;

  if (SCM_IMP (lst) || SCM_NCONSP (lst))
    return lst;

  if (SCM_CAR (lst) == item)
    return SCM_CDR (lst);

  start = lst;

  while (SCM_NIMP (SCM_CDR (lst)) && SCM_CONSP (SCM_CDR (lst)))
    {
      if (SCM_CAR (SCM_CDR (lst)) == item)
	{
	  SCM_SETCDR (lst, SCM_CDR (SCM_CDR (lst)));
	  return start;
	}
      lst = SCM_CDR (lst);
    }
  return start;
}


SCM_PROC(s_delv_x, "delv!", 2, 0, 0, scm_delv_x);
#ifdef __STDC__
SCM
scm_delv_x (SCM item, SCM lst)
#else
SCM
scm_delv_x (item, lst)
     SCM item;
     SCM lst;
#endif
{
  SCM start;

  if (SCM_IMP (lst) || SCM_NCONSP (lst))
    return lst;

  if (SCM_BOOL_F != scm_eqv_p (SCM_CAR (lst), item))
    return SCM_CDR (lst);

  start = lst;

  while (SCM_NIMP (SCM_CDR (lst)) && SCM_CONSP (SCM_CDR (lst)))
    {
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR (SCM_CDR (lst)), item))
	{
	  SCM_SETCDR (lst, SCM_CDR (SCM_CDR (lst)));
	  return start;
	}
      lst = SCM_CDR (lst);
    }
  return start;
}



SCM_PROC(s_delete_x, "delete!", 2, 0, 0, scm_delete_x);
#ifdef __STDC__
SCM
scm_delete_x (SCM item, SCM lst)
#else
SCM
scm_delete_x (item, lst)
     SCM item;
     SCM lst;
#endif
{
  SCM start;

  if (SCM_IMP (lst) || SCM_NCONSP (lst))
    return lst;

  if (SCM_BOOL_F != scm_equal_p (SCM_CAR (lst), item))
    return SCM_CDR (lst);

  start = lst;

  while (SCM_NIMP (SCM_CDR (lst)) && SCM_CONSP (SCM_CDR (lst)))
    {
      if (SCM_BOOL_F != scm_equal_p (SCM_CAR (SCM_CDR (lst)), item))
	{
	  SCM_SETCDR (lst, SCM_CDR (SCM_CDR (lst)));
	  return start;
	}
      lst = SCM_CDR (lst);
    }
  return start;
}




SCM_PROC (s_list_copy, "list-copy", 1, 0, 0, scm_list_copy);
#ifdef __STDC__
SCM 
scm_list_copy (SCM lst)
#else
SCM 
scm_list_copy (lst)
     SCM lst;
#endif
{
  SCM newlst;
  SCM * fill_here;
  SCM from_here;

  newlst = SCM_EOL;
  fill_here = &newlst;
  from_here = lst;

  while (SCM_NIMP (from_here) && SCM_CONSP (from_here))
    {
      SCM c;
      c = scm_cons (SCM_CAR (from_here), SCM_CDR (from_here));
      *fill_here = c;
      fill_here = &SCM_CDR (c);
      from_here = SCM_CDR (from_here);
    }
  return newlst;
}



SCM_PROC (s_delq, "delq", 2, 0, 0, scm_delq);
#ifdef __STDC__
SCM
scm_delq (SCM item, SCM lst)
#else
SCM
scm_delq (item, lst)
     SCM item;
     SCM lst;
#endif
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delq_x (item, copy);
}

SCM_PROC (s_delv, "delv", 2, 0, 0, scm_delv);
#ifdef __STDC__
SCM
scm_delv (SCM item, SCM lst)
#else
SCM
scm_delv (item, lst)
     SCM item;
     SCM lst;
#endif
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delv_x (item, copy);
}

SCM_PROC (s_delete, "delete", 2, 0, 0, scm_delete);
#ifdef __STDC__
SCM
scm_delete (SCM item, SCM lst)
#else
SCM
scm_delete (item, lst)
     SCM item;
     SCM lst;
#endif
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delete_x (item, copy);
}




#ifdef __STDC__
void
scm_init_list (void)
#else
void
scm_init_list ()
#endif
{
#include "list.x"
}

