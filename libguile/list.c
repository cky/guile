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


/* creating lists */

/* SCM_P won't help us deal with varargs here.  */
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
      pos = SCM_CDRLOC (*pos);
      elt = va_arg (foo, SCM);
    }
  return answer;
}


SCM_PROC(s_list, "list", 0, 0, 1, scm_list);
SCM
scm_list(objs)
     SCM objs;
{
  return objs;
}




/* general questions about lists --- null?, list?, length, etc.  */

SCM_PROC(s_null_p, "null?", 1, 0, 0, scm_null_p);
SCM
scm_null_p(x)
     SCM x;
{
 return SCM_NULLP(x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_list_p, "list?", 1, 0, 0, scm_list_p);
SCM
scm_list_p(x)
     SCM x;
{
  if (scm_ilength(x)<0)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}


/* Return the length of SX, or -1 if it's not a proper list.
   This uses the "tortoise and hare" algorithm to detect "infinitely
   long" lists (i.e. lists with cycles in their cdrs), and returns -1
   if it does find one.  */
long
scm_ilength(sx)
     SCM sx;
{
  register long i = 0;
  register SCM tortoise = sx;
  register SCM hare = sx;

  do {
    if SCM_IMP(hare) return SCM_NULLP(hare) ? i : -1;
    if SCM_NCONSP(hare) return -1;
    hare = SCM_CDR(hare);
    i++;
    if SCM_IMP(hare) return SCM_NULLP(hare) ? i : -1;
    if SCM_NCONSP(hare) return -1;
    hare = SCM_CDR(hare);
    i++;
    /* For every two steps the hare takes, the tortoise takes one.  */
    tortoise = SCM_CDR(tortoise);
  }
  while (hare != tortoise);

  /* If the tortoise ever catches the hare, then the list must contain
     a cycle.  */
  return -1;
}

SCM_PROC(s_list_length, "list-length", 1, 0, 0, scm_list_length);
SCM
scm_list_length(x)
     SCM x;
{
  int i;
  i = scm_ilength(x);
  SCM_ASSERT(i >= 0, x, SCM_ARG1, s_list_length);
  return SCM_MAKINUM (i);
}



/* appending lists */

SCM_PROC (s_list_append, "list-append", 0, 0, 1, scm_list_append);
SCM
scm_list_append(args)
     SCM args;
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
      lloc = SCM_CDRLOC(*lloc);
    }
    SCM_ASSERT(SCM_NULLP(arg), arg, SCM_ARGn, s_list_append);
  }
}


SCM_PROC (s_list_append_x, "list-append!", 0, 0, 1, scm_list_append_x);
SCM
scm_list_append_x(args)
     SCM args;
{
  SCM arg;
 tail:
  if SCM_NULLP(args) return SCM_EOL;
  arg = SCM_CAR(args);
  args = SCM_CDR(args);
  if SCM_NULLP(args) return arg;
  if SCM_NULLP(arg) goto tail;
  SCM_ASSERT(SCM_NIMP(arg) && SCM_CONSP(arg), arg, SCM_ARG1, s_list_append_x);
  SCM_SETCDR (scm_last_pair (arg), scm_list_append_x (args));
  return arg;
}


SCM_PROC(s_last_pair, "last-pair", 1, 0, 0, scm_last_pair);
SCM
scm_last_pair(sx)
     SCM sx;
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


/* reversing lists */

SCM_PROC (s_list_reverse, "list-reverse", 1, 0, 0, scm_list_reverse);
SCM
scm_list_reverse(lst)
     SCM lst;
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
SCM
scm_list_reverse_x (lst, newtail)
     SCM lst;
     SCM newtail;
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



/* indexing lists by element number */

SCM_PROC(s_list_ref, "list-ref", 2, 0, 0, scm_list_ref);
SCM
scm_list_ref(lst, k)
     SCM lst;
     SCM k;
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
SCM
scm_list_set_x(lst, k, val)
     SCM lst;
     SCM k;
     SCM val;
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
	SCM_SETCAR (lst, val);
	return val;
}


SCM_PROC(s_list_cdr_ref, "list-cdr-ref", 2, 0, 0, scm_list_tail);
SCM_PROC(s_list_tail, "list-tail", 2, 0, 0, scm_list_tail);
SCM
scm_list_tail(lst, k)
     SCM lst;
     SCM k;
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


SCM_PROC(s_list_cdr_set_x, "list-cdr-set!", 3, 0, 0, scm_list_cdr_set_x);
SCM
scm_list_cdr_set_x(lst, k, val)
     SCM lst;
     SCM k;
     SCM val;
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



/* copying lists, perhaps partially */

SCM_PROC(s_list_head, "list-head", 2, 0, 0, scm_list_head);
SCM
scm_list_head(lst, k)
     SCM lst;
     SCM k;
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
      pos = SCM_CDRLOC (*pos);
      lst = SCM_CDR(lst);
    }
  return answer;
}


SCM_PROC (s_list_copy, "list-copy", 1, 0, 0, scm_list_copy);
SCM 
scm_list_copy (lst)
     SCM lst;
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
      fill_here = SCM_CDRLOC (c);
      from_here = SCM_CDR (from_here);
    }
  return newlst;
}


/* membership tests (memq, memv, etc.) */ 

static void sloppy_mem_check SCM_P ((SCM obj, char * where, char * why));

static void
sloppy_mem_check (obj, where, why)
     SCM obj;
     char * where;
     char * why;
{
  SCM_ASSERT ((scm_ilength (obj) >= 0), obj, where, why);
}


SCM_PROC (s_sloppy_memq, "sloppy-memq", 2, 0, 0, scm_sloppy_memq);
SCM
scm_sloppy_memq(x, lst)
     SCM x;
     SCM lst;
{
  for(;  SCM_NIMP(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_CAR(lst)==x)
	return lst;
    }
  return lst;
}


SCM_PROC (s_sloppy_memv, "sloppy-memv", 2, 0, 0, scm_sloppy_memv);
SCM
scm_sloppy_memv(x, lst)
     SCM x;
     SCM lst;
{
  for(;  SCM_NIMP(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR(lst), x))
	return lst;
    }
  return lst;
}


SCM_PROC (s_sloppy_member, "sloppy-member", 2, 0, 0, scm_sloppy_member);
SCM
scm_sloppy_member (x, lst)
     SCM x;
     SCM lst;
{
  for(;  SCM_NIMP(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_BOOL_F != scm_equal_p (SCM_CAR(lst), x))
	return lst;
    }
  return lst;
}



SCM_PROC(s_memq, "memq", 2, 0, 0, scm_memq);
SCM
scm_memq(x, lst)
     SCM x;
     SCM lst;
{
  SCM answer;
  answer = scm_sloppy_memq (x, lst);
  sloppy_mem_check (answer, (char *)SCM_ARG2, s_memq);
  return (answer == SCM_EOL) ? SCM_BOOL_F : answer;
}



SCM_PROC(s_memv, "memv", 2, 0, 0, scm_memv);
SCM
scm_memv(x, lst)
     SCM x;
     SCM lst;
{
  SCM answer;
  answer = scm_sloppy_memv (x, lst);
  sloppy_mem_check (answer, (char *)SCM_ARG2, s_memv);
  return (answer == SCM_EOL) ? SCM_BOOL_F : answer;
}


SCM_PROC(s_member, "member", 2, 0, 0, scm_member);
SCM
scm_member(x, lst)
     SCM x;
     SCM lst;
{
  SCM answer;
  answer = scm_sloppy_member (x, lst);
  sloppy_mem_check (answer, (char *)SCM_ARG2, s_member);
  return (answer == SCM_EOL) ? SCM_BOOL_F : answer;
}



/* deleting elements from a list (delq, etc.) */

SCM_PROC(s_delq_x, "delq!", 2, 0, 0, scm_delq_x);
SCM
scm_delq_x (item, lst)
     SCM item;
     SCM lst;
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_NIMP (walk) && SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_CAR (walk) == item)
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}


SCM_PROC(s_delv_x, "delv!", 2, 0, 0, scm_delv_x);
SCM
scm_delv_x (item, lst)
     SCM item;
     SCM lst;
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_NIMP (walk) && SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR (walk), item))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}



SCM_PROC(s_delete_x, "delete!", 2, 0, 0, scm_delete_x);
SCM
scm_delete_x (item, lst)
     SCM item;
     SCM lst;
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_NIMP (walk) && SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_BOOL_F != scm_equal_p (SCM_CAR (walk), item))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }

  return lst;
}





SCM_PROC (s_delq, "delq", 2, 0, 0, scm_delq);
SCM
scm_delq (item, lst)
     SCM item;
     SCM lst;
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delq_x (item, copy);
}

SCM_PROC (s_delv, "delv", 2, 0, 0, scm_delv);
SCM
scm_delv (item, lst)
     SCM item;
     SCM lst;
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delv_x (item, copy);
}

SCM_PROC (s_delete, "delete", 2, 0, 0, scm_delete);
SCM
scm_delete (item, lst)
     SCM item;
     SCM lst;
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delete_x (item, copy);
}



void
scm_init_list ()
{
#include "list.x"
}
