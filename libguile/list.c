/* Copyright (C) 1995,1996,1997,2000,2001 Free Software Foundation, Inc.
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



#include "libguile/_scm.h"
#include "libguile/eq.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/list.h"

#ifdef __STDC__
#include <stdarg.h>
#define var_start(x, y) va_start(x, y)
#else
#include <varargs.h>
#define var_start(x, y) va_start(x)
#endif


/* creating lists */

#define SCM_I_CONS(cell,x,y)			\
do {						\
  cell = scm_cell ((scm_t_bits)x, (scm_t_bits)y);			\
} while (0)

SCM
scm_list_1 (SCM e1)
{
  SCM c1;
  SCM_I_CONS (c1, e1, SCM_EOL);
  return c1;
}

SCM
scm_list_2 (SCM e1, SCM e2)
{
  SCM c1, c2;
  SCM_I_CONS (c2, e2, SCM_EOL);
  SCM_I_CONS (c1, e1, c2);
  return c1;
}

SCM
scm_list_3 (SCM e1, SCM e2, SCM e3)
{
  SCM c1, c2, c3;
  SCM_I_CONS (c3, e3, SCM_EOL);
  SCM_I_CONS (c2, e2, c3);
  SCM_I_CONS (c1, e1, c2);
  return c1;
}

SCM
scm_list_4 (SCM e1, SCM e2, SCM e3, SCM e4)
{
  return scm_cons2 (e1, e2, scm_list_2 (e3, e4));
}

SCM
scm_list_5 (SCM e1, SCM e2, SCM e3, SCM e4, SCM e5)
{
  return scm_cons2 (e1, e2, scm_list_3 (e3, e4, e5));
}

SCM
scm_list_n (SCM elt, ...)
{
  va_list foo;
  SCM answer = SCM_EOL;
  SCM *pos = &answer;

  var_start (foo, elt);
  while (! SCM_UNBNDP (elt))
    {
      *pos = scm_cons (elt, SCM_EOL);
      pos = SCM_CDRLOC (*pos);
      elt = va_arg (foo, SCM);
    }
  return answer;
}


SCM_DEFINE (scm_list, "list", 0, 0, 1, 
           (SCM objs),
	    "Return a list containing @var{objs}, the arguments to\n"
	    "@code{list}.")
#define FUNC_NAME s_scm_list
{
  return objs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_cons_star, "cons*", 1, 0, 1, 
            (SCM arg, SCM rest),
	    "Like @code{list}, but the last arg provides the tail of the\n"
	    "constructed list, returning @code{(cons @var{arg1} (cons\n"
	    "@var{arg2} (cons @dots{} @var{argn})))}.  Requires at least one\n"
	    "argument.  If given one argument, that argument is returned as\n"
	    "result.  This function is called @code{list*} in some other\n"
	    "Schemes and in Common LISP.")
#define FUNC_NAME s_scm_cons_star
{
  SCM_VALIDATE_REST_ARGUMENT (rest);
  if (!SCM_NULLP (rest))
    {
      SCM prev = arg = scm_cons (arg, rest);
      while (SCM_NNULLP (SCM_CDR (rest)))
	{
	  prev = rest;
	  rest = SCM_CDR (rest);
	}
      SCM_SETCDR (prev, SCM_CAR (rest));
    }
  return arg;
}
#undef FUNC_NAME



/* general questions about lists --- null?, list?, length, etc.  */

SCM_DEFINE (scm_null_p, "null?", 1, 0, 0, 
           (SCM x),
	    "Return @code{#t} iff @var{x} is the empty list, else @code{#f}.")
#define FUNC_NAME s_scm_null_p
{
  return SCM_BOOL (SCM_NULL_OR_NIL_P (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_p, "list?", 1, 0, 0, 
           (SCM x),
	    "Return @code{#t} iff @var{x} is a proper list, else @code{#f}.")
#define FUNC_NAME s_scm_list_p
{
  return SCM_BOOL (scm_ilength (x) >= 0);
}
#undef FUNC_NAME


/* Return the length of SX, or -1 if it's not a proper list.
   This uses the "tortoise and hare" algorithm to detect "infinitely
   long" lists (i.e. lists with cycles in their cdrs), and returns -1
   if it does find one.  */
long
scm_ilength(SCM sx)
{
  long i = 0;
  SCM tortoise = sx;
  SCM hare = sx;

  do {
    if (SCM_NULL_OR_NIL_P(hare)) return i;
    if (SCM_NCONSP(hare)) return -1;
    hare = SCM_CDR(hare);
    i++;
    if (SCM_NULL_OR_NIL_P(hare)) return i;
    if (SCM_NCONSP(hare)) return -1;
    hare = SCM_CDR(hare);
    i++;
    /* For every two steps the hare takes, the tortoise takes one.  */
    tortoise = SCM_CDR(tortoise);
  }
  while (! SCM_EQ_P (hare, tortoise));

  /* If the tortoise ever catches the hare, then the list must contain
     a cycle.  */
  return -1;
}


SCM_DEFINE (scm_length, "length", 1, 0, 0, 
           (SCM lst),
	    "Return the number of elements in list @var{lst}.")
#define FUNC_NAME s_scm_length
{
  long i;
  SCM_VALIDATE_LIST_COPYLEN (1,lst,i);
  return SCM_MAKINUM (i);
}
#undef FUNC_NAME



/* appending lists */

SCM_DEFINE (scm_append, "append", 0, 0, 1, 
            (SCM args),
	    "Return a list consisting of the elements the lists passed as\n"
	    "arguments.\n"
	    "@lisp\n"
	    "(append '(x) '(y))          @result{}  (x y)\n"
	    "(append '(a) '(b c d))      @result{}  (a b c d)\n"
	    "(append '(a (b)) '((c)))    @result{}  (a (b) (c))\n"
	    "@end lisp\n"
	    "The resulting list is always newly allocated, except that it\n"
	    "shares structure with the last list argument.  The last\n"
	    "argument may actually be any object; an improper list results\n"
	    "if the last argument is not a proper list.\n"
	    "@lisp\n"
	    "(append '(a b) '(c . d))    @result{}  (a b c . d)\n"
	    "(append '() 'a)             @result{}  a\n"
	    "@end lisp")
#define FUNC_NAME s_scm_append
{
  SCM_VALIDATE_REST_ARGUMENT (args);
  if (SCM_NULLP (args)) {
    return SCM_EOL;
  } else {
    SCM res = SCM_EOL;
    SCM *lloc = &res;
    SCM arg = SCM_CAR (args);
    args = SCM_CDR (args);
    while (!SCM_NULLP (args)) {
      while (SCM_CONSP (arg)) {
	*lloc = scm_cons (SCM_CAR (arg), SCM_EOL);
	lloc = SCM_CDRLOC (*lloc);
	arg = SCM_CDR (arg);
      }
      SCM_VALIDATE_NULL_OR_NIL (SCM_ARGn, arg);
      arg = SCM_CAR (args);
      args = SCM_CDR (args);
    };
    *lloc = arg;
    return res;
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_append_x, "append!", 0, 0, 1, 
            (SCM lists),
	    "A destructive version of @code{append} (@pxref{Pairs and\n"
	    "Lists,,,r5rs, The Revised^5 Report on Scheme}).  The cdr field\n"
	    "of each list's final pair is changed to point to the head of\n"
	    "the next list, so no consing is performed.  Return a pointer to\n"
	    "the mutated list.")
#define FUNC_NAME s_scm_append_x
{
  SCM_VALIDATE_REST_ARGUMENT (lists);
  while (1) {
    if (SCM_NULLP (lists)) {
      return SCM_EOL;
    } else {
      SCM arg = SCM_CAR (lists);
      lists = SCM_CDR (lists);
      if (SCM_NULLP (lists)) {
	return arg;
      } else if (!SCM_NULL_OR_NIL_P (arg)) {
	SCM_VALIDATE_CONS (SCM_ARG1, arg);
	SCM_SETCDR (scm_last_pair (arg), scm_append_x (lists));
	return arg;
      }
    }
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_last_pair, "last-pair", 1, 0, 0, 
           (SCM lst),
	    "Return a pointer to the last pair in @var{lst}, signalling an error if\n"
	    "@var{lst} is circular.")
#define FUNC_NAME s_scm_last_pair
{
  SCM tortoise = lst;
  SCM hare = lst;

  if (SCM_NULL_OR_NIL_P (lst))
    return lst;

  SCM_VALIDATE_CONS (SCM_ARG1, lst);
  do {
    SCM ahead = SCM_CDR(hare);
    if (SCM_NCONSP(ahead)) return hare;
    hare = ahead;
    ahead = SCM_CDR(hare);
    if (SCM_NCONSP(ahead)) return hare;
    hare = ahead;
    tortoise = SCM_CDR(tortoise);
  }
  while (! SCM_EQ_P (hare, tortoise));
  SCM_MISC_ERROR ("Circular structure in position 1: ~S", scm_list_1 (lst));
}
#undef FUNC_NAME


/* reversing lists */

SCM_DEFINE (scm_reverse, "reverse", 1, 0, 0,
            (SCM lst),
	    "Return a new list that contains the elements of @var{lst} but\n"
	    "in reverse order.")
#define FUNC_NAME s_scm_reverse
{
  SCM result = SCM_EOL;
  SCM tortoise = lst;
  SCM hare = lst;

  do {
      if (SCM_NULL_OR_NIL_P(hare)) return result;
      SCM_ASSERT(SCM_CONSP(hare), lst, 1, FUNC_NAME);
      result = scm_cons (SCM_CAR (hare), result);
      hare = SCM_CDR (hare);
      if (SCM_NULL_OR_NIL_P(hare)) return result;
      SCM_ASSERT(SCM_CONSP(hare), lst, 1, FUNC_NAME);
      result = scm_cons (SCM_CAR (hare), result);
      hare = SCM_CDR (hare);
      tortoise = SCM_CDR (tortoise);
    }
  while (! SCM_EQ_P (hare, tortoise));
  SCM_MISC_ERROR ("Circular structure in position 1: ~S", scm_list_1 (lst));
}
#undef FUNC_NAME

SCM_DEFINE (scm_reverse_x, "reverse!", 1, 1, 0,
            (SCM lst, SCM new_tail),
	    "A destructive version of @code{reverse} (@pxref{Pairs and Lists,,,r5rs,\n"
	    "The Revised^5 Report on Scheme}).  The cdr of each cell in @var{lst} is\n"
	    "modified to point to the previous list element.  Return a pointer to the\n"
	    "head of the reversed list.\n\n"
	    "Caveat: because the list is modified in place, the tail of the original\n"
	    "list now becomes its head, and the head of the original list now becomes\n"
	    "the tail.  Therefore, the @var{lst} symbol to which the head of the\n"
	    "original list was bound now points to the tail.  To ensure that the head\n"
	    "of the modified list is not lost, it is wise to save the return value of\n"
	    "@code{reverse!}")
#define FUNC_NAME s_scm_reverse_x
{
  SCM_VALIDATE_LIST (1, lst);
  if (SCM_UNBNDP (new_tail))
    new_tail = SCM_EOL;
  else
    SCM_VALIDATE_LIST (2, new_tail);

  while (!SCM_NULL_OR_NIL_P (lst))
    {
      SCM old_tail = SCM_CDR (lst);
      SCM_SETCDR (lst, new_tail);
      new_tail = lst;
      lst = old_tail;
    }
  return new_tail;
}
#undef FUNC_NAME



/* indexing lists by element number */

SCM_DEFINE (scm_list_ref, "list-ref", 2, 0, 0,
	    (SCM list, SCM k),
	    "Return the @var{k}th element from @var{list}.")
#define FUNC_NAME s_scm_list_ref
{
  SCM lst = list;
  unsigned long int i;
  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  while (SCM_CONSP (lst)) {
    if (i == 0)
      return SCM_CAR (lst);
    else {
      --i;
      lst = SCM_CDR (lst);
    }
  };
  if (SCM_NULL_OR_NIL_P (lst))
    SCM_OUT_OF_RANGE (2, k);
  else
    SCM_WRONG_TYPE_ARG (1, list);
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_set_x, "list-set!", 3, 0, 0,
	    (SCM list, SCM k, SCM val),
	    "Set the @var{k}th element of @var{list} to @var{val}.")
#define FUNC_NAME s_scm_list_set_x
{
  SCM lst = list;
  unsigned long int i;
  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  while (SCM_CONSP (lst)) {
    if (i == 0) {
      SCM_SETCAR (lst, val);
      return val;
    } else {
      --i;
      lst = SCM_CDR (lst);
    }
  };
  if (SCM_NULL_OR_NIL_P (lst))
    SCM_OUT_OF_RANGE (2, k);
  else
    SCM_WRONG_TYPE_ARG (1, list);
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_cdr_ref, "list-cdr-ref", 2, 0, 0, scm_list_tail);

SCM_DEFINE (scm_list_tail, "list-tail", 2, 0, 0,
           (SCM lst, SCM k),
	    "@deffnx {Scheme Procedure} list-cdr-ref lst k\n"
	    "Return the \"tail\" of @var{lst} beginning with its @var{k}th element.\n"
	    "The first element of the list is considered to be element 0.\n\n"
	    "@code{list-tail} and @code{list-cdr-ref} are identical.  It may help to\n"
	    "think of @code{list-cdr-ref} as accessing the @var{k}th cdr of the list,\n"
	    "or returning the results of cdring @var{k} times down @var{lst}.")
#define FUNC_NAME s_scm_list_tail
{
  register long i;
  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  while (i-- > 0) {
    SCM_VALIDATE_CONS (1,lst);
    lst = SCM_CDR(lst);
  }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_cdr_set_x, "list-cdr-set!", 3, 0, 0,
           (SCM list, SCM k, SCM val),
	    "Set the @var{k}th cdr of @var{list} to @var{val}.")
#define FUNC_NAME s_scm_list_cdr_set_x
{
  SCM lst = list;
  unsigned long int i;
  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  while (SCM_CONSP (lst)) {
    if (i == 0) {
      SCM_SETCDR (lst, val);
      return val;
    } else {
      --i;
      lst = SCM_CDR (lst);
    }
  };
  if (SCM_NULL_OR_NIL_P (lst))
    SCM_OUT_OF_RANGE (2, k);
  else
    SCM_WRONG_TYPE_ARG (1, list);
}
#undef FUNC_NAME



/* copying lists, perhaps partially */

SCM_DEFINE (scm_list_head, "list-head", 2, 0, 0,
           (SCM lst, SCM k),
	    "Copy the first @var{k} elements from @var{lst} into a new list, and\n"
	    "return it.")
#define FUNC_NAME s_scm_list_head
{
  SCM answer;
  SCM * pos;
  register long i;

  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  answer = SCM_EOL;
  pos = &answer;
  while (i-- > 0)
    {
      SCM_VALIDATE_CONS (1,lst);
      *pos = scm_cons (SCM_CAR (lst), SCM_EOL);
      pos = SCM_CDRLOC (*pos);
      lst = SCM_CDR(lst);
    }
  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_copy, "list-copy", 1, 0, 0, 
            (SCM lst),
	    "Return a (newly-created) copy of @var{lst}.")
#define FUNC_NAME s_scm_list_copy
{
  SCM newlst;
  SCM * fill_here;
  SCM from_here;

  SCM_VALIDATE_LIST (1, lst);

  newlst = SCM_EOL;
  fill_here = &newlst;
  from_here = lst;

  while (SCM_CONSP (from_here))
    {
      SCM c;
      c = scm_cons (SCM_CAR (from_here), SCM_CDR (from_here));
      *fill_here = c;
      fill_here = SCM_CDRLOC (c);
      from_here = SCM_CDR (from_here);
    }
  return newlst;
}
#undef FUNC_NAME


/* membership tests (memq, memv, etc.) */ 

/* The function scm_c_memq returns the first sublist of list whose car is
 * 'eq?' obj, where the sublists of list are the non-empty lists returned by
 * (list-tail list k) for k less than the length of list.  If obj does not
 * occur in list, then #f (not the empty list) is returned.
 * List must be a proper list, otherwise scm_c_memq may crash or loop
 * endlessly.
 */
SCM
scm_c_memq (SCM obj, SCM list)
{
  for (; !SCM_NULL_OR_NIL_P (list); list = SCM_CDR (list))
    {
      if (SCM_EQ_P (SCM_CAR (list), obj))
	return list;
    }
  return SCM_BOOL_F;
}


SCM_DEFINE (scm_memq, "memq", 2, 0, 0,
           (SCM x, SCM lst),
	    "Return the first sublist of @var{lst} whose car is @code{eq?}\n"
	    "to @var{x} where the sublists of @var{lst} are the non-empty\n"
	    "lists returned by @code{(list-tail @var{lst} @var{k})} for\n"
	    "@var{k} less than the length of @var{lst}.  If @var{x} does not\n"
	    "occur in @var{lst}, then @code{#f} (not the empty list) is\n"
	    "returned.")
#define FUNC_NAME s_scm_memq
{
  SCM_VALIDATE_LIST (2, lst);
  return scm_c_memq (x, lst);
}
#undef FUNC_NAME


SCM_DEFINE (scm_memv, "memv", 2, 0, 0,
           (SCM x, SCM lst),
	    "Return the first sublist of @var{lst} whose car is @code{eqv?}\n"
	    "to @var{x} where the sublists of @var{lst} are the non-empty\n"
	    "lists returned by @code{(list-tail @var{lst} @var{k})} for\n"
	    "@var{k} less than the length of @var{lst}.  If @var{x} does not\n"
	    "occur in @var{lst}, then @code{#f} (not the empty list) is\n"
	    "returned.")
#define FUNC_NAME s_scm_memv
{
  SCM_VALIDATE_LIST (2, lst);
  for (; !SCM_NULL_OR_NIL_P (lst); lst = SCM_CDR (lst))
    {
      if (! SCM_FALSEP (scm_eqv_p (SCM_CAR (lst), x)))
	return lst;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_member, "member", 2, 0, 0,
           (SCM x, SCM lst),
	    "Return the first sublist of @var{lst} whose car is\n"
	    "@code{equal?} to @var{x} where the sublists of @var{lst} are\n"
	    "the non-empty lists returned by @code{(list-tail @var{lst}\n"
	    "@var{k})} for @var{k} less than the length of @var{lst}.  If\n"
	    "@var{x} does not occur in @var{lst}, then @code{#f} (not the\n"
	    "empty list) is returned.")
#define FUNC_NAME s_scm_member
{
  SCM_VALIDATE_LIST (2, lst);
  for (; !SCM_NULL_OR_NIL_P (lst); lst = SCM_CDR (lst))
    {
      if (! SCM_FALSEP (scm_equal_p (SCM_CAR (lst), x)))
	return lst;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


/* deleting elements from a list (delq, etc.) */

SCM_DEFINE (scm_delq_x, "delq!", 2, 0, 0,
           (SCM item, SCM lst),
	    "@deffnx {Scheme Procedure} delv! item lst\n"
	    "@deffnx {Scheme Procedure} delete! item lst\n"
	    "These procedures are destructive versions of @code{delq}, @code{delv}\n"
	    "and @code{delete}: they modify the pointers in the existing @var{lst}\n"
	    "rather than creating a new list.  Caveat evaluator: Like other\n"
	    "destructive list functions, these functions cannot modify the binding of\n"
	    "@var{lst}, and so cannot be used to delete the first element of\n"
	    "@var{lst} destructively.")
#define FUNC_NAME s_scm_delq_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_EQ_P (SCM_CAR (walk), item))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delv_x, "delv!", 2, 0, 0,
	    (SCM item, SCM lst),
	    "Destructively remove all elements from @var{lst} that are\n"
	    "@code{eqv?} to @var{item}.")
#define FUNC_NAME s_scm_delv_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (! SCM_FALSEP (scm_eqv_p (SCM_CAR (walk), item)))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME



SCM_DEFINE (scm_delete_x, "delete!", 2, 0, 0,
	    (SCM item, SCM lst),
	    "Destructively remove all elements from @var{lst} that are\n"
	    "@code{equal?} to @var{item}.")
#define FUNC_NAME s_scm_delete_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (! SCM_FALSEP (scm_equal_p (SCM_CAR (walk), item)))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }

  return lst;
}
#undef FUNC_NAME





SCM_DEFINE (scm_delq, "delq", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements\n"
	    "@code{eq?} to @var{item} removed.  This procedure mirrors\n"
	    "@code{memq}: @code{delq} compares elements of @var{lst} against\n"
	    "@var{item} with @code{eq?}.")
#define FUNC_NAME s_scm_delq
{
  SCM copy = scm_list_copy (lst);
  return scm_delq_x (item, copy);
}
#undef FUNC_NAME

SCM_DEFINE (scm_delv, "delv", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements\n"
	    "@code{eqv?}  to @var{item} removed.  This procedure mirrors\n"
	    "@code{memv}: @code{delv} compares elements of @var{lst} against\n"
	    "@var{item} with @code{eqv?}.")
#define FUNC_NAME s_scm_delv
{
  SCM copy = scm_list_copy (lst);
  return scm_delv_x (item, copy);
}
#undef FUNC_NAME

SCM_DEFINE (scm_delete, "delete", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements\n"
	    "@code{equal?}  to @var{item} removed.  This procedure mirrors\n"
	    "@code{member}: @code{delete} compares elements of @var{lst}\n"
	    "against @var{item} with @code{equal?}.")
#define FUNC_NAME s_scm_delete
{
  SCM copy = scm_list_copy (lst);
  return scm_delete_x (item, copy);
}
#undef FUNC_NAME


SCM_DEFINE (scm_delq1_x, "delq1!", 2, 0, 0,
           (SCM item, SCM lst),
	    "Like @code{delq!}, but only deletes the first occurrence of\n"
	    "@var{item} from @var{lst}.  Tests for equality using\n"
	    "@code{eq?}.  See also @code{delv1!} and @code{delete1!}.")
#define FUNC_NAME s_scm_delq1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_EQ_P (SCM_CAR (walk), item))
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delv1_x, "delv1!", 2, 0, 0,
            (SCM item, SCM lst),
	    "Like @code{delv!}, but only deletes the first occurrence of\n"
	    "@var{item} from @var{lst}.  Tests for equality using\n"
	    "@code{eqv?}.  See also @code{delq1!} and @code{delete1!}.")
#define FUNC_NAME s_scm_delv1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (! SCM_FALSEP (scm_eqv_p (SCM_CAR (walk), item)))
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delete1_x, "delete1!", 2, 0, 0,
            (SCM item, SCM lst),
	    "Like @code{delete!}, but only deletes the first occurrence of\n"
	    "@var{item} from @var{lst}.  Tests for equality using\n"
	    "@code{equal?}.  See also @code{delq1!} and @code{delv1!}.")
#define FUNC_NAME s_scm_delete1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (! SCM_FALSEP (scm_equal_p (SCM_CAR (walk), item)))
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }

  return lst;
}
#undef FUNC_NAME



void
scm_init_list ()
{
#include "libguile/list.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
