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



SCM_PROC(s_acons, "acons", 3, 0, 0, scm_acons);
#ifdef __STDC__
SCM 
scm_acons (SCM w, SCM x, SCM y)
#else
SCM 
scm_acons (w, x, y)
     SCM w;
     SCM x;
     SCM y;
#endif
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_CAR (z) = w;
  SCM_CDR (z) = x;
  x = z;
  SCM_NEWCELL (z);
  SCM_CAR (z) = x;
  SCM_CDR (z) = y;
  return z;
}



SCM_PROC (s_sloppy_assq, "sloppy-assq", 2, 0, 0, scm_sloppy_assq);
#ifdef __STDC__
SCM
scm_sloppy_assq(SCM x, SCM alist)
#else
SCM
scm_sloppy_assq(x, alist)
     SCM x;
     SCM alist;
#endif
{
  SCM tmp;
  for(;SCM_NIMP(alist);alist = SCM_CDR(alist))
    {
      if (SCM_CONSP(alist))
	{
	  tmp = SCM_CAR(alist);
	  if (SCM_NIMP (tmp) && SCM_CONSP (tmp) && (SCM_CAR (tmp)==x))
	    return tmp;
	}
    }
  return SCM_BOOL_F;
}



SCM_PROC (s_sloppy_assv, "sloppy-assv", 2, 0, 0, scm_sloppy_assv);
#ifdef __STDC__
SCM
scm_sloppy_assv(SCM x, SCM alist)
#else
SCM
scm_sloppy_assv(x, alist)
     SCM x;
     SCM alist;
#endif
{
  SCM tmp;
  for(;SCM_NIMP(alist);alist = SCM_CDR(alist))
    {
      if (SCM_CONSP(alist))
	{
	  tmp = SCM_CAR(alist);
	  if (   SCM_NIMP (tmp)
	      && SCM_CONSP (tmp)
	      && SCM_NFALSEP (scm_eqv_p (SCM_CAR (tmp), x)))
	    return tmp;
	}
    }
  return SCM_BOOL_F;
}


SCM_PROC (s_sloppy_assoc, "sloppy-assoc", 2, 0, 0, scm_sloppy_assoc);
#ifdef __STDC__
SCM
scm_sloppy_assoc(SCM x, SCM alist)
#else
SCM
scm_sloppy_assoc(x, alist)
     SCM x;
     SCM alist;
#endif
{
  SCM tmp;
  for(;SCM_NIMP(alist);alist = SCM_CDR(alist))
    {
      if (SCM_CONSP(alist))
	{
	  tmp = SCM_CAR(alist);
	  if (   SCM_NIMP (tmp)
	      && SCM_CONSP (tmp)
	      && SCM_NFALSEP (scm_equal_p (SCM_CAR (tmp), x)))
	    return tmp;
	}
    }
  return SCM_BOOL_F;
}




SCM_PROC(s_assq, "assq", 2, 0, 0, scm_assq);
#ifdef __STDC__
SCM
scm_assq(SCM x, SCM alist)
#else
SCM
scm_assq(x, alist)
     SCM x;
     SCM alist;
#endif
{
	SCM tmp;
	for(;SCM_NIMP(alist);alist = SCM_CDR(alist)) {
		SCM_ASSERT(SCM_CONSP(alist), alist, SCM_ARG2, s_assq);
		tmp = SCM_CAR(alist);
		SCM_ASSERT(SCM_NIMP(tmp) && SCM_CONSP(tmp), alist, SCM_ARG2, s_assq);
		if (SCM_CAR(tmp)==x) return tmp;
	}
	SCM_ASSERT(SCM_NULLP(alist), alist, SCM_ARG2, s_assq);
	return SCM_BOOL_F;
}


SCM_PROC(s_assv, "assv", 2, 0, 0, scm_assv);
#ifdef __STDC__
SCM
scm_assv(SCM x, SCM alist)
#else
SCM
scm_assv(x, alist)
     SCM x;
     SCM alist;
#endif
{
  SCM tmp;
  for(;SCM_NIMP(alist);alist = SCM_CDR(alist)) {
    SCM_ASRTGO(SCM_CONSP(alist), badlst);
    tmp = SCM_CAR(alist);
    SCM_ASRTGO(SCM_NIMP(tmp) && SCM_CONSP(tmp), badlst);
    if SCM_NFALSEP(scm_eqv_p(SCM_CAR(tmp), x)) return tmp;
  }
# ifndef RECKLESS
  if (!(SCM_NULLP(alist)))
    badlst: scm_wta(alist, (char *)SCM_ARG2, s_assv);
# endif
  return SCM_BOOL_F;
}


SCM_PROC(s_assoc, "assoc", 2, 0, 0, scm_assoc);
#ifdef __STDC__
SCM
scm_assoc(SCM x, SCM alist)
#else
SCM
scm_assoc(x, alist)
     SCM x;
     SCM alist;
#endif
{
	SCM tmp;
	for(;SCM_NIMP(alist);alist = SCM_CDR(alist)) {
		SCM_ASSERT(SCM_CONSP(alist), alist, SCM_ARG2, s_assoc);
		tmp = SCM_CAR(alist);
		SCM_ASSERT(SCM_NIMP(tmp) && SCM_CONSP(tmp), alist, SCM_ARG2, s_assoc);
		if SCM_NFALSEP(scm_equal_p(SCM_CAR(tmp), x)) return tmp;
	}
	SCM_ASSERT(SCM_NULLP(alist), alist, SCM_ARG2, s_assoc);
	return SCM_BOOL_F;
}




SCM_PROC (s_assq_ref, "assq-ref", 2, 0, 0, scm_assq_ref);
#ifdef __STDC__
SCM
scm_assq_ref (SCM alist, SCM key)
#else
SCM
scm_assq_ref (alist, key)
     SCM alist;
     SCM key;
#endif
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}


SCM_PROC (s_assv_ref, "assv-ref", 2, 0, 0, scm_assv_ref);
#ifdef __STDC__
SCM
scm_assv_ref (SCM alist, SCM key)
#else
SCM
scm_assv_ref (alist, key)
     SCM alist;
     SCM key;
#endif
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}


SCM_PROC (s_assoc_ref, "assoc-ref", 2, 0, 0, scm_assoc_ref);
#ifdef __STDC__
SCM
scm_assoc_ref (SCM alist, SCM key)
#else
SCM
scm_assoc_ref (alist, key)
     SCM alist;
     SCM key;
#endif
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      return SCM_CDR (handle);
    }
  return SCM_BOOL_F;
}






SCM_PROC (s_assq_set_x, "assq-set!", 3, 0, 0, scm_assq_set_x);
#ifdef __STDC__
SCM
scm_assq_set_x (SCM alist, SCM key, SCM val)
#else
SCM
scm_assq_set_x (alist, key, val)
     SCM alist;
     SCM key;
     SCM val;
#endif
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}

SCM_PROC (s_assv_set_x, "assv-set!", 3, 0, 0, scm_assv_set_x);
#ifdef __STDC__
SCM
scm_assv_set_x (SCM alist, SCM key, SCM val)
#else
SCM
scm_assv_set_x (alist, key, val)
     SCM alist;
     SCM key;
     SCM val;
#endif
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}

SCM_PROC (s_assoc_set_x, "assoc-set!", 3, 0, 0, scm_assoc_set_x);
#ifdef __STDC__
SCM
scm_assoc_set_x (SCM alist, SCM key, SCM val)
#else
SCM
scm_assoc_set_x (alist, key, val)
     SCM alist;
     SCM key;
     SCM val;
#endif
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      SCM_SETCDR (handle, val);
      return alist;
    }
  else
    return scm_acons (key, val, alist);
}




SCM_PROC (s_assq_remove_x, "assq-remove!", 2, 0, 0, scm_assq_remove_x);
#ifdef __STDC__
SCM
scm_assq_remove_x (SCM alist, SCM key)
#else
SCM
scm_assq_remove_x (alist, key)
     SCM alist;
     SCM key;
#endif
{
  SCM handle;

  handle = scm_sloppy_assq (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      return scm_delq_x (handle, alist);
    }
  else
    return alist;
}


SCM_PROC (s_assv_remove_x, "assv-remove!", 2, 0, 0, scm_assv_remove_x);
#ifdef __STDC__
SCM
scm_assv_remove_x (SCM alist, SCM key)
#else
SCM
scm_assv_remove_x (alist, key)
     SCM alist;
     SCM key;
#endif
{
  SCM handle;

  handle = scm_sloppy_assv (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      return scm_delv_x (handle, alist);
    }
  else
    return alist;
}


SCM_PROC (s_assoc_remove_x, "assoc-remove!", 2, 0, 0, scm_assoc_remove_x);
#ifdef __STDC__
SCM
scm_assoc_remove_x (SCM alist, SCM key)
#else
SCM
scm_assoc_remove_x (alist, key)
     SCM alist;
     SCM key;
#endif
{
  SCM handle;

  handle = scm_sloppy_assoc (key, alist);
  if (SCM_NIMP (handle) && SCM_CONSP (handle))
    {
      return scm_delete_x (handle, alist);
    }
  else
    return alist;
}





#ifdef __STDC__
void
scm_init_alist (void)
#else
void
scm_init_alist ()
#endif
{
#include "alist.x"
}

