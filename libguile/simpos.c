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

#include "scmsigs.h"
#include "simpos.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


extern int system();


#ifndef _Windows
SCM_PROC(s_system, "system", 1, 0, 0, scm_system);
#ifdef __STDC__
SCM
scm_system(SCM cmd)
#else
SCM
scm_system(cmd)
     SCM cmd;
#endif
{
  SCM_ASSERT(SCM_NIMP(cmd) && SCM_ROSTRINGP(cmd), cmd, SCM_ARG1, s_system);
  if (SCM_ROSTRINGP (cmd))
    cmd = scm_makfromstr (SCM_ROCHARS (cmd), SCM_ROLENGTH (cmd), 0);
  scm_ignore_signals();
# ifdef AZTEC_C
  cmd = SCM_MAKINUM(Execute(SCM_ROCHARS(cmd), 0, 0));
# else
  cmd = SCM_MAKINUM(0L+system(SCM_ROCHARS(cmd)));
# endif
  scm_unignore_signals();
  return cmd;
}
#endif

extern char *getenv();
SCM_PROC (s_sys_getenv, "getenv", 1, 0, 0, scm_sys_getenv);
#ifdef __STDC__
SCM
scm_sys_getenv(SCM nam)
#else
SCM
scm_sys_getenv(nam)
     SCM nam;
#endif
{
  char *val;
  SCM_ASSERT(SCM_NIMP(nam) && SCM_ROSTRINGP(nam), nam, SCM_ARG1, s_sys_getenv);
  if (SCM_ROSTRINGP (nam))
    nam = scm_makfromstr (SCM_ROCHARS (nam), SCM_ROLENGTH (nam), 0);
  val = getenv(SCM_CHARS(nam));
  if (!val)
    scm_syserror (s_sys_getenv);
  return scm_makfromstr(val, (scm_sizet)strlen(val), 0);
}

#ifdef vms
# define SYSTNAME "VMS"
#endif
#ifdef unix
# define SYSTNAME "UNIX"
#endif
#ifdef MWC
# define SYSTNAME "COHERENT"
#endif
#ifdef _Windows
# define SYSTNAME "WINDOWS"
#else
# ifdef MSDOS
#  define SYSTNAME "MS-DOS"
# endif
#endif
#ifdef __EMX__
# define SYSTNAME "OS/2"
#endif
#ifdef __IBMC__
# define SYSTNAME "OS/2"
#endif
#ifdef THINK_C
# define SYSTNAME "THINKC"
#endif
#ifdef AMIGA
# define SYSTNAME "AMIGA"
#endif
#ifdef atarist
# define SYSTNAME "ATARIST"
#endif
#ifdef mach
# define SYSTNAME "MACH"
#endif
#ifdef ARM_ULIB
# define SYSTNAME "ACORN"
#endif

SCM_PROC(s_software_type, "software-type", 0, 0, 0, scm_software_type);
#ifdef __STDC__
SCM
scm_software_type(void)
#else
SCM
scm_software_type()
#endif
{
#ifdef nosve
  return SCM_CAR(scm_intern("nosve", 5));
#else
  return SCM_CAR(scm_intern(SYSTNAME, sizeof SYSTNAME/sizeof(char) -1));
#endif
}

#ifdef __STDC__
void
scm_init_simpos (void)
#else
void
scm_init_simpos ()
#endif
{
#include "simpos.x"
}

