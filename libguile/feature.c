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

#ifdef HAVE_STRING_H
#include <string.h>
#endif


SCM *scm_loc_features;

#ifdef __STDC__
void
scm_add_feature(char* str)
#else
void
scm_add_feature(str)
     char* str;
#endif
{
  *scm_loc_features = scm_cons(SCM_CAR(scm_intern(str, strlen(str))), *scm_loc_features);
}



/* {Help finding slib}
 */


SCM_PROC(s_compiled_library_path, "compiled-library-path", 0, 0, 0, scm_compiled_library_path);
#ifdef __STDC__
SCM
scm_compiled_library_path (void)
#else
SCM
scm_compiled_library_path ()
#endif
{
#ifndef LIBRARY_PATH
  return SCM_BOOL_F;
#else
  return scm_makfrom0str (LIBRARY_PATH);
#endif
}




SCM_PROC(s_program_arguments, "program-arguments", 0, 0, 0, scm_program_arguments);
#ifdef __STDC__
SCM 
scm_program_arguments (void)
#else
SCM 
scm_program_arguments ()
#endif
{
  return scm_progargs;
}






#ifdef __STDC__
void
scm_init_feature(void)
#else
void
scm_init_feature()
#endif
{
  scm_loc_features = &SCM_CDR(scm_sysintern("*features*", SCM_EOL));
#ifdef RECKLESS
  scm_add_feature("reckless");
#endif
#ifndef _Windows
  scm_add_feature("system");
#endif
#ifdef vms
  scm_add_feature(s_ed);
#endif
#ifdef SICP
  scm_add_feature("sicp");
#endif
#ifndef GO32
  scm_add_feature("char-ready?");
#endif
#ifndef CHEAP_CONTINUATIONS
  scm_add_feature ("full-continuation");
#endif


  scm_sysintern ("char-code-limit", SCM_MAKINUM (SCM_CHAR_SCM_CODE_LIMIT));
#include "feature.x"
}
