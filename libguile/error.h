/* classes: h_files */

#ifndef ERRORH
#define ERRORH
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

#include "libguile/__scm.h"


extern int scm_ints_disabled;
extern SCM scm_system_error;
extern SCM scm_num_overflow;



extern void scm_error SCM_P ((SCM key, char *subr, char *message, SCM args, SCM rest));
extern void (*scm_error_callback) SCM_P ((SCM key, char *subr, char *message, SCM args, SCM rest));

#ifdef __STDC__
extern int scm_handle_it (int i);
extern void scm_warn (char *str1, char *str2);
extern SCM scm_errno (SCM arg);
extern SCM scm_perror (SCM arg);
extern void scm_def_err_response (void);
extern void scm_everr (SCM exp, SCM env, SCM arg, char *pos, char *s_subr);
extern SCM scm_wta (SCM arg, char *pos, char *s_subr);
extern void scm_init_error (void);

#else /* STDC */
extern int scm_handle_it ();
extern void scm_warn ();
extern SCM scm_errno ();
extern SCM scm_perror ();
extern void scm_def_err_response ();
extern void scm_everr ();
extern SCM scm_wta ();
extern void scm_init_error ();

#endif /* STDC */

#endif  /* ERRORH */
