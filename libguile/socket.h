/* classes: h_files */

#ifndef SOCKETH
#define SOCKETH
/*	Copyright (C) 1995 Free Software Foundation, Inc.
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





#ifdef __STDC__
extern SCM scm_sys_gethost (SCM name);
extern SCM scm_sys_inet_aton (SCM address);
extern SCM scm_inet_ntoa (SCM inetid);
extern SCM scm_inet_netof (SCM address);
extern SCM scm_lnaof (SCM address);
extern SCM scm_inet_makeaddr (SCM net, SCM lna);
extern SCM scm_sys_getnet (SCM name);
extern SCM scm_sys_getproto (SCM name);
extern SCM scm_sys_getserv (SCM name, SCM proto);
extern SCM scm_sethost (SCM arg);
extern SCM scm_setnet (SCM arg);
extern SCM scm_setproto (SCM arg);
extern SCM scm_setserv (SCM arg);
extern void scm_init_socket (void);

#else /* STDC */
extern SCM scm_sys_gethost ();
extern SCM scm_sys_inet_aton ();
extern SCM scm_inet_ntoa ();
extern SCM scm_inet_netof ();
extern SCM scm_lnaof ();
extern SCM scm_inet_makeaddr ();
extern SCM scm_sys_getnet ();
extern SCM scm_sys_getproto ();
extern SCM scm_sys_getserv ();
extern SCM scm_sethost ();
extern SCM scm_setnet ();
extern SCM scm_setproto ();
extern SCM scm_setserv ();
extern void scm_init_socket ();

#endif /* STDC */


#endif  /* SOCKETH */
