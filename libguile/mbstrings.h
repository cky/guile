/* classes: h_files */

#ifndef MBSTRINGSH
#define MBSTRINGSH
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


#include <libguile/__scm.h>
#include "symbols.h"


#define SCM_MB_STRINGP(x) (   (SCM_TYP7(x)==scm_tc7_mb_string) \
			   || (   (SCM_TYP7(x) == scm_tc7_msymbol) \
			       && (SCM_SYMBOL_MULTI_BYTE_SCM_STRINGP (x) != SCM_BOOL_F)))
#define SCM_REGULAR_STRINGP(x) (SCM_TYP7D(x)==scm_tc7_string)
				    



#ifdef __STDC__
extern SCM scm_multi_byte_string_p (SCM obj);
extern SCM scm_regular_string_p (SCM obj);
extern SCM scm_multi_byte_string (SCM chrs);
extern int scm_mb_ilength (unsigned char * data, int size);
extern SCM scm_multi_byte_string_length (SCM str);
extern SCM scm_symbol_multi_byte_p (SCM symbol);
extern SCM scm_set_symbol_multi_byte_x (SCM symbol, SCM val);
extern SCM scm_regular_port_p (SCM p);
extern SCM scm_regular_port_x (SCM p);
extern SCM scm_multi_byte_port_p (SCM p);
extern SCM scm_multi_byte_port_x (SCM p);
extern SCM scm_wide_character_port_p (SCM p);
extern SCM scm_wide_character_port_x (SCM p);
extern void scm_put_wchar (int c, SCM port, int writing);
extern void scm_print_mb_string (SCM exp, SCM port, int writing);
extern void scm_print_mb_symbol (SCM exp, SCM port);
extern void scm_init_mbstrings (void);

#else /* STDC */
extern SCM scm_multi_byte_string_p ();
extern SCM scm_regular_string_p ();
extern SCM scm_multi_byte_string ();
extern int scm_mb_ilength ();
extern SCM scm_multi_byte_string_length ();
extern SCM scm_symbol_multi_byte_p ();
extern SCM scm_set_symbol_multi_byte_x ();
extern SCM scm_regular_port_p ();
extern SCM scm_regular_port_x ();
extern SCM scm_multi_byte_port_p ();
extern SCM scm_multi_byte_port_x ();
extern SCM scm_wide_character_port_p ();
extern SCM scm_wide_character_port_x ();
extern void scm_put_wchar ();
extern void scm_print_mb_string ();
extern void scm_print_mb_symbol ();
extern void scm_init_mbstrings ();

#endif /* STDC */


#endif  /* MBSTRINGSH */
