#ifndef SCM_SRFI_14_H
#define SCM_SRFI_14_H
/* srfi-14.c --- SRFI-14 procedures for Guile
 *
 * 	Copyright (C) 2001 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives
 * permission for additional uses of the text contained in its release
 * of GUILE.
 *
 * The exception is that, if you link the GUILE library with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public
 * License.  Your use of that executable is in no way restricted on
 * account of linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public
 * License.
 *
 * This exception applies only to the code released by the Free
 * Software Foundation under the name GUILE.  If you copy code from
 * other Free Software Foundation releases into a copy of GUILE, as
 * the General Public License permits, the exception does not apply to
 * the code that you add in this way.  To avoid misleading anyone as
 * to the status of such modified files, you must delete this
 * exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#define SCM_CHARSET_SIZE 256

#define SCM_CHARSET_GET(cs, idx) (((long *) SCM_SMOB_DATA (cs))\
                     [(idx) / sizeof (long)] & (1 << ((idx) % sizeof (long))))

#define SCM_CHARSETP(x) (!SCM_IMP (x) && (SCM_TYP16 (x) == scm_tc16_charset))

/* Smob type code for character sets.  */
extern int scm_tc16_charset;

void scm_init_srfi_14 (void);

#endif /* SCM_SRFI_14_H */
