/* classes: h_files */

#ifndef EXTCHRSH
#define EXTCHRSH
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


#include <stdlib.h>

#define FAKE_EXT_SCM_CHARS 1

#if !defined(FAKE_EXT_SCM_CHARS)

#define xmblen mblen
#define xwctomb wctomb
#define xmbtowc mbtowc
#define XMB_CUR_MAX MB_CUR_MAX
typedef wchar_t xwchar_t;

#else

typedef unsigned short xwchar_t;
#define XMB_CUR_MAX 4

#endif



#ifdef __STDC__
extern int xmblen (const char * str, size_t size);
extern int xwctomb (char * _str, int c);
extern int xmbtowc (xwchar_t * result, const unsigned char * _str, size_t size);

#else /* STDC */
extern int xmblen ();
extern int xwctomb ();
extern int xmbtowc ();

#endif /* STDC */




#endif  /* EXTCHRSH */
