#ifndef LIBGUILEH
#define LIBGUILEH

/*	Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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




#include "__scm.h"

#ifdef STDC_HEADERS
# include <stdlib.h>
# ifdef AMIGA
#  include <stddef.h>
# endif /* def AMIGA */
# define scm_sizet size_t
#else
# ifdef _SIZE_T
#  define scm_sizet size_t
# else
#  define scm_sizet unsigned int
# endif /* def _SIZE_T */
#endif /* def STDC_HEADERS */

#include "smob.h"



#include "alist.h"
#include "append.h"
#include "arbiters.h"
#include "async.h"
#include "boolean.h"
#include "chars.h"
#include "continuations.h"
#include "dynwind.h"
#include "eq.h"
#include "error.h"
#include "eval.h"
#include "extchrs.h"
#include "fdsocket.h"
#include "feature.h"
#include "files.h"
#include "filesys.h"
#include "fports.h"
#include "gc.h"
#include "genio.h"
#include "gsubr.h"
#include "hash.h"
#include "hashtab.h"
#include "init.h"
#include "ioext.h"
#include "kw.h"
#include "libguile.h"
#include "list.h"
#include "load.h"
#include "mallocs.h"
#include "markers.h"
#include "marksweep.h"
#include "mbstrings.h"
#include "numbers.h"
#include "objprop.h"
#include "pairs.h"
#include "params.h"
#include "ports.h"
#include "posix.h"
#include "print.h"
#include "procprop.h"
#include "procs.h"
#include "ramap.h"
#include "read.h"
#include "root.h"
#include "scmsigs.h"
#include "sequences.h"
#include "simpos.h"
#include "socket.h"
#include "stackchk.h"
#include "stime.h"
#include "strings.h"
#include "strop.h"
#include "strorder.h"
#include "strports.h"
#include "struct.h"
#include "symbols.h"
#include "tag.h"
#include "tags.h"
#include "throw.h"
#include "unif.h"
#include "variable.h"
#include "vectors.h"
#include "vports.h"
#include "weaks.h"




#ifdef __STDC__

#else /* STDC */

#endif /* STDC */


#endif  /* LIBGUILEH */
