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




#include "libguile/__scm.h"

/* These files define typedefs used by later files, so they need to
   come first.  */
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/pairs.h"

#include "libguile/alist.h"
#include "libguile/append.h"
#include "libguile/arbiters.h"
#include "libguile/async.h"
#include "libguile/boolean.h"
#include "libguile/chars.h"
#include "libguile/continuations.h"
#ifdef DEBUG_EXTENSIONS
#include "libguile/backtrace.h"
#include "libguile/debug.h"
#include "libguile/stacks.h"
#endif
#include "libguile/dynwind.h"
#include "libguile/eq.h"
#include "libguile/error.h"
#include "libguile/eval.h"
#include "libguile/extchrs.h"
#include "libguile/feature.h"
#include "libguile/filesys.h"
#include "libguile/fports.h"
#include "libguile/gc.h"
#include "libguile/gdbint.h"
#include "libguile/genio.h"
#include "libguile/gsubr.h"
#include "libguile/hash.h"
#include "libguile/hashtab.h"
#include "libguile/init.h"
#include "libguile/ioext.h"
#include "libguile/kw.h"
#include "libguile/libpath.h"
#include "libguile/list.h"
#include "libguile/load.h"
#include "libguile/mallocs.h"
#include "libguile/markers.h"
#include "libguile/mbstrings.h"
#include "libguile/numbers.h"
#include "libguile/objprop.h"
#include "libguile/options.h"
#include "libguile/ports.h"
#include "libguile/posix.h"
#include "libguile/procprop.h"
#include "libguile/procs.h"
#include "libguile/ramap.h"
#include "libguile/read.h"
#include "libguile/root.h"
#include "libguile/scmsigs.h"
#include "libguile/sequences.h"
#include "libguile/simpos.h"
#include "libguile/snarf.h"
#include "libguile/socket.h"
#include "libguile/srcprop.h"
#include "libguile/stackchk.h"
#include "libguile/stime.h"
#include "libguile/strings.h"
#include "libguile/strop.h"
#include "libguile/strorder.h"
#include "libguile/strports.h"
#include "libguile/struct.h"
#include "libguile/symbols.h"
#include "libguile/tag.h"
#include "libguile/tags.h"
#include "libguile/throw.h"
#include "libguile/unif.h"
#include "libguile/variable.h"
#include "libguile/vectors.h"
#include "libguile/version.h"
#include "libguile/vports.h"
#include "libguile/weaks.h"
#ifdef USE_THREADS
#include "libguile/../threads/threads.h"
#endif



#endif  /* LIBGUILEH */
