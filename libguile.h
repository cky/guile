#ifndef LIBGUILEH
#define LIBGUILEH

/*	Copyright (C) 1995, 1996, 1997, 1998, 2000, 2001 Free Software Foundation, Inc.
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
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
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
 * If you do not wish that, delete this exception notice.  */


#ifdef __cplusplus
extern "C" {
#endif

#include "libguile/__scm.h"

/* These files define typedefs used by later files, so they need to
   come first.  */
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/pairs.h"

#include "libguile/alist.h"
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
#include "libguile/dynl.h"
#include "libguile/dynwind.h"
#include "libguile/eq.h"
#include "libguile/error.h"
#include "libguile/eval.h"
#include "libguile/evalext.h"
#include "libguile/feature.h"
#include "libguile/filesys.h"
#include "libguile/fluids.h"
#include "libguile/fports.h"
#include "libguile/gc.h"
#include "libguile/gdbint.h"
#include "libguile/goops.h"
#include "libguile/gsubr.h"
#include "libguile/guardians.h"
#include "libguile/hash.h"
#include "libguile/hashtab.h"
#include "libguile/init.h"
#include "libguile/ioext.h"
#include "libguile/rdelim.h"
#include "libguile/rw.h"
#include "libguile/keywords.h"
#include "libguile/list.h"
#include "libguile/load.h"
#include "libguile/macros.h"
#include "libguile/mallocs.h"
#include "libguile/modules.h"
#include "libguile/net_db.h"
#include "libguile/numbers.h"
#include "libguile/objects.h"
#include "libguile/objprop.h"
#include "libguile/options.h"
#include "libguile/ports.h"
#include "libguile/posix.h"
#include "libguile/procprop.h"
#include "libguile/properties.h"
#include "libguile/procs.h"
#include "libguile/ramap.h"
#include "libguile/random.h"
#include "libguile/read.h"
#include "libguile/root.h"
#include "libguile/scmsigs.h"
#include "libguile/script.h"
#include "libguile/simpos.h"
#include "libguile/snarf.h"
#include "libguile/socket.h"
#include "libguile/sort.h"
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
#include "libguile/validate.h"
#include "libguile/values.h"
#include "libguile/variable.h"
#include "libguile/vectors.h"
#include "libguile/version.h"
#include "libguile/vports.h"
#include "libguile/weaks.h"
#ifdef USE_THREADS
#include "libguile/threads.h"
#endif

/* Deprecated type names.  Don't use them for new code. */

#if SCM_DEBUG_DEPRECATED == 0

typedef scm_t_array_dim scm_array_dim_t;
typedef scm_t_array scm_array_t;
typedef scm_t_bits scm_bits_t;
typedef scm_t_c_bvec_limb scm_c_bvec_limb_t;
typedef scm_t_c_hook_entry scm_c_hook_entry_t;
typedef scm_t_c_hook_function scm_c_hook_function_t;
typedef scm_t_c_hook scm_c_hook_t;
typedef scm_t_catch_body scm_catch_body_t;
typedef scm_t_catch_handler scm_catch_handler_t;
typedef scm_t_complex scm_complex_t;
typedef scm_t_cond scm_cond_t;
typedef scm_t_contregs scm_contregs_t;
typedef scm_t_debug_frame scm_debug_frame_t;
typedef scm_t_debug_info scm_debug_info_t;
typedef scm_t_double scm_double_t;
typedef scm_t_fport scm_fport_t;
typedef scm_t_guard scm_guard_t;
typedef scm_t_i_rstate scm_i_rstate_t;
typedef scm_t_info_frame scm_info_frame_t;
typedef scm_t_inner scm_inner_t;
typedef scm_t_key scm_key_t;
typedef scm_t_method scm_method_t;
typedef scm_t_mutex scm_mutex_t;
typedef scm_t_option scm_option_t;
typedef scm_t_port_rw_active scm_port_rw_active_t;
typedef scm_t_port scm_port_t;
typedef scm_t_ptob_descriptor scm_ptob_descriptor_t;
typedef scm_t_rng scm_rng_t;
typedef scm_t_rstate scm_rstate_t;
typedef scm_t_signed_bits scm_signed_bits_t;
typedef scm_t_srcprops_chunk scm_srcprops_chunk_t;
typedef scm_t_srcprops scm_srcprops_t;
typedef scm_t_stack scm_stack_t;
typedef scm_t_struct_free scm_struct_free_t;
typedef scm_t_subr_entry scm_subr_entry_t;

#endif /* !SCM_DEBUG_DEPRECATED */

#ifdef __cplusplus
}
#endif



#endif  /* LIBGUILEH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
