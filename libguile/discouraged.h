/* This file contains definitions for discouraged features.  When you
   discourage something, move it here when that is feasible.

   A discouraged feature is one that shouldn't be used in new code
   since we have a better alternative now.  However, there is nothing
   wrong with using the old feature, so it is OK to continue to use
   it.

   Eventually, discouraged features can be deprecated since removing
   them will make Guile simpler.
*/

#ifndef SCM_DISCOURAGED_H
#define SCM_DISCOURAGED_H

/* Copyright (C) 2004, 2006, 2010 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include "libguile/__scm.h"

#if SCM_ENABLE_DISCOURAGED == 1

/* Discouraged because they do not follow the naming convention.  That
   is, they end in "P" but return a C boolean.  Also, SCM_BOOLP
   evaluates its argument twice.
*/

#define SCM_FALSEP		scm_is_false
#define SCM_NFALSEP		scm_is_true
#define SCM_BOOLP               scm_is_bool
#define SCM_EQ_P                scm_is_eq


/* Convert from a C boolean to a SCM boolean value */
#define SCM_BOOL		scm_from_bool

/* Convert from a C boolean to a SCM boolean value and negate it */
#define SCM_NEGATE_BOOL(f)	scm_from_bool(!(f))

/* SCM_BOOL_NOT returns the other boolean.  
 * The order of ^s here is important for Borland C++ (!?!?!)
 */
#define SCM_BOOL_NOT(x)		(SCM_PACK (SCM_UNPACK (x) \
					   ^ (SCM_UNPACK (SCM_BOOL_T) \
					      ^ SCM_UNPACK (SCM_BOOL_F))))

/* Discouraged because scm_is_symbol has a better name,
 */
#define SCM_SYMBOLP scm_is_symbol

/* Discouraged because the alternatives have the better names.
 */
#define SCM_SYMBOL_FUNC	            scm_symbol_fref
#define SCM_SET_SYMBOL_FUNC         scm_symbol_fset_x
#define SCM_SYMBOL_PROPS	    scm_symbol_pref
#define SCM_SET_SYMBOL_PROPS        scm_symbol_pset_x

/* Discouraged because there are better ways.
 */
#define SCM_SYMBOL_HASH             scm_i_symbol_hash
#define SCM_SYMBOL_INTERNED_P(X)    scm_i_symbol_is_interned

/* Discouraged because they evaluated their arguments twice and/or
   don't fit the naming scheme.
*/

#define SCM_CONSP(x)            (scm_is_pair (x))
#define SCM_NCONSP(x)           (!SCM_CONSP (x))
#define SCM_NULLP(x)		(scm_is_null (x))
#define SCM_NNULLP(x)		(!scm_is_null (x))

/* Discouraged because the 'internal' and 'thread' moniker is
   confusing.
 */

#define scm_internal_select scm_std_select
#define scm_thread_sleep    scm_std_sleep
#define scm_thread_usleep   scm_std_usleep

#endif /* SCM_ENABLE_DISCOURAGED == 1 */

#endif /* SCM_DISCOURAGED_H */
