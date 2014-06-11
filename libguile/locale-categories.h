/* Copyright (C) 2006, 2008, 2014 Free Software Foundation, Inc.
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

/* A list of all available locale categories, not including `ALL'.  */


/* The six standard categories, as defined in IEEE Std 1003.1-2001.  */
SCM_DEFINE_LOCALE_CATEGORY (COLLATE)
SCM_DEFINE_LOCALE_CATEGORY (CTYPE)

#if defined(LC_MESSAGES) && !(defined(LC_MAX) && LC_MESSAGES > LC_MAX)
/* MinGW doesn't have `LC_MESSAGES'.  libintl.h might define
   `LC_MESSAGES' for MinGW to an arbitrary large value which we cannot
   use in a call to `setlocale'.  */
SCM_DEFINE_LOCALE_CATEGORY (MESSAGES)
#endif

SCM_DEFINE_LOCALE_CATEGORY (MONETARY)
SCM_DEFINE_LOCALE_CATEGORY (NUMERIC)
SCM_DEFINE_LOCALE_CATEGORY (TIME)

/* Additional non-standard categories.  */
#ifdef LC_PAPER
SCM_DEFINE_LOCALE_CATEGORY (PAPER)
#endif
#ifdef LC_NAME
SCM_DEFINE_LOCALE_CATEGORY (NAME)
#endif
#ifdef LC_ADDRESS
SCM_DEFINE_LOCALE_CATEGORY (ADDRESS)
#endif
#ifdef LC_TELEPHONE
SCM_DEFINE_LOCALE_CATEGORY (TELEPHONE)
#endif
#ifdef LC_MEASUREMENT
SCM_DEFINE_LOCALE_CATEGORY (MEASUREMENT)
#endif
#ifdef LC_IDENTIFICATION
SCM_DEFINE_LOCALE_CATEGORY (IDENTIFICATION)
#endif
