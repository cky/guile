/* classes: h_files */

/* Macros for snarfing initialization actions from C source. */

#ifndef LIBGUILE_SNARF_H
#define LIBGUILE_SNARF_H

/*	Copyright (C) 1995,1996,1997,1998, 1999 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#ifndef SCM_MAGIC_SNARFER

#define GUILE_PROC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST
#define GUILE_PROC1(FNAME, PRIMNAME, TYPE, ARGLIST, DOCSTRING) \
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST

#define SCM_REGISTER_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
	static const char RANAME[]=STR
#define SCM_GPROC(RANAME, STR, REQ, OPT, VAR, CFN, GF)  \
	static const char RANAME[]=STR; \
	static SCM GF = 0
#define SCM_PROC1(RANAME, STR, TYPE, CFN)  \
	static const char RANAME[]=STR
#define SCM_GPROC1(RANAME, STR, TYPE, CFN, GF) \
	static const char RANAME[]=STR; \
	static SCM GF = 0
#else
#if defined(__cplusplus) || defined(GUILE_CPLUSPLUS_SNARF)

#define GUILE_PROC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
%%%     scm_make_gsubr (s_ ## FNAME, REQ, OPT, VAR, (SCM (*)(...)) FNAME); \
$$$ primname #ARGLIST req opt var @@@ docstring @!!!

#define GUILE_PROC1(FNAME, PRIMNAME, TYPE, ARGLIST, DOCSTRING) \
%%%     scm_make_subr (s_ ## FNAME, TYPE, FNAME); \
$$1 primname #ARGLIST type @@@ docstring @!!!

#define SCM_REGISTER_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
%%%	scm_make_gsubr (RANAME, REQ, OPT, VAR, (SCM (*)(...)) CFN) \
$$R RANAMEprimname #ARGLIST type @@@ docstring @!!!

#define SCM_GPROC(RANAME, STR, REQ, OPT, VAR, CFN, GF)  \
%%%	scm_make_gsubr_with_generic (RANAME, REQ, OPT, VAR, (SCM (*)(...))CFN, &GF)
#define SCM_PROC1(RANAME, STR, TYPE, CFN)  \
%%%	scm_make_subr(RANAME, TYPE, (SCM (*)(...))CFN)
#define SCM_GPROC1(RANAME, STR, TYPE, CFN, GF)  \
%%%	scm_make_subr_with_generic(RANAME, TYPE, (SCM (*)(...))CFN, &GF)
#else

#define GUILE_PROC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
%%%     scm_make_gsubr (s_ ## FNAME, REQ, OPT, VAR, (SCM (*)()) FNAME);
#define GUILE_PROC1(FNAME, PRIMNAME, TYPE, ARGLIST, DOCSTRING) \
%%%     scm_make_subr (s_ ## FNAME, TYPE, FNAME);


#define SCM_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
%%%	scm_make_gsubr (RANAME, REQ, OPT, VAR, (SCM (*)()) CFN)
#define SCM_REGISTER_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
%%%	scm_make_gsubr (RANAME, REQ, OPT, VAR, (SCM (*)()) CFN)
#define SCM_GPROC(RANAME, STR, REQ, OPT, VAR, CFN, GF)  \
%%%	scm_make_gsubr_with_generic (RANAME, REQ, OPT, VAR, (SCM (*)()) CFN, &GF)
#define SCM_PROC1(RANAME, STR, TYPE, CFN)  \
%%%	scm_make_subr(RANAME, TYPE, CFN)
#define SCM_GPROC1(RANAME, STR, TYPE, CFN, GF)  \
%%%	scm_make_subr_with_generic(RANAME, TYPE, CFN, &GF)
#endif
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_SYNTAX(RANAME, STR, TYPE, CFN)  \
	static const char RANAME[]=STR
#else
#define SCM_SYNTAX(RANAME, STR, TYPE, CFN)  \
%%%	scm_make_synt (RANAME, TYPE, CFN)
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_SYMBOL(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_SYMBOL(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (SCM_CAR (scm_intern0 (SCHEME_NAME)))
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_GLOBAL_SYMBOL(c_name, scheme_name) \
	SCM c_name = SCM_BOOL_F
#else
#define SCM_GLOBAL_SYMBOL(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (SCM_CAR (scm_intern0 (SCHEME_NAME)))
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_KEYWORD(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_KEYWORD(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (scm_c_make_keyword (SCHEME_NAME))
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_GLOBAL_KEYWORD(c_name, scheme_name) \
	SCM c_name = SCM_BOOL_F
#else
#define SCM_GLOBAL_KEYWORD(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (scm_c_make_keyword (SCHEME_NAME))
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_VCELL(c_name, scheme_name) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_VCELL(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (scm_intern0 (SCHEME_NAME)); SCM_SETCDR (C_NAME, SCM_BOOL_F)
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_GLOBAL_VCELL(c_name, scheme_name) \
	SCM c_name = SCM_BOOL_F
#else
#define SCM_GLOBAL_VCELL(C_NAME, SCHEME_NAME) \
%%%	C_NAME = scm_permanent_object (scm_intern0 (SCHEME_NAME)); SCM_SETCDR (C_NAME, SCM_BOOL_F)
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_VCELL_INIT(c_name, scheme_name, init_val) \
	static SCM c_name = SCM_BOOL_F
#else
#define SCM_VCELL_INIT(C_NAME, SCHEME_NAME, init_val) \
%%%	C_NAME = scm_permanent_object (scm_intern0 (SCHEME_NAME)); SCM_SETCDR (C_NAME, init_val)
#endif

#ifndef SCM_MAGIC_SNARFER
#define SCM_GLOBAL_VCELL_INIT(c_name, scheme_name, init_val) \
	SCM c_name = SCM_BOOL_F
#else
#define SCM_GLOBAL_VCELL_INIT(C_NAME, SCHEME_NAME, init_val) \
%%%	C_NAME = scm_permanent_object (scm_intern0 (SCHEME_NAME)); SCM_SETCDR (C_NAME, init_val)
#endif

#define SCM_CONST_LONG(C_NAME, SCHEME_NAME,VALUE) SCM_VCELL_INIT(C_NAME, SCHEME_NAME, scm_long2num(VALUE))

#endif /* LIBGUILE_SNARF_H */
