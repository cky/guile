/* classes: h_files */

#ifndef GSCMH
#define GSCMH

/*	Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
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


#include "libguile.h"


/* {Locking Out Async Execution (including async GC) and Non-Local Exits}
 */

#define GSCM_DEFER_INTS			SCM_DEFER_INTS
#define GSCM_ALLOW_INTS			SCM_ALLOW_INTS


/* {Common Constants}
 */

#define GSCM_EOL			SCM_EOL
#define GSCM_FALSE			SCM_BOOL_F
#define GSCM_TRUE			SCM_BOOL_T

#define GSCM_EOL_MARKER 		SCM_UNDEFINED
#define GSCM_NOT_PASSED	 		SCM_UNDEFINED
#define GSCM_UNSPECIFIED 		SCM_UNSPECIFIED


/* {Booleans} 
 */

#define gscm_bool(CBOOL)	((CBOOL) ? SCM_BOOL_T : SCM_BOOL_F)
#define gscm_2_bool(BOOL)	(((BOOL) == SCM_BOOL_F) ? 0 : 1)


/* {Numbers}
 */

#define gscm_ulong 		scm_ulong2num
#define gscm_long 		scm_long2num
#define gscm_double(X)		scm_makdbl ((X), 0.0)

#define gscm_2_ulong(OBJ)	scm_num2ulong((OBJ), (char *)SCM_ARG1, "gscm_2_ulong")
#define gscm_2_long(OBJ)	scm_num2long((OBJ), (char *)SCM_ARG1, "gscm_2_long")
#define gscm_2_double(OBJ)	scm_num2dbl((OBJ), "gscm_2_double")


/* {Characters}
 */

#define gscm_char(C)		SCM_MAKICHR(C)
/* extern int gscm_2_char 		P((SCM)); */


/* {Strings}
 */

#define gscm_str(SRC, LEN)	scm_makfromstr (SRC, LEN, 0)
#define gscm_str0		scm_makfrom0str



/* {Pairs and Lists} 
 */

#define gscm_cons 			scm_cons
#define gscm_list 			scm_listify
#define gscm_ilength			scm_ilength


#define gscm_set_car(OBJ, VAL) \
   ((SCM_NIMP(OBJ) && SCM_CONSP(OBJ)) \
		   ? (SCM_CAR(OBJ) = VAL) \
		   : scm_wta ((OBJ), (char *)SCM_ARG1, "set-car!"))

#define gscm_set_cdr(OBJ, VAL) \
   ((SCM_NIMP(OBJ) && SCM_CONSP(OBJ)) \
		   ? (SCM_CDR(OBJ) = VAL) \
		   : scm_wta ((OBJ), (char *)SCM_ARG1, "set-cdr!"))


#define GSCM_SAFE_CAR(X)   ((SCM_NIMP(X) && SCM_CONSP(X)) \
		   ? SCM_CAR(X) \
		   : scm_wta ((X), (char *)SCM_ARG1, "car"))

#define GSCM_SAFE_CDR(X)   ((SCM_NIMP(X) && SCM_CONSP(X)) \
		   ? SCM_CDR(X) \
		   : scm_wta ((X), (char *)SCM_ARG1, "cdr"))

#define gscm_car(OBJ)		GSCM_SAFE_CAR (OBJ)
#define gscm_cdr(OBJ)		GSCM_SAFE_CDR (OBJ)

#define gscm_caar(OBJ)		GSCM_SAFE_CAR (GSCM_SAFE_CAR (OBJ))
#define gscm_cdar(OBJ)		GSCM_SAFE_CDR (GSCM_SAFE_CAR (OBJ))
#define gscm_cadr(OBJ)		GSCM_SAFE_CAR (GSCM_SAFE_CDR (OBJ))
#define gscm_cddr(OBJ)		GSCM_SAFE_CDR (GSCM_SAFE_CDR (OBJ))

#define gscm_caaar(OBJ)		GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (OBJ)))
#define gscm_cdaar(OBJ)		GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (OBJ)))
#define gscm_cadar(OBJ)		GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (OBJ)))
#define gscm_cddar(OBJ)		GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (OBJ)))
#define gscm_caadr(OBJ)		GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (OBJ)))
#define gscm_cdadr(OBJ)		GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (OBJ)))
#define gscm_caddr(OBJ)		GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (OBJ)))
#define gscm_cdddr(OBJ)		GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (OBJ)))

#define gscm_caaaar(OBJ)	GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (OBJ))))
#define gscm_cdaaar(OBJ)	GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (OBJ))))
#define gscm_cadaar(OBJ)	GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (OBJ))))
#define gscm_cddaar(OBJ)	GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (OBJ))))
#define gscm_caadar(OBJ)	GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (OBJ))))
#define gscm_cdadar(OBJ)	GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (OBJ))))
#define gscm_caddar(OBJ)	GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (OBJ))))
#define gscm_cdddar(OBJ)	GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (OBJ))))
#define gscm_caaadr(OBJ)	GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (OBJ))))
#define gscm_cdaadr(OBJ)	GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (OBJ))))
#define gscm_cadadr(OBJ)	GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (OBJ))))
#define gscm_cddadr(OBJ)	GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (OBJ))))
#define gscm_caaddr(OBJ)	GSCM_SAFE_CAR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (OBJ))))
#define gscm_cdaddr(OBJ)	GSCM_SAFE_CDR (GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (OBJ))))
#define gscm_cadddr(OBJ)	GSCM_SAFE_CAR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (OBJ))))
#define gscm_cddddr(OBJ)	GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (GSCM_SAFE_CDR (OBJ))))


/* {Symbols}
 */

#define gscm_symbol(STR, LEN)		SCM_CAR(scm_intern (STR, LEN))
#define gscm_tmp_symbol(STR, LEN)	SCM_CAR(scm_intern_obarray (STR, LEN, SCM_BOOL_F))


/* {Vectors}
 */

#define gscm_vector(N, FILL)		scm_make_vector (SCM_MAKINUM(N), (FILL), SCM_UNDEFINED)
#define gscm_vref(V, I)			scm_vector_ref ((V), SCM_MAKINUM(I))
#define gscm_vset(V, I, VAL)		scm_vector_set_x ((V), SCM_MAKINUM(I), (VAL))


/* {Procedures}
 */

/* extern SCM gscm_make_subr P((SCM (*fn)(), int req, int opt, int varp, char * doc)); */
/* extern SCM gscm_curry P((SCM procedure, SCM first_arg)); */

#define gscm_apply(PROC, ARGS)		scm_apply ((PROC), (ARGS), SCM_EOL)



/* {Non-local Exits}
 */


#define gscm_catch(T, TH, H)		scm_catch ((T), (TH), (H))
#define gscm_throw(T, V)		scm_throw ((T), (V))
#define gscm_dynamic_wind(E, T, L)	scm_dynwind ((E), (T), (L))
/* extern void gscm_error P((char * message, SCM args)); */


/* {I/O}
 */

#define gscm_print_obj			scm_prin1
#define gscm_putc			scm_putc
#define gscm_puts			scm_puts
#define gscm_fwrite			scm_fwrite
#define gscm_flush			scm_flush

extern char * gscm_last_attempted_init_file;

/* {Equivalence}
 */


#define gscm_is_eq(OBJ)		(SCM_BOOL_F != scm_eq (OBJ))
#define gscm_is_eqv(OBJ)	(SCM_BOOL_F != scm_eqv (OBJ))
#define gscm_is_equal(OBJ)	(SCM_BOOL_F != scm_equal_p (OBJ))


/* {Procedure Properties}
 */

#define gscm_procedure_properties 		scm_procedure_properties
#define gscm_set_procedure_properties_x 	scm_set_procedure_properties_x
#define gscm_procedure_property 		scm_procedure_property
#define gscm_set_procedure_property_x 		scm_set_procedure_property_x


/* {Generic Length Procedure}
 */

#define gscm_obj_length				scm_obj_length


/* {Proc Declaration Macro}
 */
#ifndef GSCM_MAGIC_SNARFER
#define GSCM_PROC(RANAME, CFN, STR, REQ, OPT, VAR)  \
	static char RANAME[]=STR;
#else
#define GSCM_PROC(RANAME, CFN, STR, REQ, OPT, VAR)  \
%%% gscm_define_procedure (RANAME, CFN, REQ, OPT, VAR, "")
#endif

#define gscm_define_procedure(NAME, FN, REQ, OPT, VARP, DOC)	scm_make_gsubr(name, req, opt, varp, fn)
#define gscm_curry 						scm_curry
#define gscm_define						scm_sysintern


typedef int GSCM_top_level;


/* {Error Returns}
 */

typedef int GSCM_status;

#define GSCM_OK 			0
#define GSCM_ERROR 			1
#define GSCM_ILLEGALLY_REENTERED 	2
#define GSCM_OUT_OF_MEM 		3
#define GSCM_ERROR_OPENING_FILE		4
#define GSCM_ERROR_OPENING_INIT_FILE	5



extern GSCM_status gscm_seval_str SCM_P ((SCM *answer, GSCM_top_level toplvl, char * str));
extern GSCM_status gscm_seval_file SCM_P ((SCM *answer, GSCM_top_level toplvl, char * file_name));
extern GSCM_status gscm_eval_str SCM_P ((char ** answer, GSCM_top_level toplvl, char * str));
extern GSCM_status gscm_eval_file SCM_P ((char ** answer, GSCM_top_level toplvl, char * file_name));
extern GSCM_status gscm_run_scm SCM_P ((int argc, char ** argv, FILE * in, FILE * out, FILE * err, GSCM_status (*initfn)(void), char * initfile, char * initcmd));
extern char * gscm_error_msg SCM_P ((int n));
extern SCM gscm_make_subr SCM_P ((SCM (*fn)(), int req, int opt, int varp, char * doc));
extern int gscm_2_char SCM_P ((SCM c));
extern void gscm_2_str SCM_P ((char ** out, int * len_out, SCM * objp));
extern void gscm_error SCM_P ((char * message, SCM args));
extern void scm_init_guile SCM_P ((void));

#endif  /* GSCMH */

