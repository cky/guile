/* This was modified to try out compiling with Guile. */


/*   scmhob.h is a header file for scheme source compiled with hobbit4d
     Copyright (C) 1992, 1993, 1994, 1995 Tanel Tammet 

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


#include <stdio.h>
#include <ctype.h>
#include "_scm.h"



#define abrt 			scm_abort
#define absval 			scm_abs
#define angle			scm_angle
#define append			scm_append
#define assoc			scm_assoc
#define assq			scm_assq
#define assv			scm_assv
#define big2dbl			scm_big2dbl
#define close_port		scm_close_port
#define cons			scm_cons
#define cur_input_port		scm_current_input_port
#define cur_output_port		scm_current_output_port
#define difference		scm_difference
#define display			scm_display
#define divide			scm_divide
#define eof_objectp		scm_eof_object_p
#define eqp			scm_eq_p
#define equal			scm_equal_p
#define eqv			scm_eqv_p
#define evenp			scm_even_p
#define exactp			scm_exact_p
#define greaterp		scm_gr_p
#define greqp			scm_geq_p
#define imag_part		scm_imag_part
#define in2ex			scm_inexact_to_exact
#define inexactp		scm_inexact_p
#define input_portp		scm_input_port_p
#define intp			scm_int_p
#define length			scm_length
#define leqp			scm_leq_p
#define lessp			scm_less_p
#define lgcd			scm_gcd
#define list_ref		scm_list_ref
#define list_tail		scm_list_tail
#define listp			scm_list_p
#define llcm			scm_lcm
#define lmax			scm_max
#define lmin			scm_min
#define lquotient		scm_quotient
#define lread(X)		scm_read((X), SCM_UNDEFINED)
#define lremainder		scm_remainder
#define lwrite			scm_write
#define magnitude		scm_magnitude
#define makcclo			scm_makcclo
#define makdbl			scm_makdbl
#define make_string		scm_make_string
#define make_vector		scm_make_vector
#define makpolar		scm_make_polar
#define makrect			scm_make_rectangular
#define member			scm_member
#define memq			scm_memq
#define memv			scm_memv
#define modulo			scm_modulo
#define my_time			scm_get_internal_run_time
#define negativep		scm_negative_p
#define newline			scm_newline
#define number2string		scm_number_to_string
#define oddp			scm_odd_p
#define open_file		scm_open_file
#define output_portp		scm_output_port_p
#define peek_char		scm_peek_char
#define positivep		scm_positive_p
#define procedurep		scm_procedure_p
#define product			scm_product
#define quit			scm_quit
#define read_char		scm_read_char
#define real_part		scm_real_part
#define realp			scm_real_p
#define reverse			scm_reverse
#define set_inp			scm_set_current_input_port
#define set_outp		scm_set_current_output_port
#define st_append		scm_string_append
#define st_equal		scm_string_equal_p
#define st_leqp			scm_string_leq_p
#define st_lessp		scm_string_less_p
#define st_set			scm_string_set_x
#define stci_equal		scm_string_ci_equal_p
#define stci_leqp		scm_string_ci_leq_p
#define stci_lessp		scm_string_ci_less_p
#define string			scm_string
#define string2list		scm_string_to_list
#define string2number		scm_string_to_number
#define string2symbol		scm_string_to_symbol
#define string_copy		scm_string_copy
#define string_fill		scm_string_fill_x
#define substring		scm_substring
#define sum			scm_sum
#define symbol2string		scm_symbol_to_string
#define vector			scm_vector
#define vector2list		scm_vector_to_list
#define vector_ref		scm_vector_ref
#define vector_set		scm_vector_set_x
#define write_char		scm_write_char
#define zerop			scm_zero_p
			


#define STBL_VECTOR_SET(v,k,o) (v[((long)SCM_INUM(k))] = o)
#define STBL_VECTOR_REF(v,k) (v[((long)SCM_INUM(k))])
#define CHAR_LESSP(x,y) ((SCM_ICHR(x) < SCM_ICHR(y)) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHAR_LEQP(x,y) ((SCM_ICHR(x) <= SCM_ICHR(y)) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHCI_EQ(x,y) ((upcase[SCM_ICHR(x)]==upcase[SCM_ICHR(y)]) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHCI_LESSP(x,y) ((upcase[SCM_ICHR(x)] < upcase[SCM_ICHR(y)]) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHCI_LEQP(x,y) ((upcase[SCM_ICHR(x)] <= upcase[SCM_ICHR(y)]) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHAR_ALPHAP(chr) ((isascii(SCM_ICHR(chr)) && isalpha(SCM_ICHR(chr))) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHAR_SCM_NUMP(chr) ((isascii(SCM_ICHR(chr)) && isdigit(SCM_ICHR(chr))) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHAR_WHITEP(chr) ((isascii(SCM_ICHR(chr)) && isspace(SCM_ICHR(chr))) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHAR_UPPERP(chr) ((isascii(SCM_ICHR(chr)) && isupper(SCM_ICHR(chr))) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHAR_LOWERP(chr) ((isascii(SCM_ICHR(chr)) && islower(SCM_ICHR(chr))) ? SCM_BOOL_T : SCM_BOOL_F)
#define CHAR2INT(chr) SCM_MAKINUM(SCM_ICHR(chr))
#define INT2CHAR(n) SCM_MAKICHR(SCM_INUM(n))
#define CHAR_UPCASE(chr) SCM_MAKICHR(upcase[SCM_ICHR(chr)])
#define CHAR_DOWNCASE(chr) SCM_MAKICHR(downcase[SCM_ICHR(chr)])
#define ST_SCM_LENGTH(str) SCM_MAKINUM(SCM_LENGTH(str))
#define ST_REF(str,k) SCM_MAKICHR(SCM_CHARS(str)[SCM_INUM(k)])
#define VECTOR_SCM_LENGTH(v)  SCM_MAKINUM(SCM_LENGTH(v))

#ifdef SCM_FLOATS
#include <math.h>
#endif
#ifdef SCM_BIGDIG
#define PRE_TRANSC_FUN(x) (SCM_INUMP(x) ? (double) SCM_INUM(x) : (SCM_REALP(x) ? (double) SCM_REALPART(x) : (double) big2dbl(x)))
#else
#define PRE_TRANSC_FUN(x) (SCM_INUMP(x) ?  (double) SCM_INUM(x) : (double) SCM_REALPART(x))
#endif

#define SIN_FUN(x) (makdbl( sin( PRE_TRANSC_FUN(x)), 0.0))
#define COS_FUN(x) (makdbl( cos( PRE_TRANSC_FUN(x)), 0.0))
#define TAN_FUN(x) (makdbl( tan( PRE_TRANSC_FUN(x)), 0.0))
#define ASIN_FUN(x) (makdbl( asin( PRE_TRANSC_FUN(x)), 0.0))
#define ACOS_FUN(x) (makdbl( acos( PRE_TRANSC_FUN(x)), 0.0))
#define ATAN_FUN(x) (makdbl( atan( PRE_TRANSC_FUN(x)), 0.0))
#define SINH_FUN(x) (makdbl( sinh( PRE_TRANSC_FUN(x)), 0.0))
#define COSH_FUN(x) (makdbl( cosh( PRE_TRANSC_FUN(x)), 0.0))
#define TANH_FUN(x) (makdbl( tanh( PRE_TRANSC_FUN(x)), 0.0))
#define ASINH_FUN(x) (makdbl( asinh( PRE_TRANSC_FUN(x)), 0.0))
#define ACOSH_FUN(x) (makdbl( acosh( PRE_TRANSC_FUN(x)), 0.0))
#define ATANH_FUN(x) (makdbl( atanh( PRE_TRANSC_FUN(x)), 0.0))
#define SQRT_FUN(x) (makdbl( sqrt( PRE_TRANSC_FUN(x)), 0.0))
#define EXPT_FUN(x,y) (makdbl( pow(( PRE_TRANSC_FUN(x)), ( PRE_TRANSC_FUN(y))), 0.0))
#define EXP_FUN(x) (makdbl( exp( PRE_TRANSC_FUN(x)), 0.0))
#define LOG_FUN(x) (makdbl( log( PRE_TRANSC_FUN(x)), 0.0))
#define ABS_FUN(x) (makdbl( fabs( PRE_TRANSC_FUN(x)), 0.0))
#define EX2IN_FUN(x) (makdbl( PRE_TRANSC_FUN(x), 0.0))
#define SCM_FLOOR_FUN(x) (makdbl( floor( PRE_TRANSC_FUN(x)), 0.0))
#define CEILING_FUN(x) (makdbl( ceil( PRE_TRANSC_FUN(x)), 0.0))
#define TRUNCATE_FUN(x) (makdbl( ltrunc( PRE_TRANSC_FUN(x)), 0.0))
#define ROUND_FUN(x) (makdbl(round( PRE_TRANSC_FUN(x)), 0.0))

/* the following defs come from the #ifdef HOBBIT part of scm.h */

#define SBOOL(x) ((x) ? SCM_BOOL_T : SCM_BOOL_F)

#define BOOLEAN_P(x) ((x)==SCM_BOOL_T || (x)==SCM_BOOL_F)
#define CHAR_P SCM_ICHRP
#define SYMBOL_P(x) (SCM_ISYMP(x) || (!(SCM_IMP(x)) && SCM_SYMBOLP(x))) 
#define VECTOR_P(x) (!(SCM_IMP(x)) && SCM_VECTORP(x)) 
#define PAIR_P(x) (!(SCM_IMP(x)) && SCM_CONSP(x))     
#define NUMBER_P SCM_INUMP
#define INTEGER_P SCM_INUMP
#define STRING_P(x) (!(SCM_IMP(x)) && SCM_STRINGP(x))
#define NULL_P SCM_NULLP
#define ZERO_P(x) ((x)==SCM_INUM0)
#define POSITIVE_P(x) ((x) > SCM_INUM0)
#define NEGATIVE_P(x) ((x) < SCM_INUM0)

#define NOT(x) ((x)==SCM_BOOL_F ? SCM_BOOL_T : SCM_BOOL_F)
#define SET_CAR(x,y) (CAR(x) = (SCM)(y))
#define SET_CDR(x,y) (CDR(x) = (SCM)(y))
#define VECTOR_SET(v,k,o) (SCM_VELTS(v)[((long)SCM_INUM(k))] = o)
#define VECTOR_REF(v,k) (SCM_VELTS(v)[((long)SCM_INUM(k))])
#define CL_VECTOR_SET(v,k,o) (SCM_VELTS(v)[k] = o)
#define CL_VECTOR_REF(v,k) (SCM_VELTS(v)[k])
#define GLOBAL(x) (*(x))

#define append2(lst1,lst2) (append(scm_cons2(lst1,lst2,SCM_EOL)))
#define procedure_pred_(x) (SCM_BOOL_T==procedurep(x))
