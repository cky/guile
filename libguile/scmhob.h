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
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA */


#include <libguile.h>

/* Specifique à hobbit */
#define GLOBAL(x) (*(x))
#define VECTOR_SET(v,k,o) (SCM_VELTS(v)[((long)SCM_INUM(k))] = o)
#define VECTOR_REF(v,k) (SCM_VELTS(v)[((long)SCM_INUM(k))])
#define VECTOR_LENGTH(v) SCM_MAKINUM(SCM_LENGTH(v))

/* Pourrait etre mis dans le noyau de hobbit */
/*#define vector_fill_excl_ scm_vector_fill_x*/

/* Passage scm --> guile */
#define EOL SCM_EOL
#define CAR SCM_CAR
#define CDR SCM_CDR
#define NFALSEP SCM_NFALSEP
#define BOOL_F SCM_BOOL_F
#define BOOL_T SCM_BOOL_T
#define MAKINUM SCM_MAKINUM
#define listofnull scm_listofnull
#define tc7_subr_1 scm_tc7_subr_1
#define tc7_subr_2 scm_tc7_subr_2
#define tc7_subr_3 scm_tc7_subr_3
#define tc7_lsubr scm_tc7_lsubr
#define apply scm_apply
#define cons scm_cons
#define divide scm_divide
#define eqp scm_eq_p
#define evenp scm_even_p
#define greaterp scm_gr_p
#define lessp scm_less_p
#define lremainder scm_remainder
#define makrect scm_make_rectangular
#define product scm_product
#define sum scm_sum
#define difference scm_difference
#define intern(x,y) scm_permanent_object(scm_intern((x),(y)))
#define make_subr scm_make_subr
#define make_vector(vec,fill) scm_make_vector((vec),(fill),SCM_BOOL_F)
#define absval scm_magnitude
#define string2number scm_string_to_number
#define makfromstr(x,y) scm_makfromstr((x),(y),0)
#define list_ref scm_list_ref

#define NULL_P SCM_NULLP
#define SBOOL(x) ((x) ? SCM_BOOL_T : SCM_BOOL_F)
#define INUM SCM_INUM
#define cur_output_port scm_current_output_port
#define display scm_display
#define newline scm_newline
#define makcclo scm_makcclo
#define UNSPECIFIED SCM_UNSPECIFIED
#define tc7_subr_0 scm_tc7_subr_0
#define equal scm_equal_p
#define assv scm_assv
#define PAIR_P(x) (!(SCM_IMP(x)) && SCM_CONSP(x))
#define SET_CDR SCM_SETCDR
#define eval scm_eval

#define close_port scm_close_port
#define lwrite scm_write
#define open_file scm_open_file
#define set_outp scm_set_current_output_port
#define MAKICHR SCM_MAKICHR
#define memq scm_memq

#define cur_input_port scm_current_input_port
#define set_inp scm_set_current_input_port
#define lread scm_read
#define eof_objectp scm_eof_object_p

#define modulo scm_modulo
#define CHAR2INT(chr) scm_ulong2num((unsigned long)SCM_ICHR(chr))
#define ST_LENGTH(str) SCM_MAKINUM (SCM_ROLENGTH (str))
#define ST_REF(str,k) SCM_MAKICHR (SCM_ROUCHARS (str)[SCM_INUM (k)])
#define NOT(x) ((x)==SCM_BOOL_F ? SCM_BOOL_T : SCM_BOOL_F)

#define SYMBOL_P(x) (!(SCM_IMP(x)) && SCM_SYMBOLP(x))
#define STRING_P(x) (!(SCM_IMP(x)) && SCM_STRINGP(x))
#define BOOLEAN_P(obj) ((SCM_BOOL_F==(obj)) || (SCM_BOOL_T==(obj)))
#if defined(SCM_FLOATS) || defined(SCM_BIGDIG)
#define NUMBER_P(x) ((SCM_INUMP(x)) || (SCM_NIMP(x) && SCM_NUMP(x)))
#else
#define NUMBER_P SCM_INUMP
#endif
#define number2string scm_number_to_string
#define symbol2string scm_symbol_to_string

#define append2(lst1,lst2) (scm_list_append(scm_cons2((lst1),(lst2),SCM_EOL)))
#define st_append scm_string_append

#define string scm_string
#define string2list scm_string_to_list
#define string2symbol scm_string_to_symbol
#define INTEGER_P(x) (scm_integer_p(x) == SCM_BOOL_T)

#define listp scm_list_p
#define assoc scm_assoc

#define assq scm_assq
#define memv scm_memv
#define member scm_member
#define append scm_list_append

#define CHAR_P SCM_ICHRP
#define CHAR_UPCASE(chr) SCM_MAKICHR(scm_upcase(SCM_ICHR(chr)))
#define CHAR_DOWNCASE(chr) SCM_MAKICHR(scm_downcase(SCM_ICHR(chr)))
#define procedurep(x) (scm_procedure_p(x) == SCM_BOOL_T)
#define VECTOR_P(x) (!(SCM_IMP(x)) && SCM_VECTORP(x))
#define reverse scm_list_reverse
#define length scm_list_length
#define input_portp scm_input_port_p
#define output_portp scm_output_port_p
