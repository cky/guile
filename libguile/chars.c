/*	Copyright (C) 1995,1996,1998, 2000, 2001 Free Software Foundation, Inc.
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




#include <ctype.h>
#include "libguile/_scm.h"
#include "libguile/validate.h"

#include "libguile/chars.h"


SCM_DEFINE (scm_char_p, "char?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a character, else @code{#f}.")
#define FUNC_NAME s_scm_char_p
{
  return SCM_BOOL(SCM_CHARP(x));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_eq_p, "char=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is the same character as @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_char_eq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return SCM_BOOL (SCM_EQ_P (x, y));
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_char_less_p, "char<?", scm_tc7_rpsubr, 
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is less than @var{y} in the ASCII sequence,\n"
	     "else @code{#f}.")
#define FUNC_NAME s_scm_char_less_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL(SCM_CHAR(x) < SCM_CHAR(y));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_leq_p, "char<=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is less than or equal to @var{y} in the\n"
	     "ASCII sequence, else @code{#f}.")
#define FUNC_NAME s_scm_char_leq_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL(SCM_CHAR(x) <= SCM_CHAR(y));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_gr_p, "char>?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is greater than @var{y} in the ASCII\n"
	     "sequence, else @code{#f}.")
#define FUNC_NAME s_scm_char_gr_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL(SCM_CHAR(x) > SCM_CHAR(y));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_geq_p, "char>=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is greater than or equal to @var{y} in the\n"
	     "ASCII sequence, else @code{#f}.")
#define FUNC_NAME s_scm_char_geq_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL(SCM_CHAR(x) >= SCM_CHAR(y));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_eq_p, "char-ci=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is the same character as @var{y} ignoring\n"
	     "case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_eq_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL(scm_upcase(SCM_CHAR(x))==scm_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_less_p, "char-ci<?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is less than @var{y} in the ASCII sequence\n"
	     "ignoring case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_less_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL((scm_upcase(SCM_CHAR(x))) < scm_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_leq_p, "char-ci<=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is less than or equal to @var{y} in the\n"
	     "ASCII sequence ignoring case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_leq_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL(scm_upcase(SCM_CHAR(x)) <= scm_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_gr_p, "char-ci>?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is greater than @var{y} in the ASCII\n"
	     "sequence ignoring case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_gr_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL(scm_upcase(SCM_CHAR(x)) > scm_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_geq_p, "char-ci>=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is greater than or equal to @var{y} in the\n"
	     "ASCII sequence ignoring case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_geq_p
{
  SCM_VALIDATE_CHAR (1,x);
  SCM_VALIDATE_CHAR (2,y);
  return SCM_BOOL(scm_upcase(SCM_CHAR(x)) >= scm_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_alphabetic_p, "char-alphabetic?", 1, 0, 0,
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is alphabetic, else @code{#f}.\n"
	    "Alphabetic means the same thing as the isalpha C library function.")
#define FUNC_NAME s_scm_char_alphabetic_p
{
  SCM_VALIDATE_CHAR (1,chr);
  return SCM_BOOL(isalpha(SCM_CHAR(chr)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_numeric_p, "char-numeric?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is numeric, else @code{#f}.\n"
	    "Numeric means the same thing as the isdigit C library function.")
#define FUNC_NAME s_scm_char_numeric_p
{
  SCM_VALIDATE_CHAR (1,chr);
  return SCM_BOOL(isdigit(SCM_CHAR(chr)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_whitespace_p, "char-whitespace?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is whitespace, else @code{#f}.\n"
	    "Whitespace means the same thing as the isspace C library function.")
#define FUNC_NAME s_scm_char_whitespace_p
{
  SCM_VALIDATE_CHAR (1,chr);
  return SCM_BOOL(isspace(SCM_CHAR(chr)));
}
#undef FUNC_NAME



SCM_DEFINE (scm_char_upper_case_p, "char-upper-case?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is uppercase, else @code{#f}.\n"
	    "Uppercase means the same thing as the isupper C library function.")
#define FUNC_NAME s_scm_char_upper_case_p
{
  SCM_VALIDATE_CHAR (1,chr);
  return SCM_BOOL(isupper(SCM_CHAR(chr)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_lower_case_p, "char-lower-case?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is lowercase, else @code{#f}.\n"
	    "Lowercase means the same thing as the islower C library function.")
#define FUNC_NAME s_scm_char_lower_case_p
{
  SCM_VALIDATE_CHAR (1,chr);
  return SCM_BOOL(islower(SCM_CHAR(chr)));
}
#undef FUNC_NAME



SCM_DEFINE (scm_char_is_both_p, "char-is-both?", 1, 0, 0, 
            (SCM chr),
	    "Return @code{#t} iff @var{chr} is either uppercase or lowercase, else @code{#f}.\n"
	    "Uppercase and lowercase are as defined by the isupper and islower\n"
	    "C library functions.")
#define FUNC_NAME s_scm_char_is_both_p
{
  SCM_VALIDATE_CHAR (1,chr);
  return SCM_BOOL((isupper(SCM_CHAR(chr)) || islower(SCM_CHAR(chr))));
}
#undef FUNC_NAME




SCM_DEFINE (scm_char_to_integer, "char->integer", 1, 0, 0, 
            (SCM chr),
	    "Return the number corresponding to ordinal position of @var{chr} in the\n"
	    "ASCII sequence.")
#define FUNC_NAME s_scm_char_to_integer
{
  SCM_VALIDATE_CHAR (1,chr);
  return scm_ulong2num((unsigned long)SCM_CHAR(chr));
}
#undef FUNC_NAME



SCM_DEFINE (scm_integer_to_char, "integer->char", 1, 0, 0, 
           (SCM n),
	    "Return the character at position @var{n} in the ASCII sequence.")
#define FUNC_NAME s_scm_integer_to_char
{
  SCM_VALIDATE_INUM_RANGE (1, n, 0, 256);
  return SCM_MAKE_CHAR (SCM_INUM (n));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_upcase, "char-upcase", 1, 0, 0, 
           (SCM chr),
	    "Return the uppercase character version of @var{chr}.")
#define FUNC_NAME s_scm_char_upcase
{
  SCM_VALIDATE_CHAR (1,chr);
  return SCM_MAKE_CHAR(scm_upcase(SCM_CHAR(chr)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_downcase, "char-downcase", 1, 0, 0, 
           (SCM chr),
	    "Return the lowercase character version of @var{chr}.")
#define FUNC_NAME s_scm_char_downcase
{
  SCM_VALIDATE_CHAR (1,chr);
  return SCM_MAKE_CHAR(scm_downcase(SCM_CHAR(chr)));
}
#undef FUNC_NAME





static unsigned char scm_upcase_table[SCM_CHAR_CODE_LIMIT];
static unsigned char scm_downcase_table[SCM_CHAR_CODE_LIMIT];
static const unsigned char scm_lowers[] = "abcdefghijklmnopqrstuvwxyz";
static const unsigned char scm_uppers[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";


void 
scm_tables_prehistory ()
{
  int i;
  for (i = 0; i < SCM_CHAR_CODE_LIMIT; i++)
    scm_upcase_table[i] = scm_downcase_table[i] = i;
  for (i = 0; i < (int) (sizeof scm_lowers / sizeof (scm_lowers[0])); i++)
    {
      scm_upcase_table[scm_lowers[i]] = scm_uppers[i];
      scm_downcase_table[scm_uppers[i]] = scm_lowers[i];
    }
}


int
scm_upcase (unsigned int c)
{
  if (c < sizeof (scm_upcase_table))
    return scm_upcase_table[c];
  else
    return c;
}


int
scm_downcase (unsigned int c)
{
  if (c < sizeof (scm_downcase_table))
    return scm_downcase_table[c];
  else
    return c;
}


#ifdef _DCC
# define ASCII
#else
# if (('\n'=='\025') && (' '=='\100') && ('a'=='\201') && ('A'=='\301'))
#  define EBCDIC
# endif /*  (('\n'=='\025') && (' '=='\100') && ('a'=='\201') && ('A'=='\301')) */
# if (('\n'=='\012') && (' '=='\040') && ('a'=='\141') && ('A'=='\101'))
#  define ASCII
# endif /*  (('\n'=='\012') && (' '=='\040') && ('a'=='\141') && ('A'=='\101')) */
#endif /* def _DCC */


#ifdef EBCDIC
char *const scm_charnames[] =
{
  "nul","soh","stx","etx", "pf", "ht", "lc","del",
   0   , 0   ,"smm", "vt", "ff", "cr", "so", "si",
  "dle","dc1","dc2","dc3","res", "nl", "bs", "il",
  "can", "em", "cc", 0   ,"ifs","igs","irs","ius",
   "ds","sos", "fs", 0   ,"byp", "lf","eob","pre",
   0   , 0   , "sm", 0   , 0   ,"enq","ack","bel",
   0   , 0   ,"syn", 0   , "pn", "rs", "uc","eot",
   0   , 0   , 0   , 0   ,"dc4","nak", 0   ,"sub",
   "space", scm_s_newline, "tab", "backspace", "return", "page", "null"};

const char scm_charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
\040\041\042\043\044\045\046\047\
\050\051\052\053\054\055\056\057\
\060\061\062\063\064\065\066\067\
\070\071\072\073\074\075\076\077\
 \n\t\b\r\f\0";
#endif /* def EBCDIC */
#ifdef ASCII
char *const scm_charnames[] =
{
  "nul","soh","stx","etx","eot","enq","ack","bel",
   "bs", "ht", "newline", "vt", "np", "cr", "so", "si",
  "dle","dc1","dc2","dc3","dc4","nak","syn","etb",
  "can", "em","sub","esc", "fs", "gs", "rs", "us",
  "space", "nl", "tab", "backspace", "return", "page", "null", "del"};
const char scm_charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
 \n\t\b\r\f\0\177";
#endif /* def ASCII */

int scm_n_charnames = sizeof (scm_charnames) / sizeof (char *);





void
scm_init_chars ()
{
#include "libguile/chars.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
