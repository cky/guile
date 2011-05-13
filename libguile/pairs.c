/* Copyright (C) 1995,1996,2000,2001, 2004, 2005, 2006, 2008, 2009, 2011 Free Software Foundation, Inc.
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



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/validate.h"

#include "libguile/pairs.h"

#include "verify.h"



/* {Pairs}
 */

/*
 * This compile-time test verifies the properties needed for the
 * efficient test macro scm_is_null_or_nil defined in pairs.h,
 * which is defined in terms of the SCM_MATCHES_BITS_IN_COMMON macro.
 *
 * See the comments preceeding the definitions of SCM_BOOL_F and
 * SCM_MATCHES_BITS_IN_COMMON in tags.h for more information.
 */
verify (SCM_BITS_DIFFER_IN_EXACTLY_ONE_BIT_POSITION		\
        (SCM_ELISP_NIL_BITS, SCM_EOL_BITS));


#if (SCM_DEBUG_PAIR_ACCESSES == 1)

#include "libguile/ports.h"
#include "libguile/strings.h"

void scm_error_pair_access (SCM non_pair)
{
  static unsigned int running = 0;
  SCM message = scm_from_locale_string ("Non-pair accessed with SCM_C[AD]R: `~S'\n");

  if (!running)
    {
      running = 1;
      scm_simple_format (scm_current_error_port (),
			 message, scm_list_1 (non_pair));
      abort ();
    }
}

#endif

SCM_DEFINE (scm_cons, "cons", 2, 0, 0,
	    (SCM x, SCM y),
	    "Return a newly allocated pair whose car is @var{x} and whose\n"
	    "cdr is @var{y}.  The pair is guaranteed to be different (in the\n"
	    "sense of @code{eq?}) from every previously existing object.")
#define FUNC_NAME s_scm_cons
{
  return scm_cell (SCM_UNPACK (x), SCM_UNPACK (y));
}
#undef FUNC_NAME


SCM 
scm_cons2 (SCM w, SCM x, SCM y)
{
  return scm_cons (w, scm_cons (x, y));
}


SCM_DEFINE (scm_pair_p, "pair?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{x} is a pair; otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_pair_p
{
  return scm_from_bool (scm_is_pair (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_car_x, "set-car!", 2, 0, 0,
            (SCM pair, SCM value),
            "Stores @var{value} in the car field of @var{pair}.  The value returned\n"
            "by @code{set-car!} is unspecified.")
#define FUNC_NAME s_scm_set_car_x
{
  SCM_VALIDATE_CONS (1, pair);
  SCM_SETCAR (pair, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_set_cdr_x, "set-cdr!", 2, 0, 0,
            (SCM pair, SCM value),
            "Stores @var{value} in the cdr field of @var{pair}.  The value returned\n"
            "by @code{set-cdr!} is unspecified.")
#define FUNC_NAME s_scm_set_cdr_x
{
  SCM_VALIDATE_CONS (1, pair);
  SCM_SETCDR (pair, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Every cxr-pattern is made up of pairs of bits, starting with the two least
 * significant bits.  If in a pair of bits the least significant of the two
 * bits is 0, this means CDR, otherwise CAR.  The most significant bits of the
 * two bits is only needed to indicate when cxr-ing is ready.  This is the
 * case, when all remaining pairs of bits equal 00.  */

/* The compiler should unroll this. */
#define CHASE_PAIRS(tree, FUNC_NAME, pattern)                           \
  scm_t_uint32 pattern_var = pattern;                                   \
  do                                                                    \
    {                                                                   \
      if (!scm_is_pair (tree))                                          \
	scm_wrong_type_arg_msg (FUNC_NAME, 0, tree, "pair");            \
      tree = (pattern_var & 1) ? SCM_CAR (tree) : SCM_CDR (tree);       \
      pattern_var >>= 2;                                                \
    }                                                                   \
  while (pattern_var);                                                  \
  return tree


SCM_DEFINE (scm_cdr, "cdr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdr", 0x02); /* 00000010 */
}
SCM_DEFINE (scm_car, "car", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "car", 0x03); /* 00000011 */
}
SCM_DEFINE (scm_cddr, "cddr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cddr", 0x0a); /* 00001010 */
}
SCM_DEFINE (scm_cdar, "cdar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdar", 0x0b); /* 00001011 */
}
SCM_DEFINE (scm_cadr, "cadr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cadr", 0x0e); /* 00001110 */
}
SCM_DEFINE (scm_caar, "caar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caar", 0x0f); /* 00001111 */
}
SCM_DEFINE (scm_cdddr, "cdddr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdddr", 0x2a); /* 00101010 */
}
SCM_DEFINE (scm_cddar, "cddar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cddar", 0x2b); /* 00101011 */
}
SCM_DEFINE (scm_cdadr, "cdadr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdadr", 0x2e); /* 00101110 */
}
SCM_DEFINE (scm_cdaar, "cdaar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdaar", 0x2f); /* 00101111 */
}
SCM_DEFINE (scm_caddr, "caddr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caddr", 0x3a); /* 00111010 */
}
SCM_DEFINE (scm_cadar, "cadar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cadar", 0x3b); /* 00111011 */
}
SCM_DEFINE (scm_caadr, "caadr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caadr", 0x3e); /* 00111110 */
}
SCM_DEFINE (scm_caaar, "caaar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caaar", 0x3f); /* 00111111 */
}
SCM_DEFINE (scm_cddddr, "cddddr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cddddr", 0xaa); /* 10101010 */
}
SCM_DEFINE (scm_cdddar, "cdddar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdddar", 0xab); /* 10101011 */
}
SCM_DEFINE (scm_cddadr, "cddadr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cddadr", 0xae); /* 10101110 */
}
SCM_DEFINE (scm_cddaar, "cddaar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cddaar", 0xaf); /* 10101111 */
}
SCM_DEFINE (scm_cdaddr, "cdaddr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdaddr", 0xba); /* 10111010 */
}
SCM_DEFINE (scm_cdadar, "cdadar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdadar", 0xbb); /* 10111011 */
}
SCM_DEFINE (scm_cdaadr, "cdaadr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdaadr", 0xbe); /* 10111110 */
}
SCM_DEFINE (scm_cdaaar, "cdaaar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cdaaar", 0xbf); /* 10111111 */
}
SCM_DEFINE (scm_cadddr, "cadddr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cadddr", 0xea); /* 11101010 */
}
SCM_DEFINE (scm_caddar, "caddar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caddar", 0xeb); /* 11101011 */
}
SCM_DEFINE (scm_cadadr, "cadadr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cadadr", 0xee); /* 11101110 */
}
SCM_DEFINE (scm_cadaar, "cadaar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "cadaar", 0xef); /* 11101111 */
}
SCM_DEFINE (scm_caaddr, "caaddr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caaddr", 0xfa); /* 11111010 */
}
SCM_DEFINE (scm_caadar, "caadar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caadar", 0xfb); /* 11111011 */
}
SCM_DEFINE (scm_caaadr, "caaadr", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caaadr", 0xfe); /* 11111110 */
}
SCM_DEFINE (scm_caaaar, "caaaar", 1, 0, 0, (SCM x), "")
{
  CHASE_PAIRS (x, "caaaar", 0xff); /* 11111111 */
}



void
scm_init_pairs ()
{
#include "libguile/pairs.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
