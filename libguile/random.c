/* Copyright (C) 1999,2000,2001 Free Software Foundation, Inc.
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


/* Author: Mikael Djurfeldt <djurfeldt@nada.kth.se> */

#include "libguile/_scm.h"

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "libguile/smob.h"
#include "libguile/numbers.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/unif.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/random.h"


/*
 * A plugin interface for RNGs
 *
 * Using this interface, it is possible for the application to tell
 * libguile to use a different RNG.  This is desirable if it is
 * necessary to use the same RNG everywhere in the application in
 * order to prevent interference, if the application uses RNG
 * hardware, or if the application has special demands on the RNG.
 *
 * Look in random.h and how the default generator is "plugged in" in
 * scm_init_random().
 */

scm_rng scm_the_rng;


/*
 * The prepackaged RNG
 *
 * This is the MWC (Multiply With Carry) random number generator
 * described by George Marsaglia at the Department of Statistics and
 * Supercomputer Computations Research Institute, The Florida State
 * University (http://stat.fsu.edu/~geo).
 *
 * It uses 64 bits, has a period of 4578426017172946943 (4.6e18), and
 * passes all tests in the DIEHARD test suite
 * (http://stat.fsu.edu/~geo/diehard.html)
 */

#define A 2131995753UL

#if SIZEOF_LONG > 4
#if SIZEOF_INT > 4
#define LONG32 unsigned short
#else
#define LONG32 unsigned int
#endif
#define LONG64 unsigned long
#else
#define LONG32 unsigned long
#define LONG64 unsigned long long
#endif

#if SIZEOF_LONG > 4 || defined (HAVE_LONG_LONGS)

unsigned long
scm_i_uniform32 (scm_i_rstate *state)
{
  LONG64 x = (LONG64) A * state->w + state->c;
  LONG32 w = x & 0xffffffffUL;
  state->w = w;
  state->c = x >> 32L;
  return w;
}

#else

/*     ww  This is a portable version of the same RNG without 64 bit
 *   * aa  arithmetic.
 *   ----
 *     xx  It is only intended to provide identical behaviour on
 *    xx   platforms without 8 byte longs or long longs until
 *    xx   someone has implemented the routine in assembler code.
 *   xxcc
 *   ----
 *   ccww
 */

#define L(x) ((x) & 0xffff)
#define H(x) ((x) >> 16)

unsigned long
scm_i_uniform32 (scm_i_rstate *state)
{
  LONG32 x1 = L (A) * L (state->w);
  LONG32 x2 = L (A) * H (state->w);
  LONG32 x3 = H (A) * L (state->w);
  LONG32 w = L (x1) + L (state->c);
  LONG32 m = H (x1) + L (x2) + L (x3) + H (state->c) + H (w);
  LONG32 x4 = H (A) * H (state->w);
  state->w = w = (L (m) << 16) + L (w);
  state->c = H (x2) + H (x3) + x4 + H (m);
  return w;
}

#endif

void
scm_i_init_rstate (scm_i_rstate *state, char *seed, int n)
{
  LONG32 w = 0L;
  LONG32 c = 0L;
  int i, m;
  for (i = 0; i < n; ++i)
    {
      m = i % 8;
      if (m < 4)
	w += seed[i] << (8 * m);
      else
        c += seed[i] << (8 * (m - 4));
    }
  if ((w == 0 && c == 0) || (w == 0xffffffffUL && c == A - 1))
    ++c;
  state->w = w;
  state->c = c;
}

scm_i_rstate *
scm_i_copy_rstate (scm_i_rstate *state)
{
  scm_rstate *new_state = malloc (scm_the_rng.rstate_size);
  if (new_state == 0)
    scm_memory_error ("rstate");
  return memcpy (new_state, state, scm_the_rng.rstate_size);
}


/*
 * Random number library functions
 */

scm_rstate *
scm_c_make_rstate (char *seed, int n)
{
  scm_rstate *state = malloc (scm_the_rng.rstate_size);
  if (state == 0)
    scm_memory_error ("rstate");
  state->reserved0 = 0;
  scm_the_rng.init_rstate (state, seed, n);
  return state;
}


scm_rstate *
scm_c_default_rstate ()
#define FUNC_NAME "scm_c_default_rstate"
{
  SCM state = SCM_CDR (scm_var_random_state);
  if (!SCM_RSTATEP (state))
    SCM_MISC_ERROR ("*random-state* contains bogus random state", SCM_EOL);
  return SCM_RSTATE (state);
}
#undef FUNC_NAME


inline double
scm_c_uniform01 (scm_rstate *state)
{
  double x = (double) scm_the_rng.random_bits (state) / (double) 0xffffffffUL;
  return ((x + (double) scm_the_rng.random_bits (state))
	  / (double) 0xffffffffUL);
}

double
scm_c_normal01 (scm_rstate *state)
{
  if (state->reserved0)
    {
      state->reserved0 = 0;
      return state->reserved1;
    }
  else
    {
      double r, a, n;
      
      r = sqrt (-2.0 * log (scm_c_uniform01 (state)));
      a = 2.0 * M_PI * scm_c_uniform01 (state);
      
      n = r * sin (a);
      state->reserved1 = r * cos (a);
      state->reserved0 = 1;
      
      return n;
    }
}

double
scm_c_exp1 (scm_rstate *state)
{
  return - log (scm_c_uniform01 (state));
}

unsigned char scm_masktab[256];

unsigned long
scm_c_random (scm_rstate *state, unsigned long m)
{
  unsigned int r, mask;
  mask = (m < 0x100
	  ? scm_masktab[m]
	  : (m < 0x10000
	     ? scm_masktab[m >> 8] << 8 | 0xff
	     : (m < 0x1000000
		? scm_masktab[m >> 16] << 16 | 0xffff
		: scm_masktab[m >> 24] << 24 | 0xffffff)));
  while ((r = scm_the_rng.random_bits (state) & mask) >= m);
  return r;
}

SCM
scm_c_random_bignum (scm_rstate *state, SCM m)
{
  SCM b;
  int i, nd;
  LONG32 *bits, mask, w;
  nd = SCM_NUMDIGS (m);
  /* calculate mask for most significant digit */
#if SIZEOF_INT == 4
  /* 16 bit digits */
  if (nd & 1)
    {
      /* fix most significant 16 bits */
      unsigned short s = SCM_BDIGITS (m)[nd - 1];
      mask = s < 0x100 ? scm_masktab[s] : scm_masktab[s >> 8] << 8 | 0xff;
    }
  else
#endif
    {
      /* fix most significant 32 bits */
#if SIZEOF_INT == 4
      w = SCM_BDIGITS (m)[nd - 1] << 16 | SCM_BDIGITS (m)[nd - 2];
#else
      w = SCM_BDIGITS (m)[nd - 1];
#endif
      mask = (w < 0x10000
	      ? (w < 0x100
		 ? scm_masktab[w]
		 : scm_masktab[w >> 8] << 8 | 0xff)
	      : (w < 0x1000000
		 ? scm_masktab[w >> 16] << 16 | 0xffff
		 : scm_masktab[w >> 24] << 24 | 0xffffff));
    }
  b = scm_mkbig (nd, 0);
  bits = (LONG32 *) SCM_BDIGITS (b);
  do
    {
      i = nd;
      /* treat most significant digit specially */
#if SIZEOF_INT == 4
      /* 16 bit digits */
      if (i & 1)
	{
	  ((SCM_BIGDIG*) bits)[i - 1] = scm_the_rng.random_bits (state) & mask;
	  i /= 2;
	}
      else
#endif
	{
	  /* fix most significant 32 bits */
#if SIZEOF_INT == 4
	  w = scm_the_rng.random_bits (state) & mask;
	  ((SCM_BIGDIG*) bits)[i - 2] = w & 0xffff;
	  ((SCM_BIGDIG*) bits)[i - 1] = w >> 16;
	  i = i / 2 - 1;
#else
	  i /= 2;
	  bits[--i] = scm_the_rng.random_bits (state) & mask;
#endif
	}
      /* now fill up the rest of the bignum */
      while (i)
	bits[--i] = scm_the_rng.random_bits (state);
      b = scm_normbig (b);
      if (SCM_INUMP (b))
	return b;
    } while (scm_bigcomp (b, m) <= 0);
  return b;
}

/*
 * Scheme level representation of random states.
 */
 
scm_bits_t scm_tc16_rstate;

static SCM
make_rstate (scm_rstate *state)
{
  SCM_RETURN_NEWSMOB (scm_tc16_rstate, state);
}

static scm_sizet
rstate_free (SCM rstate)
{
  free (SCM_RSTATE (rstate));
  return scm_the_rng.rstate_size;
}

/*
 * Scheme level interface.
 */

SCM_GLOBAL_VARIABLE_INIT (scm_var_random_state, "*random-state*", scm_seed_to_random_state (scm_makfrom0str ("URL:http://stat.fsu.edu/~geo/diehard.html")));

SCM_DEFINE (scm_random, "random", 1, 1, 0, 
            (SCM n, SCM state),
            "Return a number in [0,N).\n"
            "\n"
            "Accepts a positive integer or real n and returns a \n"
            "number of the same type between zero (inclusive) and \n"
            "N (exclusive). The values returned have a uniform \n"
            "distribution.\n"
            "\n"
            "The optional argument @var{state} must be of the type produced\n"
	    "by @code{seed->random-state}. It defaults to the value of the\n"
	    "variable @var{*random-state*}. This object is used to maintain\n"
	    "the state of the pseudo-random-number generator and is altered\n"
	    "as a side effect of the random operation.")
#define FUNC_NAME s_scm_random
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2,state);
  if (SCM_INUMP (n))
    {
      unsigned long m = SCM_INUM (n);
      SCM_ASSERT_RANGE (1,n,m > 0);
      return SCM_MAKINUM (scm_c_random (SCM_RSTATE (state), m));
    }
  SCM_VALIDATE_NIM (1,n);
  if (SCM_REALP (n))
    return scm_make_real (SCM_REAL_VALUE (n)
			  * scm_c_uniform01 (SCM_RSTATE (state)));
  SCM_VALIDATE_SMOB (1, n, big);
  return scm_c_random_bignum (SCM_RSTATE (state), n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_copy_random_state, "copy-random-state", 0, 1, 0, 
            (SCM state),
            "Return a copy of the random state @var{state}.")
#define FUNC_NAME s_scm_copy_random_state
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1,state);
  return make_rstate (scm_the_rng.copy_rstate (SCM_RSTATE (state)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_seed_to_random_state, "seed->random-state", 1, 0, 0, 
            (SCM seed),
            "Return a new random state using @var{seed}.")
#define FUNC_NAME s_scm_seed_to_random_state
{
  if (SCM_NUMBERP (seed))
    seed = scm_number_to_string (seed, SCM_UNDEFINED);
  SCM_VALIDATE_STRING (1,seed);
  return make_rstate (scm_c_make_rstate (SCM_STRING_CHARS (seed),
					 SCM_STRING_LENGTH (seed)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_uniform, "random:uniform", 0, 1, 0, 
            (SCM state),
	    "Return a uniformly distributed inexact real random number in\n"
	    "[0,1).")
#define FUNC_NAME s_scm_random_uniform
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1,state);
  return scm_make_real (scm_c_uniform01 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_normal, "random:normal", 0, 1, 0, 
            (SCM state),
	    "Return an inexact real in a normal distribution.  The\n"
	    "distribution used has mean 0 and standard deviation 1.  For a\n"
	    "normal distribution with mean m and standard deviation d use\n"
	    "@code{(+ m (* d (random:normal)))}.")
#define FUNC_NAME s_scm_random_normal
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1,state);
  return scm_make_real (scm_c_normal01 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

#ifdef HAVE_ARRAYS

static void
vector_scale (SCM v, double c)
{
  int n = SCM_INUM (scm_uniform_vector_length (v));
  if (SCM_VECTORP (v))
    while (--n >= 0)
      SCM_REAL_VALUE (SCM_VELTS (v)[n]) *= c;
  else
    while (--n >= 0)
      ((double *) SCM_VELTS (v))[n] *= c;
}

static double
vector_sum_squares (SCM v)
{
  double x, sum = 0.0;
  int n = SCM_INUM (scm_uniform_vector_length (v));
  if (SCM_VECTORP (v))
    while (--n >= 0)
      {
	x = SCM_REAL_VALUE (SCM_VELTS (v)[n]);
	sum += x * x;
      }
  else
    while (--n >= 0)
      {
	x = ((double *) SCM_VELTS (v))[n];
	sum += x * x;
      }
  return sum;
}

/* For the uniform distribution on the solid sphere, note that in
 * this distribution the length r of the vector has cumulative
 * distribution r^n; i.e., u=r^n is uniform [0,1], so r can be
 * generated as r=u^(1/n).
 */
SCM_DEFINE (scm_random_solid_sphere_x, "random:solid-sphere!", 1, 1, 0, 
            (SCM v, SCM state),
            "Fills vect with inexact real random numbers\n"
            "the sum of whose squares is less than 1.0.\n"
            "Thinking of vect as coordinates in space of \n"
            "dimension n = (vector-length vect), the coordinates \n"
            "are uniformly distributed within the unit n-shere.\n"
            "The sum of the squares of the numbers is returned.")
#define FUNC_NAME s_scm_random_solid_sphere_x
{
  SCM_VALIDATE_VECTOR_OR_DVECTOR (1,v);
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2,state);
  scm_random_normal_vector_x (v, state);
  vector_scale (v,
		pow (scm_c_uniform01 (SCM_RSTATE (state)),
		     1.0 / SCM_INUM (scm_uniform_vector_length (v)))
		/ sqrt (vector_sum_squares (v)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_hollow_sphere_x, "random:hollow-sphere!", 1, 1, 0, 
            (SCM v, SCM state),
            "Fills vect with inexact real random numbers\n"
            "the sum of whose squares is equal to 1.0.\n"
            "Thinking of vect as coordinates in space of \n"
            "dimension n = (vector-length vect), the coordinates\n"
            "are uniformly distributed over the surface of the \n"
            "unit n-shere.")
#define FUNC_NAME s_scm_random_hollow_sphere_x
{
  SCM_VALIDATE_VECTOR_OR_DVECTOR (1,v);
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2,state);
  scm_random_normal_vector_x (v, state);
  vector_scale (v, 1 / sqrt (vector_sum_squares (v)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_random_normal_vector_x, "random:normal-vector!", 1, 1, 0, 
            (SCM v, SCM state),
            "Fills vect with inexact real random numbers that are\n"
            "independent and standard normally distributed\n"
            "(i.e., with mean 0 and variance 1).")
#define FUNC_NAME s_scm_random_normal_vector_x
{
  int n;
  SCM_VALIDATE_VECTOR_OR_DVECTOR (1,v);
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2,state);
  n = SCM_INUM (scm_uniform_vector_length (v));
  if (SCM_VECTORP (v))
    while (--n >= 0)
      SCM_VELTS (v)[n] = scm_make_real (scm_c_normal01 (SCM_RSTATE (state)));
  else
    while (--n >= 0)
      ((double *) SCM_VELTS (v))[n] = scm_c_normal01 (SCM_RSTATE (state));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif /* HAVE_ARRAYS */

SCM_DEFINE (scm_random_exp, "random:exp", 0, 1, 0, 
            (SCM state),
	    "Return an inexact real in an exponential distribution with mean\n"
	    "1.  For an exponential distribution with mean u use (* u\n"
	    "(random:exp)).")
#define FUNC_NAME s_scm_random_exp
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1,state);
  return scm_make_real (scm_c_exp1 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

void
scm_init_random ()
{
  int i, m;
  /* plug in default RNG */
  scm_rng rng =
  {
    sizeof (scm_i_rstate),
    (unsigned long (*)()) scm_i_uniform32,
    (void (*)())          scm_i_init_rstate,
    (scm_rstate *(*)())    scm_i_copy_rstate
  };
  scm_the_rng = rng;
  
  scm_tc16_rstate = scm_make_smob_type ("random-state", 0);
  scm_set_smob_free (scm_tc16_rstate, rstate_free);

  for (m = 1; m <= 0x100; m <<= 1)
    for (i = m >> 1; i < m; ++i)
      scm_masktab[i] = m - 1;
  
#ifndef SCM_MAGIC_SNARFER
#include "libguile/random.x"
#endif

  /* Check that the assumptions about bits per bignum digit are correct. */
#if SIZEOF_INT == 4
  m = 16;
#else
  m = 32;
#endif
  if (m != SCM_BITSPERDIG)
    {
      fprintf (stderr, "Internal inconsistency: Confused about bignum digit size in random.c\n");
      exit (1);
    }
  
  scm_add_feature ("random");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
