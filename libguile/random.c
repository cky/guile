/*      Copyright (C) 1999 Free Software Foundation, Inc.
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

/* Author: Mikael Djurfeldt <djurfeldt@nada.kth.se> */

/* We need this to get the definitions for HAVE_ALLOCA_H, etc.  */
#include "scmconfig.h"

/* AIX requires this to be the first thing in the file.  The #pragma
   directive is indented so pre-ANSI compilers will ignore it, rather
   than choke on it.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "_scm.h"

#include <math.h>
#include "genio.h"
#include "smob.h"
#include "numbers.h"
#include "feature.h"

#include "random.h"


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
    scm_wta (SCM_MAKINUM (scm_the_rng.rstate_size),
	     (char *) SCM_NALLOC, "rstate");
  return memcpy (new_state, state, scm_the_rng.rstate_size);
}


/*
 * Random number library functions
 */

inline double
scm_i_uniform01 (scm_rstate *state)
{
  double x = (double) scm_the_rng.random_bits (state) / (double) 0xFFFFFFFFUL;
  return ((x + (double) scm_the_rng.random_bits (state))
	  / (double) 0xFFFFFFFFUL);
}

double
scm_i_normal01 (scm_rstate *state)
{
  if (state->reserved0)
    {
      state->reserved0 = 0;
      return state->reserved1;
    }
  else
    {
      double r, a, n;
      state->reserved0 = 1;
      
      r = sqrt (-2.0 * log (scm_i_uniform01 (state)));
      a = 2.0 * M_PI * scm_i_uniform01 (state);
      
      n = r * sin (a);
      state->reserved1 = r * cos (a);
      
      return n;
    }
}

double
scm_i_exp1 (scm_rstate *state)
{
  return - log (scm_i_uniform01 (state));
}

unsigned char scm_masktab[256];

unsigned long
scm_i_random (unsigned long m, scm_rstate *state)
{
  unsigned int r, mask;
  mask = (m < 0x100
	  ? scm_masktab[m]
	  : (m < 0x10000
	     ? scm_masktab[m >> 8] << 8 | 0xFF
	     : (m < 0x1000000
		? scm_masktab[m >> 16] << 16 | 0xFFFF
		: scm_masktab[m >> 24] << 24 | 0xFFFFFF)));
  while ((r = scm_the_rng.random_bits (state) & mask) >= m);
  return r;
}

SCM
scm_i_random_bignum (SCM m, scm_rstate *state)
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
      mask = s < 0x100 ? scm_masktab[s] : scm_masktab[s >> 8] << 8 | 0xFF;
    }
  else
#endif
    {
      /* fix most significant 32 bits */
      w = ((LONG32 *) SCM_BDIGITS (m))[nd / 2 - 1];
      mask = (w < 0x10000
	      ? (w < 0x100
		 ? scm_masktab[w]
		 : scm_masktab[w >> 8] << 8 | 0xFF)
	      : (w < 0x1000000
		 ? scm_masktab[w >> 16] << 16 | 0xFFFF
		 : scm_masktab[w >> 24] << 24 | 0xFFFFFF));
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
	  i /= 2;
	  bits[--i] = scm_the_rng.random_bits (state) & mask;
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
 
long scm_tc16_rstate;

static SCM
make_rstate (scm_rstate *state)
{
  SCM cell;
  SCM_NEWCELL (cell);
  SCM_ENTER_A_SECTION;
  SCM_SETCDR (cell, state);
  SCM_SETCAR (cell, scm_tc16_rstate);
  SCM_EXIT_A_SECTION;
  return cell;
}

static int
print_rstate (SCM rstate, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<random-state ", port);
  scm_intprint ((long) SCM_RSTATE (rstate), 16, port);
  scm_putc ('>', port);
  return 1;
}

static scm_sizet
free_rstate (SCM rstate)
{
  free (SCM_RSTATE (rstate));
  return scm_the_rng.rstate_size;
}

static scm_smobfuns rstate_smob = { 0, free_rstate, print_rstate, 0};

/*
 * Scheme level interface.
 */

SCM_GLOBAL_VCELL_INIT (scm_var_random_state, "*random-state*", scm_make_random_state (scm_makfrom0str ("URL:http://stat.fsu.edu/~geo/diehard.html")));

SCM_PROC (s_random, "random", 1, 1, 0, scm_random);

SCM
scm_random (SCM n, SCM state)
{
  if (SCM_UNBNDP (state))
    state = SCM_CDR (scm_var_random_state);
  SCM_ASSERT (SCM_NIMP (state) && SCM_RSTATEP (state),
	      state, SCM_ARG2, s_random);
  if (SCM_INUMP (n))
    {
      unsigned long m = SCM_INUM (n);
      SCM_ASSERT (m > 0, n, SCM_ARG1, s_random);
      return SCM_MAKINUM (scm_i_random (m, SCM_RSTATE (state)));
    }
  SCM_ASSERT (SCM_NIMP (n), n, SCM_ARG1, s_random);
  if (SCM_REALP (n))
    return scm_makdbl (SCM_REALPART (n) * scm_i_uniform01 (SCM_RSTATE (state)),
		       0.0);
  SCM_ASSERT (SCM_TYP16 (n) == scm_tc16_bigpos, n, SCM_ARG1, s_random);
  return scm_i_random_bignum (n, SCM_RSTATE (state));
}

SCM_PROC (s_make_random_state, "make-random-state", 0, 1, 0, scm_make_random_state);

SCM
scm_make_random_state (SCM state)
{
  if (SCM_UNBNDP (state))
    {
      state = SCM_CDR (scm_var_random_state);
      goto copy_state;
    }
  else if (SCM_NUMBERP (state))
    {
      state = scm_number_to_string (state, SCM_UNDEFINED);
      goto seed;
    }
  else if (SCM_NIMP (state) && SCM_STRINGP (state))
    seed:
    {
      scm_rstate *nstate = malloc (scm_the_rng.rstate_size);
      if (nstate == 0)
	scm_wta (SCM_MAKINUM (scm_the_rng.rstate_size),
		 (char *) SCM_NALLOC,
		 "rstate");
      nstate->reserved0 = 0;
      scm_the_rng.init_rstate (nstate, SCM_ROCHARS (state), SCM_LENGTH (state));
      return make_rstate (nstate);
    }
 copy_state:
  SCM_ASSERT (SCM_NIMP (state) && SCM_RSTATEP (state),
	      state,
	      SCM_ARG1,
	      s_make_random_state);
  return make_rstate (scm_the_rng.copy_rstate (SCM_RSTATE (state)));
}

SCM_PROC (s_random_uniform, "random:uniform", 0, 1, 0, scm_random_uniform);

SCM
scm_random_uniform (SCM state)
{
  if (SCM_UNBNDP (state))
    state = SCM_CDR (scm_var_random_state);
  SCM_ASSERT (SCM_NIMP (state) && SCM_RSTATEP (state),
	      state,
	      SCM_ARG1,
	      s_random_uniform);
  return scm_makdbl (scm_i_uniform01 (SCM_RSTATE (state)), 0.0);
}

static void
vector_scale (SCM v, double c)
{
  int n = SCM_LENGTH (v);
  if (SCM_VECTORP (v))
    while (--n >= 0)
      SCM_REAL (SCM_VELTS (v)[n]) *= c;
  else
    while (--n >= 0)
      ((double *) SCM_VELTS (v))[n] *= c;
}

static double
vector_sum_squares (SCM v)
{
  double x, sum = 0.0;
  int n = SCM_LENGTH (v);
  if (SCM_VECTORP (v))
    while (--n >= 0)
      {
	x = SCM_REAL (SCM_VELTS (v)[n]);
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

SCM_PROC (s_random_solid_sphere_x, "random:solid-sphere!", 1, 1, 0, scm_random_solid_sphere_x);

/* For the uniform distribution on the solid sphere, note that in
 * this distribution the length r of the vector has cumulative
 * distribution r^n; i.e., u=r^n is uniform [0,1], so r can be
 * generated as r=u^(1/n).
 */
SCM
scm_random_solid_sphere_x (SCM v, SCM state)
{
  SCM_ASSERT (SCM_NIMP (v)
	      && (SCM_VECTORP (v) || SCM_TYP7 (v) == scm_tc7_dvect),
	      v, SCM_ARG1, s_random_solid_sphere_x);
  if (SCM_UNBNDP (state))
    state = SCM_CDR (scm_var_random_state);
  SCM_ASSERT (SCM_NIMP (state) && SCM_RSTATEP (state),
	      state,
	      SCM_ARG2,
	      s_random_solid_sphere_x);
  scm_random_normal_vector_x (v, state);
  vector_scale (v,
		pow (scm_i_uniform01 (SCM_RSTATE (state)),
		     1.0 / SCM_LENGTH (v))
		/ sqrt (vector_sum_squares (v)));
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_random_hollow_sphere_x, "random:hollow-sphere!", 1, 1, 0, scm_random_hollow_sphere_x);

SCM
scm_random_hollow_sphere_x (SCM v, SCM state)
{
  SCM_ASSERT (SCM_NIMP (v)
	      && (SCM_VECTORP (v) || SCM_TYP7 (v) == scm_tc7_dvect),
	      v, SCM_ARG1, s_random_solid_sphere_x);
  if (SCM_UNBNDP (state))
    state = SCM_CDR (scm_var_random_state);
  SCM_ASSERT (SCM_NIMP (state) && SCM_RSTATEP (state),
	      state,
	      SCM_ARG2,
	      s_random_hollow_sphere_x);
  scm_random_normal_vector_x (v, state);
  vector_scale (v, 1 / sqrt (vector_sum_squares (v)));
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_random_normal, "random:normal", 0, 1, 0, scm_random_normal);

SCM
scm_random_normal (SCM state)
{
  if (SCM_UNBNDP (state))
    state = SCM_CDR (scm_var_random_state);
  SCM_ASSERT (SCM_NIMP (state) && SCM_RSTATEP (state),
	      state,
	      SCM_ARG1,
	      s_random_normal);
  return scm_makdbl (scm_i_normal01 (SCM_RSTATE (state)), 0.0);
}

SCM_PROC (s_random_normal_vector_x, "random:normal-vector!", 1, 1, 0, scm_random_normal_vector_x);

SCM
scm_random_normal_vector_x (SCM v, SCM state)
{
  int n;
  SCM_ASSERT (SCM_NIMP (v)
	      && (SCM_VECTORP (v) || SCM_TYP7 (v) == scm_tc7_dvect),
	      v, SCM_ARG1, s_random_solid_sphere_x);
  if (SCM_UNBNDP (state))
    state = SCM_CDR (scm_var_random_state);
  SCM_ASSERT (SCM_NIMP (state) && SCM_RSTATEP (state),
	      state,
	      SCM_ARG2,
	      s_random_normal_vector_x);
  n = SCM_LENGTH (v);
  if (SCM_VECTORP (v))
    while (--n >= 0)
      SCM_VELTS (v)[n] = scm_makdbl (scm_i_normal01 (SCM_RSTATE (state)), 0.0);
  else
    while (--n >= 0)
      ((double *) SCM_VELTS (v))[n] = scm_i_normal01 (SCM_RSTATE (state));
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_random_exp, "random:exp", 0, 1, 0, scm_random_exp);

SCM
scm_random_exp (SCM state)
{
  if (SCM_UNBNDP (state))
    state = SCM_CDR (scm_var_random_state);
  SCM_ASSERT (SCM_NIMP (state) && SCM_RSTATEP (state),
	      state,
	      SCM_ARG1,
	      s_random_exp);
  return scm_makdbl (scm_i_exp1 (SCM_RSTATE (state)), 0.0);
}

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
  
  scm_tc16_rstate = scm_newsmob (&rstate_smob);

  for (m = 1; m <= 0x100; m <<= 1)
    for (i = m >> 1; i < m; ++i)
      scm_masktab[i] = m - 1;
  
#include "random.x"

  scm_add_feature ("random");
}
