/* classes: h_files */

#ifndef RANDOMH
#define RANDOMH
/*	Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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


#include "libguile/__scm.h"


/*
 * A plugin interface for RNGs
 *
 * Using this interface, it is possible for the application to tell
 * libguile to use a different RNG.  This is desirable if it is
 * necessary to use the same RNG everywhere in the application in
 * order to prevent interference, if the application uses RNG
 * hardware, or if the application has special demands on the RNG.
 *
 * Look how the default generator is "plugged in" in scm_init_random().
 */

typedef struct scm_rstate {
  int reserved0;
  double reserved1;
  /* Custom fields follow here */
} scm_rstate;

typedef struct scm_rng {
  size_t rstate_size;				    /* size of random state */
  unsigned long (*random_bits) (scm_rstate *state); /* gives 32 random bits */
  void (*init_rstate) (scm_rstate *state, char *seed, int n);
  scm_rstate *(*copy_rstate) (scm_rstate *state);
} scm_rng;

extern scm_rng scm_the_rng;


/*
 * Default RNG
 */
typedef struct scm_i_rstate {
  scm_rstate rstate;
  unsigned long w;
  unsigned long c;
} scm_i_rstate;

extern unsigned long scm_i_uniform32 (scm_i_rstate *);
extern void scm_i_init_rstate (scm_i_rstate *, char *seed, int n);
extern scm_i_rstate *scm_i_copy_rstate (scm_i_rstate *);


/*
 * Random number library functions
 */
extern scm_rstate *scm_c_make_rstate (char *, int);
extern scm_rstate *scm_c_default_rstate (void);
#define scm_c_uniform32(RSTATE) scm_the_rng.random_bits (RSTATE)
extern double scm_c_uniform01 (scm_rstate *);
extern double scm_c_normal01 (scm_rstate *);
extern double scm_c_exp1 (scm_rstate *);
extern unsigned long scm_c_random (scm_rstate *, unsigned long m);
extern SCM scm_c_random_bignum (scm_rstate *, SCM m);


/*
 * Scheme level interface
 */
extern scm_bits_t scm_tc16_rstate;
#define SCM_RSTATEP(obj) SCM_TYP16_PREDICATE (scm_tc16_rstate, obj)
#define SCM_RSTATE(obj)  ((scm_rstate *) SCM_CELL_WORD_1 (obj))

extern unsigned char scm_masktab[256];

extern SCM scm_var_random_state;
extern SCM scm_random (SCM n, SCM state);
extern SCM scm_copy_random_state (SCM state);
extern SCM scm_seed_to_random_state (SCM seed);
extern SCM scm_random_uniform (SCM state);
extern SCM scm_random_solid_sphere_x (SCM v, SCM state);
extern SCM scm_random_hollow_sphere_x (SCM v, SCM state);
extern SCM scm_random_normal (SCM state);
extern SCM scm_random_normal_vector_x (SCM v, SCM state);
extern SCM scm_random_exp (SCM state);
extern void scm_init_random (void);

#endif  /* RANDOMH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
