/*	Copyright (C) 1998, 1999 Free Software Foundation, Inc.
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


/* This is an implementation of guardians as described in
 * R. Kent Dybvig, Carl Bruggeman, and David Eby (1993) "Guardians in
 * a Generation-Based Garbage Collector" ACM SIGPLAN Conference on
 * Programming Language Design and Implementation, June 1993
 * ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/guardians.ps.gz
 *
 * Author:      Michael N. Livshin
 * Modified by: Mikael Djurfeldt
 */

#include <stdio.h>
#include <assert.h>

#include "_scm.h"
#include "print.h"
#include "smob.h"
#include "genio.h"

#include "guardians.h"

static long scm_tc16_guardian;

/* The live and zombies FIFOs are implemented as tconcs as described
   in Dybvig's paper.  This decouples addition and removal of elements
   so that no synchronization between these needs to take place.
*/
#define TCONC_IN(tc, obj, pair) \
{ \
  SCM_SETCAR ((tc).tail, obj); \
  SCM_SETCAR (pair, SCM_BOOL_F); \
  SCM_SETCDR (pair, SCM_BOOL_F); \
  SCM_SETCDR ((tc).tail, pair); \
  (tc).tail = pair; \
} \

#define TCONC_OUT(tc, res) \
{ \
  (res) = SCM_CAR ((tc).head); \
  (tc).head = SCM_CDR ((tc).head); \
} \

#define TCONC_EMPTYP(tc) ((tc).head == (tc).tail)

typedef struct tconc_t
{
  SCM head;
  SCM tail;
} tconc_t;

typedef struct guardian_t
{
  tconc_t live;
  tconc_t zombies;
  struct guardian_t *next;
} guardian_t;

#define GUARDIAN(x) ((guardian_t *) SCM_CDR (x))
#define GUARDIAN_LIVE(x) (GUARDIAN (x)->live)
#define GUARDIAN_ZOMBIES(x) (GUARDIAN (x)->zombies)
#define GUARDIAN_NEXT(x) (GUARDIAN (x)->next)

static guardian_t *first_live_guardian = NULL;
static guardian_t **current_link_field = NULL;

static SCM
g_mark (SCM ptr)
{
  *current_link_field = GUARDIAN (ptr);
  current_link_field = &GUARDIAN_NEXT (ptr);
  GUARDIAN_NEXT (ptr) = NULL;

  /* Can't mark zombies here since they can refer to objects which are
     living dead, thereby preventing them to join the zombies. */
  return SCM_BOOL_F;
}

static int
g_print (SCM exp, SCM port, scm_print_state *pstate)
{
  char buf[256];
  sprintf (buf, "#<guardian live objs: %lu zombies: %lu>",
	   scm_ilength (SCM_CDR (GUARDIAN_LIVE (exp).head)),
	   scm_ilength (SCM_CDR (GUARDIAN_ZOMBIES (exp).head)));
  scm_puts (buf, port);

  return 1;
}

#define CCLO_G(cclo) (SCM_VELTS (cclo)[1])

static SCM
guard (SCM cclo, SCM arg)
{
  if (!SCM_UNBNDP (arg))
    {
      scm_guard (cclo, arg);
      return SCM_UNSPECIFIED;
    }
  else
    return scm_get_one_zombie (cclo);
}

static SCM guard1;

SCM_PROC (s_make_guardian, "make-guardian", 0, 0, 0, scm_make_guardian);
SCM
scm_make_guardian ()
{
  SCM cclo = scm_makcclo (guard1, 2L);
  guardian_t *g = (guardian_t *) scm_must_malloc (sizeof (guardian_t),
						  s_make_guardian);
  SCM z1 = scm_cons (SCM_BOOL_F, SCM_BOOL_F);
  SCM z2 = scm_cons (SCM_BOOL_F, SCM_BOOL_F);
  SCM z;
  /* A tconc starts out with one tail pair. */
  g->live.head = g->live.tail = z1;
  g->zombies.head = g->zombies.tail = z2;

  SCM_NEWSMOB (z, scm_tc16_guardian, g);

  CCLO_G (cclo) = z;

  return cclo;
}

void
scm_guardian_gc_init()
{
  current_link_field = &first_live_guardian;
  first_live_guardian = NULL;
}

void
scm_guardian_zombify ()
{
  guardian_t *g;

  /* Note that new guardians may be stuck on the end of the live
     guardian list as we run this loop.  As we move unmarked objects
     to the zombie list and mark them, we may find some guarded
     guardians.  The guardian mark function will stick them on the end
     of this list, so they'll be processed properly.  */
  for (g = first_live_guardian; g; g = g->next)
    {
      /* Scan the live list for unmarked objects, and move them to the
         zombies tconc.  */
      SCM tconc_tail = g->live.tail;
      SCM *prev_ptr = &g->live.head;
      SCM pair = g->live.head;

      while (pair != tconc_tail)
	{
	  SCM next_pair = SCM_CDR (pair);

	  if (SCM_NMARKEDP (SCM_CAR (pair)))
	    {
	      /* got you, zombie! */

	      /* out of the live list! */
	      *prev_ptr = next_pair;

	      /* to the zombie list! */
	      TCONC_IN (g->zombies, SCM_CAR (pair), pair);
	    }
	  else
	    prev_ptr = SCM_CDRLOC (pair);

	  pair = next_pair;
	}

      /* Mark the cells of the live list.  */
      for (pair = g->live.head; SCM_NIMP (pair); pair = SCM_GCCDR (pair))
	SCM_SETGCMARK (pair);

      /* Bring the zombies back from the dead.  */
      scm_gc_mark (g->zombies.head);
    }
}

void
scm_guard (SCM guardian, SCM obj)
{
  SCM g = CCLO_G (guardian);

  if (SCM_NIMP (obj))
    {
      SCM z;
      
      SCM_NEWCELL (z);

      /* This critical section barrier will be replaced by a mutex. */
      SCM_DEFER_INTS;
      TCONC_IN (GUARDIAN_LIVE (g), obj, z);
      SCM_ALLOW_INTS;
    }
}

SCM
scm_get_one_zombie (SCM guardian)
{
  SCM g = CCLO_G (guardian);
  SCM res = SCM_BOOL_F;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_DEFER_INTS;
  if (!TCONC_EMPTYP (GUARDIAN_ZOMBIES (g)))
    TCONC_OUT (GUARDIAN_ZOMBIES (g), res);
  SCM_ALLOW_INTS;

  return res;
}

void
scm_init_guardian()
{
  scm_tc16_guardian = scm_make_smob_type_mfpe ("guardian", sizeof (guardian_t),
                                              g_mark, NULL, g_print, NULL);
  guard1 = scm_make_subr_opt ("guardian", scm_tc7_subr_2o, guard, 0);

#include "guardians.x"
}
