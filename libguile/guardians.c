/*	Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
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



/* This is an implementation of guardians as described in
 * R. Kent Dybvig, Carl Bruggeman, and David Eby (1993) "Guardians in
 * a Generation-Based Garbage Collector" ACM SIGPLAN Conference on
 * Programming Language Design and Implementation, June 1993
 * ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/guardians.ps.gz
 *
 * Author:      Michael N. Livshin
 * Modified by: Mikael Djurfeldt
 */


#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/guardians.h"


/* The live and zombies FIFOs are implemented as tconcs as described
   in Dybvig's paper.  This decouples addition and removal of elements
   so that no synchronization between these needs to take place.
*/

typedef struct tconc_t
{
  SCM head;
  SCM tail;
} tconc_t;

#define TCONC_EMPTYP(tc) (SCM_EQ_P ((tc).head, (tc).tail))

#define TCONC_IN(tc, obj, pair) \
do { \
  SCM_SETCAR ((tc).tail, obj); \
  SCM_SETCAR (pair, SCM_BOOL_F); \
  SCM_SETCDR (pair, SCM_EOL); \
  SCM_SETCDR ((tc).tail, pair); \
  (tc).tail = pair; \
} while (0)

#define TCONC_OUT(tc, res) \
do { \
  (res) = SCM_CAR ((tc).head); \
  (tc).head = SCM_CDR ((tc).head); \
} while (0)


static scm_bits_t tc16_guardian;

typedef struct guardian_t
{
  tconc_t live;
  tconc_t zombies;
  struct guardian_t *next;
} guardian_t;

#define GUARDIAN(x) ((guardian_t *) SCM_CELL_WORD_1 (x))
#define GUARDIAN_LIVE(x) (GUARDIAN (x)->live)
#define GUARDIAN_ZOMBIES(x) (GUARDIAN (x)->zombies)
#define GUARDIAN_NEXT(x) (GUARDIAN (x)->next)


/* during the gc mark phase, live guardians are linked into a list here. */
static guardian_t *first_live_guardian = NULL;
static guardian_t **current_link_field = NULL;


/* mark a guardian by adding it to the live guardian list.  */
static SCM
guardian_mark (SCM ptr)
{
  *current_link_field = GUARDIAN (ptr);
  current_link_field = &GUARDIAN_NEXT (ptr);
  GUARDIAN_NEXT (ptr) = NULL;

  /* the objects protected by the guardian are not marked here: that
     would prevent them from ever getting collected.  instead marking
     is done at the end of the mark phase by scm_guardian_zombify.  */
  return SCM_BOOL_F;
}


static scm_sizet
guardian_free (SCM ptr)
{
  scm_must_free (GUARDIAN (ptr));
  return sizeof (guardian_t);
}


static int
guardian_print (SCM g, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<guardian live objs: ", port);
  scm_display (scm_length (SCM_CDR (GUARDIAN_LIVE (g).head)), port);
  scm_puts (" zombies: ", port);
  scm_display (scm_length (SCM_CDR (GUARDIAN_ZOMBIES (g).head)), port);
  scm_puts (">", port);

  return 1;
}


/* This is the Scheme entry point for each guardian: If arg is an object, it's
 * added to the guardian's live list.  If arg is unbound, the next available
 * zombified object (or #f if none) is returned.
 */ 
static SCM
guardian_apply (SCM guardian, SCM arg)
{
  if (!SCM_UNBNDP (arg))
    {
      scm_guard (guardian, arg);
      return SCM_UNSPECIFIED;
    }
  else
    return scm_get_one_zombie (guardian);
}


void
scm_guard (SCM guardian, SCM obj)
{
  if (!SCM_IMP (obj))
    {
      SCM z;

      SCM_NEWCELL (z);

      /* This critical section barrier will be replaced by a mutex. */
      SCM_DEFER_INTS;
      TCONC_IN (GUARDIAN_LIVE (guardian), obj, z);
      SCM_ALLOW_INTS;
    }
}


SCM
scm_get_one_zombie (SCM guardian)
{
  SCM res = SCM_BOOL_F;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_DEFER_INTS;
  if (!TCONC_EMPTYP (GUARDIAN_ZOMBIES (guardian)))
    TCONC_OUT (GUARDIAN_ZOMBIES (guardian), res);
  SCM_ALLOW_INTS;
  return res;
}


SCM_DEFINE (scm_make_guardian, "make-guardian", 0, 0, 0, 
            (),
            "Create a new guardian.\n"
	    "A guardian protects a set of objects from garbage collection,\n"
	    "allowing a program to apply cleanup or other actions.\n\n"

	    "make-guardian returns a procedure representing the guardian.\n"
	    "Calling the guardian procedure with an argument adds the\n"
	    "argument to the guardian's set of protected objects.\n"
	    "Calling the guardian procedure without an argument returns\n"
	    "one of the protected objects which are ready for garbage\n"
	    "collection or @code{#f} if no such object is available.\n"
	    "Objects which are returned in this way are removed from\n"
	    "the guardian.\n\n"

            "See R. Kent Dybvig, Carl Bruggeman, and David Eby (1993)\n"
            "\"Guardians in a Generation-Based Garbage Collector\".\n"
            "ACM SIGPLAN Conference on Programming Language Design\n"
            "and Implementation, June 1993.")
#define FUNC_NAME s_scm_make_guardian
{
  guardian_t *g = SCM_MUST_MALLOC_TYPE (guardian_t);
  SCM z1 = scm_cons (SCM_BOOL_F, SCM_EOL);
  SCM z2 = scm_cons (SCM_BOOL_F, SCM_EOL);
  SCM z;

  /* A tconc starts out with one tail pair. */
  g->live.head = g->live.tail = z1;
  g->zombies.head = g->zombies.tail = z2;

  SCM_NEWSMOB (z, tc16_guardian, g);

  return z;
}
#undef FUNC_NAME


/* called before gc mark phase begins to initialise the live guardian list. */
static void *
guardian_gc_init (void *dummy1, void *dummy2, void *dummy3)
{
  current_link_field = &first_live_guardian;
  first_live_guardian = NULL;

  return 0;
}


/* this is called by the garbage collector between the mark and sweep
   phases.  for each marked guardian, it moves any unmarked object in
   its live list (tconc) to its zombie list (tconc).  */
static void *
guardian_zombify (void *dummy1, void *dummy2, void *dummy3)
{
  guardian_t *first_guardian;
  guardian_t **link_field = &first_live_guardian;

  /* Note that new guardians may be stuck on the end of the live
     guardian list as we run this loop.  As we move unmarked objects
     to the zombie list and mark them, we may find some guarded
     guardians.  The guardian mark function will stick them on the end
     of this list, so they'll be processed properly.  */

  do {
    guardian_t *g;
    
    first_guardian = *link_field;
    link_field = current_link_field;

    /* first, scan all the guardians that are currently known to be live
       and move their unmarked objects to zombie lists. */

    for (g = first_guardian; g; g = g->next)
      {
        SCM tconc_tail = g->live.tail;
        SCM *prev_ptr = &g->live.head;
        SCM pair = g->live.head;

        while (! SCM_EQ_P (pair, tconc_tail))
          {
            SCM next_pair = SCM_CDR (pair);

            if (SCM_NMARKEDP (SCM_CAR (pair)))
              {
                /* got you, zombie! */

                /* out of the live list! */
                *prev_ptr = next_pair;

                /* into the zombie list! */
                TCONC_IN (g->zombies, SCM_CAR (pair), pair);
              }
            else
              prev_ptr = SCM_CDRLOC (pair);

            pair = next_pair;
          }

        /* Mark the cells of the live list (yes, the cells in the list,
           even though we don't care about objects pointed to by the list
           cars, since we know they are already marked).  */
        for (pair = g->live.head; !SCM_NULLP (pair); pair = SCM_CDR (pair))
          SCM_SETGCMARK (pair);
      }

    /* ghouston: Doesn't it seem a bit disturbing that if a zombie
       is returned to full life after getting returned from the
       guardian procedure, it may reference objects which are in a
       guardian's zombie list?  Is it not necessary to move such
       zombies back to the live list, to avoid allowing the
       guardian procedure to return an object which is referenced,
       so not collectable?  The paper doesn't give this
       impression.

       cmm: the paper does explicitly say that an object that is    
       guarded more than once should be returned more than once.    
       I believe this covers the above scenario. */

    /* Preserve the zombies in their undead state, by marking to    
       prevent collection.  Note that this may uncover zombified    
       guardians -- if so, they'll be processed in the next loop. */
    for (g = first_guardian; g != *link_field; g = g->next)
      scm_gc_mark (g->zombies.head);
  } while (current_link_field != link_field);
  
  return 0;
}


void
scm_init_guardian()
{
  tc16_guardian = scm_make_smob_type ("guardian", 0);
  scm_set_smob_mark (tc16_guardian, guardian_mark);
  scm_set_smob_free (tc16_guardian, guardian_free);
  scm_set_smob_print (tc16_guardian, guardian_print);
  scm_set_smob_apply (tc16_guardian, guardian_apply, 0, 1, 0);

  scm_c_hook_add (&scm_before_mark_c_hook, guardian_gc_init, 0, 0);
  scm_c_hook_add (&scm_before_sweep_c_hook, guardian_zombify, 0, 0);

#ifndef SCM_MAGIC_SNARFER
#include "libguile/guardians.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
