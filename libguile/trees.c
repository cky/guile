/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010
 * Free Software Foundation, Inc.
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
#include "libguile/eq.h"

#include "libguile/validate.h"
#include "libguile/list.h"
#include "libguile/vectors.h"
#include "libguile/srcprop.h"
#include "libguile/trees.h"

#include <stdarg.h>


/* scm_copy_tree creates deep copies of pairs and vectors, but not of any other
 * data types.
 *
 * To avoid infinite recursion due to cyclic structures, the hare-and-tortoise
 * pattern is used to detect cycles.  In fact, the pattern is used in two
 * dimensions, vertical (indicated in the code by the variable names 'hare'
 * and 'tortoise') and horizontal ('rabbit' and 'turtle').  In both
 * dimensions, the hare/rabbit will take two steps when the tortoise/turtle
 * takes one.
 *
 * The vertical dimension corresponds to recursive calls to function
 * copy_tree: This happens when descending into vector elements, into cars of
 * lists and into the cdr of an improper list.  In this dimension, the
 * tortoise follows the hare by using the processor stack: Every stack frame
 * will hold an instance of struct t_trace.  These instances are connected in
 * a way that represents the trace of the hare, which thus can be followed by
 * the tortoise.  The tortoise will always point to struct t_trace instances
 * relating to SCM objects that have already been copied.  Thus, a cycle is
 * detected if the tortoise and the hare point to the same object,
 *
 * The horizontal dimension is within one execution of copy_tree, when the
 * function cdr's along the pairs of a list.  This is the standard
 * hare-and-tortoise implementation, found several times in guile.  */

struct t_trace {
  struct t_trace *trace; /* These pointers form a trace along the stack. */
  SCM obj;               /* The object handled at the respective stack frame.*/
};

static SCM
copy_tree (struct t_trace *const hare,
           struct t_trace *tortoise,
           unsigned int tortoise_delay);

SCM_DEFINE (scm_copy_tree, "copy-tree", 1, 0, 0, 
            (SCM obj),
	    "Recursively copy the data tree that is bound to @var{obj}, and return a\n"
	    "the new data structure.  @code{copy-tree} recurses down the\n"
	    "contents of both pairs and vectors (since both cons cells and vector\n"
	    "cells may point to arbitrary objects), and stops recursing when it hits\n"
	    "any other object.")
#define FUNC_NAME s_scm_copy_tree
{
  /* Prepare the trace along the stack.  */
  struct t_trace trace;
  trace.obj = obj;

  /* In function copy_tree, if the tortoise makes its step, it will do this
   * before the hare has the chance to move.  Thus, we have to make sure that
   * the very first step of the tortoise will not happen after the hare has
   * really made two steps.  This is achieved by passing '2' as the initial
   * delay for the tortoise.  NOTE: Since cycles are unlikely, giving the hare
   * a bigger advantage may improve performance slightly.  */
  return copy_tree (&trace, &trace, 2);
}
#undef FUNC_NAME


static SCM
copy_tree (struct t_trace *const hare,
           struct t_trace *tortoise,
           unsigned int tortoise_delay)
#define FUNC_NAME s_scm_copy_tree
{
  if (!scm_is_pair (hare->obj) && !scm_is_simple_vector (hare->obj))
    {
      return hare->obj;
    }
  else
    {
      /* Prepare the trace along the stack.  */
      struct t_trace new_hare;
      hare->trace = &new_hare;

      /* The tortoise will make its step after the delay has elapsed.  Note
       * that in contrast to the typical hare-and-tortoise pattern, the step
       * of the tortoise happens before the hare takes its steps.  This is, in
       * principle, no problem, except for the start of the algorithm: Then,
       * it has to be made sure that the hare actually gets its advantage of
       * two steps.  */
      if (tortoise_delay == 0)
        {
          tortoise_delay = 1;
          tortoise = tortoise->trace;
          if (SCM_UNLIKELY (scm_is_eq (hare->obj, tortoise->obj)))
            scm_wrong_type_arg_msg (FUNC_NAME, 1, hare->obj,
                                    "expected non-circular data structure");
        }
      else
        {
          --tortoise_delay;
        }

      if (scm_is_simple_vector (hare->obj))
        {
          size_t length = SCM_SIMPLE_VECTOR_LENGTH (hare->obj);
          SCM new_vector = scm_c_make_vector (length, SCM_UNSPECIFIED);

          /* Each vector element is copied by recursing into copy_tree, having
           * the tortoise follow the hare into the depths of the stack.  */
          unsigned long int i;
          for (i = 0; i < length; ++i)
            {
              SCM new_element;
              new_hare.obj = SCM_SIMPLE_VECTOR_REF (hare->obj, i);
              new_element = copy_tree (&new_hare, tortoise, tortoise_delay);
              SCM_SIMPLE_VECTOR_SET (new_vector, i, new_element);
            }

          return new_vector;
        }
      else /* scm_is_pair (hare->obj) */
        {
          SCM result;
          SCM tail;

          SCM rabbit = hare->obj;
          SCM turtle = hare->obj;

          SCM copy;

          /* The first pair of the list is treated specially, in order to
           * preserve a potential source code position.  */
          result = tail = scm_cons_source (rabbit, SCM_EOL, SCM_EOL);
          new_hare.obj = SCM_CAR (rabbit);
          copy = copy_tree (&new_hare, tortoise, tortoise_delay);
          SCM_SETCAR (tail, copy);

          /* The remaining pairs of the list are copied by, horizontally,
           * having the turtle follow the rabbit, and, vertically, having the
           * tortoise follow the hare into the depths of the stack.  */
          rabbit = SCM_CDR (rabbit);
          while (scm_is_pair (rabbit))
            {
              new_hare.obj = SCM_CAR (rabbit);
              copy = copy_tree (&new_hare, tortoise, tortoise_delay);
              SCM_SETCDR (tail, scm_cons (copy, SCM_UNDEFINED));
              tail = SCM_CDR (tail);

              rabbit = SCM_CDR (rabbit);
              if (scm_is_pair (rabbit))
                {
                  new_hare.obj = SCM_CAR (rabbit);
                  copy = copy_tree (&new_hare, tortoise, tortoise_delay);
                  SCM_SETCDR (tail, scm_cons (copy, SCM_UNDEFINED));
                  tail = SCM_CDR (tail);
                  rabbit = SCM_CDR (rabbit);

                  turtle = SCM_CDR (turtle);
                  if (SCM_UNLIKELY (scm_is_eq (rabbit, turtle)))
                    scm_wrong_type_arg_msg (FUNC_NAME, 1, rabbit,
                                            "expected non-circular data structure");
                }
            }

          /* We have to recurse into copy_tree again for the last cdr, in
           * order to handle the situation that it holds a vector.  */
          new_hare.obj = rabbit;
          copy = copy_tree (&new_hare, tortoise, tortoise_delay);
          SCM_SETCDR (tail, copy);

          return result;
        }
    }
}
#undef FUNC_NAME


void
scm_init_trees ()
{
#include "libguile/trees.x"
}
