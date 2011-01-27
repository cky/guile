/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <stdarg.h>

#include "libguile/_scm.h"
#include "libguile/gsubr.h"
#include "libguile/foreign.h"
#include "libguile/instructions.h"
#include "libguile/objcodes.h"
#include "libguile/srfi-4.h"
#include "libguile/programs.h"

#include "libguile/private-options.h"

/*
 * gsubr.c
 * Provide `gsubrs' -- subrs taking a prescribed number of required, optional,
 * and rest arguments.
 */

/* #define GSUBR_TEST */



/* OK here goes nothing: we're going to define VM assembly trampolines for
   invoking subrs, along with their meta-information, and then wrap them into
   statically allocated objcode values. Ready? Right!
*/

/* There's a maximum of 10 args, so the number of possible combinations is:
   (REQ-OPT-REST)
   for 0 args: 1 (000) (1 + 0)
   for 1 arg: 3 (100, 010, 001) (2 + 1)
   for 2 args: 5 (200, 110, 020, 101, 011) (3 + 2)
   for 3 args: 7 (300, 210, 120, 030, 201, 111, 021) (4 + 3)
   for N args: 2N+1

   and the index at which N args starts:
   for 0 args: 0
   for 1 args: 1
   for 2 args: 4
   for 3 args: 9
   for N args: N^2

   One can prove this:

   (1 + 3 + 5 + ... + (2N+1))
     = ((2N+1)+1)/2 * (N+1)
     = 2(N+1)/2 * (N+1)
     = (N+1)^2

   Thus the total sum is 11^2 = 121. Let's just generate all of them as
   read-only data.
*/

#ifdef WORDS_BIGENDIAN
#define OBJCODE_HEADER 0, 0, 0, 16, 0, 0, 0, 40
#define META_HEADER    0, 0, 0, 32, 0, 0, 0, 0
#else
#define OBJCODE_HEADER 16, 0, 0, 0, 40, 0, 0, 0
#define META_HEADER    32, 0, 0, 0, 0, 0, 0, 0
#endif

/* A: req; B: opt; C: rest */
#define A(nreq)                                                         \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_assert_nargs_ee, 0, nreq, /* assert number of args */  \
  /* 3 */ scm_op_object_ref, 0, /* push the foreign object wrapping the subr pointer */ \
  /* 5 */ scm_op_subr_call, nreq, /* and call (will return value as well) */ \
  /* 7 */ scm_op_nop,                                                   \
  /* 8 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,               \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (3, 7, nreq, 0, 0)

#define B(nopt)                                                         \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_bind_optionals, 0, nopt, /* bind optionals */          \
  /* 3 */ scm_op_assert_nargs_ee, 0, nopt, /* assert number of args */  \
  /* 6 */ scm_op_object_ref, 0, /* push the foreign object wrapping the subr pointer */ \
  /* 8 */ scm_op_subr_call, nopt, /* and call (will return value as well) */ \
  /* 10 */ scm_op_nop, scm_op_nop,                                      \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (6, 10, 0, nopt, 0)

#define C()                                                             \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_push_rest, 0, 0, /* cons all args into a list */       \
  /* 3 */ scm_op_object_ref, 0, /* push the foreign object wrapping the subr pointer */ \
  /* 5 */ scm_op_subr_call, 1, /* and call (will return value as well) */ \
  /* 7 */ scm_op_nop,                                                   \
  /* 8 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,               \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (3, 7, 0, 0, 1)

#define AB(nreq, nopt)                                                  \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_assert_nargs_ge, 0, nreq, /* assert number of args */  \
  /* 3 */ scm_op_bind_optionals, 0, nreq+nopt, /* bind optionals */     \
  /* 6 */ scm_op_assert_nargs_ee, 0, nreq+nopt, /* assert number of args */ \
  /* 9 */ scm_op_object_ref, 0, /* push the foreign object wrapping the subr pointer */ \
  /* 11 */ scm_op_subr_call, nreq+nopt, /* and call (will return value as well) */ \
  /* 13 */ scm_op_nop, scm_op_nop, scm_op_nop,                          \
  /* 16 */ META (9, 13, nreq, nopt, 0)

#define AC(nreq)                                                        \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_assert_nargs_ge, 0, nreq, /* assert number of args */  \
  /* 3 */ scm_op_push_rest, 0, nreq, /* cons rest list */               \
  /* 6 */ scm_op_object_ref, 0, /* push the foreign object wrapping the subr pointer */ \
  /* 8 */ scm_op_subr_call, nreq+1, /* and call (will return value as well) */ \
  /* 10 */ scm_op_nop, scm_op_nop,                                      \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (6, 10, nreq, 0, 1)

#define BC(nopt)                                                        \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_bind_optionals, 0, nopt, /* bind optionals */          \
  /* 3 */ scm_op_push_rest, 0, nopt, /* cons rest list */               \
  /* 6 */ scm_op_object_ref, 0, /* push the foreign object wrapping the subr pointer */ \
  /* 8 */ scm_op_subr_call, nopt+1, /* and call (will return value as well) */ \
  /* 10 */ scm_op_nop, scm_op_nop,                                      \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (6, 10, 0, nopt, 1)

#define ABC(nreq, nopt)                                                 \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_assert_nargs_ge, 0, nreq, /* assert number of args */  \
  /* 3 */ scm_op_bind_optionals, 0, nreq+nopt, /* bind optionals */     \
  /* 6 */ scm_op_push_rest, 0, nreq+nopt, /* cons rest list */          \
  /* 9 */ scm_op_object_ref, 0, /* push the foreign object wrapping the subr pointer */ \
  /* 11 */ scm_op_subr_call, nreq+nopt+1, /* and call (will return value as well) */ \
  /* 13 */ scm_op_nop, scm_op_nop, scm_op_nop,                          \
  /* 16 */ META (9, 13, nreq, nopt, 1)
  
#define META(start, end, nreq, nopt, rest)                              \
  META_HEADER,                                                          \
  /* 0 */ scm_op_make_eol, /* bindings */                               \
  /* 1 */ scm_op_make_eol, /* sources */                                \
  /* 2 */ scm_op_make_int8, start, scm_op_make_int8, end, /* arity: from ip N to ip N */ \
  /* 6 */ scm_op_make_int8, nreq, /* the arity is N required args */    \
  /* 8 */ scm_op_make_int8, nopt, /* N optionals */                     \
  /* 10 */ rest ? scm_op_make_true : scm_op_make_false, /* maybe a rest arg */ \
  /* 11 */ scm_op_list, 0, 5, /* make a list of those 5 vals */         \
  /* 14 */ scm_op_list, 0, 1, /* and the arities will be a list of that one list */ \
  /* 17 */ scm_op_load_symbol, 0, 0, 4, 'n', 'a', 'm', 'e', /* `name' */ \
  /* 25 */ scm_op_object_ref, 1, /* the name from the object table */   \
  /* 27 */ scm_op_cons, /* make a pair for the properties */            \
  /* 28 */ scm_op_list, 0, 4, /* pack bindings, sources, and arities into list */ \
  /* 31 */ scm_op_return /* and return */                               \
  /* 32 */

/*
 (defun generate-bytecode (n)
   "Generate bytecode for N arguments"
   (interactive "p")
   (insert (format "/\* %d arguments *\/\n  " n))
   (let ((nreq n))
     (while (<= 0 nreq)
       (let ((nopt (- n nreq)))
         (insert
          (if (< 0 nreq)
              (if (< 0 nopt)
                  (format "AB(%d,%d), " nreq nopt)
                  (format "A(%d), " nreq))
              (if (< 0 nopt)
                  (format "B(%d), " nopt)
                  (format "A(0), "))))
         (setq nreq (1- nreq))))
     (insert "\n  ")
     (setq nreq (1- n))
     (while (<= 0 nreq)
       (let ((nopt (- n nreq 1)))
         (insert
          (if (< 0 nreq)
              (if (< 0 nopt)
                  (format "ABC(%d,%d), " nreq nopt)
                  (format "AC(%d), " nreq))
              (if (< 0 nopt)
                  (format "BC(%d), " nopt)
                  (format "C(), "))))
         (setq nreq (1- nreq))))
     (insert "\n\n  ")))

 (defun generate-bytecodes (n)
   "Generate bytecodes for up to N arguments"
   (interactive "p")
   (let ((i 0))
     (while (<= i n)
       (generate-bytecode i)
       (setq i (1+ i)))))
*/
static const struct
{
  scm_t_uint64 dummy; /* ensure 8-byte alignment; perhaps there's a better way */
  const scm_t_uint8 bytes[121 * (sizeof (struct scm_objcode) + 16
                                 + sizeof (struct scm_objcode) + 32)];
} raw_bytecode = {
  0,
  {
    /* C-u 1 0 M-x generate-bytecodes RET */
    /* 0 arguments */
    A(0), 
  
    /* 1 arguments */
    A(1), B(1), 
    C(), 

    /* 2 arguments */
    A(2), AB(1,1), B(2), 
    AC(1), BC(1), 

    /* 3 arguments */
    A(3), AB(2,1), AB(1,2), B(3), 
    AC(2), ABC(1,1), BC(2), 

    /* 4 arguments */
    A(4), AB(3,1), AB(2,2), AB(1,3), B(4), 
    AC(3), ABC(2,1), ABC(1,2), BC(3), 

    /* 5 arguments */
    A(5), AB(4,1), AB(3,2), AB(2,3), AB(1,4), B(5), 
    AC(4), ABC(3,1), ABC(2,2), ABC(1,3), BC(4), 

    /* 6 arguments */
    A(6), AB(5,1), AB(4,2), AB(3,3), AB(2,4), AB(1,5), B(6), 
    AC(5), ABC(4,1), ABC(3,2), ABC(2,3), ABC(1,4), BC(5), 

    /* 7 arguments */
    A(7), AB(6,1), AB(5,2), AB(4,3), AB(3,4), AB(2,5), AB(1,6), B(7), 
    AC(6), ABC(5,1), ABC(4,2), ABC(3,3), ABC(2,4), ABC(1,5), BC(6), 

    /* 8 arguments */
    A(8), AB(7,1), AB(6,2), AB(5,3), AB(4,4), AB(3,5), AB(2,6), AB(1,7), B(8), 
    AC(7), ABC(6,1), ABC(5,2), ABC(4,3), ABC(3,4), ABC(2,5), ABC(1,6), BC(7), 

    /* 9 arguments */
    A(9), AB(8,1), AB(7,2), AB(6,3), AB(5,4), AB(4,5), AB(3,6), AB(2,7), AB(1,8), B(9), 
    AC(8), ABC(7,1), ABC(6,2), ABC(5,3), ABC(4,4), ABC(3,5), ABC(2,6), ABC(1,7), BC(8), 

    /* 10 arguments */
    A(10), AB(9,1), AB(8,2), AB(7,3), AB(6,4), AB(5,5), AB(4,6), AB(3,7), AB(2,8), AB(1,9), B(10), 
    AC(9), ABC(8,1), ABC(7,2), ABC(6,3), ABC(5,4), ABC(4,5), ABC(3,6), ABC(2,7), ABC(1,8), BC(9)
  }
};

#undef A
#undef B
#undef C
#undef AB
#undef AC
#undef BC
#undef ABC
#undef OBJCODE_HEADER
#undef META_HEADER
#undef META

/*
 ;; (nargs * nargs) + nopt + rest * (nargs + 1)
 (defun generate-objcode-cells-helper (n)
   "Generate objcode cells for N arguments"
   (interactive "p")
   (insert (format "    /\* %d arguments *\/\n" n))
   (let ((nreq n))
     (while (<= 0 nreq)
       (let ((nopt (- n nreq)))
         (insert
          (format "    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + %d) },\n"
                  (* (+ 4 4 16 4 4 32)
                     (+ (* n n) nopt))))
         (insert "    { SCM_BOOL_F, SCM_PACK (0) },\n")
         (setq nreq (1- nreq))))
     (insert "\n")
     (setq nreq (1- n))
     (while (<= 0 nreq)
       (let ((nopt (- n nreq 1)))
         (insert
          (format "    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + %d) },\n"
                  (* (+ 4 4 16 4 4 32)
                     (+ (* n n) nopt n 1))))
         (insert "    { SCM_BOOL_F, SCM_PACK (0) },\n")
         (setq nreq (1- nreq))))
     (insert "\n")))

 (defun generate-objcode-cells (n)
   "Generate objcode cells for up to N arguments"
   (interactive "p")
   (let ((i 0))
     (while (<= i n)
       (generate-objcode-cells-helper i)
       (setq i (1+ i)))))
*/

#define STATIC_OBJCODE_TAG                                      \
  SCM_PACK (SCM_MAKE_OBJCODE_TAG (SCM_OBJCODE_TYPE_STATIC, 0))

static const struct
{
  scm_t_uint64 dummy; /* alignment */
  scm_t_cell cells[121 * 2]; /* 11*11 double cells */
} objcode_cells = {
  0,
  /* C-u 1 0 M-x generate-objcode-cells RET */
  {
    /* 0 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 0) },
    { SCM_BOOL_F, SCM_PACK (0) },


    /* 1 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 64) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 128) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 192) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 2 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 256) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 320) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 384) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 448) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 512) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 3 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 576) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 640) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 704) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 768) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 832) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 896) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 960) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 4 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1024) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1088) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1152) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1216) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1280) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1344) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1408) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1472) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1536) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 5 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1600) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1664) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1728) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1792) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1856) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1920) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 1984) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2048) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2112) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2176) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2240) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 6 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2304) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2368) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2432) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2496) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2560) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2624) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2688) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2752) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2816) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2880) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 2944) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3008) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3072) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 7 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3136) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3200) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3264) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3328) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3392) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3456) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3520) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3584) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3648) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3712) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3776) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3840) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3904) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 3968) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4032) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 8 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4096) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4160) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4224) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4288) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4352) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4416) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4480) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4544) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4608) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4672) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4736) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4800) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4864) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4928) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 4992) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5056) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5120) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 9 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5184) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5248) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5312) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5376) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5440) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5504) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5568) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5632) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5696) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5760) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5824) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5888) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 5952) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6016) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6080) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6144) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6208) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6272) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6336) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 10 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6400) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6464) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6528) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6592) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6656) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6720) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6784) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6848) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6912) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 6976) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7040) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7104) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7168) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7232) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7296) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7360) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7424) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7488) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7552) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7616) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 7680) },
    { SCM_BOOL_F, SCM_PACK (0) }
  }
};
  
/*
 (defun generate-objcode (n)
   "Generate objcode for N arguments"
   (interactive "p")
   (insert (format "  /\* %d arguments *\/\n" n))
   (let ((i (* n n)))
     (while (< i (* (1+ n) (1+ n)))
       (insert (format "  SCM_PACK (objcode_cells.cells+%d),\n" (* i 2)))
       (setq i (1+ i)))
     (insert "\n")))

 (defun generate-objcodes (n)
   "Generate objcodes for up to N arguments"
   (interactive "p")
   (let ((i 0))
     (while (<= i n)
       (generate-objcode i)
       (setq i (1+ i)))))
*/
static const SCM scm_subr_objcode_trampolines[121] = {
  /* C-u 1 0 M-x generate-objcodes RET */
  /* 0 arguments */
  SCM_PACK (objcode_cells.cells+0),

  /* 1 arguments */
  SCM_PACK (objcode_cells.cells+2),
  SCM_PACK (objcode_cells.cells+4),
  SCM_PACK (objcode_cells.cells+6),

  /* 2 arguments */
  SCM_PACK (objcode_cells.cells+8),
  SCM_PACK (objcode_cells.cells+10),
  SCM_PACK (objcode_cells.cells+12),
  SCM_PACK (objcode_cells.cells+14),
  SCM_PACK (objcode_cells.cells+16),

  /* 3 arguments */
  SCM_PACK (objcode_cells.cells+18),
  SCM_PACK (objcode_cells.cells+20),
  SCM_PACK (objcode_cells.cells+22),
  SCM_PACK (objcode_cells.cells+24),
  SCM_PACK (objcode_cells.cells+26),
  SCM_PACK (objcode_cells.cells+28),
  SCM_PACK (objcode_cells.cells+30),

  /* 4 arguments */
  SCM_PACK (objcode_cells.cells+32),
  SCM_PACK (objcode_cells.cells+34),
  SCM_PACK (objcode_cells.cells+36),
  SCM_PACK (objcode_cells.cells+38),
  SCM_PACK (objcode_cells.cells+40),
  SCM_PACK (objcode_cells.cells+42),
  SCM_PACK (objcode_cells.cells+44),
  SCM_PACK (objcode_cells.cells+46),
  SCM_PACK (objcode_cells.cells+48),

  /* 5 arguments */
  SCM_PACK (objcode_cells.cells+50),
  SCM_PACK (objcode_cells.cells+52),
  SCM_PACK (objcode_cells.cells+54),
  SCM_PACK (objcode_cells.cells+56),
  SCM_PACK (objcode_cells.cells+58),
  SCM_PACK (objcode_cells.cells+60),
  SCM_PACK (objcode_cells.cells+62),
  SCM_PACK (objcode_cells.cells+64),
  SCM_PACK (objcode_cells.cells+66),
  SCM_PACK (objcode_cells.cells+68),
  SCM_PACK (objcode_cells.cells+70),

  /* 6 arguments */
  SCM_PACK (objcode_cells.cells+72),
  SCM_PACK (objcode_cells.cells+74),
  SCM_PACK (objcode_cells.cells+76),
  SCM_PACK (objcode_cells.cells+78),
  SCM_PACK (objcode_cells.cells+80),
  SCM_PACK (objcode_cells.cells+82),
  SCM_PACK (objcode_cells.cells+84),
  SCM_PACK (objcode_cells.cells+86),
  SCM_PACK (objcode_cells.cells+88),
  SCM_PACK (objcode_cells.cells+90),
  SCM_PACK (objcode_cells.cells+92),
  SCM_PACK (objcode_cells.cells+94),
  SCM_PACK (objcode_cells.cells+96),

  /* 7 arguments */
  SCM_PACK (objcode_cells.cells+98),
  SCM_PACK (objcode_cells.cells+100),
  SCM_PACK (objcode_cells.cells+102),
  SCM_PACK (objcode_cells.cells+104),
  SCM_PACK (objcode_cells.cells+106),
  SCM_PACK (objcode_cells.cells+108),
  SCM_PACK (objcode_cells.cells+110),
  SCM_PACK (objcode_cells.cells+112),
  SCM_PACK (objcode_cells.cells+114),
  SCM_PACK (objcode_cells.cells+116),
  SCM_PACK (objcode_cells.cells+118),
  SCM_PACK (objcode_cells.cells+120),
  SCM_PACK (objcode_cells.cells+122),
  SCM_PACK (objcode_cells.cells+124),
  SCM_PACK (objcode_cells.cells+126),

  /* 8 arguments */
  SCM_PACK (objcode_cells.cells+128),
  SCM_PACK (objcode_cells.cells+130),
  SCM_PACK (objcode_cells.cells+132),
  SCM_PACK (objcode_cells.cells+134),
  SCM_PACK (objcode_cells.cells+136),
  SCM_PACK (objcode_cells.cells+138),
  SCM_PACK (objcode_cells.cells+140),
  SCM_PACK (objcode_cells.cells+142),
  SCM_PACK (objcode_cells.cells+144),
  SCM_PACK (objcode_cells.cells+146),
  SCM_PACK (objcode_cells.cells+148),
  SCM_PACK (objcode_cells.cells+150),
  SCM_PACK (objcode_cells.cells+152),
  SCM_PACK (objcode_cells.cells+154),
  SCM_PACK (objcode_cells.cells+156),
  SCM_PACK (objcode_cells.cells+158),
  SCM_PACK (objcode_cells.cells+160),

  /* 9 arguments */
  SCM_PACK (objcode_cells.cells+162),
  SCM_PACK (objcode_cells.cells+164),
  SCM_PACK (objcode_cells.cells+166),
  SCM_PACK (objcode_cells.cells+168),
  SCM_PACK (objcode_cells.cells+170),
  SCM_PACK (objcode_cells.cells+172),
  SCM_PACK (objcode_cells.cells+174),
  SCM_PACK (objcode_cells.cells+176),
  SCM_PACK (objcode_cells.cells+178),
  SCM_PACK (objcode_cells.cells+180),
  SCM_PACK (objcode_cells.cells+182),
  SCM_PACK (objcode_cells.cells+184),
  SCM_PACK (objcode_cells.cells+186),
  SCM_PACK (objcode_cells.cells+188),
  SCM_PACK (objcode_cells.cells+190),
  SCM_PACK (objcode_cells.cells+192),
  SCM_PACK (objcode_cells.cells+194),
  SCM_PACK (objcode_cells.cells+196),
  SCM_PACK (objcode_cells.cells+198),

  /* 10 arguments */
  SCM_PACK (objcode_cells.cells+200),
  SCM_PACK (objcode_cells.cells+202),
  SCM_PACK (objcode_cells.cells+204),
  SCM_PACK (objcode_cells.cells+206),
  SCM_PACK (objcode_cells.cells+208),
  SCM_PACK (objcode_cells.cells+210),
  SCM_PACK (objcode_cells.cells+212),
  SCM_PACK (objcode_cells.cells+214),
  SCM_PACK (objcode_cells.cells+216),
  SCM_PACK (objcode_cells.cells+218),
  SCM_PACK (objcode_cells.cells+220),
  SCM_PACK (objcode_cells.cells+222),
  SCM_PACK (objcode_cells.cells+224),
  SCM_PACK (objcode_cells.cells+226),
  SCM_PACK (objcode_cells.cells+228),
  SCM_PACK (objcode_cells.cells+230),
  SCM_PACK (objcode_cells.cells+232),
  SCM_PACK (objcode_cells.cells+234),
  SCM_PACK (objcode_cells.cells+236),
  SCM_PACK (objcode_cells.cells+238),
  SCM_PACK (objcode_cells.cells+240)
};

/* (nargs * nargs) + nopt + rest * (nargs + 1) */
#define SCM_SUBR_OBJCODE_TRAMPOLINE(nreq,nopt,rest)                     \
  scm_subr_objcode_trampolines[(nreq + nopt + rest) * (nreq + nopt + rest) \
                               + nopt + rest * (nreq + nopt + rest + 1)]

SCM
scm_subr_objcode_trampoline (unsigned int nreq, unsigned int nopt,
                             unsigned int rest)
{
  if (SCM_UNLIKELY (rest > 1 || nreq + nopt + rest > 10))
    scm_out_of_range ("make-subr", scm_from_uint (nreq + nopt + rest));
      
  return SCM_SUBR_OBJCODE_TRAMPOLINE (nreq, nopt, rest);
}

static SCM
create_gsubr (int define, const char *name,
	      unsigned int nreq, unsigned int nopt, unsigned int rest,
	      SCM (*fcn) (), SCM *generic_loc)
{
  SCM ret;
  SCM sname;
  SCM table;
  scm_t_bits flags;

  /* make objtable */
  sname = scm_from_locale_symbol (name);
  table = scm_c_make_vector (generic_loc ? 3 : 2, SCM_UNDEFINED);
  SCM_SIMPLE_VECTOR_SET (table, 0, scm_from_pointer (fcn, NULL));
  SCM_SIMPLE_VECTOR_SET (table, 1, sname);
  if (generic_loc)
    SCM_SIMPLE_VECTOR_SET (table, 2,
                           scm_from_pointer (generic_loc, NULL));

  /* make program */
  ret = scm_make_program (scm_subr_objcode_trampoline (nreq, nopt, rest),
                          table, SCM_BOOL_F);

  /* set flags */
  flags = SCM_F_PROGRAM_IS_PRIMITIVE;
  flags |= generic_loc ? SCM_F_PROGRAM_IS_PRIMITIVE_GENERIC : 0;
  SCM_SET_CELL_WORD_0 (ret, SCM_CELL_WORD_0 (ret) | flags);

  /* define, if needed */
  if (define)
    scm_define (sname, ret);

  /* et voila. */
  return ret;
}

SCM
scm_c_make_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  return create_gsubr (0, name, req, opt, rst, fcn, NULL);
}

SCM
scm_c_define_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  return create_gsubr (1, name, req, opt, rst, fcn, NULL);
}

SCM
scm_c_make_gsubr_with_generic (const char *name,
			       int req,
			       int opt,
			       int rst,
			       SCM (*fcn)(),
			       SCM *gf)
{
  return create_gsubr (0, name, req, opt, rst, fcn, gf);
}

SCM
scm_c_define_gsubr_with_generic (const char *name,
				 int req,
				 int opt,
				 int rst,
				 SCM (*fcn)(),
				 SCM *gf)
{
  return create_gsubr (1, name, req, opt, rst, fcn, gf);
}


#ifdef GSUBR_TEST
/* A silly example, taking 2 required args, 1 optional, and
   a scm_list of rest args
   */
SCM
gsubr_21l(SCM req1, SCM req2, SCM opt, SCM rst)
{
  scm_puts ("gsubr-2-1-l:\n req1: ", scm_cur_outp);
  scm_display(req1, scm_cur_outp);
  scm_puts ("\n req2: ", scm_cur_outp);
  scm_display(req2, scm_cur_outp);
  scm_puts ("\n opt: ", scm_cur_outp);
  scm_display(opt, scm_cur_outp);
  scm_puts ("\n rest: ", scm_cur_outp);
  scm_display(rst, scm_cur_outp);
  scm_newline(scm_cur_outp);
  return SCM_UNSPECIFIED;
}
#endif


void
scm_init_gsubr()
{
#ifdef GSUBR_TEST
  scm_c_define_gsubr ("gsubr-2-1-l", 2, 1, 1, gsubr_21l); /* example */
#endif

#include "libguile/gsubr.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
