/* classes: h_files */

#ifndef SMOBH
#define SMOBH
/*	Copyright (C) 1995, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
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

#include "libguile/__scm.h"
#include "libguile/print.h"


/* This is the internal representation of a smob type */

typedef struct scm_smob_descriptor
{
  char *name;
  scm_sizet size;

  /* Basic functions */
  SCM (*mark) (SCM);
  scm_sizet (*free) (SCM);
  int (*print) (SCM exp, SCM port, scm_print_state *pstate);
  SCM (*equalp) (SCM, SCM);

  /* Apply functions */
  SCM (*apply) ();
  SCM (*apply_0) (SCM);
  SCM (*apply_1) (SCM, SCM);
  SCM (*apply_2) (SCM, SCM, SCM);
  SCM (*apply_3) (SCM, SCM, SCM, SCM);
  int gsubr_type; /* Used in procprop.c */

  /* Dump functions */
  void (*dump) (SCM, SCM);
  SCM (*undump) (SCM);
} scm_smob_descriptor;



#define SCM_NEWSMOB(z, tc, data) \
do { \
  SCM_NEWCELL (z); \
  SCM_SET_CELL_WORD_1 ((z), (data)); \
  SCM_SET_CELL_TYPE ((z), (tc)); \
} while (0)

#define SCM_RETURN_NEWSMOB(tc, data) \
  do { SCM __SCM_smob_answer; \
       SCM_NEWSMOB (__SCM_smob_answer, (tc), (data)); \
       return __SCM_smob_answer; \
  } while (0)

#define SCM_NEWSMOB2(z, tc, data1, data2) \
do { \
  SCM_NEWCELL2 (z); \
  SCM_SET_CELL_WORD_1 ((z), (data1)); \
  SCM_SET_CELL_WORD_2 ((z), (data2)); \
  SCM_SET_CELL_TYPE ((z), (tc)); \
} while (0)

#define SCM_RETURN_NEWSMOB2(tc, data1, data2) \
  do { SCM __SCM_smob_answer; \
       SCM_NEWSMOB2 (__SCM_smob_answer, (tc), (data1), (data2)); \
       return __SCM_smob_answer; \
  } while (0)

#define SCM_NEWSMOB3(z, tc, data1, data2, data3) \
do { \
  SCM_NEWCELL2 (z); \
  SCM_SET_CELL_WORD_1 ((z), (data1)); \
  SCM_SET_CELL_WORD_2 ((z), (data2)); \
  SCM_SET_CELL_WORD_3 ((z), (data3)); \
  SCM_SET_CELL_TYPE ((z), (tc)); \
} while (0)

#define SCM_RETURN_NEWSMOB3(tc, data1, data2, data3) \
  do { SCM __SCM_smob_answer; \
       SCM_NEWSMOB3 (__SCM_smob_answer, (tc), (data1), (data2), (data3)); \
       return __SCM_smob_answer; \
  } while (0)


#define SCM_SMOB_DATA(x)		(SCM_CELL_WORD_1 (x))
#define SCM_SET_SMOB_DATA(x, data)	(SCM_SET_CELL_WORD_1 ((x), (data)))
#define SCM_TC2SMOBNUM(x)		(0x0ff & ((x) >> 8))
#define SCM_SMOBNUM(x)			(SCM_TC2SMOBNUM (SCM_CELL_TYPE (x)))
/* SCM_SMOBNAME can be 0 if name is missing */
#define SCM_SMOBNAME(smobnum)		(scm_smobs[smobnum].name)
#define SCM_SMOB_PREDICATE(tag, obj)	SCM_TYP16_PREDICATE (tag, obj)
#define SCM_SMOB_DESCRIPTOR(x)		(scm_smobs[SCM_SMOBNUM (x)])
#define SCM_SMOB_APPLICABLE_P(x)	(SCM_SMOB_DESCRIPTOR (x).apply)
#define SCM_SMOB_APPLY_0(x)		(SCM_SMOB_DESCRIPTOR (x).apply_0 (x))
#define SCM_SMOB_APPLY_1(x,a1)		(SCM_SMOB_DESCRIPTOR (x).apply_1 (x, (a1)))
#define SCM_SMOB_APPLY_2(x,a1,a2)	(SCM_SMOB_DESCRIPTOR (x).apply_2 (x, (a1), (a2)))
#define SCM_SMOB_APPLY_3(x,a1,a2,rst)	(SCM_SMOB_DESCRIPTOR (x).apply_3 (x, (a1), (a2), (rst)))

extern int scm_numsmob;
extern scm_smob_descriptor *scm_smobs;



extern SCM scm_mark0 (SCM ptr);
extern SCM scm_markcdr (SCM ptr);
extern scm_sizet scm_free0 (SCM ptr);
extern scm_sizet scm_smob_free (SCM obj);
extern int scm_smob_print (SCM exp, SCM port, scm_print_state *pstate);

/* The following set of functions is the standard way to create new
 * SMOB types.
 *
 * Create a type tag using `scm_make_smob_type', accept default values
 * for mark, free, print and/or equalp functions, or set your own
 * values using `scm_set_smob_xxx'.
 */

extern scm_bits_t scm_make_smob_type (char *name, scm_sizet size);

extern void scm_set_smob_mark (scm_bits_t tc, SCM (*mark) (SCM));
extern void scm_set_smob_free (scm_bits_t tc, scm_sizet (*free) (SCM));
extern void scm_set_smob_print (scm_bits_t tc,
				int (*print) (SCM, SCM, scm_print_state*));
extern void scm_set_smob_equalp (scm_bits_t tc, SCM (*equalp) (SCM, SCM));
extern void scm_set_smob_apply (scm_bits_t tc,
				SCM (*apply) (),
				unsigned int req,
				unsigned int opt,
				unsigned int rst);
extern void scm_set_smob_dump (scm_bits_t tc, void (*dump) (SCM, SCM));
extern void scm_set_smob_undump (scm_bits_t tc, SCM (*undump) (SCM));

/* Function for creating smobs */

extern SCM scm_make_smob (scm_bits_t tc);
extern void scm_smob_prehistory (void);



#if (SCM_DEBUG_DEPRECATED == 0)

extern long scm_make_smob_type_mfpe (char *name, scm_sizet size,
                                    SCM (*mark) (SCM),
                                    scm_sizet (*free) (SCM),
                                    int (*print) (SCM, SCM, scm_print_state*),
                                    SCM (*equalp) (SCM, SCM));

extern void scm_set_smob_mfpe (long tc, 
			       SCM (*mark) (SCM),
			       scm_sizet (*free) (SCM),
			       int (*print) (SCM, SCM, scm_print_state*),
			       SCM (*equalp) (SCM, SCM));

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

#endif  /* SMOBH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
