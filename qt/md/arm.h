/*
 * QuickThreads -- Threads-building toolkit.
 * Copyright (c) 1993 by David Keppel
 * Copyright (c) 2002 by Marius Vollmer
 *
 * Permission to use, copy, modify and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice and this notice
 * appear in all copies.  This software is provided as a
 * proof-of-concept and for demonstration purposes; there is no
 * representation about the suitability of this software for any
 * purpose.
 */

#ifndef QT_ARM_H
#define QT_ARM_H

typedef unsigned long qt_word_t;

#define QT_GROW_DOWN

/* Stack layout on the ARM:

   Callee-save registers are: r4-r11 (f4-f7)
   Also save r14, link register, and restore as pc.

   +---
   | lr/pc
   | r11
   | r10
   | r9
   | r8
   | r7
   | r6
   | r5
   | r4      <- sp of a suspended thread
   +---

   Startup:

   +---
   | only
   | user
   | argt
   | argu    <- sp on entry to qt_start
   +---
   | pc      == qt_start
   | r11
   | r10
   | r9
   | r8
   | r7
   | r6
   | r5
   | r4
   +---

*/

/* Stack must be word aligned. */
#define QT_STKALIGN	(4)	/* Doubleword aligned. */

/* How much space is allocated to hold all the crud for
   initialization: r4-r11, r14, and the four args for qt_start. */

#define QT_STKBASE	((9+4)*4)


/* Offsets of various registers, in words, relative to final value of SP. */
#define QT_LR           8
#define QT_11           7
#define QT_10           6
#define QT_9            5
#define QT_8            4
#define QT_7            3
#define QT_6            2
#define QT_5            1
#define QT_4            0


/* When a never-before-run thread is restored, the return pc points
   to a fragment of code that starts the thread running.  For
   non-vargs functions, it just calls the client's `only' function.
 */

extern void qt_start(void);
#define QT_ARGS_MD(sp)	(QT_SPUT (sp, QT_LR, qt_start))


/* The *index* (positive offset) of where to put each value. */
#define QT_ONLY_INDEX	(12)
#define QT_USER_INDEX	(11)
#define QT_ARGT_INDEX	(10)
#define QT_ARGU_INDEX	(9)

#endif /* ndef QT_ARM_H */
