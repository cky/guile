/* classes: h_files */

#ifndef SMOBH
#define SMOBH
/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */

#include "libguile/__scm.h"


typedef struct scm_smobfuns
{
  SCM (*mark) SCM_P ((SCM));
  scm_sizet (*free) SCM_P ((SCM));
  int (*print) SCM_P ((SCM exp, SCM port, int writing));
  SCM (*equalp) SCM_P ((SCM, SCM));
} scm_smobfuns;



#define SCM_SMOBNUM(x) (0x0ff & (SCM_CAR(x)>>8));
#define SCM_PTOBNUM(x) (0x0ff & (SCM_CAR(x)>>8));

extern scm_sizet scm_numsmob;
extern scm_smobfuns *scm_smobs;



/* Everyone who uses smobs needs to print.  */
#include "libguile/ports.h"
#include "libguile/genio.h"
#include "libguile/print.h"

/* ... and they all need to GC.  */
#include "libguile/markers.h"


extern long scm_newsmob SCM_P ((scm_smobfuns *smob));
extern void scm_smob_prehistory SCM_P ((void));

#endif  /* SMOBH */
