/* classes: h_files */

#ifndef PORTSH
#define PORTSH
/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
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
#include "libguile/struct.h"



#define SCM_INITIAL_CBUF_SIZE 4

struct scm_port_table 
{
  SCM port;			/* Open port.  */
  int entry;			/* Index in port table. */
  int revealed;			/* 0 not revealed, > 1 revealed.
				 * Revealed ports do not get GC'd.
				 */

  SCM stream;

  SCM file_name;		/* debugging support.  */
  int line_number;		/* debugging support.  */
  int column_number;		/* debugging support.  */

  char *cp;			/* where to put and get unget chars */
  char *cbufend;		/* points after this struct */
  char cbuf[SCM_INITIAL_CBUF_SIZE]; /* must be last: may grow */
};

extern struct scm_port_table **scm_port_table;
extern int scm_port_table_size; /* Number of ports in scm_port_table.  */




#define SCM_EOF_OBJECT_P(x) ((x) == SCM_EOF_VAL)

/* PORT FLAGS
 * A set of flags characterizes a port.
 * Note that we reserve the bits 1 << 24 and above for use by the
 * routines in the port's scm_ptobfuns structure.
 */
#define SCM_OPN		(1L<<16) /* Is the port open? */
#define SCM_RDNG	(2L<<16) /* Is it a readable port? */
#define SCM_WRTNG	(4L<<16) /* Is it writable? */
#define SCM_BUF0	(8L<<16)
#define SCM_NOFTELL     (16L<<16) /* Does ftell work on this?  Yuck!  */
#define SCM_CRDY	(32L<<16) /* Should char-ready? return #t? */

/* A mask used to clear the char-ready port flag. */
#define SCM_CUC		(~SCM_CRDY)

#define SCM_PORTP(x) (SCM_TYP7(x)==scm_tc7_port)
#define SCM_OPPORTP(x) (((0x7f | SCM_OPN) & SCM_CAR(x))==(scm_tc7_port | SCM_OPN))
#define SCM_OPINPORTP(x) (((0x7f | SCM_OPN | SCM_RDNG) & SCM_CAR(x))==(scm_tc7_port | SCM_OPN | SCM_RDNG))
#define SCM_OPOUTPORTP(x) (((0x7f | SCM_OPN | SCM_WRTNG) & SCM_CAR(x))==(scm_tc7_port | SCM_OPN | SCM_WRTNG))
#define SCM_FPORTP(x) (SCM_TYP16S(x)==scm_tc7_port)
#define SCM_OPFPORTP(x) (((0xfeff | SCM_OPN) & SCM_CAR(x))==(scm_tc7_port | SCM_OPN))
#define SCM_OPINFPORTP(x) (((0xfeff | SCM_OPN | SCM_RDNG) & SCM_CAR(x))==(scm_tc7_port | SCM_OPN | SCM_RDNG))
#define SCM_OPOUTFPORTP(x) (((0xfeff | SCM_OPN | SCM_WRTNG) & SCM_CAR(x))==(scm_tc7_port | SCM_OPN | SCM_WRTNG))

#define SCM_INPORTP(x) (((0x7f | SCM_RDNG) & SCM_CAR(x))==(scm_tc7_port | SCM_RDNG))
#define SCM_OUTPORTP(x) (((0x7f | SCM_WRTNG) & SCM_CAR(x))==(scm_tc7_port | SCM_WRTNG))
#define SCM_OPENP(x) (SCM_OPN & SCM_CAR(x))
#define SCM_CLOSEDP(x) (!SCM_OPENP(x))
#define SCM_PTAB_ENTRY(x) ((struct scm_port_table *)SCM_CDR(x))
#define SCM_SETPTAB_ENTRY(x,ent) SCM_SETCDR ((x), (SCM)(ent))
#define SCM_STREAM(x) SCM_PTAB_ENTRY(x)->stream
#define SCM_SETSTREAM(x,s) (SCM_PTAB_ENTRY(x)->stream = s)
#define SCM_FILENAME(x) SCM_PTAB_ENTRY(x)->file_name
#define SCM_LINUM(x) SCM_PTAB_ENTRY(x)->line_number
#define SCM_COL(x) SCM_PTAB_ENTRY(x)->column_number
#define SCM_REVEALED(x) SCM_PTAB_ENTRY(x)->revealed
#define SCM_SETREVEALED(x,s) (SCM_PTAB_ENTRY(x)->revealed = s)
#define SCM_CRDYP(port) (SCM_CAR (port) & SCM_CRDY)
#define SCM_SETRDY(port) {SCM_SETOR_CAR (port, SCM_CRDY);}
#define SCM_CUNGET(c, port) \
{ \
  if (SCM_CRDYP (port)) \
    { \
      if (++SCM_PTAB_ENTRY (port)->cp == SCM_PTAB_ENTRY (port)->cbufend) \
	scm_grow_port_cbuf (port, 1); \
      *SCM_PTAB_ENTRY (port)->cp = c; \
    } \
  else \
    { \
      SCM_PTAB_ENTRY (port)->cbuf[0] = c; \
      SCM_SETRDY (port); \
    } \
} \

#define SCM_CGETUN(port) (*SCM_PTAB_ENTRY (port)->cp)
#define SCM_CLRDY(port) \
{ \
  SCM_PTAB_ENTRY (port)->cp = SCM_PTAB_ENTRY (port)->cbuf; \
  SCM_SETAND_CAR (port, SCM_CUC); \
} \

#define SCM_TRY_CLRDY(port) \
{ \
  if (SCM_PTAB_ENTRY (port)->cp == SCM_PTAB_ENTRY (port)->cbuf) \
    SCM_SETAND_CAR (port, SCM_CUC); \
  else \
    --SCM_PTAB_ENTRY (port)->cp; \
} \

/* Returns number of unread characters in a port.
   Returns wrong answer if SCM_CRDYP is false. */
#define SCM_N_READY_CHARS(port) \
(SCM_PTAB_ENTRY (port)->cp - SCM_PTAB_ENTRY (port)->cbuf + 1)

#define SCM_INCLINE(port)  	{SCM_LINUM (port) += 1; SCM_COL (port) = 0;}
#define SCM_INCCOL(port)  	{SCM_COL (port) += 1;}
#define SCM_TABCOL(port)  	{SCM_COL (port) += 8 - SCM_COL (port) % 8;}





typedef struct scm_ptobfuns
{
  SCM (*mark) (SCM);
  int (*free) (SCM);
  int (*print) (SCM exp, SCM port, scm_print_state *pstate);
  SCM (*equalp) (SCM, SCM);
  int (*fputc) (int, SCM port);
  int (*fputs) (char *, SCM port);
  scm_sizet (*fwrite) SCM_P ((char *ptr,
			      scm_sizet size,
			      scm_sizet nitems,
			      SCM port));
  int (*fflush) (SCM port);
  int (*fgetc) (SCM port);
  char * (*fgets) (SCM port, int *len);
  int (*fclose) (SCM port);
} scm_ptobfuns;

#define SCM_PTOBNUM(x) (0x0ff & (SCM_CAR(x)>>8))



extern scm_ptobfuns *scm_ptobs;
extern int scm_numptob;
extern int scm_port_table_room;



extern SCM scm_markstream SCM_P ((SCM ptr));
extern long scm_newptob SCM_P ((scm_ptobfuns *ptob));
extern SCM scm_char_ready_p SCM_P ((SCM port));
extern SCM scm_current_input_port SCM_P ((void));
extern SCM scm_current_output_port SCM_P ((void));
extern SCM scm_current_error_port SCM_P ((void));
extern SCM scm_current_load_port SCM_P ((void));
extern SCM scm_set_current_input_port SCM_P ((SCM port));
extern SCM scm_set_current_output_port SCM_P ((SCM port));
extern SCM scm_set_current_error_port SCM_P ((SCM port));
extern struct scm_port_table * scm_add_to_port_table SCM_P ((SCM port));
extern void scm_remove_from_port_table SCM_P ((SCM port));
extern void scm_grow_port_cbuf SCM_P ((SCM port, size_t requested));
extern SCM scm_pt_size SCM_P ((void));
extern SCM scm_pt_member SCM_P ((SCM member));
extern int scm_revealed_count SCM_P ((SCM port));
extern SCM scm_port_revealed SCM_P ((SCM port));
extern SCM scm_set_port_revealed_x SCM_P ((SCM port, SCM rcount));
extern long scm_mode_bits SCM_P ((char *modes));
extern SCM scm_port_mode SCM_P ((SCM port));
extern SCM scm_close_port SCM_P ((SCM port));
extern SCM scm_close_all_ports_except SCM_P ((SCM ports));
extern SCM scm_input_port_p SCM_P ((SCM x));
extern SCM scm_output_port_p SCM_P ((SCM x));
extern SCM scm_eof_object_p SCM_P ((SCM x));
extern SCM scm_force_output SCM_P ((SCM port));
extern SCM scm_flush_all_ports SCM_P ((void));
extern SCM scm_read_char SCM_P ((SCM port));
extern SCM scm_peek_char SCM_P ((SCM port));
extern SCM scm_unread_char SCM_P ((SCM cobj, SCM port));
extern SCM scm_unread_string SCM_P ((SCM str, SCM port));
extern char *scm_generic_fgets SCM_P ((SCM port, int *len));
extern SCM scm_port_line SCM_P ((SCM port));
extern SCM scm_set_port_line_x SCM_P ((SCM port, SCM line));
extern SCM scm_port_column SCM_P ((SCM port));
extern SCM scm_set_port_column_x SCM_P ((SCM port, SCM line));
extern SCM scm_port_filename SCM_P ((SCM port));
extern SCM scm_set_port_filename_x SCM_P ((SCM port, SCM filename));
extern void scm_prinport SCM_P ((SCM exp, SCM port, char *type));
extern void scm_ports_prehistory SCM_P ((void));
extern SCM scm_void_port SCM_P ((char * mode_str));
extern SCM scm_sys_make_void_port SCM_P ((SCM mode));
extern void scm_init_ports SCM_P ((void));

#ifdef GUILE_DEBUG
extern SCM scm_pt_size SCM_P ((void));
extern SCM scm_pt_member SCM_P ((SCM member));
#endif /* GUILE_DEBUG */

#endif  /* PORTSH */
