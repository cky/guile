/* classes: h_files */

#ifndef PORTSH
#define PORTSH
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



enum scm_port_representation_type
{
  scm_regular_port,
  scm_mb_port,
  scm_wchar_port
};

enum scm_string_representation_type
{
  scm_regular_string = scm_regular_port,
  scm_mb_string = scm_mb_port,
  scm_wchar_string = scm_wchar_port
};


struct scm_port_table 
{
  SCM port;			/* Open port.  */
  int revealed;			/* 0 not revealed, > 1 revealed.
				 * Revealed ports do not get GC'd.
				 */

  SCM stream;
  SCM file_name;		/* debugging support.  */
  int unchr;			/* pushed back character, if any */

  int line_number;		/* debugging support.  */
  int column_number;		/* debugging support.  */

  enum scm_port_representation_type representation;
};

extern struct scm_port_table **scm_port_table;
extern int scm_port_table_size; /* Number of ports in scm_port_table.  */




/* PORT FLAGS
 * A set of flags characterizes a port.
 */
#define SCM_OPN		(1L<<16) /* Is the port open? */
#define SCM_RDNG	(2L<<16) /* Is it a readable port? */
#define SCM_WRTNG	(4L<<16) /* Is it writable? */
#define SCM_BUF0	(8L<<16)
#define SCM_CRDY	(32L<<16) /* Should char-ready? return #t? */

/* A mask used to clear the char-ready port flag. */
#define SCM_CUC		0x001fffffL

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
#define SCM_PORT_REPRESENTATION(x) SCM_PTAB_ENTRY(x)->representation
#define SCM_SET_PORT_REPRESENTATION(x,s) (SCM_PTAB_ENTRY(x)->representation = s)
#define SCM_CRDYP(port) (SCM_CAR(port) & SCM_CRDY)
#define SCM_CLRDY(port) {SCM_CAR(port) &= SCM_CUC;}
#define SCM_SETRDY(port) {SCM_CAR(port) |= SCM_CRDY;}
#define SCM_CUNGET(c,port) {SCM_PTAB_ENTRY(port)->unchr = c; SCM_SETRDY(port);}
#define SCM_CGETUN(port) (SCM_PTAB_ENTRY(port)->unchr)

#define SCM_INCLINE(port)  	{SCM_LINUM (port) += 1; SCM_COL (port) = 0;}
#define SCM_INCCOL(port)  	{SCM_COL (port) += 1;}
#define SCM_TABCOL(port)  	{SCM_COL (port) += 8 - SCM_COL (port) % 8;}





typedef struct scm_ptobfuns
{
  SCM (*mark) SCM_P ((SCM));
  int (*free) SCM_P ((SCM));
  int (*print) SCM_P ((SCM exp, SCM port, scm_print_state *pstate));
  SCM (*equalp) SCM_P ((SCM, SCM));
  int (*fputc) SCM_P ((int, SCM stream));
  int (*fputs) SCM_P ((char *, SCM stream));
  scm_sizet (*fwrite) SCM_P ((char *ptr, scm_sizet size, scm_sizet nitems, SCM stream));
  int (*fflush) SCM_P ((SCM stream));
  int (*fgetc) SCM_P ((SCM stream));
  int (*fclose) SCM_P ((SCM stream));
} scm_ptobfuns;

#define SCM_PTOBNUM(x) (0x0ff & (SCM_CAR(x)>>8));



extern scm_ptobfuns *scm_ptobs;
extern scm_sizet scm_numptob;
extern int scm_port_table_room;



extern SCM scm_markstream SCM_P ((SCM ptr));
extern long scm_newptob SCM_P ((scm_ptobfuns *ptob));
extern void scm_fflush SCM_P ((SCM port));
extern SCM scm_char_ready_p SCM_P ((SCM port));
extern SCM scm_ungetc_char_ready_p SCM_P ((SCM port));
extern SCM scm_current_input_port SCM_P ((void));
extern SCM scm_current_output_port SCM_P ((void));
extern SCM scm_current_error_port SCM_P ((void));
extern SCM scm_set_current_input_port SCM_P ((SCM port));
extern SCM scm_set_current_output_port SCM_P ((SCM port));
extern SCM scm_set_current_error_port SCM_P ((SCM port));
extern struct scm_port_table * scm_add_to_port_table SCM_P ((SCM port));
extern void scm_remove_from_port_table SCM_P ((SCM port));
extern SCM scm_pt_size SCM_P ((void));
extern SCM scm_pt_member SCM_P ((SCM member));
extern int scm_revealed_count SCM_P ((SCM port));
extern SCM scm_port_revealed SCM_P ((SCM port));
extern SCM scm_set_port_revealed_x SCM_P ((SCM port, SCM rcount));
extern SCM scm_close_port SCM_P ((SCM port));
extern SCM scm_close_all_ports_except SCM_P ((SCM ports));
extern SCM scm_input_port_p SCM_P ((SCM x));
extern SCM scm_output_port_p SCM_P ((SCM x));
extern SCM scm_eof_object_p SCM_P ((SCM x));
extern SCM scm_force_output SCM_P ((SCM port));
extern SCM scm_read_char SCM_P ((SCM port));
extern SCM scm_peek_char SCM_P ((SCM port));
extern SCM scm_unread_char SCM_P ((SCM cobj, SCM port));
extern SCM scm_port_line SCM_P ((SCM port));
extern SCM scm_port_column SCM_P ((SCM port));
extern SCM scm_port_filename SCM_P ((SCM port));
extern SCM scm_set_port_filename_x SCM_P ((SCM port, SCM filename));
extern void scm_prinport SCM_P ((SCM exp, SCM port, char *type));
extern void scm_ports_prehistory SCM_P ((void));
extern SCM scm_void_port SCM_P ((char * mode_str));
extern SCM scm_sys_make_void_port SCM_P ((SCM mode));
extern void scm_init_ports SCM_P ((void));

#endif  /* PORTSH */
