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

#include <stdio.h>
#include "_scm.h"


#ifdef HAVE_MALLOC_H
#include "malloc.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif




/* scm_ptobs scm_numptob
 * implement a dynamicly resized array of ptob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */
scm_ptobfuns *scm_ptobs;
scm_sizet scm_numptob;

#ifdef __STDC__
SCM 
scm_markstream (SCM ptr)
#else
SCM 
scm_markstream (ptr)
     SCM ptr;
#endif
{
  int openp;
  if (SCM_GC8MARKP (ptr))
    return SCM_BOOL_F;
  openp = SCM_CAR (ptr) & SCM_OPN;
  SCM_SETGC8MARK (ptr);
  if (openp)
    return SCM_STREAM  (ptr);
  else
    return SCM_BOOL_F;
}


#ifdef __STDC__
long 
scm_newptob (scm_ptobfuns *ptob)
#else
long 
scm_newptob (ptob)
     scm_ptobfuns *ptob;
#endif
{
  char *tmp;
  if (255 <= scm_numptob)
    goto ptoberr;
  SCM_DEFER_INTS;
  SCM_SYSCALL (tmp = (char *) realloc ((char *) scm_ptobs, (1 + scm_numptob) * sizeof (scm_ptobfuns)));
  if (tmp)
    {
      scm_ptobs = (scm_ptobfuns *) tmp;
      scm_ptobs[scm_numptob].mark = ptob->mark;
      scm_ptobs[scm_numptob].free = ptob->free;
      scm_ptobs[scm_numptob].print = ptob->print;
      scm_ptobs[scm_numptob].equalp = ptob->equalp;
      scm_ptobs[scm_numptob].fputc = ptob->fputc;
      scm_ptobs[scm_numptob].fputs = ptob->fputs;
      scm_ptobs[scm_numptob].fwrite = ptob->fwrite;
      scm_ptobs[scm_numptob].fflush = ptob->fflush;
      scm_ptobs[scm_numptob].fgetc = ptob->fgetc;
      scm_ptobs[scm_numptob].fclose = ptob->fclose;
      scm_numptob++;
    }
  SCM_ALLOW_INTS;
  if (!tmp)
  ptoberr:scm_wta (SCM_MAKINUM ((long) scm_numptob), (char *) SCM_NALLOC, "newptob");
  return scm_tc7_port + (scm_numptob - 1) * 256;
}


/* internal SCM call */
#ifdef __STDC__
void 
scm_fflush (SCM port)
#else
void 
scm_fflush (port)
     SCM port;
#endif
{
  scm_sizet i = SCM_PTOBNUM (port);
  (scm_ptobs[i].fflush) (SCM_STREAM (port));
}




#ifdef __IBMC__
# define MSDOS
#endif
#ifdef MSDOS
# ifndef GO32
#  include <io.h>
#  include <conio.h>
#ifdef __STDC__
static int 
input_waiting (FILE *f)
#else
static int 
input_waiting (f)
     FILE *f;
#endif
{
  if (feof (f))
    return 1;
  if (fileno (f) == fileno (stdin) && (isatty (fileno (stdin))))
    return kbhit ();
  return -1;
}
# endif
#else
# ifdef _DCC
#  include <ioctl.h>
# else
#  ifndef AMIGA
#   ifndef vms
#    ifdef MWC
#     include <sys/io.h>
#    else
#     ifndef THINK_C
#      ifndef ARM_ULIB
#       include <sys/ioctl.h>
#      endif
#     endif
#    endif
#   endif
#  endif
# endif


#ifdef __STDC__
static int
input_waiting(FILE *f)
#else
static int
input_waiting(f)
     FILE *f;
#endif
{
# ifdef FIONREAD
  long remir;
  if (feof(f)) return 1;
  ioctl(fileno(f), FIONREAD, &remir);
  return remir;
# else
  return -1;
# endif
}
#endif

SCM_PROC(s_char_ready_p, "char-ready?", 1, 0, 0, scm_char_ready_p);
#ifdef __STDC__
SCM 
scm_char_ready_p (SCM port)
#else
SCM 
scm_char_ready_p (port)
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_char_ready_p);
  if (SCM_CRDYP (port) || !SCM_FPORTP (port))
    return SCM_BOOL_T;
  return input_waiting ((FILE *)SCM_STREAM (port)) ? SCM_BOOL_T : SCM_BOOL_F;
}




SCM_PROC (s_ungetc_char_ready_p, "ungetc-char-ready?", 1, 0, 0, scm_ungetc_char_ready_p);
#ifdef __STDC__
SCM 
scm_ungetc_char_ready_p (SCM port)
#else
SCM 
scm_ungetc_char_ready_p (port)
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_ungetc_char_ready_p);
  return (SCM_CRDYP (port)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}





/* {Standard Ports}
 */
SCM_PROC(s_current_input_port, "current-input-port", 0, 0, 0, scm_current_input_port);
#ifdef __STDC__
SCM 
scm_current_input_port (void)
#else
SCM 
scm_current_input_port ()
#endif
{
  return scm_cur_inp;
}

SCM_PROC(s_current_output_port, "current-output-port", 0, 0, 0, scm_current_output_port);
#ifdef __STDC__
SCM 
scm_current_output_port (void)
#else
SCM 
scm_current_output_port ()
#endif
{
  return scm_cur_outp;
}

SCM_PROC(s_current_error_port, "current-error-port", 0, 0, 0, scm_current_error_port);
#ifdef __STDC__
SCM 
scm_current_error_port (void)
#else
SCM 
scm_current_error_port ()
#endif
{
  return scm_cur_errp;
}

SCM_PROC(s_set_current_input_port, "set-current-input-port", 1, 0, 0, scm_set_current_input_port);
#ifdef __STDC__
SCM 
scm_set_current_input_port (SCM port)
#else
SCM 
scm_set_current_input_port (port)
     SCM port;
#endif
{
  SCM oinp = scm_cur_inp;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_set_current_input_port);
  scm_cur_inp = port;
  return oinp;
}


SCM_PROC(s_set_current_output_port, "set-current-output-port", 1, 0, 0, scm_set_current_output_port);
#ifdef __STDC__
SCM 
scm_set_current_output_port (SCM port)
#else
SCM 
scm_set_current_output_port (port)
     SCM port;
#endif
{
  SCM ooutp = scm_cur_outp;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_set_current_output_port);
  scm_cur_outp = port;
  return ooutp;
}


SCM_PROC(s_set_current_error_port, "set-current-error-port", 1, 0, 0, scm_set_current_error_port);
#ifdef __STDC__
SCM 
scm_set_current_error_port (SCM port)
#else
SCM 
scm_set_current_error_port (port)
     SCM port;
#endif
{
  SCM oerrp = scm_cur_errp;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_set_current_error_port);
  scm_cur_errp = port;
  return oerrp;
}



/* {Ports - in general}
 * 
 */

/* Array of open ports, required for reliable MOVE->FDES etc.  */
struct scm_port_table **scm_port_table;

int scm_port_table_size = 0;	/* Number of ports in scm_port_table.  */
int scm_port_table_room = 20;	/* Size of the array.  */

/* Add a port to the table.  Call with SCM_DEFER_INTS active.  */
#ifdef __STDC__
struct scm_port_table *
scm_add_to_port_table (SCM port)
#else
struct scm_port_table *
scm_add_to_port_table (port)
     SCM port;
#endif
{
  if (scm_port_table_size == scm_port_table_room)
    {
      scm_port_table = ((struct scm_port_table **)
			realloc ((char *) scm_port_table,
				 (long) (sizeof (struct scm_port_table)
					 * scm_port_table_room * 2)));
      /* !!! error checking */
      scm_port_table_room *= 2;
    }
  scm_port_table[scm_port_table_size] = ((struct scm_port_table *)
					 scm_must_malloc (sizeof (struct scm_port_table),
							  "system port table"));
  scm_port_table[scm_port_table_size]->port = port;
  scm_port_table[scm_port_table_size]->revealed = 0;
  scm_port_table[scm_port_table_size]->stream = 0;
  scm_port_table[scm_port_table_size]->file_name = SCM_BOOL_F;
  scm_port_table[scm_port_table_size]->line_number = 1;
  scm_port_table[scm_port_table_size]->column_number = 0;
  scm_port_table[scm_port_table_size]->representation = scm_regular_port;
  return scm_port_table[scm_port_table_size++];
}

/* Remove a port from the table.  Call with SCM_DEFER_INTS active.  */
#ifdef __STDC__
void
scm_remove_from_port_table (SCM port)
#else
void
scm_remove_from_port_table (port)
     SCM port;
#endif
{
  int i = 0;
  while (scm_port_table[i]->port != port)
    {
      i++;
      /* Error if not found: too violent?  May occur in GC.  */
      if (i >= scm_port_table_size)
	scm_wta (port, "Port not in table", "scm_remove_from_port_table");
    }
  scm_must_free ((char *)scm_port_table[i]);
  scm_mallocated -= sizeof (*scm_port_table[i]);
  scm_port_table[i] = scm_port_table[scm_port_table_size - 1];
  SCM_SETPTAB_ENTRY (port, 0);
  scm_port_table_size--;
}

#ifdef DEBUG
/* Undocumented functions for debugging.  */
/* Return the number of ports in the table.  */
static char s_pt_size[] = "pt-size";
#ifdef __STDC__
SCM
scm_pt_size (void)
#else
SCM
scm_pt_size ()
#endif
{
  return SCM_MAKINUM (scm_port_table_size);
}

/* Return the ith member of the port table.  */
static char s_pt_member[] = "pt-member";
#ifdef __STDC__
SCM
scm_pt_member (SCM member)
#else
SCM
scm_pt_member (member)
     SCM member;
#endif
{
  int i;
  SCM_ASSERT (SCM_INUMP (member), member, SCM_ARG1, s_pt_member);
  i = SCM_INUM (member);
  if (i < 0 || i >= scm_port_table_size)
    return SCM_BOOL_F;
  else
    return scm_port_table[i]->port;
}
#endif


/* Find a port in the table and return its revealed count.  Return -1
 * if the port isn't in the table (should not happen).  Also used by
 * the garbage collector.
 */
#ifdef __STDC__
int
scm_revealed_count (SCM port)
#else
int
scm_revealed_count (port)
     SCM port;
#endif
{
  return SCM_REVEALED(port);
}



/* Return the revealed count for a port.  */

SCM_PROC(s_port_revealed, "port-revealed", 1, 0, 0, scm_port_revealed);
#ifdef __STDC__
SCM
scm_port_revealed (SCM port)
#else
SCM
scm_port_revealed (port)
     SCM port;
#endif
{
  int result;

  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1, s_port_revealed);

  if ((result = scm_revealed_count (port)) == -1)
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (result);
}

/* Set the revealed count for a port.  */
SCM_PROC(s_set_port_revealed_x, "set-port-revealed!", 2, 0, 0, scm_set_port_revealed_x);
#ifdef __STDC__
SCM
scm_set_port_revealed_x (SCM port, SCM rcount)
#else
SCM
scm_set_port_revealed_x (port, rcount)
     SCM port;
     SCM rcount;
#endif
{
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1, s_set_port_revealed_x);
  SCM_ASSERT (SCM_INUMP (rcount), rcount, SCM_ARG2, s_set_port_revealed_x);
  SCM_DEFER_INTS;
  SCM_REVEALED (port) = SCM_INUM (rcount);
  SCM_ALLOW_INTS;
  return SCM_BOOL_F;
}

/* scm_close_port
 * Call the close operation on a port object. 
 */
SCM_PROC(s_close_port, "close-port", 1, 0, 0, scm_close_port);
#ifdef __STDC__
SCM
scm_close_port (SCM port)
#else
SCM
scm_close_port (port)
     SCM port;
#endif
{
  scm_sizet i;
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1, s_close_port);
  if (SCM_CLOSEDP (port))
    return SCM_UNSPECIFIED;
  i = SCM_PTOBNUM (port);
  SCM_DEFER_INTS;
  if (scm_ptobs[i].fclose)
    SCM_SYSCALL ((scm_ptobs[i].fclose) (SCM_STREAM (port)));
  scm_remove_from_port_table (port);
  SCM_CAR (port) &= ~SCM_OPN;
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_close_all_ports_except, "close-all-ports-except", 0, 0, 1, scm_close_all_ports_except);
#ifdef __STDC__
SCM
scm_close_all_ports_except (SCM ports)
#else
SCM
scm_close_all_ports_except (ports)
     SCM ports;
#endif
{
  int i = 0;
  SCM_ASSERT (SCM_NIMP (ports) && SCM_CONSP (ports), ports, SCM_ARG1, s_close_all_ports_except);
  SCM_DEFER_INTS;  
  while (i < scm_port_table_size)
    {
      SCM thisport = scm_port_table[i]->port;
      int found = 0;
      SCM ports_ptr = ports;

      while (SCM_NNULLP (ports_ptr))
	{
	  SCM port = SCM_CAR (ports_ptr);
	  if (i == 0)
	    SCM_ASSERT (SCM_NIMP (port) && SCM_OPPORTP (port), port, SCM_ARG1, s_close_all_ports_except);
	  if (port == thisport)
	    found = 1;
	  ports_ptr = SCM_CDR (ports_ptr);
	}
      if (found)
	i++;
      else
	/* i is not to be incremented here.  */
	scm_close_port (thisport);
    }
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_input_port_p, "input-port?", 1, 0, 0, scm_input_port_p);
#ifdef __STDC__
SCM 
scm_input_port_p (SCM x)
#else
SCM 
scm_input_port_p (x)
     SCM x;
#endif
{
  if (SCM_IMP (x))
 return SCM_BOOL_F;
  return SCM_INPORTP (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_output_port_p, "output-port?", 1, 0, 0, scm_output_port_p);
#ifdef __STDC__
SCM 
scm_output_port_p (SCM x)
#else
SCM 
scm_output_port_p (x)
     SCM x;
#endif
{
  if (SCM_IMP (x))
 return SCM_BOOL_F;
  return SCM_OUTPORTP (x) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC(s_eof_object_p, "eof-object?", 1, 0, 0, scm_eof_object_p);
#ifdef __STDC__
SCM 
scm_eof_object_p (SCM x)
#else
SCM 
scm_eof_object_p (x)
     SCM x;
#endif
{
  return (SCM_EOF_VAL == x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_force_output, "force-output", 0, 1, 0, scm_force_output);
#ifdef __STDC__
SCM 
scm_force_output (SCM port)
#else
SCM 
scm_force_output (port)
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
 port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_force_output);
  {
    scm_sizet i = SCM_PTOBNUM (port);
    SCM_SYSCALL ((scm_ptobs[i].fflush) (SCM_STREAM (port)));
    return SCM_UNSPECIFIED;
  }
}


SCM_PROC(s_read_char, "read-char", 0, 1, 0, scm_read_char);
#ifdef __STDC__
SCM 
scm_read_char (SCM port)
#else
SCM 
scm_read_char (port)
     SCM port;
#endif
{
  int c;
  if (SCM_UNBNDP (port))
 port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_read_char);
  c = scm_gen_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  return SCM_MAKICHR (c);
}


SCM_PROC(s_peek_char, "peek-char", 0, 1, 0, scm_peek_char);
#ifdef __STDC__
SCM 
scm_peek_char (SCM port)
#else
SCM 
scm_peek_char (port)
     SCM port;
#endif
{
  int c;
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_peek_char);
  c = scm_gen_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_gen_ungetc (c, port);
  return SCM_MAKICHR (c);
}

SCM_PROC (s_unread_char, "unread-char", 2, 0, 0, scm_unread_char);
#ifdef __STDC__
SCM 
scm_unread_char (SCM cobj, SCM port)
#else
SCM 
scm_unread_char (cobj, port)
     SCM cobj;
     SCM port;
#endif
{
  int c;

  SCM_ASSERT (SCM_ICHRP (cobj), cobj, SCM_ARG1, s_unread_char);

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG2, s_unread_char);


  c = SCM_ICHR (cobj);

  scm_gen_ungetc (c, port);
  return cobj;
}



SCM_PROC (s_line_number, "line-number", 0, 1, 0, scm_line_number);
#ifdef __STDC__
SCM 
scm_line_number (SCM port)
#else
SCM 
scm_line_number (port)
     SCM port;
#endif
{
  SCM p;
  p = ((port == SCM_UNDEFINED)
       ? scm_cur_inp
       : port);
  if (!(SCM_NIMP (p) && SCM_PORTP (p)))
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (SCM_LINUM (p));
}

SCM_PROC (s_column_number, "column-number", 0, 1, 0, scm_column_number);
#ifdef __STDC__
SCM
scm_column_number (SCM port)
#else
SCM
scm_column_number  (port)
     SCM port;
#endif
{
  SCM p;
  p = ((port == SCM_UNDEFINED)
       ? scm_cur_inp
       : port);
  if (!(SCM_NIMP (p) && SCM_PORTP (p)))
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (SCM_COL (p));
}

/* !!! dubious feature */
SCM_PROC (s_port_file_name, "port-file-name", 0, 1, 0, scm_port_file_name);
#ifdef __STDC__
SCM 
scm_port_file_name (SCM port)
#else
SCM 
scm_port_file_name (port)
     SCM port;
#endif
{
  SCM p;
  p = ((port == SCM_UNDEFINED)
       ? scm_cur_inp
       : port);
  if (!(SCM_NIMP (p) && SCM_PORTP (p)))
    return SCM_BOOL_F;
  else
    return SCM_PTAB_ENTRY (p)->file_name;
}

#ifndef ttyname
extern char * ttyname();
#endif

#ifdef __STDC__
void 
scm_prinport (SCM exp, SCM port, char *type)
#else
void 
scm_prinport (exp, port, type)
     SCM exp;
     SCM port;
     char *type;
#endif
{
  scm_gen_puts (scm_regular_string, "#<", port);
  if (SCM_CLOSEDP (exp))
    scm_gen_puts (scm_regular_string, "closed: ", port);
  else
    {
      if (SCM_RDNG & SCM_CAR (exp))
	scm_gen_puts (scm_regular_string, "input: ", port);
      if (SCM_WRTNG & SCM_CAR (exp))
	scm_gen_puts (scm_regular_string, "output: ", port);
    }
  scm_gen_puts (scm_regular_string, type, port);
  scm_gen_putc (' ', port);
#ifndef MSDOS
#ifndef __EMX__
#ifndef _DCC
#ifndef AMIGA
#ifndef THINK_C
  if (SCM_OPENP (exp) && scm_tc16_fport == SCM_TYP16 (exp) && isatty (fileno ((FILE *)SCM_STREAM (exp))))
    scm_gen_puts (scm_regular_string, ttyname (fileno ((FILE *)SCM_STREAM (exp))), port);
  else
#endif
#endif
#endif
#endif
#endif
  if (SCM_OPFPORTP (exp))
    scm_intprint ((long) fileno ((FILE *)SCM_STREAM (exp)), 10, port);
  else
    scm_intprint (SCM_CDR (exp), 16, port);
  scm_gen_putc ('>', port);
}

#ifdef __STDC__
void
scm_ports_prehistory (void)
#else
void
scm_ports_prehistory ()
#endif
{
  scm_numptob = 0;
  scm_ptobs = (scm_ptobfuns *) malloc (sizeof (scm_ptobfuns));
  
  /* WARNING: These scm_newptob calls must be done in this order.
   * They must agree with the port declarations in tags.h.
   */
  /* scm_tc16_fport = */ scm_newptob (&scm_fptob);
  /* scm_tc16_pipe = */ scm_newptob (&scm_pipob);
  /* scm_tc16_strport = */ scm_newptob (&scm_stptob);
  /* scm_tc16_sfport = */ scm_newptob (&scm_sfptob);
}



/* {Void Ports}
 */

int scm_tc16_void_port = 0;

static int
print_void_port (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  scm_prinport (exp, port, "void");
  return 1;
}

static int
putc_void_port (c, strm)
     int c;
     SCM strm;
{
  return 0;			/* vestigial return value */
}

static int
puts_void_port (s, strm)
     char * s;
     SCM strm;
{
  return 0;			/* vestigial return value */
}

static scm_sizet
write_void_port (ptr, size, nitems, strm)
     void * ptr;
     int size;
     int nitems;
     SCM strm;
{
  int len;
  len = size * nitems;
  return len;
}

#ifdef __STDC__
static int
flush_void_port (SCM strm)
#else
static int
flush_void_port (strm)
     SCM strm;
#endif
{
  return 0;
}

#ifdef __STDC__
static int
getc_void_port (SCM strm)
#else
static int
getc_void_port (strm)
     SCM strm;
#endif
{
  return EOF;
}

#ifdef __STDC__
static int
close_void_port (SCM strm)
#else
static int
close_void_port (strm)
     SCM strm;
#endif
{
  return 0;			/* this is ignored by scm_close_port. */
}


#ifdef __STDC__
static int 
noop0 (FILE *stream)
#else
static int 
noop0 (stream)
     FILE *stream;
#endif
{
  return 0;
}


static struct scm_ptobfuns  void_port_ptob =
{
  scm_mark0, 
  noop0,
  print_void_port,
  0,				/* equal? */
  putc_void_port,
  puts_void_port,
  write_void_port,
  flush_void_port,
  getc_void_port,
  close_void_port,
};



#ifdef __STDC__
SCM
scm_void_port (char * mode_str)
#else
SCM
scm_void_port (mode_str)
     char * mode_str;
#endif
{
  int mode_bits;
  SCM answer;
  struct scm_port_table * pt;

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  mode_bits = scm_mode_bits (mode_str);
  pt = scm_add_to_port_table (answer);
  SCM_CAR (answer) = scm_tc16_void_port | mode_bits;
  SCM_SETPTAB_ENTRY (answer, pt);
  SCM_SETSTREAM (answer, SCM_BOOL_F);
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC (s_sys_make_void_port, "%make-void-port", 1, 0, 0, scm_sys_make_void_port);
#ifdef __STDC__
SCM
scm_sys_make_void_port (SCM mode)
#else
SCM
scm_sys_make_void_port (mode)
     SCM mode;
#endif
{
  SCM_ASSERT (SCM_NIMP (mode) && SCM_STRINGP (mode), mode,
	      SCM_ARG1, s_sys_make_void_port);

  return scm_void_port (SCM_ROCHARS (mode));
}





#ifdef __STDC__
void
scm_init_ports (void)
#else
void
scm_init_ports ()
#endif
{
  scm_tc16_void_port = scm_newptob (&void_port_ptob);
#include "ports.x"
}

