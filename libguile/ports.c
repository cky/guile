/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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

#include <stdio.h>
#include "_scm.h"
#include "genio.h"
#include "chars.h"

#include "markers.h"
#include "filesys.h"
#include "fports.h"
#include "strports.h"
#include "vports.h"
#include "kw.h"

#include "ports.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif



/* scm_ptobs scm_numptob
 * implement a dynamicly resized array of ptob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */
scm_ptobfuns *scm_ptobs;
scm_sizet scm_numptob;


SCM 
scm_markstream (ptr)
     SCM ptr;
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



long 
scm_newptob (ptob)
     scm_ptobfuns *ptob;
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
      scm_ptobs[scm_numptob].fgets = ptob->fgets;
      scm_ptobs[scm_numptob].fclose = ptob->fclose;
      scm_numptob++;
    }
  SCM_ALLOW_INTS;
  if (!tmp)
  ptoberr:scm_wta (SCM_MAKINUM ((long) scm_numptob), (char *) SCM_NALLOC, "newptob");
  return scm_tc7_port + (scm_numptob - 1) * 256;
}


/* internal SCM call */

void 
scm_fflush (port)
     SCM port;
{
  scm_sizet i = SCM_PTOBNUM (port);
  (scm_ptobs[i].fflush) (SCM_STREAM (port));
}



SCM_PROC(s_char_ready_p, "char-ready?", 0, 1, 0, scm_char_ready_p);

SCM 
scm_char_ready_p (port)
     SCM port;
{
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_char_ready_p);
  if (SCM_CRDYP (port) || !SCM_FPORTP (port))
    return SCM_BOOL_T;
  return (scm_input_waiting_p ((FILE *) SCM_STREAM (port), s_char_ready_p)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}





/* {Standard Ports}
 */
SCM_PROC(s_current_input_port, "current-input-port", 0, 0, 0, scm_current_input_port);

SCM 
scm_current_input_port ()
{
  return scm_cur_inp;
}

SCM_PROC(s_current_output_port, "current-output-port", 0, 0, 0, scm_current_output_port);

SCM 
scm_current_output_port ()
{
  return scm_cur_outp;
}

SCM_PROC(s_current_error_port, "current-error-port", 0, 0, 0, scm_current_error_port);

SCM 
scm_current_error_port ()
{
  return scm_cur_errp;
}

SCM_PROC(s_set_current_input_port, "set-current-input-port", 1, 0, 0, scm_set_current_input_port);

SCM 
scm_set_current_input_port (port)
     SCM port;
{
  SCM oinp = scm_cur_inp;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_set_current_input_port);
  scm_cur_inp = port;
  return oinp;
}


SCM_PROC(s_set_current_output_port, "set-current-output-port", 1, 0, 0, scm_set_current_output_port);

SCM 
scm_set_current_output_port (port)
     SCM port;
{
  SCM ooutp = scm_cur_outp;
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_set_current_output_port);
  scm_cur_outp = port;
  return ooutp;
}


SCM_PROC(s_set_current_error_port, "set-current-error-port", 1, 0, 0, scm_set_current_error_port);

SCM 
scm_set_current_error_port (port)
     SCM port;
{
  SCM oerrp = scm_cur_errp;
  port = SCM_COERCE_OUTPORT (port);
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

struct scm_port_table *
scm_add_to_port_table (port)
     SCM port;
{
  if (scm_port_table_size == scm_port_table_room)
    {
      scm_port_table = ((struct scm_port_table **)
			realloc ((char *) scm_port_table,
				 (scm_sizet) (sizeof (struct scm_port_table *)
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
  scm_port_table[scm_port_table_size]->line_number = 0;
  scm_port_table[scm_port_table_size]->column_number = 0;
  return scm_port_table[scm_port_table_size++];
}

/* Remove a port from the table.  Call with SCM_DEFER_INTS active.  */

void
scm_remove_from_port_table (port)
     SCM port;
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

#ifdef GUILE_DEBUG
/* Undocumented functions for debugging.  */
/* Return the number of ports in the table.  */

SCM_PROC(s_pt_size, "pt-size", 0, 0, 0, scm_pt_size);
SCM
scm_pt_size ()
{
  return SCM_MAKINUM (scm_port_table_size);
}

/* Return the ith member of the port table.  */
SCM_PROC(s_pt_member, "pt-member", 1, 0, 0, scm_pt_member);
SCM
scm_pt_member (member)
     SCM member;
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


/* Find a port in the table and return its revealed count.
   Also used by the garbage collector.
 */

int
scm_revealed_count (port)
     SCM port;
{
  return SCM_REVEALED(port);
}



/* Return the revealed count for a port.  */

SCM_PROC(s_port_revealed, "port-revealed", 1, 0, 0, scm_port_revealed);

SCM
scm_port_revealed (port)
     SCM port;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1, s_port_revealed);
  return SCM_MAKINUM (scm_revealed_count (port));
}

/* Set the revealed count for a port.  */
SCM_PROC(s_set_port_revealed_x, "set-port-revealed!", 2, 0, 0, scm_set_port_revealed_x);

SCM
scm_set_port_revealed_x (port, rcount)
     SCM port;
     SCM rcount;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1, s_set_port_revealed_x);
  SCM_ASSERT (SCM_INUMP (rcount), rcount, SCM_ARG2, s_set_port_revealed_x);
  SCM_DEFER_INTS;
  SCM_REVEALED (port) = SCM_INUM (rcount);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}

/* Return the flags that characterize a port based on the mode
 * string used to open a file for that port.
 *
 * See PORT FLAGS in scm.h
 */

long
scm_mode_bits (modes)
     char *modes;
{
  return (SCM_OPN
	  | (strchr (modes, 'r') || strchr (modes, '+') ? SCM_RDNG : 0)
	  | (   strchr (modes, 'w')
	     || strchr (modes, 'a')
	     || strchr (modes, '+') ? SCM_WRTNG : 0)
	  | (strchr (modes, '0') ? SCM_BUF0 : 0));
}


/* Return the mode flags from an open port.
 * Some modes such as "append" are only used when opening
 * a file and are not returned here.  */

SCM_PROC(s_port_mode, "port-mode", 1, 0, 0, scm_port_mode);

SCM
scm_port_mode (port)
     SCM port;
{
  char modes[3];
  modes[0] = '\0';

  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPPORTP (port), port, SCM_ARG1, s_port_mode);  
  if (SCM_CAR (port) & SCM_RDNG) {
    if (SCM_CAR (port) & SCM_WRTNG)
      strcpy (modes, "r+");
    else
      strcpy (modes, "r");
  }
  else if (SCM_CAR (port) & SCM_WRTNG)
    strcpy (modes, "w");
  if (SCM_CAR (port) & SCM_BUF0)
    strcat (modes, "0");
  return scm_makfromstr (modes, strlen (modes), 0);
}


/* scm_close_port
 * Call the close operation on a port object. 
 * see also scm_close.
 */
SCM_PROC(s_close_port, "close-port", 1, 0, 0, scm_close_port);

SCM
scm_close_port (port)
     SCM port;
{
  scm_sizet i;
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1,
	      s_close_port);
  if (SCM_CLOSEDP (port))
    return SCM_BOOL_F;
  i = SCM_PTOBNUM (port);
  SCM_DEFER_INTS;
  if (scm_ptobs[i].fclose)
    {
      SCM_SYSCALL (rv = (scm_ptobs[i].fclose) (SCM_STREAM (port)));
      /* ports with a closed file descriptor can be reclosed without error.  */
      if (rv < 0 && errno != EBADF)
	scm_syserror (s_close_port);
    }
  else
    rv = 0;
  scm_remove_from_port_table (port);
  SCM_SETAND_CAR (port, ~SCM_OPN);
  SCM_ALLOW_INTS;
  return (rv < 0) ? SCM_BOOL_F : SCM_BOOL_T;
}

SCM_PROC(s_close_all_ports_except, "close-all-ports-except", 0, 0, 1, scm_close_all_ports_except);

SCM
scm_close_all_ports_except (ports)
     SCM ports;
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
	  SCM port = SCM_COERCE_OUTPORT (SCM_CAR (ports_ptr));
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

SCM 
scm_input_port_p (x)
     SCM x;
{
  if (SCM_IMP (x))
 return SCM_BOOL_F;
  return SCM_INPORTP (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_output_port_p, "output-port?", 1, 0, 0, scm_output_port_p);

SCM 
scm_output_port_p (x)
     SCM x;
{
  if (SCM_IMP (x))
 return SCM_BOOL_F;
  return SCM_OUTPORTP (x) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC(s_eof_object_p, "eof-object?", 1, 0, 0, scm_eof_object_p);

SCM 
scm_eof_object_p (x)
     SCM x;
{
  return SCM_EOF_OBJECT_P (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_force_output, "force-output", 0, 1, 0, scm_force_output);

SCM 
scm_force_output (port)
     SCM port;
{
  if (SCM_UNBNDP (port))
 port = scm_cur_outp;
  else
    {
      port = SCM_COERCE_OUTPORT (port);
      SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_force_output);
    }
  {
    scm_sizet i = SCM_PTOBNUM (port);
    SCM_SYSCALL ((scm_ptobs[i].fflush) (SCM_STREAM (port)));
    return SCM_UNSPECIFIED;
  }
}

SCM_PROC (s_flush_all_ports, "flush-all-ports", 0, 0, 0, scm_flush_all_ports);
SCM
scm_flush_all_ports (void)
{
  int i;

  for (i = 0; i < scm_port_table_size; i++)
    {
      SCM port = scm_port_table[i]->port;
      if (SCM_OPOUTPORTP (port))
	{
	  scm_sizet ptob = SCM_PTOBNUM (port);
	  (scm_ptobs[ptob].fflush) (SCM_STREAM (port));
	}
    }
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_read_char, "read-char", 0, 1, 0, scm_read_char);

SCM 
scm_read_char (port)
     SCM port;
{
  int c;
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_read_char);
  c = scm_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  return SCM_MAKICHR (c);
}


SCM_PROC(s_peek_char, "peek-char", 0, 1, 0, scm_peek_char);

SCM 
scm_peek_char (port)
     SCM port;
{
  int c;
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_peek_char);
  c = scm_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_ungetc (c, port);
  return SCM_MAKICHR (c);
}

/*
 * A generic fgets method.  We supply this method so that ports which
 * can't use fgets(3) (like string ports or soft ports) can still use
 * line-based i/o.  The generic method calls the port's own fgetc method
 * for input.  It should be possible to write a more efficient
 * method for any given port representation -- this is supplied just
 * to ensure that you don't have to.
 */

char * scm_generic_fgets SCM_P ((SCM port, int *len));

char *
scm_generic_fgets (port, len)
     SCM port;
     int *len;
{
  SCM f		= SCM_STREAM (port);
  scm_sizet p	= SCM_PTOBNUM (port);

  char *buf;
  int   limit = 80;	/* current size of buffer */
  int   c;

  /* FIXME: It would be nice to be able to check for EOF before anything. */

  *len = 0;
  buf = (char *) malloc (limit * sizeof(char));

  /* If a char has been pushed onto the port with scm_ungetc,
     read that first. */
  if (SCM_CRDYP (port))
    {
      buf[*len] = SCM_CGETUN (port);
      SCM_CLRDY (port);
      if (buf[(*len)++] == '\n')
	{
	  buf[*len] = '\0';
	  return buf;
	}
    }

  while (1) {
    if (*len >= limit-1)
      {
	buf = (char *) realloc (buf, sizeof(char) * limit * 2);
	limit *= 2;
      }

    c = (scm_ptobs[p].fgetc) (f);
    if (c != EOF)
      buf[(*len)++] = c;

    if (c == EOF || c == '\n')
      {
	if (*len)
	  {
	    buf[*len] = '\0';
	    return buf;
	  }
	free (buf);
	return NULL;
      }
  }
}

SCM_PROC (s_unread_char, "unread-char", 2, 0, 0, scm_unread_char);

SCM 
scm_unread_char (cobj, port)
     SCM cobj;
     SCM port;
{
  int c;

  SCM_ASSERT (SCM_ICHRP (cobj), cobj, SCM_ARG1, s_unread_char);

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG2, s_unread_char);


  c = SCM_ICHR (cobj);

  scm_ungetc (c, port);
  return cobj;
}

SCM_PROC (s_port_line, "port-line", 0, 1, 0, scm_port_line);

SCM 
scm_port_line (port)
     SCM port;
{
  SCM p;

  port = SCM_COERCE_OUTPORT (port);

  p = ((port == SCM_UNDEFINED)
       ? scm_cur_inp
       : port);
  if (!(SCM_NIMP (p) && SCM_PORTP (p)))
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (SCM_LINUM (p));
}

SCM_PROC (s_set_port_line_x, "set-port-line!", 1, 1, 0, scm_set_port_line_x);

SCM 
scm_set_port_line_x (port, line)
     SCM port;
     SCM line;
{
  if (line == SCM_UNDEFINED)
    {
      line = port;
      port = scm_cur_inp;
    }
  else
    {
     port = SCM_COERCE_OUTPORT (port);
     SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
		 port,
		 SCM_ARG1,
		 s_set_port_line_x);
    }
  return SCM_PTAB_ENTRY (port)->line_number = SCM_INUM (line);
}

SCM_PROC (s_port_column, "port-column", 0, 1, 0, scm_port_column);

SCM
scm_port_column  (port)
     SCM port;
{
  SCM p;

  port = SCM_COERCE_OUTPORT (port);

  p = ((port == SCM_UNDEFINED)
       ? scm_cur_inp
       : port);
  if (!(SCM_NIMP (p) && SCM_PORTP (p)))
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (SCM_COL (p));
}

SCM_PROC (s_set_port_column_x, "set-port-column!", 1, 1, 0, scm_set_port_column_x);

SCM 
scm_set_port_column_x (port, column)
     SCM port;
     SCM column;
{
  if (column == SCM_UNDEFINED)
    {
      column = port;
      port = scm_cur_inp;
    }
  else
    {
      port = SCM_COERCE_OUTPORT (port);
      SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
		  port,
		  SCM_ARG1,
		  s_set_port_column_x);
    }
  return SCM_PTAB_ENTRY (port)->column_number = SCM_INUM (column);
}

SCM_PROC (s_port_filename, "port-filename", 0, 1, 0, scm_port_filename);

SCM 
scm_port_filename (port)
     SCM port;
{
  SCM p;
  
  port = SCM_COERCE_OUTPORT (port);

  p = ((port == SCM_UNDEFINED)
       ? scm_cur_inp
       : port);
  if (!(SCM_NIMP (p) && SCM_PORTP (p)))
    return SCM_BOOL_F;
  else
    return SCM_PTAB_ENTRY (p)->file_name;
}

SCM_PROC (s_set_port_filename_x, "set-port-filename!", 1, 1, 0, scm_set_port_filename_x);

SCM 
scm_set_port_filename_x (port, filename)
     SCM port;
     SCM filename;
{
  if (filename == SCM_UNDEFINED)
    {
      filename = port;
      port = scm_cur_inp;
    }
  else
    {
      port = SCM_COERCE_OUTPORT (port);
      SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
		  port,
		  SCM_ARG1,
		  s_set_port_filename_x);
    }
  return SCM_PTAB_ENTRY (port)->file_name = filename;
}

#ifndef ttyname
extern char * ttyname();
#endif


void 
scm_prinport (exp, port, type)
     SCM exp;
     SCM port;
     char *type;
{
  scm_puts ("#<", port);
  if (SCM_CLOSEDP (exp))
    scm_puts ("closed: ", port);
  else
    {
      if (SCM_RDNG & SCM_CAR (exp))
	scm_puts ("input: ", port);
      if (SCM_WRTNG & SCM_CAR (exp))
	scm_puts ("output: ", port);
    }
  scm_puts (type, port);
  scm_putc (' ', port);
#ifndef MSDOS
#ifndef __EMX__
#ifndef _DCC
#ifndef AMIGA
#ifndef THINK_C
  if (SCM_OPENP (exp) && scm_tc16_fport == SCM_TYP16 (exp) && isatty (fileno ((FILE *)SCM_STREAM (exp))))
    scm_puts (ttyname (fileno ((FILE *)SCM_STREAM (exp))), port);
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
  scm_putc ('>', port);
}


void
scm_ports_prehistory ()
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
print_void_port (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_prinport (exp, port, "void");
  return 1;
}

static int
putc_void_port (int c, SCM strm)
{
  return 0;			/* vestigial return value */
}

static int
puts_void_port (char *s, SCM strm)
{
  return 0;			/* vestigial return value */
}

static scm_sizet
write_void_port (char *ptr, scm_sizet size, scm_sizet nitems, SCM strm)
{
  int len;
  len = size * nitems;
  return len;
}


static int
flush_void_port (SCM strm)
{
  return 0;
}


static int
getc_void_port (SCM strm)
{
  return EOF;
}

static char *
fgets_void_port (SCM strm, int *len)
{
  return NULL;
}

static int
close_void_port (SCM strm)
{
  return 0;			/* this is ignored by scm_close_port. */
}



static int 
noop0 (SCM stream)
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
  fgets_void_port,
  close_void_port,
};




SCM
scm_void_port (mode_str)
     char * mode_str;
{
  int mode_bits;
  SCM answer;
  struct scm_port_table * pt;

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  mode_bits = scm_mode_bits (mode_str);
  pt = scm_add_to_port_table (answer);
  SCM_SETCAR (answer, scm_tc16_void_port | mode_bits);
  SCM_SETPTAB_ENTRY (answer, pt);
  SCM_SETSTREAM (answer, SCM_BOOL_F);
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC (s_sys_make_void_port, "%make-void-port", 1, 0, 0, scm_sys_make_void_port);

SCM
scm_sys_make_void_port (mode)
     SCM mode;
{
  SCM_ASSERT (SCM_NIMP (mode) && SCM_ROSTRINGP (mode), mode,
	      SCM_ARG1, s_sys_make_void_port);

  SCM_COERCE_SUBSTR (mode);
  return scm_void_port (SCM_ROCHARS (mode));
}






void
scm_init_ports ()
{
  scm_tc16_void_port = scm_newptob (&void_port_ptob);
#include "ports.x"
}

