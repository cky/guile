/* classes: h_files */

#ifndef SCM_PORTS_H
#define SCM_PORTS_H

/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001 Free Software Foundation, Inc.
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

/* Not sure if this is a good idea.  We need it for off_t.  */
#include <sys/types.h>



#define SCM_INITIAL_PUTBACK_BUF_SIZE 4

/* values for the rw_active flag.  */
typedef enum scm_t_port_rw_active {
  SCM_PORT_NEITHER = 0,
  SCM_PORT_READ = 1,
  SCM_PORT_WRITE = 2
} scm_t_port_rw_active;

/* C representation of a Scheme port.  */

typedef struct 
{
  SCM port;			/* Link back to the port object.  */
  long entry;			/* Index in port table. */
  int revealed;			/* 0 not revealed, > 1 revealed.
				 * Revealed ports do not get GC'd.
				 */
  /* data for the underlying port implementation as a raw C value. */
  scm_t_bits stream;

  SCM file_name;		/* debugging support.  */
  long line_number;		/* debugging support.  */
  int column_number;		/* debugging support.  */

  /* port buffers.  the buffer(s) are set up for all ports.  
     in the case of string ports, the buffer is the string itself.
     in the case of unbuffered file ports, the buffer is a
     single char: shortbuf.  */

  /* this buffer is filled from read_buf to read_end using the ptob 
     buffer_fill.  then input requests are taken from read_pos until
     it reaches read_end.  */

  unsigned char *read_buf;	/* buffer start.  */
  const unsigned char *read_pos;/* the next unread char.  */
  unsigned char *read_end;      /* pointer to last buffered char + 1.  */
  off_t read_buf_size;		/* size of the buffer.  */

  /* when chars are put back into the buffer, e.g., using peek-char or
     unread-string, the read-buffer pointers are switched to cbuf.
     the original pointers are saved here and restored when the put-back
     chars have been consumed.  */
  unsigned char *saved_read_buf;
  const unsigned char *saved_read_pos;
  unsigned char *saved_read_end;
  off_t saved_read_buf_size;

  /* write requests are saved into this buffer at write_pos until it
     reaches write_buf + write_buf_size, then the ptob flush is
     called.  */

  unsigned char *write_buf;     /* buffer start.  */
  unsigned char *write_pos;     /* pointer to last buffered char + 1.  */
  unsigned char *write_end;     /* pointer to end of buffer + 1.  */
  off_t write_buf_size;		/* size of the buffer.  */

  unsigned char shortbuf;       /* buffer for "unbuffered" streams.  */

  int rw_random;                /* true if the port is random access.
				   implies that the buffers must be
				   flushed before switching between
				   reading and writing, seeking, etc.  */

  scm_t_port_rw_active rw_active; /* for random access ports,
                                     indicates which of the buffers
                                     is currently in use.  can be
                                     SCM_PORT_WRITE, SCM_PORT_READ,
                                     or SCM_PORT_NEITHER.  */


  /* a buffer for un-read chars and strings.  */
  unsigned char *putback_buf;
  size_t putback_buf_size;        /* allocated size of putback_buf.  */
} scm_t_port;

SCM_API scm_t_port **scm_port_table;
SCM_API long scm_port_table_size; /* Number of ports in scm_port_table.  */

#define SCM_READ_BUFFER_EMPTY_P(c_port) (c_port->read_pos >= c_port->read_end)



#define SCM_EOF_OBJECT_P(x) (SCM_EQ_P ((x), SCM_EOF_VAL))

/* PORT FLAGS
 * A set of flags characterizes a port.
 * Note that we reserve the bits 1 << 24 and above for use by the
 * routines in the port's scm_ptobfuns structure.
 */
#define SCM_OPN		(1L<<16) /* Is the port open? */
#define SCM_RDNG	(2L<<16) /* Is it a readable port? */
#define SCM_WRTNG	(4L<<16) /* Is it writable? */
#define SCM_BUF0	(8L<<16) /* Is it unbuffered? */
#define SCM_BUFLINE     (64L<<16) /* Is it line-buffered? */

#define SCM_PORTP(x) (!SCM_IMP (x) && (SCM_TYP7 (x) == scm_tc7_port))
#define SCM_OPPORTP(x) (!SCM_IMP(x) && (((0x7f | SCM_OPN) & SCM_CELL_WORD_0(x))==(scm_tc7_port | SCM_OPN)))
#define SCM_OPINPORTP(x) (!SCM_IMP(x) && (((0x7f | SCM_OPN | SCM_RDNG) & SCM_CELL_WORD_0(x))==(scm_tc7_port | SCM_OPN | SCM_RDNG)))
#define SCM_OPOUTPORTP(x) (!SCM_IMP(x) && (((0x7f | SCM_OPN | SCM_WRTNG) & SCM_CELL_WORD_0(x))==(scm_tc7_port | SCM_OPN | SCM_WRTNG)))
#define SCM_INPUT_PORT_P(x) \
  (!SCM_IMP(x) \
   && (((0x7f | SCM_RDNG) & SCM_CELL_WORD_0(x)) == (scm_tc7_port | SCM_RDNG)))
#define SCM_OUTPUT_PORT_P(x) \
  (!SCM_IMP(x) \
   && (((0x7f | SCM_WRTNG) & SCM_CELL_WORD_0(x))==(scm_tc7_port | SCM_WRTNG)))
#define SCM_OPENP(x) (!SCM_IMP(x) && (SCM_OPN & SCM_CELL_WORD_0 (x)))
#define SCM_CLOSEDP(x) (!SCM_OPENP(x))
#define SCM_CLR_PORT_OPEN_FLAG(p) \
  SCM_SET_CELL_WORD_0 ((p), SCM_CELL_WORD_0 (p) & ~SCM_OPN)

#define SCM_PTAB_ENTRY(x)         ((scm_t_port *) SCM_CELL_WORD_1 (x))
#define SCM_SETPTAB_ENTRY(x,ent)  (SCM_SET_CELL_WORD_1 ((x), (scm_t_bits) (ent)))
#define SCM_STREAM(x)             (SCM_PTAB_ENTRY(x)->stream)
#define SCM_SETSTREAM(x,s)        (SCM_PTAB_ENTRY(x)->stream = (scm_t_bits) (s))
#define SCM_FILENAME(x)           (SCM_PTAB_ENTRY(x)->file_name)
#define SCM_SET_FILENAME(x, n)    (SCM_PTAB_ENTRY(x)->file_name = (n))
#define SCM_LINUM(x)              (SCM_PTAB_ENTRY(x)->line_number)
#define SCM_COL(x)                (SCM_PTAB_ENTRY(x)->column_number)
#define SCM_REVEALED(x)           (SCM_PTAB_ENTRY(x)->revealed)
#define SCM_SETREVEALED(x,s)      (SCM_PTAB_ENTRY(x)->revealed = (s))

#define SCM_INCLINE(port)  	{SCM_LINUM (port) += 1; SCM_COL (port) = 0;}
#define SCM_INCCOL(port)  	{SCM_COL (port) += 1;}
#define SCM_TABCOL(port)  	{SCM_COL (port) += 8 - SCM_COL (port) % 8;}



/* port-type description.  */
typedef struct scm_t_ptob_descriptor
{
  char *name;
  SCM (*mark) (SCM);
  size_t (*free) (SCM);
  int (*print) (SCM exp, SCM port, scm_print_state *pstate);
  SCM (*equalp) (SCM, SCM);
  int (*close) (SCM port);

  void (*write) (SCM port, const void *data, size_t size);
  void (*flush) (SCM port);

  void (*end_input) (SCM port, int offset);
  int (*fill_input) (SCM port);
  int (*input_waiting) (SCM port);

  off_t (*seek) (SCM port, off_t OFFSET, int WHENCE);
  void (*truncate) (SCM port, off_t length);

} scm_t_ptob_descriptor;

#define SCM_TC2PTOBNUM(x) (0x0ff & ((x) >> 8))
#define SCM_PTOBNUM(x) (SCM_TC2PTOBNUM (SCM_CELL_TYPE (x)))
/* SCM_PTOBNAME can be 0 if name is missing */
#define SCM_PTOBNAME(ptobnum) scm_ptobs[ptobnum].name



SCM_API scm_t_ptob_descriptor *scm_ptobs;
SCM_API long scm_numptob;
SCM_API long scm_port_table_room;



SCM_API SCM scm_markstream (SCM ptr);
SCM_API scm_t_bits scm_make_port_type (char *name,
				       int (*fill_input) (SCM port),
				       void (*write) (SCM port, 
						      const void *data,
						      size_t size));
SCM_API void scm_set_port_mark (long tc, SCM (*mark) (SCM));
SCM_API void scm_set_port_free (long tc, size_t (*free) (SCM));
SCM_API void scm_set_port_print (long tc,
				 int (*print) (SCM exp,
					       SCM port,
					       scm_print_state *pstate));
SCM_API void scm_set_port_equalp (long tc, SCM (*equalp) (SCM, SCM));
SCM_API void scm_set_port_close (long tc, int (*close) (SCM));

SCM_API void scm_set_port_flush (long tc, 
				 void (*flush) (SCM port));
SCM_API void scm_set_port_end_input (long tc,
				     void (*end_input) (SCM port,
							int offset));
SCM_API void scm_set_port_seek (long tc,
				off_t (*seek) (SCM port,
					       off_t OFFSET,
					       int WHENCE));
SCM_API void scm_set_port_truncate (long tc,
				    void (*truncate) (SCM port,
						      off_t length));
SCM_API void scm_set_port_input_waiting (long tc, int (*input_waiting) (SCM));
SCM_API SCM scm_char_ready_p (SCM port);
size_t scm_take_from_input_buffers (SCM port, char *dest, size_t read_len);
SCM_API SCM scm_drain_input (SCM port);
SCM_API SCM scm_current_input_port (void);
SCM_API SCM scm_current_output_port (void);
SCM_API SCM scm_current_error_port (void);
SCM_API SCM scm_current_load_port (void);
SCM_API SCM scm_set_current_input_port (SCM port);
SCM_API SCM scm_set_current_output_port (SCM port);
SCM_API SCM scm_set_current_error_port (SCM port);
SCM_API scm_t_port * scm_add_to_port_table (SCM port);
SCM_API void scm_remove_from_port_table (SCM port);
SCM_API void scm_grow_port_cbuf (SCM port, size_t requested);
SCM_API SCM scm_pt_size (void);
SCM_API SCM scm_pt_member (SCM member);
SCM_API void scm_port_non_buffer (scm_t_port *pt);
SCM_API int scm_revealed_count (SCM port);
SCM_API SCM scm_port_revealed (SCM port);
SCM_API SCM scm_set_port_revealed_x (SCM port, SCM rcount);
SCM_API long scm_mode_bits (char *modes);
SCM_API SCM scm_port_mode (SCM port);
SCM_API SCM scm_close_input_port (SCM port);
SCM_API SCM scm_close_output_port (SCM port);
SCM_API SCM scm_close_port (SCM port);
SCM_API SCM scm_port_for_each (SCM proc);
SCM_API SCM scm_input_port_p (SCM x);
SCM_API SCM scm_output_port_p (SCM x);
SCM_API SCM scm_port_p (SCM x);
SCM_API SCM scm_port_closed_p (SCM port);
SCM_API SCM scm_eof_object_p (SCM x);
SCM_API SCM scm_force_output (SCM port);
SCM_API SCM scm_flush_all_ports (void);
SCM_API SCM scm_read_char (SCM port);
SCM_API void scm_putc (char c, SCM port);
SCM_API void scm_puts (const char *str_data, SCM port);
SCM_API size_t scm_c_read (SCM port, void *buffer, size_t size);
SCM_API void scm_c_write (SCM port, const void *buffer, size_t size);
SCM_API void scm_lfwrite (const char *ptr, size_t size, SCM port);
SCM_API void scm_flush (SCM port);
SCM_API void scm_end_input (SCM port);
SCM_API int scm_fill_input (SCM port);
SCM_API int scm_getc (SCM port);
SCM_API void scm_ungetc (int c, SCM port);
SCM_API void scm_ungets (const char *s, int n, SCM port);
SCM_API SCM scm_peek_char (SCM port);
SCM_API SCM scm_unread_char (SCM cobj, SCM port);
SCM_API SCM scm_unread_string (SCM str, SCM port);
SCM_API SCM scm_seek (SCM object, SCM offset, SCM whence);
SCM_API SCM scm_truncate_file (SCM object, SCM length);
SCM_API SCM scm_port_line (SCM port);
SCM_API SCM scm_set_port_line_x (SCM port, SCM line);
SCM_API SCM scm_port_column (SCM port);
SCM_API SCM scm_set_port_column_x (SCM port, SCM line);
SCM_API SCM scm_port_filename (SCM port);
SCM_API SCM scm_set_port_filename_x (SCM port, SCM filename);
SCM_API int scm_port_print (SCM exp, SCM port, scm_print_state *);
SCM_API void scm_print_port_mode (SCM exp, SCM port);
SCM_API void scm_ports_prehistory (void);
SCM_API SCM scm_void_port (char * mode_str);
SCM_API SCM scm_sys_make_void_port (SCM mode);
SCM_API void scm_init_ports (void);

#ifdef GUILE_DEBUG
SCM_API SCM scm_pt_size (void);
SCM_API SCM scm_pt_member (SCM member);
#endif /* GUILE_DEBUG */

#endif  /* SCM_PORTS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
