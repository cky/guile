/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 *   2004, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
 *   2014 Free Software Foundation, Inc.
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



#define _LARGEFILE64_SOURCE      /* ask for stat64 etc */
#define _GNU_SOURCE              /* ask for LONG_LONG_MAX/LONG_LONG_MIN */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <fcntl.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <unistd.h>
#ifdef HAVE_IO_H
#include <io.h>
#endif
#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
#include <sys/stat.h>
#endif
#include <poll.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/select.h>

#include <full-write.h>

#include "libguile/_scm.h"
#include "libguile/strings.h"
#include "libguile/validate.h"
#include "libguile/gc.h"
#include "libguile/posix.h"
#include "libguile/dynwind.h"
#include "libguile/hashtab.h"

#include "libguile/fports.h"
#include "libguile/ports-internal.h"

#if SIZEOF_OFF_T == SIZEOF_INT
#define OFF_T_MAX  INT_MAX
#define OFF_T_MIN  INT_MIN
#elif SIZEOF_OFF_T == SIZEOF_LONG
#define OFF_T_MAX  LONG_MAX
#define OFF_T_MIN  LONG_MIN
#elif SIZEOF_OFF_T == SIZEOF_LONG_LONG
#define OFF_T_MAX  LONG_LONG_MAX
#define OFF_T_MIN  LONG_LONG_MIN
#else
#error Oops, unknown OFF_T size
#endif

scm_t_bits scm_tc16_fport;


/* default buffer size, used if the O/S won't supply a value.  */
static const size_t default_buffer_size = 1024;

/* Create FPORT buffers with specified sizes (or -1 to use default size
   or 0 for no buffer.)  */
static void
scm_fport_buffer_add (SCM port, long read_size, long write_size)
#define FUNC_NAME "scm_fport_buffer_add"
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (read_size == -1 || write_size == -1)
    {
      size_t default_size;
#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
      struct stat st;
      scm_t_fport *fp = SCM_FSTREAM (port);
      
      default_size = (fstat (fp->fdes, &st) == -1) ? default_buffer_size
	: st.st_blksize;
#else
      default_size = default_buffer_size;
#endif
      if (read_size == -1)
	read_size = default_size;
      if (write_size == -1)
	write_size = default_size;
    }

  if (SCM_INPUT_PORT_P (port) && read_size > 0)
    {
      pt->read_buf = scm_gc_malloc_pointerless (read_size, "port buffer");
      pt->read_pos = pt->read_end = pt->read_buf;
      pt->read_buf_size = read_size;
    }
  else
    {
      pt->read_pos = pt->read_buf = pt->read_end = &pt->shortbuf;
      pt->read_buf_size = 1;
    }

  if (SCM_OUTPUT_PORT_P (port) && write_size > 0)
    {
      pt->write_buf = scm_gc_malloc_pointerless (write_size, "port buffer");
      pt->write_pos = pt->write_buf;
      pt->write_buf_size = write_size;
    }
  else
    {
      pt->write_buf = pt->write_pos = &pt->shortbuf;
      pt->write_buf_size = 1;
    }

  pt->write_end = pt->write_buf + pt->write_buf_size;
  if (read_size > 0 || write_size > 0)
    SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) & ~SCM_BUF0);
  else
    SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) | SCM_BUF0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_setvbuf, "setvbuf", 2, 1, 0, 
            (SCM port, SCM mode, SCM size),
	    "Set the buffering mode for @var{port}.  @var{mode} can be:\n"
	    "@table @code\n"
	    "@item _IONBF\n"
	    "non-buffered\n"
	    "@item _IOLBF\n"
	    "line buffered\n"
	    "@item _IOFBF\n"
	    "block buffered, using a newly allocated buffer of @var{size} bytes.\n"
	    "If @var{size} is omitted, a default size will be used.\n"
	    "@end table\n\n"
	    "Only certain types of ports are supported, most importantly\n"
	    "file ports.")
#define FUNC_NAME s_scm_setvbuf
{
  int cmode;
  long csize;
  size_t ndrained;
  char *drained;
  scm_t_port *pt;
  scm_t_port_internal *pti;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPENPORT (1, port);
  pti = SCM_PORT_GET_INTERNAL (port);

  if (pti->setvbuf == NULL)
    scm_wrong_type_arg_msg (FUNC_NAME, 1, port,
			    "port that supports 'setvbuf'");

  cmode = scm_to_int (mode);
  if (cmode != _IONBF && cmode != _IOFBF && cmode != _IOLBF)
    scm_out_of_range (FUNC_NAME, mode);

  if (cmode == _IOLBF)
    {
      SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) | SCM_BUFLINE);
      cmode = _IOFBF;
    }
  else
    SCM_SET_CELL_WORD_0 (port,
			 SCM_CELL_WORD_0 (port) & ~(scm_t_bits) SCM_BUFLINE);

  if (SCM_UNBNDP (size))
    {
      if (cmode == _IOFBF)
	csize = -1;
      else
	csize = 0;
    }
  else
    {
      csize = scm_to_int (size);
      if (csize < 0 || (cmode == _IONBF && csize > 0))
	scm_out_of_range (FUNC_NAME, size);
    }

  pt = SCM_PTAB_ENTRY (port);

  if (SCM_INPUT_PORT_P (port))
    {
      /* Drain pending input from PORT.  Don't use `scm_drain_input' since
	 it returns a string, whereas we want binary input here.  */
      ndrained = pt->read_end - pt->read_pos;
      if (pt->read_buf == pt->putback_buf)
	ndrained += pt->saved_read_end - pt->saved_read_pos;

      if (ndrained > 0)
	{
	  drained = scm_gc_malloc_pointerless (ndrained, "file port");
	  scm_take_from_input_buffers (port, drained, ndrained);
	}
    }
  else
    ndrained = 0;

  if (SCM_OUTPUT_PORT_P (port))
    scm_flush (port);

  if (pt->read_buf == pt->putback_buf)
    {
      pt->read_buf = pt->saved_read_buf;
      pt->read_pos = pt->saved_read_pos;
      pt->read_end = pt->saved_read_end;
      pt->read_buf_size = pt->saved_read_buf_size;
    }

  pti->setvbuf (port, csize, csize);

  if (ndrained > 0)
    /* Put DRAINED back to PORT.  */
    scm_unget_bytes ((unsigned char *) drained, ndrained, port);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Move ports with the specified file descriptor to new descriptors,
 * resetting the revealed count to 0.
 */
static void
scm_i_evict_port (void *closure, SCM port)
{
  int fd = * (int*) closure;

  if (SCM_FPORTP (port))
    {
      scm_t_port *p;
      scm_t_fport *fp;

      /* XXX: In some cases, we can encounter a port with no associated ptab
	 entry.  */
      p = SCM_PTAB_ENTRY (port);
      fp = (p != NULL) ? (scm_t_fport *) p->stream : NULL;

      if ((fp != NULL) && (fp->fdes == fd))
	{
	  fp->fdes = dup (fd);
	  if (fp->fdes == -1)
	    scm_syserror ("scm_evict_ports");
	  scm_set_port_revealed_x (port, scm_from_int (0));
	}
    }
}

void
scm_evict_ports (int fd)
{
  scm_c_port_for_each (scm_i_evict_port, (void *) &fd);
}


SCM_DEFINE (scm_file_port_p, "file-port?", 1, 0, 0,
	    (SCM obj),
	    "Determine whether @var{obj} is a port that is related to a file.")
#define FUNC_NAME s_scm_file_port_p
{
  return scm_from_bool (SCM_FPORTP (obj));
}
#undef FUNC_NAME


static SCM sys_file_port_name_canonicalization;
SCM_SYMBOL (sym_relative, "relative");
SCM_SYMBOL (sym_absolute, "absolute");

static SCM
fport_canonicalize_filename (SCM filename)
{
  SCM mode = scm_fluid_ref (sys_file_port_name_canonicalization);

  if (!scm_is_string (filename))
    {
      return filename;
    }
  else if (scm_is_eq (mode, sym_relative))
    {
      SCM path, rel;

      path = scm_variable_ref (scm_c_module_lookup (scm_the_root_module (),
                                                    "%load-path"));
      rel = scm_i_relativize_path (filename, path);

      return scm_is_true (rel) ? rel : filename;
    }
  else if (scm_is_eq (mode, sym_absolute))
    {
      char *str, *canon;
  
      str = scm_to_locale_string (filename);
      canon = canonicalize_file_name (str);
      free (str);
  
      return canon ? scm_take_locale_string (canon) : filename;
    }
  else
    {
      return filename;
    }
}

/* scm_open_file_with_encoding
   Return a new port open on a given file.

   The mode string must match the pattern: [rwa+]** which
   is interpreted in the usual unix way.

   Unless binary mode is requested, the character encoding of the new
   port is determined as follows: First, if GUESS_ENCODING is true,
   'file-encoding' is used to guess the encoding of the file.  If
   GUESS_ENCODING is false or if 'file-encoding' fails, ENCODING is used
   unless it is also false.  As a last resort, the default port encoding
   is used.  It is an error to pass a non-false GUESS_ENCODING or
   ENCODING if binary mode is requested.

   Return the new port. */
SCM
scm_open_file_with_encoding (SCM filename, SCM mode,
                             SCM guess_encoding, SCM encoding)
#define FUNC_NAME "open-file"
{
  SCM port;
  int fdes, flags = 0, binary = 0;
  unsigned int retries;
  char *file, *md, *ptr;

  if (SCM_UNLIKELY (!(scm_is_false (encoding) || scm_is_string (encoding))))
    scm_wrong_type_arg_msg (FUNC_NAME, 0, encoding,
                            "encoding to be string or false");

  scm_dynwind_begin (0);

  file = scm_to_locale_string (filename);
  scm_dynwind_free (file);

  md = scm_to_locale_string (mode);
  scm_dynwind_free (md);

  switch (*md)
    {
    case 'r':
      flags |= O_RDONLY;
      break;
    case 'w':
      flags |= O_WRONLY | O_CREAT | O_TRUNC;
      break;
    case 'a':
      flags |= O_WRONLY | O_CREAT | O_APPEND;
      break;
    default:
      scm_out_of_range (FUNC_NAME, mode);
    }
  ptr = md + 1;
  while (*ptr != '\0')
    {
      switch (*ptr)
	{
	case '+':
	  flags = (flags & ~(O_RDONLY | O_WRONLY)) | O_RDWR;
	  break;
	case 'b':
	  binary = 1;
#if defined (O_BINARY)
	  flags |= O_BINARY;
#endif
	  break;
	case '0':  /* unbuffered: handled later.  */
	case 'l':  /* line buffered: handled during output.  */
	  break;
	default:
	  scm_out_of_range (FUNC_NAME, mode);
	}
      ptr++;
    }

  for (retries = 0, fdes = -1;
       fdes < 0 && retries < 2;
       retries++)
    {
      SCM_SYSCALL (fdes = open_or_open64 (file, flags, 0666));
      if (fdes == -1)
	{
	  int en = errno;

	  if (en == EMFILE && retries == 0)
	    /* Run the GC in case it collects open file ports that are no
	       longer referenced.  */
	    scm_i_gc (FUNC_NAME);
	  else
	    SCM_SYSERROR_MSG ("~A: ~S",
			      scm_cons (scm_strerror (scm_from_int (en)),
					scm_cons (filename, SCM_EOL)), en);
	}
    }

  /* Create a port from this file descriptor.  The port's encoding is initially
     %default-port-encoding.  */
  port = scm_i_fdes_to_port (fdes, scm_i_mode_bits (mode),
                             fport_canonicalize_filename (filename));

  if (binary)
    {
      if (scm_is_true (encoding))
        scm_misc_error (FUNC_NAME,
                        "Encoding specified on a binary port",
                        scm_list_1 (encoding));
      if (scm_is_true (guess_encoding))
        scm_misc_error (FUNC_NAME,
                        "Request to guess encoding on a binary port",
                        SCM_EOL);

      /* Use the binary-friendly ISO-8859-1 encoding. */
      scm_i_set_port_encoding_x (port, NULL);
    }
  else
    {
      char *enc = NULL;

      if (scm_is_true (guess_encoding))
        {
          if (SCM_INPUT_PORT_P (port))
            enc = scm_i_scan_for_encoding (port);
          else
            scm_misc_error (FUNC_NAME,
                            "Request to guess encoding on an output-only port",
                            SCM_EOL);
        }

      if (!enc && scm_is_true (encoding))
        {
          char *buf = scm_to_latin1_string (encoding);
          enc = scm_gc_strdup (buf, "encoding");
          free (buf);
        }

      if (enc)
        scm_i_set_port_encoding_x (port, enc);
    }

  scm_dynwind_end ();

  return port;
}
#undef FUNC_NAME

SCM
scm_open_file (SCM filename, SCM mode)
{
  return scm_open_file_with_encoding (filename, mode, SCM_BOOL_F, SCM_BOOL_F);
}

/* We can't define these using SCM_KEYWORD, because keywords have not
   yet been initialized when scm_init_fports is called.  */
static SCM k_guess_encoding = SCM_UNDEFINED;
static SCM k_encoding       = SCM_UNDEFINED;

SCM_DEFINE (scm_i_open_file, "open-file", 2, 0, 1,
	    (SCM filename, SCM mode, SCM keyword_args),
	    "Open the file whose name is @var{filename}, and return a port\n"
	    "representing that file.  The attributes of the port are\n"
	    "determined by the @var{mode} string.  The way in which this is\n"
	    "interpreted is similar to C stdio.  The first character must be\n"
	    "one of the following:\n"
	    "@table @samp\n"
	    "@item r\n"
	    "Open an existing file for input.\n"
	    "@item w\n"
	    "Open a file for output, creating it if it doesn't already exist\n"
	    "or removing its contents if it does.\n"
	    "@item a\n"
	    "Open a file for output, creating it if it doesn't already\n"
	    "exist.  All writes to the port will go to the end of the file.\n"
	    "The \"append mode\" can be turned off while the port is in use\n"
	    "@pxref{Ports and File Descriptors, fcntl}\n"
	    "@end table\n"
	    "The following additional characters can be appended:\n"
	    "@table @samp\n"
	    "@item b\n"
	    "Open the underlying file in binary mode, if supported by the system.\n"
	    "Also, open the file using the binary-compatible character encoding\n"
	    "\"ISO-8859-1\", ignoring the default port encoding.\n"
	    "@item +\n"
	    "Open the port for both input and output.  E.g., @code{r+}: open\n"
	    "an existing file for both input and output.\n"
	    "@item 0\n"
	    "Create an \"unbuffered\" port.  In this case input and output\n"
	    "operations are passed directly to the underlying port\n"
	    "implementation without additional buffering.  This is likely to\n"
	    "slow down I/O operations.  The buffering mode can be changed\n"
	    "while a port is in use @pxref{Ports and File Descriptors,\n"
	    "setvbuf}\n"
	    "@item l\n"
	    "Add line-buffering to the port.  The port output buffer will be\n"
	    "automatically flushed whenever a newline character is written.\n"
	    "@end table\n"
	    "In theory we could create read/write ports which were buffered\n"
	    "in one direction only.  However this isn't included in the\n"
	    "current interfaces.  If a file cannot be opened with the access\n"
	    "requested, @code{open-file} throws an exception.")
#define FUNC_NAME s_scm_i_open_file
{
  SCM encoding = SCM_BOOL_F;
  SCM guess_encoding = SCM_BOOL_F;

  scm_c_bind_keyword_arguments (FUNC_NAME, keyword_args, 0,
                                k_guess_encoding, &guess_encoding,
                                k_encoding, &encoding,
                                SCM_UNDEFINED);

  return scm_open_file_with_encoding (filename, mode,
                                      guess_encoding, encoding);
}
#undef FUNC_NAME


/* Building Guile ports from a file descriptor.  */

/* Build a Scheme port from an open file descriptor `fdes'.
   MODE indicates whether FILE is open for reading or writing; it uses
      the same notation as open-file's second argument.
   NAME is a string to be used as the port's filename.
*/
SCM
scm_i_fdes_to_port (int fdes, long mode_bits, SCM name)
#define FUNC_NAME "scm_fdes_to_port"
{
  SCM port;
  scm_t_port *pt;
  scm_t_port_internal *pti;

  /* Test that fdes is valid.  */
#ifdef F_GETFL
  int flags = fcntl (fdes, F_GETFL, 0);
  if (flags == -1)
    SCM_SYSERROR;
  flags &= O_ACCMODE;
  if (flags != O_RDWR
      && ((flags != O_WRONLY && (mode_bits & SCM_WRTNG))
	  || (flags != O_RDONLY && (mode_bits & SCM_RDNG))))
    {
      SCM_MISC_ERROR ("requested file mode not available on fdes", SCM_EOL);
    }
#else
  /* If we don't have F_GETFL, as on mingw, at least we can test that
     it is a valid file descriptor.  */
  struct stat st;
  if (fstat (fdes, &st) != 0)
    SCM_SYSERROR;
#endif

  scm_i_scm_pthread_mutex_lock (&scm_i_port_table_mutex);

  port = scm_new_port_table_entry (scm_tc16_fport);
  SCM_SET_CELL_TYPE(port, scm_tc16_fport | mode_bits);
  pt = SCM_PTAB_ENTRY (port);

  /* File ports support 'setvbuf'.  */
  pti = SCM_PORT_GET_INTERNAL (port);
  pti->setvbuf = scm_fport_buffer_add;

  {
    scm_t_fport *fp
      = (scm_t_fport *) scm_gc_malloc_pointerless (sizeof (scm_t_fport),
						   "file port");

    fp->fdes = fdes;
    pt->rw_random = SCM_FDES_RANDOM_P (fdes);
    SCM_SETSTREAM (port, fp);
    if (mode_bits & SCM_BUF0)
      scm_fport_buffer_add (port, 0, 0);
    else
      scm_fport_buffer_add (port, -1, -1);
  }
  SCM_SET_FILENAME (port, name);
  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);
  return port;
}
#undef FUNC_NAME

SCM
scm_fdes_to_port (int fdes, char *mode, SCM name)
{
  return scm_i_fdes_to_port (fdes, scm_mode_bits (mode), name);
}

/* Return a lower bound on the number of bytes available for input.  */
static int
fport_input_waiting (SCM port)
{
  int fdes = SCM_FSTREAM (port)->fdes;

  struct pollfd pollfd = { fdes, POLLIN, 0 };

  if (poll (&pollfd, 1, 0) < 0)
    scm_syserror ("fport_input_waiting");

  return pollfd.revents & POLLIN ? 1 : 0;
}


static int 
fport_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<", port);
  scm_print_port_mode (exp, port);    
  if (SCM_OPFPORTP (exp))
    {
      int fdes;
      SCM name = SCM_FILENAME (exp);
      if (scm_is_string (name) || scm_is_symbol (name))
	scm_display (name, port);
      else
	scm_puts (SCM_PTOBNAME (SCM_PTOBNUM (exp)), port);
      scm_putc (' ', port);
      fdes = (SCM_FSTREAM (exp))->fdes;

#if (defined HAVE_TTYNAME) && (defined HAVE_POSIX)
      if (isatty (fdes))
	scm_display (scm_ttyname (exp), port);
      else
#endif /* HAVE_TTYNAME */
	scm_intprint (fdes, 10, port);
    }
  else
    {
      scm_puts (SCM_PTOBNAME (SCM_PTOBNUM (exp)), port);
      scm_putc (' ', port);
      scm_uintprint ((scm_t_bits) SCM_PTAB_ENTRY (exp), 16, port);
    }
  scm_putc ('>', port);
  return 1;
}

static void fport_flush (SCM port);

/* fill a port's read-buffer with a single read.  returns the first
   char or EOF if end of file.  */
static scm_t_wchar
fport_fill_input (SCM port)
{
  long count;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_fport *fp = SCM_FSTREAM (port);

  SCM_SYSCALL (count = read (fp->fdes, pt->read_buf, pt->read_buf_size));
  if (count == -1)
    scm_syserror ("fport_fill_input");
  if (count == 0)
    return (scm_t_wchar) EOF;
  else
    {
      pt->read_pos = pt->read_buf;
      pt->read_end = pt->read_buf + count;
      return *pt->read_buf;
    }
}

static scm_t_off
fport_seek (SCM port, scm_t_off offset, int whence)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_fport *fp = SCM_FSTREAM (port);
  off_t_or_off64_t rv;
  off_t_or_off64_t result;

  if (pt->rw_active == SCM_PORT_WRITE)
    {
      if (offset != 0 || whence != SEEK_CUR)
	{
	  fport_flush (port);
	  result = rv = lseek_or_lseek64 (fp->fdes, offset, whence);
	}
      else
	{
	  /* read current position without disturbing the buffer.  */
	  rv = lseek_or_lseek64 (fp->fdes, offset, whence);
	  result = rv + (pt->write_pos - pt->write_buf);
	}
    }
  else if (pt->rw_active == SCM_PORT_READ)
    {
      if (offset != 0 || whence != SEEK_CUR)
	{
	  /* could expand to avoid a second seek.  */
	  scm_end_input (port);
	  result = rv = lseek_or_lseek64 (fp->fdes, offset, whence);
	}
      else
	{
	  /* read current position without disturbing the buffer
	     (particularly the unread-char buffer).  */
	  rv = lseek_or_lseek64 (fp->fdes, offset, whence);
	  result = rv - (pt->read_end - pt->read_pos);

	  if (pt->read_buf == pt->putback_buf)
	    result -= pt->saved_read_end - pt->saved_read_pos;
	}
    }
  else /* SCM_PORT_NEITHER */
    {
      result = rv = lseek_or_lseek64 (fp->fdes, offset, whence);
    }

  if (rv == -1)
    scm_syserror ("fport_seek");

  return result;
}

static void
fport_truncate (SCM port, scm_t_off length)
{
  scm_t_fport *fp = SCM_FSTREAM (port);

  if (ftruncate (fp->fdes, length) == -1)
    scm_syserror ("ftruncate");
}

static void
fport_write (SCM port, const void *data, size_t size)
#define FUNC_NAME "fport_write"
{
  /* this procedure tries to minimize the number of writes/flushes.  */
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->write_buf == &pt->shortbuf
      || (pt->write_pos == pt->write_buf && size >= pt->write_buf_size))
    {
      /* Unbuffered port, or port with empty buffer and data won't fit in
	 buffer.  */
      if (full_write (SCM_FPORT_FDES (port), data, size) < size)
	SCM_SYSERROR;

      return;
    }

  {
    scm_t_off space = pt->write_end - pt->write_pos;

    if (size <= space)
      {
	/* data fits in buffer.  */
	memcpy (pt->write_pos, data, size);
	pt->write_pos += size;
	if (pt->write_pos == pt->write_end)
	  {
	    fport_flush (port);
	    /* we can skip the line-buffering check if nothing's buffered. */
	    return;
	  }
      }
    else
      {
	memcpy (pt->write_pos, data, space);
	pt->write_pos = pt->write_end;
	fport_flush (port);
	{
	  const void *ptr = ((const char *) data) + space;
	  size_t remaining = size - space;

	  if (size >= pt->write_buf_size)
	    {
	      if (full_write (SCM_FPORT_FDES (port), ptr, remaining)
		  < remaining)
		SCM_SYSERROR;
	      return;
	    }
	  else
	    {
	      memcpy (pt->write_pos, ptr, remaining);
	      pt->write_pos += remaining;
	    }
	}
      }

    /* handle line buffering.  */     
    if ((SCM_CELL_WORD_0 (port) & SCM_BUFLINE) && memchr (data, '\n', size))
      fport_flush (port);
  }
}
#undef FUNC_NAME

static void
fport_flush (SCM port)
{
  size_t written;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_fport *fp = SCM_FSTREAM (port);
  size_t count = pt->write_pos - pt->write_buf;

  written = full_write (fp->fdes, pt->write_buf, count);
  if (written < count)
    scm_syserror ("scm_flush");

  pt->write_pos = pt->write_buf;
  pt->rw_active = SCM_PORT_NEITHER;
}

/* clear the read buffer and adjust the file position for unread bytes. */
static void
fport_end_input (SCM port, int offset)
{
  scm_t_fport *fp = SCM_FSTREAM (port);
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  
  offset += pt->read_end - pt->read_pos;

  if (offset > 0)
    {
      pt->read_pos = pt->read_end;
      /* will throw error if unread-char used at beginning of file
	 then attempting to write.  seems correct.  */
      if (lseek (fp->fdes, -offset, SEEK_CUR) == -1)
	scm_syserror ("fport_end_input");
    }
  pt->rw_active = SCM_PORT_NEITHER;
}

static int
fport_close (SCM port)
{
  scm_t_fport *fp = SCM_FSTREAM (port);
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  int rv;

  fport_flush (port);
  SCM_SYSCALL (rv = close (fp->fdes));
  if (rv == -1 && errno != EBADF)
    {
      if (scm_gc_running_p)
	/* silently ignore the error.  scm_error would abort if we
	   called it now.  */
	;
      else
	scm_syserror ("fport_close");
    }
  if (pt->read_buf == pt->putback_buf)
    pt->read_buf = pt->saved_read_buf;
  if (pt->read_buf != &pt->shortbuf)
    scm_gc_free (pt->read_buf, pt->read_buf_size, "port buffer");
  if (pt->write_buf != &pt->shortbuf)
    scm_gc_free (pt->write_buf, pt->write_buf_size, "port buffer");
  scm_gc_free (fp, sizeof (*fp), "file port");
  return rv;
}

static size_t
fport_free (SCM port)
{
  fport_close (port);
  return 0;
}

static scm_t_bits
scm_make_fptob ()
{
  scm_t_bits tc = scm_make_port_type ("file", fport_fill_input, fport_write);

  scm_set_port_free            (tc, fport_free);
  scm_set_port_print           (tc, fport_print);
  scm_set_port_flush           (tc, fport_flush);
  scm_set_port_end_input       (tc, fport_end_input);
  scm_set_port_close           (tc, fport_close);
  scm_set_port_seek            (tc, fport_seek);
  scm_set_port_truncate        (tc, fport_truncate);
  scm_set_port_input_waiting   (tc, fport_input_waiting);

  return tc;
}

/* We can't initialize the keywords from 'scm_init_fports', because
   keywords haven't yet been initialized at that point.  */
void
scm_init_fports_keywords ()
{
  k_guess_encoding = scm_from_latin1_keyword ("guess-encoding");
  k_encoding       = scm_from_latin1_keyword ("encoding");
}

void
scm_init_fports ()
{
  scm_tc16_fport = scm_make_fptob ();

  scm_c_define ("_IOFBF", scm_from_int (_IOFBF));
  scm_c_define ("_IOLBF", scm_from_int (_IOLBF));
  scm_c_define ("_IONBF", scm_from_int (_IONBF));

  sys_file_port_name_canonicalization = scm_make_fluid ();
  scm_c_define ("%file-port-name-canonicalization",
                sys_file_port_name_canonicalization);
                                    
#include "libguile/fports.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
