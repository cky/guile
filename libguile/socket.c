/*	Copyright (C) 1996 Free Software Foundation, Inc.
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
#include "filesys.h"
#include "unif.h"
#include "feature.h"
#include "fports.h"

#include "socket.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>



static SCM scm_sock_fd_to_port SCM_P ((int fd, char *proc));

static SCM
scm_sock_fd_to_port (fd, proc)
     int fd;
     char *proc;
{
  SCM result;
  FILE *f;

  if (fd == -1)
    scm_syserror (proc);
  f = fdopen (fd, "r+");
  if (!f)
    {
      SCM_SYSCALL (close (fd));
      scm_syserror (proc);
    }
  SCM_NEWCELL (result);
  {
    struct scm_port_table *pt = scm_add_to_port_table (result);

    SCM_SETPTAB_ENTRY (result, pt);
  }
  SCM_SETCAR (result, scm_tc16_fport | scm_mode_bits ("r+0"));
  SCM_SETSTREAM (result, (SCM)f);
  scm_setbuf0 (result);
  return result;
}

SCM_PROC (s_socket, "socket", 3, 0, 0, scm_socket);

SCM 
scm_socket (family, style, proto)
     SCM family;
     SCM style;
     SCM proto;
{
  int fd;
  SCM result;

  SCM_ASSERT (SCM_INUMP (family), family, SCM_ARG1, s_socket);
  SCM_ASSERT (SCM_INUMP (style), style, SCM_ARG2, s_socket);
  SCM_ASSERT (SCM_INUMP (proto), proto, SCM_ARG3, s_socket);
  SCM_DEFER_INTS;
  fd = socket (SCM_INUM (family), SCM_INUM (style), SCM_INUM (proto));
  result = scm_sock_fd_to_port (fd, s_socket);
  SCM_ALLOW_INTS;
  return result;
}



SCM_PROC (s_socketpair, "socketpair", 3, 0, 0, scm_socketpair);

SCM 
scm_socketpair (family, style, proto)
     SCM family;
     SCM style;
     SCM proto;
{
  int fam;
  int fd[2];
  SCM a;
  SCM b;

  SCM_ASSERT (SCM_INUMP (family), family, SCM_ARG1, s_socketpair);
  SCM_ASSERT (SCM_INUMP (style), style, SCM_ARG2, s_socketpair);
  SCM_ASSERT (SCM_INUMP (proto), proto, SCM_ARG3, s_socketpair);

  fam = SCM_INUM (family);

  SCM_DEFER_INTS;
  if (socketpair (fam, SCM_INUM (style), SCM_INUM (proto), fd) == -1)
    scm_syserror (s_socketpair);

  a = scm_sock_fd_to_port (fd[0], s_socketpair);
  b = scm_sock_fd_to_port (fd[1], s_socketpair);
  SCM_ALLOW_INTS;
  return scm_cons (a, b);
}


SCM_PROC (s_getsockopt, "getsockopt", 3, 0, 0, scm_getsockopt);

SCM
scm_getsockopt (sock, level, optname)
     SCM sock;
     SCM level;
     SCM optname;
{
  int fd;
  int optlen;
#ifdef HAVE_STRUCT_LINGER
  char optval[sizeof (struct linger)];
#else
  char optval[sizeof (scm_sizet)];
#endif
  int ilevel;
  int ioptname;

#ifdef HAVE_STRUCT_LINGER
  optlen = (int) sizeof (struct linger);
#else
  optlen = (int) sizeof (scm_sizet);
#endif

  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1,
	      s_getsockopt);
  SCM_ASSERT (SCM_INUMP (level), level, SCM_ARG2, s_getsockopt);
  SCM_ASSERT (SCM_INUMP (optname), optname, SCM_ARG3, s_getsockopt);

  fd = fileno ((FILE *)SCM_STREAM (sock));
  ilevel = SCM_INUM (level);
  ioptname = SCM_INUM (optname);
  if (getsockopt (fd, ilevel, ioptname, (void *) optval, &optlen) == -1)
    scm_syserror (s_getsockopt);

#ifdef SO_LINGER
  if (ilevel == SOL_SOCKET && ioptname == SO_LINGER)
    {
#ifdef HAVE_STRUCT_LINGER
      struct linger *ling = (struct linger *) optval;
      return scm_cons (SCM_MAKINUM (ling->l_onoff),
		       SCM_MAKINUM (ling->l_linger));
#else
      scm_sizet *ling = (scm_sizet *) optval;
      return scm_cons (SCM_MAKINUM (*ling),
		       SCM_MAKINUM (0));
#endif
    }
#endif
#ifdef SO_SNDBUF
  if (ilevel == SOL_SOCKET && ioptname == SO_SNDBUF)
    {
      scm_sizet *bufsize = (scm_sizet *) optval;
      return SCM_MAKINUM (*bufsize);
    }
#endif
#ifdef SO_RCVBUF
  if (ilevel == SOL_SOCKET && ioptname == SO_RCVBUF)
    {
      scm_sizet *bufsize = (scm_sizet *) optval;
      return SCM_MAKINUM (*bufsize);
    }
#endif
  return SCM_MAKINUM (*(int *) optval);
}

SCM_PROC (s_setsockopt, "setsockopt", 4, 0, 0, scm_setsockopt);

SCM
scm_setsockopt (sock, level, optname, value)
     SCM sock;
     SCM level;
     SCM optname;
     SCM value;
{
  int fd;
  int optlen;
#ifdef HAVE_STRUCT_LINGER
  char optval[sizeof (struct linger)]; /* Biggest option :-(  */
#else
  char optval[sizeof (scm_sizet)];
#endif
  int ilevel, ioptname;
  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1,
	      s_setsockopt);
  SCM_ASSERT (SCM_INUMP (level), level, SCM_ARG2, s_setsockopt);
  SCM_ASSERT (SCM_INUMP (optname), optname, SCM_ARG3, s_setsockopt);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  ilevel = SCM_INUM (level);
  ioptname = SCM_INUM (optname);
  if (0);
#ifdef SO_LINGER
  else if (ilevel == SOL_SOCKET && ioptname == SO_LINGER)
    {
#ifdef HAVE_STRUCT_LINGER
      struct linger ling;
      SCM_ASSERT (SCM_NIMP (value) && SCM_CONSP (value)
		  && SCM_INUMP (SCM_CAR (value))
		  &&  SCM_INUMP (SCM_CDR (value)),
		  value, SCM_ARG4, s_setsockopt);
      ling.l_onoff = SCM_INUM (SCM_CAR (value));
      ling.l_linger = SCM_INUM (SCM_CDR (value));
      optlen = (int) sizeof (struct linger);
      memcpy (optval, (void *) &ling, optlen);
#else
      scm_sizet ling;
      SCM_ASSERT (SCM_NIMP (value) && SCM_CONSP (value)
		  && SCM_INUMP (SCM_CAR (value))
		  &&  SCM_INUMP (SCM_CDR (value)),
		  value, SCM_ARG4, s_setsockopt);
      ling = SCM_INUM (SCM_CAR (value));
      optlen = (int) sizeof (scm_sizet);
      (*(scm_sizet *) optval) = (scm_sizet) SCM_INUM (value);
#endif
    }
#endif
#ifdef SO_SNDBUF
  else if (ilevel == SOL_SOCKET && ioptname == SO_SNDBUF)
    {
      SCM_ASSERT (SCM_INUMP (value), value, SCM_ARG4, s_setsockopt);
      optlen = (int) sizeof (scm_sizet);
      (*(scm_sizet *) optval) = (scm_sizet) SCM_INUM (value);
    }
#endif
#ifdef SO_RCVBUF
  else if (ilevel == SOL_SOCKET && ioptname == SO_RCVBUF)
    {
      SCM_ASSERT (SCM_INUMP (value), value, SCM_ARG4, s_setsockopt);
      optlen = (int) sizeof (scm_sizet);
      (*(scm_sizet *) optval) = (scm_sizet) SCM_INUM (value);
    }
#endif
  else
    {
      /* Most options just take an int.  */
      SCM_ASSERT (SCM_INUMP (value), value, SCM_ARG4, s_setsockopt);
      optlen = (int) sizeof (int);
      (*(int *) optval) = (int) SCM_INUM (value);
    }
  if (setsockopt (fd, ilevel, ioptname, (void *) optval, optlen) == -1)
    scm_syserror (s_setsockopt);
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_shutdown, "shutdown", 2, 0, 0, scm_shutdown);

SCM 
scm_shutdown (sock, how)
     SCM sock;
     SCM how;
{
  int fd;
  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1,
	      s_shutdown);
  SCM_ASSERT (SCM_INUMP (how) && 0 <= SCM_INUM (how) && 2 >= SCM_INUM (how),
	  how, SCM_ARG2, s_shutdown);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  if (shutdown (fd, SCM_INUM (how)) == -1)
    scm_syserror (s_shutdown);
  return SCM_UNSPECIFIED;
}

/* convert fam/address/args into a sockaddr of the appropriate type.
   args is modified by removing the arguments actually used.
   which_arg and proc are used when reporting errors:
   which_arg is the position of address in the original argument list.
   proc is the name of the original procedure.
   size returns the size of the structure allocated.  */


static struct sockaddr * scm_fill_sockaddr SCM_P ((int fam, SCM address, SCM *args, int which_arg, char *proc, scm_sizet *size));

static struct sockaddr *
scm_fill_sockaddr (fam, address, args, which_arg, proc, size)
     int fam;
     SCM address;
     SCM *args;
     int which_arg;
     char *proc;
     scm_sizet *size;
{
  switch (fam)
    {
    case AF_INET:
      {
	SCM isport;
	struct sockaddr_in *soka;

	soka = (struct sockaddr_in *)
	  scm_must_malloc (sizeof (struct sockaddr_in), proc);
	soka->sin_family = AF_INET;
	soka->sin_addr.s_addr =
	  htonl (scm_num2ulong (address, (char *) which_arg, proc));
	SCM_ASSERT (SCM_NIMP (*args) && SCM_CONSP (*args), *args, 
		    which_arg + 1, proc);
	isport = SCM_CAR (*args);
	*args = SCM_CDR (*args);
	SCM_ASSERT (SCM_INUMP (isport), isport, which_arg + 1, proc);
	soka->sin_port = htons (SCM_INUM (isport));
	*size = sizeof (struct sockaddr_in);
	return (struct sockaddr *) soka;
      }
    case AF_UNIX:
      {
	struct sockaddr_un *soka;

	soka = (struct sockaddr_un *)
	  scm_must_malloc (sizeof (struct sockaddr_un), proc);
	soka->sun_family = AF_UNIX;
	SCM_ASSERT (SCM_NIMP (address) && SCM_STRINGP (address), address,
		which_arg, proc);
	memcpy (soka->sun_path, SCM_CHARS (address), 1 + SCM_LENGTH (address));
	*size = sizeof (struct sockaddr_un);
	return (struct sockaddr *) soka;
      }
    default:
      scm_out_of_range (proc, SCM_MAKINUM (fam));
    }
}
  
SCM_PROC (s_connect, "connect", 3, 0, 1, scm_connect);

SCM 
scm_connect (sock, fam, address, args)

     SCM sock;
     SCM fam;
     SCM address;
     SCM args;
{
  int fd;
  struct sockaddr *soka;
  scm_sizet size;

  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_connect);
  SCM_ASSERT (SCM_INUMP (fam), fam, SCM_ARG2, s_connect);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  SCM_DEFER_INTS;
  soka = scm_fill_sockaddr (SCM_INUM (fam), address, &args, 3, s_connect, &size);
  if (connect (fd, soka, size) == -1)
    scm_syserror (s_connect);
  scm_must_free ((char *) soka);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_bind, "bind", 3, 0, 1, scm_bind);

SCM 
scm_bind (sock, fam, address, args)
     SCM sock;
     SCM fam;
     SCM address;
     SCM args;
{
  int rv;
  struct sockaddr *soka;
  scm_sizet size;
  int fd;

  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_bind);
  SCM_ASSERT (SCM_INUMP (fam), fam, SCM_ARG2, s_bind);
  soka = scm_fill_sockaddr (SCM_INUM (fam), address, &args, 3, s_bind, &size);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  rv = bind (fd, soka, size);
  if (rv == -1)
    scm_syserror (s_bind);
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_listen, "listen", 2, 0, 0, scm_listen);

SCM 
scm_listen (sock, backlog)
     SCM sock;
     SCM backlog;
{
  int fd;
  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_listen);
  SCM_ASSERT (SCM_INUMP (backlog), backlog, SCM_ARG2, s_listen);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  if (listen (fd, SCM_INUM (backlog)) == -1)
    scm_syserror (s_listen);
  return SCM_UNSPECIFIED;
}

/* Put the components of a sockaddr into a new SCM vector.  */

static SCM scm_addr_vector SCM_P ((struct sockaddr *address, char *proc));

static SCM
scm_addr_vector (address, proc)
     struct sockaddr *address;
     char *proc;
{
  short int fam = address->sa_family;
  SCM result;
  SCM *ve;
  if (fam == AF_UNIX)
    {
      struct sockaddr_un *nad = (struct sockaddr_un *) address;
      result = scm_make_vector (SCM_MAKINUM (2), SCM_UNSPECIFIED, SCM_BOOL_F);
      ve = SCM_VELTS (result);
      ve[0] = scm_ulong2num ((unsigned long) fam);
      ve[1] = scm_makfromstr (nad->sun_path,
			      (scm_sizet) strlen (nad->sun_path), 0);
    }
  else if (fam == AF_INET)
    {
      struct sockaddr_in *nad = (struct sockaddr_in *) address;
      result = scm_make_vector (SCM_MAKINUM (3), SCM_UNSPECIFIED, SCM_BOOL_F);
      ve = SCM_VELTS (result);
      ve[0] = scm_ulong2num ((unsigned long) fam);
      ve[1] = scm_ulong2num (ntohl (nad->sin_addr.s_addr));
      ve[2] = scm_ulong2num ((unsigned long) ntohs (nad->sin_port));
    }
  else
    scm_misc_error (proc, "Unrecognised socket address type: %s",
		    scm_listify (SCM_MAKINUM (fam)));

  return result;
}

/* Allocate a buffer large enough to hold any sockaddr type.  */
static char *scm_addr_buffer;
static int scm_addr_buffer_size;

static void scm_init_addr_buffer SCM_P ((void));

static void
scm_init_addr_buffer ()
{
  scm_addr_buffer_size = (int) sizeof (struct sockaddr_un);
  if (sizeof (struct sockaddr_in) > scm_addr_buffer_size)
    scm_addr_buffer_size = (int) sizeof (struct sockaddr_in);
  scm_addr_buffer = scm_must_malloc (scm_addr_buffer_size, "address buffer");
}

SCM_PROC (s_accept, "accept", 1, 0, 0, scm_accept);

SCM 
scm_accept (sock)
     SCM sock;
{
  int fd;
  int newfd;
  SCM address;
  SCM newsock;

  int tmp_size;
  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_accept);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  SCM_DEFER_INTS;
  tmp_size = scm_addr_buffer_size;
  newfd = accept (fd, (struct sockaddr *) scm_addr_buffer, &tmp_size);
  newsock = scm_sock_fd_to_port (newfd, s_accept);
  if (tmp_size > 0)
    address = scm_addr_vector ((struct sockaddr *) scm_addr_buffer, s_accept);
  else
    address = SCM_BOOL_F;
  
  SCM_ALLOW_INTS;
  return scm_cons (newsock, address);
}

SCM_PROC (s_getsockname, "getsockname", 1, 0, 0, scm_getsockname);

SCM 
scm_getsockname (sock)
     SCM sock;
{
  int tmp_size;
  int fd;
  SCM result;
  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_getsockname);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  SCM_DEFER_INTS;
  tmp_size = scm_addr_buffer_size;
  if (getsockname (fd, (struct sockaddr *) scm_addr_buffer, &tmp_size) == -1)
    scm_syserror (s_getsockname);
  if (tmp_size > 0)
    result = scm_addr_vector ((struct sockaddr *) scm_addr_buffer, s_getsockname);
  else
    result = SCM_BOOL_F;
  SCM_ALLOW_INTS;
  return result;
}

SCM_PROC (s_getpeername, "getpeername", 1, 0, 0, scm_getpeername);

SCM 
scm_getpeername (sock)
     SCM sock;
{
  int tmp_size;
  int fd;
  SCM result;
  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_getpeername);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  SCM_DEFER_INTS;
  tmp_size = scm_addr_buffer_size;
  if (getpeername (fd, (struct sockaddr *) scm_addr_buffer, &tmp_size) == -1)
    scm_syserror (s_getpeername);
  if (tmp_size > 0)
    result = scm_addr_vector ((struct sockaddr *) scm_addr_buffer, s_getpeername);
  else
    result = SCM_BOOL_F;
  SCM_ALLOW_INTS;
  return result;
}

SCM_PROC (s_recv, "recv", 2, 1, 0, scm_recv);

SCM
scm_recv (sock, buff_or_size, flags)
     SCM sock;
     SCM buff_or_size;
     SCM flags;
{
  int rv;
  int fd;
  int flg;
  SCM tok_buf;
  char *p;
  int size;
  int allocated = 0;

  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_recv);
  if (SCM_INUMP (buff_or_size))
    {
      size = SCM_INUM (buff_or_size);
      tok_buf = scm_makstr (size, 0);
      allocated = 1;
    }
  else
    {
      SCM_ASSERT (SCM_NIMP (buff_or_size) && SCM_STRINGP (buff_or_size),
	      buff_or_size, SCM_ARG2, s_recv);
      tok_buf = buff_or_size;
      size = SCM_LENGTH (tok_buf);
    }
  p = SCM_CHARS (tok_buf);
  fd = fileno ((FILE *)SCM_STREAM (sock));

  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    flg = scm_num2ulong (flags, (char *) SCM_ARG3, s_recv);

  SCM_SYSCALL (rv = recv (fd, p, size, flg));
  if (rv == -1)
    scm_syserror (s_recv);

  return scm_cons (allocated
		   ? scm_vector_set_length_x (tok_buf, (SCM) SCM_MAKINUM (rv))
		   : tok_buf,
		   SCM_MAKINUM (rv));
}

SCM_PROC (s_send, "send", 2, 1, 0, scm_send);

SCM
scm_send (sock, message, flags)
     SCM sock;
     SCM message;
     SCM flags;
{
  int rv;
  int fd;
  int flg;

  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_send);
  SCM_ASSERT (SCM_NIMP (message) && SCM_STRINGP (message), message, SCM_ARG2, s_send);
  fd = fileno ((FILE *)SCM_STREAM (sock));

  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    flg = scm_num2ulong (flags, (char *) SCM_ARG3, s_send);

  SCM_SYSCALL (rv = send (fd, SCM_CHARS (message), SCM_LENGTH (message), flg));
  if (rv == -1)
    scm_syserror (s_send);
  return SCM_MAKINUM (rv);
}

SCM_PROC (s_recvfrom, "recvfrom", 2, 1, 0, scm_recvfrom);

SCM
scm_recvfrom (sock, buff_or_size, flags)
     SCM sock;
     SCM buff_or_size;
     SCM flags;
{
  int rv;
  int fd;
  int flg;
  SCM tok_buf;
  char *p;
  int size;
  int allocated = 0;
  int tmp_size;
  SCM address;

  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_recvfrom);
  if (SCM_INUMP (buff_or_size))
    {
      size = SCM_INUM (buff_or_size);
      tok_buf = scm_makstr (size, 0);
      allocated = 1;
    }
  else
    {
      SCM_ASSERT (SCM_NIMP (buff_or_size) && SCM_STRINGP (buff_or_size),
	      buff_or_size, SCM_ARG2, s_recvfrom);
      tok_buf = buff_or_size;
      size = SCM_LENGTH (tok_buf);
    }
  p = SCM_CHARS (tok_buf);
  fd = fileno ((FILE *)SCM_STREAM (sock));

  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    flg = scm_num2ulong (flags, (char *) SCM_ARG3, s_recvfrom);

  tmp_size = scm_addr_buffer_size;
  SCM_SYSCALL (rv = recvfrom (fd, p, size, flg,
			  (struct sockaddr *) scm_addr_buffer,
			  &tmp_size));
  if (rv == -1)
    scm_syserror (s_recvfrom);
  if (tmp_size > 0)
    address = scm_addr_vector ((struct sockaddr *) scm_addr_buffer, s_recvfrom);
  else
    address = SCM_BOOL_F;

  return scm_listify (allocated
		      ? scm_vector_set_length_x (tok_buf,
						 (SCM) SCM_MAKINUM (rv))
		      : tok_buf,
		      SCM_MAKINUM (rv),
		      address,
		      SCM_UNDEFINED);
}

SCM_PROC (s_sendto, "sendto", 4, 0, 1, scm_sendto);

SCM
scm_sendto (sock, message, fam, address, args_and_flags)
     SCM sock;
     SCM message;
     SCM fam;
     SCM address;
     SCM args_and_flags;
{
  int rv;
  int fd;
  int flg;
  struct sockaddr *soka;
  scm_sizet size;

  SCM_ASSERT (SCM_NIMP (sock) && SCM_FPORTP (sock), sock, SCM_ARG1, s_sendto);
  SCM_ASSERT (SCM_NIMP (message) && SCM_STRINGP (message), message, SCM_ARG2, s_sendto);
  SCM_ASSERT (SCM_INUMP (fam), fam, SCM_ARG3, s_sendto);
  fd = fileno ((FILE *)SCM_STREAM (sock));
  SCM_DEFER_INTS;
  soka = scm_fill_sockaddr (SCM_INUM (fam), address, &args_and_flags, 4,
			    s_sendto, &size);
  if (SCM_NULLP (args_and_flags))
    flg = 0;
  else
    {
      SCM_ASSERT (SCM_NIMP (args_and_flags) && SCM_CONSP (args_and_flags),
	      args_and_flags, SCM_ARG5, s_sendto);
      flg = scm_num2ulong (SCM_CAR (args_and_flags), (char *) SCM_ARG5, s_sendto);
    }
  SCM_SYSCALL (rv = sendto (fd, SCM_CHARS (message), SCM_LENGTH (message), flg,
			soka, size));
  if (rv == -1)
    scm_syserror (s_sendto);
  scm_must_free ((char *) soka);
  SCM_ALLOW_INTS;
  return SCM_MAKINUM (rv);
}



void
scm_init_socket ()
{
  /* protocol families.  */
#ifdef AF_UNSPEC
  scm_sysintern ("AF_UNSPEC", SCM_MAKINUM (AF_UNSPEC));
#endif
#ifdef AF_UNIX
  scm_sysintern ("AF_UNIX", SCM_MAKINUM (AF_UNIX));
#endif
#ifdef AF_INET
  scm_sysintern ("AF_INET", SCM_MAKINUM (AF_INET));
#endif

#ifdef PF_UNSPEC
  scm_sysintern ("PF_UNSPEC", SCM_MAKINUM (PF_UNSPEC));
#endif
#ifdef PF_UNIX
  scm_sysintern ("PF_UNIX", SCM_MAKINUM (PF_UNIX));
#endif
#ifdef PF_INET
  scm_sysintern ("PF_INET", SCM_MAKINUM (PF_INET));
#endif

  /* socket types.  */
#ifdef SOCK_STREAM
  scm_sysintern ("SOCK_STREAM", SCM_MAKINUM (SOCK_STREAM));
#endif
#ifdef SOCK_DGRAM
  scm_sysintern ("SOCK_DGRAM", SCM_MAKINUM (SOCK_DGRAM));
#endif
#ifdef SOCK_RAW
  scm_sysintern ("SOCK_RAW", SCM_MAKINUM (SOCK_RAW));
#endif

  /* setsockopt level.  */
#ifdef SOL_SOCKET
  scm_sysintern ("SOL_SOCKET", SCM_MAKINUM (SOL_SOCKET));
#endif
#ifdef SOL_IP
  scm_sysintern ("SOL_IP", SCM_MAKINUM (SOL_IP));
#endif
#ifdef SOL_TCP
  scm_sysintern ("SOL_TCP", SCM_MAKINUM (SOL_TCP));
#endif
#ifdef SOL_UDP
  scm_sysintern ("SOL_UDP", SCM_MAKINUM (SOL_UDP));
#endif

  /* setsockopt names.  */
#ifdef SO_DEBUG
  scm_sysintern ("SO_DEBUG", SCM_MAKINUM (SO_DEBUG));
#endif
#ifdef SO_REUSEADDR
  scm_sysintern ("SO_REUSEADDR", SCM_MAKINUM (SO_REUSEADDR));
#endif
#ifdef SO_STYLE
  scm_sysintern ("SO_STYLE", SCM_MAKINUM (SO_STYLE));
#endif
#ifdef SO_TYPE
  scm_sysintern ("SO_TYPE", SCM_MAKINUM (SO_TYPE));
#endif
#ifdef SO_ERROR
  scm_sysintern ("SO_ERROR", SCM_MAKINUM (SO_ERROR));
#endif
#ifdef SO_DONTROUTE
  scm_sysintern ("SO_DONTROUTE", SCM_MAKINUM (SO_DONTROUTE));
#endif
#ifdef SO_BROADCAST
  scm_sysintern ("SO_BROADCAST", SCM_MAKINUM (SO_BROADCAST));
#endif
#ifdef SO_SNDBUF
  scm_sysintern ("SO_SNDBUF", SCM_MAKINUM (SO_SNDBUF));
#endif
#ifdef SO_RCVBUF
  scm_sysintern ("SO_RCVBUF", SCM_MAKINUM (SO_RCVBUF));
#endif
#ifdef SO_KEEPALIVE
  scm_sysintern ("SO_KEEPALIVE", SCM_MAKINUM (SO_KEEPALIVE));
#endif
#ifdef SO_OOBINLINE
  scm_sysintern ("SO_OOBINLINE", SCM_MAKINUM (SO_OOBINLINE));
#endif
#ifdef SO_NO_CHECK
  scm_sysintern ("SO_NO_CHECK", SCM_MAKINUM (SO_NO_CHECK));
#endif
#ifdef SO_PRIORITY
  scm_sysintern ("SO_PRIORITY", SCM_MAKINUM (SO_PRIORITY));
#endif
#ifdef SO_LINGER
  scm_sysintern ("SO_LINGER", SCM_MAKINUM (SO_LINGER));
#endif

  /* recv/send options.  */
#ifdef MSG_OOB
  scm_sysintern ("MSG_OOB", SCM_MAKINUM (MSG_OOB));
#endif
#ifdef MSG_PEEK
  scm_sysintern ("MSG_PEEK", SCM_MAKINUM (MSG_PEEK));
#endif
#ifdef MSG_DONTROUTE
  scm_sysintern ("MSG_DONTROUTE", SCM_MAKINUM (MSG_DONTROUTE));
#endif

  scm_add_feature ("socket");
  scm_init_addr_buffer ();

#include "socket.x"
}

