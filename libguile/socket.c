/*	Copyright (C) 1996,1997,1998 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>

#include "_scm.h"
#include "unif.h"
#include "feature.h"
#include "fports.h"

#include "scm_validate.h"
#include "socket.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#ifdef HAVE_UNIX_DOMAIN_SOCKETS
#include <sys/un.h>
#endif
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>



GUILE_PROC (scm_htons, "htons", 1, 0, 0, 
            (SCM in),
            "")
#define FUNC_NAME s_scm_htons
{
  unsigned short c_in;

  SCM_VALIDATE_INT_COPY(1,in,c_in);
  if (c_in != SCM_INUM (in))
    SCM_OUT_OF_RANGE (1,in);

  return SCM_MAKINUM (htons (c_in));
}
#undef FUNC_NAME

GUILE_PROC (scm_ntohs, "ntohs", 1, 0, 0, 
            (SCM in),
            "")
#define FUNC_NAME s_scm_ntohs
{
  unsigned short c_in;

  SCM_VALIDATE_INT_COPY(1,in,c_in);
  if (c_in != SCM_INUM (in))
    SCM_OUT_OF_RANGE (1,in);

  return SCM_MAKINUM (ntohs (c_in));
}
#undef FUNC_NAME

GUILE_PROC (scm_htonl, "htonl", 1, 0, 0, 
            (SCM in),
            "")
#define FUNC_NAME s_scm_htonl
{
  unsigned long c_in = SCM_NUM2ULONG (1,in);
  return scm_ulong2num (htonl (c_in));
}
#undef FUNC_NAME

GUILE_PROC (scm_ntohl, "ntohl", 1, 0, 0, 
            (SCM in),
            "")
#define FUNC_NAME s_scm_ntohl
{
  unsigned long c_in = SCM_NUM2ULONG (1,in);
  return scm_ulong2num (ntohl (c_in));
}
#undef FUNC_NAME

SCM_SYMBOL (sym_socket, "socket");

static SCM
scm_sock_fd_to_port (int fd, const char *proc)
{
  SCM result;
  if (fd == -1)
    scm_syserror (proc);
  result = scm_fdes_to_port (fd, "r+0", sym_socket);
  return result;
}


#define SCM_SOCK_FD_TO_PORT(fd) (scm_sock_fd_to_port((fd),FUNC_NAME))

GUILE_PROC (scm_socket, "socket", 3, 0, 0,
            (SCM family, SCM style, SCM proto),
"")
#define FUNC_NAME s_scm_socket
{
  int fd;
  SCM result;

  SCM_VALIDATE_INT(1,family);
  SCM_VALIDATE_INT(2,style);
  SCM_VALIDATE_INT(3,proto);
  fd = socket (SCM_INUM (family), SCM_INUM (style), SCM_INUM (proto));
  result = SCM_SOCK_FD_TO_PORT (fd);
  return result;
}
#undef FUNC_NAME



#ifdef HAVE_SOCKETPAIR
GUILE_PROC (scm_socketpair, "socketpair", 3, 0, 0,
            (SCM family, SCM style, SCM proto),
"")
#define FUNC_NAME s_scm_socketpair
{
  int fam;
  int fd[2];
  SCM a;
  SCM b;

  SCM_VALIDATE_INT(1,family);
  SCM_VALIDATE_INT(2,style);
  SCM_VALIDATE_INT(3,proto);

  fam = SCM_INUM (family);

  if (socketpair (fam, SCM_INUM (style), SCM_INUM (proto), fd) == -1)
    SCM_SYSERROR;

  a = SCM_SOCK_FD_TO_PORT(fd[0]);
  b = SCM_SOCK_FD_TO_PORT(fd[1]);
  return scm_cons (a, b);
}
#undef FUNC_NAME
#endif

GUILE_PROC (scm_getsockopt, "getsockopt", 3, 0, 0,
            (SCM sock, SCM level, SCM optname),
"")
#define FUNC_NAME s_scm_getsockopt
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

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_INT_COPY(2,level,ilevel);
  SCM_VALIDATE_INT_COPY(3,optname,ioptname);

  fd = SCM_FPORT_FDES (sock);
  if (getsockopt (fd, ilevel, ioptname, (void *) optval, &optlen) == -1)
    SCM_SYSERROR;

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
#undef FUNC_NAME

GUILE_PROC (scm_setsockopt, "setsockopt", 4, 0, 0,
            (SCM sock, SCM level, SCM optname, SCM value),
"")
#define FUNC_NAME s_scm_setsockopt
{
  int fd;
  int optlen;
#ifdef HAVE_STRUCT_LINGER
  char optval[sizeof (struct linger)]; /* Biggest option :-(  */
#else
  char optval[sizeof (scm_sizet)];
#endif
  int ilevel, ioptname;
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_INT_COPY(2,level,ilevel);
  SCM_VALIDATE_INT_COPY(3,optname,ioptname);
  fd = SCM_FPORT_FDES (sock);
  if (0);
#ifdef SO_LINGER
  else if (ilevel == SOL_SOCKET && ioptname == SO_LINGER)
    {
#ifdef HAVE_STRUCT_LINGER
      struct linger ling;
      SCM_ASSERT (SCM_NIMP (value) && SCM_CONSP (value)
		  && SCM_INUMP (SCM_CAR (value))
		  &&  SCM_INUMP (SCM_CDR (value)),
		  value, SCM_ARG4, FUNC_NAME);
      ling.l_onoff = SCM_INUM (SCM_CAR (value));
      ling.l_linger = SCM_INUM (SCM_CDR (value));
      optlen = (int) sizeof (struct linger);
      memcpy (optval, (void *) &ling, optlen);
#else
      scm_sizet ling;
      SCM_ASSERT (SCM_NIMP (value) && SCM_CONSP (value)
		  && SCM_INUMP (SCM_CAR (value))
		  &&  SCM_INUMP (SCM_CDR (value)),
		  value, SCM_ARG4, FUNC_NAME);
      ling = SCM_INUM (SCM_CAR (value));
      optlen = (int) sizeof (scm_sizet);
      (*(scm_sizet *) optval) = (scm_sizet) SCM_INUM (value);
#endif
    }
#endif
#ifdef SO_SNDBUF
  else if (ilevel == SOL_SOCKET && ioptname == SO_SNDBUF)
    {
      SCM_VALIDATE_INT(4,value);
      optlen = (int) sizeof (scm_sizet);
      (*(scm_sizet *) optval) = (scm_sizet) SCM_INUM (value);
    }
#endif
#ifdef SO_RCVBUF
  else if (ilevel == SOL_SOCKET && ioptname == SO_RCVBUF)
    {
      SCM_VALIDATE_INT(4,value);
      optlen = (int) sizeof (scm_sizet);
      (*(scm_sizet *) optval) = (scm_sizet) SCM_INUM (value);
    }
#endif
  else
    {
      /* Most options just take an int.  */
      SCM_VALIDATE_INT(4,value);
      optlen = (int) sizeof (int);
      (*(int *) optval) = (int) SCM_INUM (value);
    }
  if (setsockopt (fd, ilevel, ioptname, (void *) optval, optlen) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC (scm_shutdown, "shutdown", 2, 0, 0,
          (SCM sock, SCM how),
"")
#define FUNC_NAME s_scm_shutdown
{
  int fd;
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_INT(2,how);
  SCM_ASSERT_RANGE(2,how,0 <= SCM_INUM (how) && 2 >= SCM_INUM (how));
  fd = SCM_FPORT_FDES (sock);
  if (shutdown (fd, SCM_INUM (how)) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* convert fam/address/args into a sockaddr of the appropriate type.
   args is modified by removing the arguments actually used.
   which_arg and proc are used when reporting errors:
   which_arg is the position of address in the original argument list.
   proc is the name of the original procedure.
   size returns the size of the structure allocated.  */


static struct sockaddr *
scm_fill_sockaddr (int fam,SCM address,SCM *args,int which_arg,const char *proc,scm_sizet *size)
{
  switch (fam)
    {
    case AF_INET:
      {
	SCM isport;
	struct sockaddr_in *soka;

	soka = (struct sockaddr_in *)
	  scm_must_malloc (sizeof (struct sockaddr_in), proc);
	/* e.g., for BSDs which don't like invalid sin_len.  */
	memset (soka, 0, sizeof (struct sockaddr_in));
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
#ifdef HAVE_UNIX_DOMAIN_SOCKETS
    case AF_UNIX:
      {
	struct sockaddr_un *soka;

	soka = (struct sockaddr_un *)
	  scm_must_malloc (sizeof (struct sockaddr_un), proc);
	memset (soka, 0, sizeof (struct sockaddr_un));
	soka->sun_family = AF_UNIX;
	SCM_ASSERT (SCM_NIMP (address) && SCM_ROSTRINGP (address), address,
		    which_arg, proc);
	memcpy (soka->sun_path, SCM_ROCHARS (address),
		1 + SCM_ROLENGTH (address));
	*size = sizeof (struct sockaddr_un);
	return (struct sockaddr *) soka;
      }
#endif
    default:
      scm_out_of_range (proc, SCM_MAKINUM (fam));
    }
}
  
GUILE_PROC (scm_connect, "connect", 3, 0, 1,
            (SCM sock, SCM fam, SCM address, SCM args),
"")
#define FUNC_NAME s_scm_connect
{
  int fd;
  struct sockaddr *soka;
  scm_sizet size;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_INT(2,fam);
  fd = SCM_FPORT_FDES (sock);
  soka = scm_fill_sockaddr (SCM_INUM (fam), address, &args, 3, FUNC_NAME, &size);
  if (connect (fd, soka, size) == -1)
    SCM_SYSERROR;
  scm_must_free ((char *) soka);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC (scm_bind, "bind", 3, 0, 1,
            (SCM sock, SCM fam, SCM address, SCM args),
"")
#define FUNC_NAME s_scm_bind
{
  int rv;
  struct sockaddr *soka;
  scm_sizet size;
  int fd;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_INT(2,fam);
  soka = scm_fill_sockaddr (SCM_INUM (fam), address, &args, 3, FUNC_NAME, &size);
  fd = SCM_FPORT_FDES (sock);
  rv = bind (fd, soka, size);
  if (rv == -1)
    SCM_SYSERROR;
  scm_must_free ((char *) soka);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC (scm_listen, "listen", 2, 0, 0,
            (SCM sock, SCM backlog),
"")
#define FUNC_NAME s_scm_listen
{
  int fd;
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_INT(2,backlog);
  fd = SCM_FPORT_FDES (sock);
  if (listen (fd, SCM_INUM (backlog)) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Put the components of a sockaddr into a new SCM vector.  */

static SCM
scm_addr_vector (struct sockaddr *address,const char *proc)
{
  short int fam = address->sa_family;
  SCM result;
  SCM *ve;
#ifdef HAVE_UNIX_DOMAIN_SOCKETS
  if (fam == AF_UNIX)
    {
      struct sockaddr_un *nad = (struct sockaddr_un *) address;
      result = scm_make_vector (SCM_MAKINUM (2), SCM_UNSPECIFIED);
      ve = SCM_VELTS (result);
      ve[0] = scm_ulong2num ((unsigned long) fam);
      ve[1] = scm_makfromstr (nad->sun_path,
			      (scm_sizet) strlen (nad->sun_path), 0);
    }
  else 
#endif
  if (fam == AF_INET)
    {
      struct sockaddr_in *nad = (struct sockaddr_in *) address;
      result = scm_make_vector (SCM_MAKINUM (3), SCM_UNSPECIFIED);
      ve = SCM_VELTS (result);
      ve[0] = scm_ulong2num ((unsigned long) fam);
      ve[1] = scm_ulong2num (ntohl (nad->sin_addr.s_addr));
      ve[2] = scm_ulong2num ((unsigned long) ntohs (nad->sin_port));
    }
  else
    scm_misc_error (proc, "Unrecognised address family: %s",
		    scm_listify (SCM_MAKINUM (fam), SCM_UNDEFINED));

  return result;
}

/* Allocate a buffer large enough to hold any sockaddr type.  */
static char *scm_addr_buffer;
static int scm_addr_buffer_size;

static void
scm_init_addr_buffer (void)
{
  scm_addr_buffer_size =
#ifdef HAVE_UNIX_DOMAIN_SOCKETS
  (int) sizeof (struct sockaddr_un)
#else
  0
#endif
  ;
  if (sizeof (struct sockaddr_in) > scm_addr_buffer_size)
    scm_addr_buffer_size = (int) sizeof (struct sockaddr_in);
  scm_addr_buffer = scm_must_malloc (scm_addr_buffer_size, "address buffer");
}

GUILE_PROC (scm_accept, "accept", 1, 0, 0, 
            (SCM sock),
"")
#define FUNC_NAME s_scm_accept
{
  int fd;
  int newfd;
  SCM address;
  SCM newsock;

  int tmp_size;
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  fd = SCM_FPORT_FDES (sock);
  tmp_size = scm_addr_buffer_size;
  newfd = accept (fd, (struct sockaddr *) scm_addr_buffer, &tmp_size);
  newsock = scm_sock_fd_to_port (newfd, FUNC_NAME);
  if (tmp_size > 0)
    address = scm_addr_vector ((struct sockaddr *) scm_addr_buffer, FUNC_NAME);
  else
    address = SCM_BOOL_F;
  
  return scm_cons (newsock, address);
}
#undef FUNC_NAME

GUILE_PROC (scm_getsockname, "getsockname", 1, 0, 0, 
            (SCM sock),
"")
#define FUNC_NAME s_scm_getsockname
{
  int tmp_size;
  int fd;
  SCM result;
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  fd = SCM_FPORT_FDES (sock);
  tmp_size = scm_addr_buffer_size;
  if (getsockname (fd, (struct sockaddr *) scm_addr_buffer, &tmp_size) == -1)
    SCM_SYSERROR;
  if (tmp_size > 0)
    result = scm_addr_vector ((struct sockaddr *) scm_addr_buffer, FUNC_NAME);
  else
    result = SCM_BOOL_F;
  return result;
}
#undef FUNC_NAME

GUILE_PROC (scm_getpeername, "getpeername", 1, 0, 0, 
            (SCM sock),
"")
#define FUNC_NAME s_scm_getpeername
{
  int tmp_size;
  int fd;
  SCM result;
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  fd = SCM_FPORT_FDES (sock);
  tmp_size = scm_addr_buffer_size;
  if (getpeername (fd, (struct sockaddr *) scm_addr_buffer, &tmp_size) == -1)
    SCM_SYSERROR;
  if (tmp_size > 0)
    result = scm_addr_vector ((struct sockaddr *) scm_addr_buffer, FUNC_NAME);
  else
    result = SCM_BOOL_F;
  return result;
}
#undef FUNC_NAME

GUILE_PROC (scm_recv, "recv!", 2, 1, 0,
            (SCM sock, SCM buf, SCM flags),
"")
#define FUNC_NAME s_scm_recv
{
  int rv;
  int fd;
  int flg;

  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_STRING(2,buf);
  SCM_VALIDATE_INT_DEF_COPY(3,flags,0,flg);
  fd = SCM_FPORT_FDES (sock);

  SCM_SYSCALL (rv = recv (fd, SCM_CHARS (buf), SCM_LENGTH (buf), flg));
  if (rv == -1)
    SCM_SYSERROR;

  return SCM_MAKINUM (rv);
}
#undef FUNC_NAME

GUILE_PROC (scm_send, "send", 2, 1, 0,
            (SCM sock, SCM message, SCM flags),
"")
#define FUNC_NAME s_scm_send
{
  int rv;
  int fd;
  int flg;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_ROSTRING(2,message);
  SCM_VALIDATE_INT_DEF_COPY(3,flags,0,flg);
  fd = SCM_FPORT_FDES (sock);

  SCM_SYSCALL (rv = send (fd, SCM_ROCHARS (message), SCM_ROLENGTH (message), flg));
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_MAKINUM (rv);
}
#undef FUNC_NAME

GUILE_PROC (scm_recvfrom, "recvfrom!", 2, 3, 0,
            (SCM sock, SCM buf, SCM flags, SCM start, SCM end),
"")
#define FUNC_NAME s_scm_recvfrom
{
  int rv;
  int fd;
  int flg;
  int offset = 0;
  int cend;
  int tmp_size;
  SCM address;

  SCM_VALIDATE_OPFPORT(1,sock);
  SCM_VALIDATE_STRING(2,buf);
  cend = SCM_LENGTH (buf);
  
  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    {
      flg = SCM_NUM2ULONG (3,flags);

      if (!SCM_UNBNDP (start))
	{
	  offset = (int) SCM_NUM2LONG (4,start);
	  
	  if (offset < 0 || offset >= cend)
	    SCM_OUT_OF_RANGE (4, start);

	  if (!SCM_UNBNDP (end))
	    {
	      int tend = (int) SCM_NUM2LONG (5,end);
      
	      if (tend <= offset || tend > cend)
		SCM_OUT_OF_RANGE (5, end);

	      cend = tend;
	    }
	}
    }

  fd = SCM_FPORT_FDES (sock);

  tmp_size = scm_addr_buffer_size;
  SCM_SYSCALL (rv = recvfrom (fd, SCM_CHARS (buf) + offset,
			      cend - offset, flg,
			      (struct sockaddr *) scm_addr_buffer,
			      &tmp_size));
  if (rv == -1)
    SCM_SYSERROR;
  if (tmp_size > 0)
    address = scm_addr_vector ((struct sockaddr *) scm_addr_buffer, FUNC_NAME);
  else
    address = SCM_BOOL_F;

  return scm_cons (SCM_MAKINUM (rv), address);
}
#undef FUNC_NAME

GUILE_PROC (scm_sendto, "sendto", 4, 0, 1,
            (SCM sock, SCM message, SCM fam, SCM address, SCM args_and_flags),
"")
#define FUNC_NAME s_scm_sendto
{
  int rv;
  int fd;
  int flg;
  struct sockaddr *soka;
  scm_sizet size;
  int save_err;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_FPORT(1,sock);
  SCM_VALIDATE_ROSTRING(2,message);
  SCM_VALIDATE_INT(3,fam);
  fd = SCM_FPORT_FDES (sock);
  soka = scm_fill_sockaddr (SCM_INUM (fam), address, &args_and_flags, 4,
			    FUNC_NAME, &size);
  if (SCM_NULLP (args_and_flags))
    flg = 0;
  else
    {
      SCM_VALIDATE_NIMCONS(5,args_and_flags);
      flg = SCM_NUM2ULONG (5,SCM_CAR (args_and_flags));
    }
  SCM_SYSCALL (rv = sendto (fd, SCM_ROCHARS (message), SCM_ROLENGTH (message),
			    flg, soka, size));
  save_err = errno;
  scm_must_free ((char *) soka);
  errno = save_err;
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_MAKINUM (rv);
}
#undef FUNC_NAME



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

