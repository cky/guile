/* Copyright (C) 2001 Free Software Foundation, Inc.
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
#include "libguile/modules.h"
#include "libguile/numbers.h"

#include <errno.h>

#include "win32-socket.h"

/* Winsock API error description structure.  The error description is 
   necessary because there is no error list available.  */
typedef struct
{
  int error;         /* Error code.  */
  char *str;         /* Error description.  */
  int replace;       /* Possible error code replacement.  */
  char *replace_str; /* Replacement symbol.  */
  char *correct_str; /* Original symbol.  */
}
socket_error_t;

/* Define replacement symbols for most of the WSA* error codes.  */
#ifndef EWOULDBLOCK
# define EWOULDBLOCK     WSAEWOULDBLOCK
#endif
#ifndef EINPROGRESS
# define EINPROGRESS     WSAEINPROGRESS
#endif
#ifndef EALREADY
# define EALREADY        WSAEALREADY
#endif
#ifndef EDESTADDRREQ
# define EDESTADDRREQ    WSAEDESTADDRREQ
#endif
#ifndef EMSGSIZE
# define EMSGSIZE        WSAEMSGSIZE
#endif
#ifndef EPROTOTYPE
# define EPROTOTYPE      WSAEPROTOTYPE
#endif
#ifndef ENOTSOCK
# define ENOTSOCK        WSAENOTSOCK
#endif
#ifndef ENOPROTOOPT
# define ENOPROTOOPT     WSAENOPROTOOPT
#endif
#ifndef EPROTONOSUPPORT
# define EPROTONOSUPPORT WSAEPROTONOSUPPORT
#endif
#ifndef ESOCKTNOSUPPORT
# define ESOCKTNOSUPPORT WSAESOCKTNOSUPPORT
#endif
#ifndef EOPNOTSUPP
# define EOPNOTSUPP      WSAEOPNOTSUPP
#endif
#ifndef EPFNOSUPPORT
# define EPFNOSUPPORT    WSAEPFNOSUPPORT
#endif
#ifndef EAFNOSUPPORT
# define EAFNOSUPPORT    WSAEAFNOSUPPORT
#endif
#ifndef EADDRINUSE
# define EADDRINUSE      WSAEADDRINUSE
#endif
#ifndef EADDRNOTAVAIL
# define EADDRNOTAVAIL   WSAEADDRNOTAVAIL
#endif
#ifndef ENETDOWN
# define ENETDOWN        WSAENETDOWN
#endif
#ifndef ENETUNREACH
# define ENETUNREACH     WSAENETUNREACH
#endif
#ifndef ENETRESET
# define ENETRESET       WSAENETRESET
#endif
#ifndef ECONNABORTED
# define ECONNABORTED    WSAECONNABORTED
#endif
#ifndef ECONNRESET
# define ECONNRESET      WSAECONNRESET
#endif
#ifndef ENOBUFS
# define ENOBUFS         WSAENOBUFS
#endif
#ifndef EISCONN
# define EISCONN         WSAEISCONN
#endif
#ifndef ENOTCONN
# define ENOTCONN        WSAENOTCONN
#endif
#ifndef ESHUTDOWN
# define ESHUTDOWN       WSAESHUTDOWN
#endif
#ifndef ETOOMANYREFS
# define ETOOMANYREFS    WSAETOOMANYREFS
#endif
#ifndef ETIMEDOUT
# define ETIMEDOUT       WSAETIMEDOUT
#endif
#ifndef ECONNREFUSED
# define ECONNREFUSED    WSAECONNREFUSED
#endif
#ifndef ELOOP
# define ELOOP           WSAELOOP
#endif
#ifndef EHOSTDOWN
# define EHOSTDOWN       WSAEHOSTDOWN
#endif
#ifndef EHOSTUNREACH
# define EHOSTUNREACH    WSAEHOSTUNREACH
#endif
#ifndef EPROCLIM
# define EPROCLIM        WSAEPROCLIM
#endif
#ifndef EUSERS
# define EUSERS          WSAEUSERS
#endif
#ifndef EDQUOT
# define EDQUOT          WSAEDQUOT
#endif
#ifndef ESTALE
# define ESTALE          WSAESTALE
#endif
#ifndef EREMOTE
# define EREMOTE         WSAEREMOTE
#endif

/* List of error structures.  */
static socket_error_t socket_errno [] = {
  /* 000 */ { 0, NULL, 0, NULL, NULL },
  /* 001 */ { 0, NULL, 0, NULL, NULL },
  /* 002 */ { 0, NULL, 0, NULL, NULL },
  /* 003 */ { 0, NULL, 0, NULL, NULL },
  /* 004 */ { WSAEINTR, "Interrupted function call", EINTR, NULL, "WSAEINTR" },
  /* 005 */ { 0, NULL, 0, NULL, NULL },
  /* 006 */ { 0, NULL, 0, NULL, NULL },
  /* 007 */ { 0, NULL, 0, NULL, NULL },
  /* 008 */ { 0, NULL, 0, NULL, NULL },
  /* 009 */ { WSAEBADF, "Bad file number", EBADF, NULL, "WSAEBADF" },
  /* 010 */ { 0, NULL, 0, NULL, NULL },
  /* 011 */ { 0, NULL, 0, NULL, NULL },
  /* 012 */ { 0, NULL, 0, NULL, NULL },
  /* 013 */ { WSAEACCES, "Permission denied", EACCES, NULL, "WSAEACCES" },
  /* 014 */ { WSAEFAULT, "Bad address", EFAULT, NULL, "WSAEFAULT" },
  /* 015 */ { 0, NULL, 0, NULL, NULL },
  /* 016 */ { 0, NULL, 0, NULL, NULL },
  /* 017 */ { 0, NULL, 0, NULL, NULL },
  /* 018 */ { 0, NULL, 0, NULL, NULL },
  /* 019 */ { 0, NULL, 0, NULL, NULL },
  /* 020 */ { 0, NULL, 0, NULL, NULL },
  /* 021 */ { 0, NULL, 0, NULL, NULL },
  /* 022 */ { WSAEINVAL, "Invalid argument", EINVAL, NULL, "WSAEINVAL" },
  /* 023 */ { 0, NULL, 0, NULL, NULL },
  /* 024 */ { WSAEMFILE, "Too many open files", EMFILE, NULL, "WSAEMFILE" },
  /* 025 */ { 0, NULL, 0, NULL, NULL },
  /* 026 */ { 0, NULL, 0, NULL, NULL },
  /* 027 */ { 0, NULL, 0, NULL, NULL },
  /* 028 */ { 0, NULL, 0, NULL, NULL },
  /* 029 */ { 0, NULL, 0, NULL, NULL },
  /* 030 */ { 0, NULL, 0, NULL, NULL },
  /* 031 */ { 0, NULL, 0, NULL, NULL },
  /* 032 */ { 0, NULL, 0, NULL, NULL },
  /* 033 */ { 0, NULL, 0, NULL, NULL },
  /* 034 */ { 0, NULL, 0, NULL, NULL },
  /* 035 */ { WSAEWOULDBLOCK, "Resource temporarily unavailable", 
	      EWOULDBLOCK, "EWOULDBLOCK", "WSAEWOULDBLOCK" },
  /* 036 */ { WSAEINPROGRESS, "Operation now in progress", 
	      EINPROGRESS, "EINPROGRESS", "WSAEINPROGRESS" },
  /* 037 */ { WSAEALREADY, "Operation already in progress", 
	      EALREADY, "EALREADY", "WSAEALREADY" },
  /* 038 */ { WSAENOTSOCK, "Socket operation on non-socket", 
	      ENOTSOCK, "ENOTSOCK", "WSAENOTSOCK"},
  /* 039 */ { WSAEDESTADDRREQ, "Destination address required", 
	      EDESTADDRREQ, "EDESTADDRREQ", "WSAEDESTADDRREQ" },
  /* 040 */ { WSAEMSGSIZE, "Message too long", 
	      EMSGSIZE, "EMSGSIZE", "WSAEMSGSIZE" },
  /* 041 */ { WSAEPROTOTYPE, "Protocol wrong type for socket", 
	      EPROTOTYPE, "EPROTOTYPE", "WSAEPROTOTYPE" },
  /* 042 */ { WSAENOPROTOOPT, "Bad protocol option", 
	      ENOPROTOOPT, "ENOPROTOOPT", "WSAENOPROTOOPT" },
  /* 043 */ { WSAEPROTONOSUPPORT, "Protocol not supported", 
	      EPROTONOSUPPORT, "EPROTONOSUPPORT", "WSAEPROTONOSUPPORT" },
  /* 044 */ { WSAESOCKTNOSUPPORT, "Socket type not supported",
	      ESOCKTNOSUPPORT, "ESOCKTNOSUPPORT", "WSAESOCKTNOSUPPORT" },
  /* 045 */ { WSAEOPNOTSUPP, "Operation not supported",
	      EOPNOTSUPP, "EOPNOTSUPP", "WSAEOPNOTSUPP" },
  /* 046 */ { WSAEPFNOSUPPORT, "Protocol family not supported",
	      EPFNOSUPPORT, "EPFNOSUPPORT", "WSAEPFNOSUPPORT" },
  /* 047 */ { WSAEAFNOSUPPORT, 
	      "Address family not supported by protocol family", 
	      EAFNOSUPPORT, "EAFNOSUPPORT", "WSAEAFNOSUPPORT" },
  /* 048 */ { WSAEADDRINUSE, "Address already in use", 
	      EADDRINUSE, "EADDRINUSE", "WSAEADDRINUSE" },
  /* 049 */ { WSAEADDRNOTAVAIL, "Cannot assign requested address",
	      EADDRNOTAVAIL, "EADDRNOTAVAIL", "WSAEADDRNOTAVAIL" },
  /* 050 */ { WSAENETDOWN, "Network is down",
	      ENETDOWN, "ENETDOWN", "WSAENETDOWN" },
  /* 051 */ { WSAENETUNREACH, "Network is unreachable",
	      ENETUNREACH, "ENETUNREACH", "WSAENETUNREACH" },
  /* 052 */ { WSAENETRESET, "Network dropped connection on reset",
	      ENETRESET, "ENETRESET", "WSAENETRESET" },
  /* 053 */ { WSAECONNABORTED, "Software caused connection abort",
	      ECONNABORTED, "ECONNABORTED", "WSAECONNABORTED" },
  /* 054 */ { WSAECONNRESET, "Connection reset by peer",
	      ECONNRESET, "ECONNRESET", "WSAECONNRESET" },
  /* 055 */ { WSAENOBUFS, "No buffer space available",
	      ENOBUFS, "ENOBUFS", "WSAENOBUFS" },
  /* 056 */ { WSAEISCONN, "Socket is already connected",
	      EISCONN, "EISCONN", "WSAEISCONN" },
  /* 057 */ { WSAENOTCONN, "Socket is not connected",
	      ENOTCONN, "ENOTCONN", "WSAENOTCONN" },
  /* 058 */ { WSAESHUTDOWN, "Cannot send after socket shutdown",
	      ESHUTDOWN, "ESHUTDOWN", "WSAESHUTDOWN" },
  /* 059 */ { WSAETOOMANYREFS, "Too many references; can't splice",
	      ETOOMANYREFS, "ETOOMANYREFS", "WSAETOOMANYREFS" },
  /* 060 */ { WSAETIMEDOUT, "Connection timed out",
	      ETIMEDOUT, "ETIMEDOUT", "WSAETIMEDOUT" },
  /* 061 */ { WSAECONNREFUSED, "Connection refused",
	      ECONNREFUSED, "ECONNREFUSED", "WSAECONNREFUSED" },
  /* 062 */ { WSAELOOP, "Too many levels of symbolic links",
	      ELOOP, "ELOOP", "WSAELOOP" },
  /* 063 */ { WSAENAMETOOLONG, "File name too long",
	      ENAMETOOLONG, NULL, "WSAENAMETOOLONG" },
  /* 064 */ { WSAEHOSTDOWN, "Host is down",
	      EHOSTDOWN, "EHOSTDOWN", "WSAEHOSTDOWN" },
  /* 065 */ { WSAEHOSTUNREACH, "No route to host",
	      EHOSTUNREACH, "EHOSTUNREACH", "WSAEHOSTUNREACH" },
  /* 066 */ { WSAENOTEMPTY, "Directory not empty",
	      ENOTEMPTY, NULL, "WSAENOTEMPTY" },
  /* 067 */ { WSAEPROCLIM, "Too many processes",
	      EPROCLIM, "EPROCLIM", "WSAEPROCLIM" },
  /* 068 */ { WSAEUSERS, "Too many users",
	      EUSERS, "EUSERS", "WSAEUSERS" },
  /* 069 */ { WSAEDQUOT, "Disc quota exceeded",
	      EDQUOT, "EDQUOT", "WSAEDQUOT" },
  /* 070 */ { WSAESTALE, "Stale NFS file handle",
	      ESTALE, "ESTALE", "WSAESTALE" },
  /* 071 */ { WSAEREMOTE, "Too many levels of remote in path",
	      EREMOTE, "EREMOTE", "WSAEREMOTE" },
  /* 072 */ { 0, NULL, 0, NULL, NULL },
  /* 073 */ { 0, NULL, 0, NULL, NULL },
  /* 074 */ { 0, NULL, 0, NULL, NULL },
  /* 075 */ { 0, NULL, 0, NULL, NULL },
  /* 076 */ { 0, NULL, 0, NULL, NULL },
  /* 077 */ { 0, NULL, 0, NULL, NULL },
  /* 078 */ { 0, NULL, 0, NULL, NULL },
  /* 079 */ { 0, NULL, 0, NULL, NULL },
  /* 080 */ { 0, NULL, 0, NULL, NULL },
  /* 081 */ { 0, NULL, 0, NULL, NULL },
  /* 082 */ { 0, NULL, 0, NULL, NULL },
  /* 083 */ { 0, NULL, 0, NULL, NULL },
  /* 084 */ { 0, NULL, 0, NULL, NULL },
  /* 085 */ { 0, NULL, 0, NULL, NULL },
  /* 086 */ { 0, NULL, 0, NULL, NULL },
  /* 087 */ { 0, NULL, 0, NULL, NULL },
  /* 088 */ { 0, NULL, 0, NULL, NULL },
  /* 089 */ { 0, NULL, 0, NULL, NULL },
  /* 090 */ { 0, NULL, 0, NULL, NULL },
  /* 091 */ { WSASYSNOTREADY, "Network subsystem is unavailable",
	      0, NULL, "WSASYSNOTREADY" },
  /* 092 */ { WSAVERNOTSUPPORTED, "WINSOCK.DLL version out of range", 
	      0, NULL, "WSAVERNOTSUPPORTED" },
  /* 093 */ { WSANOTINITIALISED, "Successful WSAStartup not yet performed", 
	      0, NULL, "WSANOTINITIALISED" },
  /* 094 */ { 0, NULL, 0, NULL, NULL },
  /* 095 */ { 0, NULL, 0, NULL, NULL },
  /* 096 */ { 0, NULL, 0, NULL, NULL },
  /* 097 */ { 0, NULL, 0, NULL, NULL },
  /* 098 */ { 0, NULL, 0, NULL, NULL },
  /* 099 */ { 0, NULL, 0, NULL, NULL },
  /* 100 */ { 0, NULL, 0, NULL, NULL },
  /* 101 */ { WSAEDISCON, "Graceful shutdown in progress",
	      0, NULL, "WSAEDISCON" },
  /* 102 */ { WSAENOMORE, "No more services", 
	      0, NULL, "WSAENOMORE" },
  /* 103 */ { WSAECANCELLED, "Service lookup cancelled",
	      0, NULL, "WSAECANCELLED" },
  /* 104 */ { WSAEINVALIDPROCTABLE, "Invalid procedure call table", 
	      0, NULL, "WSAEINVALIDPROCTABLE" },
  /* 105 */ { WSAEINVALIDPROVIDER, "Invalid service provider",
	      0, NULL, "WSAEINVALIDPROVIDER" },
  /* 106 */ { WSAEPROVIDERFAILEDINIT, "Service provider failure", 
	      0, NULL, "WSAEPROVIDERFAILEDINIT" },
  /* 107 */ { WSASYSCALLFAILURE, "System call failed",
	      0, NULL, "WSASYSCALLFAILURE" },
  /* 108 */ { WSASERVICE_NOT_FOUND, "No such service",
	      0, NULL, "WSASERVICE_NOT_FOUND" },
  /* 109 */ { WSATYPE_NOT_FOUND, "Class not found", 
	      0, NULL, "WSATYPE_NOT_FOUND" },
  /* 110 */ { WSA_E_NO_MORE, "No more services",
	      0, NULL, "WSA_E_NO_MORE" },
  /* 111 */ { WSA_E_CANCELLED, "Service lookup cancelled", 
	      0, NULL, "WSA_E_CANCELLED" },
  /* 112 */ { WSAEREFUSED, "Database query refused", 
	      0, NULL, "WSAEREFUSED" },
  /* end */ { -1, NULL, -1, NULL, NULL }
};

/* Extended list of error structures.  */
static socket_error_t socket_h_errno [] = {
  /* 000 */ { 0, NULL, 0, NULL, NULL },
  /* 001 */ { WSAHOST_NOT_FOUND, "Host not found",
	      HOST_NOT_FOUND, "HOST_NOT_FOUND", "WSAHOST_NOT_FOUND" },
  /* 002 */ { WSATRY_AGAIN, "Non-authoritative host not found",
	      TRY_AGAIN, "TRY_AGAIN", "WSATRY_AGAIN" },
  /* 003 */ { WSANO_RECOVERY, "This is a non-recoverable error", 
	      NO_RECOVERY, "NO_RECOVERY", "WSANO_RECOVERY" },
  /* 004 */ { WSANO_DATA, "Valid name, no data record of requested type",
	      NO_DATA, "NO_DATA", "WSANO_DATA" },
  /* 005 */ { WSANO_ADDRESS, "No address, look for MX record",
	      NO_ADDRESS, "NO_ADDRESS", "WSANO_ADDRESS" },
  /* end */ { -1, NULL, -1, NULL, NULL }
};

/* Returns the result of @code{WSAGetLastError()}.  */
int
scm_i_socket_errno (void)
{
  return WSAGetLastError ();
}

/* Returns a valid error message for Winsock-API error codes obtained via
   @code{WSAGetLastError()} or NULL otherwise.  */
char *
scm_i_socket_strerror (int error)
{
  if (error >= WSABASEERR && error <= (WSABASEERR + 112))
    return socket_errno[error - WSABASEERR].str;
  else if (error >= (WSABASEERR + 1000) && error <= (WSABASEERR + 1005))
    return socket_h_errno[error - (WSABASEERR + 1000)].str;
  return NULL;
}

/* Define both the original and replacement error symbol is possible.  Thus
   the user is able to check symbolic errors after unsuccessful networking
   function calls.  */
static void
scm_socket_symbols_Win32 (socket_error_t * e)
{
  while (e->error != -1)
    {
      if (e->error)
	{
	  if (e->correct_str)
	    scm_c_define (e->correct_str, SCM_MAKINUM (e->error));
	  if (e->replace && e->replace_str)
	    scm_c_define (e->replace_str, SCM_MAKINUM (e->replace));
	}
      e++;
    }
}

/* Initialize Winsock API under M$-Windows.  */
void
scm_i_init_socket_Win32 (void)
{
  scm_socket_symbols_Win32 (socket_errno);
  scm_socket_symbols_Win32 (socket_h_errno);
}
