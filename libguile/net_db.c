/* "net_db.c" network database support
 * Copyright (C) 1995, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
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


/* Written in 1994 by Aubrey Jaffer.
 * Thanks to Hallvard.Tretteberg@si.sintef.no for inspiration and discussion.
 * Rewritten by Gary Houston to be a closer interface to the C socket library.
 * Split into net_db.c and socket.c.
 */


#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/net_db.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#if !defined (HAVE_H_ERRNO)
extern int h_errno;
#endif



#ifndef STDC_HEADERS
int close ();
#endif /* STDC_HEADERS */

#ifndef HAVE_INET_ATON
extern int inet_aton ();
#endif

SCM_DEFINE (scm_inet_aton, "inet-aton", 1, 0, 0, 
            (SCM address),
	    "Converts a string containing an Internet host address in the traditional\n"
	    "dotted decimal notation into an integer.\n\n"
	    "@smalllisp\n"
	    "(inet-aton \"127.0.0.1\") @result{} 2130706433\n\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_inet_aton
{
  struct in_addr soka;

  SCM_VALIDATE_ROSTRING (1,address);
  if (SCM_SUBSTRP (address))
    address = scm_makfromstr (SCM_ROCHARS (address), SCM_ROLENGTH (address), 0);
  if (inet_aton (SCM_ROCHARS (address), &soka) == 0)
    SCM_MISC_ERROR ("bad address", SCM_EOL);
  return scm_ulong2num (ntohl (soka.s_addr));
}
#undef FUNC_NAME


SCM_DEFINE (scm_inet_ntoa, "inet-ntoa", 1, 0, 0, 
            (SCM inetid),
	    "Converts an integer Internet host address into a string with the\n"
	    "traditional dotted decimal representation.\n\n"
	    "@smalllisp\n"
	    "(inet-ntoa 2130706433) @result{} \"127.0.0.1\"\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_inet_ntoa
{
  struct in_addr addr;
  char *s;
  SCM answer;
  addr.s_addr = htonl (SCM_NUM2ULONG (1,inetid));
  s = inet_ntoa (addr);
  answer = scm_makfromstr (s, strlen (s), 0);
  return answer;
}
#undef FUNC_NAME

#ifdef HAVE_INET_NETOF
SCM_DEFINE (scm_inet_netof, "inet-netof", 1, 0, 0, 
            (SCM address),
	    "Returns the network number part of the given integer Internet address.\n\n"
	    "@smalllisp\n"
	    "(inet-netof 2130706433) @result{} 127\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_inet_netof
{
  struct in_addr addr;
  addr.s_addr = htonl (SCM_NUM2ULONG (1,address));
  return scm_ulong2num ((unsigned long) inet_netof (addr));
}
#undef FUNC_NAME
#endif

#ifdef HAVE_INET_LNAOF
SCM_DEFINE (scm_lnaof, "inet-lnaof", 1, 0, 0, 
            (SCM address),
	    "Returns the local-address-with-network part of the given Internet\n"
	    "address.\n\n"
	    "@smalllisp\n"
	    "(inet-lnaof 2130706433) @result{} 1\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_lnaof
{
  struct in_addr addr;
  addr.s_addr = htonl (SCM_NUM2ULONG (1,address));
  return scm_ulong2num ((unsigned long) inet_lnaof (addr));
}
#undef FUNC_NAME
#endif

#ifdef HAVE_INET_MAKEADDR
SCM_DEFINE (scm_inet_makeaddr, "inet-makeaddr", 2, 0, 0,
            (SCM net, SCM lna),
	    "Makes an Internet host address by combining the network number @var{net}\n"
	    "with the local-address-within-network number @var{lna}.\n\n"
	    "@smalllisp\n"
	    "(inet-makeaddr 127 1) @result{} 2130706433\n"
	    "@end smalllisp")
#define FUNC_NAME s_scm_inet_makeaddr
{
  struct in_addr addr;
  unsigned long netnum;
  unsigned long lnanum;

#if 0 /* GJB:FIXME:: */
  SCM_VALIDATE_INUM_COPY (1,net,netnum);
  SCM_VALIDATE_INUM_COPY (2,lna,lnanum);
#else
  netnum = SCM_NUM2ULONG (1, net);
  lnanum = SCM_NUM2ULONG (2, lna);
#endif
  addr = inet_makeaddr (netnum, lnanum);
  return scm_ulong2num (ntohl (addr.s_addr));
}
#undef FUNC_NAME
#endif

SCM_SYMBOL (scm_host_not_found_key, "host-not-found");
SCM_SYMBOL (scm_try_again_key, "try-again");
SCM_SYMBOL (scm_no_recovery_key, "no-recovery");
SCM_SYMBOL (scm_no_data_key, "no-data");

static void scm_resolv_error (const char *subr, SCM bad_value)
{
#ifdef NETDB_INTERNAL
  if (h_errno == NETDB_INTERNAL)
    {
      /* errno supposedly contains a useful value.  */
      scm_syserror (subr);
    }
  else
#endif
    {
      SCM key;
      const char *errmsg;

      switch (h_errno)
	{
	case HOST_NOT_FOUND:
	  key = scm_host_not_found_key;
	  errmsg = "Unknown host"; 
	  break;
	case TRY_AGAIN:	
	  key = scm_try_again_key;
	  errmsg = "Host name lookup failure";
	  break;
	case NO_RECOVERY:
	  key = scm_no_recovery_key;
	  errmsg = "Unknown server error"; 
	  break;
	case NO_DATA:
	  key = scm_no_data_key;
	  errmsg = "No address associated with name";
	  break;
	default:
	  scm_misc_error (subr, "Unknown resolver error", SCM_EOL);
	  errmsg = NULL;
	}

#ifdef HAVE_HSTRERROR
      errmsg = (const char *) hstrerror (h_errno);
#endif
      scm_error (key, subr, errmsg, scm_cons (bad_value, SCM_EOL), SCM_EOL);
    }
}

/* Should take an extra arg for address format (will be needed for IPv6).
   Should use reentrant facilities if available.
 */

SCM_DEFINE (scm_gethost, "gethost", 0, 1, 0, 
            (SCM host),
	    "@deffnx procedure gethostbyname hostname\n"
	    "@deffnx procedure gethostbyaddr address\n"
	    "Look up a host by name or address, returning a host object.  The\n"
	    "@code{gethost} procedure will accept either a string name or an integer\n"
	    "address; if given no arguments, it behaves like @code{gethostent} (see\n"
	    "below).  If a name or address is supplied but the address can not be\n"
	    "found, an error will be thrown to one of the keys:\n"
	    "@code{host-not-found}, @code{try-again}, @code{no-recovery} or\n"
	    "@code{no-data}, corresponding to the equivalent @code{h_error} values.\n"
	    "Unusual conditions may result in errors thrown to the\n"
	    "@code{system-error} or @code{misc_error} keys.")
#define FUNC_NAME s_scm_gethost
{
  SCM ans = scm_make_vector (SCM_MAKINUM (5), SCM_UNSPECIFIED);
  SCM *ve = SCM_VELTS (ans);
  SCM lst = SCM_EOL;
  struct hostent *entry;
  struct in_addr inad;
  char **argv;
  int i = 0;
  if (SCM_UNBNDP (host))
    {
#ifdef HAVE_GETHOSTENT
      entry = gethostent ();
#else
      entry = NULL;
#endif
      if (! entry)
	{
	  /* As far as I can tell, there's no good way to tell whether
             zero means an error or end-of-file.  The trick of
             clearing errno before calling gethostent and checking it
             afterwards doesn't cut it, because, on Linux, it seems to
             try to contact some other server (YP?) and fails, which
             is a benign failure.  */
	  return SCM_BOOL_F;
	}
    }
  else if (SCM_ROSTRINGP (host))
    {
      SCM_COERCE_SUBSTR (host);
      entry = gethostbyname (SCM_ROCHARS (host));
    }
  else
    {
      inad.s_addr = htonl (SCM_NUM2ULONG (1,host));
      entry = gethostbyaddr ((char *) &inad, sizeof (inad), AF_INET);
    }
  if (!entry)
    scm_resolv_error (FUNC_NAME, host);
  
  ve[0] = scm_makfromstr (entry->h_name, 
			  (scm_sizet) strlen (entry->h_name), 0);
  ve[1] = scm_makfromstrs (-1, entry->h_aliases);
  ve[2] = SCM_MAKINUM (entry->h_addrtype + 0L);
  ve[3] = SCM_MAKINUM (entry->h_length + 0L);
  if (sizeof (struct in_addr) != entry->h_length)
    {
      ve[4] = SCM_BOOL_F;
      return ans;
    }
  for (argv = entry->h_addr_list; argv[i]; i++);
  while (i--)
    {
      inad = *(struct in_addr *) argv[i];
      lst = scm_cons (scm_ulong2num (ntohl (inad.s_addr)), lst);
    }
  ve[4] = lst;
  return ans;
}
#undef FUNC_NAME


/* In all subsequent getMUMBLE functions, when we're called with no
   arguments, we're supposed to traverse the tables entry by entry.
   However, there doesn't seem to be any documented way to distinguish
   between end-of-table and an error; in both cases the functions
   return zero.  Gotta love Unix.  For the time being, we clear errno,
   and if we get a zero and errno is set, we signal an error.  This
   doesn't seem quite right (what if errno gets set as part of healthy
   operation?), but it seems to work okay.  We'll see.  */

#if defined(HAVE_GETNETENT) && defined(HAVE_GETNETBYNAME) && defined(HAVE_GETNETBYADDR)
SCM_DEFINE (scm_getnet, "getnet", 0, 1, 0, 
            (SCM net),
	    "@deffnx procedure getnetbyname net-name\n"
	    "@deffnx procedure getnetbyaddr net-number\n"
	    "Look up a network by name or net number in the network database.  The\n"
	    "@var{net-name} argument must be a string, and the @var{net-number}\n"
	    "argument must be an integer.  @code{getnet} will accept either type of\n"
	    "argument, behaving like @code{getnetent} (see below) if no arguments are\n"
	    "given.")
#define FUNC_NAME s_scm_getnet
{
  SCM ans;
  SCM *ve;
  struct netent *entry;

  ans = scm_make_vector (SCM_MAKINUM (4), SCM_UNSPECIFIED);
  ve = SCM_VELTS (ans);
  if (SCM_UNBNDP (net))
    {
      errno = 0;
      entry = getnetent ();
      if (! entry)
	{
	  if (errno)
	    SCM_SYSERROR;
	  else 
	    return SCM_BOOL_F;
	}
    }
  else if (SCM_ROSTRINGP (net))
    {
      SCM_COERCE_SUBSTR (net);
      entry = getnetbyname (SCM_ROCHARS (net));
    }
  else
    {
      unsigned long netnum;
      netnum = SCM_NUM2ULONG (1, net);
      entry = getnetbyaddr (netnum, AF_INET);
    }
  if (!entry)
    SCM_SYSERROR_MSG ("no such network ~A",
		      scm_listify (net, SCM_UNDEFINED), errno);
  ve[0] = scm_makfromstr (entry->n_name, (scm_sizet) strlen (entry->n_name), 0);
  ve[1] = scm_makfromstrs (-1, entry->n_aliases);
  ve[2] = SCM_MAKINUM (entry->n_addrtype + 0L);
  ve[3] = scm_ulong2num (entry->n_net + 0L);
  return ans;
}
#undef FUNC_NAME
#endif

#ifdef HAVE_GETPROTOENT
SCM_DEFINE (scm_getproto, "getproto", 0, 1, 0, 
            (SCM protocol),
	    "@deffnx procedure getprotobyname name\n"
	    "@deffnx procedure getprotobynumber number\n"
	    "Look up a network protocol by name or by number.  @code{getprotobyname}\n"
	    "takes a string argument, and @code{getprotobynumber} takes an integer\n"
	    "argument.  @code{getproto} will accept either type, behaving like\n"
	    "@code{getprotoent} (see below) if no arguments are supplied.")
#define FUNC_NAME s_scm_getproto
{
  SCM ans;
  SCM *ve;
  struct protoent *entry;

  ans = scm_make_vector (SCM_MAKINUM (3), SCM_UNSPECIFIED);
  ve = SCM_VELTS (ans);
  if (SCM_UNBNDP (protocol))
    {
      errno = 0;
      entry = getprotoent ();
      if (! entry)
	{
	  if (errno)
	    SCM_SYSERROR;
	  else
	    return SCM_BOOL_F;
	}
    }
  else if (SCM_ROSTRINGP (protocol))
    {
      SCM_COERCE_SUBSTR (protocol);
      entry = getprotobyname (SCM_ROCHARS (protocol));
    }
  else
    {
      unsigned long protonum;
      protonum = SCM_NUM2ULONG (1,protocol);
      entry = getprotobynumber (protonum);
    }
  if (!entry)
    SCM_SYSERROR_MSG ("no such protocol ~A",
		      scm_listify (protocol, SCM_UNDEFINED), errno);
  ve[0] = scm_makfromstr (entry->p_name, (scm_sizet) strlen (entry->p_name), 0);
  ve[1] = scm_makfromstrs (-1, entry->p_aliases);
  ve[2] = SCM_MAKINUM (entry->p_proto + 0L);
  return ans;
}
#undef FUNC_NAME
#endif

static SCM
scm_return_entry (struct servent *entry)
{
  SCM ans;
  SCM *ve;

  ans = scm_make_vector (SCM_MAKINUM (4), SCM_UNSPECIFIED);
  ve = SCM_VELTS (ans);
  ve[0] = scm_makfromstr (entry->s_name, (scm_sizet) strlen (entry->s_name), 0);
  ve[1] = scm_makfromstrs (-1, entry->s_aliases);
  ve[2] = SCM_MAKINUM (ntohs (entry->s_port) + 0L);
  ve[3] = scm_makfromstr (entry->s_proto, (scm_sizet) strlen (entry->s_proto), 0);
  return ans;
}

#ifdef HAVE_GETSERVENT
SCM_DEFINE (scm_getserv, "getserv", 0, 2, 0,
            (SCM name, SCM protocol),
	    "@deffnx procedure getservbyname name protocol\n"
	    "@deffnx procedure getservbyport port protocol\n"
	    "Look up a network service by name or by service number, and return a\n"
	    "network service object.  The @var{protocol} argument specifies the name\n"
	    "of the desired protocol; if the protocol found in the network service\n"
	    "database does not match this name, a system error is signalled.\n\n"
	    "The @code{getserv} procedure will take either a service name or number\n"
	    "as its first argument; if given no arguments, it behaves like\n"
	    "@code{getservent} (see below).")
#define FUNC_NAME s_scm_getserv
{
  struct servent *entry;
  if (SCM_UNBNDP (name))
    {
      errno = 0;
      entry = getservent ();
      if (!entry)
	{
	  if (errno)
	    SCM_SYSERROR;
	  else
	    return SCM_BOOL_F;
	}
      return scm_return_entry (entry);
    }
  SCM_VALIDATE_ROSTRING (2,protocol);
  SCM_COERCE_SUBSTR (protocol);
  if (SCM_ROSTRINGP (name))
    {
      SCM_COERCE_SUBSTR (name);
      entry = getservbyname (SCM_ROCHARS (name), SCM_ROCHARS (protocol));
    }
  else
    {
      SCM_VALIDATE_INUM (1,name);
      entry = getservbyport (htons (SCM_INUM (name)), SCM_ROCHARS (protocol));
    }
  if (!entry)
    SCM_SYSERROR_MSG("no such service ~A", 
                     scm_listify (name, SCM_UNDEFINED), errno);
  return scm_return_entry (entry);
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETHOSTENT) && defined(HAVE_ENDHOSTENT)
SCM_DEFINE (scm_sethost, "sethost", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endhostent}.\n"
	    "Otherwise it is equivalent to @code{sethostent stayopen}.")
#define FUNC_NAME s_scm_sethost
{
  if (SCM_UNBNDP (stayopen))
    endhostent ();
  else
    sethostent (SCM_NFALSEP (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETNETENT) && defined(HAVE_ENDNETENT) 
SCM_DEFINE (scm_setnet, "setnet", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endnetent}.\n"
	    "Otherwise it is equivalent to @code{setnetent stayopen}.")
#define FUNC_NAME s_scm_setnet
{
  if (SCM_UNBNDP (stayopen))
    endnetent ();
  else
    setnetent (SCM_NFALSEP (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETPROTOENT) && defined(HAVE_ENDPROTOENT)
SCM_DEFINE (scm_setproto, "setproto", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endprotoent}.\n"
	    "Otherwise it is equivalent to @code{setprotoent stayopen}.")
#define FUNC_NAME s_scm_setproto
{
  if (SCM_UNBNDP (stayopen))
    endprotoent ();
  else
    setprotoent (SCM_NFALSEP (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETSERVENT) && defined(HAVE_ENDSERVENT)
SCM_DEFINE (scm_setserv, "setserv", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endservent}.\n"
	    "Otherwise it is equivalent to @code{setservent stayopen}.")
#define FUNC_NAME s_scm_setserv
{
  if (SCM_UNBNDP (stayopen))
    endservent ();
  else
    setservent (SCM_NFALSEP (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif


void 
scm_init_net_db ()
{
#ifdef INADDR_ANY
  scm_sysintern ("INADDR_ANY", scm_ulong2num (INADDR_ANY));
#endif
#ifdef INADDR_BROADCAST
  scm_sysintern ("INADDR_BROADCAST", scm_ulong2num (INADDR_BROADCAST));
#endif
#ifdef INADDR_NONE
  scm_sysintern ("INADDR_NONE", scm_ulong2num (INADDR_NONE));
#endif
#ifdef INADDR_LOOPBACK
  scm_sysintern ("INADDR_LOOPBACK", scm_ulong2num (INADDR_LOOPBACK));
#endif

  scm_add_feature ("net-db");
#include "libguile/net_db.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
