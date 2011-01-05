/* "net_db.c" network database support
 * Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2006, 2009, 2010, 2011 Free Software Foundation, Inc.
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



/* Written in 1994 by Aubrey Jaffer.
 * Thanks to Hallvard.Tretteberg@si.sintef.no for inspiration and discussion.
 * Rewritten by Gary Houston to be a closer interface to the C socket library.
 * Split into net_db.c and socket.c.
 */


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <verify.h>
#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/dynwind.h"

#include "libguile/validate.h"
#include "libguile/net_db.h"
#include "libguile/socket.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#ifdef __MINGW32__
#include "win32-socket.h"
#endif

#if !defined (HAVE_H_ERRNO) && !defined (__MINGW32__) && !defined (__CYGWIN__)
/* h_errno not found in netdb.h, maybe this will help.  */
extern int h_errno;
#endif

#if defined HAVE_HSTRERROR && !HAVE_DECL_HSTRERROR	\
  && !defined __MINGW32__ && !defined __CYGWIN__
/* Some OSes, such as Tru64 5.1b, lack a declaration for hstrerror(3).  */
extern const char *hstrerror (int);
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
      scm_error (key, subr, errmsg, SCM_BOOL_F, SCM_EOL);
    }
}

/* Should take an extra arg for address format (will be needed for IPv6).
   Should use reentrant facilities if available.
 */

SCM_DEFINE (scm_gethost, "gethost", 0, 1, 0, 
            (SCM host),
	    "@deffnx {Scheme Procedure} gethostbyname hostname\n"
	    "@deffnx {Scheme Procedure} gethostbyaddr address\n"
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
  SCM result = scm_c_make_vector (5, SCM_UNSPECIFIED);
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
  else if (scm_is_string (host))
    {
      char *str = scm_to_locale_string (host);
      entry = gethostbyname (str);
      free (str);
    }
  else
    {
      inad.s_addr = htonl (scm_to_ulong (host));
      entry = gethostbyaddr ((char *) &inad, sizeof (inad), AF_INET);
    }

  if (!entry)
    scm_resolv_error (FUNC_NAME, host);
  
  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->h_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_makfromstrs (-1, entry->h_aliases));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_int (entry->h_addrtype));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_int (entry->h_length));
  if (sizeof (struct in_addr) != entry->h_length)
    {
      SCM_SIMPLE_VECTOR_SET(result, 4, SCM_BOOL_F);
      return result;
    }
  for (argv = entry->h_addr_list; argv[i]; i++);
  while (i--)
    {
      inad = *(struct in_addr *) argv[i];
      lst = scm_cons (scm_from_ulong (ntohl (inad.s_addr)), lst);
    }
  SCM_SIMPLE_VECTOR_SET(result, 4, lst);
  return result;
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
	    "@deffnx {Scheme Procedure} getnetbyname net-name\n"
	    "@deffnx {Scheme Procedure} getnetbyaddr net-number\n"
	    "Look up a network by name or net number in the network database.  The\n"
	    "@var{net-name} argument must be a string, and the @var{net-number}\n"
	    "argument must be an integer.  @code{getnet} will accept either type of\n"
	    "argument, behaving like @code{getnetent} (see below) if no arguments are\n"
	    "given.")
#define FUNC_NAME s_scm_getnet
{
  SCM result = scm_c_make_vector (4, SCM_UNSPECIFIED);
  struct netent *entry;
  int eno;

  if (SCM_UNBNDP (net))
    {
      entry = getnetent ();
      if (! entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
    }
  else if (scm_is_string (net))
    {
      char *str = scm_to_locale_string (net);
      entry = getnetbyname (str);
      eno = errno;
      free (str);
    }
  else
    {
      unsigned long netnum = scm_to_ulong (net);
      entry = getnetbyaddr (netnum, AF_INET);
      eno = errno;
    }

  if (!entry)
    SCM_SYSERROR_MSG ("no such network ~A", scm_list_1 (net), eno);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->n_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_makfromstrs (-1, entry->n_aliases));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_int (entry->n_addrtype));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_ulong (entry->n_net));
  return result;
}
#undef FUNC_NAME
#endif

#if defined (HAVE_GETPROTOENT) || defined (__MINGW32__)
SCM_DEFINE (scm_getproto, "getproto", 0, 1, 0, 
            (SCM protocol),
	    "@deffnx {Scheme Procedure} getprotobyname name\n"
	    "@deffnx {Scheme Procedure} getprotobynumber number\n"
	    "Look up a network protocol by name or by number.  @code{getprotobyname}\n"
	    "takes a string argument, and @code{getprotobynumber} takes an integer\n"
	    "argument.  @code{getproto} will accept either type, behaving like\n"
	    "@code{getprotoent} (see below) if no arguments are supplied.")
#define FUNC_NAME s_scm_getproto
{
  SCM result = scm_c_make_vector (3, SCM_UNSPECIFIED);
  struct protoent *entry;
  int eno;

  if (SCM_UNBNDP (protocol))
    {
      entry = getprotoent ();
      if (! entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
    }
  else if (scm_is_string (protocol))
    {
      char *str = scm_to_locale_string (protocol);
      entry = getprotobyname (str);
      eno = errno;
      free (str);
    }
  else
    {
      unsigned long protonum = scm_to_ulong (protocol);
      entry = getprotobynumber (protonum);
      eno = errno;
    }

  if (!entry)
    SCM_SYSERROR_MSG ("no such protocol ~A", scm_list_1 (protocol), eno);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->p_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_makfromstrs (-1, entry->p_aliases));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_int (entry->p_proto));
  return result;
}
#undef FUNC_NAME
#endif

#if defined (HAVE_GETSERVENT) || defined (__MINGW32__)
static SCM
scm_return_entry (struct servent *entry)
{
  SCM result = scm_c_make_vector (4, SCM_UNSPECIFIED);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->s_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_makfromstrs (-1, entry->s_aliases));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_uint16 (ntohs (entry->s_port)));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_locale_string (entry->s_proto));
  return result;
}

SCM_DEFINE (scm_getserv, "getserv", 0, 2, 0,
            (SCM name, SCM protocol),
	    "@deffnx {Scheme Procedure} getservbyname name protocol\n"
	    "@deffnx {Scheme Procedure} getservbyport port protocol\n"
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
  char *protoname;
  int eno;

  if (SCM_UNBNDP (name))
    {
      entry = getservent ();
      if (!entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
      return scm_return_entry (entry);
    }

  scm_dynwind_begin (0);

  protoname = scm_to_locale_string (protocol);
  scm_dynwind_free (protoname);

  if (scm_is_string (name))
    {
      char *str = scm_to_locale_string (name);
      entry = getservbyname (str, protoname);
      eno = errno;
      free (str);
    }
  else
    {
      entry = getservbyport (htons (scm_to_int (name)), protoname);
      eno = errno;
    }

  if (!entry)
    SCM_SYSERROR_MSG("no such service ~A", scm_list_1 (name), eno);

  scm_dynwind_end ();
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
    sethostent (scm_is_true (stayopen));
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
    setnetent (scm_is_true (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined (HAVE_SETPROTOENT) && defined (HAVE_ENDPROTOENT) || defined (__MINGW32__)
SCM_DEFINE (scm_setproto, "setproto", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endprotoent}.\n"
	    "Otherwise it is equivalent to @code{setprotoent stayopen}.")
#define FUNC_NAME s_scm_setproto
{
  if (SCM_UNBNDP (stayopen))
    endprotoent ();
  else
    setprotoent (scm_is_true (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined (HAVE_SETSERVENT) && defined (HAVE_ENDSERVENT) || defined (__MINGW32__)
SCM_DEFINE (scm_setserv, "setserv", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endservent}.\n"
	    "Otherwise it is equivalent to @code{setservent stayopen}.")
#define FUNC_NAME s_scm_setserv
{
  if (SCM_UNBNDP (stayopen))
    endservent ();
  else
    setservent (scm_is_true (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif


/* Protocol-independent name resolution with getaddrinfo(3) & co.  */

SCM_SYMBOL (sym_getaddrinfo_error, "getaddrinfo-error");

/* Make sure the `AI_*' flags can be stored as INUMs.  */
verify (SCM_I_INUM (SCM_I_MAKINUM (AI_ALL)) == AI_ALL);

/* Valid values for the `ai_flags' to `struct addrinfo'.  */
SCM_VARIABLE_INIT (sym_ai_passive, "AI_PASSIVE",
		   SCM_I_MAKINUM (AI_PASSIVE));
SCM_VARIABLE_INIT (sym_ai_canonname, "AI_CANONNAME",
		   SCM_I_MAKINUM (AI_CANONNAME));
SCM_VARIABLE_INIT (sym_ai_numerichost, "AI_NUMERICHOST",
		   SCM_I_MAKINUM (AI_NUMERICHOST));
SCM_VARIABLE_INIT (sym_ai_numericserv, "AI_NUMERICSERV",
		   SCM_I_MAKINUM (AI_NUMERICSERV));
SCM_VARIABLE_INIT (sym_ai_v4mapped, "AI_V4MAPPED",
		   SCM_I_MAKINUM (AI_V4MAPPED));
SCM_VARIABLE_INIT (sym_ai_all, "AI_ALL",
		   SCM_I_MAKINUM (AI_ALL));
SCM_VARIABLE_INIT (sym_ai_addrconfig, "AI_ADDRCONFIG",
		   SCM_I_MAKINUM (AI_ADDRCONFIG));

/* Return a Scheme vector whose elements correspond to the fields of C_AI,
   ignoring the `ai_next' field.  This function is not exported because the
   definition of `struct addrinfo' is provided by Gnulib.  */
static SCM
scm_from_addrinfo (const struct addrinfo *c_ai)
{
  SCM ai;

  /* Note: The indices here must be kept synchronized with those used by the
     `addrinfo:' procedures in `networking.scm'.  */

  ai = scm_c_make_vector (6, SCM_UNDEFINED);
  SCM_SIMPLE_VECTOR_SET (ai, 0, scm_from_int (c_ai->ai_flags));
  SCM_SIMPLE_VECTOR_SET (ai, 1, scm_from_int (c_ai->ai_family));
  SCM_SIMPLE_VECTOR_SET (ai, 2, scm_from_int (c_ai->ai_socktype));
  SCM_SIMPLE_VECTOR_SET (ai, 3, scm_from_int (c_ai->ai_protocol));
  SCM_SIMPLE_VECTOR_SET (ai, 4,
			 scm_from_sockaddr (c_ai->ai_addr, c_ai->ai_addrlen));
  SCM_SIMPLE_VECTOR_SET (ai, 5,
			 c_ai->ai_canonname != NULL
			 ? scm_from_locale_string (c_ai->ai_canonname)
			 : SCM_BOOL_F);

  return ai;
}

SCM_DEFINE (scm_getaddrinfo, "getaddrinfo", 1, 5, 0,
	    (SCM name, SCM service, SCM hint_flags, SCM hint_family,
	     SCM hint_socktype, SCM hint_protocol),
	    "Return a list of @code{addrinfo} structures containing "
	    "a socket address and associated information for host @var{name} "
	    "and/or @var{service} to be used in creating a socket with "
	    "which to address the specified service.\n\n"
	    "@example\n"
	    "(let* ((ai (car (getaddrinfo \"www.gnu.org\" \"http\")))\n"
	    "       (s  (socket (addrinfo:fam ai) (addrinfo:socktype ai)\n"
	    "                   (addrinfo:protocol ai))))\n"
	    "  (connect s (addrinfo:addr ai))\n"
	    "  s)\n"
	    "@end example\n\n"
	    "When @var{service} is omitted or is @code{#f}, return "
	    "network-level addresses for @var{name}.  When @var{name} "
	    "is @code{#f} @var{service} must be provided and service "
	    "locations local to the caller are returned.\n"
	    "\n"
	    "Additional hints can be provided.  When specified, "
	    "@var{hint_flags} should be a bitwise-or of zero or more "
	    "constants among the following:\n\n"
	    "@table @code\n"
	    "@item AI_PASSIVE\n"
	    "Socket address is intended for @code{bind}.\n\n"
	    "@item AI_CANONNAME\n"
	    "Request for canonical host name, available via "
	    "@code{addrinfo:canonname}.  This makes sense mainly when "
	    "DNS lookups are involved.\n\n"
	    "@item AI_NUMERICHOST\n"
	    "Specifies that @var{name} is a numeric host address string "
	    "(e.g., @code{\"127.0.0.1\"}), meaning that name resolution "
	    "will not be used.\n\n"
	    "@item AI_NUMERICSERV\n"
	    "Likewise, specifies that @var{service} is a numeric port "
	    "string (e.g., @code{\"80\"}).\n\n"
	    "@item AI_ADDRCONFIG\n"
	    "Return only addresses configured on the local system.  It is "
	    "highly recommended to provide this flag when the returned "
	    "socket addresses are to be used to make connections; "
	    "otherwise, some of the returned addresses could be unreachable "
	    "or use a protocol that is not supported.\n\n"
	    "@item AI_V4MAPPED\n"
	    "When looking up IPv6 addresses, return mapped "
	    "IPv4 addresses if there is no IPv6 address available at all.\n\n"
	    "@item AI_ALL\n"
	    "If this flag is set along with @code{AI_V4MAPPED} when looking "
	    "up IPv6 addresses, return all IPv6 addresses "
	    "as well as all IPv4 addresses, the latter mapped to IPv6 "
	    "format.\n"
	    "@end table\n\n"
	    "When given, @var{hint_family} should specify the requested "
	    "address family, e.g., @code{AF_INET6}.  Similarly, "
	    "@var{hint_socktype} should specify the requested socket type "
	    "(e.g., @code{SOCK_DGRAM}), and @var{hint_protocol} should "
	    "specify the requested protocol (its value is interpretered "
	    "as in calls to @code{socket}).\n"
	    "\n"
	    "On error, an exception with key @code{getaddrinfo-error} is "
	    "thrown, with an error code (an integer) as its argument:\n\n"
	    "@example\n"
	    "(catch 'getaddrinfo-error\n"
            "  (lambda ()\n"
            "    (getaddrinfo \"www.gnu.org\" \"gopher\"))\n"
            "  (lambda (key errcode)\n"
            "    (cond ((= errcode EAI_SERVICE)\n"
            "           (display \"doesn't know about Gopher!\\n\"))\n"
            "          ((= errcode EAI_NONAME)\n"
            "           (display \"www.gnu.org not found\\n\"))\n"
            "          (else\n"
            "           (format #t \"something wrong: ~a\\n\"\n"
            "                   (gai-strerror errcode))))))\n"
	    "@end example\n"
	    "\n"
	    "Error codes are:\n\n"
	    "@table @code\n"
	    "@item EAI_AGAIN\n"
	    "The name or service could not be resolved at this time. Future "
	    "attempts may succeed.\n\n"
	    "@item EAI_BADFLAGS\n"
	    "@var{hint_flags} contains an invalid value.\n\n"
	    "@item EAI_FAIL\n"
	    "A non-recoverable error occurred when attempting to "
	    "resolve the name.\n\n"
	    "@item EAI_FAMILY\n"
	    "@var{hint_family} was not recognized.\n\n"
	    "@item EAI_NONAME\n"
	    "Either @var{name} does not resolve for the supplied parameters, "
	    "or neither @var{name} nor @var{service} were supplied.\n\n"
	    "@item EAI_NODATA\n"
	    "This non-POSIX error code can be returned on GNU systems when a\n"
	    "request was actually made but returned no data, meaning\n"
	    "that no address is associated with @var{name}.  Error handling\n"
	    "code should be prepared to handle it when it is defined.\n\n"
	    "@item EAI_SERVICE\n"
	    "@var{service} was not recognized for the specified socket type.\n\n"
	    "@item EAI_SOCKTYPE\n"
	    "@var{hint_socktype} was not recognized.\n\n"
	    "@item EAI_SYSTEM\n"
	    "A system error occurred; the error code can be found in "
	    "@code{errno}.\n"
	    "@end table\n"
	    "\n"
	    "Users are encouraged to read the "
	    "@url{http://www.opengroup.org/onlinepubs/9699919799/functions/getaddrinfo.html,"
	    "POSIX specification} for more details.\n")
#define FUNC_NAME s_scm_getaddrinfo
{
  int err;
  char *c_name, *c_service;
  struct addrinfo c_hints, *c_result;
  SCM result = SCM_EOL;

  if (scm_is_true (name))
    SCM_VALIDATE_STRING (SCM_ARG1, name);

  if (!SCM_UNBNDP (service) && scm_is_true (service))
    SCM_VALIDATE_STRING (SCM_ARG2, service);

  scm_dynwind_begin (0);

  if (scm_is_string (name))
    {
      c_name = scm_to_locale_string (name);
      scm_dynwind_free (c_name);
    }
  else
    c_name = NULL;

  if (scm_is_string (service))
    {
      c_service = scm_to_locale_string (service);
      scm_dynwind_free (c_service);
    }
  else
    c_service = NULL;

  memset (&c_hints, 0, sizeof (c_hints));
  if (!SCM_UNBNDP (hint_flags))
    {
      c_hints.ai_flags = scm_to_int (hint_flags);
      if (!SCM_UNBNDP (hint_family))
	{
	  c_hints.ai_family = scm_to_int (hint_family);
	  if (!SCM_UNBNDP (hint_socktype))
	    {
	      c_hints.ai_socktype = scm_to_int (hint_socktype);
	      if (!SCM_UNBNDP (hint_family))
		c_hints.ai_family = scm_to_int (hint_family);
	    }
	}
    }

  err = getaddrinfo (c_name, c_service, &c_hints, &c_result);
  if (err == 0)
    {
      SCM *prev_addr;
      struct addrinfo *a;

      for (prev_addr = &result, a = c_result;
	   a != NULL;
	   a = a->ai_next, prev_addr = SCM_CDRLOC (*prev_addr))
	*prev_addr = scm_list_1 (scm_from_addrinfo (a));

      freeaddrinfo (c_result);
    }
  else
    scm_throw (sym_getaddrinfo_error, scm_list_1 (scm_from_int (err)));

  scm_dynwind_end ();

  return result;
}
#undef FUNC_NAME

/* Make sure the `EAI_*' flags can be stored as INUMs.  */
verify (SCM_I_INUM (SCM_I_MAKINUM (EAI_BADFLAGS)) == EAI_BADFLAGS);

/* Error codes returned by `getaddrinfo'.  */
SCM_VARIABLE_INIT (sym_eai_badflags, "EAI_BADFLAGS",
		   SCM_I_MAKINUM (EAI_BADFLAGS));
SCM_VARIABLE_INIT (sym_eai_noname, "EAI_NONAME",
		   SCM_I_MAKINUM (EAI_NONAME));
SCM_VARIABLE_INIT (sym_eai_again, "EAI_AGAIN",
		   SCM_I_MAKINUM (EAI_AGAIN));
SCM_VARIABLE_INIT (sym_eai_fail, "EAI_FAIL",
		   SCM_I_MAKINUM (EAI_FAIL));
SCM_VARIABLE_INIT (sym_eai_family, "EAI_FAMILY",
		   SCM_I_MAKINUM (EAI_FAMILY));
SCM_VARIABLE_INIT (sym_eai_socktype, "EAI_SOCKTYPE",
		   SCM_I_MAKINUM (EAI_SOCKTYPE));
SCM_VARIABLE_INIT (sym_eai_service, "EAI_SERVICE",
		   SCM_I_MAKINUM (EAI_SERVICE));
SCM_VARIABLE_INIT (sym_eai_memory, "EAI_MEMORY",
		   SCM_I_MAKINUM (EAI_MEMORY));
SCM_VARIABLE_INIT (sym_eai_system, "EAI_SYSTEM",
		   SCM_I_MAKINUM (EAI_SYSTEM));
SCM_VARIABLE_INIT (sym_eai_overflow, "EAI_OVERFLOW",
		   SCM_I_MAKINUM (EAI_OVERFLOW));

/* The following values are GNU extensions.  */
#ifdef EAI_NODATA
SCM_VARIABLE_INIT (sym_eai_nodata, "EAI_NODATA",
		   SCM_I_MAKINUM (EAI_NODATA));
#endif
#ifdef EAI_ADDRFAMILY
SCM_VARIABLE_INIT (sym_eai_addrfamily, "EAI_ADDRFAMILY",
		   SCM_I_MAKINUM (EAI_ADDRFAMILY));
#endif
#ifdef EAI_INPROGRESS
SCM_VARIABLE_INIT (sym_eai_inprogress, "EAI_INPROGRESS",
		   SCM_I_MAKINUM (EAI_INPROGRESS));
#endif
#ifdef EAI_CANCELED
SCM_VARIABLE_INIT (sym_eai_canceled, "EAI_CANCELED",
		   SCM_I_MAKINUM (EAI_CANCELED));
#endif
#ifdef EAI_NOTCANCELED
SCM_VARIABLE_INIT (sym_eai_notcanceled, "EAI_NOTCANCELED",
		   SCM_I_MAKINUM (EAI_NOTCANCELED));
#endif
#ifdef EAI_ALLDONE
SCM_VARIABLE_INIT (sym_eai_alldone, "EAI_ALLDONE",
		   SCM_I_MAKINUM (EAI_ALLDONE));
#endif
#ifdef EAI_INTR
SCM_VARIABLE_INIT (sym_eai_intr, "EAI_INTR",
		   SCM_I_MAKINUM (EAI_INTR));
#endif
#ifdef EAI_IDN_ENCODE
SCM_VARIABLE_INIT (sym_eai_idn_encode, "EAI_IDN_ENCODE",
		   SCM_I_MAKINUM (EAI_IDN_ENCODE));
#endif

SCM_DEFINE (scm_gai_strerror, "gai-strerror", 1, 0, 0,
	    (SCM error),
	    "Return a string describing @var{error}, an integer error code "
	    "returned by @code{getaddrinfo}.")
#define FUNC_NAME s_scm_gai_strerror
{
  return scm_from_locale_string (gai_strerror (scm_to_int (error)));
}
#undef FUNC_NAME

/* TODO: Add a getnameinfo(3) wrapper.  */


void
scm_init_net_db ()
{
  scm_add_feature ("net-db");
#include "libguile/net_db.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
