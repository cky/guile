/* "socket.c" internet socket support for client/server in SCM
    Copyright (C) 1994 Aubrey Jaffer.
    Thanks to Hallvard.Tretteberg@si.sintef.no
    who credits NCSA httpd software by Rob McCool 3/21/93.
    Rewritten by Gary Houston to be a closer interface to the C
    socket library.
    */


#include <stdio.h>
#include <string.h>
#include "_scm.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>



#ifndef STDC_HEADERS
int close P ((int fd));
#endif /* STDC_HEADERS */

SCM_PROC (s_sys_inet_aton, "%inet-aton", 1, 0, 0, scm_sys_inet_aton);
#ifdef __STDC__
SCM 
scm_sys_inet_aton (SCM address)
#else
SCM 
scm_sys_inet_aton (address)
     SCM address;
#endif
{
  struct in_addr soka;
  int rv;
  SCM_ASSERT (SCM_NIMP (address) && SCM_ROSTRINGP (address), address, SCM_ARG1, s_sys_inet_aton);
  if (SCM_SUBSTRP (address))
    address = scm_makfromstr (SCM_ROCHARS (address), SCM_ROLENGTH (address), 0);
  rv = inet_aton (SCM_ROCHARS (address), &soka);
  return rv ? scm_ulong2num (ntohl (soka.s_addr)) : SCM_BOOL_F;
}


SCM_PROC (s_inet_ntoa, "inet-ntoa", 1, 0, 0, scm_inet_ntoa);
#ifdef __STDC__
SCM 
scm_inet_ntoa (SCM inetid)
#else
SCM 
scm_inet_ntoa (inetid)
     SCM inetid;
#endif
{
  struct in_addr addr;
  char *s;
  SCM answer;
  addr.s_addr = htonl (scm_num2ulong (inetid, (char *) SCM_ARG1, s_inet_ntoa));
  SCM_DEFER_INTS;
  s = inet_ntoa (addr);
  answer = scm_makfromstr (s, strlen (s), 0);
  SCM_ALLOW_INTS;
  return answer;
}

SCM_PROC (s_inet_netof, "inet-netof", 1, 0, 0, scm_inet_netof);
#ifdef __STDC__
SCM 
scm_inet_netof (SCM address)
#else
SCM 
scm_inet_netof (address)
     SCM address;
#endif
{
  struct in_addr addr;
  addr.s_addr = htonl (scm_num2ulong (address, (char *) SCM_ARG1, s_inet_netof));
  return scm_ulong2num ((unsigned long) inet_netof (addr));
}

SCM_PROC (s_lnaof, "lnaof", 1, 0, 0, scm_lnaof);
#ifdef __STDC__
SCM 
scm_lnaof (SCM address)
#else
SCM 
scm_lnaof (address)
     SCM address;
#endif
{
  struct in_addr addr;
  addr.s_addr = htonl (scm_num2ulong (address, (char *) SCM_ARG1, s_lnaof));
  return scm_ulong2num ((unsigned long) inet_lnaof (addr));
}


SCM_PROC (s_inet_makeaddr, "inet-makeaddr", 2, 0, 0, scm_inet_makeaddr);
#ifdef __STDC__
SCM 
scm_inet_makeaddr (SCM net, SCM lna)
#else
SCM 
scm_inet_makeaddr (net, lna)
     SCM net;
     SCM lna;
#endif
{
  struct in_addr addr;
  unsigned long netnum;
  unsigned long lnanum;

  netnum = scm_num2ulong (net, (char *) SCM_ARG1, s_inet_makeaddr);
  lnanum = scm_num2ulong (lna, (char *) SCM_ARG2, s_inet_makeaddr);
  addr = inet_makeaddr (netnum, lnanum);
  return scm_ulong2num (ntohl (addr.s_addr));
}


/* !!! Doesn't take address format.
 * Assumes hostent stream isn't reused.
 */

SCM_PROC (s_sys_gethost, "%gethost", 0, 1, 0, scm_sys_gethost);
#ifdef __STDC__
SCM 
scm_sys_gethost (SCM name)
#else
SCM 
scm_sys_gethost (name)
     SCM name;
#endif
{
  SCM ans = scm_make_vector (SCM_MAKINUM (5), SCM_UNSPECIFIED, SCM_BOOL_F);
  SCM *ve = SCM_VELTS (ans);
  SCM lst = SCM_EOL;
  struct hostent *entry;
  struct in_addr inad;
  char **argv;
  int i = 0;
#ifdef HAVE_GETHOSTENT
  if (SCM_UNBNDP (name))
    {
      SCM_DEFER_INTS;
      entry = gethostent ();
    }
  else
#endif
  if (SCM_NIMP (name) && SCM_STRINGP (name))
    {
      SCM_DEFER_INTS;
      entry = gethostbyname (SCM_CHARS (name));
    }
  else
    {
      inad.s_addr = htonl (scm_num2ulong (name, (char *) SCM_ARG1, s_sys_gethost));
      SCM_DEFER_INTS;
      entry = gethostbyaddr ((char *) &inad, sizeof (inad), AF_INET);
    }
  SCM_ALLOW_INTS;
  if (!entry)
    return SCM_BOOL_F;
  ve[0] = scm_makfromstr (entry->h_name, (scm_sizet) strlen (entry->h_name), 0);
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


SCM_PROC (s_sys_getnet, "%getnet", 0, 1, 0, scm_sys_getnet);
#ifdef __STDC__
SCM 
scm_sys_getnet (SCM name)
#else
SCM 
scm_sys_getnet (name)
     SCM name;
#endif
{
  SCM ans;
  SCM *ve;
  struct netent *entry;

  ans = scm_make_vector (SCM_MAKINUM (4), SCM_UNSPECIFIED, SCM_BOOL_F);
  ve = SCM_VELTS (ans);
  if (SCM_UNBNDP (name))
    {
      SCM_DEFER_INTS;
      entry = getnetent ();
    }
  else if (SCM_NIMP (name) && SCM_STRINGP (name))
    {
      SCM_DEFER_INTS;
      entry = getnetbyname (SCM_CHARS (name));
    }
  else
    {
      unsigned long netnum;
      netnum = scm_num2ulong (name, (char *) SCM_ARG1, s_sys_getnet);
      SCM_DEFER_INTS;
      entry = getnetbyaddr (netnum, AF_INET);
    }
  SCM_ALLOW_INTS;
  if (!entry)
    return SCM_BOOL_F;
  ve[0] = scm_makfromstr (entry->n_name, (scm_sizet) strlen (entry->n_name), 0);
  ve[1] = scm_makfromstrs (-1, entry->n_aliases);
  ve[2] = SCM_MAKINUM (entry->n_addrtype + 0L);
  ve[3] = scm_ulong2num (entry->n_net + 0L);
  return ans;
}

SCM_PROC (s_sys_getproto, "%getproto", 0, 1, 0, scm_sys_getproto);
#ifdef __STDC__
SCM 
scm_sys_getproto (SCM name)
#else
SCM 
scm_sys_getproto (name)
     SCM name;
#endif
{
  SCM ans;
  SCM *ve;
  struct protoent *entry;

  ans = scm_make_vector (SCM_MAKINUM (3), SCM_UNSPECIFIED, SCM_BOOL_F);
  ve = SCM_VELTS (ans);
  if (SCM_UNBNDP (name))
    {
      SCM_DEFER_INTS;
      entry = getprotoent ();
    }
  else if (SCM_NIMP (name) && SCM_STRINGP (name))
    {
      SCM_DEFER_INTS;
      entry = getprotobyname (SCM_CHARS (name));
    }
  else
    {
      unsigned long protonum;
      protonum = scm_num2ulong (name, (char *) SCM_ARG1, s_sys_getproto);
      SCM_DEFER_INTS;
      entry = getprotobynumber (protonum);
    }
  SCM_ALLOW_INTS;
  if (!entry)
    return SCM_BOOL_F;
  ve[0] = scm_makfromstr (entry->p_name, (scm_sizet) strlen (entry->p_name), 0);
  ve[1] = scm_makfromstrs (-1, entry->p_aliases);
  ve[2] = SCM_MAKINUM (entry->p_proto + 0L);
  return ans;
}

#ifdef __STDC__
static SCM
scm_return_entry (struct servent *entry)
#else
static SCM
scm_return_entry (entry)
     struct servent *entry;
#endif
{
  SCM ans;
  SCM *ve;

  ans = scm_make_vector (SCM_MAKINUM (4), SCM_UNSPECIFIED, SCM_BOOL_F);
  ve = SCM_VELTS (ans);
  if (!entry)
    {
      SCM_ALLOW_INTS;
      return SCM_BOOL_F;
    }
  ve[0] = scm_makfromstr (entry->s_name, (scm_sizet) strlen (entry->s_name), 0);
  ve[1] = scm_makfromstrs (-1, entry->s_aliases);
  ve[2] = SCM_MAKINUM (ntohs (entry->s_port) + 0L);
  ve[3] = scm_makfromstr (entry->s_proto, (scm_sizet) strlen (entry->s_proto), 0);
  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_sys_getserv, "%getserv", 0, 2, 0, scm_sys_getserv);
#ifdef __STDC__
SCM 
scm_sys_getserv (SCM name, SCM proto)
#else
SCM 
scm_sys_getserv (name, proto)
     SCM name;
     SCM proto;
#endif
{
  struct servent *entry;
  if (SCM_UNBNDP (name))
    {
      SCM_DEFER_INTS;
      entry = getservent ();
      return scm_return_entry (entry);
    }
  SCM_ASSERT (SCM_NIMP (proto) && SCM_STRINGP (proto), proto, SCM_ARG2, s_sys_getserv);
  if (SCM_NIMP (name) && SCM_STRINGP (name))
    {
      SCM_DEFER_INTS;
      entry = getservbyname (SCM_CHARS (name), SCM_CHARS (proto));
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (name), name, SCM_ARG1, s_sys_getserv);
      SCM_DEFER_INTS;
      entry = getservbyport (SCM_INUM (name), SCM_CHARS (proto));
    }
  return scm_return_entry (entry);
}

SCM_PROC (s_sethost, "sethost", 0, 1, 0, scm_sethost);
#ifdef __STDC__
SCM 
scm_sethost (SCM arg)
#else
SCM 
scm_sethost (arg)
     SCM arg;
#endif
{
  if (SCM_UNBNDP (arg))
    endhostent ();
  else
    sethostent (SCM_NFALSEP (arg));
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_setnet, "setnet", 0, 1, 0, scm_setnet);
#ifdef __STDC__
SCM 
scm_setnet (SCM arg)
#else
SCM 
scm_setnet (arg)
     SCM arg;
#endif
{
  if (SCM_UNBNDP (arg))
    endnetent ();
  else
    setnetent (SCM_NFALSEP (arg));
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_setproto, "setproto", 0, 1, 0, scm_setproto);
#ifdef __STDC__
SCM 
scm_setproto (SCM arg)
#else
SCM 
scm_setproto (arg)
     SCM arg;
#endif
{
  if (SCM_UNBNDP (arg))
    endprotoent ();
  else
    setprotoent (SCM_NFALSEP (arg));
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_setserv, "setserv", 0, 1, 0, scm_setserv);
#ifdef __STDC__
SCM 
scm_setserv (SCM arg)
#else
SCM 
scm_setserv (arg)
     SCM arg;
#endif
{
  if (SCM_UNBNDP (arg))
    endservent ();
  else
    setservent (SCM_NFALSEP (arg));
  return SCM_UNSPECIFIED;
}

#ifdef __STDC__
void 
scm_init_socket (void)
#else
void 
scm_init_socket ()
#endif
{
  scm_add_feature ("socket");
#include "socket.x"
}


