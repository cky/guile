/* classes: h_files */

#ifndef SCM_POSIX_W32_H
#define SCM_POSIX_W32_H

/* Copyright (C) 2001, 2006 Free Software Foundation, Inc.
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

#include <string.h>

#define _UTSNAME_LENGTH 65
#define _UTSNAME_NODENAME_LENGTH _UTSNAME_LENGTH
#define _UTSNAME_DOMAIN_LENGTH _UTSNAME_LENGTH

/* Structure describing the system and machine.  */
struct utsname
{
  /* Name of the implementation of the operating system.  */
  char sysname[_UTSNAME_LENGTH];

  /* Name of this node on the network.  */
  char nodename[_UTSNAME_NODENAME_LENGTH];

  /* Current release level of this implementation.  */
  char release[_UTSNAME_LENGTH];

  /* Current version level of this release.  */
  char version[_UTSNAME_LENGTH];

  /* Name of the hardware type the system is running on.  */
  char machine[_UTSNAME_LENGTH];

  /* Name of the domain of this node on the network.  */
  char domainname[_UTSNAME_DOMAIN_LENGTH];
};

#define WNOHANG               1

#define WEXITSTATUS(stat_val) ((stat_val) & 255)
/* MS-Windows programs that crash due to a fatal exception exit with
   an exit code whose 2 MSB bits are set.  */
#define WIFEXITED(stat_val)   (((stat_val) & 0xC0000000) == 0)
#define WIFSIGNALED(stat_val) (((stat_val) & 0xC0000000) == 0xC0000000)
#define WTERMSIG(stat_val)    w32_status_to_termsig (stat_val)
/* The funny conditional avoids a compiler warning in status:stop_sig.  */
#define WIFSTOPPED(stat_val)  ((stat_val) == (stat_val) ? 0 : 0)
#define WSTOPSIG(stat_var)    (0)

#define CPU_ZERO(s)     memset(s,0,sizeof(*s))
#define CPU_ISSET(b,s)  ((*s) & (1U << (b))) != 0
#define CPU_SET(b,s)    (*s) |= (1U << (b))
#define CPU_SETSIZE     (8*sizeof(DWORD_PTR))
typedef DWORD_PTR cpu_set_t;

#define PRIO_PROCESS 1
#define PRIO_PGRP    2
#define PRIO_USER    3

SCM_INTERNAL int uname (struct utsname * uts);
SCM_INTERNAL int waitpid (intptr_t, int *, int);
SCM_INTERNAL int w32_status_to_termsig (DWORD status);

SCM_INTERNAL int start_child (const char *exec_file, char **argv,
                              int reading, int c2p[2], int writing, int p2c[2],
                              int infd, int outfd, int errfd);

SCM_INTERNAL int kill (int pid, int sig);

SCM_INTERNAL int getpriority (int which, int who);
SCM_INTERNAL int setpriority (int which, int who, int nice_val);
SCM_INTERNAL int sched_getaffinity (int pid, size_t mask_size, cpu_set_t *mask);
SCM_INTERNAL int sched_setaffinity (int pid, size_t mask_size, cpu_set_t *mask);

#define HAVE_UNAME 1
#define HAVE_WAITPID 1
#define HAVE_START_CHILD 1
#define HAVE_KILL 1
#define HAVE_GETPRIORITY 1
#define HAVE_SETPRIORITY 1
#define HAVE_SCHED_GETAFFINITY 1
#define HAVE_SCHED_SETAFFINITY 1

#endif /* SCM_POSIX_W32_H */
