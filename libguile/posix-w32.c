/* Copyright (C) 2001, 2006, 2008, 2016 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/__scm.h"

# define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <c-strcase.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <io.h>
#include <fcntl.h>

#include "posix-w32.h"
#include "libguile/gc.h"        /* for scm_*alloc, scm_strdup */
#include "libguile/threads.h"   /* for scm_i_scm_pthread_mutex_lock */

/*
 * Get name and information about current kernel.
 */
int
uname (struct utsname *uts)
{
  enum { WinNT, Win95, Win98, WinUnknown };
  OSVERSIONINFO osver;
  SYSTEM_INFO sysinfo;
  DWORD sLength;
  DWORD os = WinUnknown;

  memset (uts, 0, sizeof (*uts));

  osver.dwOSVersionInfoSize = sizeof (osver);
  GetVersionEx (&osver);
  GetSystemInfo (&sysinfo);

  switch (osver.dwPlatformId)
    {
    case VER_PLATFORM_WIN32_NT: /* NT, Windows 2000 or Windows XP */
      if (osver.dwMajorVersion == 4)
        strcpy (uts->sysname, "Windows NT4x"); /* NT4x */
      else if (osver.dwMajorVersion <= 3)
        strcpy (uts->sysname, "Windows NT3x"); /* NT3x */
      else if (osver.dwMajorVersion == 5 && osver.dwMinorVersion < 1)
        strcpy (uts->sysname, "Windows 2000"); /* 2k */
      else if (osver.dwMajorVersion < 6)
        strcpy (uts->sysname, "Windows XP");   /* XP */
      else if (osver.dwMajorVersion == 6)
        {
          if (osver.dwMinorVersion < 1)
            strcpy (uts->sysname, "Windows Vista");   /* Vista */
          else if (osver.dwMinorVersion < 2)
            strcpy (uts->sysname, "Windows 7"); /* Windows 7 */
          else if (osver.dwMinorVersion < 3)
            strcpy (uts->sysname, "Windows 8"); /* Windows 8 */
          else if (osver.dwMinorVersion < 4)
            strcpy (uts->sysname, "Windows 8.1"); /* Windows 8.1 */
        }
      else if (osver.dwMajorVersion >= 10)
        strcpy (uts->sysname, "Windows 10 or later"); /* Windows 10 and later */
      os = WinNT;
      break;

    case VER_PLATFORM_WIN32_WINDOWS: /* Win95, Win98 or WinME */
      if ((osver.dwMajorVersion > 4) ||
          ((osver.dwMajorVersion == 4) && (osver.dwMinorVersion > 0)))
        {
          if (osver.dwMinorVersion >= 90)
            strcpy (uts->sysname, "Windows ME"); /* ME */
          else
            strcpy (uts->sysname, "Windows 98"); /* 98 */
          os = Win98;
        }
      else
        {
          strcpy (uts->sysname, "Windows 95"); /* 95 */
          os = Win95;
        }
      break;

    case VER_PLATFORM_WIN32s: /* Windows 3.x */
      strcpy (uts->sysname, "Windows");
      break;
    }

  sprintf (uts->version, "%ld.%02ld",
           osver.dwMajorVersion, osver.dwMinorVersion);

  if (osver.szCSDVersion[0] != '\0' &&
      (strlen (osver.szCSDVersion) + strlen (uts->version) + 1) <
      sizeof (uts->version))
    {
      strcat (uts->version, " ");
      strcat (uts->version, osver.szCSDVersion);
    }

  sprintf (uts->release, "build %ld", osver.dwBuildNumber & 0xFFFF);

  switch (sysinfo.wProcessorArchitecture)
    {
    case PROCESSOR_ARCHITECTURE_PPC:
      strcpy (uts->machine, "ppc");
      break;
    case PROCESSOR_ARCHITECTURE_ALPHA:
      strcpy (uts->machine, "alpha");
      break;
    case PROCESSOR_ARCHITECTURE_MIPS:
      strcpy (uts->machine, "mips");
      break;
    case PROCESSOR_ARCHITECTURE_IA64:
      strcpy (uts->machine, "ia64");
      break;
    case PROCESSOR_ARCHITECTURE_INTEL:
      /*
       * dwProcessorType is only valid in Win95 and Win98 and WinME
       * wProcessorLevel is only valid in WinNT
       */
      switch (os)
        {
        case Win95:
        case Win98:
          switch (sysinfo.dwProcessorType)
            {
            case PROCESSOR_INTEL_386:
            case PROCESSOR_INTEL_486:
            case PROCESSOR_INTEL_PENTIUM:
              sprintf (uts->machine, "i%ld", sysinfo.dwProcessorType);
              break;
            default:
              strcpy (uts->machine, "i386");
              break;
          }
          break;
        case WinNT:
          sprintf (uts->machine, "i%d86", sysinfo.wProcessorLevel);
          break;
        default:
          strcpy (uts->machine, "unknown");
          break;
        }
      break;
    case PROCESSOR_ARCHITECTURE_AMD64:
      strcpy (uts->machine, "x86_64");
      break;
    default:
      strcpy (uts->machine, "unknown");
      break;
  }

  sLength = sizeof (uts->nodename) - 1;
  GetComputerName (uts->nodename, &sLength);
  return 0;
}

/* Utility functions for maintaining the list of subprocesses launched
   by Guile.  */

struct proc_record {
  DWORD pid;
  HANDLE handle;
};

static struct proc_record *procs;
static ptrdiff_t proc_size;

/* Find the process slot that corresponds to PID.  Return the index of
   the slot, or -1 if not found.  */
static ptrdiff_t
find_proc (pid_t pid)
{
  ptrdiff_t found = -1, i;

  for (i = 0; i < proc_size; i++)
    {
      if (procs[i].pid == pid && procs[i].handle != INVALID_HANDLE_VALUE)
        found = i;
    }

  return found;
}

/* Return the process handle corresponding to its PID.  If not found,
   return invalid handle value.  */
static HANDLE
proc_handle (pid_t pid)
{
  ptrdiff_t idx = find_proc (pid);

  if (idx < 0)
    return INVALID_HANDLE_VALUE;
  return procs[idx].handle;
}

/* Store a process record in the procs[] array.  */
static void
record_proc (pid_t proc_pid, HANDLE proc_handle)
{
  ptrdiff_t i;

  /* Find a vacant slot.  */
  for (i = 0; i < proc_size; i++)
    {
      if (procs[i].handle == INVALID_HANDLE_VALUE)
        break;
    }

  /* If no vacant slot, enlarge the array.  */
  if (i == proc_size)
    {
      proc_size++;
      procs = scm_realloc (procs, proc_size * sizeof(procs[0]));
    }

  /* Store the process data.  */
  procs[i].pid = proc_pid;
  procs[i].handle = proc_handle;
}

/* Delete a process record for process PID.  */
static void
delete_proc (pid_t pid)
{
  ptrdiff_t idx = find_proc (pid);

  if (0 <= idx && idx < proc_size)
    procs[idx].handle = INVALID_HANDLE_VALUE;
}

/* Run a child process with redirected standard handles, without
   redirecting standard handles of the parent.  This is required in
   multithreaded programs, where redirecting a standard handle affects
   all threads.  */

/* Prepare a possibly redirected file handle to be passed to a child
   process.  The handle is for the file/device open on file descriptor
   FD; if FD is invalid, use the null device instead.

   USE_STD non-zero means we have been passed the descriptor used by
   the parent.

   ACCESS is the Windows access mode for opening the null device.

   Returns the Win32 handle to be passed to CreateProcess.  */
static HANDLE
prepare_child_handle (int fd, int use_std, DWORD access)
{
  HANDLE htem, hret;
  DWORD err = 0;

  /* Start with the descriptor, if specified by the caller and valid,
     otherwise open the null device.  */
  if (fd < 0)
    htem = INVALID_HANDLE_VALUE;
  else
    htem = (HANDLE)_get_osfhandle (fd);

  /* Duplicate the handle and make it inheritable.  */
  if (DuplicateHandle (GetCurrentProcess (),
                       htem,
                       GetCurrentProcess (),
                       &hret,
                       0,
                       TRUE,
                       DUPLICATE_SAME_ACCESS) == FALSE)
    {
      /* If the original standard handle was invalid (happens, e.g.,
         in GUI programs), open the null device instead.  */
      if ((err = GetLastError ()) == ERROR_INVALID_HANDLE
          && use_std)
        {
          htem = CreateFile ("NUL", access,
                             FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                             OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
          if (htem != INVALID_HANDLE_VALUE
              && DuplicateHandle (GetCurrentProcess (),
                                  htem,
                                  GetCurrentProcess (),
                                  &hret,
                                  0,
                                  TRUE,
                                  DUPLICATE_SAME_ACCESS) == FALSE)
            {
              err = GetLastError ();
              CloseHandle (htem);
              hret = INVALID_HANDLE_VALUE;
            }
        }
    }

  if (hret == INVALID_HANDLE_VALUE)
    {
      switch (err)
        {
          case ERROR_NO_MORE_FILES:
            errno = EMFILE;
            break;
          case ERROR_INVALID_HANDLE:
          default:
            errno = EBADF;
            break;
        }
    }

  return hret;
}

/* A comparison function for sorting the environment.  */
static int
compenv (const void *a1, const void *a2)
{
  return stricmp (*((char**)a1), *((char**)a2));
}

/* Convert the program's 'environ' array to a block of environment
   variables suitable to be passed to CreateProcess.  This is needed
   to ensure the child process inherits the up-to-date environment of
   the parent, including any variables inserted by the parent.  */
static void
prepare_envblk (char **envp, char **envblk)
{
  char **tmp;
  int size_needed;
  int envcnt;
  char *ptr;

  for (envcnt = 0; envp[envcnt]; envcnt++)
    ;

  tmp = scm_calloc ((envcnt + 1) * sizeof (*tmp));

  for (envcnt = size_needed = 0; envp[envcnt]; envcnt++)
    {
      tmp[envcnt] = envp[envcnt];
      size_needed += strlen (envp[envcnt]) + 1;
    }
  size_needed++;

  /* Windows likes its environment variables sorted.  */
  qsort ((void *) tmp, (size_t) envcnt, sizeof (char *), compenv);

  /* CreateProcess needs the environment block as a linear array,
     where each variable is terminated by a null character, and the
     last one is terminated by 2 null characters.  */
  ptr = *envblk = scm_calloc (size_needed);

  for (envcnt = 0; tmp[envcnt]; envcnt++)
    {
      strcpy (ptr, tmp[envcnt]);
      ptr += strlen (tmp[envcnt]) + 1;
    }

  free (tmp);
}

/* Find an executable PROGRAM on PATH, return result in malloc'ed
   storage.  If PROGRAM is /bin/sh, and no sh.exe was found on PATH,
   fall back on the Windows shell and set BIN_SH_REPLACED to non-zero.  */
static char *
lookup_cmd (const char *program, int *bin_sh_replaced)
{
  static const char *extensions[] = {
    ".exe", ".cmd", ".bat", "", ".com", NULL
  };
  int bin_sh_requested = 0;
  char *path, *dir, *sep;
  char abs_name[MAX_PATH];
  DWORD abs_namelen = 0;

  /* If they ask for the Unix system shell, try to find it on PATH.  */
  if (c_strcasecmp (program, "/bin/sh") == 0)
    {
      bin_sh_requested = 1;
      program = "sh.exe";
    }

  /* If PROGRAM includes leading directories, the caller already did
     our job.  */
  if (strchr (program, '/') != NULL
      || strchr (program, '\\') != NULL)
    return scm_strdup (program);

  /* Note: It is OK for getenv below to return NULL -- in that case,
     SearchPath will search in the directories whose list is specified
     by the system Registry.  */
  path = getenv ("PATH");
  if (!path)    /* shouldn't happen, really */
    path = ".";
  dir = sep = path = strdup (path);
  for ( ; sep && *sep; dir = sep + 1)
    {
      int i;

      sep = strpbrk (dir, ";");
      if (sep == dir)   /* two or more ;'s in a row */
        continue;
      if (sep)
        *sep = '\0';
      for (i = 0; extensions[i]; i++)
        {
          abs_namelen = SearchPath (dir, program, extensions[i],
                                    MAX_PATH, abs_name, NULL);
          if (0 < abs_namelen && abs_namelen <= MAX_PATH)       /* found! */
            break;
        }
      if (extensions[i])        /* found! */
        break;
      if (sep)
        *sep = ';';
    }

  free (path);

  /* If they asked for /bin/sh and we didn't find it, fall back on the
     default Windows shell.  */
  if (abs_namelen <= 0 && bin_sh_requested)
    {
      const char *shell = getenv ("ComSpec");

      if (!shell)
        shell = "C:\\Windows\\system32\\cmd.exe";

      *bin_sh_replaced = 1;
      strcpy (abs_name, shell);
      abs_namelen = strlen (abs_name);
    }

  /* If not found, return the original PROGRAM name.  */
  if (abs_namelen <= 0 || abs_namelen > MAX_PATH)
    return scm_strdup (program);

  return scm_strndup (abs_name, abs_namelen);
}

/* Concatenate command-line arguments in argv[] into a single
   command-line string, while quoting arguments as needed.  The result
   is malloc'ed.  */
static char *
prepare_cmdline (const char *cmd, const char * const *argv, int bin_sh_replaced)
{
  /* These characters should include anything that is special to _any_
     program, including both Windows and Unixy shells, and the
     widlcard expansion in startup code of a typical Windows app.  */
  const char need_quotes[] = " \t#;\"\'*?[]&|<>(){}$`^";
  size_t cmdlen = 1;    /* for terminating null */
  char *cmdline = scm_malloc (cmdlen);
  char *dst = cmdline;
  int cmd_exe_quoting = 0;
  int i;
  const char *p;

  /* Are we constructing a command line for cmd.exe?  */
  if (bin_sh_replaced)
    cmd_exe_quoting = 1;
  else
    {
      for (p = cmd + strlen (cmd);
           p > cmd && p[-1] != '/' && p[-1] != '\\' && p[-1] != ':';
           p--)
        ;
      if (c_strcasecmp (p, "cmd.exe") == 0
          || c_strcasecmp (p, "cmd") == 0)
        cmd_exe_quoting = 1;
    }

  /* Initialize the command line to empty.  */
  *dst = '\0';

  /* Append arguments, if any, from argv[]. */
  for (i = 0; argv[i]; i++)
    {
      const char *src = argv[i];
      size_t len;
      int quote_this = 0, n_backslashes = 0;
      int j;

      /* Append the blank separator.  We don't do that for argv[0]
         because that is the command name (will end up in child's
         argv[0]), and is only recognized as such if there're no
         blanks before it.  */
      if (i > 0)
        *dst++ = ' ';
      len = dst - cmdline;

      /* How much space is required for this argument?  */
      cmdlen += strlen (argv[i]) + 1; /* 1 for a blank separator */
      /* cmd.exe needs a different style of quoting: all the arguments
         beyond the /c switch are enclosed in an extra pair of quotes,
         and not otherwise quoted/escaped. */
      if (cmd_exe_quoting)
        {
          if (i == 2)
            cmdlen += 2;
        }
      else if (strpbrk (argv[i], need_quotes))
        {
          quote_this = 1;
          cmdlen += 2;
          for ( ; *src; src++)
            {
              /* An embedded quote needs to be escaped by a backslash.
                 Any backslashes immediately preceding that quote need
                 each one to be escaped by another backslash.  */
              if (*src == '\"')
                cmdlen += n_backslashes + 1;
              if (*src == '\\')
                n_backslashes++;
              else
                n_backslashes = 0;
            }
          /* If the closing quote we will add is preceded by
             backslashes, those backslashes need to be escaped.  */
          cmdlen += n_backslashes;
        }

      /* Enlarge the command-line string as needed.  */
      cmdline = scm_realloc (cmdline, cmdlen);
      dst = cmdline + len;

      if (i == 0
          && c_strcasecmp (argv[0], "/bin/sh") == 0
          && bin_sh_replaced)
        {
          strcpy (dst, "cmd.exe");
          dst += sizeof ("cmd.exe") - 1;
          continue;
        }
      if (i == 1 && bin_sh_replaced && strcmp (argv[1], "-c") == 0)
        {
          *dst++ = '/';
          *dst++ = 'c';
          *dst = '\0';
          continue;
        }

      /* Add this argument, possibly quoted, to the command line.  */
      if (quote_this || (i == 2 && cmd_exe_quoting))
        *dst++ = '\"';
      for (src = argv[i]; *src; src++)
        {
          if (quote_this)
            {
              if (*src == '\"')
                for (j = n_backslashes + 1; j > 0; j--)
                  *dst++ = '\\';
              if (*src == '\\')
                n_backslashes++;
              else
                n_backslashes = 0;
            }
          *dst++ = *src;
        }
      if (quote_this)
        {
          for (j = n_backslashes; j > 0; j--)
            *dst++ = '\\';
          *dst++ = '\"';
        }
      *dst = '\0';
    }

  if (cmd_exe_quoting && i > 2)
    {
      /* One extra slot was already reserved when we enlarged cmdlen
         by 2 in the "if (cmd_exe_quoting)" clause above.  So we can
         safely append a closing quote.  */
      *dst++ = '\"';
      *dst = '\0';
    }

  return cmdline;
}

/* Start a child process running the program in EXEC_FILE with its
   standard input and output optionally redirected to a pipe.  ARGV is
   the array of command-line arguments to pass to the child.  P2C and
   C2P are 2 pipes for communicating with the child, and ERRFD is the
   standard error file descriptor to be inherited by the child.
   READING and WRITING, if non-zero, mean that the corresponding pipe
   will be used.

   Return the PID of the child process, or -1 if couldn't start a
   process.  */
pid_t
start_child (const char *exec_file, char **argv,
             int reading, int c2p[2], int writing, int p2c[2],
             int infd, int outfd, int errfd)
{
  HANDLE hin = INVALID_HANDLE_VALUE, hout = INVALID_HANDLE_VALUE;
  HANDLE herr = INVALID_HANDLE_VALUE;
  STARTUPINFO si;
  char *env_block = NULL;
  char *cmdline = NULL;
  PROCESS_INFORMATION pi;
  char *progfile, *p;
  int errno_save;
  intptr_t pid;
  int bin_sh_replaced = 0;

  if (!reading)
    c2p[1] = outfd;
  if (!writing)
    p2c[0] = infd;

  /* Prepare standard handles to be passed to the child process.  */
  hin = prepare_child_handle (p2c[0], !writing, GENERIC_READ);
  if (hin == INVALID_HANDLE_VALUE)
    return -1;
  hout = prepare_child_handle (c2p[1], !reading, GENERIC_WRITE);
  if (hout == INVALID_HANDLE_VALUE)
    return -1;
  herr = prepare_child_handle (errfd, 1, GENERIC_WRITE);
  if (herr == INVALID_HANDLE_VALUE)
    return -1;

  /* Make sure the parent side of both pipes is not inherited.  This
     is required because gnulib's 'pipe' creates pipes whose both ends
     are inheritable, which is traditional on Posix (where pipe
     descriptors are implicitly duplicated by 'fork'), but wrong on
     Windows (where pipe handles need to be explicitly
     duplicated).  */
  if (writing)
    SetHandleInformation ((HANDLE)_get_osfhandle (p2c[1]),
                          HANDLE_FLAG_INHERIT, 0);
  if (reading)
    {
      SetHandleInformation ((HANDLE)_get_osfhandle (c2p[0]),
                            HANDLE_FLAG_INHERIT, 0);
      /* Gnulib's 'pipe' opens the pipe in binary mode, but we don't
         want to read text-mode input of subprocesses in binary more,
         because then we will get the ^M (a.k.a. "CR") characters we
         don't expect.  */
      _setmode (c2p[0], _O_TEXT);
    }

  /* Set up the startup info for the child, using the parent's as the
     starting point, and specify in it the redirected handles.  */
  GetStartupInfo (&si);
  si.dwFlags = STARTF_USESTDHANDLES;
  si.lpReserved = 0;
  si.cbReserved2 = 0;
  si.lpReserved2 = 0;
  si.hStdInput = hin;
  si.hStdOutput = hout;
  si.hStdError = herr;

  /* Create the environment block for the child.  This is needed
     because the environment we have in 'environ' is not in the format
     expected by CreateProcess.  */
  prepare_envblk (environ, &env_block);

  /* CreateProcess doesn't search PATH, so we must do that for it.  */
  progfile = lookup_cmd (exec_file, &bin_sh_replaced);

  /* CreateProcess doesn't like forward slashes in the application
     file name.  */
  for (p = progfile; *p; p++)
    if (*p == '/')
      *p = '\\';

  /* Construct the command line.  */
  cmdline = prepare_cmdline (exec_file, (const char * const *)argv,
                             bin_sh_replaced);

  /* All set and ready to fly.  Launch the child process.  */
  if (!CreateProcess (progfile, cmdline, NULL, NULL, TRUE, 0, env_block, NULL,
                      &si, &pi))
    {
      pid = -1;

      /* Since we use Win32 APIs directly, we need to translate their
         errors to errno values by hand.  */
      switch (GetLastError ())
        {
          case ERROR_FILE_NOT_FOUND:
          case ERROR_PATH_NOT_FOUND:
          case ERROR_INVALID_DRIVE:
          case ERROR_BAD_PATHNAME:
            errno = ENOENT;
            break;
          case ERROR_ACCESS_DENIED:
            errno = EACCES;
            break;
          case ERROR_BAD_ENVIRONMENT:
            errno = E2BIG;
            break;
          case ERROR_BROKEN_PIPE:
            errno = EPIPE;
            break;
          case ERROR_INVALID_HANDLE:
            errno = EBADF;
            break;
          case ERROR_MAX_THRDS_REACHED:
            errno = EAGAIN;
            break;
          case ERROR_BAD_EXE_FORMAT:
          case ERROR_BAD_FORMAT:
          default:
            errno = ENOEXEC;
            break;
        }
    }
  else
    {
      scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
      record_proc (pi.dwProcessId, pi.hProcess);
      scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
      pid = pi.dwProcessId;
    }

  errno_save = errno;

  /* Free resources.  */
  free (progfile);
  free (cmdline);
  free (env_block);
  CloseHandle (hin);
  CloseHandle (hout);
  CloseHandle (herr);
  CloseHandle (pi.hThread);

  /* Posix requires to call the shell if execvp fails to invoke EXEC_FILE.  */
  if (errno_save == ENOEXEC || errno_save == ENOENT)
    {
      const char *shell = getenv ("ComSpec");

      if (!shell)
        shell = "cmd.exe";

      if (c_strcasecmp (exec_file, shell) != 0)
        {
          argv[0] = (char *)exec_file;
          return start_child (shell, argv, reading, c2p, writing, p2c,
                              infd, outfd, errfd);
        }
    }

  errno = errno_save;
  return pid;
}


/* Emulation of waitpid which only supports WNOHANG, since _cwait doesn't.  */
int
waitpid (pid_t pid, int *status, int options)
{
  HANDLE ph;

  /* Not supported on MS-Windows.  */
  if (pid <= 0)
    {
      errno = ENOSYS;
      return -1;
    }

  scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
  ph = proc_handle (pid);
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
  /* Since scm_waitpid is documented to work only on child processes,
     being unable to find a process in our records means failure.  */
  if (ph == INVALID_HANDLE_VALUE)
    {
      errno = ECHILD;
      return -1;
    }

  if ((options & WNOHANG) != 0)
    {
      DWORD st;

      if (!GetExitCodeProcess (ph, &st))
        {
          errno = ECHILD;
          return -1;
        }
      if (st == STILL_ACTIVE)
        return 0;
      if (status)
        *status = st;
      CloseHandle (ph);
    }
  else
    _cwait (status, (intptr_t)ph, WAIT_CHILD);

  scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
  delete_proc (pid);
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  return pid;
}


/* Translate abnormal exit status of Windows programs into the signal
   that terminated the program.  This is required to support scm_kill
   and WTERMSIG.  */

struct signal_and_status {
  int sig;
  DWORD status;
};

static const struct signal_and_status sigtbl[] = {
  {SIGSEGV, 0xC0000005},        /* access to invalid address */
  {SIGSEGV, 0xC0000008},        /* invalid handle */
  {SIGILL,  0xC000001D},        /* illegal instruction */
  {SIGILL,  0xC0000025},        /* non-continuable instruction */
  {SIGSEGV, 0xC000008C},        /* array bounds exceeded */
  {SIGFPE,  0xC000008D},        /* float denormal */
  {SIGFPE,  0xC000008E},        /* float divide by zero */
  {SIGFPE,  0xC000008F},        /* float inexact */
  {SIGFPE,  0xC0000090},        /* float invalid operation */
  {SIGFPE,  0xC0000091},        /* float overflow */
  {SIGFPE,  0xC0000092},        /* float stack check */
  {SIGFPE,  0xC0000093},        /* float underflow */
  {SIGFPE,  0xC0000094},        /* integer divide by zero */
  {SIGFPE,  0xC0000095},        /* integer overflow */
  {SIGILL,  0xC0000096},        /* privileged instruction */
  {SIGSEGV, 0xC00000FD},        /* stack overflow */
  {SIGTERM, 0xC000013A},        /* Ctrl-C exit */
  {SIGINT,  0xC000013A}
};

static int
w32_signal_to_status (int sig)
{
  int i;

  for (i = 0; i < sizeof (sigtbl) / sizeof (sigtbl[0]); i++)
    if (sig == sigtbl[i].sig)
      return sigtbl[i].status;

  return (int)0xC000013A;
}

int
w32_status_to_termsig (DWORD status)
{
  int i;

  for (i = 0; i < sizeof (sigtbl) / sizeof (sigtbl[0]); i++)
    if (status == sigtbl[i].status)
      return sigtbl[i].sig;

  return SIGTERM;
}

/* Support for scm_kill.  */
int
kill (int pid, int sig)
{
  HANDLE ph;
  int child_proc = 0;

  if (pid == getpid ())
    {
      if (raise (sig) == 0)
        errno = ENOSYS;
      return -1;
    }

  scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
  ph = proc_handle (pid);
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
  /* If not found among our subprocesses, look elsewhere in the
     system.  */
  if (ph == INVALID_HANDLE_VALUE)
    ph = OpenProcess (PROCESS_TERMINATE, 0, pid);
  else
    child_proc = 1;
  if (!ph)
    {
      errno = EPERM;
      return -1;
    }
  if (!TerminateProcess (ph, w32_signal_to_status (sig)))
    {
      /* If it's our subprocess, it could have already exited.  In
         that case, waitpid will handily delete the process from our
         records, and we should return a more meaningful ESRCH to the
         caller.  */
      if (child_proc && waitpid (pid, NULL, WNOHANG) == pid)
        errno = ESRCH;
      else
        errno = EINVAL;
      return -1;
    }
  CloseHandle (ph);
  if (child_proc)
    {
      scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
      delete_proc (pid);
      scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
    }

  return 0;
}

/* Emulation of getpriority and setpriority.  */
#define NZERO        8

int
getpriority (int which, int who)
{
  HANDLE hp;
  int nice_value = -1;
  int error = 0;
  int child_proc = 0;

  /* We don't support process groups and users.  */
  if (which != PRIO_PROCESS)
    {
      errno = ENOSYS;
      return -1;
    }

  if (who == 0)
    hp = GetCurrentProcess ();
  else
    {
      scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
      hp = proc_handle (who);
      scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
      /* If not found among our subprocesses, look elsewhere in the
         system.  */
      if (hp == INVALID_HANDLE_VALUE)
        hp = OpenProcess (PROCESS_QUERY_INFORMATION, FALSE, who);
      else
        child_proc = 1;
    }

  if (hp)
    {
      DWORD pri_class = GetPriorityClass (hp);

      /* The pseudo-handle returned by GetCurrentProcess doesn't need
         to be closed.  */
      if (who > 0 && !child_proc)
        CloseHandle (hp);

      if (pri_class > 0)
        {
          switch (pri_class)
            {
            case IDLE_PRIORITY_CLASS:
              nice_value = 4;
              break;
            case BELOW_NORMAL_PRIORITY_CLASS:
              nice_value = 6;
              break;
            case NORMAL_PRIORITY_CLASS:
              nice_value = 8;
              break;
            case ABOVE_NORMAL_PRIORITY_CLASS:
              nice_value = 10;
              break;
            case HIGH_PRIORITY_CLASS:
              nice_value = 13;
              break;
            case REALTIME_PRIORITY_CLASS:
              nice_value = 24;
              break;
            }
          /* If WHO is us, we can provide a more fine-grained value by
             looking at the current thread's priority value.  (For
             other processes, it is not clear which thread to use.)  */
          if (who == 0 || who == GetCurrentProcessId ())
            {
              HANDLE ht = GetCurrentThread ();
              int tprio = GetThreadPriority (ht);

              switch (tprio)
                {
                case THREAD_PRIORITY_IDLE:
                  if (pri_class == REALTIME_PRIORITY_CLASS)
                    nice_value = 16;
                  else
                    nice_value = 1;
                  break;
                case THREAD_PRIORITY_TIME_CRITICAL:
                  if (pri_class == REALTIME_PRIORITY_CLASS)
                    nice_value = 31;
                  else
                    nice_value = 15;
                case THREAD_PRIORITY_ERROR_RETURN:
                  nice_value = -1;
                  error = 1;
                  break;
                default:
                  nice_value += tprio;
                  break;
                }
            }
          /* Map to "nice values" similar to what one would see on
             Posix platforms.  */
          if (!error)
            nice_value = - (nice_value - NZERO);
        }
      else
        error = 1;
    }
  else
    error = 1;

  if (error)
    {
      DWORD err = GetLastError ();

      switch (err)
        {
        case ERROR_INVALID_PARAMETER:
        case ERROR_INVALID_THREAD_ID:
          errno = ESRCH;
          break;
        default:
          errno = EPERM;
          break;
        }
    }

  return nice_value;
}

int
setpriority (int which, int who, int nice_val)
{
  HANDLE hp;
  DWORD err;
  int child_proc = 0, retval = -1;

  if (which != PRIO_PROCESS)
    {
      errno = ENOSYS;
      return -1;
    }

  if (who == 0)
    hp = GetCurrentProcess ();
  else
    {
      scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
      hp = proc_handle (who);
      scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
      /* If not found among our subprocesses, look elsewhere in the
         system.  */
      if (hp == INVALID_HANDLE_VALUE)
        hp = OpenProcess (PROCESS_SET_INFORMATION, FALSE, who);
      else
        child_proc = 1;
    }

  if (hp)
    {
      DWORD pri_class;

      /* Map "nice values" back to process priority classes.  */
      nice_val = -nice_val + NZERO;
      if (nice_val < 6)
        pri_class = IDLE_PRIORITY_CLASS;
      else if (nice_val < 8)
        pri_class = BELOW_NORMAL_PRIORITY_CLASS;
      else if (nice_val < 10)
        pri_class = NORMAL_PRIORITY_CLASS;
      else if (nice_val < 13)
        pri_class = ABOVE_NORMAL_PRIORITY_CLASS;
      else if (nice_val < 16)
        pri_class = HIGH_PRIORITY_CLASS;
      else
        pri_class = REALTIME_PRIORITY_CLASS;

      if (SetPriorityClass (hp, pri_class))
        retval = 0;
    }

  err = GetLastError ();

  switch (err)
    {
    case ERROR_INVALID_PARAMETER:
      errno = ESRCH;
      break;
    default:
      errno = EPERM;
      break;
    }
  /* The pseudo-handle returned by GetCurrentProcess doesn't
     need to be closed.  */
  if (hp && who > 0 && !child_proc)
    CloseHandle (hp);

  return retval;
}

/* Emulation of sched_getaffinity and sched_setaffinity.  */
int
sched_getaffinity (int pid, size_t mask_size, cpu_set_t *mask)
{
  HANDLE hp;
  DWORD err;
  int child_proc = 0;

  if (mask == NULL)
    {
      errno = EFAULT;
      return -1;
    }

  if (pid == 0)
    hp = GetCurrentProcess ();
  else
    {
      scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
      hp = proc_handle (pid);
      scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
      /* If not found among our subprocesses, look elsewhere in the
         system.  */
      if (hp == INVALID_HANDLE_VALUE)
        hp = OpenProcess (PROCESS_QUERY_INFORMATION, FALSE, pid);
      else
        child_proc = 1;
    }

  if (hp)
    {
      DWORD_PTR ignored;
      BOOL result = GetProcessAffinityMask (hp, (DWORD_PTR *)mask, &ignored);

      /* The pseudo-handle returned by GetCurrentProcess doesn't
         need to be closed.  */
      if (pid > 0 && !child_proc)
        CloseHandle (hp);
      if (result)
        return 0;
    }

  err = GetLastError ();

  switch (err)
    {
    case ERROR_INVALID_PARAMETER:
      errno = ESRCH;
      break;
    case ERROR_ACCESS_DENIED:
    default:
      errno = EPERM;
      break;
    }

  return -1;
}

int
sched_setaffinity (int pid, size_t mask_size, cpu_set_t *mask)
{
  HANDLE hp;
  DWORD err;
  int child_proc = 0;

  if (mask == NULL)
    {
      errno = EFAULT;
      return -1;
    }

  if (pid == 0)
    hp = GetCurrentProcess ();
  else
    {
      scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);
      hp = proc_handle (pid);
      scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);
      /* If not found among our subprocesses, look elsewhere in the
         system.  */
      if (hp == INVALID_HANDLE_VALUE)
        hp = OpenProcess (PROCESS_SET_INFORMATION, FALSE, pid);
      else
        child_proc = 1;
    }

  if (hp)
    {
      BOOL result = SetProcessAffinityMask (hp, *(DWORD_PTR *)mask);

      /* The pseudo-handle returned by GetCurrentProcess doesn't
         need to be closed.  */
      if (pid > 0 && !child_proc)
        CloseHandle (hp);
      if (result)
        return 0;
    }

  err = GetLastError ();

  switch (err)
    {
    case ERROR_INVALID_PARAMETER:
      errno = ESRCH;
      break;
    case ERROR_ACCESS_DENIED:
    default:
      errno = EPERM;
      break;
    }

  return -1;
}
