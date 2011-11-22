/* Copyright (C) 1996, 1997, 2000, 2001, 2006, 2008,
 *   2011, 2013 Free Software Foundation, Inc.
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

/* This is the 'main' function for the `guile' executable.  It is not
   included in libguile.a.

   Eventually, we hope this file will be automatically generated,
   based on the list of installed, statically linked libraries on the
   system.  For now, please don't put interesting code in here.  */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef __MINGW32__
# define SCM_IMPORT 1
#endif
#include <libguile.h>

#ifdef HAVE_CONFIG_H
#include <libguile/scmconfig.h>
#endif
#include <ltdl.h>
#include <locale.h>

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#endif

/* Debugger interface (don't change the order of the following lines) */
#define GDB_TYPE SCM
#include <libguile/gdb_interface.h>
GDB_INTERFACE;

static void
inner_main (void *closure SCM_UNUSED, int argc, char **argv)
{
#ifdef __MINGW32__
  /* This is necessary to startup the Winsock API under Win32. */
  WSADATA WSAData;
  WSAStartup (0x0202, &WSAData);
  GDB_INTERFACE_INIT;
#endif /* __MINGW32__ */

  /* module initializations would go here */
  scm_shell (argc, argv);

#ifdef __MINGW32__
  WSACleanup ();
#endif /* __MINGW32__ */
}

static int
get_integer_from_environment (const char *var, int def)
{
  char *end = 0;
  char *val = getenv (var);
  long res = def;
  if (!val)
    return def;
  res = strtol (val, &end, 10);
  if (end == val)
    {
      fprintf (stderr, "guile: warning: invalid %s: %s\n", var, val);
      return def;
    }
  return res;
}

static int
should_install_locale (void)
{
  /* If the GUILE_INSTALL_LOCALE environment variable is set to a
     nonzero value, we should install the locale via setlocale().  This
     behavior is off by default for compatibility with previous 2.0.x
     releases.  It will be on by default in 2.2.  */
  return get_integer_from_environment ("GUILE_INSTALL_LOCALE", 0);
}

int
main (int argc, char **argv)
{
  /* If we should install a locale, do it right at the beginning so that
     string conversion for command-line arguments, along with possible
     error messages, use the right locale.  See
     <https://lists.gnu.org/archive/html/guile-devel/2011-11/msg00041.html>
     for the rationale.  */
  if (should_install_locale () && setlocale (LC_ALL, "") == NULL)
    fprintf (stderr, "guile: warning: failed to install locale\n");

  scm_install_gmp_memory_functions = 1;
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
