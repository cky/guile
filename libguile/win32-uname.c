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

#include <windows.h>
#include <stdio.h>
#include <string.h>

#include "uname.h"

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
      else if (osver.dwMajorVersion >= 5)
        strcpy (uts->sysname, "Windows XP");   /* XP */
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
    default:
      strcpy (uts->machine, "unknown");
      break;
  }
  
  sLength = sizeof (uts->nodename) - 1;
  GetComputerName (uts->nodename, &sLength);
  return 0;
}
