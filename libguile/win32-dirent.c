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

#include "dirent.h"

DIR *
opendir (const char * name)
{
  DIR *dir;
  HANDLE hnd;
  char *file;
  WIN32_FIND_DATA find;

  if (!name || !*name) 
    return NULL;
  file = malloc (strlen (name) + 3);
  strcpy (file, name);
  if (file[strlen (name) - 1] != '/' && file[strlen (name) - 1] != '\\')
    strcat (file, "/*");
  else
    strcat (file, "*");
  
  if ((hnd = FindFirstFile (file, &find)) == INVALID_HANDLE_VALUE)
    {
      free (file);
      return NULL;
    }

  dir = malloc (sizeof (DIR));
  dir->mask = file;
  dir->fd = (int) hnd;
  dir->data = malloc (sizeof (WIN32_FIND_DATA));
  dir->allocation = sizeof (WIN32_FIND_DATA);
  dir->size = dir->allocation;
  dir->filepos = 0;
  memcpy (dir->data, &find, sizeof (WIN32_FIND_DATA));
  return dir;
}

struct dirent *
readdir (DIR * dir)
{
  static struct dirent entry;
  WIN32_FIND_DATA *find;

  entry.d_ino = 0;
  entry.d_type = 0;
  find = (WIN32_FIND_DATA *) dir->data;

  if (dir->filepos)
    {
      if (!FindNextFile ((HANDLE) dir->fd, find))
	return NULL;
    }

  entry.d_off = dir->filepos;
  strncpy (entry.d_name, find->cFileName, sizeof (entry.d_name));
  entry.d_reclen = strlen (find->cFileName);
  dir->filepos++;
  return &entry;
}

int 
closedir (DIR * dir)
{
  HANDLE hnd = (HANDLE) dir->fd;
  free (dir->data);
  free (dir->mask);
  free (dir);
  return FindClose (hnd) ? 0 : -1;
}

void 
rewinddir (DIR * dir)
{
  HANDLE hnd = (HANDLE) dir->fd;
  WIN32_FIND_DATA *find = (WIN32_FIND_DATA *) dir->data;

  FindClose (hnd);
  hnd = FindFirstFile (dir->mask, find);
  dir->fd = (int) hnd;
  dir->filepos = 0;
}

void 
seekdir (DIR * dir, off_t offset)
{
  off_t n;

  rewinddir (dir);
  for (n = 0; n < offset; n++)
    {
      if (FindNextFile ((HANDLE) dir->fd, (WIN32_FIND_DATA *) dir->data))
	dir->filepos++;
    }
}

off_t 
telldir (DIR * dir)
{
  return dir->filepos;
}

int 
dirfd (DIR * dir)
{
  return dir->fd;
}
