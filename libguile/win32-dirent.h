/* classes: h_files */

#ifndef SCM_DIRENT_H
#define SCM_DIRENT_H

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

/* Directory stream type.
   The miscellaneous Unix `readdir' implementations read directory data
   into a buffer and return `struct dirent *' pointers into it.  */

#include <sys/types.h>

struct dirstream
{
  int fd;		/* File descriptor.  */
  char *data;		/* Directory block.  */
  size_t allocation;	/* Space allocated for the block.  */
  size_t size;		/* Total valid data in the block.  */
  size_t offset;	/* Current offset into the block.  */
  off_t filepos;	/* Position of next entry to read.  */
  char *mask;           /* Initial file mask. */
};

struct dirent
{
  long d_ino;
  off_t d_off;
  unsigned short int d_reclen;
  unsigned char d_type;
  char d_name[256];
};

#define d_fileno d_ino /* Backwards compatibility. */

/* This is the data type of directory stream objects.
   The actual structure is opaque to users.  */

typedef struct dirstream DIR;

DIR * opendir (const char * name);
struct dirent * readdir (DIR * dir);
int closedir (DIR * dir);
void rewinddir (DIR * dir);
void seekdir (DIR * dir, off_t offset);
off_t telldir (DIR * dir);
int dirfd (DIR * dir);

#endif /* SCM_DIRENT_H */
