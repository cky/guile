/* image-type.c
 * 
 *	Copyright (C) 1998 Free Software Foundation, Inc.
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

#include <stdlib.h>
#include <libguile.h>

static long image_tag;

struct image {
  int width, height;
  char *pixels;

  /* The name of this image */
  SCM name;

  /* A function to call when this image is
     modified, e.g., to update the screen,
     or SCM_BOOL_F if no action necessary */
  SCM update_func;
};

static SCM
make_image (SCM name, SCM s_width, SCM s_height)
{
  struct image *image;
  SCM image_smob;
  int width, height;

  SCM_ASSERT (SCM_NIMP (name) && SCM_STRINGP (name), name,
	      SCM_ARG1, "make-image");
  SCM_ASSERT (SCM_INUMP (s_width),  s_width,  SCM_ARG2, "make-image");
  SCM_ASSERT (SCM_INUMP (s_height), s_height, SCM_ARG3, "make-image");

  width = SCM_INUM (s_width);
  height = SCM_INUM (s_height);
  
  image = (struct image *) scm_must_malloc (sizeof (struct image), "image");
  image->width = width;
  image->height = height;
  image->pixels = scm_must_malloc (width * height, "image pixels");
  image->name = name;
  image->update_func = SCM_BOOL_F;

  SCM_NEWCELL (image_smob);
  SCM_SETCDR (image_smob, image);
  SCM_SETCAR (image_smob, image_tag);

  return image_smob;
}

static SCM
clear_image (SCM image_smob)
{
  int area;
  struct image *image;

  SCM_ASSERT ((SCM_NIMP (image_smob)
	       && SCM_CAR (image_smob) == image_tag),
	      image_smob, SCM_ARG1, "clear-image");

  image = (struct image *) SCM_CDR (image_smob);
  area = image->width * image->height;
  memset (image->pixels, 0, area);

  /* Invoke the image's update function.  */
  if (image->update_func != SCM_BOOL_F)
    scm_apply (image->update_func, SCM_EOL, SCM_EOL);

  return SCM_UNSPECIFIED;
}

static SCM
mark_image (SCM image_smob)
{
  struct image *image = (struct image *) SCM_CDR (image_smob);

  scm_gc_mark (image->name);
  return image->update_func;
}

static scm_sizet
free_image (SCM image_smob)
{
  struct image *image = (struct image *) SCM_CDR (image_smob);
  scm_sizet size = image->width * image->height + sizeof (struct image);

  free (image->pixels);
  free (image);

  return size;
}

static int
print_image (SCM image_smob, SCM port, scm_print_state *pstate)
{
  struct image *image = (struct image *) SCM_CDR (image_smob);

  scm_puts ("#<image ", port);
  scm_display (image->name, port);
  scm_puts (">", port);

  /* non-zero means success */
  return 1;
}

static scm_smobfuns image_funs = {
  mark_image, free_image, print_image, 0
};

void
init_image_type ()
{
  image_tag = scm_newsmob (&image_funs);

  scm_make_gsubr ("clear-image", 1, 0, 0, clear_image);
  scm_make_gsubr ("make-image", 3, 0, 0, make_image);
}
