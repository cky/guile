/* image-type.c
 * 
 *	Copyright (C) 1998, 2000 Free Software Foundation, Inc.
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
 */

#include <stdlib.h>
#include <libguile.h>

static scm_bits_t image_tag;

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
  int width, height;

  SCM_ASSERT (SCM_STRINGP (name), name, SCM_ARG1, "make-image");
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

  SCM_RETURN_NEWSMOB (image_tag, image);
}

static SCM
clear_image (SCM image_smob)
{
  int area;
  struct image *image;

  SCM_ASSERT (SCM_SMOB_PREDICATE (image_tag, image_smob),
              image_smob, SCM_ARG1, "clear-image");

  image = (struct image *) SCM_SMOB_DATA (image_smob);
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
  /* Mark the image's name and update function.  */
  struct image *image = (struct image *) SCM_SMOB_DATA (image_smob);

  scm_gc_mark (image->name);
  return image->update_func;
}

static size_t
free_image (SCM image_smob)
{
  struct image *image = (struct image *) SCM_SMOB_DATA (image_smob);
  size_t size = image->width * image->height + sizeof (struct image);

  free (image->pixels);
  free (image);

  return size;
}

static int
print_image (SCM image_smob, SCM port, scm_print_state *pstate)
{
  struct image *image = (struct image *) SCM_SMOB_DATA (image_smob);

  scm_puts ("#<image ", port);
  scm_display (image->name, port);
  scm_puts (">", port);

  /* non-zero means success */
  return 1;
}

void
init_image_type (void)
{
  image_tag = scm_make_smob_type ("image", sizeof (struct image));
  scm_set_smob_mark (image_tag, mark_image);
  scm_set_smob_free (image_tag, free_image);
  scm_set_smob_print (image_tag, print_image);

  scm_c_define_gsubr ("clear-image", 1, 0, 0, clear_image);
  scm_c_define_gsubr ("make-image", 3, 0, 0, make_image);
}
