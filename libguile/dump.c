/*	Copyright (C) 2001 Free Software Foundation, Inc.
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



#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "libguile/_scm.h"
#include "libguile/tags.h"
#include "libguile/root.h"
#include "libguile/alist.h"
#include "libguile/smob.h"
#include "libguile/ports.h"
#include "libguile/fports.h"
#include "libguile/strings.h"
#include "libguile/hashtab.h"
#include "libguile/vectors.h"
#include "libguile/validate.h"
#include "libguile/dump.h"

#define SCM_DUMP_COOKIE			"\x7fGBF-0.1"

#define SCM_DUMP_HASH_SIZE		151
#define SCM_DUMP_IMAGE_SIZE		4096

#define SCM_DUMP_INDEX_TO_WORD(x)	((scm_bits_t) ((x) << 3))
#define SCM_DUMP_WORD_TO_INDEX(x)	((long) ((x) >> 3))

struct scm_dump_header {
  scm_bits_t cookie;			/* cookie string */
  scm_bits_t version;			/* version string */
  scm_bits_t nobjs;			/* the number of objects */
					/* or immediate value */
};


/*
 * Dump state
 */

static scm_bits_t scm_tc16_dstate;

struct scm_dstate {
  int mmapped;
  scm_sizet image_size;
  int image_index;
  char *image_base;		/* Memory image */
  int table_index;
  SCM table;			/* Object table */
  SCM task;			/* Update task */
};

#define SCM_DSTATE_DATA(d)	    ((struct scm_dstate *) SCM_SMOB_DATA (d))
#define SCM_DSTATE_TABLE(d)	    (SCM_DSTATE_DATA (d)->table)
#define SCM_DSTATE_TABLE_REF(d,i)   (SCM_VELTS (SCM_DSTATE_TABLE (d))[i])
#define SCM_DSTATE_TABLE_SET(d,i,x) (SCM_VELTS (SCM_DSTATE_TABLE (d))[i] = (x))
#define SCM_DSTATE_TASK(d)	    (SCM_DSTATE_DATA (d)->task)

#define SCM_DTASK_ID(t)		    ((scm_bits_t) SCM_CELL_WORD_1 (t))
#define SCM_DTASK_ADDR(t)	    ((scm_bits_t *) SCM_CELL_WORD_2 (t))
#define SCM_DTASK_NEXT(t)	    (SCM_CELL_OBJECT_3 (t))
#define SCM_SET_DTASK_ID(t,x)	    SCM_SET_CELL_WORD_1 (t, x)
#define SCM_SET_DTASK_ADDR(t,x)	    SCM_SET_CELL_WORD_2 (t, x)
#define SCM_SET_DTASK_NEXT(t,x)	    SCM_SET_CELL_OBJECT_3 (t, x)

static SCM
make_dstate ()
#define FUNC_NAME "make_dstate"
{
  struct scm_dstate *p = SCM_MUST_MALLOC (sizeof (struct scm_dstate));
  p->mmapped     = 0;
  p->image_size  = SCM_DUMP_IMAGE_SIZE;
  p->image_index = 0;
  p->image_base  = SCM_MUST_MALLOC (p->image_size);
  p->table_index = 0;
  p->table       = SCM_BOOL_F;
  p->task        = SCM_EOL;
  SCM_RETURN_NEWSMOB (scm_tc16_dstate, p);
}
#undef FUNC_NAME

static SCM
make_dstate_by_mmap (int fd)
#define FUNC_NAME "make_dstate_by_mmap"
{
  int ret;
  char *addr;
  struct stat st;
  struct scm_dstate *p = SCM_MUST_MALLOC (sizeof (struct scm_dstate));

  SCM_SYSCALL (ret = fstat (fd, &st));
  if (ret < 0)
    SCM_SYSERROR;

  SCM_SYSCALL (addr = mmap (0, st.st_size, PROT_READ, MAP_SHARED, fd, 0));
  if (addr == MAP_FAILED)
    SCM_SYSERROR;

  p->mmapped     = 1;
  p->image_size  = st.st_size;
  p->image_index = 0;
  p->image_base  = addr;
  p->table_index = 0;
  p->table       = SCM_BOOL_F;
  p->task        = SCM_EOL;
  SCM_RETURN_NEWSMOB (scm_tc16_dstate, p);
}
#undef FUNC_NAME

static SCM
dstate_mark (SCM obj)
{
  SCM task;
  struct scm_dstate *p = SCM_DSTATE_DATA (obj);
  for (task = p->task; !SCM_NULLP (task); task = SCM_DTASK_NEXT (task))
    scm_gc_mark (task);
  return p->table;
}

static scm_sizet
dstate_free (SCM obj)
#define FUNC_NAME "dstate_free"
{
  int size = sizeof (struct scm_dstate);
  struct scm_dstate *p = SCM_DSTATE_DATA (obj);

  /* Free dump image */
  if (p->mmapped)
    {
      int rv;
      SCM_SYSCALL (rv = munmap (p->image_base, p->image_size));
      if (rv < 0)
	SCM_SYSERROR;
    }
  else
    {
      size += p->image_size;
      if (p->image_base)
	scm_must_free (p->image_base);
    }

  scm_must_free (p);
  return size;
}
#undef FUNC_NAME

static void
dstate_extend (struct scm_dstate *p)
{
  scm_sizet old_size = p->image_size;
  p->image_size *= 2;
  p->image_base = scm_must_realloc (p->image_base,
				    old_size,
				    p->image_size,
				    "dstate_extend");
}


/*
 * Object indicator
 */

static scm_bits_t
scm_object_indicator (SCM obj, SCM dstate)
{
  if (SCM_IMP (obj))
    {
      return SCM_UNPACK (obj);
    }
  else
    {
      SCM id = scm_hashq_ref (SCM_DSTATE_TABLE (dstate), obj, SCM_BOOL_F);
      if (SCM_FALSEP (id))
	return -1;
      else
	return SCM_DUMP_INDEX_TO_WORD (SCM_INUM (id));
    }
}

static SCM
scm_indicator_object (scm_bits_t word, SCM dstate)
{
  if (SCM_IMP (SCM_PACK (word)))
    return SCM_PACK (word);
  else
    return SCM_DSTATE_TABLE_REF (dstate, SCM_DUMP_WORD_TO_INDEX (word));
}


/*
 * Dump interface
 */

/* store functions */

static void
scm_store_pad (SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  while (p->image_index + sizeof (scm_bits_t) >= p->image_size)
    dstate_extend (p);
  while (p->image_index % sizeof (scm_bits_t) != 0)
    p->image_base[p->image_index++] = '\0';
}

void
scm_store_string (const char *addr, scm_sizet size, SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  while (p->image_index + size + 1 >= p->image_size)
    dstate_extend (p);
  memcpy (p->image_base + p->image_index, addr, size);
  memcpy (p->image_base + p->image_index + size, "\0", 1);
  p->image_index += size + 1;
  scm_store_pad (dstate);
}

void
scm_store_bytes (const void *addr, scm_sizet size, SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  while (p->image_index + size >= p->image_size)
    dstate_extend (p);
  memcpy (p->image_base + p->image_index, addr, size);
  p->image_index += size;
  scm_store_pad (dstate);
}

void
scm_store_word (const scm_bits_t word, SCM dstate)
{
  scm_store_bytes (&word, sizeof (scm_bits_t), dstate);
}

void
scm_store_object (SCM obj, SCM dstate)
{
  scm_bits_t id = scm_object_indicator (obj, dstate);
  if (id == -1)
    {
      /* OBJ is not stored yet.  Do it later */
      struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
      SCM task;
      SCM_NEWCELL2 (task);
      SCM_SET_DTASK_ID (task, SCM_UNPACK (obj));
      SCM_SET_DTASK_ADDR (task, p->image_index);
      SCM_SET_DTASK_NEXT (task, p->task);
      p->task = task;
    }
  scm_store_word (id, dstate);
}

/* restore functions */

static void
scm_restore_pad (struct scm_dstate *p)
{
  while (p->image_index % sizeof (scm_bits_t) != 0)
    p->image_index++;
}

void
scm_restore_string (const char **pp, scm_sizet *sizep, SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  *pp = p->image_base + p->image_index;
  *sizep = strlen (*pp);
  p->image_index += *sizep + 1;
  scm_restore_pad (p);
}

void
scm_restore_bytes (const void **pp, scm_sizet size, SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  *pp = p->image_base + p->image_index;
  p->image_index += size;
  scm_restore_pad (p);
}

void
scm_restore_word (scm_bits_t *wordp, SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  *wordp = *(scm_bits_t *) (p->image_base + p->image_index);
  p->image_index += sizeof (scm_bits_t);
}

void
scm_restore_object (SCM *objp, SCM dstate)
{
  scm_bits_t id;
  scm_restore_word (&id, dstate);
  *objp = scm_indicator_object (id, dstate);

  if (SCM_UNBNDP (*objp))
    {
      struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
      SCM task;
      SCM_NEWCELL2 (task);
      SCM_SET_DTASK_ID (task, id);
      SCM_SET_DTASK_ADDR (task, objp);
      SCM_SET_DTASK_NEXT (task, p->task);
      p->task = task;
    }
}


/*
 * Dump routine
 */

static void
scm_dump (SCM obj, SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);

  /* Check if immediate or already dumpped */
  if (scm_object_indicator (obj, dstate) != -1)
    return;

  /* Mark it */
  scm_hashq_set_x (p->table, obj, SCM_MAKINUM (p->table_index));
  p->table_index++;

  if (SCM_SLOPPY_CONSP (obj))
    {
      scm_store_word (scm_tc3_cons, dstate);
      /* Store cdr first in order to avoid a possible deep recursion
       * with a long list */
      scm_store_object (SCM_CDR (obj), dstate);
      scm_store_object (SCM_CAR (obj), dstate);
      goto next_dump;
    }
  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_symbol:
      {
	scm_store_word (scm_tc7_symbol, dstate);
	scm_store_string (SCM_SYMBOL_CHARS (obj),
			  SCM_SYMBOL_LENGTH (obj),
			  dstate);
	return;
      }
    case scm_tc7_substring:
    case scm_tc7_string:
      {
	scm_store_word (scm_tc7_string, dstate);
	scm_store_string (SCM_STRING_CHARS (obj),
			  SCM_STRING_LENGTH (obj),
			  dstate);
	return;
      }
    case scm_tc7_vector:
      {
	int i;
	int len = SCM_VECTOR_LENGTH (obj);
	SCM *base = SCM_VELTS (obj);
	scm_store_word (scm_tc7_vector, dstate);
	scm_store_word (len, dstate);
	for (i = 0; i < len; i++)
	  scm_store_object (base[i], dstate);
	goto next_dump;
      }
    case scm_tc7_smob:
      {
	void (*dump) () = SCM_SMOB_DESCRIPTOR (obj).dump;
	if (!dump)
	  goto error;

	/* FIXME: SCM_CELL_TYPE may change when undump!! */
	scm_store_word (SCM_CELL_TYPE (obj), dstate);
	dump (obj, dstate);
	goto next_dump;
      }
    default:
    error:
      scm_misc_error ("scm_dump_mark", "Cannot dump: ~A", SCM_LIST1 (obj));
    }

 next_dump:
  {
    SCM task;
    for (task = p->task; !SCM_NULLP (task); task = SCM_DTASK_NEXT (task))
      {
	SCM obj = SCM_PACK (SCM_DTASK_ID (task));
	scm_dump (obj, dstate);
	*(scm_bits_t *) (p->image_base + (int) SCM_DTASK_ADDR (task)) =
	  scm_object_indicator (obj, dstate);
      }
  }
}

static void
scm_undump (SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  scm_bits_t tc;
  SCM obj;

  scm_restore_word (&tc, dstate);

  if (SCM_ITAG3 (SCM_PACK (tc)) == scm_tc3_cons)
    {
      SCM_NEWCELL (obj);
      /* cdr was stored first */
      scm_restore_object ((SCM *) &SCM_CDR (obj), dstate);
      scm_restore_object ((SCM *) &SCM_CAR (obj), dstate);
      goto store_object;
    }

  switch (SCM_ITAG7 (SCM_PACK (tc)))
    {
    case scm_tc7_symbol:
      {
	int len;
	const char *mem;
	scm_restore_string (&mem, &len, dstate);
	obj = scm_mem2symbol (mem, len);
	goto store_object;
      }
    case scm_tc7_string:
      {
	int len;
	const char *mem;
	scm_restore_string (&mem, &len, dstate);
	obj = scm_makfromstr (mem, len, 0);
	goto store_object;
      }
    case scm_tc7_vector:
      {
	int i;
	scm_bits_t len;
	SCM *base;
	scm_restore_word (&len, dstate);
	obj = scm_c_make_vector (len, SCM_BOOL_F);
	base = SCM_VELTS (obj);
	for (i = 0; i < len; i++)
	  scm_restore_object (&base[i], dstate);
	goto store_object;
      }
    case scm_tc7_smob:
      {
	SCM (*undump) () = scm_smobs[SCM_TC2SMOBNUM (tc)].undump;
	if (!undump)
	  goto error;
	obj = undump (dstate);
	goto store_object;
      }
    default:
    error:
      scm_misc_error ("scm_undump", "Cannot undump", SCM_EOL);
    }

 store_object:
  SCM_DSTATE_TABLE_SET (dstate, p->table_index, obj);
  p->table_index++;
}


/*
 * Scheme interface
 */

SCM_DEFINE (scm_binary_write, "binary-write", 1, 1, 0, 
	    (SCM obj, SCM port),
	    "Write OBJ to PORT in a binary format.")
#define FUNC_NAME s_scm_binary_write
{
  struct scm_dstate *p;
  struct scm_dump_header header;
  SCM dstate;

  /* Check port */
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_VALIDATE_OUTPUT_PORT (2, port);

  /* Dump objects */
  dstate = make_dstate ();
  p = SCM_DSTATE_DATA (dstate);
  p->table = scm_c_make_hash_table (SCM_DUMP_HASH_SIZE);
  scm_dump (obj, dstate);

  /* Write image */
  header.cookie  = ((scm_bits_t *) SCM_DUMP_COOKIE)[0];
  header.version = ((scm_bits_t *) SCM_DUMP_COOKIE)[1];
  header.nobjs   = (p->table_index
		    ? SCM_DUMP_INDEX_TO_WORD (p->table_index)
		    : SCM_UNPACK (obj));
  scm_lfwrite ((const char *) &header, sizeof (struct scm_dump_header), port);
  if (p->image_index)
    scm_lfwrite (p->image_base, p->image_index, port);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_binary_read, "binary-read", 0, 1, 0, 
	    (SCM port),
	    "Read an object from PORT in a binary format.")
#define FUNC_NAME s_scm_binary_read
{
  int i, nobjs;
  struct scm_dstate *p;
  struct scm_dump_header *header;
  SCM dstate;

  /* Check port */
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_VALIDATE_INPUT_PORT (1, port);

  /* Initialize */
  if (SCM_FPORTP (port))
    /* Undump with mmap */
    dstate = make_dstate_by_mmap (SCM_FPORT_FDES (port));
  else
    /* Undump with malloc */
    SCM_MISC_ERROR ("Not supported yet", SCM_EOL);
  p = SCM_DSTATE_DATA (dstate);

  /* Read header */
  header = (struct scm_dump_header *) p->image_base;
  p->image_index += sizeof (struct scm_dump_header);
  if (p->image_size < sizeof (*header))
    SCM_MISC_ERROR ("Invalid binary format: ~A", SCM_LIST1 (port));
  if (header->cookie != ((scm_bits_t *) SCM_DUMP_COOKIE)[0])
    SCM_MISC_ERROR ("Invalid binary format: ~A", SCM_LIST1 (port));
  if (header->version != ((scm_bits_t *) SCM_DUMP_COOKIE)[1])
    SCM_MISC_ERROR ("Unsupported binary version: ~A", SCM_LIST1 (port));

  /* Check for immediate */
  if (SCM_IMP (SCM_PACK (header->nobjs)))
    return SCM_PACK (header->nobjs);

  /* Create object table */
  nobjs = SCM_DUMP_WORD_TO_INDEX (header->nobjs);
  p->table = scm_c_make_vector (nobjs, SCM_UNDEFINED);

  /* Undump */
  for (i = 0; i < nobjs; i++)
    scm_undump (dstate);

  /* Update references */
  {
    SCM task;
    for (task = p->task; !SCM_NULLP (task); task = SCM_DTASK_NEXT (task))
      {
	*SCM_DTASK_ADDR (task) =
	  SCM_UNPACK (scm_indicator_object (SCM_DTASK_ID (task), dstate));
      }
  }

  /* Return */
  {
    SCM obj = SCM_DSTATE_TABLE_REF (dstate, 0);
    p->table = SCM_BOOL_F;
    return obj;
  }
}
#undef FUNC_NAME


void
scm_init_dump ()
{
  scm_tc16_dstate = scm_make_smob_type ("dstate", 0);
  scm_set_smob_mark (scm_tc16_dstate, dstate_mark);
  scm_set_smob_free (scm_tc16_dstate, dstate_free);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/dump.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
