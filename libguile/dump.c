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

#define SCM_DUMP_COOKIE			"\x7fGBF-0.0"

#define SCM_DUMP_INITIAL_HASH_SIZE	511
#define SCM_DUMP_INITIAL_IMAGE_SIZE	4096

#define SCM_DUMP_INDEX_TO_WORD(x)	((scm_bits_t) ((x) << 3))
#define SCM_DUMP_WORD_TO_INDEX(x)	((long) ((x) >> 3))

struct scm_dump_header {
  scm_bits_t cookie;		/* cookie string */
  scm_bits_t version;		/* version string */
  scm_bits_t nmeta;		/* the number of meta data */
  scm_bits_t init;		/* initial object indicator */
};

struct scm_dump_meta {
  scm_bits_t tc;		/* the type of objects */
  scm_bits_t nobjs;		/* the number of objects */
};


/*
 * Dump state
 */

static scm_bits_t scm_tc16_dstate;

struct scm_dstate {
  int mmapped;
  scm_sizet image_size;
  int image_index;
  char *image_base;		/* memory image */
  SCM table;			/* object table */
};

#define SCM_DSTATE_DATA(d)	((struct scm_dstate *) SCM_SMOB_DATA (d))

#define SCM_DSTATE_TABLE(d)	   (SCM_DSTATE_DATA (d)->table)
#define SCM_DSTATE_TABLE_LENGTH(d) SCM_VECTOR_LENGTH (SCM_DSTATE_TABLE (d))
#define SCM_DSTATE_TABLE_BASE(d)   SCM_VELTS (SCM_DSTATE_TABLE (d))

static SCM
make_dstate ()
#define FUNC_NAME "make_dstate"
{
  struct scm_dstate *p = SCM_MUST_MALLOC (sizeof (struct scm_dstate));
  p->mmapped      = 0;
  p->image_size   = SCM_DUMP_INITIAL_IMAGE_SIZE;
  p->image_index  = 0;
  p->image_base   = SCM_MUST_MALLOC (p->image_size);
  p->table        = SCM_BOOL_F;
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
  p->table       = SCM_BOOL_F;
  SCM_RETURN_NEWSMOB (scm_tc16_dstate, p);
}
#undef FUNC_NAME

static SCM
dstate_mark (SCM obj)
{
  return SCM_DSTATE_TABLE (obj);
}

static scm_sizet
dstate_free (SCM obj)
#define FUNC_NAME "dstate_free"
{
  int size = sizeof (struct scm_dstate);
  struct scm_dstate *p = SCM_DSTATE_DATA (obj);
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
    return SCM_UNPACK (obj);
  else
    {
      int i;
      int len = SCM_DSTATE_TABLE_LENGTH (dstate);
      SCM *base = SCM_DSTATE_TABLE_BASE (dstate);
      for (i = 0; i < len; i++)
	if (SCM_EQ_P (obj, base[i]))
	  return SCM_DUMP_INDEX_TO_WORD (i);
    }
  scm_misc_error ("scm_object_indicator",
		  "Non-marked object: ~A", SCM_LIST1 (obj));
  return 0;
}

static SCM
scm_indicator_object (scm_bits_t word, SCM dstate)
{
  if (SCM_IMP (SCM_PACK (word)))
    return SCM_PACK (word);
  else
    return SCM_DSTATE_TABLE_BASE (dstate)[SCM_DUMP_WORD_TO_INDEX (word)];
}


/*
 * Dump interface
 */

static void
scm_store_pad (SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  while (p->image_index + sizeof (scm_bits_t) >= p->image_size)
    dstate_extend (p);
  while (p->image_index % sizeof (scm_bits_t) != 0)
    p->image_base[p->image_index++] = '\0';
}

static void
scm_store_chars (const char *addr, scm_sizet size, SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  while (p->image_index + size >= p->image_size)
    dstate_extend (p);
  memcpy (p->image_base + p->image_index, addr, size);
  memcpy (p->image_base + p->image_index + size, "\0", 1);
  p->image_index += size + 1;
}

void
scm_store_string (const char *addr, scm_sizet size, SCM dstate)
{
  scm_store_chars (addr, size, dstate);
  scm_store_pad (dstate);
}

void
scm_store_bytes (const char *addr, scm_sizet size, SCM dstate)
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
  scm_store_bytes ((const char *) &word, sizeof (scm_bits_t), dstate);
}

void
scm_store_object (SCM obj, SCM dstate)
{
  scm_store_word (scm_object_indicator (obj, dstate), dstate);
}

static void
scm_restore_pad (SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  while (p->image_index % sizeof (scm_bits_t) != 0)
    p->image_index++;
}

static const char *
scm_restore_chars (SCM dstate, int *lenp)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  const char *addr = p->image_base + p->image_index;
  *lenp = strlen (addr);
  p->image_index += *lenp + 1;
  return addr;
}

const char *
scm_restore_string (SCM dstate, int *lenp)
{
  const char *addr = scm_restore_chars (dstate, lenp);
  scm_restore_pad (dstate);
  return addr;
}

const char *
scm_restore_bytes (SCM dstate, scm_sizet size)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  const char *addr = p->image_base + p->image_index;
  p->image_index += size;
  scm_restore_pad (dstate);
  return addr;
}

scm_bits_t
scm_restore_word (SCM dstate)
{
  struct scm_dstate *p = SCM_DSTATE_DATA (dstate);
  scm_bits_t word = *(scm_bits_t *) (p->image_base + p->image_index);
  p->image_index += sizeof (scm_bits_t);
  return word;
}

SCM
scm_restore_object (SCM dstate)
{
  return scm_indicator_object (scm_restore_word (dstate), dstate);
}


/*
 * Dump routine
 */

void
scm_dump_mark (SCM obj, SCM dstate)
{
  SCM table = SCM_DSTATE_TABLE (dstate);

 loop:
  /* Nothing with immediates */
  if (SCM_IMP (obj))
    return;

  /* Return if already marked */
  if (!SCM_FALSEP (scm_hashq_ref (table, obj, SCM_BOOL_F)))
    return;

  if (SCM_SLOPPY_CONSP (obj))
    {
      scm_hashq_set_x (table, obj, SCM_MAKINUM (scm_tc3_cons));
      scm_dump_mark (SCM_CAR (obj), dstate);
      obj = SCM_CDR (obj);
      goto loop;
    }

  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_symbol:
      scm_hashq_set_x (table, obj, SCM_MAKINUM (scm_tc7_symbol));
      return;
    case scm_tc7_substring:
    case scm_tc7_string:
      scm_hashq_set_x (table, obj, SCM_MAKINUM (scm_tc7_string));
      return;
    case scm_tc7_vector:
      {
	int i;
	int len = SCM_VECTOR_LENGTH (obj);
	SCM *base = SCM_VELTS (obj);
	scm_hashq_set_x (table, obj, SCM_MAKINUM (scm_tc7_vector));
	for (i = 0; i < len; i++)
	  scm_dump_mark (base[i], dstate);
	return;
      }
    case scm_tc7_smob:
      {
	SCM (*mark) ()     = SCM_SMOB_DESCRIPTOR (obj).dump_mark;
	void (*dealloc) () = SCM_SMOB_DESCRIPTOR (obj).dump_dealloc;
	void (*store) ()   = SCM_SMOB_DESCRIPTOR (obj).dump_store;

	if (!(mark || dealloc || store))
	  break;

	scm_hashq_set_x (table, obj, SCM_MAKINUM (SCM_CELL_TYPE (obj)));
	if (mark)
	  {
	    obj = mark (obj, dstate);
	    goto loop;
	  }
	return;
      }
    }
  scm_misc_error ("scm_dump_mark", "Cannot dump: ~A", SCM_LIST1 (obj));
}

static void
scm_dump_dealloc (scm_bits_t tc, int nobjs, SCM *table, SCM dstate)
{
  switch (SCM_ITAG7 (SCM_PACK (tc)))
    {
    case scm_tc7_symbol:
      {
	int i;
	for (i = 0; i < nobjs; i++)
	  {
	    SCM obj = table[i];
	    scm_store_chars (SCM_SYMBOL_CHARS (obj),
			     SCM_SYMBOL_LENGTH (obj),
			     dstate);
	  }
	scm_store_pad (dstate);
	return;
      }
    case scm_tc7_string:
      {
	int i;
	for (i = 0; i < nobjs; i++)
	  {
	    SCM obj = table[i];
	    scm_store_chars (SCM_STRING_CHARS (obj),
			     SCM_STRING_LENGTH (obj),
			     dstate);
	  }
	scm_store_pad (dstate);
	return;
      }
    case scm_tc7_vector:
      {
	int i;
	for (i = 0; i < nobjs; i++)
	  scm_store_word (SCM_VECTOR_LENGTH (table[i]), dstate);
	return;
      }
    case scm_tc7_smob:
      {
	int i;
	void (*dealloc) () = scm_smobs[SCM_TC2SMOBNUM(tc)].dump_dealloc;
	if (dealloc)
	  for (i = 0; i < nobjs; i++)
	    dealloc (table[i], dstate);
	return;
      }
    }
}

static void
scm_dump_store (scm_bits_t tc, int nobjs, SCM *table, SCM dstate)
{
  if (SCM_ITAG3 (SCM_PACK (tc)) == scm_tc3_cons)
    {
      int i;
      for (i = 0; i < nobjs; i++)
	{
	  SCM obj = table[i];
	  scm_store_object (SCM_CAR (obj), dstate);
	  scm_store_object (SCM_CDR (obj), dstate);
	}
      return;
    }

  switch (SCM_ITAG7 (SCM_PACK (tc)))
    {
    case scm_tc7_vector:
      {
	int i, j;
	for (i = 0; i < nobjs; i++)
	  {
	    SCM obj = table[i];
	    int len = SCM_VECTOR_LENGTH (obj);
	    SCM *base = SCM_VELTS (obj);
	    for (j = 0; j < len; j++)
	      scm_store_object (base[j], dstate);
	  }
	return;
      }
    case scm_tc7_smob:
      {
	int i;
	void (*store) () = scm_smobs[SCM_TC2SMOBNUM(tc)].dump_store;
	if (store)
	  for (i = 0; i < nobjs; i++)
	    store (table[i], dstate);
	return;
      }
    }
}

static void
scm_undump_alloc (scm_bits_t tc, int nobjs, SCM *table, SCM dstate)
{
  if (SCM_ITAG3 (SCM_PACK (tc)) == scm_tc3_cons)
    {
      int i;
      for (i = 0; i < nobjs; i++)
	SCM_NEWCELL (table[i]);
      return;
    }

  switch (SCM_ITAG7 (SCM_PACK (tc)))
    {
    case scm_tc7_symbol:
      {
	int i;
	for (i = 0; i < nobjs; i++)
	  {
	    int len;
	    const char *mem = scm_restore_chars (dstate, &len);
	    table[i] = scm_mem2symbol (mem, len);
	  }
	scm_restore_pad (dstate);
	return;
      }
    case scm_tc7_string:
      {
	int i;
	for (i = 0; i < nobjs; i++)
	  {
	    int len;
	    const char *mem = scm_restore_chars (dstate, &len);
	    table[i] = scm_makfromstr (mem, len, 0);
	  }
	scm_restore_pad (dstate);
	return;
      }
    case scm_tc7_vector:
      {
	int i;
	for (i = 0; i < nobjs; i++)
	  {
	    int len = scm_restore_word (dstate);
	    table[i] = scm_c_make_vector (len, SCM_BOOL_F);
	  }
	return;
      }
    case scm_tc7_smob:
      {
	int i;
	SCM (*alloc) () = scm_smobs[SCM_TC2SMOBNUM(tc)].undump_alloc;
	if (!alloc)
	  break;
	for (i = 0; i < nobjs; i++)
	  table[i] = alloc (dstate);
	return;
      }
    }
  scm_misc_error ("scm_undump_alloc", "Cannot undump", SCM_EOL);
}

static void
scm_undump_restore (scm_bits_t tc, int nobjs, SCM *table, SCM dstate)
#define FUNC_NAME "scm_undump_restore"
{
  if (SCM_ITAG3 (SCM_PACK (tc)) == scm_tc3_cons)
    {
      int i;
      for (i = 0; i < nobjs; i++)
	{
	  SCM obj = table[i];
	  SCM_SETCAR (obj, scm_restore_object (dstate));
	  SCM_SETCDR (obj, scm_restore_object (dstate));
	}
      return;
    }

  switch (SCM_ITAG7 (SCM_PACK (tc)))
    {
    case scm_tc7_vector:
      {
	int i, j;
	for (i = 0; i < nobjs; i++)
	  {
	    SCM obj = table[i];
	    int len = SCM_VECTOR_LENGTH (obj);
	    SCM *base = SCM_VELTS (obj);
	    for (j = 0; j < len; j++)
	      base[j] = scm_restore_object (dstate);
	  }
	return;
      }
    case scm_tc7_smob:
      {
	int i;
	void (*restore) () = scm_smobs[SCM_TC2SMOBNUM(tc)].undump_restore;
	if (restore)
	  for (i = 0; i < nobjs; i++)
	    restore (table[i], dstate);
      }
    }
}
#undef FUNC_NAME

static void
scm_undump_init (scm_bits_t tc, int nobjs, SCM *table, SCM dstate)
{
  if (SCM_ITAG7 (SCM_PACK (tc)) == scm_tc7_smob)
    {
      int i;
      void (*init) () = scm_smobs[SCM_TC2SMOBNUM(tc)].undump_init;
      if (init)
	for (i = 0; i < nobjs; i++)
	  init (table[i]);
    }
}


/*
 * Scheme interface
 */

#define DUMP_APPLY(f,nmeta,meta,table)				\
{								\
  int i;							\
  int len = 0;							\
  for (i = 0; i < nmeta; i++)					\
    {								\
      f (meta[i].tc, meta[i].nobjs, table + len, dstate);	\
      len += meta[i].nobjs;					\
    }								\
}

static SCM
scm_dump_table_fold (void *proc, SCM key, SCM data, SCM value)
{
  SCM handle = scm_sloppy_assq (data, value);
  if (SCM_CONSP (handle))
    {
      SCM_SETCDR (handle, scm_cons (key, SCM_CDR (handle)));
      return value;
    }
  else
    return scm_acons (data, SCM_LIST1 (key), value);
}

SCM_DEFINE (scm_binary_write, "binary-write", 1, 1, 0, 
	    (SCM obj, SCM port),
	    "Write OBJ to PORT in a binary format.")
#define FUNC_NAME s_scm_binary_write
{
  int i, index, len, nmeta;
  struct scm_dump_header header;
  struct scm_dump_meta *meta;
  SCM dstate, alist, list, *base;

  /* Check port */
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_VALIDATE_OUTPUT_PORT (2, port);

  /* Mark objects */
  dstate = make_dstate ();
  SCM_DSTATE_TABLE (dstate) =
    scm_c_make_hash_table (SCM_DUMP_INITIAL_HASH_SIZE);
  scm_dump_mark (obj, dstate);

  /* Build meta information */
  alist = scm_internal_hash_fold (scm_dump_table_fold, 0, SCM_EOL,
				  SCM_DSTATE_TABLE (dstate));
  nmeta = scm_ilength (alist);
  meta  = alloca (nmeta * sizeof (struct scm_dump_meta));
  list  = alist;
  len   = 0;
  for (i = 0; i < nmeta; i++)
    {
      meta[i].tc    = SCM_INUM (SCM_CAAR (list));
      meta[i].nobjs = scm_ilength (SCM_CDAR (list));
      len += meta[i].nobjs;
      list = SCM_CDR (list);
    }

  /* Build object table */
  SCM_DSTATE_TABLE (dstate) = scm_c_make_vector (len, SCM_BOOL_F);
  base  = SCM_DSTATE_TABLE_BASE (dstate);
  index = 0;
  for (i = 0; i < nmeta; i++)
    {
      SCM list;
      for (list = SCM_CDAR (alist); !SCM_NULLP (list); list = SCM_CDR (list))
	base[index++] = SCM_CAR (list);
      alist = SCM_CDR (alist);
    }

  /* Dump */
  DUMP_APPLY (scm_dump_dealloc, nmeta, meta, base);
  DUMP_APPLY (scm_dump_store, nmeta, meta, base);

  /* Write header */
  header.cookie  = ((scm_bits_t *) SCM_DUMP_COOKIE)[0];
  header.version = ((scm_bits_t *) SCM_DUMP_COOKIE)[1];
  header.nmeta   = nmeta;
  header.init    = scm_object_indicator (obj, dstate);
  scm_lfwrite ((const char *) &header, sizeof (struct scm_dump_header), port);

  /* Write the rest */
  scm_lfwrite ((const char *) meta,
	       nmeta * sizeof (struct scm_dump_meta),
	       port);
  scm_lfwrite (SCM_DSTATE_DATA (dstate)->image_base,
	       SCM_DSTATE_DATA (dstate)->image_index,
	       port);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_binary_read, "binary-read", 0, 1, 0, 
	    (SCM port),
	    "Read an object from PORT in a binary format.")
#define FUNC_NAME s_scm_binary_read
{
  int i, len;
  scm_bits_t *data;
  struct scm_dump_header *header;
  struct scm_dump_meta *meta;
  SCM dstate, *base;

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

  /* Read header */
  header = (struct scm_dump_header *) SCM_DSTATE_DATA (dstate)->image_base;
  if (SCM_DSTATE_DATA (dstate)->image_size < sizeof (*header))
    SCM_MISC_ERROR ("Invalid binary format: ~A", SCM_LIST1 (port));
  if (header->cookie != ((scm_bits_t *) SCM_DUMP_COOKIE)[0])
    SCM_MISC_ERROR ("Invalid binary format: ~A", SCM_LIST1 (port));
  if (header->version != ((scm_bits_t *) SCM_DUMP_COOKIE)[1])
    SCM_MISC_ERROR ("Unsupported binary version: ~A", SCM_LIST1 (port));

  /* Read the rest */
  meta = (struct scm_dump_meta *) ((char *) header + sizeof (*header));
  data = (scm_bits_t *) (meta + header->nmeta);
  SCM_DSTATE_DATA (dstate)->image_index = (char *) data - (char *) header;

  /* Create object table */
  len = 0;
  for (i = 0; i < header->nmeta; i++)
    len += meta[i].nobjs;
  SCM_DSTATE_TABLE (dstate) = scm_c_make_vector (len, SCM_BOOL_F);
  base = SCM_DSTATE_TABLE_BASE (dstate);

  /* Undump */
  DUMP_APPLY (scm_undump_alloc, header->nmeta, meta, base);
  DUMP_APPLY (scm_undump_restore, header->nmeta, meta, base);
  DUMP_APPLY (scm_undump_init, header->nmeta, meta, base);

  /* Return */
  {
    SCM obj = scm_indicator_object (header->init, dstate);
    SCM_DSTATE_TABLE (dstate) = SCM_BOOL_F;
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
