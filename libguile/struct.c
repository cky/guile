/*	Copyright (C) 1996 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#include "genio.h"

#include "struct.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



static SCM required_vtable_fields = SCM_BOOL_F;
static int struct_num = 0;


SCM_PROC (s_struct_make_layout, "make-struct-layout", 1, 0, 0, scm_make_struct_layout);

SCM
scm_make_struct_layout (fields)
     SCM fields;
{
  SCM new_sym;
  SCM_ASSERT (SCM_NIMP (fields) && SCM_ROSTRINGP (fields),
	      fields, SCM_ARG1, s_struct_make_layout);

  {
    char * field_desc;
    int len;
    int x;

    len = SCM_ROLENGTH (fields);
    field_desc = SCM_ROCHARS (fields);
    SCM_ASSERT (!(len & 1), fields, "odd length field specification", s_struct_make_layout);

    for (x = 0; x < len; x += 2)
      {
	switch (field_desc[x])
	  {
	  case 'u':
	  case 'p':
#if 0
	  case 'i':
	  case 'd':
#endif
	  case 's':
	    break;
	  default:
	    SCM_ASSERT (0, SCM_MAKICHR (field_desc[x]) , "unrecognized field type", s_struct_make_layout);
	  }

	switch (field_desc[x + 1])
	  {
	  case 'w':
	    SCM_ASSERT (field_desc[x] != 's', SCM_MAKICHR (field_desc[x + 1]),
			"self fields not writable", s_struct_make_layout);
	      
	  case 'r':
	  case 'o':
	    break;
	  case 'R':
	  case 'W':
	  case 'O':
	    SCM_ASSERT (field_desc[x] != 's', SCM_MAKICHR (field_desc[x + 1]),
			"self fields not allowed in tail array",
			s_struct_make_layout);
	    SCM_ASSERT (x == len - 2, SCM_MAKICHR (field_desc[x + 1]),
			"tail array field must be last field in layout",
			s_struct_make_layout);
	    break;
	  default:
	    SCM_ASSERT (0, SCM_MAKICHR (field_desc[x]) , "unrecognized ref specification", s_struct_make_layout);
	  }
#if 0
	if (field_desc[x] == 'd')
	  {
	    SCM_ASSERT (field_desc[x + 2] == '-', SCM_MAKINUM (x / 2), "missing dash field", s_struct_make_layout);
	    x += 2;
	    goto recheck_ref;
	  }
#endif
      }
    new_sym = SCM_CAR (scm_intern_obarray (field_desc, len, SCM_BOOL_F));
  }
  return scm_return_first (new_sym, fields);
}





static void init_struct SCM_P ((SCM handle, int tail_elts, SCM inits));

static void
init_struct (handle, tail_elts, inits)
     SCM handle;
     int tail_elts;
     SCM inits;
{
  SCM layout;
  SCM * data;
  unsigned char * fields_desc;
  unsigned char prot = 0;
  int n_fields;
  SCM * mem;
  int tailp = 0;
  
  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  fields_desc = (unsigned char *) SCM_CHARS (layout) - 2;
  n_fields = SCM_LENGTH (layout) / 2;
  mem = SCM_STRUCT_DATA (handle);
  while (n_fields)
    {
      if (!tailp)
	{
	  fields_desc += 2;
	  prot = fields_desc[1];
	  if (SCM_LAYOUT_TAILP (prot))
	    {
	      tailp = 1;
	      prot = prot == 'R' ? 'r' : prot == 'W' ? 'w' : 'o';
	      *mem++ = tail_elts;
	      n_fields += tail_elts - 1;
	      if (n_fields == 0)
		break;
	    }
	}
      
      switch (*fields_desc)
	{
#if 0
	case 'i':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *mem = 0;
	  else
	    {
	      *mem = scm_num2long (SCM_CAR (inits), SCM_ARGn, "init_struct");
	      inits = SCM_CDR (inits);
	    }
	  break;
#endif

	case 'u':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *mem = 0;
	  else
	    {
	      *mem = scm_num2ulong (SCM_CAR (inits), SCM_ARGn, "init_struct");
	      inits = SCM_CDR (inits);
	    }
	  break;

	case 'p':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *mem = SCM_EOL;
	  else
	    {
	      *mem = SCM_CAR (inits);
	      inits = SCM_CDR (inits);
	    }
	      
	  break;

#if 0
	case 'd':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *((double *)mem) = 0.0;
	  else
	    {
	      *mem = scm_num2dbl (SCM_CAR (inits), "init_struct");
	      inits = SCM_CDR (inits);
	    }
	  fields_desc += 2;
	  break;
#endif

	case 's':
	  *mem = handle;
	  break;
	}

      n_fields--;
      mem++;
    }
}


SCM_PROC (s_struct_p, "struct?", 1, 0, 0, scm_struct_p);

SCM
scm_struct_p (x)
     SCM x;
{
  return ((SCM_NIMP (x) && SCM_STRUCTP (x))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC (s_struct_vtable_p, "struct-vtable?", 1, 0, 0, scm_struct_vtable_p);

SCM
scm_struct_vtable_p (x)
     SCM x;
{
  SCM layout;
  SCM * mem;

  if (SCM_IMP (x))
    return SCM_BOOL_F;

  if (!SCM_STRUCTP (x))
    return SCM_BOOL_F;

  layout = SCM_STRUCT_LAYOUT (x);

  if (SCM_LENGTH (layout) < SCM_LENGTH (required_vtable_fields))
    return SCM_BOOL_F;

  if (strncmp (SCM_CHARS (layout), SCM_CHARS (required_vtable_fields),
	       SCM_LENGTH (required_vtable_fields)))
    return SCM_BOOL_F;

  mem = SCM_STRUCT_DATA (x);

  if (mem[1] != 0)
    return SCM_BOOL_F;

  if (SCM_IMP (mem[0]))
    return SCM_BOOL_F;

  return (SCM_SYMBOLP (mem[0])
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/* All struct data must be allocated at an address whose bottom three
   bits are zero.  This is because the tag for a struct lives in the
   bottom three bits of the struct's car, and the upper bits point to
   the data of its vtable, which is a struct itself.  Thus, if the
   address of that data doesn't end in three zeros, tagging it will
   destroy the pointer.

   This function allocates a block of memory, and returns a pointer at
   least scm_struct_n_extra_words words into the block.  Furthermore,
   it guarantees that that pointer's least three significant bits are
   all zero.

   The argument n_words should be the number of words that should
   appear after the returned address.  (That is, it shouldn't include
   scm_struct_n_extra_words.)

   This function initializes the following fields of the struct:

     scm_struct_i_ptr --- the actual stort of the block of memory; the
	 address you should pass to 'free' to dispose of the block.
	 This field allows us to both guarantee that the returned
	 address is divisible by eight, and allow the GC to free the
	 block.

     scm_struct_i_n_words --- the number of words allocated to the
         block, including the extra fields.  This is used by the GC.

     scm_struct_i_tag --- a unique tag assigned to this struct,
         allocated according to struct_num.

     Ugh.  */


static SCM *alloc_struct SCM_P ((int n_words, char *who));

static SCM *
alloc_struct (n_words, who)
     int n_words;
     char *who;
{
  int size = sizeof (SCM) * (n_words + scm_struct_n_extra_words) + 7;
  SCM *block = (SCM *) scm_must_malloc (size, who);

  /* Adjust the pointer to hide the extra words.  */
  SCM *p = block + scm_struct_n_extra_words;

  /* Adjust it even further so it's aligned on an eight-byte boundary.  */
  p = (SCM *) (((SCM) p + 7) & ~7);

  /* Initialize a few fields as described above.  */
  p[scm_struct_i_ptr] = (SCM) block;
  p[scm_struct_i_n_words] = (SCM) (scm_struct_n_extra_words + n_words);
  p[scm_struct_i_tag] = struct_num++;

  return p;
}


SCM_PROC (s_make_struct, "make-struct", 2, 0, 1, scm_make_struct);

SCM
scm_make_struct (vtable, tail_array_size, init)
     SCM vtable;
     SCM tail_array_size;
     SCM init;
{
  SCM layout;
  int basic_size;
  int tail_elts;
  SCM * data;
  SCM handle;

  SCM_ASSERT ((SCM_BOOL_F != scm_struct_vtable_p (vtable)),
	      vtable, SCM_ARG1, s_make_struct);
  SCM_ASSERT (SCM_INUMP (tail_array_size), tail_array_size, SCM_ARG2,
	      s_make_struct);

  layout = SCM_STRUCT_DATA (vtable)[scm_struct_i_layout];
  basic_size = SCM_LENGTH (layout) / 2;
  tail_elts = SCM_INUM (tail_array_size);
  SCM_NEWCELL (handle);
  SCM_DEFER_INTS;
  data = alloc_struct (basic_size + tail_elts, "make-struct");
  SCM_SETCDR (handle, data);
  SCM_SETCAR (handle, ((SCM)SCM_STRUCT_DATA (vtable)) + scm_tc3_cons_gloc);
  init_struct (handle, tail_elts, init);
  SCM_ALLOW_INTS;
  return handle;
}



SCM_PROC (s_make_vtable_vtable, "make-vtable-vtable", 2, 0, 1, scm_make_vtable_vtable);

SCM
scm_make_vtable_vtable (extra_fields, tail_array_size, init)
     SCM extra_fields;
     SCM tail_array_size;
     SCM init;
{
  SCM fields;
  SCM layout;
  int basic_size;
  int tail_elts;
  SCM * data;
  SCM handle;

  SCM_ASSERT (SCM_NIMP (extra_fields) && SCM_ROSTRINGP (extra_fields),
	      extra_fields, SCM_ARG1, s_make_vtable_vtable);
  SCM_ASSERT (SCM_INUMP (tail_array_size), tail_array_size, SCM_ARG2,
	      s_make_vtable_vtable);

  fields = scm_string_append (scm_listify (required_vtable_fields,
					   extra_fields,
					   SCM_UNDEFINED));
  layout = scm_make_struct_layout (fields);
  basic_size = SCM_LENGTH (layout) / 2;
  tail_elts = SCM_INUM (tail_array_size);
  SCM_NEWCELL (handle);
  SCM_DEFER_INTS;
  data = alloc_struct (basic_size + tail_elts, "make-vtable-vtable");
  SCM_SETCDR (handle, data);
  SCM_SETCAR (handle, ((SCM)data) + scm_tc3_cons_gloc);
  SCM_STRUCT_LAYOUT (handle) = layout;
  init_struct (handle, tail_elts, scm_cons (layout, init));
  SCM_ALLOW_INTS;
  return handle;
}




SCM_PROC (s_struct_ref, "struct-ref", 2, 0, 0, scm_struct_ref);

SCM
scm_struct_ref (handle, pos)
     SCM handle;
     SCM pos;
{
  SCM answer = SCM_UNDEFINED;
  SCM * data;
  SCM layout;
  int p;
  int n_fields;
  unsigned char * fields_desc;
  unsigned char field_type;
  

  SCM_ASSERT (SCM_NIMP (handle) && SCM_STRUCTP (handle), handle,
	      SCM_ARG1, s_struct_ref);
  SCM_ASSERT (SCM_INUMP (pos), pos, SCM_ARG2, s_struct_ref);

  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  p = SCM_INUM (pos);

  fields_desc = (unsigned char *)SCM_CHARS (layout);
  n_fields = data[scm_struct_i_n_words] - scm_struct_n_extra_words;
  
  SCM_ASSERT (p < n_fields, pos, SCM_OUTOFRANGE, s_struct_ref);

  if (p * 2 < SCM_LENGTH (layout))
    {
      unsigned char ref;
      field_type = fields_desc[p * 2];
      ref = fields_desc[p * 2 + 1];
      if ((ref != 'r') && (ref != 'w'))
	{
	  if ((ref == 'R') || (ref == 'W'))
	    field_type = 'u';
	  else
	    SCM_ASSERT (0, pos, "ref denied", s_struct_ref);
	}
    }
  else if (fields_desc[SCM_LENGTH (layout) - 1] != 'O')    
    field_type = fields_desc[SCM_LENGTH (layout) - 2];
  else
    {
      SCM_ASSERT (0, pos, "ref denied", s_struct_ref);
      abort ();
    }
  
  switch (field_type)
    {
    case 'u':
      answer = scm_ulong2num (data[p]);
      break;

#if 0
    case 'i':
      answer = scm_long2num (data[p]);
      break;

    case 'd':
      answer = scm_makdbl (*((double *)&(data[p])), 0.0);
      break;
#endif

    case 's':
    case 'p':
      answer = data[p];
      break;


    default:
      SCM_ASSERT (0, SCM_MAKICHR (field_type), "unrecognized field type", s_struct_ref);
      break;
    }

  return answer;
}


SCM_PROC (s_struct_set_x, "struct-set!", 3, 0, 0, scm_struct_set_x);

SCM
scm_struct_set_x (handle, pos, val)
     SCM handle;
     SCM pos;
     SCM val;
{
  SCM * data;
  SCM layout;
  int p;
  int n_fields;
  unsigned char * fields_desc;
  unsigned char field_type;
  


  SCM_ASSERT (SCM_NIMP (handle) && SCM_STRUCTP (handle), handle,
	      SCM_ARG1, s_struct_ref);
  SCM_ASSERT (SCM_INUMP (pos), pos, SCM_ARG2, s_struct_ref);

  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  p = SCM_INUM (pos);

  fields_desc = (unsigned char *)SCM_CHARS (layout);
  n_fields = data[scm_struct_i_n_words] - scm_struct_n_extra_words;

  SCM_ASSERT (p < n_fields, pos, SCM_OUTOFRANGE, s_struct_set_x);

  if (p * 2 < SCM_LENGTH (layout))
    {
      unsigned char set_x;
      field_type = fields_desc[p * 2];
      set_x = fields_desc [p * 2 + 1];
      if (set_x != 'w')
	SCM_ASSERT (0, pos, "set_x denied", s_struct_set_x);
    }
  else if (fields_desc[SCM_LENGTH (layout) - 1] == 'W')    
    field_type = fields_desc[SCM_LENGTH (layout) - 2];
  else
    {
      SCM_ASSERT (0, pos, "set_x denied", s_struct_ref);
      abort ();
    }
  
  switch (field_type)
    {
    case 'u':
      data[p] = (SCM)scm_num2ulong (val, (char *)SCM_ARG3, s_struct_set_x);
      break;

#if 0
    case 'i':
      data[p] = scm_num2long (val, (char *)SCM_ARG3, s_struct_set_x);
      break;

    case 'd':
      *((double *)&(data[p])) = scm_num2dbl (val, (char *)SCM_ARG3);
      break;
#endif

    case 'p':
      data[p] = val;
      break;

    case 's':
      SCM_ASSERT (0, SCM_MAKICHR (field_type), "self fields immutable", s_struct_set_x);
      break;

    default:
      SCM_ASSERT (0, SCM_MAKICHR (field_type), "unrecognized field type", s_struct_set_x);
      break;
    }

  return val;
}


SCM_PROC (s_struct_vtable, "struct-vtable", 1, 0, 0, scm_struct_vtable);

SCM
scm_struct_vtable (handle)
     SCM handle;
{
  SCM_ASSERT (SCM_NIMP (handle) && SCM_STRUCTP (handle), handle,
	      SCM_ARG1, s_struct_vtable);
  return SCM_STRUCT_VTABLE (handle);
}


SCM_PROC (s_struct_vtable_tag, "struct-vtable-tag", 1, 0, 0, scm_struct_vtable_tag);

SCM
scm_struct_vtable_tag (handle)
     SCM handle;
{
  SCM_ASSERT (SCM_NIMP (handle) && (SCM_BOOL_F != scm_struct_vtable_p (handle)),
	      handle, SCM_ARG1, s_struct_vtable_tag);
  return scm_long2num (SCM_STRUCT_DATA (handle)[-1]);
}




void
scm_print_struct (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
#if 0 /* XXX - too verbose */
  SCM * data;
  SCM layout;
  int p;
  int n_fields;
  unsigned char * fields_desc;
  unsigned char field_type;
  
  layout = SCM_STRUCT_LAYOUT (exp);
  data = SCM_STRUCT_DATA (exp);

  fields_desc = (unsigned char *)SCM_CHARS (layout);
  n_fields = data[scm_struct_i_n_words] - scm_struct_n_extra_words;

  scm_gen_write (scm_regular_string, "#<struct ", sizeof ("#<struct ") - 1, port);
  for (p = 0; p < n_fields; p++)
    {
      if (fields_desc[2*p] == 'p')
	scm_iprin1 (data[p], port, pstate);
      if (p < n_fields-1)
	scm_gen_putc (' ', port);
    }
  scm_gen_putc ('>', port);
#else
  scm_gen_write (scm_regular_string, "#<struct ", sizeof ("#<struct ") - 1, port);
  scm_intprint (exp, 16, port);
  scm_gen_putc ('>', port);
#endif
}

void
scm_init_struct ()
{
  required_vtable_fields = SCM_CAR (scm_intern_obarray ("pruosr", sizeof ("pruosr") - 1, SCM_BOOL_F));
  scm_permanent_object (required_vtable_fields);
  scm_sysintern ("struct-vtable-offset", SCM_MAKINUM (scm_struct_i_vtable_offset));
#include "struct.x"
}

