/* Copyright (C) 1996, 97, 98, 99, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/alist.h"
#include "libguile/weaks.h"
#include "libguile/hashtab.h"
#include "libguile/ports.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/struct.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



static SCM required_vtable_fields = SCM_BOOL_F;
SCM scm_struct_table;


SCM_DEFINE (scm_make_struct_layout, "make-struct-layout", 1, 0, 0, 
            (SCM fields),
	    "Return a new structure layout object.\n\n"
	    "@var{fields} must be a string made up of pairs of characters\n"
	    "strung together.  The first character of each pair describes a field\n"
	    "type, the second a field protection.  Allowed types are 'p' for\n"
	    "GC-protected Scheme data, 'u' for unprotected binary data, and 's' for\n"
	    "a field that points to the structure itself.    Allowed protections\n"
	    "are 'w' for mutable fields, 'r' for read-only fields, and 'o' for opaque \n"
	    "fields.  The last field protection specification may be capitalized to\n"
	    "indicate that the field is a tail-array.")
#define FUNC_NAME s_scm_make_struct_layout
{
  SCM new_sym;
  SCM_VALIDATE_STRING (1, fields);
  { /* scope */
    char * field_desc;
    int len;
    int x;

    len = SCM_STRING_LENGTH (fields);
    field_desc = SCM_STRING_CHARS (fields);
    SCM_ASSERT (!(len & 1), fields, "odd length field specification", FUNC_NAME);

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
	    SCM_ASSERT (0, SCM_MAKE_CHAR (field_desc[x]) , "unrecognized field type", FUNC_NAME);
	  }

	switch (field_desc[x + 1])
	  {
	  case 'w':
	    SCM_ASSERT (field_desc[x] != 's', SCM_MAKE_CHAR (field_desc[x + 1]),
			"self fields not writable", FUNC_NAME);
	      
	  case 'r':
	  case 'o':
	    break;
	  case 'R':
	  case 'W':
	  case 'O':
	    SCM_ASSERT (field_desc[x] != 's', SCM_MAKE_CHAR (field_desc[x + 1]),
			"self fields not allowed in tail array",
                        FUNC_NAME);
	    SCM_ASSERT (x == len - 2, SCM_MAKE_CHAR (field_desc[x + 1]),
			"tail array field must be last field in layout",
                        FUNC_NAME);
	    break;
	  default:
	    SCM_ASSERT (0, SCM_MAKE_CHAR (field_desc[x]) , "unrecognized ref specification", FUNC_NAME);
	  }
#if 0
	if (field_desc[x] == 'd')
	  {
	    SCM_ASSERT (field_desc[x + 2] == '-', SCM_MAKINUM (x / 2), "missing dash field", FUNC_NAME);
	    x += 2;
	    goto recheck_ref;
	  }
#endif
      }
    new_sym = SCM_CAR (scm_intern_obarray (field_desc, len, SCM_BOOL_F));
  }
  return scm_return_first (new_sym, fields);
}
#undef FUNC_NAME





static void
scm_struct_init (SCM handle, SCM layout, scm_bits_t * mem, int tail_elts, SCM inits)
{
  unsigned char * fields_desc = (unsigned char *) SCM_SYMBOL_CHARS (layout) - 2;
  unsigned char prot = 0;
  int n_fields = SCM_SYMBOL_LENGTH (layout) / 2;
  int tailp = 0;

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
	      *mem = scm_num2long (SCM_CAR (inits), SCM_ARGn, "scm_struct_init");
	      inits = SCM_CDR (inits);
	    }
	  break;
#endif

	case 'u':
	  if ((prot != 'r' && prot != 'w') || SCM_NULLP (inits))
	    *mem = 0;
	  else
	    {
	      *mem = scm_num2ulong (SCM_CAR (inits),
				    SCM_ARGn,
				    "scm_struct_init");
	      inits = SCM_CDR (inits);
	    }
	  break;

	case 'p':
	  if ((prot != 'r' && prot != 'w') || SCM_NULLP (inits))
	    *mem = SCM_UNPACK (SCM_BOOL_F);
	  else
	    {
	      *mem = SCM_UNPACK (SCM_CAR (inits));
	      inits = SCM_CDR (inits);
	    }
	      
	  break;

#if 0
	case 'd':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *((double *)mem) = 0.0;
	  else
	    {
	      *mem = scm_num2dbl (SCM_CAR (inits), "scm_struct_init");
	      inits = SCM_CDR (inits);
	    }
	  fields_desc += 2;
	  break;
#endif

	case 's':
	  *mem = SCM_UNPACK (handle);
	  break;
	}

      n_fields--;
      mem++;
    }
}


SCM_DEFINE (scm_struct_p, "struct?", 1, 0, 0, 
            (SCM x),
	    "Return #t iff @var{obj} is a structure object, else #f.")
#define FUNC_NAME s_scm_struct_p
{
  return SCM_BOOL(SCM_STRUCTP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_struct_vtable_p, "struct-vtable?", 1, 0, 0, 
            (SCM x),
	    "Return #t iff obj is a vtable structure.")
#define FUNC_NAME s_scm_struct_vtable_p
{
  SCM layout;
  scm_bits_t * mem;

  if (!SCM_STRUCTP (x))
    return SCM_BOOL_F;

  layout = SCM_STRUCT_LAYOUT (x);

  if (SCM_SYMBOL_LENGTH (layout) < SCM_STRING_LENGTH (required_vtable_fields))
    return SCM_BOOL_F;

  if (strncmp (SCM_SYMBOL_CHARS (layout), SCM_STRING_CHARS (required_vtable_fields),
	       SCM_STRING_LENGTH (required_vtable_fields)))
    return SCM_BOOL_F;

  mem = SCM_STRUCT_DATA (x);

  if (mem[1] != 0)
    return SCM_BOOL_F;

  return SCM_BOOL (SCM_SYMBOLP (SCM_PACK (mem[0])));
}
#undef FUNC_NAME


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

     scm_struct_i_ptr --- the actual start of the block of memory; the
	 address you should pass to 'free' to dispose of the block.
	 This field allows us to both guarantee that the returned
	 address is divisible by eight, and allow the GC to free the
	 block.

     scm_struct_i_n_words --- the number of words allocated to the
         block, including the extra fields.  This is used by the GC.

     Ugh.  */


scm_bits_t *
scm_alloc_struct (int n_words, int n_extra, char *who)
{
  int size = sizeof (scm_bits_t) * (n_words + n_extra) + 7;
  void * block = scm_must_malloc (size, who);

  /* Adjust the pointer to hide the extra words.  */
  scm_bits_t * p = (scm_bits_t *) block + n_extra;

  /* Adjust it even further so it's aligned on an eight-byte boundary.  */
  p = (scm_bits_t *) (((scm_bits_t) p + 7) & ~7);

  /* Initialize a few fields as described above.  */
  p[scm_struct_i_free] = (scm_bits_t) scm_struct_free_standard;
  p[scm_struct_i_ptr] = (scm_bits_t) block;
  p[scm_struct_i_n_words] = n_words;
  p[scm_struct_i_flags] = 0;

  return p;
}

scm_sizet
scm_struct_free_0 (scm_bits_t * vtable, scm_bits_t * data)
{
  return 0;
}

scm_sizet
scm_struct_free_light (scm_bits_t * vtable, scm_bits_t * data)
{
  scm_must_free (data);
  return vtable [scm_struct_i_size] & ~SCM_STRUCTF_MASK;
}

scm_sizet
scm_struct_free_standard (scm_bits_t * vtable, scm_bits_t * data)
{
  size_t n = (data[scm_struct_i_n_words] + scm_struct_n_extra_words)
	     * sizeof (scm_bits_t) + 7;
  scm_must_free ((void *) data[scm_struct_i_ptr]);
  return n;
}

scm_sizet
scm_struct_free_entity (scm_bits_t * vtable, scm_bits_t * data)
{
  size_t n = (data[scm_struct_i_n_words] + scm_struct_entity_n_extra_words)
	     * sizeof (scm_bits_t) + 7;
  scm_must_free ((void *) data[scm_struct_i_ptr]);
  return n;
}

static void *
scm_struct_gc_init (void *dummy1, void *dummy2, void *dummy3)
{
  scm_structs_to_free = SCM_EOL;
  return 0;
}

static void *
scm_free_structs (void *dummy1, void *dummy2, void *dummy3)
{
  SCM newchain = scm_structs_to_free;
  do
    {
      /* Mark vtables in GC chain.  GC mark set means delay freeing. */
      SCM chain = newchain;
      while (SCM_NNULLP (chain))
	{
	  SCM vtable = SCM_STRUCT_VTABLE (chain);
	  if (SCM_STRUCT_GC_CHAIN (vtable) != 0 && vtable != chain)
	    SCM_SETGCMARK (vtable);
	  chain = SCM_STRUCT_GC_CHAIN (chain);
	}
      /* Free unmarked structs.  */
      chain = newchain;
      newchain = SCM_EOL;
      while (SCM_NNULLP (chain))
	{
	  SCM obj = chain;
	  chain = SCM_STRUCT_GC_CHAIN (chain);
	  if (SCM_GCMARKP (obj))
	    {
	      SCM_CLRGCMARK (obj);
	      SCM_SET_STRUCT_GC_CHAIN (obj, newchain);
	      newchain = obj;
	    }
	  else
	    {
	      scm_bits_t word0 = SCM_CELL_WORD_0 (obj) - scm_tc3_cons_gloc;
	      /* access as struct */
	      scm_bits_t * vtable_data = (scm_bits_t *) word0;
	      scm_bits_t * data = (scm_bits_t *) SCM_UNPACK (SCM_CDR (obj));
	      scm_struct_free_t free_struct_data
		= ((scm_struct_free_t) vtable_data[scm_struct_i_free]);
	      SCM_SET_CELL_TYPE (obj, scm_tc_free_cell);
	      free_struct_data (vtable_data, data);
	    }
	}
    }
  while (SCM_NNULLP (newchain));
  return 0;
}

SCM_DEFINE (scm_make_struct, "make-struct", 2, 0, 1, 
            (SCM vtable, SCM tail_array_size, SCM init),
	    "Create a new structure.\n\n"
	    "@var{type} must be a vtable structure (@pxref{Vtables}).\n\n"
	    "@var{tail-elts} must be a non-negative integer.  If the layout\n"
	    "specification indicated by @var{type} includes a tail-array,\n"
	    "this is the number of elements allocated to that array.\n\n"
	    "The @var{init1}, @dots{} are optional arguments describing how\n"
	    "successive fields of the structure should be initialized.  Only fields\n"
	    "with protection 'r' or 'w' can be initialized, except for fields of\n"
	    "type 's', which are automatically initialized to point to the new\n"
	    "structure itself; fields with protection 'o' can not be initialized by\n"
	    "Scheme programs.\n\n"
	    "If fewer optional arguments than initializable fields are supplied,\n"
	    "fields of type 'p' get default value #f while fields of type 'u' are\n"
	    "initialized to 0.\n\n"
	    "Structs are currently the basic representation for record-like data\n"
	    "structures in Guile.  The plan is to eventually replace them with a\n"
	    "new representation which will at the same time be easier to use and\n"
	    "more powerful.\n\n"
	    "For more information, see the documentation for @code{make-vtable-vtable}.")
#define FUNC_NAME s_scm_make_struct
{
  SCM layout;
  int basic_size;
  int tail_elts;
  scm_bits_t * data;
  SCM handle;

  SCM_VALIDATE_VTABLE (1,vtable);
  SCM_VALIDATE_INUM (2,tail_array_size);
  SCM_VALIDATE_REST_ARGUMENT (init);

  layout = SCM_PACK (SCM_STRUCT_DATA (vtable) [scm_vtable_index_layout]);
  basic_size = SCM_SYMBOL_LENGTH (layout) / 2;
  tail_elts = SCM_INUM (tail_array_size);
  SCM_NEWCELL2 (handle);
  SCM_DEFER_INTS;
  if (SCM_STRUCT_DATA (vtable)[scm_struct_i_flags] & SCM_STRUCTF_ENTITY)
    {
      data = scm_alloc_struct (basic_size + tail_elts,
			       scm_struct_entity_n_extra_words,
			       "make-struct");
      data[scm_struct_i_procedure] = SCM_UNPACK (SCM_BOOL_F);
      data[scm_struct_i_setter] = SCM_UNPACK (SCM_BOOL_F);
    }
  else
    data = scm_alloc_struct (basic_size + tail_elts,
			     scm_struct_n_extra_words,
			     "make-struct");
  SCM_SET_CELL_WORD_1 (handle, data);
  SCM_SET_STRUCT_GC_CHAIN (handle, 0);
  scm_struct_init (handle, layout, data, tail_elts, init);
  SCM_SET_CELL_WORD_0 (handle, (scm_bits_t) SCM_STRUCT_DATA (vtable) + scm_tc3_cons_gloc);
  SCM_ALLOW_INTS;
  return handle;
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_vtable_vtable, "make-vtable-vtable", 2, 0, 1,
            (SCM user_fields, SCM tail_array_size, SCM init),
	    "Return a new, self-describing vtable structure.\n\n"
	    "@var{user-fields} is a string describing user defined fields of the\n"
	    "vtable beginning at index @code{vtable-offset-user}\n"
	    "(see @code{make-struct-layout}).\n\n"
	    "@var{tail-size} specifies the size of the tail-array (if any) of\n"
	    "this vtable.\n\n"
	    "@var{init1}, @dots{} are the optional initializers for the fields of\n"
	    "the vtable.\n\n"
	    "Vtables have one initializable system field---the struct printer.\n"
	    "This field comes before the user fields in the initializers passed\n"
	    "to @code{make-vtable-vtable} and @code{make-struct}, and thus works as\n"
	    "a third optional argument to @code{make-vtable-vtable} and a fourth to\n"
	    "@code{make-struct} when creating vtables:\n\n"
	    "If the value is a procedure, it will be called instead of the standard\n"
	    "printer whenever a struct described by this vtable is printed.\n"
	    "The procedure will be called with arguments STRUCT and PORT.\n\n"
	    "The structure of a struct is described by a vtable, so the vtable is\n"
	    "in essence the type of the struct.  The vtable is itself a struct with\n"
	    "a vtable.  This could go on forever if it weren't for the\n"
	    "vtable-vtables which are self-describing vtables, and thus terminate\n"
	    "the chain.\n\n"
	    "There are several potential ways of using structs, but the standard\n"
	    "one is to use three kinds of structs, together building up a type\n"
	    "sub-system: one vtable-vtable working as the root and one or several\n"
	    "\"types\", each with a set of \"instances\".  (The vtable-vtable should be\n"
	    "compared to the class <class> which is the class of itself.)\n\n"
	    "@example\n"
	    "(define ball-root (make-vtable-vtable \"pr\" 0))\n\n"
	    "(define (make-ball-type ball-color)\n"
	    "  (make-struct ball-root 0\n"
	    "	       (make-struct-layout \"pw\")\n"
	    "               (lambda (ball port)\n"
	    "                 (format port \"#<a ~A ball owned by ~A>\"\n"
	    "                         (color ball)\n"
	    "                         (owner ball)))\n"
	    "               ball-color))\n"
	    "(define (color ball) (struct-ref (struct-vtable ball) vtable-offset-user))\n"
	    "(define (owner ball) (struct-ref ball 0))\n\n"
	    "(define red (make-ball-type 'red))\n"
	    "(define green (make-ball-type 'green))\n\n"
	    "(define (make-ball type owner) (make-struct type 0 owner))\n\n"
	    "(define ball (make-ball green 'Nisse))\n"
	    "ball @result{} #<a green ball owned by Nisse>\n"
	    "@end example\n"
	    "")
#define FUNC_NAME s_scm_make_vtable_vtable
{
  SCM fields;
  SCM layout;
  int basic_size;
  int tail_elts;
  scm_bits_t * data;
  SCM handle;

  SCM_VALIDATE_STRING (1, user_fields);
  SCM_VALIDATE_INUM (2, tail_array_size);
  SCM_VALIDATE_REST_ARGUMENT (init);

  fields = scm_string_append (scm_listify (required_vtable_fields,
					   user_fields,
					   SCM_UNDEFINED));
  layout = scm_make_struct_layout (fields);
  basic_size = SCM_SYMBOL_LENGTH (layout) / 2;
  tail_elts = SCM_INUM (tail_array_size);
  SCM_NEWCELL2 (handle);
  SCM_DEFER_INTS;
  data = scm_alloc_struct (basic_size + tail_elts,
			   scm_struct_n_extra_words,
			   "make-vtable-vtable");
  SCM_SET_CELL_WORD_1 (handle, data);
  SCM_SET_STRUCT_GC_CHAIN (handle, 0);
  data [scm_vtable_index_layout] = SCM_UNPACK (layout);
  scm_struct_init (handle, layout, data, tail_elts, scm_cons (layout, init));
  SCM_SET_CELL_WORD_0 (handle, (scm_bits_t) data + scm_tc3_cons_gloc);
  SCM_ALLOW_INTS;
  return handle;
}
#undef FUNC_NAME




SCM_DEFINE (scm_struct_ref, "struct-ref", 2, 0, 0,
            (SCM handle, SCM pos),
	    "@deffnx primitive struct-set! struct n value\n"
	    "Access (or modify) the @var{n}th field of @var{struct}.\n\n"
	    "If the field is of type 'p', then it can be set to an arbitrary value.\n\n"
	    "If the field is of type 'u', then it can only be set to a non-negative\n"
	    "integer value small enough to fit in one machine word.")
#define FUNC_NAME s_scm_struct_ref
{
  SCM answer = SCM_UNDEFINED;
  scm_bits_t * data;
  SCM layout;
  int p;
  scm_bits_t n_fields;
  unsigned char * fields_desc;
  unsigned char field_type = 0;
  

  SCM_VALIDATE_STRUCT (1,handle);
  SCM_VALIDATE_INUM (2,pos);

  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  p = SCM_INUM (pos);

  fields_desc = SCM_SYMBOL_UCHARS (layout);
  n_fields = data[scm_struct_i_n_words];
  
  SCM_ASSERT_RANGE(1,pos, p < n_fields);

  if (p * 2 < SCM_SYMBOL_LENGTH (layout))
    {
      unsigned char ref;
      field_type = fields_desc[p * 2];
      ref = fields_desc[p * 2 + 1];
      if ((ref != 'r') && (ref != 'w'))
	{
	  if ((ref == 'R') || (ref == 'W'))
	    field_type = 'u';
	  else
	    SCM_ASSERT (0, pos, "ref denied", FUNC_NAME);
	}
    }
  else if (fields_desc[SCM_SYMBOL_LENGTH (layout) - 1] != 'O')    
    field_type = fields_desc[SCM_SYMBOL_LENGTH (layout) - 2];
  else
    {
      SCM_ASSERT (0, pos, "ref denied", FUNC_NAME);
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
      answer = scm_make_real (*((double *)&(data[p])));
      break;
#endif

    case 's':
    case 'p':
      answer = SCM_PACK (data[p]);
      break;


    default:
      SCM_ASSERT (0, SCM_MAKE_CHAR (field_type), "unrecognized field type", FUNC_NAME);
      break;
    }

  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_set_x, "struct-set!", 3, 0, 0,
            (SCM handle, SCM pos, SCM val),
	    "")
#define FUNC_NAME s_scm_struct_set_x
{
  scm_bits_t * data;
  SCM layout;
  int p;
  int n_fields;
  unsigned char * fields_desc;
  unsigned char field_type = 0;

  SCM_VALIDATE_STRUCT (1,handle);
  SCM_VALIDATE_INUM (2,pos);

  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  p = SCM_INUM (pos);

  fields_desc = SCM_SYMBOL_UCHARS (layout);
  n_fields = data[scm_struct_i_n_words];

  SCM_ASSERT_RANGE (1,pos, p < n_fields);

  if (p * 2 < SCM_SYMBOL_LENGTH (layout))
    {
      unsigned char set_x;
      field_type = fields_desc[p * 2];
      set_x = fields_desc [p * 2 + 1];
      if (set_x != 'w')
	SCM_ASSERT (0, pos, "set_x denied", FUNC_NAME);
    }
  else if (fields_desc[SCM_SYMBOL_LENGTH (layout) - 1] == 'W')    
    field_type = fields_desc[SCM_SYMBOL_LENGTH (layout) - 2];
  else
    {
      SCM_ASSERT (0, pos, "set_x denied", FUNC_NAME);
      abort ();
    }
  
  switch (field_type)
    {
    case 'u':
      data[p] = SCM_NUM2ULONG (3, val);
      break;

#if 0
    case 'i':
      data[p] = SCM_NUM2LONG (3,val);
      break;

    case 'd':
      *((double *)&(data[p])) = scm_num2dbl (val, (char *)SCM_ARG3);
      break;
#endif

    case 'p':
      data[p] = SCM_UNPACK (val);
      break;

    case 's':
      SCM_ASSERT (0, SCM_MAKE_CHAR (field_type), "self fields immutable", FUNC_NAME);
      break;

    default:
      SCM_ASSERT (0, SCM_MAKE_CHAR (field_type), "unrecognized field type", FUNC_NAME);
      break;
    }

  return val;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable, "struct-vtable", 1, 0, 0, 
            (SCM handle),
	    "Return the vtable structure that describes the type of @var{struct}.")
#define FUNC_NAME s_scm_struct_vtable
{
  SCM_VALIDATE_STRUCT (1,handle);
  return SCM_STRUCT_VTABLE (handle);
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable_tag, "struct-vtable-tag", 1, 0, 0, 
            (SCM handle),
	    "")
#define FUNC_NAME s_scm_struct_vtable_tag
{
  SCM_VALIDATE_VTABLE (1,handle);
  return scm_long2num ((long) SCM_STRUCT_DATA (handle) >> 3);
}
#undef FUNC_NAME

/* {Associating names and classes with vtables}
 *
 * The name of a vtable should probably be stored as a slot.  This is
 * a backward compatible solution until agreement has been achieved on
 * how to associate names with vtables.
 */

unsigned int
scm_struct_ihashq (SCM obj, unsigned int n)
{
  /* The length of the hash table should be a relative prime it's not
     necessary to shift down the address.  */
  return SCM_UNPACK (obj) % n;
}

SCM
scm_struct_create_handle (SCM obj)
{
  SCM handle = scm_hash_fn_create_handle_x (scm_struct_table,
					    obj,
					    SCM_BOOL_F,
					    scm_struct_ihashq,
					    scm_sloppy_assq,
					    0);
  if (SCM_FALSEP (SCM_CDR (handle)))
    SCM_SETCDR (handle, scm_cons (SCM_BOOL_F, SCM_BOOL_F));
  return handle;
}

SCM_DEFINE (scm_struct_vtable_name, "struct-vtable-name", 1, 0, 0, 
            (SCM vtable),
	    "")
#define FUNC_NAME s_scm_struct_vtable_name
{
  SCM_VALIDATE_VTABLE (1,vtable);
  return SCM_STRUCT_TABLE_NAME (SCM_CDR (scm_struct_create_handle (vtable)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_struct_vtable_name_x, "set-struct-vtable-name!", 2, 0, 0, 
            (SCM vtable, SCM name),
	    "")
#define FUNC_NAME s_scm_set_struct_vtable_name_x
{
  SCM_VALIDATE_VTABLE (1,vtable);
  SCM_VALIDATE_SYMBOL (2,name);
  SCM_SET_STRUCT_TABLE_NAME (SCM_CDR (scm_struct_create_handle (vtable)),
			     name);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_print_struct (SCM exp, SCM port, scm_print_state *pstate)
{
  if (SCM_NFALSEP (scm_procedure_p (SCM_STRUCT_PRINTER (exp))))
    scm_printer_apply (SCM_STRUCT_PRINTER (exp), exp, port, pstate);
  else
    {
      SCM vtable = SCM_STRUCT_VTABLE (exp);
      SCM name = scm_struct_vtable_name (vtable);
      scm_puts ("#<", port);
      if (SCM_NFALSEP (name))
	scm_display (name, port);
      else
	scm_puts ("struct", port);
      scm_putc (' ', port);
      scm_intprint (SCM_UNPACK (vtable), 16, port);
      scm_putc (':', port);
      scm_intprint (SCM_UNPACK (exp), 16, port);
      scm_putc ('>', port);
    }
}

void
scm_struct_prehistory ()
{
  scm_c_hook_add (&scm_before_mark_c_hook, scm_struct_gc_init, 0, 0);
  scm_c_hook_add (&scm_after_sweep_c_hook, scm_free_structs, 0, 0);
}

void
scm_init_struct ()
{
  scm_struct_table
    = scm_permanent_object (scm_make_weak_key_hash_table (SCM_MAKINUM (31)));
  required_vtable_fields = scm_makfrom0str ("pruosrpw");
  scm_permanent_object (required_vtable_fields);
  scm_sysintern ("vtable-index-layout", SCM_MAKINUM (scm_vtable_index_layout));
  scm_sysintern ("vtable-index-vtable", SCM_MAKINUM (scm_vtable_index_vtable));
  scm_sysintern ("vtable-index-printer", SCM_MAKINUM (scm_vtable_index_printer));
  scm_sysintern ("vtable-offset-user", SCM_MAKINUM (scm_vtable_offset_user));
#ifndef SCM_MAGIC_SNARFER
#include "libguile/struct.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
