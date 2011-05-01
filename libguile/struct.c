/* Copyright (C) 1996,1997,1998,1999,2000,2001, 2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <alloca.h>
#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/alist.h"
#include "libguile/weaks.h"
#include "libguile/hashtab.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"

#include "libguile/validate.h"
#include "libguile/struct.h"

#include "libguile/eq.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libguile/bdw-gc.h"



/* A needlessly obscure test. */
#define SCM_LAYOUT_TAILP(X)		(((X) & 32) == 0) /* R, W or O */

static SCM required_vtable_fields = SCM_BOOL_F;
static SCM required_applicable_fields = SCM_BOOL_F;
static SCM required_applicable_with_setter_fields = SCM_BOOL_F;
SCM scm_applicable_struct_vtable_vtable;
SCM scm_applicable_struct_with_setter_vtable_vtable;
SCM scm_standard_vtable_vtable;



SCM_DEFINE (scm_make_struct_layout, "make-struct-layout", 1, 0, 0, 
            (SCM fields),
	    "Return a new structure layout object.\n\n"
	    "@var{fields} must be a string made up of pairs of characters\n"
	    "strung together.  The first character of each pair describes a field\n"
	    "type, the second a field protection.  Allowed types are 'p' for\n"
	    "GC-protected Scheme data, 'u' for unprotected binary data, and 's' for\n"
	    "a field that points to the structure itself.    Allowed protections\n"
	    "are 'w' for mutable fields, 'h' for hidden fields, 'r' for read-only\n"
            "fields, and 'o' for opaque fields.\n\n"
            "Hidden fields are writable, but they will not consume an initializer arg\n"
            "passed to @code{make-struct}. They are useful to add slots to a struct\n"
            "in a way that preserves backward-compatibility with existing calls to\n"
            "@code{make-struct}, especially for derived vtables.\n\n"
            "The last field protection specification may be capitalized to indicate\n"
	    "that the field is a tail-array.")
#define FUNC_NAME s_scm_make_struct_layout
{
  SCM new_sym;
  scm_t_wchar c;

  SCM_VALIDATE_STRING (1, fields);

  { /* scope */
    size_t len;
    int x;

    len = scm_i_string_length (fields);
    if (len % 2 == 1)
      SCM_MISC_ERROR ("odd length field specification: ~S", 
		      scm_list_1 (fields));

    for (x = 0; x < len; x += 2)
      {
	switch (c = scm_i_string_ref (fields, x))
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
	    SCM_MISC_ERROR ("unrecognized field type: ~S", 
			    scm_list_1 (SCM_MAKE_CHAR (c)));
	  }

	switch (c = scm_i_string_ref (fields, x + 1))
	  {
	  case 'w':
	  case 'h':
	    if (scm_i_string_ref (fields, x) == 's')
	      SCM_MISC_ERROR ("self fields not writable", SCM_EOL);
	  case 'r':
	  case 'o':
	    break;
	  case 'R':
	  case 'W':
	  case 'O':
	    if (scm_i_string_ref (fields, x) == 's')
	      SCM_MISC_ERROR ("self fields not allowed in tail array", 
			      SCM_EOL);
	    if (x != len - 2)
	      SCM_MISC_ERROR ("tail array field must be last field in layout",
			      SCM_EOL);
	    break;
	  default:
	    SCM_MISC_ERROR ("unrecognized ref specification: ~S",
			    scm_list_1 (SCM_MAKE_CHAR (c)));
	  }
#if 0
	if (scm_i_string_ref (fields, x, 'd'))
	  {
	    if (!scm_i_string_ref (fields, x+2, '-'))
	      SCM_MISC_ERROR ("missing dash field at position ~A",
			      scm_list_1 (scm_from_int (x / 2)));
	    x += 2;
	    goto recheck_ref;
	  }
#endif
      }
    new_sym = scm_string_to_symbol (fields);
  }
  scm_remember_upto_here_1 (fields);
  return new_sym;
}
#undef FUNC_NAME


/* Check whether VTABLE instances have a simple layout (i.e., either only "pr"
   or only "pw" fields) and update its flags accordingly.  */
static void
set_vtable_layout_flags (SCM vtable)
{
  size_t len, field;
  SCM layout;
  const char *c_layout;
  scm_t_bits flags = SCM_VTABLE_FLAG_SIMPLE;

  layout = SCM_VTABLE_LAYOUT (vtable);
  c_layout = scm_i_symbol_chars (layout);
  len = scm_i_symbol_length (layout);

  assert (len % 2 == 0);

  /* Update FLAGS according to LAYOUT.  */
  for (field = 0;
       field < len && flags & SCM_VTABLE_FLAG_SIMPLE;
       field += 2)
    {
      if (c_layout[field] != 'p')
	flags = 0;
      else
	switch (c_layout[field + 1])
	  {
	  case 'w':
	  case 'W':
	    if (field == 0)
	      flags |= SCM_VTABLE_FLAG_SIMPLE_RW;
	    break;

	  case 'r':
	  case 'R':
	    flags &= ~SCM_VTABLE_FLAG_SIMPLE_RW;
	    break;

	  default:
	    flags = 0;
	  }
    }

  if (flags & SCM_VTABLE_FLAG_SIMPLE)
    {
      /* VTABLE is simple so update its flags and record the size of its
	 instances.  */
      SCM_SET_VTABLE_FLAGS (vtable, flags);
      SCM_STRUCT_DATA_SET (vtable, scm_vtable_index_size, len / 2);
    }
}

static int
scm_is_valid_vtable_layout (SCM layout)
{
  size_t len, n;
  const char *c_layout;

  c_layout = scm_i_symbol_chars (layout);
  len = scm_i_symbol_length (layout);

  if (len % 2)
    return 0;
  
  for (n = 0; n < len; n += 2)
    switch (c_layout[n])
      {
      case 'u':
      case 'p':
      case 's':
        switch (c_layout[n+1])
          {
          case 'W':
          case 'R':
          case 'O':
            if (n + 2 != len)
              return 0;
          case 'w':
          case 'h':
          case 'r':
          case 'o':
            break;
          default:
            return 0;
          }
        break;
      default:        
        return 0;
      }
  return 1;
}

/* Have OBJ, a newly created vtable, inherit flags from VTABLE.  VTABLE is a
   vtable-vtable and OBJ is an instance of VTABLE.  */
void
scm_i_struct_inherit_vtable_magic (SCM vtable, SCM obj)
#define FUNC_NAME "%inherit-vtable-magic"
{
  /* Verily, what is the deal here, you ask? Basically, we need to know a couple
     of properties of structures at runtime. For example, "is this structure a
     vtable of vtables (a metaclass)?"; also, "is this structure applicable?".
     Both of these questions also imply a certain layout of the structure. So
     instead of checking the layout at runtime, what we do is pre-verify the
     layout -- so that at runtime we can just check the applicable flag and
     dispatch directly to the Scheme procedure in slot 0.  */
  SCM olayout;

  /* Verify that OBJ is a valid vtable.  */
  if (! scm_is_valid_vtable_layout (SCM_VTABLE_LAYOUT (obj)))
    SCM_MISC_ERROR ("invalid layout for new vtable: ~a",
                    scm_list_1 (SCM_VTABLE_LAYOUT (obj)));

  set_vtable_layout_flags (obj);

  /* If OBJ's vtable is compatible with the required vtable (class) layout, it
     is a metaclass.  */
  olayout = scm_symbol_to_string (SCM_VTABLE_LAYOUT (obj));
  if (scm_is_true (scm_leq_p (scm_string_length (required_vtable_fields),
                              scm_string_length (olayout)))
      && scm_is_true (scm_string_eq (olayout, required_vtable_fields,
                                     scm_from_size_t (0), 
                                     scm_string_length (required_vtable_fields),
                                     scm_from_size_t (0),
                                     scm_string_length (required_vtable_fields))))
    SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_VTABLE);

  /* Finally, if OBJ is an applicable class, verify that its vtable is
     compatible with the required applicable layout.  */
  if (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SETTER_VTABLE))
    {
      if (scm_is_false (scm_string_eq (olayout, required_applicable_with_setter_fields,
                                       scm_from_size_t (0), 
                                       scm_from_size_t (4), 
                                       scm_from_size_t (0),
                                       scm_from_size_t (4))))
        SCM_MISC_ERROR ("invalid applicable-with-setter struct layout",
                        scm_list_1 (olayout));
      SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_APPLICABLE | SCM_VTABLE_FLAG_SETTER);
    }
  else if (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_APPLICABLE_VTABLE))
    {
      if (scm_is_false (scm_string_eq (olayout, required_applicable_fields,
                                       scm_from_size_t (0), 
                                       scm_from_size_t (2), 
                                       scm_from_size_t (0),
                                       scm_from_size_t (2))))
        SCM_MISC_ERROR ("invalid applicable struct layout",
                        scm_list_1 (olayout));
      SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_APPLICABLE);
    }

  SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_VALIDATED);
}
#undef FUNC_NAME


static void
scm_struct_init (SCM handle, SCM layout, size_t n_tail,
                 size_t n_inits, scm_t_bits *inits)
{
  SCM vtable;
  scm_t_bits *mem;

  vtable = SCM_STRUCT_VTABLE (handle);
  mem = SCM_STRUCT_DATA (handle);

  if (SCM_UNPACK (vtable) != 0
      && SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SIMPLE)
      && n_tail == 0
      && n_inits == SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size))
    /* The fast path: HANDLE has N_INITS "p" fields.  */
    memcpy (mem, inits, n_inits * sizeof (SCM));
  else
    {
      scm_t_wchar prot = 0;
      int n_fields = scm_i_symbol_length (layout) / 2;
      int tailp = 0;
      int i;
      size_t inits_idx = 0;

      i = -2;
      while (n_fields)
	{
	  if (!tailp)
	    {
	      i += 2;
	      prot = scm_i_symbol_ref (layout, i+1);
	      if (SCM_LAYOUT_TAILP (prot))
		{
		  tailp = 1;
		  prot = prot == 'R' ? 'r' : prot == 'W' ? 'w' : 'o';
		  *mem++ = (scm_t_bits)n_tail;
		  n_fields += n_tail - 1;
		  if (n_fields == 0)
		    break;
		}
	    }
	  switch (scm_i_symbol_ref (layout, i))
	    {
	    case 'u':
	      if ((prot != 'r' && prot != 'w') || inits_idx == n_inits)
		*mem = 0;
	      else
		{
		  *mem = scm_to_ulong (SCM_PACK (inits[inits_idx]));
		  inits_idx++;
		}
	      break;

	    case 'p':
	      if ((prot != 'r' && prot != 'w') || inits_idx == n_inits)
		*mem = SCM_UNPACK (SCM_BOOL_F);
	      else
		{
		  *mem = inits[inits_idx];
		  inits_idx++;
		}

	      break;

	    case 's':
	      *mem = SCM_UNPACK (handle);
	      break;
	    }

	  n_fields--;
	  mem++;
	}
    }
}


SCM_DEFINE (scm_struct_p, "struct?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a structure object, else\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_struct_p
{
  return scm_from_bool(SCM_STRUCTP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_struct_vtable_p, "struct-vtable?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a vtable structure.")
#define FUNC_NAME s_scm_struct_vtable_p
{
  if (!SCM_STRUCTP (x)
      || !SCM_STRUCT_VTABLE_FLAG_IS_SET (x, SCM_VTABLE_FLAG_VTABLE))
    return SCM_BOOL_F;
  if (!SCM_VTABLE_FLAG_IS_SET (x, SCM_VTABLE_FLAG_VALIDATED))
    SCM_MISC_ERROR ("vtable has invalid layout: ~A",
                    scm_list_1 (SCM_VTABLE_LAYOUT (x)));
  return SCM_BOOL_T;
}
#undef FUNC_NAME


/* Finalization: invoke the finalizer of the struct pointed to by PTR.  */
static void
struct_finalizer_trampoline (GC_PTR ptr, GC_PTR unused_data)
{
  SCM obj = PTR2SCM (ptr);
  scm_t_struct_finalize finalize = SCM_STRUCT_FINALIZER (obj);

  if (finalize)
    finalize (obj);
}

/* All struct data must be allocated at an address whose bottom three
   bits are zero.  This is because the tag for a struct lives in the
   bottom three bits of the struct's car, and the upper bits point to
   the data of its vtable, which is a struct itself.  Thus, if the
   address of that data doesn't end in three zeros, tagging it will
   destroy the pointer.

   I suppose we should make it clear here that, the data must be 8-byte aligned,
   *within* the struct, and the struct itself should be 8-byte aligned. In
   practice we ensure this because the data starts two words into a struct.

   This function allocates an 8-byte aligned block of memory, whose first word
   points to the given vtable data, then a data pointer, then n_words of data.
 */
SCM
scm_i_alloc_struct (scm_t_bits *vtable_data, int n_words)
{
  SCM ret;

  ret = scm_words ((scm_t_bits)vtable_data | scm_tc3_struct, n_words + 2);
  SCM_SET_CELL_WORD_1 (ret, (scm_t_bits)SCM_CELL_OBJECT_LOC (ret, 2));

  /* vtable_data can be null when making a vtable vtable */
  if (vtable_data && vtable_data[scm_vtable_index_instance_finalize])
    {
      /* Register a finalizer for the newly created instance.  */
      GC_finalization_proc prev_finalizer;
      GC_PTR prev_finalizer_data;
      GC_REGISTER_FINALIZER_NO_ORDER (SCM2PTR (ret),
				      struct_finalizer_trampoline,
				      NULL,
				      &prev_finalizer,
				      &prev_finalizer_data);
    }

  return ret;
}


SCM
scm_c_make_structv (SCM vtable, size_t n_tail, size_t n_init, scm_t_bits *init)
#define FUNC_NAME "make-struct"
{
  SCM layout;
  size_t basic_size;
  SCM obj;

  SCM_VALIDATE_VTABLE (1, vtable);

  layout = SCM_VTABLE_LAYOUT (vtable);
  basic_size = scm_i_symbol_length (layout) / 2;

  if (n_tail != 0)
    {
      SCM layout_str, last_char;
      
      if (basic_size == 0)
        {
        bad_tail: 
          SCM_MISC_ERROR ("tail array not allowed unless layout ends R, W, or O", SCM_EOL);
        }

      layout_str = scm_symbol_to_string (layout);
      last_char = scm_string_ref (layout_str,
                                  scm_from_size_t (2 * basic_size - 1));
      if (! SCM_LAYOUT_TAILP (SCM_CHAR (last_char)))
        goto bad_tail;
    }

  obj = scm_i_alloc_struct (SCM_STRUCT_DATA (vtable), basic_size + n_tail);

  scm_struct_init (obj, layout, n_tail, n_init, init);

  /* If we're making a vtable, validate its layout and inherit
     flags. However we allow for separation of allocation and
     initialization, to humor GOOPS, so only validate if the layout was
     passed as an initarg. */
  if (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_VTABLE)
      && scm_is_true (SCM_VTABLE_LAYOUT (obj)))
    scm_i_struct_inherit_vtable_magic (vtable, obj);

  return obj;
}
#undef FUNC_NAME

SCM
scm_c_make_struct (SCM vtable, size_t n_tail, size_t n_init, scm_t_bits init, ...)
{
  va_list foo;
  scm_t_bits *v;
  size_t i;

  v = alloca (sizeof (scm_t_bits) * n_init);

  va_start (foo, init);
  for (i = 0; i < n_init; i++)
    {
      v[i] = init;
      init = va_arg (foo, scm_t_bits);
    }
  va_end (foo);

  return scm_c_make_structv (vtable, n_tail, n_init, v);
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
	    "structure itself. Fields with protection 'o' can not be initialized by\n"
	    "Scheme programs.\n\n"
	    "If fewer optional arguments than initializable fields are supplied,\n"
	    "fields of type 'p' get default value #f while fields of type 'u' are\n"
	    "initialized to 0.\n\n"
	    "For more information, see the documentation for @code{make-vtable-vtable}.")
#define FUNC_NAME s_scm_make_struct
{
  size_t i, n_init;
  long ilen;
  scm_t_bits *v;

  SCM_VALIDATE_VTABLE (1, vtable);
  ilen = scm_ilength (init);
  if (ilen < 0)
    SCM_MISC_ERROR ("Rest arguments do not form a proper list.", SCM_EOL);
  
  n_init = (size_t)ilen;

  /* best to use alloca, but init could be big, so hack to avoid a possible
     stack overflow */
  if (n_init < 64)
    v = alloca (n_init * sizeof(scm_t_bits));
  else
    v = scm_gc_malloc (n_init * sizeof(scm_t_bits), "struct");

  for (i = 0; i < n_init; i++, init = SCM_CDR (init))
    v[i] = SCM_UNPACK (SCM_CAR (init));

  return scm_c_make_structv (vtable, scm_to_size_t (tail_array_size), n_init, v);
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
	    "@lisp\n"
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
	    "@end lisp")
#define FUNC_NAME s_scm_make_vtable_vtable
{
  SCM fields, layout, obj;
  size_t basic_size, n_tail, i, n_init;
  long ilen;
  scm_t_bits *v;

  SCM_VALIDATE_STRING (1, user_fields);
  ilen = scm_ilength (init);
  if (ilen < 0)
    SCM_MISC_ERROR ("Rest arguments do not form a proper list.", SCM_EOL);
  
  n_init = (size_t)ilen + 1; /* + 1 for the layout */

  /* best to use alloca, but init could be big, so hack to avoid a possible
     stack overflow */
  if (n_init < 64)
    v = alloca (n_init * sizeof(scm_t_bits));
  else
    v = scm_gc_malloc (n_init * sizeof(scm_t_bits), "struct");

  fields = scm_string_append (scm_list_2 (required_vtable_fields,
					  user_fields));
  layout = scm_make_struct_layout (fields);
  if (!scm_is_valid_vtable_layout (layout))
    SCM_MISC_ERROR ("invalid user fields", scm_list_1 (user_fields));

  basic_size = scm_i_symbol_length (layout) / 2;
  n_tail = scm_to_size_t (tail_array_size);

  i = 0;
  v[i++] = SCM_UNPACK (layout);
  for (; i < n_init; i++, init = SCM_CDR (init))
    v[i] = SCM_UNPACK (SCM_CAR (init));

  SCM_CRITICAL_SECTION_START;
  obj = scm_i_alloc_struct (NULL, basic_size + n_tail);
  /* Make it so that the vtable of OBJ is itself.  */
  SCM_SET_CELL_WORD_0 (obj, (scm_t_bits) SCM_STRUCT_DATA (obj) | scm_tc3_struct);
  SCM_CRITICAL_SECTION_END;

  scm_struct_init (obj, layout, n_tail, n_init, v);
  SCM_SET_VTABLE_FLAGS (obj,
                        SCM_VTABLE_FLAG_VTABLE | SCM_VTABLE_FLAG_VALIDATED);

  return obj;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_vtable, "make-vtable", 1, 1, 0,
            (SCM fields, SCM printer),
	    "Create a vtable, for creating structures with the given\n"
	    "@var{fields}.\n"
	    "\n"
	    "The optional @var{printer} argument is a function to be called\n"
	    "@code{(@var{printer} struct port)} on the structures created.\n"
	    "It should look at @var{struct} and write to @var{port}.")
#define FUNC_NAME s_scm_make_vtable
{
  if (SCM_UNBNDP (printer))
    printer = SCM_BOOL_F;

  return scm_make_struct (scm_standard_vtable_vtable, SCM_INUM0,
                          scm_list_2 (scm_make_struct_layout (fields),
                                      printer));
}
#undef FUNC_NAME


/* Return true if S1 and S2 are equal structures, i.e., if their vtable and
   contents are the same.  Field protections are honored.  Thus, it is an
   error to test the equality of structures that contain opaque fields.  */
SCM
scm_i_struct_equalp (SCM s1, SCM s2)
#define FUNC_NAME "scm_i_struct_equalp"
{
  SCM vtable1, vtable2, layout;
  size_t struct_size, field_num;

  SCM_VALIDATE_STRUCT (1, s1);
  SCM_VALIDATE_STRUCT (2, s2);

  vtable1 = SCM_STRUCT_VTABLE (s1);
  vtable2 = SCM_STRUCT_VTABLE (s2);

  if (!scm_is_eq (vtable1, vtable2))
    return SCM_BOOL_F;

  layout = SCM_STRUCT_LAYOUT (s1);
  struct_size = scm_i_symbol_length (layout) / 2;

  for (field_num = 0; field_num < struct_size; field_num++)
    {
      SCM s_field_num;
      SCM field1, field2;

      /* We have to use `scm_struct_ref ()' here so that fields are accessed
	 consistently, notably wrt. field types and access rights.  */
      s_field_num = scm_from_size_t (field_num);
      field1 = scm_struct_ref (s1, s_field_num);
      field2 = scm_struct_ref (s2, s_field_num);

      /* Self-referencing fields (type `s') must be skipped to avoid infinite
	 recursion.  */
      if (!(scm_is_eq (field1, s1) && (scm_is_eq (field2, s2))))
	if (scm_is_false (scm_equal_p (field1, field2)))
	  return SCM_BOOL_F;
    }

  /* FIXME: Tail elements should be tested for equality.  */

  return SCM_BOOL_T;
}
#undef FUNC_NAME





SCM_DEFINE (scm_struct_ref, "struct-ref", 2, 0, 0,
            (SCM handle, SCM pos),
	    "Access the @var{n}th field of @var{struct}.\n\n"
	    "If the field is of type 'p', then it can be set to an arbitrary value.\n\n"
	    "If the field is of type 'u', then it can only be set to a non-negative\n"
	    "integer value small enough to fit in one machine word.")
#define FUNC_NAME s_scm_struct_ref
{
  SCM vtable, answer = SCM_UNDEFINED;
  scm_t_bits *data;
  size_t p;

  SCM_VALIDATE_STRUCT (1, handle);

  vtable = SCM_STRUCT_VTABLE (handle);
  data = SCM_STRUCT_DATA (handle);
  p = scm_to_size_t (pos);

  if (SCM_LIKELY (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SIMPLE)
  		  && p < SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size)))
    /* The fast path: HANDLE is a struct with only "p" fields.  */
    answer = SCM_PACK (data[p]);
  else
    {
      SCM layout;
      size_t layout_len, n_fields;
      scm_t_wchar field_type = 0;

      layout = SCM_STRUCT_LAYOUT (handle);
      layout_len = scm_i_symbol_length (layout);
      n_fields = layout_len / 2;

      if (SCM_LAYOUT_TAILP (scm_i_symbol_ref (layout, layout_len - 1)))
	n_fields += data[n_fields - 1];

      SCM_ASSERT_RANGE (1, pos, p < n_fields);

      if (p * 2 < layout_len)
	{
	  scm_t_wchar ref;
	  field_type = scm_i_symbol_ref (layout, p * 2);
	  ref = scm_i_symbol_ref (layout, p * 2 + 1);
	  if ((ref != 'r') && (ref != 'w') && (ref != 'h'))
	    {
	      if ((ref == 'R') || (ref == 'W'))
		field_type = 'u';
	      else
		SCM_MISC_ERROR ("ref denied for field ~A", scm_list_1 (pos));
	    }
	}
      else if (scm_i_symbol_ref (layout, layout_len - 1) != 'O')
	field_type = scm_i_symbol_ref(layout, layout_len - 2);
      else
	SCM_MISC_ERROR ("ref denied for field ~A", scm_list_1 (pos));

      switch (field_type)
	{
	case 'u':
	  answer = scm_from_ulong (data[p]);
	  break;

#if 0
	case 'i':
	  answer = scm_from_long (data[p]);
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
	  SCM_MISC_ERROR ("unrecognized field type: ~S",
			  scm_list_1 (SCM_MAKE_CHAR (field_type)));
	}
    }

  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_set_x, "struct-set!", 3, 0, 0,
            (SCM handle, SCM pos, SCM val),
	    "Set the slot of the structure @var{handle} with index @var{pos}\n"
	    "to @var{val}.  Signal an error if the slot can not be written\n"
	    "to.")
#define FUNC_NAME s_scm_struct_set_x
{
  SCM vtable;
  scm_t_bits *data;
  size_t p;

  SCM_VALIDATE_STRUCT (1, handle);

  vtable = SCM_STRUCT_VTABLE (handle);
  data = SCM_STRUCT_DATA (handle);
  p = scm_to_size_t (pos);

  if (SCM_LIKELY (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SIMPLE)
  		  && SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SIMPLE_RW)
  		  && p < SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size)))
    /* The fast path: HANDLE is a struct with only "pw" fields.  */
    data[p] = SCM_UNPACK (val);
  else
    {
      SCM layout;
      size_t layout_len, n_fields;
      scm_t_wchar field_type = 0;

      layout = SCM_STRUCT_LAYOUT (handle);
      layout_len = scm_i_symbol_length (layout);
      n_fields = layout_len / 2;

      if (SCM_LAYOUT_TAILP (scm_i_symbol_ref (layout, layout_len - 1)))
	n_fields += data[n_fields - 1];

      SCM_ASSERT_RANGE (1, pos, p < n_fields);

      if (p * 2 < layout_len)
	{
	  char set_x;
	  field_type = scm_i_symbol_ref (layout, p * 2);
	  set_x = scm_i_symbol_ref (layout, p * 2 + 1);
	  if (set_x != 'w' && set_x != 'h')
	    SCM_MISC_ERROR ("set! denied for field ~A", scm_list_1 (pos));
	}
      else if (scm_i_symbol_ref (layout, layout_len - 1) == 'W')
	field_type = scm_i_symbol_ref (layout, layout_len - 2);
      else
	SCM_MISC_ERROR ("set! denied for field ~A", scm_list_1 (pos));

      switch (field_type)
	{
	case 'u':
	  data[p] = SCM_NUM2ULONG (3, val);
	  break;

#if 0
	case 'i':
	  data[p] = SCM_NUM2LONG (3, val);
	  break;

	case 'd':
	  *((double *)&(data[p])) = scm_num2dbl (val, (char *)SCM_ARG3);
	  break;
#endif

	case 'p':
	  data[p] = SCM_UNPACK (val);
	  break;

	case 's':
	  SCM_MISC_ERROR ("self fields immutable", SCM_EOL);

	default:
	  SCM_MISC_ERROR ("unrecognized field type: ~S",
			  scm_list_1 (SCM_MAKE_CHAR (field_type)));
	}
    }

  return val;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable, "struct-vtable", 1, 0, 0, 
            (SCM handle),
	    "Return the vtable structure that describes the type of @var{struct}.")
#define FUNC_NAME s_scm_struct_vtable
{
  SCM_VALIDATE_STRUCT (1, handle);
  return SCM_STRUCT_VTABLE (handle);
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable_tag, "struct-vtable-tag", 1, 0, 0, 
            (SCM handle),
	    "Return the vtable tag of the structure @var{handle}.")
#define FUNC_NAME s_scm_struct_vtable_tag
{
  SCM_VALIDATE_VTABLE (1, handle);
  return scm_from_unsigned_integer
    (((scm_t_bits)SCM_STRUCT_DATA (handle)) >> 3);
}
#undef FUNC_NAME

/* {Associating names and classes with vtables}
 *
 * The name of a vtable should probably be stored as a slot.  This is
 * a backward compatible solution until agreement has been achieved on
 * how to associate names with vtables.
 */

unsigned long
scm_struct_ihashq (SCM obj, unsigned long n, void *closure)
{
  /* The length of the hash table should be a relative prime it's not
     necessary to shift down the address.  */
  return SCM_UNPACK (obj) % n;
}

SCM_DEFINE (scm_struct_vtable_name, "struct-vtable-name", 1, 0, 0, 
            (SCM vtable),
	    "Return the name of the vtable @var{vtable}.")
#define FUNC_NAME s_scm_struct_vtable_name
{
  SCM_VALIDATE_VTABLE (1, vtable);
  return SCM_VTABLE_NAME (vtable);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_struct_vtable_name_x, "set-struct-vtable-name!", 2, 0, 0, 
            (SCM vtable, SCM name),
	    "Set the name of the vtable @var{vtable} to @var{name}.")
#define FUNC_NAME s_scm_set_struct_vtable_name_x
{
  SCM_VALIDATE_VTABLE (1, vtable);
  SCM_VALIDATE_SYMBOL (2, name);
  SCM_SET_VTABLE_NAME (vtable, name);
  /* FIXME: remove this, and implement proper struct classes instead.
     (Vtables *are* classes.)  */
  scm_i_define_class_for_vtable (vtable);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_print_struct (SCM exp, SCM port, scm_print_state *pstate)
{
  if (scm_is_true (scm_procedure_p (SCM_STRUCT_PRINTER (exp))))
    scm_printer_apply (SCM_STRUCT_PRINTER (exp), exp, port, pstate);
  else
    {
      SCM vtable = SCM_STRUCT_VTABLE (exp);
      SCM name = scm_struct_vtable_name (vtable);
      scm_puts ("#<", port);
      if (scm_is_true (name))
	{
          scm_display (name, port);
          scm_putc (' ', port);
        }
      else
	{
          if (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_VTABLE))
            scm_puts ("vtable:", port);
          else
            scm_puts ("struct:", port);
          scm_uintprint (SCM_UNPACK (vtable), 16, port);
          scm_putc (' ', port);
          scm_write (SCM_VTABLE_LAYOUT (vtable), port);
          scm_putc (' ', port);
        }
      scm_uintprint (SCM_UNPACK (exp), 16, port);
      /* hackety hack */
      if (SCM_STRUCT_APPLICABLE_P (exp))
        {
          if (scm_is_true (SCM_STRUCT_PROCEDURE (exp)))
            {
              scm_puts (" proc: ", port);
              if (scm_is_true (scm_procedure_p (SCM_STRUCT_PROCEDURE (exp))))
                scm_write (SCM_STRUCT_PROCEDURE (exp), port);
              else
                scm_puts ("(not a procedure?)", port);
            }
          if (SCM_STRUCT_SETTER_P (exp))
            {
              scm_puts (" setter: ", port);
              scm_write (SCM_STRUCT_SETTER (exp), port);
            }
        }
      scm_putc ('>', port);
    }
}

void
scm_init_struct ()
{
  /* The first word of a struct is equal to `SCM_STRUCT_DATA (vtable) +
     scm_tc3_struct', and `SCM_STRUCT_DATA (vtable)' is 2 words after VTABLE by
     default.  */
  GC_REGISTER_DISPLACEMENT (2 * sizeof (scm_t_bits) + scm_tc3_struct);

  /* In the general case, `SCM_STRUCT_DATA (obj)' points 2 words after the
     beginning of a GC-allocated region; that region is different from that of
     OBJ once OBJ has undergone class redefinition.  */
  GC_REGISTER_DISPLACEMENT (2 * sizeof (scm_t_bits));

  required_vtable_fields = scm_from_locale_string (SCM_VTABLE_BASE_LAYOUT);
  required_applicable_fields = scm_from_locale_string (SCM_APPLICABLE_BASE_LAYOUT);
  required_applicable_with_setter_fields = scm_from_locale_string (SCM_APPLICABLE_WITH_SETTER_BASE_LAYOUT);

  scm_standard_vtable_vtable =
    scm_make_vtable_vtable (scm_nullstr, SCM_INUM0, SCM_EOL);

  scm_applicable_struct_vtable_vtable =
    scm_make_struct (scm_standard_vtable_vtable, SCM_INUM0,
                     scm_list_1 (scm_make_struct_layout (required_vtable_fields)));
  SCM_SET_VTABLE_FLAGS (scm_applicable_struct_vtable_vtable,
                        SCM_VTABLE_FLAG_APPLICABLE_VTABLE);
  scm_c_define ("<applicable-struct-vtable>", scm_applicable_struct_vtable_vtable);

  scm_applicable_struct_with_setter_vtable_vtable =
    scm_make_struct (scm_standard_vtable_vtable, SCM_INUM0,
                     scm_list_1 (scm_make_struct_layout (required_vtable_fields)));
  SCM_SET_VTABLE_FLAGS (scm_applicable_struct_with_setter_vtable_vtable,
                        SCM_VTABLE_FLAG_APPLICABLE_VTABLE | SCM_VTABLE_FLAG_SETTER_VTABLE);
  scm_c_define ("<applicable-struct-with-setter-vtable>", scm_applicable_struct_with_setter_vtable_vtable);

  scm_c_define ("vtable-index-layout", scm_from_int (scm_vtable_index_layout));
  scm_c_define ("vtable-index-printer",
		scm_from_int (scm_vtable_index_instance_printer));
  scm_c_define ("vtable-offset-user", scm_from_int (scm_vtable_offset_user));
#include "libguile/struct.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
