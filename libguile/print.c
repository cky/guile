/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#include "genio.h"
#include "mbstrings.h"
#include "smob.h"
#include "eval.h"
#include "procprop.h"
#include "read.h"
#include "weaks.h"
#include "unif.h"
#include "alist.h"
#include "struct.h"

#include "print.h"


/* {Names of immediate symbols}
 * 
 * This table must agree with the declarations in scm.h: {Immediate Symbols}.
 */

char *scm_isymnames[] =
{
  /* This table must agree with the declarations */
  "#@and",
  "#@begin",
  "#@case",
  "#@cond",
  "#@do",
  "#@if",
  "#@lambda",
  "#@let",
  "#@let*",
  "#@letrec",
  "#@or",
  "#@quote",
  "#@set!",
  "#@define",
#if 0
  "#@literal-variable-ref",
  "#@literal-variable-set!",
#endif
  "#@apply",
  "#@call-with-current-continuation",

 /* user visible ISYMS */
 /* other keywords */
 /* Flags */

  "#f",
  "#t",
  "#<undefined>",
  "#<eof>",
  "()",
  "#<unspecified>"
};

scm_option scm_print_opts[] = {
  { SCM_OPTION_SCM, "closure-hook", SCM_BOOL_F,
    "Hook for printing closures." },
  { SCM_OPTION_BOOLEAN, "source", 0,
    "Print closures with source." }
};

SCM_PROC (s_print_options, "print-options-interface", 0, 1, 0, scm_print_options);
#ifdef __STDC__
SCM
scm_print_options (SCM setting)
#else
SCM
scm_print_options (setting)
     SCM setting;
#endif
{
  SCM ans = scm_options (setting,
			 scm_print_opts,
			 SCM_N_PRINT_OPTIONS,
			 s_print_options);
  return ans;
}


/* {Printing of Scheme Objects}
 */

/* Detection of circular references.
 *
 * Due to other constraints in the implementation, this code has bad
 * time complexity (O (depth * N)), The printer code will be
 * completely rewritten before next release of Guile.  The new code
 * will be O(N).
 */
#define PUSH_REF(pstate, obj) \
{ \
  pstate->ref_stack[pstate->top++] = (obj); \
  if (pstate->top == pstate->ceiling) \
    grow_ref_stack (pstate); \
}

#define ENTER_NESTED_DATA(pstate, obj, label) \
{ \
  register int i; \
  for (i = 0; i < pstate->top; ++i) \
    if (pstate->ref_stack[i] == (obj)) \
      goto label; \
  if (pstate->fancyp) \
    { \
      if (pstate->top - pstate->list_offset >= pstate->level) \
	{ \
	  scm_gen_putc ('#', port); \
	  return; \
	} \
    } \
  PUSH_REF(pstate, obj); \
} \

#define EXIT_NESTED_DATA(pstate) { --pstate->top; }

static SCM print_state_pool;

#if 1 /* Used for debugging purposes */
SCM_PROC(s_current_pstate, "current-pstate", 0, 0, 0, scm_current_pstate);
#ifdef __STDC__
SCM
scm_current_pstate (void)
#else
SCM
scm_current_pstate ()
#endif
{
  return SCM_CADR (print_state_pool);
}
#endif

#define PSTATE_SIZE 50L

#ifdef __STDC__
SCM
scm_make_print_state (void)
#else
SCM
scm_make_print_state ()
#endif
{
  return scm_make_struct (SCM_CAR (print_state_pool), /* pstate type */
			  SCM_MAKINUM (PSTATE_SIZE),
			  SCM_EOL);
}

#ifdef __STDC__
static void
grow_ref_stack (scm_print_state *pstate)
#else
static void
grow_ref_stack (pstate)
     scm_print_state *pstate;
#endif
{
  int i, size = pstate->ceiling;
  int total_size;
  SCM handle;
  SCM *data;
  SCM_DEFER_INTS;
  handle = pstate->handle;
  data = (SCM *) pstate - scm_struct_n_extra_words;
  total_size = ((SCM *) pstate)[scm_struct_i_n_words];
  data = (SCM *) scm_must_realloc ((char *) data,
				   total_size,
				   total_size + size,
				   "grow_ref_stack");
  pstate = (scm_print_state *) (data + scm_struct_n_extra_words);
  ((SCM *) pstate)[scm_struct_i_n_words] = total_size + size;
  pstate->ceiling += size;
  for (i = size; i < pstate->ceiling; ++i)
    pstate->ref_stack[i] = SCM_BOOL_F;
  SCM_SETCDR (handle, pstate);
  SCM_ALLOW_INTS;
}

#ifdef __STDC__
static void
print_circref (SCM port, scm_print_state *pstate, SCM ref)
#else
static void
print_circref (port, pstate, ref)
     SCM port;
     scm_print_state *pstate;
     SCM ref;
#endif
{
  register int i;
  int self = pstate->top - 1;
  i = pstate->top - 1;
  if (SCM_CONSP (pstate->ref_stack[i]))
    {
      while (i > 0)
	{
	  if (SCM_NCONSP (pstate->ref_stack[i - 1])
	      || SCM_CDR (pstate->ref_stack[i - 1]) != pstate->ref_stack[i])
	    break;
	  --i;
	}
      self = i;
    }
  for (i = pstate->top - 1; 1; --i)
    if (pstate->ref_stack[i] == ref)
      break;
  scm_gen_putc ('#', port);
  scm_intprint (i - self, 10, port);
  scm_gen_putc ('#', port);
}

/* Print generally.  Handles both write and display according to PSTATE.
 */

#ifdef __STDC__
void 
scm_iprin1 (SCM exp, SCM port, scm_print_state *pstate)
#else
void 
scm_iprin1 (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
#endif
{
  register long i;
taloop:
  switch (7 & (int) exp)
    {
    case 2:
    case 6:
      scm_intprint (SCM_INUM (exp), 10, port);
      break;
    case 4:
      if (SCM_ICHRP (exp))
	{
	  i = SCM_ICHR (exp);
	  scm_put_wchar (i, port, SCM_WRITINGP (pstate));

	}
      else if (SCM_IFLAGP (exp)
	       && (SCM_ISYMNUM (exp) < (sizeof scm_isymnames / sizeof (char *))))
	  scm_gen_puts (scm_regular_string, SCM_ISYMCHARS (exp), port);
      else if (SCM_ILOCP (exp))
	{
	  scm_gen_puts (scm_regular_string, "#@", port);
	  scm_intprint ((long) SCM_IFRAME (exp), 10, port);
	  scm_gen_putc (SCM_ICDRP (exp) ? '-' : '+', port);
	  scm_intprint ((long) SCM_IDIST (exp), 10, port);
	}
      else
	goto idef;
      break;
    case 1:
      /* gloc */
      scm_gen_puts (scm_regular_string, "#@", port);
      exp = SCM_CAR (exp - 1);
      goto taloop;
    default:
    idef:
      scm_ipruk ("immediate", exp, port);
      break;
    case 0:
      switch (SCM_TYP7 (exp))
	{
	case scm_tcs_cons_gloc:

	  if (SCM_CDR (SCM_CAR (exp) - 1L) == 0)
	    {
	      scm_gen_write (scm_regular_string, "#<struct ", sizeof ("#<struct ") - 1, port);
	      scm_intprint(exp, 16, port);
	      scm_gen_putc ('>', port);
	      break;
	    }

	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  scm_iprlist ("(", exp, ')', port, pstate);
	  EXIT_NESTED_DATA (pstate);
	  break;
	circref:
	  print_circref (port, pstate, exp);
	  break;
	case scm_tcs_closures:
	  if (SCM_NFALSEP (scm_procedure_p (SCM_PRINT_CLOSURE)))
	    {
	      SCM ans = scm_cons2 (exp, port,
				   scm_cons (SCM_WRITINGP (pstate)
					     ? SCM_BOOL_T
					     : SCM_BOOL_F,
					     SCM_EOL));
	      ans = scm_apply (SCM_PRINT_CLOSURE, ans, SCM_EOL);
	    }
	  else
	    {
	      SCM name, code;
	      name = scm_procedure_property (exp, scm_i_name);
	      code = SCM_CODE (exp);
	      scm_gen_puts (scm_regular_string, "#<procedure ", port);
	      if (SCM_NIMP (name) && SCM_ROSTRINGP (name))
		{
		  scm_gen_puts (scm_regular_string, SCM_ROCHARS (name), port);
		  scm_gen_putc (' ', port);
		}
	      scm_iprin1 (SCM_CAR (code), port, pstate);
	      if (SCM_PRINT_SOURCE_P)
		{
		  code = scm_unmemocopy (SCM_CDR (code),
					 SCM_EXTEND_ENV (SCM_CAR (code),
							 SCM_EOL,
							 SCM_ENV (exp)));
		  ENTER_NESTED_DATA (pstate, exp, circref);
		  scm_iprlist (" ", code, '>', port, pstate);
		  EXIT_NESTED_DATA (pstate);
		}
	      else
		scm_gen_putc ('>', port);
	    }
	  break;
	case scm_tc7_mb_string:
	case scm_tc7_mb_substring:
	  scm_print_mb_string (exp, port, SCM_WRITINGP (pstate));
	  break;
	case scm_tc7_substring:
	case scm_tc7_string:
	  if (SCM_WRITINGP (pstate))
	    {
	      scm_gen_putc ('"', port);
	      for (i = 0; i < SCM_ROLENGTH (exp); ++i)
		switch (SCM_ROCHARS (exp)[i])
		  {
		  case '"':
		  case '\\':
		    scm_gen_putc ('\\', port);
		  default:
		    scm_gen_putc (SCM_ROCHARS (exp)[i], port);
		  }
	      scm_gen_putc ('"', port);
	      break;
	    }
	  else
	    scm_gen_write (scm_regular_string, SCM_ROCHARS (exp),
			   (scm_sizet) SCM_ROLENGTH (exp),
			   port);
	  break;
	case scm_tcs_symbols:
	  if (SCM_MB_STRINGP (exp))
	    {
	      scm_print_mb_symbol (exp, port);
	      break;
	    }
	  else
	    {
	      int pos;
	      int end;
	      int len;
	      char * str;
	      int weird;
	      int maybe_weird;
	      int mw_pos;

	      len = SCM_LENGTH (exp);
	      str = SCM_CHARS (exp);
	      scm_remember (&exp);
	      pos = 0;
	      weird = 0;
	      maybe_weird = 0;

	      if (len == 0)
		scm_gen_write (scm_regular_string, "#{}#", 4, port);

	      for (end = pos; end < len; ++end)
		switch (str[end])
		  {
#ifdef BRACKETS_AS_PARENS
		  case '[':
		  case ']':
#endif
		  case '(':
		  case ')':
		  case '"':
		  case ';':
		  case SCM_WHITE_SPACES:
		  case SCM_LINE_INCREMENTORS:
		  weird_handler:
		    if (maybe_weird)
		      {
			end = mw_pos;
			maybe_weird = 0;
		      }
		    if (!weird)
		      {
			scm_gen_write (scm_regular_string, "#{", 2, port);
			weird = 1;
		      }
		    if (pos < end)
		      {
			scm_gen_write (scm_regular_string, str + pos, end - pos, port);
		      }
		    {
		      char buf[2];
		      buf[0] = '\\';
		      buf[1] = str[end];
		      scm_gen_write (scm_regular_string, buf, 2, port);
		    }
		    pos = end + 1;
		    break;
		  case '\\':
		    if (weird)
		      goto weird_handler;
		    if (!maybe_weird)
		      {
			maybe_weird = 1;
			mw_pos = pos;
		      }
		    break;
		  case '}':
		  case '#':
		    if (weird)
		      goto weird_handler;
		    break;
		  default:
		    break;
		  }
	      if (pos < end)
		scm_gen_write (scm_regular_string, str + pos, end - pos, port);
	      if (weird)
		scm_gen_write (scm_regular_string, "}#", 2, port);
	      break;
	    }
	case scm_tc7_wvect:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  if (SCM_IS_WHVEC (exp))
	    scm_gen_puts (scm_regular_string, "#wh(", port);
	  else
	    scm_gen_puts (scm_regular_string, "#w(", port);
	  goto common_vector_printer;

	case scm_tc7_vector:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  scm_gen_puts (scm_regular_string, "#(", port);
	common_vector_printer:
	  for (i = 0; i + 1 < SCM_LENGTH (exp); ++i)
	    {
	      /* CHECK_INTS; */
	      scm_iprin1 (SCM_VELTS (exp)[i], port, pstate);
	      scm_gen_putc (' ', port);
	    }
	  if (i < SCM_LENGTH (exp))
	    {
	      /* CHECK_INTS; */
	      scm_iprin1 (SCM_VELTS (exp)[i], port, pstate);
	    }
	  scm_gen_putc (')', port);
	  EXIT_NESTED_DATA (pstate);
	  break;
	case scm_tc7_bvect:
	case scm_tc7_byvect:
	case scm_tc7_svect:
	case scm_tc7_ivect:
	case scm_tc7_uvect:
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
#ifdef LONGLONGS
	case scm_tc7_llvect:
#endif
	  scm_raprin1 (exp, port, pstate);
	  break;
	case scm_tcs_subrs:
	  scm_gen_puts (scm_regular_string, "#<primitive-procedure ", port);
	  scm_gen_puts ((SCM_MB_STRINGP (SCM_SNAME(exp))
			 ? scm_mb_string
			 : scm_regular_string),
			SCM_CHARS (SCM_SNAME (exp)), port);
	  scm_gen_putc ('>', port);
	  break;
#ifdef CCLO
	case scm_tc7_cclo:
	  scm_gen_puts (scm_regular_string, "#<compiled-closure ", port);
	  scm_iprin1 (SCM_CCLO_SUBR (exp), port, pstate);
	  scm_gen_putc ('>', port);
	  break;
#endif
	case scm_tc7_contin:
	  scm_gen_puts (scm_regular_string, "#<continuation ", port);
	  scm_intprint (SCM_LENGTH (exp), 10, port);
	  scm_gen_puts (scm_regular_string, " @ ", port);
	  scm_intprint ((long) SCM_CHARS (exp), 16, port);
	  scm_gen_putc ('>', port);
	  break;
	case scm_tc7_port:
	  i = SCM_PTOBNUM (exp);
	  if (i < scm_numptob
	      && scm_ptobs[i].print
	      && (scm_ptobs[i].print) (exp, port, pstate))
	    break;
	  goto punk;
	case scm_tc7_smob:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  i = SCM_SMOBNUM (exp);
	  if (i < scm_numsmob && scm_smobs[i].print
	      && (scm_smobs[i].print) (exp, port, pstate))
	    {
	      EXIT_NESTED_DATA (pstate);
	      break;
	    }
	  EXIT_NESTED_DATA (pstate);
	default:
	punk:
	  scm_ipruk ("type", exp, port);
	}
    }
}

/* Print states are necessary for circular reference safe printing.
 * They are also expensive to allocate.  Therefore print states are
 * kept in a pool so that they can be reused.
 */
#ifdef __STDC__
void 
scm_prin1 (SCM exp, SCM port, int writingp)
#else
void 
scm_prin1 (exp, port, writingp)
     SCM exp;
     SCM port;
     int writingp;
#endif
{
  SCM handle = 0; /* Will GC protect the handle whilst unlinked */
  scm_print_state *pstate;

  /* First try to allocate a print state from the pool */
  SCM_DEFER_INTS;
  if (SCM_NNULLP (SCM_CDR (print_state_pool)))
    {
      handle = SCM_CDR (print_state_pool);
      SCM_SETCDR (print_state_pool, SCM_CDDR (print_state_pool));
    }
  SCM_ALLOW_INTS;
  
  if (!handle)
    handle = scm_cons (scm_make_print_state (), SCM_EOL);

  pstate = (scm_print_state *) SCM_STRUCT_DATA (SCM_CAR (handle));
  pstate->writingp = writingp;
  scm_iprin1 (exp, port, pstate);

  /* Return print state to pool */
  SCM_DEFER_INTS;
  SCM_SETCDR (handle, SCM_CDR (print_state_pool));
  SCM_SETCDR (print_state_pool, handle);
  SCM_ALLOW_INTS;
}


/* Print an integer.
 */
#ifdef __STDC__
void 
scm_intprint (long n, int radix, SCM port)
#else
void 
scm_intprint (n, radix, port)
     long n;
     int radix;
     SCM port;
#endif
{
  char num_buf[SCM_INTBUFLEN];
  scm_gen_write (scm_regular_string, num_buf, scm_iint2str (n, radix, num_buf), port);
}

/* Print an object of unrecognized type.
 */
#ifdef __STDC__
void 
scm_ipruk (char *hdr, SCM ptr, SCM port)
#else
void 
scm_ipruk (hdr, ptr, port)
     char *hdr;
     SCM ptr;
     SCM port;
#endif
{
  scm_gen_puts (scm_regular_string, "#<unknown-", port);
  scm_gen_puts (scm_regular_string, hdr, port);
  if (SCM_CELLP (ptr))
    {
      scm_gen_puts (scm_regular_string, " (0x", port);
      scm_intprint (SCM_CAR (ptr), 16, port);
      scm_gen_puts (scm_regular_string, " . 0x", port);
      scm_intprint (SCM_CDR (ptr), 16, port);
      scm_gen_puts (scm_regular_string, ") @", port);
    }
  scm_gen_puts (scm_regular_string, " 0x", port);
  scm_intprint (ptr, 16, port);
  scm_gen_putc ('>', port);
}

/* Print a list.
 */

#ifdef __STDC__
void 
scm_iprlist (char *hdr, SCM exp, char tlr, SCM port, scm_print_state *pstate)
#else
void 
scm_iprlist (hdr, exp, tlr, port, pstate)
     char *hdr;
     SCM exp;
     char tlr;
     SCM port;
     scm_print_state *pstate;
#endif
{
  register int i;
  register SCM hare, tortoise;
  int floor = pstate->top - 2;
  scm_gen_puts (scm_regular_string, hdr, port);
  /* CHECK_INTS; */
  if (pstate->fancyp)
    goto fancy_printing;
  
  /* Run a hare and tortoise so that total time complexity will be
     O(depth * N) instead of O(N^2). */
  hare = SCM_CDR (exp);
  tortoise = exp;
  while (SCM_NIMP (hare) && SCM_ECONSP (hare))
    {
      if (hare == tortoise)
	goto fancy_printing;
      hare = SCM_CDR (hare);
      if (SCM_IMP (hare) || SCM_NECONSP (hare))
	break;
      hare = SCM_CDR (hare);
      tortoise = SCM_CDR (tortoise);
    }
  
  /* No cdr cycles intrinsic to this list */
  scm_iprin1 (SCM_CAR (exp), port, pstate);
  exp = SCM_CDR (exp);
  for (; SCM_NIMP (exp); exp = SCM_CDR (exp))
    {
      if (SCM_NECONSP (exp))
	break;
      for (i = floor; i >= 0; --i)
	if (pstate->ref_stack[i] == exp)
	  goto circref;
      PUSH_REF (pstate, exp);
      scm_gen_putc (' ', port);
      /* CHECK_INTS; */
      scm_iprin1 (SCM_CAR (exp), port, pstate);
    }
  if (SCM_NNULLP (exp))
    {
      scm_gen_puts (scm_regular_string, " . ", port);
      scm_iprin1 (exp, port, pstate);
    }

end:
  scm_gen_putc (tlr, port);
  pstate->top = floor + 2;
  return;
  
fancy_printing:
  {
    int n = pstate->length;
    
    scm_iprin1 (SCM_CAR (exp), port, pstate);
    exp = SCM_CDR (exp); --n;
    for (; SCM_NIMP (exp); exp = SCM_CDR (exp))
      {
	if (SCM_NECONSP (exp))
	  break;
	for (i = 0; i < pstate->top; ++i)
	  if (pstate->ref_stack[i] == exp)
	    goto fancy_circref;
	if (pstate->fancyp)
	  {
	    if (n == 0)
	      {
		scm_gen_puts (scm_regular_string, " ...", port);
		goto skip_tail;
	      }
	    else
	      --n;
	  }
	PUSH_REF(pstate, exp);
	++pstate->list_offset;
	scm_gen_putc (' ', port);
	/* CHECK_INTS; */
	scm_iprin1 (SCM_CAR (exp), port, pstate);
      }
  }
  if (SCM_NNULLP (exp))
    {
      scm_gen_puts (scm_regular_string, " . ", port);
      scm_iprin1 (exp, port, pstate);
    }
skip_tail:
  pstate->list_offset -= pstate->top - floor - 2;
  goto end;

fancy_circref:
  pstate->list_offset -= pstate->top - floor - 2;
  
circref:
  scm_gen_puts (scm_regular_string, " . ", port);
  print_circref (port, pstate, exp);
  goto end;
}



SCM_PROC(s_write, "write", 1, 1, 0, scm_write);
#ifdef __STDC__
SCM 
scm_write (SCM obj, SCM port)
#else
SCM 
scm_write (obj, port)
     SCM obj;
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG2, s_write);
  scm_prin1 (obj, port, 1);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_display, "display", 1, 1, 0, scm_display);
#ifdef __STDC__
SCM 
scm_display (SCM obj, SCM port)
#else
SCM 
scm_display (obj, port)
     SCM obj;
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG2, s_display);
  scm_prin1 (obj, port, 0);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_newline, "newline", 0, 1, 0, scm_newline);
#ifdef __STDC__
SCM
scm_newline(SCM port)
#else
SCM 
scm_newline (port)
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
 port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_newline);
  scm_gen_putc ('\n', port);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
  else
# endif
#endif
  if (port == scm_cur_outp)
    scm_fflush (port);
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_write_char, "write-char", 1, 1, 0, scm_write_char);
#ifdef __STDC__
SCM 
scm_write_char (SCM chr, SCM port)
#else
SCM 
scm_write_char (chr, port)
     SCM chr;
     SCM port;
#endif
{
  if (SCM_UNBNDP (port))
 port = scm_cur_outp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG2, s_write_char);
  SCM_ASSERT (SCM_ICHRP (chr), chr, SCM_ARG1, s_write_char);
  scm_gen_putc ((int) SCM_ICHR (chr), port);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}




#ifdef __STDC__
void
scm_init_print (void)
#else
void
scm_init_print ()
#endif
{
  SCM vtable, type;
  scm_init_opts (scm_print_options, scm_print_opts, SCM_N_PRINT_OPTIONS);
  vtable = scm_make_vtable_vtable (scm_make_struct_layout (scm_makfrom0str ("")), SCM_INUM0, SCM_EOL);
  type = scm_make_struct (vtable,
			  SCM_INUM0,
			  scm_cons (scm_make_struct_layout (scm_makfrom0str (SCM_PRINT_STATE_LAYOUT)),
				    SCM_EOL));
  print_state_pool = scm_permanent_object (scm_cons (type, SCM_EOL));
#include "print.x"
}
