/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2006, 2008, 2009 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/procprop.h"
#include "libguile/root.h"

#include "libguile/gsubr.h"
#include "libguile/deprecation.h"

#include "libguile/private-options.h"

/*
 * gsubr.c
 * Provide `gsubrs' -- subrs taking a prescribed number of required, optional,
 * and rest arguments.
 */

/* #define GSUBR_TEST */

SCM_GLOBAL_SYMBOL (scm_sym_name, "name");

static SCM
create_gsubr (int define, const char *name,
	      unsigned int req, unsigned int opt, unsigned int rst,
	      SCM (*fcn) ())
{
  SCM subr;

  switch (SCM_GSUBR_MAKTYPE (req, opt, rst))
    {
    case SCM_GSUBR_MAKTYPE(0, 0, 0):
      subr = scm_c_make_subr (name, scm_tc7_subr_0, fcn);
      break;
    case SCM_GSUBR_MAKTYPE(1, 0, 0):
      subr = scm_c_make_subr (name, scm_tc7_subr_1, fcn);
      break;
    case SCM_GSUBR_MAKTYPE(0, 1, 0):
      subr = scm_c_make_subr (name, scm_tc7_subr_1o, fcn);
      break;
    case SCM_GSUBR_MAKTYPE(1, 1, 0):
      subr = scm_c_make_subr (name, scm_tc7_subr_2o, fcn);
      break;
    case SCM_GSUBR_MAKTYPE(2, 0, 0):
      subr = scm_c_make_subr (name, scm_tc7_subr_2, fcn);
      break;
    case SCM_GSUBR_MAKTYPE(3, 0, 0):
      subr = scm_c_make_subr (name, scm_tc7_subr_3, fcn);
      break;
    case SCM_GSUBR_MAKTYPE(0, 0, 1):
      subr = scm_c_make_subr (name, scm_tc7_lsubr, fcn);
      break;
    case SCM_GSUBR_MAKTYPE(2, 0, 1):
      subr = scm_c_make_subr (name, scm_tc7_lsubr_2, fcn);
      break;
    default:
      {
	unsigned type;

	type = SCM_GSUBR_MAKTYPE (req, opt, rst);
	if (SCM_GSUBR_REQ (type) != req
	    || SCM_GSUBR_OPT (type) != opt
	    || SCM_GSUBR_REST (type) != rst)
	  scm_out_of_range ("create_gsubr", scm_from_uint (req + opt + rst));

	subr = scm_c_make_subr (name, scm_tc7_gsubr | (type << 8U),
				fcn);
      }
    }

  if (define)
    scm_define (SCM_SNAME (subr), subr);

  return subr;
}

SCM
scm_c_make_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  return create_gsubr (0, name, req, opt, rst, fcn);
}

SCM
scm_c_define_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  return create_gsubr (1, name, req, opt, rst, fcn);
}

static SCM
create_gsubr_with_generic (int define,
			   const char *name,
			   int req,
			   int opt,
			   int rst,
			   SCM (*fcn)(),
			   SCM *gf)
{
  SCM subr;

  switch (SCM_GSUBR_MAKTYPE(req, opt, rst))
    {
    case SCM_GSUBR_MAKTYPE(0, 0, 0):
      subr = scm_c_make_subr_with_generic (name, scm_tc7_subr_0, fcn, gf);
      goto create_subr;
    case SCM_GSUBR_MAKTYPE(1, 0, 0):
      subr = scm_c_make_subr_with_generic (name, scm_tc7_subr_1, fcn, gf);
      goto create_subr;
    case SCM_GSUBR_MAKTYPE(0, 1, 0):
      subr = scm_c_make_subr_with_generic (name, scm_tc7_subr_1o, fcn, gf);
      goto create_subr;
    case SCM_GSUBR_MAKTYPE(1, 1, 0):
      subr = scm_c_make_subr_with_generic (name, scm_tc7_subr_2o, fcn, gf);
      goto create_subr;
    case SCM_GSUBR_MAKTYPE(2, 0, 0):
      subr = scm_c_make_subr_with_generic (name, scm_tc7_subr_2, fcn, gf);
      goto create_subr;
    case SCM_GSUBR_MAKTYPE(3, 0, 0):
      subr = scm_c_make_subr_with_generic (name, scm_tc7_subr_3, fcn, gf);
      goto create_subr;
    case SCM_GSUBR_MAKTYPE(0, 0, 1):
      subr = scm_c_make_subr_with_generic (name, scm_tc7_lsubr, fcn, gf);
      goto create_subr;
    case SCM_GSUBR_MAKTYPE(2, 0, 1):
      subr = scm_c_make_subr_with_generic (name, scm_tc7_lsubr_2, fcn, gf);
    create_subr:
      if (define)
	scm_define (SCM_SNAME (subr), subr);
      return subr;
    default:
      ;
    }
  scm_misc_error ("scm_c_make_gsubr_with_generic",
		  "can't make primitive-generic with this arity",
		  SCM_EOL);
  return SCM_BOOL_F; /* never reached */
}

SCM
scm_c_make_gsubr_with_generic (const char *name,
			       int req,
			       int opt,
			       int rst,
			       SCM (*fcn)(),
			       SCM *gf)
{
  return create_gsubr_with_generic (0, name, req, opt, rst, fcn, gf);
}

SCM
scm_c_define_gsubr_with_generic (const char *name,
				 int req,
				 int opt,
				 int rst,
				 SCM (*fcn)(),
				 SCM *gf)
{
  return create_gsubr_with_generic (1, name, req, opt, rst, fcn, gf);
}


SCM
scm_gsubr_apply (SCM args)
#define FUNC_NAME "scm_gsubr_apply"
{
  SCM self = SCM_CAR (args);
  SCM (*fcn)() = SCM_SUBRF (self);
  SCM v[SCM_GSUBR_MAX];
  unsigned int typ = SCM_GSUBR_TYPE (self);
  long i, n = SCM_GSUBR_REQ (typ) + SCM_GSUBR_OPT (typ) + SCM_GSUBR_REST (typ);

  args = SCM_CDR (args);
  for (i = 0; i < SCM_GSUBR_REQ (typ); i++) {
    if (scm_is_null (args))
      scm_wrong_num_args (SCM_SNAME (self));
    v[i] = SCM_CAR(args);
    args = SCM_CDR(args);
  }
  for (; i < SCM_GSUBR_REQ (typ) + SCM_GSUBR_OPT (typ); i++) {
    if (SCM_NIMP (args)) {
      v[i] = SCM_CAR (args);
      args = SCM_CDR(args);
    }
    else
      v[i] = SCM_UNDEFINED;
  }
  if (SCM_GSUBR_REST(typ))
    v[i] = args;
  else if (!scm_is_null (args))
    scm_wrong_num_args (SCM_SNAME (self));
  switch (n) {
  case 2: return (*fcn)(v[0], v[1]);
  case 3: return (*fcn)(v[0], v[1], v[2]);
  case 4: return (*fcn)(v[0], v[1], v[2], v[3]);
  case 5: return (*fcn)(v[0], v[1], v[2], v[3], v[4]);
  case 6: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5]);
  case 7: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6]);
  case 8: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
  case 9: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]);
  case 10: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
  default:
    scm_misc_error ((char *) SCM_SNAME (self),
		    "gsubr invocation with more than 10 arguments not implemented",
		    SCM_EOL);
  }
  return SCM_BOOL_F; /* Never reached. */
}
#undef FUNC_NAME


#ifdef GSUBR_TEST
/* A silly example, taking 2 required args, 1 optional, and
   a scm_list of rest args
   */
SCM
gsubr_21l(SCM req1, SCM req2, SCM opt, SCM rst)
{
  scm_puts ("gsubr-2-1-l:\n req1: ", scm_cur_outp);
  scm_display(req1, scm_cur_outp);
  scm_puts ("\n req2: ", scm_cur_outp);
  scm_display(req2, scm_cur_outp);
  scm_puts ("\n opt: ", scm_cur_outp);
  scm_display(opt, scm_cur_outp);
  scm_puts ("\n rest: ", scm_cur_outp);
  scm_display(rst, scm_cur_outp);
  scm_newline(scm_cur_outp);
  return SCM_UNSPECIFIED;
}
#endif


void
scm_init_gsubr()
{
#ifdef GSUBR_TEST
  scm_c_define_gsubr ("gsubr-2-1-l", 2, 1, 1, gsubr_21l); /* example */
#endif

#include "libguile/gsubr.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
