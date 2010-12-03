/* Copyright (C) 2010 Free Software Foundation, Inc.
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




#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/bytevectors.h"
#include "libguile/numbers.h"
#include "libguile/error.h"
#include "libguile/validate.h"

#include "libguile/poll.h"


#ifdef HAVE_POLL_H
#include <poll.h>
#endif



/* {Poll}
 */

/* Poll a set of file descriptors, waiting until one or more of them is
   ready to perform input or output.

   This is a low-level interface.  See the `(ice-9 poll)' module for a more
   usable wrapper.

   `pollfds' is expected to be a bytevector, laid out in contiguous blocks of 64
   bits.  Each block has the format of one `struct pollfd': a 32-bit int file
   descriptor, a 16-bit int events mask, and a 16-bit int revents mask.

   The number of pollfd structures in `pollfds' is specified in
   `nfds'. `pollfds' must be at least long enough to support that number of
   structures. It may be longer, in which case the trailing entries are left
   untouched.

   The pollfds bytevector is modified directly, setting the returned events in
   the final two bytes (the revents member).

   If timeout is given and is non-negative, the poll will return after that
   number of milliseconds if no fd became active.
   */
#ifdef HAVE_POLL
static SCM
scm_primitive_poll (SCM pollfds, SCM nfds, SCM timeout)
#define FUNC_NAME "primitive-poll"
{
  int rv;
  nfds_t c_nfds;
  int c_timeout;
  struct pollfd *fds;

  SCM_VALIDATE_BYTEVECTOR (SCM_ARG1, pollfds);
  c_nfds = scm_to_uint32 (nfds);
  c_timeout = scm_to_int (timeout);
  
  if (SCM_UNLIKELY (SCM_BYTEVECTOR_LENGTH (pollfds)
                    < c_nfds * sizeof(struct pollfd)))
    SCM_OUT_OF_RANGE (SCM_ARG1, nfds);
  
  fds = (struct pollfd*)SCM_BYTEVECTOR_CONTENTS (pollfds);
  
  SCM_SYSCALL (rv = poll (fds, c_nfds, c_timeout));

  if (rv == -1)
    SCM_SYSERROR;

  return scm_from_int (rv);
}
#undef FUNC_NAME
#endif /* HAVE_POLL */




static void
scm_init_poll (void)
{
#if HAVE_POLL
  scm_c_define_gsubr ("primitive-poll", 3, 0, 0, scm_primitive_poll);
#else
  scm_misc_error ("%init-poll", "`poll' unavailable on this platform", SCM_EOL);
#endif

#ifdef POLLIN
  scm_c_define ("POLLIN", scm_from_int (POLLIN));
#endif 	       
#ifdef POLLPRI
  scm_c_define ("POLLPRI", scm_from_int (POLLPRI));
#endif 	       
#ifdef POLLOUT
  scm_c_define ("POLLOUT", scm_from_int (POLLOUT));
#endif 	       
#ifdef POLLRDHUP
  scm_c_define ("POLLRDHUP", scm_from_int (POLLRDHUP));
#endif 	       
#ifdef POLLERR
  scm_c_define ("POLLERR", scm_from_int (POLLERR));
#endif 	       
#ifdef POLLHUP
  scm_c_define ("POLLHUP", scm_from_int (POLLHUP));
#endif 	       
#ifdef POLLNVAL
  scm_c_define ("POLLNVAL", scm_from_int (POLLNVAL));
#endif 	       

}

void
scm_register_poll (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_poll",
			    (scm_t_extension_init_func) scm_init_poll,
			    NULL);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
