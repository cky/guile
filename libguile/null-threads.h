/* classes: h_files */

#ifndef SCM_NULL_THREADS_H
#define SCM_NULL_THREADS_H

/* Copyright (C) 2002 Free Software Foundation, Inc.
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



/* The null-threads implementation.  We provide the standard API, but
   no new threads can be created.
*/

#error temporarily broken, compile with threads enabled (default option)

/* We can't switch so don't bother trying. 
*/
#undef  SCM_THREAD_SWITCHING_CODE
#define SCM_THREAD_SWITCHING_CODE

#define scm_t_thread int

/* The "(void)(...)" constructs in the expansions are there to ensure
   that the side effects of the argument expressions take place.
*/

#define scm_thread_create(th,proc,data) ((void)(proc), (void)(data), ENOTSUP)
#define scm_thread_join(th)             do { (void)(th); abort(); } while(0)
#define scm_thread_detach(th)           do { (void)(th); abort(); } while(0)
#define scm_thread_self()               0

#define scm_t_mutex int

#define scm_mutex_init(mx)              do { (void)(mx); } while(0)
#define scm_mutex_destroy(mx)           do { (void)(mx); } while(0)
#define scm_mutex_lock(mx)              do { (void)(mx); } while(0)
#define scm_mutex_trylock(mx)           ((void)(mx), 1)
#define scm_mutex_unlock(mx)            do { (void)(mx); } while(0)

#define scm_t_cond int

#define scm_cond_init(cv)               do { (void)(cv); } while(0)
#define scm_cond_destroy(cv)            do { (void)(cv); } while(0)
#define scm_cond_wait(cv,mx)            ((void)(cv), (void)(mx), ENOTSUP)
#define scm_cond_timedwait(cv,mx,at)    ((void)(cv), (void)(mx), (void)(at), \
                                         ENOTSUP)
#define scm_cond_signal(cv)             do { (void)(cv); } while(0)
#define scm_cond_broadcast(cv)          do { (void)(cv); } while(0)

#define scm_thread_select               select

typedef void **scm_t_key;

#define scm_key_create(keyp)            do { *(keyp) = malloc(sizeof(void*)); \
                                           } while(0)
#define scm_key_delete(key)             do { free(key); } while(0)
#define scm_key_setspecific(key, value) do { *(key) = (value); } while(0)
#define scm_key_getspecific(key)        *(key)

#if 0

/* These are the actual prototypes of the functions/macros defined
   above.  We list them here for reference. */

typedef int scm_t_thread;

SCM_API int  scm_thread_create (scm_t_thread *th,
				void (*proc) (void *), void *data);
SCM_API void scm_thread_join (scm_t_thread th);
SCM_API void scm_thread_detach (scm_t_thread th);
SCM_API scm_t_thread scm_thread_self (void);

typedef int scm_t_mutex;

SCM_API void scm_mutex_init (scm_t_mutex *mx);
SCM_API void scm_mutex_destroy (scm_t_mutex *mx);
SCM_API void scm_mutex_lock (scm_t_mutex *mx);
SCM_API int  scm_mutex_trylock (scm_t_mutex *mx);
SCM_API void scm_mutex_unlock (scm_t_mutex *mx);

typedef int scm_t_cond;

SCM_API void scm_cond_init (scm_t_cond *cv);
SCM_API void scm_cond_destroy (scm_t_cond *cv);
SCM_API void scm_cond_wait (scm_t_cond *cv, scm_t_mutex *mx);
SCM_API int  scm_cond_timedwait (scm_t_cond *cv, scm_t_mutex *mx,
				 scm_t_timespec *abstime);
SCM_API void scm_cond_signal (scm_t_cond *cv);
SCM_API void scm_cond_broadcast (scm_t_cond *cv);

typedef int scm_t_key;

SCM_API void scm_key_create (scm_t_key *keyp);
SCM_API void scm_key_delete (scm_t_key key);
SCM_API void scm_key_setspecific (scm_t_key key, const void *value);
SCM_API void *scm_key_getspecific (scm_t_key key);

SCM_API int  scm_thread_select (int nfds,
				SELECT_TYPE *readfds,
				SELECT_TYPE *writefds,
				SELECT_TYPE *exceptfds,
				struct timeval *timeout);

#endif

#endif  /* SCM_NULL_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
