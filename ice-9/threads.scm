;;;; 	Copyright (C) 1996 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;;
;;;; ----------------------------------------------------------------
;;;; threads.scm -- User-level interface to Guile's thread system
;;;; 4 March 1996, Anthony Green <green@cygnus.com>
;;;; Modified 5 October 1996, MDJ <djurfeldt@nada.kth.se>
;;;; ----------------------------------------------------------------
;;;;


(define-module #/ice-9/threads)



; --- MACROS -------------------------------------------------------

(defmacro-public make-thread (fn . args)
  `(call-with-new-thread
    (lambda ()
      (,fn ,@args))
    (lambda args args)))

(defmacro-public begin-thread (first . thunk)
  `(call-with-new-thread
    (lambda ()
      (begin
	,first ,@thunk))
    (lambda args args)))

(defmacro-public with-mutex (m . thunk)
  `(dynamic-wind
    (lambda () (lock-mutex ,m))
    (lambda () (begin ,@thunk))
    (lambda () (unlock-mutex ,m))))

(defmacro-public monitor (first . thunk)
  `(with-mutex ,(make-mutex)
    (begin
      ,first ,@thunk)))
