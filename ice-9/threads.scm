;;;; 	Copyright (C) 1996, 1998, 2001 Free Software Foundation, Inc.
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
;;;; Modified 6 April 2001, ttn
;;;; ----------------------------------------------------------------
;;;;

;;; Commentary:

;; This module is documented in the Guile Reference Manual.
;; Briefly, one procedure is exported: `%thread-handler';
;; as well as four macros: `make-thread', `begin-thread',
;; `with-mutex' and `monitor'.

;;; Code:

(define-module (ice-9 threads))



(define (%thread-handler tag . args)
  (fluid-set! the-last-stack #f)
  (unmask-signals)
  (let ((n (length args))
	(p (current-error-port)))
    (display "In thread:" p)
    (newline p)
    (if (>= n 3)
        (display-error #f
                       p
                       (car args)
                       (cadr args)
                       (caddr args)
                       (if (= n 4)
                           (cadddr args)
                           '()))
        (begin
          (display "uncaught throw to " p)
          (display tag p)
          (display ": " p)
          (display args p)
          (newline p)))))

; --- MACROS -------------------------------------------------------

(defmacro make-thread (proc . args)
  `(call-with-new-thread
    (lambda ()
      (,proc ,@args))
    %thread-handler))

(defmacro begin-thread (first . rest)
  `(call-with-new-thread
    (lambda ()
      (begin
	,first ,@rest))
    %thread-handler))

(defmacro with-mutex (m . body)
  `(dynamic-wind
       (lambda () (lock-mutex ,m))
       (lambda () (begin ,@body))
       (lambda () (unlock-mutex ,m))))

(defmacro monitor (first . rest)
  `(with-mutex ,(make-mutex)
     (begin
       ,first ,@rest)))

;; export

(export %thread-handler)

(export-syntax make-thread
               begin-thread
               with-mutex
               monitor)

;;; threads.scm ends here
