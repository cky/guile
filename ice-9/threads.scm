;;;; 	Copyright (C) 1996, 1998, 2001, 2002 Free Software Foundation, Inc.
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
;;;; As a special exception, the Free Software Foundation gives permission
;;;; for additional uses of the text contained in its release of GUILE.
;;;;
;;;; The exception is that, if you link the GUILE library with other files
;;;; to produce an executable, this does not by itself cause the
;;;; resulting executable to be covered by the GNU General Public License.
;;;; Your use of that executable is in no way restricted on account of
;;;; linking the GUILE library code into it.
;;;;
;;;; This exception does not however invalidate any other reasons why
;;;; the executable file might be covered by the GNU General Public License.
;;;;
;;;; This exception applies only to the code released by the
;;;; Free Software Foundation under the name GUILE.  If you copy
;;;; code from other Free Software Foundation releases into a copy of
;;;; GUILE, as the General Public License permits, the exception does
;;;; not apply to the code that you add in this way.  To avoid misleading
;;;; anyone as to the status of such modified files, you must delete
;;;; this exception notice from them.
;;;;
;;;; If you write modifications of your own for GUILE, it is your choice
;;;; whether to permit this exception to apply to your modifications.
;;;; If you do not wish that, delete this exception notice.
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

(define-module (ice-9 threads)
  :export (future-ref
	   par-map
	   par-for-each
	   n-par-map
	   n-par-for-each)
  :export-syntax (begin-thread
		  future
		  parallel
		  letpar
		  make-thread
		  with-mutex
		  monitor))



(define future-ref join-thread)

(define ((par-mapper mapper)  proc . arglists)
  (mapper join-thread
	  (apply map
		 (lambda args
		   (call-with-new-thread (lambda ()
					   (apply proc args))
					 %thread-handler))
		 arglists)))

(define par-map (par-mapper map))
(define par-for-each (par-mapper for-each))

(define (n-par-map n proc . arglists)
  (let* ((m (make-mutex))
	 (threads '())
	 (results (make-list (length (car arglists))))
	 (result results))
    (do ((i 0 (+ 1 i)))
	((= i n)
	 (for-each join-thread threads)
	 results)
      (set! threads
	    (cons (call-with-new-thread
		   (lambda ()
		     (let loop ()
		       (lock-mutex m)
		       (if (null? result)
			   (unlock-mutex m)
			   (let ((args (map car arglists))
				 (my-result result))
			     (set! arglists (map cdr arglists))
			     (set! result (cdr result))
			     (unlock-mutex m)
			     (set-car! my-result (apply proc args))
			     (loop)))))
		   %thread-handler)
		  threads)))))

(define (n-par-for-each n proc . arglists)
  (let ((m (make-mutex))
	(threads '()))
    (do ((i 0 (+ 1 i)))
	((= i n)
	 (for-each join-thread threads))
      (set! threads
	    (cons (call-with-new-thread
		   (lambda ()
		     (let loop ()
		       (lock-mutex m)
		       (if (null? (car arglists))
			   (unlock-mutex m)
			   (let ((args (map car arglists)))
			     (set! arglists (map cdr arglists))
			     (unlock-mutex m)
			     (apply proc args)
			     (loop)))))
		   %thread-handler)
		  threads)))))

(define (thread-handler tag . args)
  (fluid-set! the-last-stack #f)
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
          (newline p)))
    #f))

;;; Set system thread handler
(set! %thread-handler thread-handler)

; --- MACROS -------------------------------------------------------

(define-macro (begin-thread . forms)
  (if (null? forms)
      '(begin)
      `(call-with-new-thread
	(lambda ()
	  ,@forms)
	%thread-handler)))

(define-macro (parallel . forms)
  (cond ((null? forms) '(begin))
	((null? (cdr forms)) (car forms))
	(else
	 `(apply values
		 (map future-ref
		      (list ,@(map (lambda (form) `(future ,form)) forms)))))))

(define-macro (letpar bindings . body)
  `(call-with-values
       (lambda ()
	 (parallel ,@(map cadr bindings)))
     (lambda ,(map car bindings)
       ,@body)))

(define-macro (make-thread proc . args)
  `(call-with-new-thread
    (lambda ()
      (,proc ,@args))
    %thread-handler))

(define-macro (with-mutex m . body)
  `(dynamic-wind
       (lambda () (lock-mutex ,m))
       (lambda () (begin ,@body))
       (lambda () (unlock-mutex ,m))))

(define-macro (monitor first . rest)
  `(with-mutex ,(make-mutex)
     (begin
       ,first ,@rest)))

;;; threads.scm ends here
