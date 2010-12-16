;;;; 	Copyright (C) 1996, 1998, 2001, 2002, 2003, 2006, 2010 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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
  #:use-module (ice-9 futures)
  #:export (begin-thread
            parallel
            letpar
            make-thread
            with-mutex
            monitor

            par-map
            par-for-each
            n-par-map
            n-par-for-each
            n-for-each-par-map
            %thread-handler))



;;; Macros first, so that the procedures expand correctly.

(define-syntax begin-thread
  (syntax-rules ()
    ((_ e0 e1 ...)
     (call-with-new-thread
      (lambda () e0 e1 ...)
      %thread-handler))))

(define-syntax parallel
  (lambda (x)
    (syntax-case x ()
      ((_ e0 ...)
       (with-syntax (((tmp0 ...) (generate-temporaries (syntax (e0 ...)))))
         #'(let ((tmp0 (future e0))
                 ...)
             (values (touch tmp0) ...)))))))

(define-syntax letpar
  (syntax-rules ()
    ((_ ((v e) ...) b0 b1 ...)
     (call-with-values
         (lambda () (parallel e ...))
       (lambda (v ...)
         b0 b1 ...)))))

(define-syntax make-thread
  (syntax-rules ()
    ((_ proc arg ...)
     (call-with-new-thread
      (lambda () (proc arg ...))
      %thread-handler))))

(define-syntax with-mutex
  (syntax-rules ()
    ((_ m e0 e1 ...)
     (let ((x m))
       (dynamic-wind
         (lambda () (lock-mutex x))
         (lambda () (begin e0 e1 ...))
         (lambda () (unlock-mutex x)))))))

(define-syntax monitor
  (syntax-rules ()
    ((_ first rest ...)
     (with-mutex (make-mutex)
       first rest ...))))

(define (par-mapper mapper)
  (lambda (proc . arglists)
    (mapper touch
            (apply map
                   (lambda args
                     (future (apply proc args)))
                   arglists))))

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
	    (cons (begin-thread
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
		  threads)))))

(define (n-par-for-each n proc . arglists)
  (let ((m (make-mutex))
	(threads '()))
    (do ((i 0 (+ 1 i)))
	((= i n)
	 (for-each join-thread threads))
      (set! threads
	    (cons (begin-thread
		   (let loop ()
		     (lock-mutex m)
		     (if (null? (car arglists))
			 (unlock-mutex m)
			 (let ((args (map car arglists)))
			   (set! arglists (map cdr arglists))
			   (unlock-mutex m)
			   (apply proc args)
			   (loop)))))
		  threads)))))

;;; The following procedure is motivated by the common and important
;;; case where a lot of work should be done, (not too much) in parallel,
;;; but the results need to be handled serially (for example when
;;; writing them to a file).
;;;
(define (n-for-each-par-map n s-proc p-proc . arglists)
  "Using N parallel processes, apply S-PROC in serial order on the results
of applying P-PROC on ARGLISTS."
  (let* ((m (make-mutex))
	 (threads '())
	 (no-result '(no-value))
	 (results (make-list (length (car arglists)) no-result))
	 (result results))
    (do ((i 0 (+ 1 i)))
	((= i n)
	 (for-each join-thread threads))
      (set! threads
	    (cons (begin-thread
		   (let loop ()
		     (lock-mutex m)
		     (cond ((null? results)
			    (unlock-mutex m))
			   ((not (eq? (car results) no-result))
			    (let ((arg (car results)))
			      ;; stop others from choosing to process results
			      (set-car! results no-result)
			      (unlock-mutex m)
			      (s-proc arg)
			      (lock-mutex m)
			      (set! results (cdr results))
			      (unlock-mutex m)
			      (loop)))
			   ((null? result)
			    (unlock-mutex m))
			   (else
			    (let ((args (map car arglists))
				  (my-result result))
			      (set! arglists (map cdr arglists))
			      (set! result (cdr result))
			      (unlock-mutex m)
			      (set-car! my-result (apply p-proc args))
			      (loop))))))
		  threads)))))

(define (thread-handler tag . args)
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
(define %thread-handler thread-handler)

;;; threads.scm ends here
