;;;; benchmark-suite/lib.scm --- generic support for benchmarking
;;;; Copyright (C) 2002 Free Software Foundation, Inc.
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

(define-module (benchmark-suite lib)
  :export (

 ;; Controlling the execution.
 iteration-factor
 scale-iterations

 ;; Running benchmarks.
 run-benchmark
 benchmark

 ;; Naming groups of benchmarks in a regular fashion.
 with-benchmark-prefix with-benchmark-prefix* current-benchmark-prefix
 format-benchmark-name

 ;; Reporting results in various ways.
 register-reporter unregister-reporter reporter-registered?
 make-log-reporter
 full-reporter
 user-reporter))

;;;; MISCELLANEOUS
;;;;

;;; Scale the number of iterations according to the given scaling factor.
(define iteration-factor 1)
(define (scale-iterations iterations)
  (let* ((i (inexact->exact (round (* iterations iteration-factor)))))
    (if (< i 1) 1 i)))

;;;; CORE FUNCTIONS
;;;;

;;; The central routine for executing benchmarks.
;;; The idea is taken from Greg, the GNUstep regression test environment.
(define run-benchmark #f)
(let ((benchmark-running #f))
  (define (local-run-benchmark name iterations thunk)
    (if benchmark-running
	(error "Nested calls to run-benchmark are not permitted.")
	(let ((benchmark-name (full-name name))
	      (iterations (scale-iterations iterations)))
	  (set! benchmark-running #t)
	  (let ((before #f) (after #f) (gc-time #f))
	    (gc)
	    (set! gc-time (gc-run-time))
	    (set! before (times))
	    (do ((i 0 (+ i 1)))
		((= i iterations))
	      (thunk))
	    (set! after (times))
	    (set! gc-time (- (gc-run-time) gc-time))
	    (report benchmark-name iterations before after gc-time))
	  (set! benchmark-running #f))))
  (set! run-benchmark local-run-benchmark))

;;; A short form for benchmarks.
(defmacro benchmark (name iterations body . rest)
  `(,run-benchmark ,name ,iterations (lambda () ,body ,@rest)))


;;;; BENCHMARK NAMES
;;;;

;;;; Turn a benchmark name into a nice human-readable string.
(define (format-benchmark-name name)
  (call-with-output-string
   (lambda (port)
     (let loop ((name name)
		(separator ""))
       (if (pair? name)
	   (begin
	     (display separator port)
	     (display (car name) port)
	     (loop (cdr name) ": ")))))))

;;;; For a given benchmark-name, deliver the full name including all prefixes.
(define (full-name name)
  (append (current-benchmark-prefix) (list name)))

;;; A fluid containing the current benchmark prefix, as a list.
(define prefix-fluid (make-fluid))
(fluid-set! prefix-fluid '())
(define (current-benchmark-prefix)
  (fluid-ref prefix-fluid))

;;; Postpend PREFIX to the current name prefix while evaluting THUNK.
;;; The name prefix is only changed within the dynamic scope of the
;;; call to with-benchmark-prefix*.  Return the value returned by THUNK.
(define (with-benchmark-prefix* prefix thunk)
  (with-fluids ((prefix-fluid
		 (append (fluid-ref prefix-fluid) (list prefix))))
    (thunk)))

;;; (with-benchmark-prefix PREFIX BODY ...)
;;; Postpend PREFIX to the current name prefix while evaluating BODY ...
;;; The name prefix is only changed within the dynamic scope of the
;;; with-benchmark-prefix expression.  Return the value returned by the last
;;; BODY expression.
(defmacro with-benchmark-prefix (prefix . body)
  `(with-benchmark-prefix* ,prefix (lambda () ,@body)))


;;;; TIME CALCULATION
;;;;

(define time-base 
  internal-time-units-per-second)

(define frame-time/iteration
  "<will be set during initialization>")

(define (total-time before after)
  (- (tms:clock after) (tms:clock before)))

(define (user-time before after)
  (- (tms:utime after) (tms:utime before)))

(define (system-time before after)
  (- (tms:stime after) (tms:stime before)))

(define (frame-time iterations)
  (* iterations frame-time/iteration))

(define (benchmark-time iterations before after)
  (- (user-time before after) (frame-time iterations)))

(define (user-time\interpreter before after gc-time)
  (- (user-time before after) gc-time))

(define (benchmark-time\interpreter iterations before after gc-time)
  (- (benchmark-time iterations before after) gc-time))


;;;; REPORTERS
;;;;

;;; The global list of reporters.
(define reporters '())

;;; The default reporter, to be used only if no others exist.
(define default-reporter #f)

;;; Add the procedure REPORTER to the current set of reporter functions.
;;; Signal an error if that reporter procedure object is already registered.
(define (register-reporter reporter)
  (if (memq reporter reporters)
      (error "register-reporter: reporter already registered: " reporter))
  (set! reporters (cons reporter reporters)))

;;; Remove the procedure REPORTER from the current set of reporter
;;; functions.  Signal an error if REPORTER is not currently registered.
(define (unregister-reporter reporter)
  (if (memq reporter reporters)
      (set! reporters (delq! reporter reporters))
      (error "unregister-reporter: reporter not registered: " reporter)))

;;; Return true iff REPORTER is in the current set of reporter functions.
(define (reporter-registered? reporter)
  (if (memq reporter reporters) #t #f))

;;; Send RESULT to all currently registered reporter functions.
(define (report . args)
  (if (pair? reporters)
      (for-each (lambda (reporter) (apply reporter args))
		reporters)
      (apply default-reporter args)))


;;;; Some useful standard reporters:
;;;; Log reporters write all test results to a given log file.
;;;; Full reporters write all benchmark results to the standard output.
;;;; User reporters write some interesting results to the standard output.

;;; Display a single benchmark result to the given port
(define (print-result port name iterations before after gc-time)
  (let* ((name (format-benchmark-name name))
	 (total-time (total-time before after))
	 (user-time (user-time before after))
	 (system-time (system-time before after))
	 (frame-time (frame-time iterations))
	 (benchmark-time (benchmark-time iterations before after))
	 (user-time\interpreter (user-time\interpreter before after gc-time))
	 (benchmark-time\interpreter 
	  (benchmark-time\interpreter iterations before after gc-time)))
    (write (list name iterations
		 "total:" (/ total-time time-base)
		 "user:" (/ user-time time-base)
		 "system:" (/ system-time time-base)
		 "frame:" (/ frame-time time-base)
		 "benchmark:" (/ benchmark-time time-base)
		 "user/interp:" (/ user-time\interpreter time-base)
		 "bench/interp:" (/ benchmark-time\interpreter time-base)
		 "gc:" (/ gc-time time-base))
	   port)
    (newline port)))

;;; Return a reporter procedure which prints all results to the file
;;; FILE, in human-readable form.  FILE may be a filename, or a port.
(define (make-log-reporter file)
  (let ((port (if (output-port? file) file
		  (open-output-file file))))
    (lambda args
      (apply print-result port args)
      (force-output port))))

;;; A reporter that reports all results to the user.
(define (full-reporter . args)
  (apply print-result (current-output-port) args))

;;; Display interesting results of a single benchmark to the given port
(define (print-user-result port name iterations before after gc-time)
  (let* ((name (format-benchmark-name name))
	 (user-time (user-time before after))
	 (benchmark-time (benchmark-time iterations before after))
	 (benchmark-time\interpreter
	  (benchmark-time\interpreter iterations before after gc-time)))
    (write (list name iterations 
		 "user:" (/ user-time time-base)
		 "benchmark:" (/ benchmark-time time-base)
		 "bench/interp:" (/ benchmark-time\interpreter time-base)
		 "gc:" (/ gc-time time-base))
	   port)
    (newline port)))

;;; A reporter that reports interesting results to the user.
(define (user-reporter . args)
  (apply print-user-result (current-output-port) args))


;;;; Initialize the benchmarking system:
;;;;

;;; First, make sure the benchmarking routines are compiled.
(define (null-reporter . args) #t)
(set! default-reporter null-reporter)
(benchmark "empty initialization benchmark" 2 #t)

;;; Second, initialize the system constants
(define (initialization-reporter name iterations before after gc-time)
  (let* ((frame-time (- (tms:utime after) (tms:utime before) gc-time 3)))
    (set! frame-time/iteration (/ frame-time iterations))
    (display ";; frame time per iteration: " (current-output-port))
    (display (/ frame-time/iteration time-base) (current-output-port))
    (newline (current-output-port))))
(set! default-reporter initialization-reporter)
(benchmark "empty initialization benchmark" 524288 #t)

;;; Finally, set the default reporter
(set! default-reporter user-reporter)
