;;;; benchmark-suite/lib.scm --- generic support for benchmarking
;;;; Copyright (C) 2002, 2006, 2011, 2012 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.LESSER.
;;;; If not, write to the Free Software Foundation, Inc., 51 Franklin
;;;; Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (benchmark-suite lib)
  #:use-module (srfi srfi-9)
  #:export (;; Controlling the execution.
            iteration-factor
            scale-iterations

            ;; Running benchmarks.
            run-benchmark
            benchmark

            ;; Naming groups of benchmarks in a regular fashion.
            with-benchmark-prefix with-benchmark-prefix*
            current-benchmark-prefix format-benchmark-name

            ;; <benchmark-result> accessors
            benchmark-result:name
            benchmark-result:iterations
            benchmark-result:real-time
            benchmark-result:run-time
            benchmark-result:gc-time
            benchmark-result:core-time

            ;; Reporting results in various ways.
            report current-reporter
            register-reporter unregister-reporter reporter-registered?
            make-log-reporter
            full-reporter
            user-reporter))


;;;; If you're using Emacs's Scheme mode:
;;;;   (put 'with-benchmark-prefix 'scheme-indent-function 1)
;;;;   (put 'benchmark 'scheme-indent-function 1)


;;;; CORE FUNCTIONS
;;;;
;;;; The function (run-benchmark name iterations thunk) is the heart of the
;;;; benchmarking environment.  The first parameter NAME is a unique name for
;;;; the benchmark to be executed (for an explanation of this parameter see
;;;; below under ;;;; NAMES.  The second parameter ITERATIONS is a positive
;;;; integer value that indicates how often the thunk shall be executed (for
;;;; an explanation of how iteration counts should be used, see below under
;;;; ;;;; ITERATION COUNTS).  For example:
;;;;
;;;;    (run-benchmark "small integer addition" 100000 (lambda () (+ 1 1)))
;;;;
;;;; This will run the function (lambda () (+ 1 1)) a 100000 times (the
;;;; iteration count can, however be scaled.  See below for details).  Some
;;;; different time data for running the thunk for the given number of
;;;; iterations is measured and reported.
;;;;
;;;; Convenience macro
;;;;
;;;; * (benchmark name iterations body) is a short form for
;;;;   (run-benchmark name iterations (lambda () body))


;;;; NAMES
;;;;
;;;; Every benchmark in the benchmark suite has a unique name to be able to
;;;; compare the results of individual benchmarks across several runs of the
;;;; benchmark suite.
;;;;
;;;; A benchmark name is a list of printable objects.  For example:
;;;; ("ports.scm" "file" "read and write back list of strings")
;;;; ("ports.scm" "pipe" "read")
;;;;
;;;; Benchmark names may contain arbitrary objects, but they always have
;;;; the following properties:
;;;; - Benchmark names can be compared with EQUAL?.
;;;; - Benchmark names can be reliably stored and retrieved with the standard
;;;;   WRITE and READ procedures; doing so preserves their identity.
;;;;
;;;; For example:
;;;;
;;;;    (benchmark "simple addition" 100000 (+ 2 2))
;;;;
;;;; In that case, the benchmark name is the list ("simple addition").
;;;;
;;;; The WITH-BENCHMARK-PREFIX syntax and WITH-BENCHMARK-PREFIX* procedure
;;;; establish a prefix for the names of all benchmarks whose results are
;;;; reported within their dynamic scope.  For example:
;;;;
;;;; (begin
;;;;   (with-benchmark-prefix "basic arithmetic"
;;;;     (benchmark "addition" 100000 (+ 2 2))
;;;;     (benchmark "subtraction" 100000 (- 4 2)))
;;;;   (benchmark "multiplication" 100000 (* 2 2))))
;;;;
;;;; In that example, the three benchmark names are:
;;;;   ("basic arithmetic" "addition"),
;;;;   ("basic arithmetic" "subtraction"), and
;;;;   ("multiplication").
;;;;
;;;; WITH-BENCHMARK-PREFIX can be nested.  Each WITH-BENCHMARK-PREFIX
;;;; appends a new element to the current prefix:
;;;;
;;;; (with-benchmark-prefix "arithmetic"
;;;;   (with-benchmark-prefix "addition"
;;;;     (benchmark "integer" 100000 (+ 2 2))
;;;;     (benchmark "complex" 100000 (+ 2+3i 4+5i)))
;;;;   (with-benchmark-prefix "subtraction"
;;;;     (benchmark "integer" 100000 (- 2 2))
;;;;     (benchmark "complex" 100000 (- 2+3i 1+2i))))
;;;;
;;;; The four benchmark names here are:
;;;;   ("arithmetic" "addition" "integer")
;;;;   ("arithmetic" "addition" "complex")
;;;;   ("arithmetic" "subtraction" "integer")
;;;;   ("arithmetic" "subtraction" "complex")
;;;;
;;;; To print a name for a human reader, we DISPLAY its elements,
;;;; separated by ": ".  So, the last set of benchmark names would be
;;;; reported as:
;;;;
;;;;   arithmetic: addition: integer
;;;;   arithmetic: addition: complex
;;;;   arithmetic: subtraction: integer
;;;;   arithmetic: subtraction: complex
;;;;
;;;; The Guile benchmarks use with-benchmark-prefix to include the name of
;;;; the source file containing the benchmark in the benchmark name, to
;;;; provide each file with its own namespace.


;;;; ITERATION COUNTS
;;;;
;;;; Every benchmark has to be given an iteration count that indicates how
;;;; often it should be executed.  The reason is, that in most cases a single
;;;; execution of the benchmark code would not deliver usable timing results:
;;;; The resolution of the system time is not arbitrarily fine.  Thus, some
;;;; benchmarks would be executed too quickly to be measured at all.  A rule
;;;; of thumb is, that the longer a benchmark runs, the more exact is the
;;;; information about the execution time.
;;;;
;;;; However, execution time depends on several influences:  First, the
;;;; machine you are running the benchmark on.  Second, the compiler you use.
;;;; Third, which compiler options you use.  Fourth, which version of guile
;;;; you are using.  Fifth, which guile options you are using (for example if
;;;; you are using the debugging evaluator or not).  There are even more
;;;; influences.
;;;;
;;;; For this reason, the same number of iterations for a single benchmark may
;;;; lead to completely different execution times in different
;;;; constellations.  For someone working on a slow machine, the default
;;;; execution counts may lead to an inacceptable execution time of the
;;;; benchmark suite.  For someone on a very fast machine, however, it may be
;;;; desireable to increase the number of iterations in order to increase the
;;;; accuracy of the time data.
;;;;
;;;; For this reason, the benchmark suite allows to scale the number of
;;;; executions by a global factor, stored in the exported variable
;;;; iteration-factor.  The default for iteration-factor is 1.  A number of 2
;;;; means, that all benchmarks are executed twice as often, which will also
;;;; roughly double the execution time for the benchmark suite.  Similarly, if
;;;; iteration-factor holds a value of 0.5, only about half the execution time
;;;; will be required.
;;;;
;;;; It is probably a good idea to choose the iteration count for each
;;;; benchmark such that all benchmarks will take about the same time, for
;;;; example one second.  To achieve this, the benchmark suite holds an empty
;;;; benchmark in the file 0-reference.bm named "reference benchmark for
;;;; iteration counts".  It's iteration count is calibrated to make the
;;;; benchmark run about one second on Dirk's laptop :-)  If you are adding
;;;; benchmarks to the suite, it would be nice if you could calibrate the
;;;; number of iterations such that each of your added benchmarks takes about
;;;; as long to run as the reference benchmark.  But:  Don't be too accurate
;;;; to figure out the correct iteration count.


;;;; REPORTERS
;;;;
;;;; A reporter is a function which we apply to each benchmark outcome.
;;;; Reporters can log results, print interesting results to the standard
;;;; output, collect statistics, etc.
;;;;
;;;; A reporter function takes the following arguments:  NAME ITERATIONS
;;;; BEFORE AFTER GC-TIME.  The argument NAME holds the name of the benchmark,
;;;; ITERATIONS holds the actual number of iterations that were performed.
;;;; BEFORE holds the result of the function (times) at the very beginning of
;;;; the excution of the benchmark, AFTER holds the result of the function
;;;; (times) after the execution of the benchmark.  GC-TIME, finally, holds
;;;; the difference of calls to (gc-run-time) before and after the execution
;;;; of the benchmark.
;;;;
;;;; This library provides some standard reporters for logging results
;;;; to a file, reporting interesting results to the user, (FIXME: and
;;;; collecting totals).
;;;;
;;;; You can use the REGISTER-REPORTER function and friends to add whatever
;;;; reporting functions you like.  See under ;;;; TIMING DATA to see how the
;;;; library helps you to extract relevant timing information from the values
;;;; ITERATIONS, BEFORE, AFTER and GC-TIME.  If you don't register any
;;;; reporters, the library uses USER-REPORTER, which writes the most
;;;; interesting results to the standard output.


;;;; TIME CALCULATION
;;;;
;;;; The library uses the guile functions `get-internal-run-time',
;;;; `get-internal-real-time', and `gc-run-time' to determine the
;;;; execution time for a single benchmark.  Based on these functions,
;;;; Guile makes a <benchmark-result>, a record containing the elapsed
;;;; run time, real time, gc time, and possibly other metrics.  These
;;;; times include the time needed to executed the benchmark code
;;;; itself, but also the surrounding code that implements the loop to
;;;; run the benchmark code for the given number of times.  This is
;;;; undesirable, since one would prefer to only get the timing data for
;;;; the benchmarking code.
;;;;
;;;; To cope with this, the benchmarking framework uses a trick:  During
;;;; initialization of the library, the time for executing an empty
;;;; benchmark is measured and stored.  This is an estimate for the time
;;;; needed by the benchmarking framework itself.  For later benchmarks,
;;;; this time can then be subtracted from the measured execution times.
;;;; Note that for very short benchmarks, this may result in a negative
;;;; number.
;;;;
;;;; The benchmarking framework provides the following accessors for
;;;; <benchmark-result> values.  Note that all time values are in
;;;; internal time units; divide by internal-time-units-per-second to
;;;; get seconds.
;;;;
;;;; benchmark-result:name : Return the name of the benchmark.
;;;;
;;;; benchmark-result:iterations : Return the number of iterations that
;;;;     this benchmark ran for.
;;;;
;;;; benchmark-result:real-time : Return the clock time elapsed while
;;;;     this benchmark executed.
;;;;
;;;; benchmark-result:run-time : Return the CPU time elapsed while this
;;;;     benchmark executed, both in user and kernel space.
;;;;
;;;; benchmark-result:gc-time : Return the approximate amount of time
;;;;     spent in garbage collection while this benchmark executed, both
;;;;     in user and kernel space.
;;;;
;;;; benchmark-result:core-time : Like benchmark-result:run-time, but
;;;;     also estimates the time spent by the framework for the number
;;;;     of iterations, and subtracts off that time from the result.
;;;;

;;;; This module is used when benchmarking different Guiles, and so it
;;;; should run on all the Guiles of interest.  Currently this set
;;;; includes Guile 1.8, so be careful with introducing features that
;;;; only Guile 2.0 supports.


;;;; MISCELLANEOUS
;;;;

(define-record-type <benchmark-result>
  (make-benchmark-result name iterations real-time run-time gc-time)
  benchmark-result?
  (name benchmark-result:name)
  (iterations benchmark-result:iterations)
  (real-time benchmark-result:real-time)
  (run-time benchmark-result:run-time)
  (gc-time benchmark-result:gc-time))

;;; Perform a division and convert the result to inexact.
(define (->seconds time)
  (/ time 1.0 internal-time-units-per-second))

;;; Scale the number of iterations according to the given scaling factor.
(define iteration-factor 1)
(define (scale-iterations iterations)
  (let* ((i (inexact->exact (round (* iterations iteration-factor)))))
    (if (< i 1) 1 i)))

;;; Parameters.
(cond-expand
 (srfi-39 #t)
 (else (use-modules (srfi srfi-39))))

;;;; CORE FUNCTIONS
;;;;

;;; The central routine for executing benchmarks.
;;; The idea is taken from Greg, the GNUstep regression test environment.
(define benchmark-running? (make-parameter #f))
(define (run-benchmark name iterations thunk)
  (if (benchmark-running?)
      (error "Nested calls to run-benchmark are not permitted."))
  (if (not (and (integer? iterations) (exact? iterations)))
      (error "Expected exact integral number of iterations"))
  (parameterize ((benchmark-running? #t))
    ;; Warm up the benchmark first.  This will resolve any toplevel-ref
    ;; forms.
    (thunk)
    (gc)
    (let* ((before-gc-time (gc-run-time))
           (before-real-time (get-internal-real-time))
           (before-run-time (get-internal-run-time)))
      (do ((i iterations (1- i)))
          ((zero? i))
        (thunk))
      (let ((after-run-time (get-internal-run-time))
            (after-real-time (get-internal-real-time))
            (after-gc-time (gc-run-time)))
        (report (make-benchmark-result (full-name name) iterations
                                       (- after-real-time before-real-time)
                                       (- after-run-time before-run-time)
                                       (- after-gc-time before-gc-time)))))))

;;; A short form for benchmarks.
(cond-expand
 (guile-2
  (define-syntax-rule (benchmark name iterations body body* ...)
    (run-benchmark name iterations (lambda () body body* ...))))
 (else
  (defmacro benchmark (name iterations body . rest)
    `(run-benchmark ,name ,iterations (lambda () ,body ,@rest)))))


;;;; BENCHMARK NAMES
;;;;

;;;; Turn a benchmark name into a nice human-readable string.
(define (format-benchmark-name name)
  (string-join name ": "))

;;;; For a given benchmark-name, deliver the full name including all prefixes.
(define (full-name name)
  (append (current-benchmark-prefix) (list name)))

;;; A parameter containing the current benchmark prefix, as a list.
(define current-benchmark-prefix
  (make-parameter '()))

;;; Postpend PREFIX to the current name prefix while evaluting THUNK.
;;; The name prefix is only changed within the dynamic scope of the
;;; call to with-benchmark-prefix*.  Return the value returned by THUNK.
(define (with-benchmark-prefix* prefix thunk)
  (parameterize ((current-benchmark-prefix (full-name prefix)))
    (thunk)))

;;; (with-benchmark-prefix PREFIX BODY ...)
;;; Postpend PREFIX to the current name prefix while evaluating BODY ...
;;; The name prefix is only changed within the dynamic scope of the
;;; with-benchmark-prefix expression.  Return the value returned by the last
;;; BODY expression.
(cond-expand
 (guile-2
  (define-syntax-rule (with-benchmark-prefix prefix body body* ...)
    (with-benchmark-prefix* prefix (lambda () body body* ...))))
 (else
  (defmacro with-benchmark-prefix (prefix . body)
    `(with-benchmark-prefix* ,prefix (lambda () ,@body)))))


;;;; Benchmark results
;;;;

(define *calibration-result*
  "<will be set during initialization>")

(define (benchmark-overhead iterations accessor)
  (* (/ iterations 1.0 (benchmark-result:iterations *calibration-result*))
     (accessor *calibration-result*)))

(define (benchmark-result:core-time result)
  (- (benchmark-result:run-time result)
     (benchmark-overhead (benchmark-result:iterations result)
                         benchmark-result:run-time)))


;;;; REPORTERS
;;;;

;;; The global set of reporters.
(define report-hook (make-hook 1))

(define (default-reporter result)
  (if (hook-empty? report-hook)
      (user-reporter result)
      (run-hook report-hook result)))

(define current-reporter
  (make-parameter default-reporter))

(define (register-reporter reporter)
  (add-hook! report-hook reporter))

(define (unregister-reporter reporter)
  (remove-hook! report-hook reporter))

;;; Return true iff REPORTER is in the current set of reporter functions.
(define (reporter-registered? reporter)
  (if (memq reporter (hook->list report-hook)) #t #f))

;;; Send RESULT to all currently registered reporter functions.
(define (report result)
  ((current-reporter) result))


;;;; Some useful standard reporters:
;;;; Log reporters write all benchmark results to a given log file.
;;;; Full reporters write all benchmark results to the standard output.
;;;; User reporters write some interesting results to the standard output.

;;; Display a single benchmark result to the given port
(define (print-result port result)
  (let ((name (format-benchmark-name (benchmark-result:name result)))
        (iterations (benchmark-result:iterations result))
        (real-time (benchmark-result:real-time result))
        (run-time (benchmark-result:run-time result))
        (gc-time (benchmark-result:gc-time result))
        (core-time (benchmark-result:core-time result)))
    (write (list name iterations
		 'total (->seconds real-time)
		 'user (->seconds run-time)
		 'system 0
                 'frame (->seconds (- run-time core-time))
		 'benchmark (->seconds core-time)
		 'user/interp (->seconds (- run-time gc-time))
		 'bench/interp (->seconds (- core-time gc-time))
		 'gc (->seconds gc-time))
	   port)
    (newline port)))

;;; Return a reporter procedure which prints all results to the file
;;; FILE, in human-readable form.  FILE may be a filename, or a port.
(define (make-log-reporter file)
  (let ((port (if (output-port? file) file
		  (open-output-file file))))
    (lambda (result)
      (print-result port result)
      (force-output port))))

;;; A reporter that reports all results to the user.
(define (full-reporter result)
  (print-result (current-output-port) result))

;;; Display interesting results of a single benchmark to the given port
(define (print-user-result port result)
  (let ((name (format-benchmark-name (benchmark-result:name result)))
        (iterations (benchmark-result:iterations result))
        (real-time (benchmark-result:real-time result))
        (run-time (benchmark-result:run-time result))
        (gc-time (benchmark-result:gc-time result))
        (core-time (benchmark-result:core-time result)))
    (write (list name iterations
                 'real (->seconds real-time)
		 'real/iteration (->seconds (/ real-time iterations))
		 'run/iteration (->seconds (/ run-time iterations))
		 'core/iteration (->seconds (/ core-time iterations))
		 'gc (->seconds gc-time))
	   port)
    (newline port)))

;;; A reporter that reports interesting results to the user.
(define (user-reporter result)
  (print-user-result (current-output-port) result))


;;;; Initialize the benchmarking system:
;;;;

(define (calibrate-benchmark-framework)
  (display ";; running guile version ")
  (display (version))
  (newline)
  (display ";; calibrating the benchmarking framework...")
  (newline)
  (parameterize ((current-reporter
                  (lambda (result)
                    (set! *calibration-result* result)
                    (display ";; calibration: ")
                    (print-user-result (current-output-port) result))))
    (benchmark "empty initialization benchmark" 10000000 #t)))

(calibrate-benchmark-framework)
