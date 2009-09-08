#!/bin/sh
# -*- Scheme -*-
exec ${GUILE-guile} -q -l "$0"                                  \
                    -c '(apply main (cdr (command-line)))'      \
                    --benchmark-dir="$(dirname $0)" "$@"
!#
;;; Copyright (C) 2008, 2009 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this software; see the file COPYING.LESSER.  If
;;; not, write to the Free Software Foundation, Inc., 51 Franklin
;;; Street, Fifth Floor, Boston, MA 02110-1301 USA

(use-modules (ice-9 rdelim)
             (ice-9 popen)
             (ice-9 regex)
             (ice-9 format)
             (ice-9 pretty-print)
             (srfi srfi-1)
             (srfi srfi-37))


;;;
;;; Running Guile.
;;;

(define (run-reference-guile env bench-dir profile-opts bench)
  "Run the ``mainstream'' Guile, i.e., Guile 1.9 with its own GC."
  (open-input-pipe (string-append
                    env " "
                    bench-dir "/gc-profile.scm " profile-opts
                    " \"" bench "\"")))

(define (run-bdwgc-guile env bench-dir profile-opts options bench)
  "Run the Guile port to the Boehm-Demers-Weiser GC (BDW-GC)."
  (let ((fsd (assoc-ref options 'free-space-divisor)))
    (open-input-pipe (string-append env " "
                                    "GC_FREE_SPACE_DIVISOR="
                                    (number->string fsd)

                                    (if (or (assoc-ref options 'incremental?)
                                            (assoc-ref options 'generational?))
                                        " GC_ENABLE_INCREMENTAL=yes"
                                        "")
                                    (if (assoc-ref options 'generational?)
                                        " GC_PAUSE_TIME_TARGET=999999"
                                        "")
                                    (if (assoc-ref options 'parallel?)
                                        "" ;; let it choose the number of procs
                                        " GC_MARKERS=1")
                                    " "
                                    bench-dir "/gc-profile.scm " profile-opts
                                    " \"" bench "\""))))


;;;
;;; Extracting performance results.
;;;

(define (grep regexp input)
  "Read line by line from the @var{input} port and return all matches for
@var{regexp}."
  (let ((regexp (if (string? regexp) (make-regexp regexp) regexp)))
    (with-input-from-port input
      (lambda ()
        (let loop ((line   (read-line))
                   (result '()))
          (format #t "> ~A~%" line)
          (if (eof-object? line)
              (reverse result)
              (cond ((regexp-exec regexp line)
                     =>
                     (lambda (match)
                       (loop (read-line)
                             (cons match result))))
                    (else
                     (loop (read-line) result)))))))))

(define (parse-result benchmark-output)
  (let ((result (grep "^(execution time|heap size):[[:blank:]]+([0-9.]+)"
                      benchmark-output)))
    (fold (lambda (match result)
            (cond ((equal? (match:substring match 1) "execution time")
                   (cons (cons 'execution-time
                               (string->number (match:substring match 2)))
                         result))
                  ((equal? (match:substring match 1) "heap size")
                   (cons (cons 'heap-size
                               (string->number (match:substring match 2)))
                         result))
                  (else
                   result)))
          '()
          result)))

(define (pretty-print-result benchmark reference bdwgc)
  (define ref-heap (assoc-ref reference 'heap-size))
  (define ref-time (assoc-ref reference 'execution-time))

  (define (distance x1 y1 x2 y2)
    ;; Return the distance between (X1,Y1) and (X2,Y2).  Y is the heap size,
    ;; in MiB and X is the execution time in seconds.
    (let ((y1 (/ y1 (expt 2 20)))
          (y2 (/ y2 (expt 2 20))))
      (sqrt (+ (expt (- y1 y2) 2)
               (expt (- x1 x2) 2)))))

  (define (score time heap)
    ;; Return a score lower than +1.0.  The score is positive if the
    ;; distance to the origin of (TIME,HEAP) is smaller than that of
    ;; (REF-TIME,REF-HEAP), negative otherwise.

    ;; heap  ^         .
    ;; size  |         .   worse
    ;;       |         .    [-]
    ;;       |         .
    ;;       | . . . .ref. . . .
    ;;       |         .
    ;;       |  [+]    .
    ;;       | better  .
    ;;     0 +-------------------->
    ;;                        exec. time

    (let ((ref-dist (distance ref-time ref-heap 0 0))
          (dist     (distance time heap 0 0)))
      (/ (- ref-dist dist) ref-dist)))

  (define (score-string time heap)
    ;; Return a string denoting a bar to illustrate the score of (TIME,HEAP)
    ;; relative to (REF-TIME,REF-HEAP).
    (define %max-width 15)

    (let ((s (score time heap)))
      (make-string (inexact->exact (round (* (if (< s 0.0) (- s) s)
                                             %max-width)))
                   (if (< s 0.0)
                       #\-
                       #\+))))

  (define (print-line name result ref?)
    (let ((name (string-pad-right name 23))
          (time (assoc-ref result 'execution-time))
          (heap (assoc-ref result 'heap-size)))
      (format #t "~a ~6,2f (~,2fx)    ~7,3f (~,2fx)~A~%"
              name
              (/ heap (expt 2.0 20)) (/ heap ref-heap 1.0)
              time (/ time ref-time 1.0)
              (if (not ref?)
                  (string-append " "
                                 (score-string time heap))
                  ""))))

  (format #t "benchmark: `~a'~%" benchmark)
  (format #t "                       heap size (MiB)  execution time (s.)~%")
  (print-line "Guile" reference #t)
  (for-each (lambda (bdwgc)
              (let ((name (format #f "BDW-GC, FSD=~a~a"
                                  (assoc-ref bdwgc 'free-space-divisor)
                                  (cond ((assoc-ref bdwgc 'incremental?)
                                         " incr.")
                                        ((assoc-ref bdwgc 'generational?)
                                         " gene.")
                                        ((assoc-ref bdwgc 'parallel?)
                                         " paral.")
                                        (else "")))))
                (print-line name bdwgc #f)))
            bdwgc))

(define (print-raw-result benchmark reference bdwgc)
  (pretty-print `(,benchmark
                  (reference . ,reference)
                  (bdw-gc    . ,bdwgc))))



;;;
;;; Option processing.
;;;

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\r "reference") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'reference-environment arg
                              (alist-delete 'reference-environment result
                                            eq?))))
        (option '(#\b "bdw-gc") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'bdwgc-environment arg
                              (alist-delete 'bdwgc-environment result
                                            eq?))))
        (option '(#\d "benchmark-dir") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'benchmark-directory arg
                              (alist-delete 'benchmark-directory result
                                            eq?))))
        (option '(#\p "profile-options") #t #f
                (lambda (opt name arg result)
                  (let ((opts (assoc-ref result 'profile-options)))
                    (alist-cons 'profile-options
                                (string-append opts " " arg)
                                (alist-delete 'profile-options result
                                              eq?)))))
        (option '(#\l "log-file") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'log-port (open-output-file arg)
                              (alist-delete 'log-port result
                                            eq?))))
        (option '("raw") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'printer print-raw-result
                              (alist-delete 'printer result eq?))))
        (option '("load-results") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'load-results? #t result)))))

(define %default-options
  `((reference-environment . "GUILE=guile")
    (benchmark-directory   . "./gc-benchmarks")
    (log-port              . ,(current-output-port))
    (profile-options       . "")
    (input                 . ())
    (printer               . ,pretty-print-result)))

(define (show-help)
  (format #t "Usage: run-benchmark [OPTIONS] BENCHMARKS...
Run BENCHMARKS (a list of Scheme files) and display a performance
comparison of standard Guile (1.9) and the BDW-GC-based Guile.

  -h, --help      Show this help message

  -r, --reference=ENV
  -b, --bdw-gc=ENV
                  Use ENV as the environment necessary to run the
                  \"reference\" Guile (1.9) or the BDW-GC-based Guile,
                  respectively.  At a minimum, ENV should define the
                  `GUILE' environment variable.  For example:

                    --reference='GUILE=/foo/bar/guile LD_LIBRARY_PATH=/foo'

  -p, --profile-options=OPTS
                  Pass OPTS as additional options for `gc-profile.scm'.
  -l, --log-file=FILE
                  Save output to FILE instead of the standard output.

      --raw       Write benchmark results in raw (s-exp) format.
      --load-results
                  Load raw (s-exp) results instead of actually running
                  the benchmarks.

  -d, --benchmark-dir=DIR
                  Use DIR as the GC benchmark directory where `gc-profile.scm'
                  lives (it is automatically determined by default).

Report bugs to <bug-guile@gnu.org>.~%"))

(define (parse-args args)
  (define (leave fmt . args)
    (apply format (current-error-port) (string-append fmt "~%") args)
    (exit 1))

  (args-fold args %options
             (lambda (opt name arg result)
               (leave "~A: unrecognized option" opt))
             (lambda (file result)
               (let ((files (or (assoc-ref result 'input) '())))
                 (alist-cons 'input (cons file files)
                             (alist-delete 'input result eq?))))
             %default-options))


;;;
;;; The main program.
;;;

(define (main . args)
  (let* ((args            (parse-args args))
         (benchmark-files (assoc-ref args 'input)))

    (let* ((log       (assoc-ref args 'log-port))
           (bench-dir (assoc-ref args 'benchmark-directory))
           (ref-env   (assoc-ref args 'reference-environment))
           (bdwgc-env (or (assoc-ref args 'bdwgc-environment)
                          (string-append "GUILE=" bench-dir
                                         "/../meta/guile")))
           (prof-opts (assoc-ref args 'profile-options))
           (print     (assoc-ref args 'printer)))
      (define (run benchmark)
        (let ((ref   (parse-result (run-reference-guile ref-env
                                                        bench-dir
                                                        prof-opts
                                                        benchmark)))
              (bdwgc (map (lambda (fsd incremental?
                                       generational? parallel?)
                            (let ((opts
                                   (list
                                    (cons 'free-space-divisor fsd)
                                    (cons 'incremental? incremental?)
                                    (cons 'generational? generational?)
                                    (cons 'parallel? parallel?))))
                              (append opts
                                      (parse-result
                                       (run-bdwgc-guile bdwgc-env
                                                        bench-dir
                                                        prof-opts
                                                        opts
                                                        benchmark)))))
                          '( 3  6  9  3  3)
                          '(#f #f #f #t #f)      ;; incremental
                          '(#f #f #f #f #t)      ;; generational
                          '(#f #f #f #f #f))))   ;; parallel
          `(,benchmark
            (reference . ,ref)
            (bdw-gc    . ,bdwgc))))

      (define (load-results file)
        (with-input-from-file file
          (lambda ()
            (let loop ((results '()) (o (read)))
              (if (eof-object? o)
                  (reverse results)
                  (loop (cons o results)
                        (read)))))))

      (for-each (lambda (result)
                  (let ((benchmark (car result))
                        (ref       (assoc-ref (cdr result) 'reference))
                        (bdwgc     (assoc-ref (cdr result) 'bdw-gc)))
                    (with-output-to-port log
                      (lambda ()
                        (print benchmark ref bdwgc)
                        (newline)
                        (force-output)))))
                (if (assoc-ref args 'load-results?)
                    (append-map load-results benchmark-files)
                    (map run benchmark-files))))))
