#!/bin/sh
# -*- Scheme -*-
exec ${GUILE-guile} --no-debug -q -l "$0" \
                    -c '(apply main (cdr (command-line)))' "$@"
!#
;;; Copyright (C) 2008, 2011 Free Software Foundation, Inc.
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

(use-modules (ice-9 format)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-37)
             (srfi srfi-39))


;;;
;;; Memory usage.
;;;

(define (memory-mappings pid)
  "Return an list of alists, each of which contains information about a
memory mapping of process @var{pid}.  This information is obtained by reading
@file{/proc/PID/smaps} on Linux.  See `procs(5)' for details."

  (define mapping-line-rx
    ;; As of Linux 2.6.32.28, an `smaps' line looks like this:
    ;; "00400000-00401000 r-xp 00000000 fe:00 108264 /home/ludo/soft/bin/guile"
    (make-regexp
     "^([[:xdigit:]]+)-([[:xdigit:]]+) ([rwx-]{3}[ps]) ([[:xdigit:]]+) [[:xdigit:]]{2}:[[:xdigit:]]{2} [0-9]+[[:blank:]]+(.*)$"))

  (define rss-line-rx
    (make-regexp
     "^Rss:[[:blank:]]+([[:digit:]]+) kB$"))

  (if (not (string-contains %host-type "-linux-"))
      (error "this procedure only works on Linux-based systems" %host-type))

  (with-input-from-port (open-input-file (format #f "/proc/~a/smaps" pid))
    (lambda ()
      (let loop ((line   (read-line))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (cond ((regexp-exec mapping-line-rx line)
                   =>
                   (lambda (match)
                     (let ((mapping-start (string->number
                                           (match:substring match 1)
                                           16))
                           (mapping-end   (string->number
                                           (match:substring match 2)
                                           16))
                           (access-bits   (match:substring match 3))
                           (name          (match:substring match 5)))
                       (loop (read-line)
                             (cons `((mapping-start . ,mapping-start)
                                     (mapping-end   . ,mapping-end)
                                     (access-bits   . ,access-bits)
                                     (name          . ,(if (string=? name "")
                                                           #f
                                                           name)))
                                   result)))))
                  ((regexp-exec rss-line-rx line)
                   =>
                   (lambda (match)
                     (let ((section+ (cons (cons 'rss
                                                 (string->number
                                                  (match:substring match 1)))
                                           (car result))))
                       (loop (read-line)
                             (cons section+ (cdr result))))))
                  (else
                   (loop (read-line) result))))))))

(define (total-heap-size pid)
  "Return a pair representing the total and RSS heap size of PID."

  (define heap-or-anon-rx
    (make-regexp "\\[(heap|anon)\\]"))

  (define private-mapping-rx
    (make-regexp "^[r-][w-][x-]p$"))

  (fold (lambda (heap total+rss)
          (let ((name (assoc-ref heap 'name))
                (perm (assoc-ref heap 'access-bits)))
            ;; Include anonymous private mappings.
            (if (or (and (not name)
                         (regexp-exec private-mapping-rx perm))
                    (and name
                         (regexp-exec heap-or-anon-rx name)))
                (let ((start (assoc-ref heap 'mapping-start))
                      (end   (assoc-ref heap 'mapping-end))
                      (rss   (assoc-ref heap 'rss)))
                  (cons (+ (car total+rss) (- end start))
                        (+ (cdr total+rss) rss)))
                total+rss)))
        '(0 . 0)
        (memory-mappings pid)))


(define (display-stats start end)
  (define (->usecs sec+usecs)
    (+ (* 1000000 (car sec+usecs))
       (cdr sec+usecs)))

  (let ((usecs        (- (->usecs end) (->usecs start)))
        (heap-size    (total-heap-size (getpid)))
        (gc-heap-size (assoc-ref (gc-stats) 'heap-size)))

    (format #t "execution time:              ~6,3f seconds~%"
            (/ usecs 1000000.0))

    (and gc-heap-size
         (format #t "GC-reported heap size: ~8d B   (~1,2f MiB)~%"
                 gc-heap-size
                 (/ gc-heap-size 1024.0 1024.0)))

    (format #t "heap size:             ~8d B   (~1,2f MiB)~%"
            (car heap-size)
            (/ (car heap-size) 1024.0 1024.0))
    (format #t "heap RSS:              ~8d KiB (~1,2f MiB)~%"
            (cdr heap-size)
            (/ (cdr heap-size) 1024.0))
;;     (system (format #f "cat /proc/~a/smaps" (getpid)))
;;     (system (format #f "exmtool procs | grep -E '^(PID|~a)'" (getpid)))
    ))


;;;
;;; Larceny/Twobit benchmarking compability layer.
;;;

(define *iteration-count*
  (make-parameter #f))

(define (run-benchmark name . args)
  "A @code{run-benchmark} procedure compatible with Larceny's GC benchmarking
framework.  See
@url{http://www.ccs.neu.edu/home/will/Twobit/benchmarksAbout.html} for
details."

  (define %concise-invocation?
    ;; This procedure can be called with only two arguments, NAME and
    ;; RUN-MAKER.
    (procedure? (car args)))

  (let ((count     (or (*iteration-count*)
                       (if %concise-invocation? 0 (car args))))
        (run-maker (if %concise-invocation? (car args) (cadr args)))
        (ok?       (if %concise-invocation?
                       (lambda (result) #t)
                       (caddr args)))
        (args      (if %concise-invocation? '() (cdddr args))))
    (let loop ((i 0))
      (and (< i count)
           (let ((result (apply run-maker args)))
             (if (not (ok? result))
                 (begin
                   (format (current-output-port) "invalid result for `~A'~%"
                           name)
                   (exit 1)))
             (loop (1+ i)))))))

(define (save-directory-excursion directory thunk)
  (let ((previous-dir (getcwd)))
    (dynamic-wind
      (lambda ()
        (chdir directory))
      thunk
      (lambda ()
        (chdir previous-dir)))))

(define (load-larceny-benchmark file)
  "Load the Larceny benchmark from @var{file}."
  (let ((name   (let ((base (basename file)))
                  (substring base 0 (or (string-rindex base #\.)
                                        (string-length base)))))
        (module (let ((m (make-module)))
                  (beautify-user-module! m)
                  (module-use! m (resolve-interface '(ice-9 syncase)))
                  m)))
    (save-directory-excursion (dirname file)
     (lambda ()
       (save-module-excursion
        (lambda ()
          (set-current-module module)
          (module-define! module 'run-benchmark run-benchmark)
          (load (basename file))

          ;; Invoke the benchmark's entry point.
          (let ((entry (module-ref (current-module)
                                   (symbol-append (string->symbol name)
                                                  '-benchmark))))
            (entry))))))))



;;;
;;; Option processing.
;;;

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\l "larceny") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'larceny? #t result)))
        (option '(#\i "iterations") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'iterations (string->number arg) result)))))

(define (show-help)
  (format #t "Usage: gc-profile [OPTIONS] FILE.SCM
Load FILE.SCM, a Guile Scheme source file, and report its execution time and
final heap usage.

  -h, --help      Show this help message

  -l, --larceny   Provide mechanisms compatible with the Larceny/Twobit
                  GC benchmark suite.
  -i, --iterations=COUNT
                  Run the given benchmark COUNT times, regardless of the
                  iteration count passed to `run-benchmark' (for Larceny
                  benchmarks).

Report bugs to <bug-guile@gnu.org>.~%"))

(define (parse-args args)
  (define (leave fmt . args)
    (apply format (current-error-port) (string-append fmt "~%") args)
    (exit 1))

  (args-fold args %options
             (lambda (opt name arg result)
               (leave "~A: unrecognized option" opt))
             (lambda (file result)
               (if (pair? (assoc 'input result))
                   (leave "~a: only one input file at a time" file)
                   (alist-cons 'input file result)))
             '()))


;;;
;;; Main program.
;;;

(define (main . args)
  (let* ((options (parse-args args))
         (prog    (assoc-ref options 'input))
         (load    (if (assoc-ref options 'larceny?)
                      load-larceny-benchmark
                      load)))

    (parameterize ((*iteration-count* (assoc-ref options 'iterations)))
      (format #t "running `~a' with Guile ~a...~%" prog (version))

      (let ((start (gettimeofday)))
        (dynamic-wind
          (lambda ()
            #t)
          (lambda ()
            (set! quit (lambda args args))
            (load prog))
          (lambda ()
            (let ((end (gettimeofday)))
              (format #t "done~%")
              (display-stats start end))))))))
