#!/bin/sh
# -*- Scheme -*-
exec ${GUILE-guile} --no-debug -q -l "$0" \
                    -c '(apply main (command-line))' "$@"
!#
;;; Copyright (C) 2008 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301 USA

(use-modules (ice-9 format)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1))

(define (memory-mappings pid)
  "Return an list of alists, each of which contains information about a
memory mapping of process @var{pid}.  This information is obtained by reading
@file{/proc/PID/smaps} on Linux.  See `procs(5)' for details."

  (define mapping-line-rx
    (make-regexp
     "^([[:xdigit:]]+)-([[:xdigit:]]+) ([rwx-]{3}[ps]) ([[:xdigit:]]+) [0-9]{2}:[0-9]{2} [0-9]+[[:blank:]]+(.*)$"))

  (define rss-line-rx
    (make-regexp
     "^Rss:[[:blank:]]+([[:digit:]]+) kB$"))

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
  "Return the total heap size of process @var{pid}."

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


(define (main . args)
  (if (not (= (length args) 2))
      (begin
        (format #t "Usage: run FILE.SCM

Load FILE.SCM, a Guile Scheme source file, and report its execution time and
final heap usage.~%")
        (exit 1)))

  (let ((prog  (cadr args))
        (start (gettimeofday)))
    (format #t "running `~a'...~%" prog)
    (dynamic-wind
        (lambda ()
          #t)
        (lambda ()
          (set! quit (lambda args args))
          (load prog))
        (lambda ()
          (let ((end (gettimeofday)))
            (format #t "done~%")
            (display-stats start end))))))
