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

;;;
;;; This file provides compatibility routines with the benchmarking framework
;;; used in Larceny/Twobit.
;;;
;;; See http://www.ccs.neu.edu/home/will/Twobit/benchmarksAbout.html for details.
;;;

(define (run-benchmark name . args)
  (define %concise-invocation?
    ;; This procedure can be called with only two arguments, NAME and
    ;; RUN-MAKER.
    (procedure? (car args)))

  (let ((count     (if %concise-invocation? 0 (car args)))
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
