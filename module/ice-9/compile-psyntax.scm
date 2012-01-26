;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(use-modules (language tree-il)
             (language tree-il optimize)
             (language tree-il canonicalize)
             (ice-9 pretty-print)
             (system syntax))

;; Avoid gratuitous churn in psyntax-pp.scm due to the marks and labels
;; changing session identifiers.
(set! syntax-session-id (lambda () "*"))

(let ((source (list-ref (command-line) 1))
      (target (list-ref (command-line) 2)))
  (let ((in (open-input-file source))
        (out (open-output-file (string-append target ".tmp"))))
    (write '(eval-when (compile) (set-current-module (resolve-module '(guile))))
           out)
    (newline out)
    (let loop ((x (read in)))
      (if (eof-object? x)
          (begin
            (close-port out)
            (close-port in))
          (begin
            (pretty-print (tree-il->scheme
                           (canonicalize!
                            (optimize!
                             (macroexpand x 'c '(compile load eval))
                             (current-module)
                             '())))
                          out)
            (newline out)
            (loop (read in))))))
  (system (format #f "mv -f ~s.tmp ~s" target target)))
