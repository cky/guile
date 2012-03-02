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
             (language tree-il primitives)
             (language tree-il canonicalize)
             (srfi srfi-1)
             (ice-9 pretty-print)
             (system syntax))

;; Minimize a syntax-object such that it can no longer be used as the
;; first argument to 'datum->syntax', but is otherwise equivalent.
(define (squeeze-syntax-object! syn)
  (define (ensure-list x) (if (vector? x) (vector->list x) x))
  (let ((x    (vector-ref syn 1))
        (wrap (vector-ref syn 2))
        (mod  (vector-ref syn 3)))
    (let ((marks (car wrap))
          (subst (cdr wrap)))
      (define (set-wrap! marks subst)
        (vector-set! syn 2 (cons marks subst)))
      (cond
       ((symbol? x)
        (let loop ((marks marks) (subst subst))
          (cond
           ((null? subst) (set-wrap! marks subst) syn)
           ((eq? 'shift (car subst)) (loop (cdr marks) (cdr subst)))
           ((find (lambda (entry) (and (eq? x (car entry))
                                       (equal? marks (cadr entry))))
                  (apply map list (map ensure-list
                                       (cdr (vector->list (car subst))))))
            => (lambda (entry)
                 (set-wrap! marks
                            (list (list->vector
                                   (cons 'ribcage
                                         (map vector entry)))))
                 syn))
           (else (loop marks (cdr subst))))))
       ((or (pair? x) (vector? x))
        syn)
       (else x)))))

(define (squeeze-constant! x)
  (define (syntax-object? x)
    (and (vector? x)
         (= 4 (vector-length x))
         (eq? 'syntax-object (vector-ref x 0))))
  (cond ((syntax-object? x)
         (squeeze-syntax-object! x))
        ((pair? x)
         (set-car! x (squeeze-constant! (car x)))
         (set-cdr! x (squeeze-constant! (cdr x)))
         x)
        ((vector? x)
         (for-each (lambda (i)
                     (vector-set! x i (squeeze-constant! (vector-ref x i))))
                   (iota (vector-length x)))
         x)
        (else x)))

(define (squeeze-tree-il! x)
  (post-order! (lambda (x)
                 (if (const? x)
                     (set! (const-exp x)
                           (squeeze-constant! (const-exp x))))
                 #f)
               x))

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
                           (squeeze-tree-il!
                            (canonicalize!
                             (resolve-primitives!
                              (macroexpand x 'c '(compile load eval))
                              (current-module))))
                           (current-module)
                           (list #:avoid-lambda? #f
                                 #:use-case? #f
                                 #:strip-numeric-suffixes? #t
                                 #:use-derived-syntax?
                                 (and (pair? x)
                                      (eq? 'let (car x)))))
                          out #:width 120 #:max-expr-width 70)
            (newline out)
            (loop (read in))))))
  (system (format #f "mv -f ~s.tmp ~s" target target)))
