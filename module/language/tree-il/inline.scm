;;; a simple inliner

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (language tree-il inline)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:export (inline!))

;; Possible optimizations:
;; * constant folding, propagation
;; * procedure inlining
;;   * always when single call site
;;   * always for "trivial" procs
;;   * otherwise who knows
;; * dead code elimination
;; * degenerate case optimizations
;; * "fixing letrec"

;; This is a completely brain-dead optimization pass whose sole claim to
;; fame is ((lambda () x)) => x.
(define (inline! x)
  (post-order!
   (lambda (x)
     (record-case x
       ((<application> proc args)
        (and (lambda? proc) (null? args)
             (lambda-body proc)))
       (else #f)))
   x))
