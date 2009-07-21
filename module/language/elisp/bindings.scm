;;; Guile Emac Lisp

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language elisp bindings)
  #:export (make-bindings mark-fluid-needed! map-fluids-needed))

; This module defines routines to handle analysis of symbol bindings used
; during elisp compilation.  This data allows to collect the symbols, for
; which fluids need to be created, or mark certain symbols as lexically bound.


; Record type used to hold the data necessary.

(define bindings-type (make-record-type 'bindings '(needed-fluids)))


; Construct an 'empty' instance of the bindings data structure to be used
; at the start of a fresh compilation.

(define (make-bindings)
  ((record-constructor bindings-type) '()))


; Mark that a given symbol is needed as fluid in the specified slot-module.

(define (mark-fluid-needed! bindings sym module)
  (let* ((old-needed ((record-accessor bindings-type 'needed-fluids) bindings))
         (old-in-module (or (assoc-ref old-needed module) '()))
         (new-in-module (if (memq sym old-in-module)
                          old-in-module
                          (cons sym old-in-module)))
         (new-needed (assoc-set! old-needed module new-in-module)))
    ((record-modifier bindings-type 'needed-fluids) bindings new-needed)))


; Cycle through all fluids needed in order to generate the code for their
; creation or some other analysis.

(define (map-fluids-needed bindings proc)
  (let* ((needed ((record-accessor bindings-type 'needed-fluids) bindings)))
    (let iterate-modules ((mod-tail needed)
                          (mod-result '()))
      (if (null? mod-tail)
        mod-result
        (iterate-modules
          (cdr mod-tail)
          (let* ((aentry (car mod-tail))
                 (module (car aentry))
                 (symbols (cdr aentry)))
            (let iterate-symbols ((sym-tail symbols)
                                  (sym-result mod-result))
              (if (null? sym-tail)
                sym-result
                (iterate-symbols (cdr sym-tail)
                                 (cons (proc module (car sym-tail))
                                       sym-result))))))))))
