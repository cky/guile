;;; Multi-language support

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

(define-module (system base language)
  #:use-module (system base syntax)
  #:export (define-language language? lookup-language make-language
            language-name language-title language-version language-reader
            language-printer language-parser language-read-file
            language-compilers language-decompilers language-evaluator

            lookup-compilation-order lookup-decompilation-order
            invalidate-compilation-cache!))


;;;
;;; Language class
;;;

(define-record <language>
  name
  title
  version
  reader
  printer
  (parser #f)
  (read-file #f)
  (compilers '())
  (decompilers '())
  (evaluator #f))

(define-macro (define-language name . spec)
  `(begin
     (invalidate-compilation-cache!)
     (define ,name (make-language #:name ',name ,@spec))))

(define (lookup-language name)
  (let ((m (resolve-module `(language ,name spec))))
    (if (module-bound? m name)
	(module-ref m name)
	(error "no such language" name))))

(define *compilation-cache* '())
(define *decompilation-cache* '())

(define (invalidate-compilation-cache!)
  (set! *decompilation-cache* '())
  (set! *compilation-cache* '()))

(define (compute-translation-order from to language-translators)
  (cond
   ((not (language? to))
    (compute-translation-order from (lookup-language to) language-translators))
   (else
    (let lp ((from from) (seen '()))
      (cond
       ((not (language? from))
        (lp (lookup-language from) seen))
       ((eq? from to) (reverse! seen))
       ((memq from seen) #f)
       (else (or-map (lambda (pair)
                       (lp (car pair) (acons from (cdr pair) seen)))
                     (language-translators from))))))))

(define (lookup-compilation-order from to)
  (let ((key (cons from to)))
    (or (assoc-ref *compilation-cache* key)
        (let ((order (compute-translation-order from to language-compilers)))
          (set! *compilation-cache*
                (acons key order *compilation-cache*))
          order))))

(define (lookup-decompilation-order from to)
  (let ((key (cons from to)))
    (or (assoc-ref *decompilation-cache* key)
        ;; trickery!
        (let ((order (and=>
                      (compute-translation-order to from language-decompilers)
                      reverse!)))
          (set! *decompilation-cache* (acons key order *decompilation-cache*))
          order))))
