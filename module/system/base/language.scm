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
  #:export (define-language lookup-language make-language
            language-name language-title language-version language-reader
            language-printer language-parser language-read-file
            language-compilers language-evaluator

            lookup-compilation-order invalidate-compilation-cache!))


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

(define (invalidate-compilation-cache!)
  (set! *compilation-cache* '()))

(define (compute-compilation-order from to)
  (let lp ((from from) (seen '()))
    (cond ((eq? from to) (reverse! (cons from seen)))
          ((memq from seen) #f)
          (else (or-map (lambda (lang) (lp lang (cons from seen)))
                        (map car (language-compilers from)))))))

(define (lookup-compilation-order from to)
  (or (assoc-ref *compilation-cache* (cons from to))
      (let ((order (compute-compilation-order from to)))
        (set! *compilation-cache*
              (acons (cons from to) order *compilation-cache*))
        order)))
