;;; srfi-13.scm --- String Library

;; 	Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
;;
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This module is fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-13)
  :export (
;;; Predicates
 ;; string? string-null?       <= in the core
 string-any string-every

;;; Constructors
 ;; make-string string         <= in the core
 string-tabulate

;;; List/string conversion
 ;; string->list	       extended
 ;; list->string               <= in the core
 reverse-list->string
 string-join

;;; Selection
 ;; string-length string-ref   <= in the core
 ;; string-copy		       extended
 substring/shared
 string-copy!
 string-take string-take-right
 string-drop string-drop-right
 string-pad string-pad-right
 string-trim string-trim-right
 string-trim-both

;;; Modification
 ;; string-set!                <= in the core
 ;; string-fill!		extended

;;; Comparison
 string-compare string-compare-ci
 string= string<>
 string< string>
 string<= string>=
 string-ci= string-ci<>
 string-ci< string-ci>
 string-ci<= string-ci>=
 string-hash string-hash-ci

;;; Prefixes/Suffixes
 string-prefix-length
 string-prefix-length-ci
 string-suffix-length
 string-suffix-length-ci
 string-prefix?
 string-prefix-ci?
 string-suffix?
 string-suffix-ci?

;;; Searching
 ;; string-index			extended
 string-index-right
 string-skip string-skip-right
 string-count
 string-contains string-contains-ci

;;; Alphabetic case mapping

 ;; string-upcase string-upcase!	extended
 ;; string-downcase string-downcase!	extended
 string-titlecase string-titlecase!

;;; Reverse/Append
 string-reverse string-reverse!
 ;; string-append                    <= in the core
 string-append/shared
 string-concatenate
 string-concatenate-reverse
 string-concatenate/shared
 string-concatenate-reverse/shared

;;; Fold/Unfold/Map
 string-map string-map!
 string-fold
 string-fold-right
 string-unfold
 string-unfold-right
 string-for-each
 string-for-each-index

;;; Replicate/Rotate
 xsubstring string-xcopy!

;;; Miscellaneous
 string-replace
 string-tokenize

;;; Filtering/Deleting
 string-filter
 string-delete
 )
  :replace (string->list string-copy string-fill!
	    string-upcase! string-upcase string-downcase! string-downcase
	    string-index)
  )

(cond-expand-provide (current-module) '(srfi-13))

(load-extension "libguile-srfi-srfi-13-14" "scm_init_srfi_13")

(define string-hash
  (lambda (s . rest)
    (let ((bound (if (pair? rest)
		     (or (car rest)
			 871)
		     871))
	  (start (if (and (pair? rest) (pair? (cdr rest)))
		     (cadr rest)
		     0))
	  (end (if (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest)))
		   (caddr rest)
		   (string-length s))))
      (hash (substring/shared s start end) bound))))

(define string-hash-ci
  (lambda (s . rest)
    (let ((bound (if (pair? rest)
		     (or (car rest)
			 871)
		     871))
	  (start (if (and (pair? rest) (pair? (cdr rest)))
		     (cadr rest)
		     0))
	  (end (if (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest)))
		   (caddr rest)
		   (string-length s))))
      (hash (string-upcase (substring/shared s start end)) bound))))

;;; srfi-13.scm ends here
