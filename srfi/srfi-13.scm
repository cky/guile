;;;; srfi-13.scm --- SRFI-13 procedures for Guile
;;;;
;;;; 	Copyright (C) 2001 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 2, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA

(define-module (srfi srfi-13))

(export 
;;; Predicates
 ;; string? string-null?       <= in the core
 string-any string-every

;;; Constructors
 ;; make-string string         <= in the core
 string-tabulate

;;; List/string conversion
 string->list
 ;; list->string               <= in the core
 reverse-list->string
 string-join

;;; Selection
 ;; string-length string-ref   <= in the core
 string-copy
 substring/shared
 string-copy!
 string-take string-take-right
 string-drop string-drop-right
 string-pad string-pad-right
 string-trim string-trim-right
 string-trim-both

;;; Modification
 ;; string-set!                <= in the core
 string-fill!

;;; Comparison
 string-compare string-compare-ci
 string= string<>
 string< string>
 string<= string>=
 string-ci= string-ci<>
 string-ci< string-ci>
 string-ci<= string-ci>=
 string-hash string-hash-ci		; FIXME::martin: rewrite in C?

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
 string-index string-index-right
 string-skip string-skip-right
 string-count
 string-contains string-contains-ci

;;; Alphabetic case mapping

 string-upcase string-upcase!
 string-downcase string-downcase!
 string-titlecase string-titlecase!

;;; Reverse/Append
 string-reverse string-reverse!
 ;; string-append                    <= in the core
 string-append/shared
 string-concatenate
 reverse-string-concatenate
 string-concatenate/shared
 reverse-string-concatenate/shared

;;; Fold/Unfold/Map
 string-map string-map!
 string-fold
 string-fold-right
 string-unfold
 string-unfold-right
 string-for-each

;;; Replicate/Rotate
 xsubstring string-xcopy!

;;; Miscellaneous
 string-replace
 string-tokenize

;;; Filtering/Deleting
 string-filter
 string-delete
 )

(dynamic-call "scm_init_srfi_13_14" (dynamic-link "libguile-srfi-srfi-13-14"))

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
