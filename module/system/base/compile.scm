;;; High-level compiler interface

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

(define-module (system base compile)
  :use-module (system base language)
  :use-module (ice-9 regex)
  :export (compile-file object-file-name))

(define scheme (lookup-language 'scheme))

(define (compile-file file)
  (let ((comp (object-file-name file)))
    (call-with-compile-error-catch
     (lambda ()
       (catch #t
	 (lambda ()
	   (call-with-output-file comp
	     (lambda (port)
	       (uniform-array-write (compile-file-in file scheme) port))))
	 (lambda (key . args)
	   (format #t "ERROR: In ~A:\n" file)
	   (display "ERROR: ")
	   (format #t (cadr args) (caddr args))
	   (newline)
	   (delete-file comp)))))
    (format #t "Wrote ~A\n" comp)))

(define (object-file-name file)
  (let ((m (string-match "\\.[^.]*$" file)))
    (string-append (if m (match:prefix m) file) ".go")))
