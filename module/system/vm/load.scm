;;; Guile VM compiling loader

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

(define-module (system vm load)
  :use-module (system vm core)
  :autoload (system base language) (compile-file-in lookup-language)
  :use-module (ice-9 regex)
  :export (load/compile))

(define (load/compile file)
  (let* ((file (file-name-full-name file))
	 (compiled (object-file-name file)))
    (if (or (not (file-exists? compiled))
	    (> (stat:mtime (stat file)) (stat:mtime (stat compiled))))
	(compile-file-in file #f (lookup-language 'gscheme) #:O))
    (vm-load (the-vm) (load-dumpcode compiled))))

(define (file-name-full-name filename)
  (let ((oldname (and (current-load-port)
		      (port-filename (current-load-port)))))
    (if (and oldname
	     (> (string-length filename) 0)
	     (not (char=? (string-ref filename 0) #\/))
	     (not (string=? (dirname oldname) ".")))
	(string-append (dirname oldname) "/" filename)
	filename)))

(define (object-file-name file)
  (let ((m (string-match "\\.[^.]*$" file)))
    (string-append (if m (match:prefix m) file) ".go")))
