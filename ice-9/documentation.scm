;;;; 	Copyright (C) 2000 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

(define-module (ice-9 documentation)
  :use-module (ice-9 rdelim)
  :no-backtrace)



;;
;; documentation-files is the list of places to look for documentation
;;
(define-public documentation-files
  (map (lambda (vicinity)
	 (in-vicinity (vicinity) "guile-procedures.txt"))
       (list %library-dir
	     %package-data-dir
	     %site-dir
	     (lambda () "."))))

(define (find-documentation name)
  (or-map (lambda (file)
	    (find-documentation-in-file name file))
	  documentation-files))

(define entry-delimiter "\f")

(define (find-documentation-in-file name file)
  (and (file-exists? file)
       (let ((port (open-input-file file))
	     (name (symbol->string name)))
	 (let ((len (string-length name)))
	   (read-delimited entry-delimiter port) ;skip to first entry
	   (let loop ((entry (read-delimited entry-delimiter port)))
	     (cond ((eof-object? entry) #f)
		   ;; match?
		   ((and ;; large enough?
		         (>= (string-length entry) len)
			 ;; matching name?
			 (string=? (substring entry 0 len) name)
			 ;; terminated?
			 (memq (string-ref entry len) '(#\newline)))
		    ;; cut away name tag and extra surrounding newlines
		    (substring entry (+ len 2) (- (string-length entry) 2)))
		   (else (loop (read-delimited entry-delimiter port)))))))))

;; helper until the procedure documentation property is cleaned up
(define (proc-doc proc)
  (or (procedure-documentation proc)
      (procedure-property proc 'documentation)))

(define-public (object-documentation object)
  "Return the docstring for OBJECT."
  (or (and (procedure? object)
	   (proc-doc object))
      (and (macro? object)
	   (let ((transformer (macro-transformer object)))
	     (and transformer
		  (proc-doc transformer))))
      (object-property object 'documentation)
      ;; find-documentation currently only works for builtin primitives
      (and (procedure? object)
	   (not (closure? object))
	   (procedure-name object)
	   (let ((docstring (find-documentation (procedure-name object))))
	     (if docstring
		 (set-procedure-property! object 'documentation docstring))
	     docstring))))
