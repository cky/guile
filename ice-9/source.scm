;;;; source.scm --- finding lines of source code for the debugger to display
;;;; Jim Blandy <jimb@cyclic.com> --- February 1997
;;;;
;;;; 	Copyright (C) 1997 Free Software Foundation, Inc.
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
;;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This module retrieves selected lines from files of source code.
;;; It's meant for use by source-level debuggers, to display regions
;;; of code to the user.
;;;

;;; We don't keep the text in memory.  However, we do cache the
;;; starting positions of each line in the file, so once the file has
;;; been scanned, we can extract any line we want pretty quickly.  We
;;; also only scan as far as we need, i.e., up to the latest line
;;; we've been asked for.  This is fast enough for interactive use,
;;; but doesn't take up much memory.

;(define-module '(ice-9 source))


;;;; public interface

;;; (get-source-line FILE LINE)
;;; Return the text of the LINE'th line from the file FILE, as a
;;; string.
(define-public (get-source-line file line)
  ...)

;;; (check-source-cache FILE)
;;; The source access routines in this module cache some information
;;; about the contents of the files they examine.  If we have cached
;;; any information about FILE, check its modification time, and forget
;;; our cached information if it is out of date.
;;;
;;; Try to call this function periodically before retrieving lines
;;; from FILE; however, it does perform a file system access, so don't
;;; call it too often.


;;;; trivia

(define (throw-out-of-range op index)
  (throw 'out-of-range op "Argument out of range: %S" (list index) #f))


;;;; stretchy vectors

;;; A stretchy vector is one that you can append new entries onto
;;; efficiently.

;;; (make-svect) => a new, zero-length stretchy vector
(define (make-svect)
  (cons 0 (make-vector 20 '-)))

;;; (svect-ref SVECT K) => the K'th element of SVECT
(define (svect-ref svect k)
  (vector-ref (cdr svect) k))

;;; (svect-add! SVECT ELT) => append ELT to the end of SVECT
(define (svect-add! svect elt)

  ;; If we don't have room in the current vector, create a new, larger one.
  (if (>= (car svect) (vector-length (cdr svect)))
      (let* ((old-vect (cdr svect))
	     (new-vect (make-vector (* 2 (vector-length old-vect)) '-)))
	(vector-move-left! old-vect 0 (vector-length old-vect)
			   new-vect 0)
	(set-cdr! svect new-vect)))

  (vector-set! (cdr svect) (car svect) elt)
  (set-car! svect (+ 1 (car svect))))

;;; (svect-length SVECT) => the length of SVECT
(define svect-length car)


;;;; source files

;;; We keep a limited number of source files open at any given time.
;;; A source file structure looks like:
;;;
;;;   (PORT LINESTARTS)
;;;
;;; PORT is a seekable input port looking at a text file.
;;; LINESTARTS is a stretchy vector, whose Ith element is the seek
;;;	position of the start of line I (assuming lines are numbered
;;;	starting with zero).  The file may have more lines than this
;;;	array records; this occurs when we haven't completely scanned
;;;	the file.
(define (make-source file)
  (let ((port (open-input-file file)))
    (let ((linestarts (make-svect)))
      (svect-add! linestarts 0)		; We know where the first line starts.
      (list port linestarts))))

;;; Return the text of the LINE'th line of SOURCE.  Return the EOF
;;; object if SOURCE doesn't have that many lines.
(define (get-line source line)
  (let* ((port (car source))
	 (linestarts (cadr source))
	 (known (svect-length linestarts)))

    ;; Starting at the last line whose position is known, scan SOURCE for
    ;; new line positions, until we've found the start of the LINE'th
    ;; line.  Assume that LINE is not one whose position we already know.
    ;; Record all line positions in SOURCE.  Return the text of
    ;; the LINE'th line (without a newline), or the EOF object if the
    ;; file doesn't have that many lines.
    (define (scan line)
      (fseek port (svect-ref linestarts (- known 1)) SEEK_SET)
      (let loop ((known known))
	(let ((text (read-line port)))
	  (if (eof-object? text) text
	      (begin
		(svect-add! linestarts (ftell port))
		(if (>= line known) (loop (+ known 1))
		    text))))))

    ;; Return the text of the line that starts at POS.
    (define (get pos)
      (fseek port pos SEEK_SET)
      (read-line port))

    (if (< line known)
	(get (svect-ref linestarts line))
	(scan line))))


;;;; cache of files

;;; The cache is an alist mapping filenames onto source objects.  We
;;; put a fixed limit on the number of files it contains; new files replace
;;; the least recently used file.

(define (cache-add! cache max file source)
  (let* ((cache (cons (cons file source) cache))
	 (
    (set-cdr! 

(define (cache-ref file)
  



(define the-file-cache '())
(define file-cache-max 10)

