;;; utils.scm --- 

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; This file is part of Guile VM.

;; Guile VM is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; Guile VM is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with Guile VM; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (vm utils)
  :use-module (ice-9 and-let*)
  :use-module (ice-9 format))

(export and-let*)

(define-public (assert predicate obj)
  (if (not (predicate obj))
      (scm-error 'wrong-type-arg #f
		 "Wrong type argument: ~S, ~S"
		 (list (procedure-name predicate) obj) #f)))

(define-public (assert-for-each predicate list)
  (for-each (lambda (x) (assert predicate x)) list))

(define-public (check-nargs args pred n)
  (if (not (pred (length args) n))
      (error "Too many or few arguments")))

(define-public (last list)
  (car (last-pair list)))

(define-public (rassq key alist)
  (let loop ((alist alist))
    (cond ((null? alist) #f)
	  ((eq? key (cdar alist)) (car alist))
	  (else (loop (cdr alist))))))

(define-public (rassq-ref alist key)
  (let ((obj (rassq key alist)))
    (if obj (car obj) #f)))

(define-public (map-if pred func list)
  (let loop ((list list) (result '()))
    (if (null? list)
	(reverse! result)
	(if (pred (car list))
	    (loop (cdr list) (cons (func (car list)) result))
	    (loop (cdr list) result)))))

(define-public (map-tree func tree)
  (cond ((null? tree) '())
	((pair? tree)
	 (cons (map-tree func (car tree)) (map-tree func (cdr tree))))
	(else (func tree))))

(define-public (filter pred list)
  (let loop ((list list) (result '()))
    (if (null? list)
	(reverse! result)
	(if (pred (car list))
	    (loop (cdr list) (cons (car list) result))
	    (loop (cdr list) result)))))

(define-public (uniq! list)
  (do ((rest list (begin (set-cdr! rest (delq! (car rest) (cdr rest)))
			 (cdr rest))))
      ((null? rest) list)))

(define-public (finalize obj)
  (if (promise? obj) (force obj) obj))

(export time)
(define-macro (time form)
  `(let* ((gc-start (gc-run-time))
	  (tms-start (times))
	  (result ,form)
	  (tms-end (times))
	  (gc-end (gc-run-time))
	  (get (lambda (proc start end)
		 (/ (- (proc end) (proc start))
		    internal-time-units-per-second))))
     (display "clock utime stime cutime cstime gc\n")
     (format #t "~5a ~5a ~5a ~6a ~6a ~a~%"
	     (get tms:clock tms-start tms-end)
	     (get tms:utime tms-start tms-end)
	     (get tms:stime tms-start tms-end)
	     (get tms:cutime tms-start tms-end)
	     (get tms:cstime tms-start tms-end)
	     (get id gc-start gc-end))
     result))

;;; utils.scm ends here
