;;;; (ice-9 debugger breakpoints range) -- experimental range breakpoints

;;; Copyright (C) 2002 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA
;;;
;;; As a special exception, the Free Software Foundation gives permission
;;; for additional uses of the text contained in its release of GUILE.
;;;
;;; The exception is that, if you link the GUILE library with other files
;;; to produce an executable, this does not by itself cause the
;;; resulting executable to be covered by the GNU General Public License.
;;; Your use of that executable is in no way restricted on account of
;;; linking the GUILE library code into it.
;;;
;;; This exception does not however invalidate any other reasons why
;;; the executable file might be covered by the GNU General Public License.
;;;
;;; This exception applies only to the code released by the
;;; Free Software Foundation under the name GUILE.  If you copy
;;; code from other Free Software Foundation releases into a copy of
;;; GUILE, as the General Public License permits, the exception does
;;; not apply to the code that you add in this way.  To avoid misleading
;;; anyone as to the status of such modified files, you must delete
;;; this exception notice from them.
;;;
;;; If you write modifications of your own for GUILE, it is your choice
;;; whether to permit this exception to apply to your modifications.
;;; If you do not wish that, delete this exception notice.

(define-module (ice-9 debugger breakpoints range)
  #:use-module (ice-9 debugger breakpoints)
  #:use-module (ice-9 debugger trap-hooks)
  #:use-module (ice-9 debugger trc)
  #:use-module (ice-9 debugger utils)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:export (<range-breakpoint>
	    bp-range))

;;; {Range Breakpoints}
;;;
;;; Breakpoints that activate upon entry to a frame whose source lies
;;; in a specified range.

(define-generic bp-range)

(define-class <range-breakpoint> (<breakpoint>)

  ;; The range of this breakpoint.
  (range #:accessor bp-range #:init-keyword #:range))

(define (range->string filename from-line from-column to-line to-column)
  (if (positive? from-line)
      (format #f "~A:~A:~A-~A:~A" filename
	      (+ from-line 1) (+ from-column 1)
	      (+ to-line 1) (+ to-column 1))
      (format #f "~A (whole file)" filename)))

(define-method (bp-message (bp <range-breakpoint>) message port)
  (format port
	  "~A ~A: ~A\n"
	  message
	  (bp-number bp)
	  (apply range->string (bp-range bp))))

;;; Alist of all range breakpoints:
;;;   ((RANGE . BREAKPOINT) ...)
;;; where RANGE is
;;;   (FILE-NAME FROM-LINE FROM-COLUMN TO-LINE TO-COLUMN)
;;; Keys are unique according to `equal?'.

(define range-breakpoints '())

(define-method (get-breakpoint (filename <string>)
			       (from-line <integer>)
			       (from-column <integer>)
			       (to-line <integer>)
			       (to-column <integer>))
  (assoc-ref range-breakpoints
	     (if (positive? from-line)
		 (list filename
		       (- from-line 1)
		       (- from-column 1)
		       (- to-line 1)
		       (- to-column 1))
		 (list filename 0 0 0 0))))

(define-method (get-breakpoint (filename <string>))
  (get-breakpoint filename 0 0 0 0))

(define-method (get-breakpoint (filename <string>)
			       (line <integer>))
  (get-breakpoint filename line 1 (+ line 1) 1))

(define-method (get-breakpoint (filename <string>)
			       (from-line <integer>)
			       (to-line <integer>))
  (get-breakpoint filename from-line 1 to-line 1))

(define (add-breakpoint filename from-line from-column to-line to-column)
  (let* ((range (if (positive? from-line)
		    (list filename
			  (- from-line 1)
			  (- from-column 1)
			  (- to-line 1)
			  (- to-column 1))
		    (list filename 0 0 0 0)))
	 (bp (make <range-breakpoint> #:range range)))
    (set! range-breakpoints (assoc-set! range-breakpoints range bp))
    (remove/install-range-breakpoint-hooks)
    bp))

(define-method (set-breakpoint! behaviour
				(filename <string>)
				(from-line <integer>)
				(from-column <integer>)
				(to-line <integer>)
				(to-column <integer>))
  (let ((bp (or (get-breakpoint filename from-line from-column to-line to-column)
		(add-breakpoint filename from-line from-column to-line to-column))))
    (set! (bp-behaviour bp) behaviour)
    (bp-message bp "Set breakpoint" #t)
    bp))

(define-method (set-breakpoint! behaviour
				(filename <string>))
  (set-breakpoint! behaviour filename 0 0 0 0))

(define-method (set-breakpoint! behaviour
				(filename <string>)
				(line <integer>))
  (set-breakpoint! behaviour filename line 1 (+ line 1) 1))

(define-method (set-breakpoint! behaviour
				(filename <string>)
				(from-line <integer>)
				(to-line <integer>))
  (set-breakpoint! behaviour filename from-line 1 to-line 1))

(define remove/install-range-breakpoint-hooks
  (let ((hooks-installed? #f))
    (lambda ()
      (cond ((and hooks-installed?
		  (null? range-breakpoints))
	     (remove-hook! before-enter-frame-hook
			   range-before-enter-frame-hook)
	     (remove-enter-frame-hook! range-enter-frame-hook)
	     (set! hooks-installed? #f))
	    ((and (not hooks-installed?)
		  (not (null? range-breakpoints)))
	     (add-hook! before-enter-frame-hook
			range-before-enter-frame-hook)
	     (add-enter-frame-hook! range-enter-frame-hook)
	     (set! hooks-installed? #t))))))

(define *cont* #f)

(define (range-before-enter-frame-hook cont . ignored)
  (trc 'range-before-enter-frame-hook)
  (set! *cont* cont))

(define (range-enter-frame-hook)
  (trc 'range-enter-frame-hook)
  (let* ((frame (last-stack-frame *cont*))
	 (source (frame-source frame))
	 (position (and source (source-position source))))
    (if position
	(for-each (lambda (range bp)
		    (if (apply position-in-range position range)
			(bp-run bp)))
		  (map car range-breakpoints)
		  (map cdr range-breakpoints)))))

(define (position-in-range position
			   filename
			   from-line
			   from-column
			   to-line
			   to-column)
  (and (string=? (car position) filename)
       (if (positive? from-line)
	   (let ((pline (cadr position))
		 (pcolumn (caddr position)))
	     (and (or (and (= pline from-line)
			   (>= pcolumn from-column))
		      (> pline from-line))
		  (or (and (= pline to-line)
			   (< pcolumn to-column))
		      (< pline to-line))))
	   #t)))

(define-method (bp-delete! (bp <range-breakpoint>))
  (set! range-breakpoints
	(assoc-remove! range-breakpoints (bp-range bp)))
  (remove/install-range-breakpoint-hooks)
  (bp-message bp "Deleted breakpoint" #t)
  *unspecified*)

(register-breakpoint-subclass <range-breakpoint>
			      (lambda ()
				(map cdr range-breakpoints)))

;;; (ice-9 debugger breakpoints range) ends here.
