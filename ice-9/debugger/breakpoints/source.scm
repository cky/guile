;;;; (ice-9 debugger breakpoints source) -- source location breakpoints

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

(define-module (ice-9 debugger breakpoints source)
  #:use-module (ice-9 format)
  #:use-module (ice-9 debugger breakpoints)
  #:use-module (ice-9 debugger trap-hooks)
  #:use-module (ice-9 debugger trc)
  #:use-module (ice-9 debugger utils)
  #:use-module (oop goops)
  #:export (<source-breakpoint>
	    bp-location
	    bp-expression))

;;; {Source Breakpoints}
;;;
;;; Breakpoints that activate upon reaching a particular source
;;; location or range of source locations.

(define-generic bp-location)
(define-generic bp-expression)

(define-class <source-breakpoint> (<breakpoint>)

  ;; The location of this breakpoint.
  (location #:accessor bp-location
	    #:init-keyword #:location)

  ;; The source expression at this breakpoint.
  (expression #:accessor bp-expression
	      #:init-keyword #:expression)

  ;; Counter: incremented when the breakpoint is set, decremented when
  ;; a source expression using this breakpoint applied has been GC'd.
  (use-count #:accessor bp-use-count
	     #:init-value 0))

(define (location->string filename line column)
  (format #f "~A:~A:~A" filename (+ line 1) (+ column 1)))

(define-method (bp-message (bp <source-breakpoint>) message port)
  (format port
	  "~A ~A: ~A: ~S\n"
	  message
	  (bp-number bp)
	  (apply location->string (bp-location bp))
	  (bp-expression bp)))

(define-method (bp-describe (bp <source-breakpoint>) port)
  (next-method)
  (if (zero? (bp-use-count bp))
      (format port "\t(this breakpoint is a zombie)\n"))
  *unspecified*)

;;; Alist of all source breakpoints:
;;;   ((LOCATION . BREAKPOINT) ...)
;;; where LOCATION is
;;;   (FILE-NAME LINE COLUMN)
;;; Keys are unique according to `equal?'.

(define source-breakpoints '())

(define-method (get-breakpoint (filename <string>)
			       (line <integer>)
			       (column <integer>))
  (assoc-ref source-breakpoints (list filename line column)))

;;; When the source expression that a breakpoint is attached to is
;;; GC'd, typically because the variable that included it in its value
;;; has been redefined, we'd like to mark the breakpoint as no longer
;;; relevant.  We do this by using a property guardian ...

(define (make-property-guardian)
  ;; Return a new property guardian.  A property guardian is a
  ;; combination of a guardian and an object property that accepts KEY
  ;; -> VALUE associations and gives you back the VALUE when its KEY
  ;; has been garbage collected.
  ;;
  ;; To store an association, call it in the same way as you would an
  ;; object property: (set! (PROPERTY-GUARDIAN KEY) VALUE).
  ;;
  ;; To retrieve the VALUE for a KEY that has been GC'd, call the
  ;; property guardian in the same way as you would a guardian, with
  ;; no args: (PROPERTY-GUARDIAN).
  (let ((p (make-object-property))
	(g (make-guardian)))
    (make-procedure-with-setter
     (lambda ()
       (let ((collected (g)))
	 (and collected (car collected))))
     (lambda (key value)
       (let ((collectible (list value)))
	 ;; Store the collectible value both as an object property,
	 ;; and in the guardian.
	 (set! (p key) collectible)
	 (g collectible))))))

(define source-breakpoint-guardian (make-property-guardian))

(add-hook! after-gc-hook
	   (lambda ()
	     (let loop ((bp (source-breakpoint-guardian)))
	       (if bp
		   (let ((new-use-count (- (bp-use-count bp) 1)))
		     (set! (bp-use-count bp) new-use-count)
		     (if (zero? new-use-count)
			 (bp-message bp "Zombified breakpoint" #t))
		     (loop (source-breakpoint-guardian)))))))

(define (add-breakpoint filename line column expression)
  (let* ((location (list filename line column))
	 (bp (make <source-breakpoint>
	       #:location location
	       #:expression (if (pair? expression)
				;; The point of this strange looking
				;; copy is to copy the expression
				;; without its source properties.
				;; This is necessary to allow the
				;; source properties to be GC'd when
				;; the source expression becomes
				;; obsolete.  (Note that `copy-tree'
				;; copies source properties as well!)
				(cons (car expression) (cdr expression))
				expression))))
    (set! source-breakpoints (assoc-set! source-breakpoints location bp))
    bp))

(define-method (set-breakpoint! behaviour
				x-as-read
				(x-pairified <pair>))
  (let ((filename (source-property x-pairified 'filename))
	(line     (source-property x-pairified 'line))
	(column   (source-property x-pairified 'column)))
    (let ((bp (or (get-breakpoint filename line column)
		  (add-breakpoint filename line column x-as-read))))
      (set! (bp-behaviour bp) behaviour)
      (install-breakpoint x-pairified bp)
      (bp-message bp "Set breakpoint" #t)
      bp)))

(define (install-breakpoint x bp)
  ;; Make the necessary connections with the specified expression and
  ;; its breakpoint.
  (set-source-property! x 'breakpoint #t)
  (set! (source-breakpoint-guardian x) bp)
  (set! (bp-use-count bp) (+ (bp-use-count bp) 1))
  (remove/install-source-breakpoint-hooks))

(define remove/install-source-breakpoint-hooks
  (let ((hooks-installed? #f))
    (lambda ()
      (cond ((and hooks-installed?
		  (null? source-breakpoints))
	     (remove-hook! before-enter-frame-hook
			   source-before-enter-frame-hook)
	     (remove-breakpoint-hook! source-breakpoint-hook)
	     (set! hooks-installed? #f))
	    ((and (not hooks-installed?)
		  (not (null? source-breakpoints)))
	     (add-hook! before-enter-frame-hook
			source-before-enter-frame-hook)
	     (add-breakpoint-hook! source-breakpoint-hook)
	     (set! hooks-installed? #t))))))

(define *cont* #f)

(define (source-before-enter-frame-hook cont . ignored)
  (trc 'source-before-enter-frame-hook)
  (set! *cont* cont))

(define (source-breakpoint-hook)
  (trc 'source-breakpoint-hook)
  (let* ((frame (last-stack-frame *cont*))
	 (source (frame-source frame))
	 (position (and source (source-position source)))
	 (bp (and position (apply get-breakpoint position))))
    (if bp
	(bp-run bp))))

(define-method (bp-delete! (bp <source-breakpoint>))
  (set! source-breakpoints (assoc-remove! source-breakpoints (bp-location bp)))
  (remove/install-source-breakpoint-hooks)
  (bp-message bp "Deleted breakpoint" #t)
  *unspecified*)

(register-breakpoint-subclass <source-breakpoint>
			      (lambda ()
				(map cdr source-breakpoints)))

(read-hash-extend #\#
		  (lambda (c port)
		    (let (;; Save off port coordinates before reading
			  ;; the following expression, as we'll need
			  ;; to install source coordinates by hand if
			  ;; the expression turns out not to be a
			  ;; pair.
			  (filename (port-filename port))
			  (line (port-line port))
			  (column (port-column port)))
		      ;; Now read the marked expression.
		      (let* ((x (read port))
			     (x' (if (pair? x)
				     x
				     ;; The marked expression isn't a
				     ;; pair, so it can't carry source
				     ;; properties by itself.
				     ;; Therefore we pretend instead
				     ;; to have read `(begin X)', and
				     ;; attach coordinate and
				     ;; breakpoint information to the
				     ;; begin expression.
				     (let ((x' (list begin x)))
				       (set-source-property! x' 'filename
							     filename)
				       (set-source-property! x' 'line
							     line)
				       (set-source-property! x' 'column
							     column)
				       x'))))
			;; Don't allow breakpointed expression to have
			;; a filename property that isn't a string.
			(or (string? filename)
			    (set-source-property! x' 'filename "<unnamed port>"))
			(break! x x')
			x'))))

(read-enable 'positions)

;;; (ice-9 debugger breakpoints source) ends here.
