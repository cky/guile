;;;; (ice-9 debugger trc) -- tracing for Guile debugger code

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

(define-module (ice-9 debugger trc)
  #:export (trc trc-syms trc-all trc-none trc-add trc-remove trc-port))

(define *syms* #f)

(define (trc-set! syms)
  (set! *syms* syms))

(define (trc-syms . syms)
  (trc-set! syms))

(define (trc-all)
  (trc-set! #f))

(define (trc-none)
  (trc-set! '()))

(define (trc-add sym)
  (trc-set! (cons sym *syms*)))

(define (trc-remove sym)
  (trc-set! (delq1! sym *syms*)))

(define (trc sym . args)
  (if (or (not *syms*)
	  (memq sym *syms*))
      (let ((port (trc-port)))
	(write sym port)
	(display ":" port)
	(for-each (lambda (arg)
		    (display " " port)
		    (write arg port))
		  args)
	(newline port))))

(define trc-port
  (let ((port (current-error-port)))
    (make-procedure-with-setter
     (lambda () port)
     (lambda (p) (set! port p)))))

;; Default to no tracing.
(trc-none)

;;; (ice-9 debugger trc) ends here.
