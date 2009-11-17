;;; Guile VM debugging facilities

;;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system vm debug)
  #:use-module (system base syntax)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (ice-9 format)
  #:export (vm-debugger vm-backtrace))


;;;
;;; Debugger
;;;

(define-record/keywords <debugger> vm chain index)

(define (vm-debugger vm)
  (let ((chain (vm-last-frame-chain vm)))
    (if (null? chain)
      (display "Nothing to debug\n")
      (debugger-repl (make-debugger
                      #:vm vm #:chain chain #:index (length chain))))))

(define (debugger-repl db)
  (let loop ()
    (display "debug> ")
    (let ((cmd (read)))
      (case cmd
	((bt) (vm-backtrace (debugger-vm db)))
	((stack)
	 (write (vm-fetch-stack (debugger-vm db)))
	 (newline))
	(else
	 (format #t "Unknown command: ~A" cmd))))))


;;;
;;; Backtrace
;;;

(define (vm-backtrace vm)
  (print-frame-chain-as-backtrace
   (reverse (vm-last-frame-chain vm))))
