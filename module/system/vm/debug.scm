;;; Guile VM debugging facilities

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

(define-module (system vm debug)
  :use-syntax (system base syntax)
  :use-module (system vm core)
  :use-module (system vm frame)
  :use-module (ice-9 format)
  :export (vm-debugger vm-backtrace))


;;;
;;; Debugger
;;;

(define-record (<debugger> vm chain index))

(define (vm-debugger vm)
  (let ((chain (vm-last-frame-chain vm)))
    (if (null? chain)
      (display "Nothing to debug\n")
      (debugger-repl (<debugger> :vm vm :chain chain :index (length chain))))))

(define (debugger-repl db)
  (let loop ()
    (display "debug> ")
    (let ((cmd (read)))
      (case cmd
	((bt) (vm-backtrace db.vm))
	((stack)
	 (write (vm-fetch-stack db.vm))
	 (newline))
	(else
	 (format #t "Unknown command: ~A" cmd))))))


;;;
;;; Backtrace
;;;

(define (vm-backtrace vm)
  (let ((chain (vm-last-frame-chain vm)))
    (if (null? chain)
      (display "No backtrace available\n")
      (for-each print-frame (reverse! chain)))))
