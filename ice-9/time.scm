;;;; 	Copyright (C) 2001 Free Software Foundation, Inc.
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


(define-module (ice-9 time)
  :use-module (ice-9 format)
  :export (time))

(define-macro (time form)
  (let* ((gc-start (gc-run-time))
	 (tms-start (times))
	 (result (eval form (interaction-environment)))
	 (tms-end (times))
	 (gc-end (gc-run-time)))
    (define (get proc start end)
      (/ (- (proc end) (proc start)) internal-time-units-per-second))
    (display "clock utime stime cutime cstime gctime\n")
    (format #t "~5,2F ~5,2F ~5,2F ~6,2F ~6,2F ~6,2F\n"
	    (get tms:clock tms-start tms-end)
	    (get tms:utime tms-start tms-end)
	    (get tms:stime tms-start tms-end)
	    (get tms:cutime tms-start tms-end)
	    (get tms:cstime tms-start tms-end)
	    (get id gc-start gc-end))
    result))
