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

;;;; R5RS bindings

(define-module (ice-9 r5rs))

(module-use! %module-public-interface (resolve-interface '(ice-9 safe-r5rs)))

(export scheme-report-environment
	interaction-environment

	call-with-input-file call-with-output-file
	with-input-from-file with-output-to-file
	open-input-file open-output-file
	close-input-port close-output-port

	load
	;;transcript-on
	;;transcript-off
	)

(define scheme-report-interface %module-public-interface)

(define (scheme-report-environment n)
  (if (not (= n 5))
      (scm-error 'misc-error 'scheme-report-environment
		 "~A is not a valid version"
		 (list n)
		 '()))
  scheme-report-interface)
