;;; Guile VM profiler

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

(define-module (system vm profile)
  :use-module (system vm core)
  :use-module (ice-9 format)
  :export (vm-profile))

(define (vm-profile vm prog . opts)
  (let ((flag (vm-option vm 'debug)))
    (dynamic-wind
	(lambda ()
	  (set-vm-option! vm 'debug #t)
	  (set-vm-option! vm 'profile-data '())
	  (add-hook! (vm-next-hook vm) profile-next)
	  (add-hook! (vm-enter-hook vm) profile-enter)
	  (add-hook! (vm-exit-hook vm) profile-exit))
	(lambda ()
	  (let ((val (vm prog)))
	    (display-result vm)
	    val))
	(lambda ()
	  (set-vm-option! vm 'debug flag)
	  (remove-hook! (vm-next-hook vm) profile-next)
	  (remove-hook! (vm-enter-hook vm) profile-enter)
	  (remove-hook! (vm-exit-hook vm) profile-exit)))))

(define (profile-next vm)
  (set-vm-option! vm 'profile-data
		  (cons (vm-fetch-code vm) (vm-option vm 'profile-data))))

(define (profile-enter vm)
  #f)

(define (profile-exit vm)
  #f)

(define (display-result vm . opts)
  (do ((data (vm-option vm 'profile-data) (cdr data))
       (summary '() (let ((inst (caar data)))
		      (assq-set! summary inst
				 (1+ (or (assq-ref summary inst) 0))))))
      ((null? data)
       (display "Count  Instruction\n")
       (display "-----  -----------\n")
       (for-each (lambda (entry)
		   (format #t "~5@A  ~A\n" (cdr entry) (car entry)))
		 (sort summary (lambda (e1 e2) (> (cdr e1) (cdr e2))))))))
