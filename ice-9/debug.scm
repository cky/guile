;;;; 	Copyright (C) 1996 Mikael Djurfeldt
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
;;;;
;;;; The author can be reached at djurfeldt@nada.kth.se
;;;; Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
;;;;


(define-module #/ice-9/debug)



;;; {Run-time options}

(define names '((debug-options-interface
		 (debug-options debug-enable debug-disable)
		 (debug-set!))
		
		(evaluator-traps-interface
		 (traps trap-enable trap-disable)
		 (trap-set!))
		
		(read-options-interface
		 (read-options read-enable read-disable)
		 (read-set!))
		
		(print-options-interface
		 (print-options print-enable print-disable)
		 (print-set!))
		))

(define option-name car)
(define option-value cadr)
(define option-documentation caddr)

(define (print-option option)
  (display (option-name option))
  (if (< (string-length (symbol->string (option-name option))) 8)
      (display #\tab))
  (display #\tab)
  (display (option-value option))
  (display #\tab)
  (display (option-documentation option))
  (newline))

;;; Below follows the macros defining the run-time option interfaces.
;;; *fixme* These should not be macros, but need to be until module
;;; system is improved.
;;;

(define (make-options interface)
  `(lambda args
     (cond ((null? args) (,interface))
	   ((pair? (car args)) (,interface (car args)) (,interface))
	   (else (for-each print-option (,interface #t))))))

(define (make-enable interface)
  `(lambda flags
     (,interface (append (if (pair? flags)
			     flags
			     (list flags))
			 (,interface)))
     (,interface)))

(define (make-disable interface)
  `(lambda flags
     (let ((options (,interface)))
       (for-each (lambda (flag)
		   (set! options (delq! flag options)))
		 (if (pair? flags) flags (list flags)))
       (,interface options)
       (,interface))))

(define (make-set! interface)
  `((name exp)
    (,'quasiquote
     (begin (,interface (append (,interface)
				(list '(,'unquote name)
				      (,'unquote exp))))
	    (,interface)))))
       
(defmacro define-all ()
  (cons 'begin
	(apply append
	       (map (lambda (group)
		      (let ((interface (car group)))
			(append (map (lambda (name constructor)
				       `(define-public ,name
					  ,(constructor interface)))
				     (cadr group)
				     (list make-options
					   make-enable
					   make-disable))
				(map (lambda (name constructor)
				       `(defmacro-public ,name
					  ,@(constructor interface)))
				     (caddr group)
				     (list make-set!)))))
		    names))))

(define-all)
