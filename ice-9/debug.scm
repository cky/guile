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
     (,interface (append flags (,interface)))
     (,interface)))

(define (make-disable interface)
  `(lambda flags
     (let ((options (,interface)))
       (for-each (lambda (flag)
		   (set! options (delq! flag options)))
		 flags)
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


;;; {Trace}
;;;
;;; This code is just an experimental prototype (e. g., it is not
;;; thread safe), but since it's at the same time useful, it's
;;; included anyway.
;;;
(define traced-procedures '())

(define-public (trace . args)
  (if (null? args)
      (nameify traced-procedures)
      (begin
	(for-each (lambda (proc)
		    (if (not (procedure? proc))
			(error "trace: Wrong type argument:" proc))
		    (set-procedure-property! proc 'trace #t)
		    (if (not (memq proc traced-procedures))
			(set! traced-procedures
			      (cons proc traced-procedures))))
		  args)
	(set! apply-frame-handler trace-entry)
	(set! exit-frame-handler trace-exit)
	(set! trace-level 0)
	(debug-enable 'trace)
	(nameify args))))

(define-public (untrace . args)
  (if (and (null? args)
	   (not (null? traced-procedures)))
      (apply untrace traced-procedures)
      (begin
	(for-each (lambda (proc)
		    (set-procedure-property! proc 'trace #f)
		    (set! traced-procedures (delq! proc traced-procedures)))
		  args)
	(if (null? traced-procedures)
	    (debug-disable 'trace))
	(nameify args))))

(define (nameify ls)
  (map (lambda (proc)
	 (let ((name (procedure-name proc)))
	   (or name proc)))
       ls))

(define trace-level 0)
(add-hook! abort-hook (lambda () (set! trace-level 0)))

(define (trace-entry key cont tail)
  (if (eq? (stack-id cont) 'repl-stack)
      (let ((cep (current-error-port))
	    (frame (last-stack-frame cont)))
	(if (not tail)
	    (set! trace-level (+ trace-level 1)))
	(let indent ((n trace-level))
	  (cond ((> n 1) (display "|  " cep) (indent (- n 1)))))
	(display-application frame cep)
	(newline cep)))
  (debug-enable 'trace)
  ;; It's not necessary to call the continuation since
  ;; execution will continue if the handler returns
  ;(cont #f)
  )

(define (trace-exit key cont retval)
  (if (eq? (stack-id cont) 'repl-stack)
      (let ((cep (current-error-port)))
	(set! trace-level (- trace-level 1))
	(let indent ((n trace-level))
	  (cond ((> n 0) (display "|  " cep) (indent (- n 1)))))
	(write retval cep)
	(newline cep)))
  (debug-enable 'trace))

(define (display-application frame port)
  (display #\[ port)
  (display (car (unmemoize (frame-source frame))) port)
  (let loop ((args (frame-arguments frame)))
    (if (not (null? args))
	(begin
	  (display #\space port)
	  (write (car args) port)
	  (loop (cdr args)))))
  (display #\] port))


;;; A fix to get the error handling working together with the module system.
;;;
(variable-set! (builtin-variable 'debug-options) debug-options)



(debug-enable 'debug)
(read-enable 'positions)
