;;;; 	Copyright (C) 1996, 1997 Mikael Djurfeldt
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

;;; *******************************
;;; * Experimental hack           *
;;; * Shouldn't go into snapshots *
;;; * Don't distribute!           *
;;; *******************************

(define-module (ice-9 gtcl) :use-module (ice-9 debug))

(define-public TCL_VERSION "7.4")
(define-public TCL_MAJOR_VERSION 7)
(define-public TCL_MINOR_VERSION 4)

;;; When a TCL command returns, the string pointer interp->result points to
;;; a string containing return information from the command.  In addition,
;;; the command procedure returns an integer value, which is one of the
;;; following:
;;;
;;; TCL_OK		Command completed normally;  interp->result contains
;;;			the command's result.
;;; TCL_ERROR		The command couldn't be completed successfully;
;;;			interp->result describes what went wrong.
;;; TCL_RETURN		The command requests that the current procedure
;;;			return;  interp->result contains the procedure's
;;;			return value.
;;; TCL_BREAK		The command requests that the innermost loop
;;;			be exited;  interp->result is meaningless.
;;; TCL_CONTINUE		Go on to the next iteration of the current loop;
;;;			interp->result is meaningless.
;;;

(define-public TCL_OK		0)
(define-public TCL_ERROR	1)
(define-public TCL_RETURN	2)
(define-public TCL_BREAK	3)
(define-public TCL_CONTINUE	4)

;;; Flag values passed to variable-related procedures.
;;;

(define-public TCL_GLOBAL_ONLY		1)
(define-public TCL_APPEND_VALUE	2)
(define-public TCL_LIST_ELEMENT	4)
(define-public TCL_TRACE_READS		#x10)
(define-public TCL_TRACE_WRITES	#x20)
(define-public TCL_TRACE_UNSETS	#x40)
(define-public TCL_TRACE_DESTROYED	#x80)
(define-public TCL_INTERP_DESTROYED	#x100)
(define-public TCL_LEAVE_ERR_MSG	#x200)

;;; Flag values to pass to TCL_DoOneEvent to disable searches
;;; for some kinds of events:
;;;

(define-public TCL_DONT_WAIT		2)
(define-public TCL_X_EVENTS		4)
(define-public TCL_FILE_EVENTS		8)
(define-public TCL_TIMER_EVENTS		#x10)
(define-public TCL_IDLE_EVENTS		#x20)
(define-public TCL_ALL_EVENTS		-3)

;; A convenience function for combining flag bits.  Like logior, but
;; handles the cases of 0 and 1 arguments.
;;
(define (flags . args)
  (cond
   ((null? args) 0)
   ((null? (cdr args)) (car args))
   (else (apply logior args))))


;; MDJ 961023 <djurfeldt@nada.kth.se>
(define-public (tcl-eval . strings)
;  (pk 'cmd strings)
  (let* ((cmd (tcl-merge the-interpreter (map ->tcl-arg-string strings)))
	 (status (tcl-global-eval the-interpreter cmd)))
    (if (zero? (car status))
	(cdr status)
	(error (cdr status)))))

(define uniq-command
  (let ((cnt 0))
    (lambda (prefix)
      (set! cnt (+ cnt 1))
      (string-append prefix (number->string cnt)))))

(define (closure->tcl-name p)
  ;(let ((tcl-name (procedure-property p 'tcl-name)))
    ;(or #f tcl-name
	(let ((name (uniq-command ;(or (procedure-property p 'name)
		                       "*__guile#"))
	      ;(template (procedure-property p 'tcl-calling-convention)))
	      )
	  (tcl-create-command the-interpreter name p)
	  ;(set-procedure-property! p 'tcl-name name)
	  name))

(define (->tcl-arg-string v)
  (cond
   ((symbol? v) v)
   ((keyword? v) (keyword-dash-symbol v))
   ((string? v) v)
   ((number? v) (number->string v))
   ((eq? #f v) "0")
   ((eq? #t v) "1")
   ((closure? v)
    (let ((cc (procedure-property v 'tcl-calling-convention)))
      (if cc
	  (string-append (closure->tcl-name v) " " cc)
	  (closure->tcl-name v))))
   (else "")))

(define (tcl-args args)
  (cond ((null? args) '())
	((symbol? (car args))
	 (cons (car args) (tcl-args (cdr args))))
	((keyword? (car args))
	 (cons (keyword-dash-symbol (car args)) (tcl-args (cdr args))))
	((string? (car args))
	 (cons (car args) (tcl-args (cdr args))))
	((number? (car args))
	 (cons (number->string (car args)) (tcl-args (cdr args))))
	((eq? #f (car args))
	 (cons "0" (tcl-args (cdr args))))
	((eq? #t (car args))
	 (cons "1" (tcl-args (cdr args))))
	((closure? (car args))
	 (let ((cc (procedure-property (car args) 'tcl-calling-convention)))
	   (if cc
	       (cons (string-append (closure->tcl-name (car args)) " " cc)
		     (tcl-args (cdr args)))
	       (cons (closure->tcl-name (car args))
		     (tcl-args (cdr args))))))
	(else "")))

;; MDJ 961023 <djurfeldt@nada.kth.se>
(define-public (tcl-command interp name)
  (let ((proc
	 (lambda args
	   (let ((status (tcl-global-eval
			  the-interpreter
			  (tcl-merge interp (map ->tcl-arg-string
						 (cons name args))))))
	     (if (zero? (car status))
		 (cdr status)
		 (throw 'tcl-error (cdr status)))))))
    (set-procedure-property! proc 'name name)
    proc))

;; Reifying a Tcl command as a Scheme procedure.
;;
;(define-public (reify-tcl-command interp name)
;  (let ((command-object (tcl-command interp name)))
;    (and command-object
;	 (let ((reified
;		(lambda args
;		  (let ((answer
;			 (tcl-apply-command command-object
;					    (map (lambda (a)
;						   (or (procedure-property a 'tk-command)
;						       a))
;						 args))))
;		    (if (eq? 0 (car answer))
;			(tcl-string-> (cdr answer))
;			(throw 'tcl-error (cdr answer)))))))
;	   (set-procedure-property! reified 'tk-command name)
;	   reified))))
(define-public reify-tcl-command tcl-command)

(define-public (tcl-command? p)
  (and (procedure? p) (procedure-property 'tcl-command)))

;; Evaluate some code in the scope of a TCL-ERROR handler.
;; The handler returns a conventional Tcl error value (i.e. (cons 1 message))
;; Some type conversion is automaticly done on the return value to put it
;; in a form Tcl will like.
;;
(defmacro-public with-tcl-error-handling body
  `(catch 'tcl-error
	  (lambda () (->tcl-string (begin ,@body)))
	  (lambda (tag . message)
	    (cons 1 (apply errcat message)))))

(define (errcat . args)
  (apply string-append
	 (map (lambda (x)
		(call-with-output-string
		     (lambda (p)
		       ((if (string? x) display write) x p)
		       (display " " p))))
	      args)))

;; If this is defined to be an unary function, it gets to extend the 
;; default type conversion rules for arguments (it is passed otherwise
;; unhandled values).
;;
(define-public tcl-type-converter #f)

;; Default conversions from Scheme to Tcl strings.
;;
(define-public (->tcl-string val)
  (cond
   ((string? val) val)
   ((symbol? val) val)
   ((number? val) (number->string val))
   ((eq? #f val) "0")
   ((eq? #t val) "1")
   ((keyword? val) (keyword->symbol val))
   (#t "")))

(define (that x) x)

;; Default conversions from Tcl strings to Scheme.
;;
(define-public (tcl-string-> val)
   (cond
    ((string->number val) => that)
    ((equal? "" val) #f)
    (#t val)))


;;; {An Implicit Default Interpreter}
;;;
;;; For programs like "wish" in which there is one designated default
;;; interpreter.
;;;

(define-public the-interpreter #f)


;; Use defined-tcl-command to extend the global namespace
;; with commands from the default Tcl interpreter.
;;
;(define-public (use-default-tcl-commands)
;  (set! *top-level-lookup-thunk* defined-tcl-command))

;; If there is a defined variable called NAME, return it.
;; If not, but there is a Tcl command in the default interpreter
;; called NAME, create a variable an initialize it to point to the
;; reified Tcl command.
;;
;; Finally, always return a variable, perhaps undefined, if DEFINING?
;; is a true value.
;;
(define-public (make-tcl-binder interp)
  (lambda (m s define?)
    (if define?
	(let ((b (make-undefined-variable s)))
	  (module-obarray-set! (module-obarray m) s b)
	  (if (tcl-defined? the-interpreter s)
	      (variable-set! b (tcl-command interp s)))
	  b)
	(and (tcl-defined? interp s)
	     (let ((b (make-undefined-variable s)))
	       (module-obarray-set! (module-obarray m) s b)
	       (variable-set! b (tcl-command interp s))
	       b)))))

;; Used to define Scheme procedures which are also Tcl commands.
;; The declarations syntax is;
;; 
;; (proc name (?<calling-convention>? ?.? <formals>) <body>)
;;
;; which is expanded in terms of tcl-lambda.
;;
(defmacro-public proc (name . spec)
  `(begin
     (define ,name (tcl-lambda ,@ spec))
     (tcl-create-command the-interpreter ',name ,name)))


;; Used to define an anonymous Scheme procedure which is suitable
;; for use as a Tcl command.
;;
;; The declaration syntax is:
;;
;; (tcl-lambda (?<calling-convention>? ?.? <formals>) <body>)
;;
;; A <calling-convention> is a string that describes how the procedure
;; should be called when it is used as a Tcl command.
;; If the procedure hash the tcl-name PROC, and the calling convention
;; "%x %y", then the procedure will be called as:
;;
;;		PROC %x %y
;;
;; Such calling conventions are useful in cases such as binding a Scheme
;; procedure to Tk event.
;;
;; Formals specifications are as usual except that non-rest parameters
;; can have declarations.  Declarations are arbitrary expressions in which
;; the name of the formal is in the second position.  The expressions are
;; evaluated in the scope of the formals, and may modify the formals by
;; side effect.  Declarations should return a false value to cause an error
;; to be thrown, a true value otherwise.
;;
(defmacro-public tcl-lambda (formals . body)
  (let* ((calling-convention (if (and (pair? formals)
				      (string? (car formals)))
				 (let ((a (car formals)))
				   (set! formals (cdr formals))
				   a)
				 #f))
	 (args (tcl-formals-vars formals))
	 (full-body `(begin
		       ,@(tcl-type-checks formals 1)
		       ,@body)))
    `(let ((proc (lambda ,args
		   (with-tcl-error-handling ,full-body))))
       ,@(if calling-convention
	     `((set-procedure-property! proc
					'tcl-calling-convention
					,calling-convention)
	       proc)
	     `(proc)))))


;; From a list of formals, perhaps with declarations, return the
;; formals <<e.g.  (a (tcl->int b) . c) => (a b . c) >>
;;
(define-public (tcl-formals-vars formals)
  (if (not (pair? formals))
      formals
      (cons (if (pair? (car formals))
		(cadar formals)
		(car formals))
	    (tcl-formals-vars (cdr formals)))))

(define-public (tcl-error . args)
  (apply throw (cons 'tcl-error args)))


(define-public (tcl-type-checks formals pos)
  (cond
   ((not (pair? formals)) '())
   ((not (pair? (car formals)))
    (tcl-type-checks (cdr formals) (+ 1 pos)))
   (#t (cons (tcl-type-check (car formals))
	     (tcl-type-checks (cdr formals) (+ 1 pos))))))

(define-public tcl-type-converters
  `( (number 	. 	,(lambda (x) (tcl->number x))) ))		

(define (tcl-type-check x)
  (let ((a (assoc (car x) tcl-type-converters)))
    (if (not a)
	(error "Unsupported declaration" x)
	(list 'set! (cadr x) (cons (cdr a) (cdr x))))))

(define-public (tcl->number x)
  (cond ((string? x) (string->number x))
	((integer? x) x)
	(#t (tcl-error "Expected integer but got" x))))

;;; To support stack handling:

(define v (builtin-variable 'tk-stack-mark))

(variable-set! v ->tcl-string)
