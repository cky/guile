;;; installed-scm-file

;;;; 	Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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


;;; This file is the first thing loaded into Guile.  It adds many mundane
;;; definitions and a few that are interesting.
;;;
;;; The module system (hence the hierarchical namespace) are defined in this 
;;; file.
;;;



;; {Simple Debugging Tools}
;;


;; peek takes any number of arguments, writes them to the
;; current ouput port, and returns the last argument.
;; It is handy to wrap around an expression to look at
;; a value each time is evaluated, e.g.:
;;
;;	(+ 10 (troublesome-fn))
;;	=> (+ 10 (pk 'troublesome-fn-returned (troublesome-fn)))
;;

(define (peek . stuff)
  (newline)
  (display ";;; ")
  (write stuff)
  (newline)
  (car (last-pair stuff)))

(define pk peek)

(define (warn . stuff)
  (with-output-to-port (current-error-port)
    (lambda ()
      (newline)
      (display ";;; WARNING ")
      (print stuff)
      (newline)
      (car (last-pair stuff)))))


;;; {apply and call-with-current-continuation}
;;;
;;; These turn syntax, @apply and @call-with-current-continuation,
;;; into procedures.   
;;;

(set! apply (lambda (fun . args) (@apply fun (apply:nconc2last args))))
(define (call-with-current-continuation proc) (@call-with-current-continuation proc))



;;; {apply-to-args}
;;;
;;; apply-to-args is functionally redunant with apply and, worse,
;;; is less general than apply since it only takes two arguments.
;;;
;;; On the other hand, apply-to-args is a syntacticly convenient way to 
;;; perform binding in many circumstances when the "let" family of
;;; of forms don't cut it.  E.g.:
;;;
;;;	(apply-to-args (return-3d-mouse-coords)
;;;	  (lambda (x y z) 
;;;		...))
;;;

(define (apply-to-args args fn) (apply fn args))


;;; {Silly Naming Cleanups and Trivial Functions}
;;;

(define (id x) x)
(define < <?)
(define <= <=?)
(define = =?)
(define > >?)
(define >= >=?)
(define (1+ n) (+ n 1))
(define (-1+ n) (+ n -1))
(define 1- -1+)
(define return-it noop)
(define (and=> value thunk) (and value (thunk value)))
(define (make-hash-table k) (make-vector k '()))

;;; {Integer Math}
;;;

(define (integer? x) (and (number? x) (= x (inexact->exact x))))

(define (ipow-by-squaring x k acc proc)
  (cond ((zero? k) acc)
	((= 1 k) (proc acc x))
	(else (logical:ipow-by-squaring (proc x x)
					(quotient k 2)
					(if (even? k) acc (proc acc x))
					proc))))

(define string-character-length string-length)



;; A convenience function for combining flag bits.  Like logior, but
;; handles the cases of 0 and 1 arguments.
;;
(define (flags . args)
  (cond
   ((null? args) 0)
   ((null? (cdr args)) (car args))
   (else (apply logior args))))


;;; {Basic Port Code}
;;; 
;;; Specificly, the parts of the low-level port code that are written in 
;;; Scheme rather than C.
;;;
;;; WARNING: the parts of this interface that refer to file ports
;;; is going away.   It would be gone already except that it is used
;;; "internally" in a few places.
;;;


;; OPEN_READ, OPEN_WRITE, and OPEN_BOTH are used to request the proper
;; mode to open files in.  MSDOS does carraige return - newline
;; translation if not opened in `b' mode.
;;
(define OPEN_READ (case (software-type)
		    ((MS-DOS WINDOWS ATARIST) "rb")
		    (else "r")))
(define OPEN_WRITE (case (software-type)
		     ((MS-DOS WINDOWS ATARIST) "wb")
		     (else "w")))
(define OPEN_BOTH (case (software-type)
		    ((MS-DOS WINDOWS ATARIST) "r+b")
		    (else "r+")))

(define *null-device* "/dev/null")

(define (open-input-file str)
  (open-file str OPEN_READ))

(define (open-output-file str)
  (open-file str OPEN_WRITE))

(define (open-io-file str) (open-file str OPEN_BOTH))
(define close-input-port close-port)
(define close-output-port close-port)
(define close-io-port close-port)

(define (call-with-input-file str proc)
  (let* ((file (open-input-file str))
	 (ans (proc file)))
    (close-input-port file)
    ans))

(define (call-with-output-file str proc)
  (let* ((file (open-output-file str))
	 (ans (proc file)))
    (close-output-port file)
    ans))

(define (with-input-from-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-input-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-output-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-output-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-error-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-error-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-input-from-file file thunk)
  (let* ((nport (open-input-file file))
	 (ans (with-input-from-port nport thunk)))
    (close-port nport)
    ans))

(define (with-output-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-output-to-port nport thunk)))
    (close-port nport)
    ans))

(define (with-error-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-error-to-port nport thunk)))
    (close-port nport)
    ans))

(define (with-input-from-string string thunk)
  (call-with-input-string string
   (lambda (p) (with-input-from-port p thunk))))

(define (with-output-to-string thunk)
  (call-with-output-string
   (lambda (p) (with-output-to-port p thunk))))

(define (with-error-to-string thunk)
  (call-with-output-string
   (lambda (p) (with-error-to-port p thunk))))

(define the-eof-object (call-with-input-string "" (lambda (p) (read-char p))))



;;; {Symbol Properties}
;;;

(define (symbol-property sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (and pair (cdr pair))))

(define (set-symbol-property! sym prop val)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
	(set-cdr! pair val)
	(symbol-pset! sym (acons prop val (symbol-pref sym))))))

(define (symbol-property-remove! sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
	(symbol-pset! sym (delq! pair (symbol-pref sym))))))


;;; {Arrays}
;;;

(begin
  (define uniform-vector? array?)
  (define make-uniform-vector dimensions->uniform-array)
  ;      (define uniform-vector-ref array-ref)
  (define (uniform-vector-set! u i o)
    (uniform-vector-set1! u o i))
  (define uniform-vector-fill! array-fill!)
  (define uniform-vector-read! uniform-array-read!)
  (define uniform-vector-write uniform-array-write)

  (define (make-array fill . args)
    (dimensions->uniform-array args () fill))
  (define (make-uniform-array prot . args)
    (dimensions->uniform-array args prot))
  (define (list->array ndim lst)
    (list->uniform-array ndim '() lst))
  (define (list->uniform-vector prot lst)
    (list->uniform-array 1 prot lst))
  (define (array-shape a)
    (map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
	 (array-dimensions a))))


;;; {Keywords}
;;;

(define (symbol->keyword symbol)
  (make-keyword-from-dash-symbol (symbol-append '- symbol)))

(define (keyword->symbol kw)
  (let ((sym (keyword-dash-symbol kw)))
    (string->symbol (substring sym 1 (length sym)))))

(define (kw-arg-ref args kw)
  (let ((rem (member kw args)))
    (and rem (pair? (cdr rem)) (cadr rem))))



;;; {Print}
;;;

(define (print obj . args)
  (let ((default-args (list (current-output-port) 0 0 default-print-style #f)))
    (apply-to-args (append args (list-cdr-ref default-args (length args)))
      (lambda (port depth length style table)
	(cond
	 ((and table (print-table-ref table obj))		((print-style-tag-hook style 'eq-val)
								 obj port depth length style table))
	 (else
	  (and table (print-table-add! table obj))
	  (cond
	   ((print-style-max-depth? style depth)        	((print-style-excess-depth-hook style)))
	   ((print-style-max-length? style length)      	((print-style-excess-length-hook style)))
	   (else 					    	((print-style-hook style obj)
								 obj port depth length style table)))))))))

(define (make-print-style) (make-vector 59))

(define (extend-print-style! style utag printer) (hashq-set! style utag printer))

(define (print-style-hook style obj)
  (let ((type-tag (tag obj)))
    (or (hashq-ref style type-tag)
	(hashq-ref style (logand type-tag 255))
	print-obj)))

(define (print-style-tag-hook style type-tag)
  (or (hashq-ref style type-tag)
      print-obj))

(define (print-style-max-depth? style d) #f)
(define (print-style-max-length? style l) #f)
(define (print-style-excess-length-hook style) (hashq-ref style 'excess-length-hook))
(define (print-style-excess-depth-hook style) (hashq-ref style 'excess-depth-hook))

(define (make-print-table) (make-vector 59))
(define (print-table-ref table obj) (hashq-ref table obj))
(define (print-table-add! table obj) (hashq-set! table obj (gensym 'ref)))

(define (print-obj obj port depth length style table) (write obj port))

(define (print-pair pair port depth length style table)
  (if (= 0 length)
      (display #\( port))

  (print (car pair) port (+ 1 depth) 0 style table)

  (cond
   ((and (pair? (cdr pair))
	 (or (not table)
	     (not (print-table-ref table (cdr pair)))))

    (display #\space port)
    (print (cdr pair) port depth (+ 1 length) style table))

   ((null? (cdr pair))		(display #\) port))
   
   (else			(display " . " port)
				(print (cdr pair) port (+ 1 depth) 0 style table)
				(display #\) port))))

(define (print-vector obj port depth length style table)
  (if (= 0 length)
      (cond
       ((weak-key-hash-table? obj)	(display "#wh(" port))
       ((weak-value-hash-table? obj)	(display "#whv(" port))
       ((doubly-weak-hash-table? obj)	(display "#whd(" port))
       (else		  		(display "#(" port))))
	  
  (if (< length (vector-length obj))
      (print (vector-ref obj length) port (+ 1 depth) 0 style table))

  (cond
   ((>= (+ 1 length) (vector-length obj))	(display #\) port))
   (else					(display #\space port)
						(print obj port depth (+ 1 length) style table))))

(define default-print-style (make-print-style))

(extend-print-style! default-print-style utag_vector print-vector)
(extend-print-style! default-print-style utag_wvect print-vector)
(extend-print-style! default-print-style utag_pair print-pair)
(extend-print-style! default-print-style 'eq-val
		     (lambda (obj port depth length style table)
		       (if (symbol? obj)
			   (display obj)
			   (begin
			     (display "##" port)
			     (display (print-table-ref table obj))))))


;;; {Records}
;;;

(define record-type-vtable (make-vtable-vtable "prpr" 0))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

(define (make-record-type type-name fields . opt)
  (let ((printer-fn (and opt (car opt))))
    (let ((struct (make-struct record-type-vtable 0
			       (make-struct-layout (apply symbol-append (map (lambda (f) "pw") fields)))
			       type-name
			       (copy-tree fields))))
      ;; !!! leaks printer functions
      (if printer-fn
	  (extend-print-style! default-print-style
			       (logior utag_struct_base (ash (struct-vtable-tag struct) 8))
			       printer-fn))
      struct)))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj struct-vtable-offset)
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 struct-vtable-offset))
      (error 'not-a-record-type obj)))

(define (record-constructor rtd . opt)
  (let ((field-names (if opt (car opt) (record-type-fields rtd))))
    (eval `(lambda ,field-names
	     (make-struct ',rtd 0 ,@(map (lambda (f)
					  (if (memq f field-names)
					      f
					      #f))
					(record-type-fields rtd)))))))

(define (record-predicate rtd)
  (lambda (obj) (and (struct? obj) (eq? rtd (struct-vtable obj)))))

(define (record-accessor rtd field-name)
  (let* ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (eval `(lambda (obj)
	     (and (eq? ',rtd (record-type-descriptor obj))
		  (struct-ref obj ,pos))))))

(define (record-modifier rtd field-name)
  (let* ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (eval `(lambda (obj val)
	     (and (eq? ',rtd (record-type-descriptor obj))
		  (struct-set! obj ,pos val))))))


(define (record? obj)
  (and (struct? obj) (record-type? (struct-vtable obj))))

(define (record-type-descriptor obj)
  (if (struct? obj)
      (struct-vtable obj)
      (error 'not-a-record obj)))


;;; {Booleans}
;;;

(define (->bool x) (not (not x)))


;;; {Symbols}
;;;

(define (symbol-append . args)
  (string->symbol (apply string-append args)))

(define (list->symbol . args)
  (string->symbol (apply list->string args)))

(define (symbol . args)
  (string->symbol (apply string args)))

(define (obarray-symbol-append ob . args)
  (string->obarray-symbol (apply string-append ob args)))

(define obarray-gensym
  (let ((n -1))
    (lambda (obarray . opt)
      (if (null? opt)
	  (set! opt '(%%gensym)))
      (let loop ((proposed-name (apply string-append opt)))
	(if (string->obarray-symbol obarray proposed-name #t)
	    (loop (apply string-append (append opt (begin (set! n (1+ n)) (list (number->string n))))))
	    (string->obarray-symbol obarray proposed-name))))))

(define (gensym . args) (apply obarray-gensym #f args))


;;; {Lists}
;;;

(define (list-index l k)
  (let loop ((n 0)
	     (l l))
    (and (not (null? l))
	 (if (eq? (car l) k)
	     n
	     (loop (+ n 1) (cdr l))))))

(define (make-list n init)
  (let loop ((answer '())
	     (n n))
    (if (<= n 0)
	answer
	(loop (cons init answer) (- n 1)))))



;;; {and-map, or-map, and map-in-order}
;;;
;;; (and-map fn lst) is like (and (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;; (or-map fn lst) is like (or (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;; (map-in-order fn lst) is like (map fn lst) but definately in order of lst.
;;;

;; and-map f l
;;
;; Apply f to successive elements of l until exhaustion or f returns #f.
;; If returning early, return #f.  Otherwise, return the last value returned
;; by f.  If f has never been called because l is empty, return #t.
;; 
(define (and-map f lst)
  (let loop ((result #t)
	     (l lst))
    (and result
	 (or (and (null? l)
		  result)
	     (loop (f (car l)) (cdr l))))))

;; or-map f l
;;
;; Apply f to successive elements of l until exhaustion or while f returns #f.
;; If returning early, return the return value of f.
;;
(define (or-map f lst)
  (let loop ((result #f)
	     (l lst))
    (or result
	(and (not (null? l))
	     (loop (f (car l)) (cdr l))))))

;; map-in-order
;;
;; Like map, but guaranteed to process the list in order.
;;
(define (map-in-order fn l)
  (if (null? l)
      '()
      (cons (fn (car l))
	    (map-in-order fn (cdr l)))))


;;; {Files}
;;; !!!! these should be implemented using Tcl commands, not fports.
;;;

(define (file-exists? str)
  ;; we don't have false-if-exception (or defmacro) yet.
  (let ((port (catch #t (lambda () (open-file str OPEN_READ))
		     (lambda args #f))))
    (if port (begin (close-port port) #t)
	#f)))

(define (file-is-directory? str)
  (let ((port (catch #t (lambda () (open-file (string-append str "/.")
					      OPEN_READ))
		     (lambda args #f))))
    (if port (begin (close-port port) #t)
	#f)))

(define (has-suffix? str suffix)
  (let ((sufl (string-length suffix))
	(sl (string-length str)))
    (and (> sl sufl)
	 (string=? (substring str (- sl sufl) sl) suffix))))




;;; {Error Handling}
;;;


;; (error . args) is short for (throw (quote error) . args)
;;
(define (error . args)
  (apply throw 'error args))




;; Error handling a la SCM.
;;
(define (%%default-error-handler tag . args)
  (define cep (current-error-port))
  (perror "ERROR")
  (errno 0)
  (display "ERROR: " cep)
  (if (not (null? args))
      (begin (display (car args) cep)
	     (for-each (lambda (x) (display #\  cep) (write x cep))
		       (cdr args))))
  (newline cep)
  (force-output cep)
  (apply throw 'abort tag args))



;; Install SCM error handling as the default.
;;
(set-symbol-property! 'error
		      'throw-handler-default
		      %%default-error-handler)





;; %%bad-throw is the hook that is called upon a throw to a an unhandled
;; key.  If the key has a default handler (a throw-handler-default property),
;; it is applied to the throw.
;;
(define (%%bad-throw key . args)
  (let ((default (symbol-property key 'throw-handler-default)))
    (or (and default (apply default key args))
	(throw 'error 'unhandled-exception key args))))





;; A number of internally defined error types are represented
;; as integers.  Here is the mapping to symbolic names
;; and error messages.
;;
(define %%system-errors
  '((-1 UNKNOWN "Unknown error")
    (0 ARGn  "Wrong type argument to ")
    (1 ARG1  "Wrong type argument in position 1 to ")
    (2 ARG2  "Wrong type argument in position 2 to ")
    (3 ARG3  "Wrong type argument in position 3 to ")
    (4 ARG4  "Wrong type argument in position 4 to ")
    (5 ARG5  "Wrong type argument in position 5 to ")
    (6 ARG5  "Wrong type argument in position 5 to ")
    (7 ARG5  "Wrong type argument in position 5 to ")
    (8 WNA "Wrong number of arguments to ")
    (9 OVFLOW "Numerical overflow to ")
    (10 OUTOFRANGE "Argument out of range to ")
    (11 NALLOC "Could not allocate to ")
    (12 STACK_OVFLOW "Stack overflow")
    (13 EXIT "Exit (internal error?).")
    (14 HUP_SIGNAL "hang-up")
    (15 INT_SIGNAL "user interrupt")
    (16 FPE_SIGNAL "arithmetic error")
    (17 BUS_SIGNAL "bus error")
    (18 SEGV_SIGNAL "segmentation violation")
    (19 ALRM_SIGNAL "alarm")
    (20 GC_SIGNAL "gc")
    (21 TICK_SIGNAL "tick")))


(define (timer-thunk) #t)
(define (gc-thunk) #t)
(define (alarm-thunk) #t)

(define (signal-handler n)
  (cond
   ((= n 21)	(unmask-signals) (timer-thunk))
   ((= n 20)	(unmask-signals) (gc-thunk))
   ((= n 19)	(unmask-signals) (alarm-thunk))
   (else	(unmask-signals) (throw '%%system-error n #f))))


;; The default handler for built-in error types when
;; thrown by their symbolic name.
(define (%%handle-system-error key . arg-list)
  (cond ((= (length arg-list) 4)
	 (letrec ((subr (car arg-list))
		  (message (cadr arg-list))
		  (args (caddr arg-list))
		  (rest (cadddr arg-list))
		  (cep (current-error-port))
		  (fill-message
		   (lambda (message args)
		     (if (null? args)
			 (display message cep)
			 (let ((len (string-length message)))
			   (cond ((< len 2)
				  (display message cep))
				 ((string=? (substring message 0 2)
					    "%S")
				  (display (car args) cep)
				  (fill-message
				   (substring message 2 len)
				   (cdr args)))
				 (else
				  (display (substring message 0 1)
					   cep)
				  (fill-message
				   (substring message 1 len)
				   args))))))))
	   (display "ERROR: " cep)
	   (display subr cep)
	   (display ": " cep)
	   (cond ((list? args)
		  (fill-message message args))
		 (else
		  (display message cep)
		  (display " (bad message args)" cep)))
	   (newline cep)
	   (force-output cep)
	   (apply throw 'abort key arg-list)))
	(else
	 ;; old style errors.
	 (let* ((desc (car arg-list))
		(proc (cadr arg-list))
		(args (cddr arg-list))
		(b (assoc desc %%system-errors))
		(msghead (cond
			  (b (caddr b))
			  ((or (symbol? desc) (string? desc))
			   (string-append desc " "))
			  (#t "Unknown error")))
		(msg (if (symbol? proc)
			 (string-append msghead proc ":")
			 msghead))
		(rest (if (and proc (not (symbol? proc)))
			  (cons proc args)
			  args))
		(fixed-args (cons msg rest)))
	   (apply error fixed-args)))))


(set-symbol-property! '%%system-error
		      'throw-handler-default
		      %%handle-system-error)


;; Install default handlers for built-in errors.
;;
(map (lambda (err)
       (set-symbol-property! (cadr err)
			     'throw-handler-default
			     %%handle-system-error))
     (cdr %%system-errors))



(begin
  (define (syserror key fn err . args)
    (errno err)
    (apply error (cons fn args)))
  (set-symbol-property! 'syserror 'throw-handler-default syserror))


(define (getgrnam name) (getgr name))
(define (getgrgid id) (getgr id))
(define (gethostbyaddr addr) (gethost addr))
(define (gethostbyname name) (gethost name))
(define (getnetbyaddr addr) (getnet addr))
(define (getnetbyname name) (getnet name))
(define (getprotobyname name) (getproto name))
(define (getprotobynumber addr) (getproto addr))
(define (getpwnam name) (getpw name))
(define (getpwuid uid) (getpw uid))
(define (getservbyname name proto) (%getserv name proto))
(define (getservbyport port proto) (%getserv port proto))
(define (endgrent) (setgr))
(define (endhostent) (sethost))
(define (endnetent) (setnet))
(define (endprotoent) (setproto))
(define (endpwent) (setpw))
(define (endservent) (setserv))
(define (file-position . args) (apply ftell args))
(define (file-set-position . args) (apply fseek args))
(define (getgrent) (getgr))
(define (gethostent) (gethost))
(define (getnetent) (getnet))
(define (getprotoent) (getproto))
(define (getpwent) (getpw))
(define (getservent) (getserv))
(define (reopen-file . args) (apply freopen args))
(define (setgrent arg) (setgr arg))
(define (sethostent arg) (sethost arg))
(define (setnetent arg) (setnet arg))
(define (setprotoent arg) (setproto arg))
(define (setpwent arg) (setpw arg))
(define (setservent arg) (setserv arg))

(define (move->fdes port fd)
  (primitive-move->fdes port fd)
  (set-port-revealed! port 1)
  port)

(define (release-port-handle port)
  (let ((revealed (port-revealed port)))
    (if (> revealed 0)
	(set-port-revealed! port (- revealed 1)))))


;;; {Load Paths}
;;;

;;; Here for backward compatability
;;
(define scheme-file-suffix (lambda () ".scm"))

(define (in-vicinity vicinity file)
  (let ((tail (let ((len (string-length vicinity)))
		(if (zero? len) #f
		    (string-ref vicinity (- len 1))))))
    (string-append vicinity
		   (if (eq? tail #\/) "" "/")
		   file)))


;;; {try-load}
;;;

(define (try-load-with-path file-name path)
  (or-map (lambda (d)
	    (let ((f (in-vicinity d file-name)))
	      (and (not (file-is-directory? f))
		   (%try-load f #t read-sharp))))
	  path))

(define (try-load name)
  (if (eval '(defined? %load-path))
      (try-load-with-path name (eval '%load-path))
      (%try-load name #t read-sharp)))

;;; {Load}
;;;

(define %load-verbosely #t)
(define (assert-load-verbosity v) (set! %load-verbosely v))
(define %load-indent -2)

(define (%load f)
  (current-module)
  (or (and (not (file-is-directory? f))
	   (%try-load f #t read-sharp))
      (and (not (has-suffix? f (scheme-file-suffix)))
	   (%try-load (string-append f (scheme-file-suffix)) #t read-sharp))))

(define (%load-announce file)
  (if %load-verbosely
      (with-output-to-port (current-error-port)
	(lambda ()
	  (display ";;; ")
	  (display (make-string %load-indent #\ ))
	  (display "loading ")
	  (display file)
	  (display "...")
	  (newline)
	  (force-output)))))

(define (%load-announce-win file)
  (if %load-verbosely
      (with-output-to-port (current-error-port)
	(lambda () 
	  (display ";;; ")
	  (display (make-string %load-indent #\ ))
	  (display "...loaded ")
	  (display file)
	  (display ".")
	  (newline)
	  (force-output)))))

(define (%load-announce-lossage file path)
  (if %load-verbosely
      (with-output-to-port (current-error-port)
	(lambda ()
	  (display ";;; ")
	  (display (make-string %load-indent #\ ))
	  (display "...COULD NOT LOAD ")
	  (display file)
	  (display " from ")
	  (write path)
	  (newline)
	  (force-output))))
  (throw 'could-not-load file path))


(define (load-with-path name path)
  (define (do-load)
    (%load-announce name)
    (if (not (or-map (lambda (d)
		       (if (%load (in-vicinity d name))
			   (begin
			     (%load-announce-win (in-vicinity d name))
			     #t)
			   #f))
		     path))
	(%load-announce-lossage name path)))

  (let ((indent %load-indent))
    (dynamic-wind
     (lambda () (set! %load-indent (modulo (+ indent 2) 16)))
     do-load
     (lambda () (set! %load-indent indent))))
  #t)


(define (load name)
  (if (eval '(defined? %load-path))
      (load-with-path name (eval '%load-path))
      (load-with-path name '())))



;;; {Transcendental Functions}
;;;
;;; Derived from "Transcen.scm", Complex trancendental functions for SCM.
;;; Copyright (C) 1992, 1993 Jerry D. Hedden.
;;; See the file `COPYING' for terms applying to this program.
;;;

(define (exp z)
  (if (real? z) ($exp z)
      (make-polar ($exp (real-part z)) (imag-part z))))

(define (log z)
  (if (and (real? z) (>= z 0))
      ($log z)
      (make-rectangular ($log (magnitude z)) (angle z))))

(define (sqrt z)
  (if (real? z)
      (if (negative? z) (make-rectangular 0 ($sqrt (- z)))
	  ($sqrt z))
      (make-polar ($sqrt (magnitude z)) (/ (angle z) 2))))

(define expt
  (let ((integer-expt integer-expt))
    (lambda (z1 z2)
      (cond ((exact? z2)
	     (integer-expt z1 z2))
	    ((and (real? z2) (real? z1) (>= z1 0))
	     ($expt z1 z2))
	    (else
	     (exp (* z2 (log z1))))))))

(define (sinh z)
  (if (real? z) ($sinh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sinh x) ($cos y))
			  (* ($cosh x) ($sin y))))))
(define (cosh z)
  (if (real? z) ($cosh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cosh x) ($cos y))
			  (* ($sinh x) ($sin y))))))
(define (tanh z)
  (if (real? z) ($tanh z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cosh x) ($cos y))))
	(make-rectangular (/ ($sinh x) w) (/ ($sin y) w)))))

(define (asinh z)
  (if (real? z) ($asinh z)
      (log (+ z (sqrt (+ (* z z) 1))))))

(define (acosh z)
  (if (and (real? z) (>= z 1))
      ($acosh z)
      (log (+ z (sqrt (- (* z z) 1))))))

(define (atanh z)
  (if (and (real? z) (> z -1) (< z 1))
      ($atanh z)
      (/ (log (/ (+ 1 z) (- 1 z))) 2)))

(define (sin z)
  (if (real? z) ($sin z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sin x) ($cosh y))
			  (* ($cos x) ($sinh y))))))
(define (cos z)
  (if (real? z) ($cos z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cos x) ($cosh y))
			  (- (* ($sin x) ($sinh y)))))))
(define (tan z)
  (if (real? z) ($tan z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cos x) ($cosh y))))
	(make-rectangular (/ ($sin x) w) (/ ($sinh y) w)))))

(define (asin z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($asin z)
      (* -i (asinh (* +i z)))))

(define (acos z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($acos z)
      (+ (/ (angle -1) 2) (* +i (asinh (* +i z))))))

(define (atan z . y)
  (if (null? y)
      (if (real? z) ($atan z)
	  (/ (log (/ (- +i z) (+ +i z))) +2i))
      ($atan2 z (car y))))

(set! abs magnitude)


;;; {User Settable Hooks}
;;;
;;; Parts of the C code check the bindings of these variables.
;;; 

(define ticks-interrupt #f)
(define user-interrupt #f)
(define alarm-interrupt #f)
(define out-of-storage #f)
(define could-not-open #f)
(define end-of-program #f)
(define hang-up #f)
(define arithmetic-error #f)
(define read-sharp #f)



;;; {Reader Extensions}
;;;

;;; Reader code for various "#c" forms.
;;;

(define (parse-path-symbol s)
  (define (seperate-fields-discarding-char ch str ret)
    (let loop ((fields '())
	       (str str))
      (cond
       ((string-rindex str ch)
	=> (lambda (pos) (loop (cons (make-shared-substring str (+ 1 pos)) fields)
			       (make-shared-substring str 0 pos))))
       (else (ret (cons str fields))))))
  (seperate-fields-discarding-char #\/
				   s
				   (lambda (fields)
				     (map string->symbol fields))))
	 

(define (%read-sharp c port)
  (define (barf)
    (error "unknown # object" c))

  (case c
    ((#\/) (let ((look (peek-char port)))
	     (if (or (eof-object? look)
		     (and (char? look)
			  (or (char-whitespace? look)
			      (string-index ")" look))))
		 '()
		 (parse-path-symbol (read port #t read-sharp)))))
    ((#\') (read port #t read-sharp))
    ((#\.) (eval (read port #t read-sharp)))
    ((#\b) (read:uniform-vector #t port))
    ((#\a) (read:uniform-vector #\a port))
    ((#\u) (read:uniform-vector 1 port))
    ((#\e) (read:uniform-vector -1 port))
    ((#\s) (read:uniform-vector 1.0 port))
    ((#\i) (read:uniform-vector 1/3 port))
    ((#\c) (read:uniform-vector 0+i port))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (read:array c port))
    ((#\!) (if (= 1 (line-number))
	       (let skip () (if (eq? #\newline (peek-char port))
				(read port #t read-sharp)
				(begin (read-char port) (skip))))
	       (barf)))
    (else (barf))))

(define (read:array digit port)
  (define chr0 (char->integer #\0))
  (let ((rank (let readnum ((val (- (char->integer digit) chr0)))
		(if (char-numeric? (peek-char port))
		    (readnum (+ (* 10 val)
				(- (char->integer (read-char port)) chr0)))
		    val)))
	(prot (if (eq? #\( (peek-char port))
		  '()
		  (let ((c (read-char port)))
		    (case c ((#\b) #t)
			  ((#\a) #\a)
			  ((#\u) 1)
			  ((#\e) -1)
			  ((#\s) 1.0)
			  ((#\i) 1/3)
			  ((#\c) 0+i)
			  (else (error "read:array unknown option " c)))))))
    (if (eq? (peek-char port) #\()
	(list->uniform-array rank prot (read port #t read-sharp))
	(error "read:array list not found"))))

(define (read:uniform-vector proto port)
  (if (eq? #\( (peek-char port))
      (list->uniform-array 1 proto (read port #t read-sharp))
      (error "read:uniform-vector list not found")))


(define read-sharp (lambda a (apply %read-sharp a)))



;;; {Dynamic Roots}
;;;

; mystery integers passed dynamic root error handlers
(define repl-quit -1)
(define repl-abort -2)



;;; {Command Line Options}
;;;

(define (get-option argv kw-opts kw-args return)
  (cond
   ((null? argv)
    (return #f #f argv))

   ((or (not (eq? #\- (string-ref (car argv) 0)))
	(eq? (string-length (car argv)) 1))
    (return 'normal-arg (car argv) (cdr argv)))

   ((eq? #\- (string-ref (car argv) 1))
    (let* ((kw-arg-pos (or (string-index (car argv) #\=)
			   (string-length (car argv))))
	   (kw (symbol->keyword (substring (car argv) 2 kw-arg-pos)))
	   (kw-opt? (member kw kw-opts))
	   (kw-arg? (member kw kw-args))
	   (arg (or (and (not (eq? kw-arg-pos (string-length (car argv))))
			 (substring (car argv)
				    (+ kw-arg-pos 1)
				    (string-length (car argv))))
		    (and kw-arg?
			 (begin (set! argv (cdr argv)) (car argv))))))
      (if (or kw-opt? kw-arg?)
	  (return kw arg (cdr argv))
	  (return 'usage-error kw (cdr argv)))))

   (else
    (let* ((char (substring (car argv) 1 2))
	   (kw (symbol->keyword char)))
      (cond

       ((member kw kw-opts)
	(let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
	       (new-argv (if (= 0 (string-length rest-car))
			     (cdr argv)
			     (cons (string-append "-" rest-car) (cdr argv)))))
	  (return kw #f new-argv)))

       ((member kw kw-args)
	(let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
	       (arg (if (= 0 (string-length rest-car))
			(cadr argv)
			rest-car))
	       (new-argv (if (= 0 (string-length rest-car))
			     (cddr argv)
			     (cdr argv))))
	  (return kw arg new-argv)))

       (else (return 'usage-error kw argv)))))))

(define (for-next-option proc argv kw-opts kw-args)
  (let loop ((argv argv))
    (get-option argv kw-opts kw-args
		(lambda (opt opt-arg argv)
		  (and opt (proc opt opt-arg argv loop))))))

(define (display-usage-report kw-desc)
  (for-each
   (lambda (kw)
     (or (eq? (car kw) #t)
	 (eq? (car kw) 'else)
	 (let* ((opt-desc kw)
		(help (cadr opt-desc))
		(opts (car opt-desc))
		(opts-proper (if (string? (car opts)) (cdr opts) opts))
		(arg-name (if (string? (car opts))
			      (string-append "<" (car opts) ">")
			      ""))
		(left-part (string-append
			    (with-output-to-string
			      (lambda ()
				(map (lambda (x) (display (keyword-symbol x)) (display " "))
				     opts-proper)))
			    arg-name))
		(middle-part (if (and (< (length left-part) 30)
				      (< (length help) 40))
				 (make-string (- 30 (length left-part)) #\ )
				 "\n\t")))
	   (display left-part)
	   (display middle-part)
	   (display help)
	   (newline))))
   kw-desc))
		  

	   
(define (delq-all! obj l)
  (let ((answer (cons '() l)))
    (let loop ((pos answer))
      (cond
       ((null? (cdr pos)) 	(cdr answer))
       ((eq? (cadr pos) obj)	(set-cdr! pos (cddr pos))
				(loop pos))
       (else			(loop (cdr pos)))))))

(define (transform-usage-lambda cases)
  (let* ((raw-usage (delq! 'else (map car cases)))
	 (usage-sans-specials (map (lambda (x)
				    (or (and (not (list? x)) x)
					(and (symbol? (car x)) #t)
					(and (boolean? (car x)) #t)
					x))
				  raw-usage))
	 (usage-desc (delq-all! #t usage-sans-specials))
	 (kw-desc (map car usage-desc))
	 (kw-opts (apply append (map (lambda (x) (and (not (string? (car x))) x)) kw-desc)))
	 (kw-args (apply append (map (lambda (x) (and (string? (car x)) (cdr x))) kw-desc)))
	 (transmogrified-cases (map (lambda (case)
				      (cons (let ((opts (car case)))
					      (if (or (boolean? opts) (eq? 'else opts))
						  opts
						  (cond
						   ((symbol? (car opts))  opts)
						   ((boolean? (car opts)) opts)
						   ((string? (caar opts)) (cdar opts))
						   (else (car opts)))))
					    (cdr case)))
				    cases)))
    `(let ((%display-usage (lambda () (display-usage-report ',usage-desc))))
       (lambda (%argv)
	 (let %next-arg ((%argv %argv))
	   (get-option %argv
		       ',kw-opts
		       ',kw-args
		       (lambda (%opt %arg %new-argv)
			 (case %opt
			   ,@ transmogrified-cases))))))))




;;; {Low Level Modules}
;;;
;;; These are the low level data structures for modules.
;;;
;;; !!! warning: The interface to lazy binder procedures is going
;;; to be changed in an incompatible way to permit all the basic
;;; module ops to be virtualized.
;;;
;;; (make-module size use-list lazy-binding-proc) => module
;;; module-{obarray,uses,binder}[|-set!]
;;; (module? obj) => [#t|#f]
;;; (module-locally-bound? module symbol) => [#t|#f]
;;; (module-bound? module symbol) => [#t|#f]
;;; (module-symbol-locally-interned? module symbol) => [#t|#f]
;;; (module-symbol-interned? module symbol) => [#t|#f]
;;; (module-local-variable module symbol) => [#<variable ...> | #f]
;;; (module-variable module symbol) => [#<variable ...> | #f]
;;; (module-symbol-binding module symbol opt-value)
;;;		=> [ <obj> | opt-value | an error occurs ]
;;; (module-make-local-var! module symbol) => #<variable...>
;;; (module-add! module symbol var) => unspecified
;;; (module-remove! module symbol) =>  unspecified
;;; (module-for-each proc module) => unspecified
;;; (make-scm-module) => module ; a lazy copy of the symhash module
;;; (set-current-module module) => unspecified
;;; (current-module) => #<module...>
;;;
;;;


;; This is how modules are printed.
;; You can re-define it.
;;
(define (%print-module mod port depth length style table)
  (display "#<" port)
  (display (or (module-kind mod) "module") port)
  (let ((name (module-name mod)))
    (if name
	(begin
	  (display " " port)
	  (display name port))))
  (display " " port)
  (display (number->string (object-address mod) 16) port)
  (display ">" port))

;; module-type
;;
;; A module is characterized by an obarray in which local symbols
;; are interned, a list of modules, "uses", from which non-local
;; bindings can be inherited, and an optional lazy-binder which
;; is a (THUNK module symbol) which, as a last resort, can provide
;; bindings that would otherwise not be found locally in the module.
;;
(define module-type
  (make-record-type 'module '(obarray uses binder eval-thunk name kind) %print-module))

;; make-module &opt size uses
;;
;; Create a new module, perhaps with a particular size of obarray
;; or initial uses list.
;;
(define module-constructor (record-constructor module-type))

(define make-module
    (lambda args
      (let* ((size 1021)
	     (uses '())
	     (binder #f)
	     (answer #f)
	     (eval-thunk
	      (lambda (symbol define?)
		(if define?
		    (module-make-local-var! answer symbol)
		    (module-variable answer symbol)))))

	(if (> (length args) 0)
	    (begin
	      (set! size (or (car args) size))
	      (set! args (cdr args))))

	(if (> (length args) 0)
	    (begin
	      (set! uses (or (car args) uses))
	      (set! args (cdr args))))

	(if (> (length args) 0)
	    (begin
	      (set! binder (or (car args) binder))
	      (set! args (cdr args))))

	(if (not (null? args))
	    (error "Too many args to make-module." args))

	(if (not (integer? size))
	    (error "Illegal size to make-module." size))

	(and (list? uses)
	     (or (and-map module? uses)
		 (error "Incorrect use list." uses)))

	(if (and binder (not (procedure? binder)))
	    (error
	     "Lazy-binder expected to be a procedure or #f." binder))

	(set! answer
	      (module-constructor (make-vector size '())
				  uses
				  binder
				  eval-thunk
				  #f
				  #f))
	answer)))

(define module-obarray  (record-accessor module-type 'obarray))
(define set-module-obarray! (record-modifier module-type 'obarray))
(define module-uses  (record-accessor module-type 'uses))
(define set-module-uses! (record-modifier module-type 'uses))
(define module-binder (record-accessor module-type 'binder))
(define set-module-binder! (record-modifier module-type 'binder))
(define module-eval-thunk (record-accessor module-type 'eval-thunk))
(define set-module-eval-thunk! (record-modifier module-type 'eval-thunk))
(define module-name (record-accessor module-type 'name))
(define set-module-name! (record-modifier module-type 'name))
(define module-kind (record-accessor module-type 'kind))
(define set-module-kind! (record-modifier module-type 'kind))
(define module? (record-predicate module-type))

(define (eval-in-module exp module)
  (eval2 exp (module-eval-thunk module)))


;;; {Module Searching in General}
;;;
;;; We sometimes want to look for properties of a symbol
;;; just within the obarray of one module.  If the property
;;; holds, then it is said to hold ``locally'' as in, ``The symbol
;;; DISPLAY is locally rebound in the module `safe-guile'.''
;;;
;;;
;;; Other times, we want to test for a symbol property in the obarray
;;; of M and, if it is not found there, try each of the modules in the
;;; uses list of M.  This is the normal way of testing for some
;;; property, so we state these properties without qualification as
;;; in: ``The symbol 'fnord is interned in module M because it is
;;; interned locally in module M2 which is a member of the uses list
;;; of M.''
;;;

;; module-search fn m
;; 
;; return the first non-#f result of FN applied to M and then to
;; the modules in the uses of m, and so on recursively.  If all applications
;; return #f, then so does this function.
;;
(define (module-search fn m v)
  (define (loop pos)
    (and (pair? pos)
	 (or (module-search fn (car pos) v)
	     (loop (cdr pos)))))
  (or (fn m v)
      (loop (module-uses m))))


;;; {Is a symbol bound in a module?}
;;;
;;; Symbol S in Module M is bound if S is interned in M and if the binding
;;; of S in M has been set to some well-defined value.
;;;

;; module-locally-bound? module symbol
;;
;; Is a symbol bound (interned and defined) locally in a given module?
;;
(define (module-locally-bound? m v)
  (let ((var (module-local-variable m v)))
    (and var
	 (variable-bound? var))))

;; module-bound? module symbol
;;
;; Is a symbol bound (interned and defined) anywhere in a given module
;; or its uses?
;;
(define (module-bound? m v)
  (module-search module-locally-bound? m v))

;;; {Is a symbol interned in a module?}
;;;
;;; Symbol S in Module M is interned if S occurs in 
;;; of S in M has been set to some well-defined value.
;;;
;;; It is possible to intern a symbol in a module without providing
;;; an initial binding for the corresponding variable.  This is done
;;; with:
;;;       (module-add! module symbol (make-undefined-variable))
;;;
;;; In that case, the symbol is interned in the module, but not
;;; bound there.  The unbound symbol shadows any binding for that
;;; symbol that might otherwise be inherited from a member of the uses list.
;;;

(define (module-obarray-get-handle ob key)
  ((if (symbol? key) hashq-get-handle hash-get-handle) ob key))

(define (module-obarray-ref ob key)
  ((if (symbol? key) hashq-ref hash-ref) ob key))

(define (module-obarray-set! ob key val)
  ((if (symbol? key) hashq-set! hash-set!) ob key val))

(define (module-obarray-remove! ob key)
  ((if (symbol? key) hashq-remove! hash-remove!) ob key))

;; module-symbol-locally-interned? module symbol
;; 
;; is a symbol interned (not neccessarily defined) locally in a given module
;; or its uses?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-locally-interned? m v)
  (not (not (module-obarray-get-handle (module-obarray m) v))))

;; module-symbol-interned? module symbol
;; 
;; is a symbol interned (not neccessarily defined) anywhere in a given module
;; or its uses?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-interned? m v)
  (module-search module-symbol-locally-interned? m v))


;;; {Mapping modules x symbols --> variables}
;;;

;; module-local-variable module symbol
;; return the local variable associated with a MODULE and SYMBOL.
;;
;;; This function is very important. It is the only function that can
;;; return a variable from a module other than the mutators that store
;;; new variables in modules.  Therefore, this function is the location
;;; of the "lazy binder" hack.
;;;
;;; If symbol is defined in MODULE, and if the definition binds symbol
;;; to a variable, return that variable object.
;;;
;;; If the symbols is not found at first, but the module has a lazy binder,
;;; then try the binder.
;;;
;;; If the symbol is not found at all, return #f.
;;;
(define (module-local-variable m v)
  (caddr
   (list m v
	 (let ((b (module-obarray-ref (module-obarray m) v)))
	   (or (and (variable? b) b)
	       (and (module-binder m)
		    ((module-binder m) m v #f)))))))

;; module-variable module symbol
;; 
;; like module-local-variable, except search the uses in the 
;; case V is not found in M.
;;
(define (module-variable m v)
  (module-search module-local-variable m v))


;;; {Mapping modules x symbols --> bindings}
;;;
;;; These are similar to the mapping to variables, except that the
;;; variable is dereferenced.
;;;

;; module-symbol-binding module symbol opt-value
;; 
;; return the binding of a variable specified by name within
;; a given module, signalling an error if the variable is unbound.
;; If the OPT-VALUE is passed, then instead of signalling an error,
;; return OPT-VALUE.
;;
(define (module-symbol-local-binding m v . opt-val)
  (let ((var (module-local-variable m v)))
    (if var
	(variable-ref var)
	(if (not (null? opt-val))
	    (car opt-val)
	    (error "Locally unbound variable." v)))))

;; module-symbol-binding module symbol opt-value
;; 
;; return the binding of a variable specified by name within
;; a given module, signalling an error if the variable is unbound.
;; If the OPT-VALUE is passed, then instead of signalling an error,
;; return OPT-VALUE.
;;
(define (module-symbol-binding m v . opt-val)
  (let ((var (module-variable m v)))
    (if var
	(variable-ref var)
	(if (not (null? opt-val))
	    (car opt-val)
	    (error "Unbound variable." v)))))



;;; {Adding Variables to Modules}
;;;
;;;


;; module-make-local-var! module symbol
;; 
;; ensure a variable for V in the local namespace of M.
;; If no variable was already there, then create a new and uninitialzied
;; variable.
;;
(define (module-make-local-var! m v)
  (or (let ((b (module-obarray-ref (module-obarray m) v)))
	(and (variable? b) b))
      (and (module-binder m)
	   ((module-binder m) m v #t))
      (begin
	(let ((answer (make-undefined-variable v)))
	  (module-obarray-set! (module-obarray m) v answer)
	  answer))))

;; module-add! module symbol var
;; 
;; ensure a particular variable for V in the local namespace of M.
;;
(define (module-add! m v var)
  (if (not (variable? var))
      (error "Bad variable to module-add!" var))
  (module-obarray-set! (module-obarray m) v var))

;; module-remove! 
;; 
;; make sure that a symbol is undefined in the local namespace of M.
;;
(define (module-remove! m v)
  (module-obarray-remove!  (module-obarray m) v))

(define (module-clear! m)
  (vector-fill! (module-obarray m) '()))

;; MODULE-FOR-EACH -- exported
;; 
;; Call PROC on each symbol in MODULE, with arguments of (SYMBOL VARIABLE).
;;
(define (module-for-each proc module)
  (let ((obarray (module-obarray module)))
    (do ((index 0 (+ index 1))
	 (end (vector-length obarray)))
	((= index end))
      (for-each
       (lambda (bucket)
	 (proc (car bucket) (cdr bucket)))
       (vector-ref obarray index)))))


(define (module-map proc module)
  (let* ((obarray (module-obarray module))
	 (end (vector-length obarray)))

    (let loop ((i 0)
	       (answer '()))
      (if (= i end)
	  answer
	  (loop (+ 1 i)
		(append!
		 (map (lambda (bucket)
			(proc (car bucket) (cdr bucket)))
		      (vector-ref obarray i))
		 answer))))))


;;; {Low Level Bootstrapping}
;;;

;; make-root-module 

:; A root module uses the symhash table (the system's privileged 
;; obarray).  Being inside a root module is like using SCM without
;; any module system.
;;


(define (root-module-thunk m s define?)
  (let ((bi (and (symbol-interned? #f s)
		 (builtin-variable s))))
    (and bi
	 (or define? (variable-bound? bi))
	 (begin
	   (module-add! m s bi)
	   bi))))

(define (make-root-module)
  (make-module 1019 #f root-module-thunk))


;; make-scm-module 

;; An scm module is a module into which the lazy binder copies
;; variable bindings from the system symhash table.  The mapping is
;; one way only; newly introduced bindings in an scm module are not
;; copied back into the system symhash table (and can be used to override
;; bindings from the symhash table).
;;

(define (make-scm-module)
  (make-module 1019 #f
	       (lambda (m s define?)
		 (let ((bi (and (symbol-interned? #f s)
				(builtin-variable s))))
		   (and bi
			(variable-bound? bi)
			(begin
			  (module-add! m s bi)
			  bi))))))




;; the-module
;; 
(define the-module #f)

;; set-current-module module
;;
;; set the current module as viewed by the normalizer.
;;
(define (set-current-module m)
  (set! the-module m)
  (if m
      (set! *top-level-lookup-thunk* (module-eval-thunk the-module))
      (set! *top-level-lookup-thunk* #f)))


;; current-module
;;
;; return the current module as viewed by the normalizer.
;;
(define (current-module) the-module)

;;; {Module-based Loading}
;;;

(define (save-module-excursion thunk)
  (let ((inner-module (current-module))
	(outer-module #f))
    (dynamic-wind (lambda ()
		    (set! outer-module (current-module))
		    (set-current-module inner-module)
		    (set! inner-module #f))
		  thunk
		  (lambda ()
		    (set! inner-module (current-module))
		    (set-current-module outer-module)
		    (set! outer-module #f)))))

(define basic-try-load-with-path try-load-with-path)
(define basic-try-load try-load)
(define basic-load-with-path load-with-path)
(define basic-load load)


(define (try-load-module-with-path . args)
  (save-module-excursion (lambda () (apply basic-try-load-with-path args))))

(define (try-load-module . args)
  (save-module-excursion (lambda () (apply basic-try-load args))))

(define (load-module-with-path . args)
  (save-module-excursion (lambda () (apply basic-load-with-path args))))

(define (load-module . args)
  (save-module-excursion (lambda () (apply basic-load args))))




;; MODULE-REF -- exported
;;
;; Returns the value of a variable called NAME in MODULE or any of its
;; used modules.  If there is no such variable, then if the optional third
;; argument DEFAULT is present, it is returned; otherwise an error is signaled.
;; 
(define (module-ref module name . rest)
  (let ((variable (module-variable module name)))
    (if (and variable (variable-bound? variable))
	(variable-ref variable)
	(if (null? rest)
	    (error "No variable named" name 'in module)
	    (car rest)			; default value
	    ))))

;; MODULE-SET! -- exported
;;
;; Sets the variable called NAME in MODULE (or in a module that MODULE uses)
;; to VALUE; if there is no such variable, an error is signaled.
;; 
(define (module-set! module name value)
  (let ((variable (module-variable module name)))
    (if variable
	(variable-set! variable value)
	(error "No variable named" name 'in module))))

;; MODULE-DEFINE! -- exported
;;
;; Sets the variable called NAME in MODULE to VALUE; if there is no such
;; variable, it is added first.
;; 
(define (module-define! module name value)
  (let ((variable (module-local-variable module name)))
    (if variable
	(variable-set! variable value)
	(module-add! module name (make-variable value name)))))

;; MODULE-USE! module interface
;;
;; Add INTERFACE to the list of interfaces used by MODULE.
;; 
(define (module-use! module interface)
  (set-module-uses! module
		    (cons interface (delq! interface (module-uses module)))))




;;;;
;;; {Recursive Namespaces}
;;;
;;;
;;; A hierarchical namespace emerges if we consider some module to be
;;; root, and variables bound to modules as nested namespaces.
;;;
;;; The routines in this file manage variable names in hierarchical namespace.
;;; Each variable name is a list of elements, looked up in successively nested
;;; modules.
;;;
;;;		(resolved-ref some-root-module '(foo bar baz))
;;;		=> <value of a variable named baz in the module bound to bar in 
;;;		    the module bound to foo in some-root-module>
;;;
;;;
;;; There are:
;;;
;;;	;; a-root is a module
;;;	;; name is a list of symbols
;;;
;;;	resolved-ref a-root name
;;;	resolved-set! a-root name val
;;;	resolved-define! a-root name val
;;;	resolved-remove! a-root name
;;;
;;;
;;; (current-module) is a natural choice for a-root so for convenience there are
;;; also:
;;;
;;;	value-ref name		==	resolved-ref (current-module) name
;;;	value-set! name val	==	resolved-set! (current-module) name val
;;;	value-define! name val	==	resolved-define! (current-module) name val
;;;	value-remove! name	==	resolved-remove! (current-module) name
;;;


(define (resolved-ref root names)
  (let loop ((cur root)
	     (elts names))
    (cond
     ((null? elts)		cur)
     ((not (module? cur))	#f)
     (else (loop (module-ref cur (car elts) #f) (cdr elts))))))

(define (resolved-set! root names val)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-set! cur (car elts) val)
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (resolved-define! root names val)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-define! cur (car elts) val)
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (resolved-remove! root names)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-remove! cur (car elts))
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (value-ref names) (resolved-ref (current-module) names))
(define (value-set! names val) (resolved-set! (current-module) names val))
(define (value-define names val) (resolved-define! (current-module) names val))
(define (value-remove names) (resolved-remove! (current-module) names))




;;;;
;;; #/app
;;;
;;; The root of conventionally named objects not directly in the top level.
;;;
;;; #/app/modules
;;; #/app/modules/guile
;;;
;;; The directory of all modules and the standard root module.
;;;

(define (module-public-interface m) (module-ref m '%module-public-interface #f))
(define (set-module-public-interface! m i) (module-define! m '%module-public-interface i))
(define the-root-module (make-root-module))
(define the-scm-module (make-scm-module))
(set-module-public-interface! the-root-module the-scm-module)
(set-module-name! the-root-module 'the-root-module)
(set-module-name! the-scm-module 'the-scm-module)

(set-current-module the-root-module)

(define app (make-module 31))
(value-define '(app modules) (make-module 31))
(value-define '(app modules guile) the-root-module)

;; (define-special-value '(app modules new-ws) (lambda () (make-scm-module)))

(define (resolve-module name)
  (let ((full-name (append '(app modules) name)))
    (let ((already (value-ref full-name)))
    (or already
	(begin
	  (try-module-autoload name)
	  (make-modules-in (current-module) full-name))))))
	    
(define (beautify-user-module! module)
  (if (not (module-public-interface module))
      (let ((interface (make-module 31)))
	(set-module-name! interface (module-name module))
	(set-module-kind! interface 'interface)
	(set-module-public-interface! module interface)))
  (if (not (memq the-scm-module (module-uses module)))
      (set-module-uses! module (append (module-uses module) (list the-scm-module)))))

(define (make-modules-in module name)
  (if (null? name)
      module
      (cond
       ((module-ref module (car name) #f) => (lambda (m) (make-modules-in m (cdr name))))
       (else	(let ((m (make-module 31)))
		  (set-module-kind! m 'directory)
		  (set-module-name! m (car name))
		  (module-define! module (car name) m)
		  (make-modules-in m (cdr name)))))))

(define (resolve-interface name)
  (let ((module (resolve-module name)))
    (and module (module-public-interface module))))


(define %autoloader-developer-mode #t)

(define (process-define-module args)
  (let*  ((module-id (car args))
	  (module (resolve-module module-id))
	  (kws (cdr args)))
    (beautify-user-module! module)
    (let loop ((kws kws))
      (and (not (null? kws))
	   (case (car kws)
	     ((:use-module)
	      (if (not (pair? (cdr kws)))
		  (error "unrecognized defmodule argument" kws))
	      (let* ((used-name (cadr kws))
		     (used-module (resolve-module used-name)))
		(if (not (module-ref used-module '%module-public-interface #f))
		    (begin
		      ((if %autoloader-developer-mode warn error) "no code for module" used-module)
		      (beautify-user-module! used-module)))
		(let ((interface (module-ref used-module '%module-public-interface #f)))
		  (if (not interface)
		      (error "missing interface for use-module" used-module))
		  (set-module-uses! module
				    (append! (delq! interface (module-uses module))
					     (list interface)))))
	      (loop (cddr kws)))

	     (else	(error "unrecognized defmodule argument" kws)))))
    module))


(define autoloads-in-progress '())

(define (try-module-autoload module-name)
  
  (define (sfx name) (string-append name (scheme-file-suffix)))
  (let* ((reverse-name (reverse module-name))
	 (name (car reverse-name))
	 (dir-hint-module-name (reverse (cdr reverse-name)))
	 (dir-hint (apply symbol-append (map (lambda (elt) (symbol-append elt "/")) dir-hint-module-name))))
    (resolve-module dir-hint-module-name)
    (and (not (autoload-done-or-in-progress? dir-hint name))
	 (let ((didit #f))
	   (dynamic-wind
	    (lambda () (autoload-in-progress! dir-hint name))
	    (lambda () 
	      (let loop ((dirs %load-path))
		(and (not (null? dirs))
		     (or
		      (let ((d (car dirs))
			    (trys (list
				   dir-hint
				   (sfx dir-hint)
				   (in-vicinity dir-hint name)
				   (in-vicinity dir-hint (sfx name)))))
			(and (or-map (lambda (f)
				       (let ((full (in-vicinity d f)))
					 full
					 (and (not (file-is-directory? full))
					      (file-exists? full)
					      (begin
						(save-module-excursion
						 (lambda ()
						   (list f d)
						   (load-with-path f (list d))))
						#t))))
				     trys)
			     (begin
			       (set! didit #t)
			       #t)))
		      (loop (cdr dirs))))))
	    (lambda () (set-autoloaded! dir-hint name didit)))
	   didit))))

(define autoloads-done '((guile . guile)))

(define (autoload-done-or-in-progress? p m)
  (let ((n (cons p m)))
    (->bool (or (member n autoloads-done)
		(member n autoloads-in-progress)))))

(define (autoload-done! p m)
  (let ((n (cons p m)))
    (set! autoloads-in-progress
	  (delete! n autoloads-in-progress))
    (or (member n autoloads-done)
	(set! autoloads-done (cons n autoloads-done)))))

(define (autoload-in-progress! p m)
  (let ((n (cons p m)))
    (set! autoloads-done
	  (delete! n autoloads-done))
    (set! autoloads-in-progress (cons n autoloads-in-progress))))

(define (set-autoloaded! p m done?)
  (if done?
      (autoload-done! p m)
      (let ((n (cons p m)))
	(set! autoloads-done (delete! n autoloads-done))
	(set! autoloads-in-progress (delete! n autoloads-in-progress)))))





;;; {Macros}
;;;

(define macro-table (make-weak-key-hash-table 523))
(define xformer-table (make-weak-key-hash-table 523))

(define (defmacro? m)  (hashq-ref macro-table m))
(define (assert-defmacro?! m) (hashq-set! macro-table m #t))
(define (defmacro-transformer m) (hashq-ref xformer-table m))
(define (set-defmacro-transformer! m t) (hashq-set! xformer-table m t))

(define defmacro:transformer
  (lambda (f)
    (let* ((xform (lambda (exp env)
		    (copy-tree (apply f (cdr exp)))))
	   (a (procedure->memoizing-macro xform)))
      (assert-defmacro?! a)
      (set-defmacro-transformer! a f)
      a)))


(define defmacro
  (let ((defmacro-transformer
	  (lambda (name parms . body)
	    (let ((transformer `(lambda ,parms ,@body)))
	      `(define ,name
		 (,(lambda (transformer)
		     (defmacro:transformer transformer))
		  ,transformer))))))
    (defmacro:transformer defmacro-transformer)))

(define defmacro:syntax-transformer
  (lambda (f)
    (procedure->syntax
	      (lambda (exp env)
		(copy-tree (apply f (cdr exp)))))))

(define (macroexpand-1 e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (eval `(defined? ,a)) (eval a))))
		(if (defmacro? val)
		    (apply (defmacro-transformer val) (cdr e))
		    e)))
   (#t e)))

(define (macroexpand e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (eval `(defined? ,a)) (eval a))))
		(if (defmacro? val)
		    (macroexpand (apply (defmacro-transformer val) (cdr e)))
		    e)))
   (#t e)))

(define gentemp
  (let ((*gensym-counter* -1))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "scm:G" (number->string *gensym-counter*))))))




;;; {Running Repls}
;;;

(define (repl read evaler print)
  (let loop ((source (read (current-input-port) #t read-sharp)))
    (print (evaler source))
    (loop (read (current-input-port) #t read-sharp))))

;; A provisional repl that acts like the SCM repl:
;;
(define scm-repl-silent #f)
(define (assert-repl-silence v) (set! scm-repl-silent v))

(define scm-repl-verbose #t)
(define (assert-repl-verbosity v) (set! scm-repl-verbose v))

(define scm-repl-prompt #t)
(define (assert-repl-prompt v) (set! scm-repl-prompt v))

(define the-prompt-string "guile> ")

(define (error-catching-loop thunk)
  (define (loop first)
    (let ((next 
	   (catch #t
		  (lambda ()
		    (dynamic-wind
		     (lambda () (unmask-signals))
		     (lambda ()
		       (first)

		       ;; This line is needed because mark doesn't do closures quite right.
		       ;; Unreferenced locals should be collected.
		       ;;
		       (set! first #f)
		       (let loop ((v (thunk)))
			 (loop (thunk)))
		       #f)
		     (lambda () (mask-signals))))

		  (lambda (key . args)
		    (case key
		      ((quit)				(force-output)
							(pk 'quit args)
							#f)

		      ((abort)				;; This is one of the closures that require (set! first #f)
		       					;; above
		       					;;
		       					(lambda ()
								(force-output)
								(display  "ABORT: "  (current-error-port))
								(write args (current-error-port))
								(newline (current-error-port))))

		      (else				;; This is the other cons-leak closure...
		       					(lambda ()
							  (apply %%bad-throw  key args))))))))
      (and next (loop next))))
  (loop (lambda () #t)))

(define (quit . args)
  (apply throw 'quit args))

(define (error-catching-repl r e p)
  (error-catching-loop (lambda () (p (e (r))))))

(define (gc-run-time)
  (cdr (assq 'gc-time-taken (gc-stats))))

(define (scm-style-repl)
  (letrec (
	   (start-gc-rt #f)
	   (start-rt #f)
	   (repl-report-reset (lambda () #f))
	   (repl-report-start-timing (lambda ()
				       (set! start-gc-rt (gc-run-time))
				       (set! start-rt (get-internal-run-time))))
	   (repl-report (lambda ()
			  (display ";;; ")
			  (display (inexact->exact
				    (* 1000 (/ (- (get-internal-run-time) start-rt)
					       internal-time-units-per-second))))
			  (display "  msec  (")
			  (display  (inexact->exact
				     (* 1000 (/ (- (gc-run-time) start-gc-rt)
						internal-time-units-per-second))))
			  (display " msec in gc)\n")))
	   (-read (lambda ()
		    (if scm-repl-prompt
			(begin
			  (display the-prompt-string)
			  (force-output)
			  (repl-report-reset)))
		    (let ((val (read (current-input-port) #t read-sharp)))
		      (if (eof-object? val)
			  (begin
			    (if scm-repl-verbose
				(begin
				  (newline)
				  (display ";;; EOF -- quitting")
				  (newline)))
			    (quit 0)))
		      val)))

	   (-eval (lambda (sourc)
		    (repl-report-start-timing)
		    (eval sourc)))

	   (-print (lambda (result)
		     (if (not scm-repl-silent)
			 (begin
			   (print result)
			   (newline)
			   (if scm-repl-verbose
			       (repl-report))
			   (force-output)))))

	   (-quit (lambda ()
		    (if scm-repl-verbose
			(begin
			  (display ";;; QUIT executed, repl exitting")
			  (newline)
			  (repl-report)))
		    #t))

	   (-abort (lambda ()
		     (if scm-repl-verbose
			 (begin
			   (display ";;; ABORT executed.")
			   (newline)
			   (repl-report)))
		     (repl -read -eval -print))))

    (error-catching-repl -read
			 -eval
			 -print)))

(define (stand-alone-repl)
  (let ((oport (current-input-port)))
    (set-current-input-port *stdin*)
    (scm-style-repl)
    (set-current-input-port oport)))





(define (reverse-iota n) (if (> n 0) (cons (1- n) (reverse-iota (1- n))) '()))
(define (iota n) (list-reverse! (reverse-iota n)))


;;; {While}
;;;
;;; with `continue' and `break'.
;;;

(defmacro while (cond . body)
  `(letrec ((continue (lambda () (or (not ,cond) (begin (begin ,@ body) (continue)))))
	    (break (lambda val (apply throw 'break val))))
     (catch 'break
	    (lambda () (continue))
	    (lambda v (cadr v)))))




;;; {Macros}
;;;

;; actually....hobbit might be able to hack these with a little
;; coaxing
;;

(defmacro define-macro (first . rest)
  (let ((name (if (symbol? first) first (car first)))
	(transformer
	 (if (symbol? first)
	     (car rest)
	     `(lambda ,(cdr first) ,@rest))))
    `(define ,name (defmacro:transformer ,transformer))))


(defmacro define-syntax-macro (first . rest)
  (let ((name (if (symbol? first) first (car first)))
	(transformer
	 (if (symbol? first)
	     (car rest)
	     `(lambda ,(cdr first) ,@rest))))
    `(define ,name (defmacro:syntax-transformer ,transformer))))

;;; {Module System Macros}
;;;

(defmacro define-module args
  `(let* ((process-define-module process-define-module)
	  (set-current-module set-current-module)
	  (module (process-define-module ',args)))
     (set-current-module module)
     module))

(define define-private define)

(defmacro define-public args
  (define (syntax)
    (error "bad syntax" (list 'define-public args)))
  (define (defined-name n)
    (cond
     ((symbol? n)	n)
     ((pair? n)		(defined-name (car n)))
     (else 		(syntax))))
  (cond
   ((null? args)	(syntax))

   (#t 			(let ((name (defined-name (car args))))
			  `(begin
			     (let ((public-i (module-public-interface (current-module))))
			       ;; Make sure there is a local variable:
			       ;;
			       (module-define! (current-module)
					       ',name
					       (module-ref (current-module) ',name #f))
			       
			       ;; Make sure that local is exported:
			       ;;
			       (module-add! public-i ',name (module-variable (current-module) ',name)))
			       
			     ;; Now (re)define the var normally.
			     ;;
			     (define-private ,@ args))))))



(defmacro defmacro-public args
  (define (syntax)
    (error "bad syntax" (list 'defmacro-public args)))
  (define (defined-name n)
    (cond
     ((symbol? n)	n)
     (else 		(syntax))))
  (cond
   ((null? args)	(syntax))

   (#t 			(let ((name (defined-name (car args))))
			  `(begin
			     (let ((public-i (module-public-interface (current-module))))
			       ;; Make sure there is a local variable:
			       ;;
			       (module-define! (current-module)
					       ',name
					       (module-ref (current-module) ',name #f))
			       
			       ;; Make sure that local is exported:
			       ;;
			       (module-add! public-i ',name (module-variable (current-module) ',name)))
			       
			     ;; Now (re)define the var normally.
			     ;;
			     (defmacro ,@ args))))))




(define try-load-with-path try-load-module-with-path)
(define try-load try-load-module)
(define load-with-path load-module-with-path)
(define load load-module)




;; (define in-ch (get-standard-channel TCL_STDIN))
;; (define out-ch (get-standard-channel TCL_STDOUT))
;; (define err-ch (get-standard-channel TCL_STDERR))
;; 
;; (define inp (%make-channel-port in-ch "r"))
;; (define outp (%make-channel-port out-ch "w"))
;; (define errp (%make-channel-port err-ch "w"))
;; 
;; (define %system-char-ready? char-ready?)
;; 
;; (define (char-ready? p)
;;   (if (not (channel-port? p))
;;       (%system-char-ready? p)
;;       (let* ((channel (%channel-port-channel p))
;; 	     (old-blocking (channel-option-ref channel :blocking)))
;; 	(dynamic-wind
;; 	 (lambda () (set-channel-option the-root-tcl-interpreter channel :blocking "0"))
;; 	 (lambda () (not (eof-object? (peek-char p))))
;; 	 (lambda () (set-channel-option the-root-tcl-interpreter channel :blocking old-blocking))))))
;; 
;; (define (top-repl)
;;   (with-input-from-port inp
;;     (lambda ()
;;       (with-output-to-port outp
;; 	(lambda ()
;; 	  (with-error-to-port errp
;; 	    (lambda ()
;; 	      (scm-style-repl))))))))
;; 
;; (set-current-input-port inp)
;; (set-current-output-port outp)
;; (set-current-error-port errp)

(define (top-repl) (scm-style-repl))

(defmacro false-if-exception (expr)
  `(catch #t (lambda () ,expr)
	  (lambda args #f)))


(define-module (ice-9 calling))

;;;;
;;; {Calling Conventions}
;;;
;;; This file contains a number of macros that support 
;;; common calling conventions.

;;;
;;; with-excursion-function <vars> proc
;;;  <vars> is an unevaluated list of names that are bound in the caller.
;;;  proc is a procedure, called:
;;;	     (proc excursion)
;;;
;;;  excursion is a procedure isolates all changes to <vars>
;;;  in the dynamic scope of the call to proc.  In other words,
;;;  the values of <vars> are saved when proc is entered, and when
;;;  proc returns, those values are restored.   Values are also restored
;;;  entering and leaving the call to proc non-locally, such as using
;;;  call-with-current-continuation, error, or throw.
;;;
(defmacro-public with-excursion-function (vars proc)
  `(,proc ,(excursion-function-syntax vars)))



;;; with-getter-and-setter <vars> proc
;;;  <vars> is an unevaluated list of names that are bound in the caller.
;;;  proc is a procedure, called:
;;;	(proc getter setter)
;;; 
;;;  getter and setter are procedures used to access
;;;  or modify <vars>.
;;; 
;;;  setter, called with keywords arguments, modifies the named
;;;  values.   If "foo" and "bar" are among <vars>, then:
;;; 
;;;	(setter :foo 1 :bar 2)
;;;	== (set! foo 1 bar 2)
;;; 
;;;  getter, called with just keywords, returns
;;;  a list of the corresponding values.  For example,
;;;  if "foo" and "bar" are among the <vars>, then
;;; 
;;;	(getter :foo :bar)
;;;	=> (<value-of-foo> <value-of-bar>)
;;; 
;;;  getter, called with no arguments, returns a list of all accepted 
;;;  keywords and the corresponding values.  If "foo" and "bar" are
;;;  the *only* <vars>, then:
;;; 
;;;	(getter)
;;;	=> (:foo <value-of-bar> :bar <value-of-foo>)
;;; 
;;;  The unusual calling sequence of a getter supports too handy
;;;  idioms:
;;; 
;;;	(apply setter (getter))		;; save and restore
;;; 
;;;	(apply-to-args (getter :foo :bar)		;; fetch and bind
;;;		    (lambda (foo bar) ....))
;;; 
;;;     ;; [ "apply-to-args" is just like two-argument "apply" except that it 
;;;	;;   takes its arguments in a different order.
;;; 
;;;
(defmacro-public with-getter-and-setter (vars proc)
  `(,proc ,@ (getter-and-setter-syntax vars)))

;;; with-getter vars proc
;;;   A short-hand for a call to with-getter-and-setter.
;;;   The procedure is called:
;;;		(proc getter)
;;;
(defmacro-public with-getter (vars proc)
  `(,proc ,(car (getter-and-setter-syntax vars))))


;;; with-delegating-getter-and-setter <vars> get-delegate set-delegate proc
;;;   Compose getters and setters.
;;; 
;;;   <vars> is an unevaluated list of names that are bound in the caller.
;;;   
;;;   get-delegate is called by the new getter to extend the set of 
;;;	gettable variables beyond just <vars>
;;;   set-delegate is called by the new setter to extend the set of 
;;;	gettable variables beyond just <vars>
;;;
;;;   proc is a procedure that is called
;;;		(proc getter setter)
;;;
(defmacro-public with-delegating-getter-and-setter (vars get-delegate set-delegate proc)
  `(,proc ,@ (delegating-getter-and-setter-syntax vars get-delegate set-delegate)))


;;; with-delegating-getter-and-setter <vars> get-delegate set-delegate proc
;;;   <vars> is an unevaluated list of names that are bound in the caller.
;;;   proc is called:
;;;
;;;		(proc excursion getter setter)
;;;
;;;   See also:
;;;	with-getter-and-setter
;;;	with-excursion-function
;;;
(defmacro-public with-excursion-getter-and-setter (vars proc)
  `(,proc  ,(excursion-function-syntax vars)
	  ,@ (getter-and-setter-syntax vars)))


(define (excursion-function-syntax vars)
  (let ((saved-value-names (map gensym vars))
	(tmp-var-name (gensym 'temp))
	(swap-fn-name (gensym 'swap))
	(thunk-name (gensym 'thunk)))
    `(lambda (,thunk-name)
	      (letrec ((,tmp-var-name #f)
		       (,swap-fn-name
			(lambda () ,@ (map (lambda (n sn) `(set! ,tmp-var-name ,n ,n ,sn ,sn ,tmp-var-name))
					   vars saved-value-names)))
		       ,@ (map (lambda (sn n) `(,sn ,n)) saved-value-names vars))
		(dynamic-wind
		 ,swap-fn-name
		 ,thunk-name
		 ,swap-fn-name)))))


(define (getter-and-setter-syntax vars)
  (let ((args-name (gensym 'args))
	(an-arg-name (gensym 'an-arg))
	(new-val-name (gensym 'new-value))
	(loop-name (gensym 'loop))
	(kws (map symbol->keyword vars)))
    (list `(lambda ,args-name
	     (let ,loop-name ((,args-name ,args-name))
		  (if (null? ,args-name)
		      ,(if (null? kws)
			   ''()
			   `(let ((all-vals (,loop-name ',kws)))
			      (let ,loop-name ((vals all-vals)
					       (kws ',kws))
				   (if (null? vals)
				       '()
				       `(,(car kws) ,(car vals) ,@(,loop-name (cdr vals) (cdr kws)))))))
		      (map (lambda (,an-arg-name)
			     (case ,an-arg-name
			       ,@ (append
				   (map (lambda (kw v) `((,kw) ,v)) kws vars)
				   `((else (throw 'bad-get-option ,an-arg-name))))))
			   ,args-name))))

	  `(lambda ,args-name
	     (let ,loop-name ((,args-name ,args-name))
		  (or (null? ,args-name)
		      (null? (cdr ,args-name))
		      (let ((,an-arg-name (car ,args-name))
			    (,new-val-name (cadr ,args-name)))
			(case ,an-arg-name
			  ,@ (append
			      (map (lambda (kw v) `((,kw) (set! ,v ,new-val-name))) kws vars)
			      `((else (throw 'bad-set-option ,an-arg-name)))))
			(,loop-name (cddr ,args-name)))))))))

(define (delegating-getter-and-setter-syntax  vars get-delegate set-delegate)
  (let ((args-name (gensym 'args))
	(an-arg-name (gensym 'an-arg))
	(new-val-name (gensym 'new-value))
	(loop-name (gensym 'loop))
	(kws (map symbol->keyword vars)))
    (list `(lambda ,args-name
	     (let ,loop-name ((,args-name ,args-name))
		  (if (null? ,args-name)
		      (append!
		       ,(if (null? kws)
			    ''()
			    `(let ((all-vals (,loop-name ',kws)))
			       (let ,loop-name ((vals all-vals)
						(kws ',kws))
				    (if (null? vals)
					'()
					`(,(car kws) ,(car vals) ,@(,loop-name (cdr vals) (cdr kws)))))))
		       (,get-delegate))
		      (map (lambda (,an-arg-name)
			     (case ,an-arg-name
			       ,@ (append
				   (map (lambda (kw v) `((,kw) ,v)) kws vars)
				   `((else (car (,get-delegate ,an-arg-name)))))))
			   ,args-name))))

	  `(lambda ,args-name
	     (let ,loop-name ((,args-name ,args-name))
		  (or (null? ,args-name)
		      (null? (cdr ,args-name))
		      (let ((,an-arg-name (car ,args-name))
			    (,new-val-name (cadr ,args-name)))
			(case ,an-arg-name
			  ,@ (append
			      (map (lambda (kw v) `((,kw) (set! ,v ,new-val-name))) kws vars)
			      `((else  (,set-delegate ,an-arg-name ,new-val-name)))))
			(,loop-name (cddr ,args-name)))))))))




;;; with-configuration-getter-and-setter <vars-etc> proc
;;;
;;;  Create a getter and setter that can trigger arbitrary computation.
;;;
;;;  <vars-etc> is a list of variable specifiers, explained below.
;;;  proc is called:
;;;
;;;		(proc getter setter)
;;;
;;;   Each element of the <vars-etc> list is of the form:
;;;
;;;	(<var> getter-hook setter-hook)
;;;
;;;   Both hook elements are evaluated; the variable name is not.
;;;   Either hook may be #f or procedure.
;;;
;;;   A getter hook is a thunk that returns a value for the corresponding
;;;   variable.   If omitted (#f is passed), the binding of <var> is
;;;   returned.
;;;
;;;   A setter hook is a procedure of one argument that accepts a new value
;;;   for the corresponding variable.  If omitted, the binding of <var>
;;;   is simply set using set!.
;;;
(defmacro-public with-configuration-getter-and-setter (vars-etc proc)
  `((lambda (simpler-get simpler-set body-proc)
      (with-delegating-getter-and-setter ()
	simpler-get simpler-set body-proc))

    (lambda (kw)
      (case kw
	,@(map (lambda (v) `((,(symbol->keyword (car v)))
			     ,(cond
			       ((cadr v)	=> list)
			       (else		`(list ,(car v))))))
	       vars-etc)))

    (lambda (kw new-val)
      (case kw
	,@(map (lambda (v) `((,(symbol->keyword (car v)))
			     ,(cond
			       ((caddr v)	=> (lambda (proc) `(,proc new-val)))
			       (else		`(set! ,(car v) new-val)))))
	       vars-etc)))

       ,proc))

(defmacro-public with-delegating-configuration-getter-and-setter (vars-etc delegate-get delegate-set proc)
  `((lambda (simpler-get simpler-set body-proc)
      (with-delegating-getter-and-setter ()
	simpler-get simpler-set body-proc))

    (lambda (kw)
      (case kw
	,@(append! (map (lambda (v) `((,(symbol->keyword (car v)))
				      ,(cond
					((cadr v)	=> list)
					(else		`(list ,(car v))))))
			vars-etc)
		   `((else (,delegate-get kw))))))

    (lambda (kw new-val)
      (case kw
	,@(append! (map (lambda (v) `((,(symbol->keyword (car v)))
				      ,(cond
					((caddr v)	=> (lambda (proc) `(,proc new-val)))
					(else		`(set! ,(car v) new-val)))))
			vars-etc)
		   `((else (,delegate-set kw new-val))))))

    ,proc))


;;; let-configuration-getter-and-setter <vars-etc> proc
;;;
;;;   This procedure is like with-configuration-getter-and-setter (q.v.)
;;;   except that each element of <vars-etc> is:
;;;
;;;		(<var> initial-value getter-hook setter-hook)
;;;
;;;   Unlike with-configuration-getter-and-setter, let-configuration-getter-and-setter
;;;   introduces bindings for the variables named in <vars-etc>.
;;;   It is short-hand for:
;;;
;;;		(let ((<var1> initial-value-1)
;;;		      (<var2> initial-value-2)
;;;			...)
;;;		  (with-configuration-getter-and-setter ((<var1> v1-get v1-set) ...) proc))
;;;
(defmacro-public let-with-configuration-getter-and-setter (vars-etc proc)
  `(let ,(map (lambda (v) `(,(car v) ,(cadr v))) vars-etc)
     (with-configuration-getter-and-setter ,(map (lambda (v) `(,(car v) ,(caddr v) ,(cadddr v))) vars-etc)
					   ,proc)))




(define-module (ice-9 common-list))

;;"comlist.scm" Implementation of COMMON LISP list functions for Scheme
; Copyright (C) 1991, 1993, 1995 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.




;;;From: hugh@ear.mit.edu (Hugh Secker-Walker)
(define-public (make-list k . init)
  (set! init (if (pair? init) (car init)))
  (do ((k k (+ -1 k))
       (result '() (cons init result)))
      ((<= k 0) result)))

(define-public (adjoin e l) (if (memq e l) l (cons e l)))

(define-public (union l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	(else (union (cdr l1) (adjoin (car l1) l2)))))

(define-public (intersection l1 l2)
  (cond ((null? l1) l1)
	((null? l2) l2)
	((memv (car l1) l2) (cons (car l1) (intersection (cdr l1) l2)))
	(else (intersection (cdr l1) l2))))

(define-public (set-difference l1 l2)
  (cond ((null? l1) l1)
	((memv (car l1) l2) (set-difference (cdr l1) l2))
	(else (cons (car l1) (set-difference (cdr l1) l2)))))

(define-public (reduce-init p init l)
  (if (null? l)
      init
      (reduce-init p (p init (car l)) (cdr l))))

(define-public (reduce p l)
  (cond ((null? l) l)
	((null? (cdr l)) (car l))
	(else (reduce-init p (car l) (cdr l)))))

(define-public (some pred l . rest)
  (cond ((null? rest)
	 (let mapf ((l l))
	   (and (not (null? l))
		(or (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(and (not (null? l))
		     (or (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

(define-public (every pred l . rest)
  (cond ((null? rest)
	 (let mapf ((l l))
	   (or (null? l)
	       (and (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(or (null? l)
		    (and (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

(define-public (notany pred . ls) (not (apply some pred ls)))

(define-public (notevery pred . ls) (not (apply every pred ls)))

(define-public (find-if t l)
  (cond ((null? l) #f)
	((t (car l)) (car l))
	(else (find-if t (cdr l)))))

(define-public (member-if t l)
  (cond ((null? l) #f)
	((t (car l)) l)
	(else (member-if t (cdr l)))))

(define-public (remove-if p l)
  (cond ((null? l) '())
	((p (car l)) (remove-if p (cdr l)))
	(else (cons (car l) (remove-if p (cdr l))))))

(define-public (delete-if! pred list)
  (let delete-if ((list list))
    (cond ((null? list) '())
	  ((pred (car list)) (delete-if (cdr list)))
	  (else
	   (set-cdr! list (delete-if (cdr list)))
	   list)))) 

(define-public (delete-if-not! pred list)
  (let delete-if ((list list))
    (cond ((null? list) '())
	  ((not (pred (car list))) (delete-if (cdr list)))
	  (else
	   (set-cdr! list (delete-if (cdr list)))
	   list))))

(define-public (butlast lst n)
  (letrec ((l (- (length lst) n))
	   (bl (lambda (lst n)
		 (cond ((null? lst) lst)
		       ((positive? n)
			(cons (car lst) (bl (cdr lst) (+ -1 n))))
		       (else '())))))
    (bl lst (if (negative? n)
		(slib:error "negative argument to butlast" n)
		l))))

(define-public (and? . args)
  (cond ((null? args) #t)
	((car args) (apply and? (cdr args)))
	(else #f)))

(define-public (or? . args)
  (cond ((null? args) #f)
	((car args) #t)
	(else (apply or? (cdr args)))))

(define-public (has-duplicates? lst)
  (cond ((null? lst) #f)
	((member (car lst) (cdr lst)) #t)
	(else (has-duplicates? (cdr lst)))))

(define-public (list* x . y)	
  (define (list*1 x)
    (if (null? (cdr x))
	(car x)
	(cons (car x) (list*1 (cdr x)))))
  (if (null? y)
      x
      (cons x (list*1 y))))

;; pick p l
;; Apply P to each element of L, returning a list of elts
;; for which P returns a non-#f value.
;;
(define-public (pick p l)
  (let loop ((s '())
	     (l l))
    (cond
     ((null? l) 	s)
     ((p (car l))	(loop (cons (car l) s) (cdr l)))
     (else		(loop s (cdr l))))))

;; pick p l
;; Apply P to each element of L, returning a list of the 
;; non-#f return values of P.
;;
(define-public (pick-mappings p l)
  (let loop ((s '())
	     (l l))
    (cond
     ((null? l) 	s)
     ((p (car l)) =>	(lambda (mapping) (loop (cons mapping s) (cdr l))))
     (else		(loop s (cdr l))))))

(define-public (uniq l)
  (if (null? l)
      '()
      (let ((u (uniq (cdr l))))
	(if (memq (car l) u)
	    u
	    (cons (car l) u)))))


(define-module (ice-9 ls)
  :use-module (ice-9 common-list))



;;;;
;;;	local-definitions-in root name
;;;		Returns a list of names defined locally in the named subdirectory of root.
;;;	definitions-in root name
;;;		Returns a list of all names defined in the named subdirectory of root.
;;;		The list includes alll locally defined names as well as all names inherited
;;;		from a member of a use-list.
;;;
;;; A convenient interface for examining the nature of things:
;;;
;;;	ls . various-names
;;;
;;;		With just one argument, interpret that argument as the name of a subdirectory
;;;		of the current module and return a list of names defined there.
;;;
;;;		With more than one argument, still compute subdirectory lists, but
;;;		return a list:
;;;			((<subdir-name> . <names-defined-there>)
;;;			 (<subdir-name> . <names-defined-there>)
;;;			 ...)
;;;

(define-public (local-definitions-in root names)
  (let ((m (resolved-ref root names))
	(answer '()))
    (if (not (module? m))
	(set! answer m)
	(module-for-each (lambda (k v) (set! answer (cons k answer))) m))
    answer))

(define-public (definitions-in root names)
  (let ((m (resolved-ref root names)))
    (if (not (module? m))
	m
	(reduce union
		(cons (local-definitions-in m  '())
		      (map (lambda (m2) (definitions-in m2 '())) (module-uses m)))))))

(define-public (ls . various-refs)
  (and various-refs
       (if (cdr various-refs)
	   (map (lambda (ref)
		  (cons ref (definitions-in (current-module) ref)))
		various-refs)
	   (definitions-in (current-module) (car various-refs)))))

(define-public (lls . various-refs)
  (and various-refs
       (if (cdr various-refs)
	   (map (lambda (ref)
		  (cons ref (local-definitions-in (current-module) ref)))
		various-refs)
	   (local-definitions-in (current-module) (car various-refs)))))

(define-public (recursive-value-define name value)
  (let ((parent (reverse! (cdr (reverse name)))))
    (and parent (make-modules-in (current-module) parent))
    (value-define name value)))

(define-module (ice-9 q))

;;;; 	Copyright (C) 1995 Free Software Foundation, Inc.
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


;;;;
;;; Q: Based on the interface to 
;;;
;;; "queue.scm"  Queues/Stacks for Scheme 
;;;  Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.
;;;


;;;;
;;; {Q}
;;;
;;; A list is just a bunch of cons pairs that follows some constrains, right?
;;; Association lists are the same.  Hash tables are just vectors and association
;;; lists.  You can print them, read them, write them as constants, pun them off as other data
;;; structures etc. This is good.  This is lisp.   These structures are fast and compact
;;; and easy to manipulate arbitrarily because of their simple, regular structure and 
;;; non-disjointedness (associations being lists and so forth).   
;;;
;;; So I figured, queues should be the same -- just a "subtype" of cons-pair 
;;; structures in general.
;;;
;;; A queue is a cons pair:
;;;		( <the-q> . <last-pair> )
;;;
;;; <the-q> is a list of things in the q.   New elements go at the end of that list.
;;;
;;; <last-pair> is #f if the q is empty, and otherwise is the last pair of <the-q>.
;;;
;;; q's print nicely, but alas, they do not read well because the eq?-ness of 
;;; <last-pair> and (last-pair <the-q>) is lost by read.   The procedure
;;; 
;;;		(sync-q! q)
;;;
;;; recomputes and resets the <last-pair> component of a queue.
;;;

(define-public (sync-q! obj) (set-cdr! obj (and (car obj) (last-pair (car obj)))))

;;; make-q
;;;  return a new q.
;;;
(define-public (make-q) (cons '() '()))

;;; q? obj
;;;   Return true if obj is a Q.
;;;   An object is a queue if it is equal? to '(#f . #f) or
;;;   if it is a pair P with (list? (car P)) and (eq? (cdr P) (last-pair P)).
;;;
(define-public (q? obj) (and (pair? obj)
				 (or (and (null? (car obj))
					  (null? (cdr obj)))
				     (and
				      (list? (car obj))
				      (eq? (cdr obj) (last-pair (car obj)))))))

;;; q-empty? obj
;;;  
(define-public (q-empty? obj) (null? (car obj)))

;;; q-empty-check q
;;;  Throw a q-empty exception if Q is empty.
(define-public (q-empty-check q) (if (q-empty? q) (throw 'q-empty q)))


;;; q-front q
;;;  Return the first element of Q.
(define-public (q-front q) (q-empty-check q) (caar q))

;;; q-front q
;;;  Return the last element of Q.
(define-public (q-rear q) (q-empty-check q) (cadr q))

;;; q-remove! q obj
;;;  Remove all occurences of obj from Q.
(define-public (q-remove! q obj)
  (while (memq obj (car q))
	 (set-car! q (delq! obj (car q))))
  (set-cdr! q (last-pair (car q))))

;;; q-push! q obj
;;;  Add obj to the front of Q
(define-public (q-push! q d)
  (let ((h (cons d (car q))))
    (set-car! q h)
    (if (null? (cdr q))
	(set-cdr! q h))))

;;; enq! q obj
;;;  Add obj to the rear of Q
(define-public (enq! q d)
  (let ((h (cons d '())))
    (if (not (null? (cdr q)))
       	(set-cdr! (cdr q) h)
	(set-car! q h))
    (set-cdr! q h)))

;;; q-pop! q
;;;  Take the front of Q and return it.
(define-public (q-pop! q)
  (q-empty-check q)
  (let ((it (caar q))
	(next (cdar q)))
    (if (not next)
	(set-cdr! q #f))
    (set-car! q next)
    it))

;;; deq! q
;;;  Take the front of Q and return it.
(define-public deq! q-pop!)

;;; q-length q
;;;  Return the number of enqueued elements.
;;;
(define-public (q-length q) (length (car q)))




;;; installed-scm-file
(define-module (ice-9 runq)
  :use-module (ice-9 q))



;;;; 	Copyright (C) 1996 Free Software Foundation, Inc.
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



;;;;
;;; {The runq data structure}
;;; 
;;; One way to schedule parallel computations in a serial environment is
;;; to explicitly divide each task up into small, finite execution time,
;;; strips.  Then you interleave the execution of strips from various
;;; tasks to achieve a kind of parallelism.  Runqs are a handy data
;;; structure for this style of programming.
;;; 
;;; We use thunks (nullary procedures) and lists of thunks to represent
;;; strips.  By convention, the return value of a strip-thunk must either
;;; be another strip or the value #f.
;;; 
;;; A runq is a procedure that manages a queue of strips.  Called with no
;;; arguments, it processes one strip from the queue.  Called with
;;; arguments, the arguments form a control message for the queue.  The
;;; first argument is a symbol which is the message selector.
;;; 
;;; A strip is processed this way: If the strip is a thunk, the thunk is
;;; called -- if it returns a strip, that strip is added back to the
;;; queue.  To process a strip which is a list of thunks, the CAR of that
;;; list is called.  After a call to that CAR, there are 0, 1, or 2 strips
;;; -- perhaps one returned by the thunk, and perhaps the CDR of the
;;; original strip if that CDR is not nil.  The runq puts whichever of
;;; these strips exist back on the queue.  (The exact order in which
;;; strips are put back on the queue determines the scheduling behavior of
;;; a particular queue -- it's a parameter.)
;;; 
;;; 



;;;;
;;; 	(runq-control q msg . args)
;;; 
;;; 		processes in the default way the control messages that
;;; 		can be sent to a runq.  Q should be an ordinary
;;; 		Q (see utils/q.scm).
;;; 
;;; 		The standard runq messages are:
;;; 
;;; 		'add! strip0 strip1...		;; to enqueue one or more strips
;;; 		'enqueue! strip0 strip1...	;; to enqueue one or more strips
;;; 		'push! strip0 ...		;; add strips to the front of the queue
;;; 		'empty?				;; true if it is
;;; 		'length				;; how many strips in the queue?
;;; 		'kill!				;; empty the queue
;;; 		else				;; throw 'not-understood
;;; 
(define-public (runq-control q msg . args)
  (case msg
    ((add!)			(for-each (lambda (t) (enq! q t)) args) '*unspecified*)
    ((enque!)			(for-each (lambda (t) (enq! q t)) args) '*unspecified*)
    ((push!)			(for-each (lambda (t) (q-push! q t)) args) '*unspecified*)
    ((empty?)			(q-empty? q))
    ((length)			(q-length q))
    ((kill!)			(set! q (make-q)))
    (else			(throw 'not-understood msg args))))

(define (run-strip thunk) (catch #t thunk (lambda ign (warn 'runq-strip thunk ign) #f)))

;;;;
;;; make-void-runq
;;;
;;; Make a runq that discards all messages except "length", for which
;;; it returns 0.
;;;
(define-public (make-void-runq)
  (lambda opts
    (and opts
	(apply-to-args opts
	  (lambda (msg . args)
	    (case msg
	      ((length)		0)
	      (else		#f)))))))

;;;; 
;;; 	(make-fair-runq)
;;; 
;;; 		Returns a runq procedure.
;;; 		Called with no arguments, the procedure processes one strip from the queue.
;;; 		Called with arguments, it uses runq-control.
;;; 
;;; 		In a fair runq, if a strip returns a new strip X, X is added
;;; 		to the end of the queue, meaning it will be the last to execute
;;; 		of all the remaining procedures.
;;; 
(define-public (make-fair-runq)
  (letrec ((q (make-q))
	   (self 
	    (lambda ctl
	      (if ctl
		  (apply runq-control q ctl)
		  (and (not (q-empty? q))
		       (let ((next-strip (deq! q)))
			 (cond
			  ((procedure? next-strip)	(let ((k (run-strip next-strip)))
							  (and k (enq! q k))))
			  ((pair? next-strip) (let ((k (run-strip (car next-strip))))
						(and k (enq! q k)))
					      (if (not (null? (cdr next-strip)))
						  (enq! q (cdr next-strip)))))
			 self))))))
    self))


;;;; 
;;; 	(make-exclusive-runq)
;;; 
;;; 		Returns a runq procedure.
;;; 		Called with no arguments, the procedure processes one strip from the queue.
;;; 		Called with arguments, it uses runq-control.
;;; 
;;; 		In an exclusive runq, if a strip W returns a new strip X, X is added
;;; 		to the front of the queue, meaning it will be the next to execute
;;; 		of all the remaining procedures.
;;; 
;;; 		An exception to this occurs if W was the CAR of a list of strips.
;;; 		In that case, after the return value of W is pushed onto the front
;;; 	 	of the queue, the CDR of the list of strips is pushed in front
;;; 		of that (if the CDR is not nil).   This way, the rest of the thunks
;;; 		in the list that contained W have priority over the return value of W.
;;; 
(define-public (make-exclusive-runq)
  (letrec ((q (make-q))
	   (self 
	    (lambda ctl
	      (if ctl
		  (apply runq-control q ctl)
		  (and (not (q-empty? q))
		       (let ((next-strip (deq! q)))
			 (cond
			  ((procedure? next-strip)	(let ((k (run-strip next-strip)))
							  (and k (q-push! q k))))
			  ((pair? next-strip) (let ((k (run-strip (car next-strip))))
						(and k (q-push! q k)))
					      (if (not (null? (cdr next-strip)))
						  (q-push! q (cdr next-strip)))))
			 self))))))
    self))


;;;; 
;;; 	(make-subordinate-runq-to superior basic-inferior)
;;; 
;;; 		Returns a runq proxy for the runq basic-inferior.
;;; 
;;; 		The proxy watches for operations on the basic-inferior that cause
;;; 		a transition from a queue length of 0 to a non-zero length and 
;;; 		vice versa.   While the basic-inferior queue is not empty,
;;; 		the proxy installs a task on the superior runq.  Each strip
;;; 		of that task processes N strips from the basic-inferior where
;;; 		N is the length of the basic-inferior queue when the proxy
;;; 		strip is entered.  [Countless scheduling variations are possible.]		
;;; 
(define-public (make-subordinate-runq-to superior-runq basic-runq)
  (let ((runq-task (cons #f #f)))
    (set-car! runq-task
	      (lambda ()
		(if (basic-runq 'empty?)
		    (set-cdr! runq-task #f)
		    (do ((n (basic-runq 'length) (1- n)))
			((<= n 0)		 #f)
		      (basic-runq)))))
    (letrec ((self
	      (lambda ctl
		(if (not ctl)
		    (let ((answer (basic-runq)))
		      (self 'empty?)
		      answer)
		    (begin
		      (case (car ctl)
			((suspend)		(set-cdr! runq-task #f))
			(else		      	(let ((answer (apply basic-runq ctl)))
						  (if (and (not (cdr runq-task)) (not (basic-runq 'empty?)))
						      (begin
							(set-cdr! runq-task runq-task)
							(superior-runq 'add! runq-task)))
						  answer))))))))
      self)))

;;;;
;;;	(define fork-strips (lambda args args))
;;;		Return a strip that starts several strips in 
;;;		parallel.   If this strip is enqueued on a fair
;;;		runq, strips of the parallel subtasks will run
;;;		round-robin style.
;;;
(define fork-strips (lambda args args))


;;;; 		
;;; 	(strip-sequence . strips)
;;; 
;;; 		Returns a new strip which is the concatenation of the argument strips.
;;; 
(define-public ((strip-sequence . strips))
  (let loop ((st (let ((a strips)) (set! strips #f) a)))
    (and (not (null? st))
	 (let ((then ((car st))))
	   (if then
	       (lambda () (loop (cons then (cdr st))))
	       (lambda () (loop (cdr st))))))))


;;;;
;;; 	(fair-strip-subtask . initial-strips)
;;; 
;;; 		Returns a new strip which is the synchronos, fair,
;;; 		parallel execution of the argument strips.
;;; 
;;; 
;;;
(define-public (fair-strip-subtask . initial-strips)
  (let ((st (make-fair-runq)))
    (apply st 'add! initial-strips)
    st))



;;; installed-scm-file

(define-module (ice-9 string-fun))


;;;;
;;; {String Fun}
;;;
;;; Various string funcitons, particularly those that take
;;; advantage of the "shared substring" capability.
;;;

;;;;
;;; {Dividing Strings Into Fields}
;;; 
;;; The names of these functions are very regular.
;;; Here is a grammar of a call to one of these:
;;;
;;;   <string-function-invocation>
;;;   := (<action>-<seperator-disposition>-<seperator-determination> <seperator-param> <str> <ret>)
;;;
;;; <str>    = the string
;;;
;;; <ret>    = The continuation.  String functions generally return
;;;	       multiple values by passing them to this procedure.
;;;
;;; <action> =    split
;;;		| separate-fields
;;;
;;;		"split" means to divide a string into two parts.
;;;			<ret> will be called with two arguments.
;;;
;;;		"separate-fields" means to divide a string into as many
;;;			parts as possible.  <ret> will be called with
;;;			however many fields are found.
;;;
;;; <seperator-disposition> = 	  before
;;;				| after
;;;				| discarding
;;;
;;;		"before" means to leave the seperator attached to
;;;			the beginning of the field to its right.
;;;		"after" means to leave the seperator attached to
;;;			the end of the field to its left.
;;;		"discarding" means to discard seperators.
;;;
;;;		Other dispositions might be handy.  For example, "isolate"
;;;		could mean to treat the separator as a field unto itself.
;;;
;;; <seperator-determination> =	  char
;;;				| predicate
;;;
;;;		"char" means to use a particular character as field seperator.
;;;		"predicate" means to check each character using a particular predicate.
;;;		
;;;		Other determinations might be handy.  For example, "character-set-member".
;;;
;;; <seperator-param> = A parameter that completes the meaning of the determinations.
;;;			For example, if the determination is "char", then this parameter
;;;			says which character.  If it is "predicate", the parameter is the
;;;			predicate.
;;;
;;;
;;; For example:
;;;
;;;		(separate-fields-discarding-char #\, "foo, bar, baz, , bat" list)
;;;		=> ("foo" " bar" " baz" " " " bat")
;;;
;;;		(split-after-char #\- 'an-example-of-split list)
;;;		=> ("an-" "example-of-split")
;;;
;;; As an alternative to using a determination "predicate", or to trying to do anything
;;; complicated with these functions, consider using regular expressions.
;;;

(define-public (split-after-char char str ret)
  (let ((end (cond
	      ((string-index str char) => 1+)
	      (else (string-length str)))))
    (ret (make-shared-substring str 0 end)
	 (make-shared-substring str end))))

(define-public (split-before-char char str ret)
  (let ((end (or (string-index str char)
		 (string-length str))))
    (ret (make-shared-substring str 0 end)
	 (make-shared-substring str end))))

(define-public (split-discarding-char char str ret)
  (let ((end (string-index str char)))
    (if (not end)
	(ret str "")
	(ret (make-shared-substring str 0 end)
	     (make-shared-substring str (1+ end))))))

(define-public (split-after-char-last char str ret)
  (let ((end (cond
	      ((string-rindex str char) => 1+)
	      (else 0))))
    (ret (make-shared-substring str 0 end)
	 (make-shared-substring str end))))

(define-public (split-before-char-last char str ret)
  (let ((end (or (string-rindex str char) 0)))
    (ret (make-shared-substring str 0 end)
	 (make-shared-substring str end))))

(define-public (split-discarding-char-last char str ret)
  (let ((end (string-rindex str char)))
    (if (not end)
	(ret str "")
	(ret (make-shared-substring str 0 end)
	     (make-shared-substring str (1+ end))))))

(define (split-before-predicate pred str ret)
  (let loop ((n 0))
    (cond
     ((= n (length str))		(ret str ""))
     ((not (pred (string-ref str n)))	(loop (1+ n)))
     (else				(ret (make-shared-substring str 0 n)
					     (make-shared-substring str n))))))
(define (split-after-predicate pred str ret)
  (let loop ((n 0))
    (cond
     ((= n (length str))		(ret str ""))
     ((not (pred (string-ref str n)))	(loop (1+ n)))
     (else				(ret (make-shared-substring str 0 (1+ n))
					     (make-shared-substring str (1+ n)))))))

(define (split-discarding-predicate pred str ret)
  (let loop ((n 0))
    (cond
     ((= n (length str))		(ret str ""))
     ((not (pred (string-ref str n)))	(loop (1+ n)))
     (else				(ret (make-shared-substring str 0 n)
					     (make-shared-substring str (1+ n)))))))

(define-public (seperate-fields-discarding-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-rindex str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str (+ 1 w)) fields)
			     (make-shared-substring str 0 w))))
     (else (ret (cons str fields))))))

(define-public (seperate-fields-after-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-rindex str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str (+ 1 w)) fields)
			     (make-shared-substring str 0 (+ 1 w)))))
     (else (ret (cons str fields))))))

(define-public (seperate-fields-before-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-rindex str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str w) fields)
			     (make-shared-substring str 0 w))))
     (else (ret (cons str fields))))))


;;;;
;;; {String Prefix Predicates}
;;;
;;; Very simple:
;;;
:;; (define-public ((string-prefix-predicate pred?) prefix str)
;;;  (and (<= (length prefix) (length str))
;;;	  (pred? prefix (make-shared-substring str 0 (length prefix)))))
;;;
;;; (define-public string-prefix=? (string-prefix-predicate string=?))
;;;

(define-public ((string-prefix-predicate pred?) prefix str)
  (and (<= (length prefix) (length str))
       (pred? prefix (make-shared-substring str 0 (length prefix)))))

(define-public string-prefix=? (string-prefix-predicate string=?))


;;;;
;;; {Strippers}
;;;
;;; <stripper> = sans-<removable-part>
;;;
;;; <removable-part> = 	  surrounding-whitespace
;;;			| trailing-whitespace
;;;			| leading-whitespace
;;;			| final-newline
;;;

(define-public (sans-surrounding-whitespace s)
  (let ((st 0)
	(end (string-length s)))
    (while (and (< st (string-length s))
		(char-whitespace? (string-ref s st)))
	   (set! st (1+ st)))
    (while (and (< 0 end)
		(char-whitespace? (string-ref s (1- end))))
	   (set! end (1- end)))
    (if (< end st)
	""
	(make-shared-substring s st end))))

(define-public (sans-trailing-whitespace s)
  (let ((st 0)
	(end (string-length s)))
    (while (and (< 0 end)
		(char-whitespace? (string-ref s (1- end))))
	   (set! end (1- end)))
    (if (< end st)
	""
	(make-shared-substring s st end))))

(define-public (sans-leading-whitespace s)
  (let ((st 0)
	(end (string-length s)))
    (while (and (< st (string-length s))
		(char-whitespace? (string-ref s st)))
	   (set! st (1+ st)))
    (if (< end st)
	""
	(make-shared-substring s st end))))

(define-public (sans-final-newline str)
  (cond
   ((= 0 (string-length str))
    str)

   ((char=? #\nl (string-ref str (1- (string-length str))))
    (make-shared-substring str 0 (1- (string-length str))))

   (else str)))

;;;;
;;; {has-trailing-newline?}
;;;

(define-public (has-trailing-newline? str)
  (and (< 0 (string-length str))
       (char=? #\nl (string-ref str (1- (string-length str))))))





(define-public (with-regexp-parts regexp fields str return fail)
  (let ((parts (regexec regexp str fields)))
    (if (number? parts)
	(fail parts)
	(apply return parts))))



;;; {Load debug extension code if debug extensions present.}
;;;
;;; *fixme* This is a temporary solution.
;;;

(if (memq 'debug-extensions *features*)
    (define-module (guile) :use-module (ice-9 debug))
    (define-module (guile)))
