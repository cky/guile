;;; installed-scm-file

;;;; 	Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
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


;;; This file is the first thing loaded into Guile.  It adds many mundane
;;; definitions and a few that are interesting.
;;;
;;; The module system (hence the hierarchical namespace) are defined in this 
;;; file.
;;;


;;; {Features}
;;

(define (provide sym)
  (if (not (memq sym *features*))
      (set! *features* (cons sym *features*))))


;;; {R4RS compliance}

(primitive-load-path "ice-9/r4rs.scm")


;;; {Simple Debugging Tools}
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
      (display stuff)
      (newline)
      (car (last-pair stuff)))))


;;; {Trivial Functions}
;;;

(define (id x) x)
(define (1+ n) (+ n 1))
(define (-1+ n) (+ n -1))
(define 1- -1+)
(define return-it noop)
(define (and=> value procedure) (and value (procedure value)))
(define (make-hash-table k) (make-vector k '()))

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


;;; {Integer Math}
;;;

(define (ipow-by-squaring x k acc proc)
  (cond ((zero? k) acc)
	((= 1 k) (proc acc x))
	(else (ipow-by-squaring (proc x x)
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



;;; {Line and Delimited I/O}

;;; corresponds to SCM_LINE_INCREMENTORS in libguile.
(define scm-line-incrementors "\n")

(define (read-line! string . maybe-port)
  (let* ((port (if (pair? maybe-port)
		   (car maybe-port)
		   (current-input-port))))
    (let* ((rv (%read-delimited! scm-line-incrementors
				 string
				 #t
				 port))
	   (terminator (car rv))
	   (nchars (cdr rv)))
      (cond ((and (= nchars 0)
		  (eof-object? terminator))
	     terminator)
	    ((not terminator) #f)
	    (else nchars)))))

(define (read-delimited! delims buf . args)
  (let* ((num-args (length args))
	 (port (if (> num-args 0)
		   (car args)
		   (current-input-port)))
	 (handle-delim (if (> num-args 1)
			   (cadr args)
			   'trim))
	 (start (if (> num-args 2)
		    (caddr args)
		    0))
	 (end (if (> num-args 3)
		  (cadddr args)
		  (string-length buf))))
    (let* ((rv (%read-delimited! delims
				 buf
				 (not (eq? handle-delim 'peek))
				 port
				 start
				 end))
	   (terminator (car rv))
	   (nchars (cdr rv)))
      (cond ((or (not terminator)	; buffer filled
		 (eof-object? terminator))
	     (if (zero? nchars)
		 (if (eq? handle-delim 'split)
		     (cons terminator terminator)
		     terminator)
		 (if (eq? handle-delim 'split)
		     (cons nchars terminator)
		     nchars)))
	    (else
	     (case handle-delim
	       ((trim peek) nchars)
	       ((concat) (string-set! buf nchars terminator)
			 (+ nchars 1))
	       ((split) (cons nchars terminator))
	       (else (error "unexpected handle-delim value: " 
			    handle-delim))))))))
  
(define (read-delimited delims . args)
  (let* ((port (if (pair? args)
		   (let ((pt (car args)))
		     (set! args (cdr args))
		     pt)
		   (current-input-port)))
	 (handle-delim (if (pair? args)
			   (car args)
			   'trim)))
    (let loop ((substrings ())
	       (total-chars 0)
	       (buf-size 100))		; doubled each time through.
      (let* ((buf (make-string buf-size))
	     (rv (%read-delimited! delims
				   buf
				   (not (eq? handle-delim 'peek))
				   port))
	     (terminator (car rv))
	     (nchars (cdr rv))
	     (join-substrings
	      (lambda ()
		(apply string-append
		       (reverse
			(cons (if (and (eq? handle-delim 'concat)
				       (not (eof-object? terminator)))
				  (string terminator)
				  "")
			      (cons (make-shared-substring buf 0 nchars)
				    substrings))))))
	     (new-total (+ total-chars nchars)))
	(cond ((not terminator)
	       ;; buffer filled.
	       (loop (cons (substring buf 0 nchars) substrings)
		     new-total
		     (* buf-size 2)))
	      ((eof-object? terminator)
	       (if (zero? new-total)
		   (if (eq? handle-delim 'split)
		       (cons terminator terminator)
		       terminator)
		   (if (eq? handle-delim 'split)
		       (cons (join-substrings) terminator)
		       (join-substrings))))
	      (else
	       (case handle-delim
		   ((trim peek concat) (join-substrings))
		   ((split) (cons (join-substrings) terminator))
		   (else (error "unexpected handle-delim value: "
				handle-delim)))))))))
    
(define (read-line . args)
  (apply read-delimited scm-line-incrementors args))


;;; {Arrays}
;;;

(begin
  (define uniform-vector? array?)
  (define make-uniform-vector dimensions->uniform-array)
  ;      (define uniform-vector-ref array-ref)
  (define (uniform-vector-set! u i o)
    (uniform-array-set1! u o i))
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



;;; Printing structs

;; The printing of structures can be customized by setting the builtin
;; variable *struct-printer* to a procedure.  A second dispatching
;; step is implemented here to allow for struct-type specific printing
;; procedures.
;;
;; A particular type of structures is characterized by its vtable.  In
;; addition to some internal fields, such a vtable can contain
;; arbitrary user-defined fields.  We use the first of these fields to
;; hold the specific printing procedure.  To avoid breaking code that
;; already uses this first extra-field for some other purposes, we use
;; a unique tag to decide whether it really contains a structure
;; printer or not.
;;
;; XXX - Printing structures is probably fundamental enough that we
;; can simply hardcode the vtable slot convention and expect everyone
;; to obey it.
;;
;; A structure-type specific printer follows the same calling
;; convention as the builtin *struct-printer*.

;; A shorthand for one already hardcoded vtable convention

(define (struct-layout s)
  (struct-ref (struct-vtable s) 0))

;; This is our new convention for storing printing procedures

(define %struct-printer-tag (cons '%struct-printer-tag #f))

(define (struct-printer s)
  (let ((vtable (struct-vtable s)))
    (and (> (string-length (struct-layout vtable))
	    (* 2 struct-vtable-offset))
	 (let ((p (struct-ref vtable struct-vtable-offset)))
	   (and (pair? p)
	        (eq? (car p) %struct-printer-tag)
		(cdr p))))))

(define (make-struct-printer printer)
  (cons %struct-printer-tag printer))

;; Note: While the printer is extracted from a structure itself, it
;; has to be set in the vtable of the structure.

(define (set-struct-printer-in-vtable! vtable printer)
  (struct-set! vtable struct-vtable-offset (make-struct-printer printer)))

;; The dispatcher

(set! *struct-printer* (lambda (s p)
			 (let ((printer (struct-printer s)))
			   (and printer
				(printer s p)))))


;;; {Records}
;;;

;; Printing records: by default, records are printed as
;;
;;   #<type-name field1: val1 field2: val2 ...>
;;
;; You can change that by giving a custom printing function to
;; MAKE-RECORD-TYPE (after the list of field symbols).  This function
;; will be called like
;;
;;   (<printer> object port)
;;
;; It should print OBJECT to PORT.

;; 0: printer, 1: type-name, 2: fields
(define record-type-vtable 
  (make-vtable-vtable "prprpr" 0
		      (make-struct-printer
		       (lambda (s p)
			 (cond ((eq? s record-type-vtable)
				(display "#<record-type-vtable>" p))
			       (else
				(display "#<record-type " p)
				(display (record-type-name s) p)
				(display ">" p)))))))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

(define (make-record-type type-name fields . opt)
  (let ((printer-fn (and (pair? opt) (car opt))))
    (let ((struct (make-struct record-type-vtable 0
			       (make-struct-layout
				(apply symbol-append
				       (map (lambda (f) "pw") fields)))
			       (make-struct-printer
				(or printer-fn
				    (lambda (s p)
				      (display "#<" p)
				      (display type-name p)
				      (let loop ((fields fields)
						 (off 0))
					(cond
					 ((not (null? fields))
					  (display " " p)
					  (display (car fields) p)
					  (display ": " p)
					  (display (struct-ref s off) p)
					  (loop (cdr fields) (+ 1 off)))))
				      (display ">" p))))
			       type-name
			       (copy-tree fields))))
      struct)))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 struct-vtable-offset))
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 2 struct-vtable-offset))
      (error 'not-a-record-type obj)))

(define (record-constructor rtd . opt)
  (let ((field-names (if (pair? opt) (car opt) (record-type-fields rtd))))
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

(provide 'record)


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

(define (make-list n . init)
  (if (pair? init) (set! init (car init)))
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


;;; {Hooks}
(define (run-hooks hook)
  (for-each (lambda (thunk) (thunk)) hook))

(define add-hook!
  (procedure->macro
    (lambda (exp env)
      `(let ((thunk ,(caddr exp)))
	 (if (not (memq thunk ,(cadr exp)))
	     (set! ,(cadr exp)
		   (cons thunk ,(cadr exp))))))))


;;; {Files}
;;; !!!! these should be implemented using Tcl commands, not fports.
;;;

(define (feature? feature)
  (and (memq feature *features*) #t))

;; Using the vector returned by stat directly is probably not a good
;; idea (it could just as well be a record).  Hence some accessors.
(define (stat:dev f) (vector-ref f 0))
(define (stat:ino f) (vector-ref f 1))
(define (stat:mode f) (vector-ref f 2))
(define (stat:nlink f) (vector-ref f 3))
(define (stat:uid f) (vector-ref f 4))
(define (stat:gid f) (vector-ref f 5))
(define (stat:rdev f) (vector-ref f 6))
(define (stat:size f) (vector-ref f 7))
(define (stat:atime f) (vector-ref f 8))
(define (stat:mtime f) (vector-ref f 9))
(define (stat:ctime f) (vector-ref f 10))
(define (stat:blksize f) (vector-ref f 11))
(define (stat:blocks f) (vector-ref f 12))

;; derived from stat mode.
(define (stat:type f) (vector-ref f 13))
(define (stat:perms f) (vector-ref f 14))

(define file-exists?
  (if (feature? 'posix)
      (lambda (str)
	(access? str F_OK))
      (lambda (str)
	(let ((port (catch 'system-error (lambda () (open-file str OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

(define file-is-directory?
  (if (feature? 'i/o-extensions)
      (lambda (str)
	(eq? (stat:type (stat str)) 'directory))
      (lambda (str)
	(display str)
	(newline)
	(let ((port (catch 'system-error
			   (lambda () (open-file (string-append str "/.")
						 OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

(define (has-suffix? str suffix)
  (let ((sufl (string-length suffix))
	(sl (string-length str)))
    (and (> sl sufl)
	 (string=? (substring str (- sl sufl) sl) suffix))))


;;; {Error Handling}
;;;

(define (error . args)
  (save-stack)
  (if (null? args)
      (scm-error 'misc-error #f "?" #f #f)
      (let loop ((msg "%s")
		 (rest (cdr args)))
	(if (not (null? rest))
	    (loop (string-append msg " %S")
		  (cdr rest))
	    (scm-error 'misc-error #f msg args #f)))))

;; bad-throw is the hook that is called upon a throw to a an unhandled
;; key (unless the throw has four arguments, in which case
;; it's usually interpreted as an error throw.)
;; If the key has a default handler (a throw-handler-default property),
;; it is applied to the throw.
;;
(define (bad-throw key . args)
  (let ((default (symbol-property key 'throw-handler-default)))
    (or (and default (apply default key args))
	(apply error "unhandled-exception:" key args))))


;;; {Non-polymorphic versions of POSIX functions}

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
(define (getservbyname name proto) (getserv name proto))
(define (getservbyport port proto) (getserv port proto))
(define (endgrent) (setgr))
(define (endhostent) (sethost))
(define (endnetent) (setnet))
(define (endprotoent) (setproto))
(define (endpwent) (setpw))
(define (endservent) (setserv))
(define (getgrent) (getgr))
(define (gethostent) (gethost))
(define (getnetent) (getnet))
(define (getprotoent) (getproto))
(define (getpwent) (getpw))
(define (getservent) (getserv))
(define (reopen-file . args) (apply freopen args))
(define (setgrent) (setgr #f))
(define (sethostent) (sethost #t))
(define (setnetent) (setnet #t))
(define (setprotoent) (setproto #t))
(define (setpwent) (setpw #t))
(define (setservent) (setserv #t))

(define (passwd:name obj) (vector-ref obj 0))
(define (passwd:passwd obj) (vector-ref obj 1))
(define (passwd:uid obj) (vector-ref obj 2))
(define (passwd:gid obj) (vector-ref obj 3))
(define (passwd:gecos obj) (vector-ref obj 4))
(define (passwd:dir obj) (vector-ref obj 5))
(define (passwd:shell obj) (vector-ref obj 6))

(define (group:name obj) (vector-ref obj 0))
(define (group:passwd obj) (vector-ref obj 1))
(define (group:gid obj) (vector-ref obj 2))
(define (group:mem obj) (vector-ref obj 3))

(define (hostent:name obj) (vector-ref obj 0))
(define (hostent:aliases obj) (vector-ref obj 1))
(define (hostent:addrtype obj) (vector-ref obj 2))
(define (hostent:length obj) (vector-ref obj 3))
(define (hostent:addr-list obj) (vector-ref obj 4))

(define (netent:name obj) (vector-ref obj 0))
(define (netent:aliases obj) (vector-ref obj 1))
(define (netent:addrtype obj) (vector-ref obj 2))
(define (netent:net obj) (vector-ref obj 3))

(define (protoent:name obj) (vector-ref obj 0))
(define (protoent:aliases obj) (vector-ref obj 1))
(define (protoent:proto obj) (vector-ref obj 2))

(define (servent:name obj) (vector-ref obj 0))
(define (servent:aliases obj) (vector-ref obj 1))
(define (servent:port obj) (vector-ref obj 2))
(define (servent:proto obj) (vector-ref obj 3))

(define (sockaddr:fam obj) (vector-ref obj 0))
(define (sockaddr:path obj) (vector-ref obj 1))
(define (sockaddr:addr obj) (vector-ref obj 1))
(define (sockaddr:port obj) (vector-ref obj 2))

(define (utsname:sysname obj) (vector-ref obj 0))
(define (utsname:nodename obj) (vector-ref obj 1))
(define (utsname:release obj) (vector-ref obj 2))
(define (utsname:version obj) (vector-ref obj 3))
(define (utsname:machine obj) (vector-ref obj 4))

(define (tm:sec obj) (vector-ref obj 0))
(define (tm:min obj) (vector-ref obj 1))
(define (tm:hour obj) (vector-ref obj 2))
(define (tm:mday obj) (vector-ref obj 3))
(define (tm:mon obj) (vector-ref obj 4))
(define (tm:year obj) (vector-ref obj 5))
(define (tm:wday obj) (vector-ref obj 6))
(define (tm:yday obj) (vector-ref obj 7))
(define (tm:isdst obj) (vector-ref obj 8))
(define (tm:gmtoff obj) (vector-ref obj 9))
(define (tm:zone obj) (vector-ref obj 10))

(define (set-tm:sec obj val) (vector-set! obj 0 val))
(define (set-tm:min obj val) (vector-set! obj 1 val))
(define (set-tm:hour obj val) (vector-set! obj 2 val))
(define (set-tm:mday obj val) (vector-set! obj 3 val))
(define (set-tm:mon obj val) (vector-set! obj 4 val))
(define (set-tm:year obj val) (vector-set! obj 5 val))
(define (set-tm:wday obj val) (vector-set! obj 6 val))
(define (set-tm:yday obj val) (vector-set! obj 7 val))
(define (set-tm:isdst obj val) (vector-set! obj 8 val))
(define (set-tm:gmtoff obj val) (vector-set! obj 9 val))
(define (set-tm:zone obj val) (vector-set! obj 10 val))

(define (file-position . args) (apply ftell args))
(define (file-set-position . args) (apply fseek args))

(define (open-input-pipe command) (open-pipe command OPEN_READ))
(define (open-output-pipe command) (open-pipe command OPEN_WRITE))

(define (move->fdes fd/port fd)
  (cond ((integer? fd/port)
	 (primitive-dup2 fd/port fd)
	 (close fd/port)
	 fd)
	(else
	 (primitive-move->fdes fd/port fd)
	 (set-port-revealed! fd/port 1)
	 fd/port)))

(define (release-port-handle port)
  (let ((revealed (port-revealed port)))
    (if (> revealed 0)
	(set-port-revealed! port (- revealed 1)))))

(define (dup->port port/fd mode . maybe-fd)
  (let ((port (fdopen (if (pair? maybe-fd)
			  (primitive-dup2 port/fd (car maybe-fd))
			  (primitive-dup port/fd))
		      mode)))
    (if (pair? maybe-fd)
	(set-port-revealed! port 1))
    port))
  
(define (dup->inport port/fd . maybe-fd)
  (apply dup->port port/fd "r" maybe-fd))

(define (dup->outport port/fd . maybe-fd)
  (apply dup->port port/fd "w" maybe-fd))

(define (dup->fdes port/fd . maybe-fd)
  (if (pair? maybe-fd)
      (primitive-dup2 port/fd (car maybe-fd))
      (primitive-dup port/fd)))

(define (dup port/fd . maybe-fd)
  (if (integer? port/fd)
      (apply dup->fdes port/fd maybe-fd)
      (apply dup->port port/fd (port-mode port/fd) maybe-fd)))

(define (duplicate-port port modes)
  (dup->port port modes))

(define (fdes->inport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
	   (let ((result (fdopen fdes "r")))
	     (set-port-revealed! result 1)
	     result))
	  ((input-port? (car rest-ports))
	   (set-port-revealed! (car rest-ports)
			       (+ (port-revealed (car rest-ports)) 1))
	   (car rest-ports))
	  (else
	   (loop (cdr rest-ports))))))

(define (fdes->outport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
	   (let ((result (fdopen fdes "w")))
	     (set-port-revealed! result 1)
	     result))
	  ((output-port? (car rest-ports))
	   (set-port-revealed! (car rest-ports)
			       (+ (port-revealed (car rest-ports)) 1))
	   (car rest-ports))
	  (else
	   (loop (cdr rest-ports))))))

(define (port->fdes port)
  (set-port-revealed! port (+ (port-revealed port) 1))
  (fileno port))

(define (setenv name value)
  (if value
      (putenv (string-append name "=" value))
      (putenv name)))


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


;;; {Help for scm_shell}
;;; The argument-processing code used by Guile-based shells generates
;;; Scheme code based on the argument list.  This page contains help
;;; functions for the code it generates.

(define (command-line) (program-arguments))

;; This is mostly for the internal use of the code generated by
;; scm_compile_shell_switches.
(define (load-user-init)
  (define (has-init? dir)
    (let ((path (in-vicinity dir ".guile")))
      (catch 'system-error 
	     (lambda ()
	       (let ((stats (stat path)))
		 (if (not (eq? (stat:type stats) 'directory))
		     path)))
	     (lambda dummy #f))))
  (let ((path (or (has-init? (or (getenv "HOME") "/"))
                  (has-init? (passwd:dir (getpw (getuid)))))))
    (if path (primitive-load path))))


;;; {Loading by paths}

;;; Load a Scheme source file named NAME, searching for it in the
;;; directories listed in %load-path, and applying each of the file
;;; name extensions listed in %load-extensions.
(define (load-from-path name)
  (start-stack 'load-stack
	       (primitive-load-path name)))



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

(define (log10 arg)
  (/ (log arg) (log 10)))



;;; {Reader Extensions}
;;;

;;; Reader code for various "#c" forms.
;;;

;;; Parse the portion of a #/ list that comes after the first slash.
(define (read-path-list-notation slash port)
  (letrec 
      
      ;; Is C a delimiter?
      ((delimiter? (lambda (c) (or (eof-object? c)
				   (char-whitespace? c)
				   (string-index "()\";" c))))

       ;; Read and return one component of a path list.
       (read-component
	(lambda ()
	  (let loop ((reversed-chars '()))
	    (let ((c (peek-char port)))
	      (if (or (delimiter? c)
		      (char=? c #\/))
		  (string->symbol (list->string (reverse reversed-chars)))
		  (loop (cons (read-char port) reversed-chars))))))))

    ;; Read and return a path list.
    (let loop ((reversed-path (list (read-component))))
      (let ((c (peek-char port)))
	(if (and (char? c) (char=? c #\/))
	    (begin
	      (read-char port)
	      (loop (cons (read-component) reversed-path)))
	    (reverse reversed-path))))))

(read-hash-extend #\' (lambda (c port)
			(read port)))
(read-hash-extend #\. (lambda (c port)
			(eval (read port))))

(if (feature? 'array)
    (begin
      (let ((make-array-proc (lambda (template)
			       (lambda (c port)
				 (read:uniform-vector template port)))))
	(for-each (lambda (char template)
		    (read-hash-extend char
				      (make-array-proc template)))
		  '(#\b #\a #\u #\e #\s #\i #\c)
		  '(#t  #\a 1   -1  1.0 1/3 0+i)))
      (let ((array-proc (lambda (c port)
			  (read:array c port))))
	(for-each (lambda (char) (read-hash-extend char array-proc))
		  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))))

;; pushed to the beginning of the alist since it's used more than the
;; others at present.
(read-hash-extend #\/ read-path-list-notation)

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
	(list->uniform-array rank prot (read port))
	(error "read:array list not found"))))

(define (read:uniform-vector proto port)
  (if (eq? #\( (peek-char port))
      (list->uniform-array 1 proto (read port))
      (error "read:uniform-vector list not found")))


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
		  

	   
(define (transform-usage-lambda cases)
  (let* ((raw-usage (delq! 'else (map car cases)))
	 (usage-sans-specials (map (lambda (x)
				    (or (and (not (list? x)) x)
					(and (symbol? (car x)) #t)
					(and (boolean? (car x)) #t)
					x))
				  raw-usage))
	 (usage-desc (delq! #t usage-sans-specials))
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


;;; {Printing Modules}
;; This is how modules are printed.  You can re-define it.
;; (Redefining is actually more complicated than simply redefining
;; %print-module because that would only change the binding and not
;; the value stored in the vtable that determines how record are
;; printed. Sigh.)

(define (%print-module mod port)  ; unused args: depth length style table)
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
;; is a (CLOSURE module symbol) which, as a last resort, can provide
;; bindings that would otherwise not be found locally in the module.
;;
(define module-type
  (make-record-type 'module '(obarray uses binder eval-closure name kind)
		    %print-module))

;; make-module &opt size uses binder
;;
;; Create a new module, perhaps with a particular size of obarray,
;; initial uses list, or binding procedure.
;;
(define make-module
    (lambda args

      (define (parse-arg index default)
	(if (> (length args) index)
	    (list-ref args index)
	    default))

      (if (> (length args) 3)
	  (error "Too many args to make-module." args))

      (let ((size (parse-arg 0 1021))
	    (uses (parse-arg 1 '()))
	    (binder (parse-arg 2 #f)))

	(if (not (integer? size))
	    (error "Illegal size to make-module." size))
	(if (not (and (list? uses)
		      (and-map module? uses)))
	    (error "Incorrect use list." uses))
	(if (and binder (not (procedure? binder)))
	    (error
	     "Lazy-binder expected to be a procedure or #f." binder))

	(let ((module (module-constructor (make-vector size '())
					  uses binder #f #f #f)))

	  ;; We can't pass this as an argument to module-constructor,
	  ;; because we need it to close over a pointer to the module
	  ;; itself.
	  (set-module-eval-closure! module
				  (lambda (symbol define?)
				    (if define?
					(module-make-local-var! module symbol)
					(module-variable module symbol))))

	  module))))

(define module-constructor (record-constructor module-type))
(define module-obarray  (record-accessor module-type 'obarray))
(define set-module-obarray! (record-modifier module-type 'obarray))
(define module-uses  (record-accessor module-type 'uses))
(define set-module-uses! (record-modifier module-type 'uses))
(define module-binder (record-accessor module-type 'binder))
(define set-module-binder! (record-modifier module-type 'binder))
(define module-eval-closure (record-accessor module-type 'eval-closure))
(define set-module-eval-closure! (record-modifier module-type 'eval-closure))
(define module-name (record-accessor module-type 'name))
(define set-module-name! (record-modifier module-type 'name))
(define module-kind (record-accessor module-type 'kind))
(define set-module-kind! (record-modifier module-type 'kind))
(define module? (record-predicate module-type))


(define (eval-in-module exp module)
  (eval2 exp (module-eval-closure module)))


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
;  (caddr
;   (list m v
	 (let ((b (module-obarray-ref (module-obarray m) v)))
	   (or (and (variable? b) b)
	       (and (module-binder m)
		    ((module-binder m) m v #f)))))
;))

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

;; A root module uses the symhash table (the system's privileged 
;; obarray).  Being inside a root module is like using SCM without
;; any module system.
;;


(define (root-module-closure m s define?)
  (let ((bi (and (symbol-interned? #f s)
		 (builtin-variable s))))
    (and bi
	 (or define? (variable-bound? bi))
	 (begin
	   (module-add! m s bi)
	   bi))))

(define (make-root-module)
  (make-module 1019 '() root-module-closure))


;; make-scm-module 

;; An scm module is a module into which the lazy binder copies
;; variable bindings from the system symhash table.  The mapping is
;; one way only; newly introduced bindings in an scm module are not
;; copied back into the system symhash table (and can be used to override
;; bindings from the symhash table).
;;

(define (make-scm-module)
  (make-module 1019 '()
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
      (set! *top-level-lookup-closure* (module-eval-closure the-module))
      (set! *top-level-lookup-closure* #f)))


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

(define basic-load load)

(define (load-module . args)
  (save-module-excursion (lambda () (apply basic-load args))))



;;; {MODULE-REF -- exported}
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

;; MODULE-DEFINED? -- exported
;;
;; Return #t iff NAME is defined in MODULE (or in a module that MODULE
;; uses)
;;
(define (module-defined? module name)
  (let ((variable (module-variable module name)))
    (and variable (variable-bound? variable))))

;; MODULE-USE! module interface
;;
;; Add INTERFACE to the list of interfaces used by MODULE.
;; 
(define (module-use! module interface)
  (set-module-uses! module
		    (cons interface (delq! interface (module-uses module)))))


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
;;;		(nested-ref some-root-module '(foo bar baz))
;;;		=> <value of a variable named baz in the module bound to bar in 
;;;		    the module bound to foo in some-root-module>
;;;
;;;
;;; There are:
;;;
;;;	;; a-root is a module
;;;	;; name is a list of symbols
;;;
;;;	nested-ref a-root name
;;;	nested-set! a-root name val
;;;	nested-define! a-root name val
;;;	nested-remove! a-root name
;;;
;;;
;;; (current-module) is a natural choice for a-root so for convenience there are
;;; also:
;;;
;;;	local-ref name		==	nested-ref (current-module) name
;;;	local-set! name val	==	nested-set! (current-module) name val
;;;	local-define! name val	==	nested-define! (current-module) name val
;;;	local-remove! name	==	nested-remove! (current-module) name
;;;


(define (nested-ref root names)
  (let loop ((cur root)
	     (elts names))
    (cond
     ((null? elts)		cur)
     ((not (module? cur))	#f)
     (else (loop (module-ref cur (car elts) #f) (cdr elts))))))

(define (nested-set! root names val)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-set! cur (car elts) val)
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (nested-define! root names val)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-define! cur (car elts) val)
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (nested-remove! root names)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-remove! cur (car elts))
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (local-ref names) (nested-ref (current-module) names))
(define (local-set! names val) (nested-set! (current-module) names val))
(define (local-define names val) (nested-define! (current-module) names val))
(define (local-remove names) (nested-remove! (current-module) names))



;;; {The (app) module}
;;;
;;; The root of conventionally named objects not directly in the top level.
;;;
;;; (app modules)
;;; (app modules guile)
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
(local-define '(app modules) (make-module 31))
(local-define '(app modules guile) the-root-module)

;; (define-special-value '(app modules new-ws) (lambda () (make-scm-module)))

(define (resolve-module name . maybe-autoload)
  (let ((full-name (append '(app modules) name)))
    (let ((already (local-ref full-name)))
    (or already
	(begin
	  (if (or (null? maybe-autoload) (car maybe-autoload))
	      (or (try-module-autoload name)
		  (try-module-dynamic-link name)))
	  (make-modules-in (current-module) full-name))))))
	    
(define (beautify-user-module! module)
  (if (not (module-public-interface module))
      (let ((interface (make-module 31)))
	(set-module-name! interface (module-name module))
	(set-module-kind! interface 'interface)
	(set-module-public-interface! module interface)))
  (if (and (not (memq the-scm-module (module-uses module)))
	   (not (eq? module the-root-module)))
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
	  (module (resolve-module module-id #f))
	  (kws (cdr args)))
    (beautify-user-module! module)
    (let loop ((kws kws)
	       (reversed-interfaces '()))
      (if (null? kws)
	  (for-each (lambda (interface)
		      (module-use! module interface))
		    reversed-interfaces)
	  (case (cond ((keyword? (car kws))
		       (keyword->symbol (car kws)))
		      ((and (symbol? (car kws))
			    (eq? (string-ref (car kws) 0) #\:))
		       (string->symbol (substring (car kws) 1)))
		      (else #f))
	    ((use-module)
	     (if (not (pair? (cdr kws)))
		 (error "unrecognized defmodule argument" kws))
	     (let* ((used-name (cadr kws))
		    (used-module (resolve-module used-name)))
	       (if (not (module-ref used-module '%module-public-interface #f))
		   (begin
		     ((if %autoloader-developer-mode warn error)
		      "no code for module" (module-name used-module))
		     (beautify-user-module! used-module)))
	       (let ((interface (module-public-interface used-module)))
		 (if (not interface)
		     (error "missing interface for use-module" used-module))
		 (loop (cddr kws) (cons interface reversed-interfaces)))))
	    (else	
	     (error "unrecognized defmodule argument" kws)))))
    module))

;;; {Autoloading modules}

(define autoloads-in-progress '())

(define (try-module-autoload module-name)

  (define (sfx name) (string-append name (scheme-file-suffix)))
  (let* ((reverse-name (reverse module-name))
	 (name (car reverse-name))
	 (dir-hint-module-name (reverse (cdr reverse-name)))
	 (dir-hint (apply symbol-append (map (lambda (elt) (symbol-append elt "/")) dir-hint-module-name))))
    (resolve-module dir-hint-module-name #f)
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
					 (and (file-exists? full)
					      (not (file-is-directory? full))
					      (begin
						(save-module-excursion
						 (lambda ()
						   (load (string-append
							  d "/" f))))
						#t))))
				     trys)
			     (begin
			       (set! didit #t)
			       #t)))
		      (loop (cdr dirs))))))
	    (lambda () (set-autoloaded! dir-hint name didit)))
	   didit))))

;;; Dynamic linking of modules

;; Initializing a module that is written in C is a two step process.
;; First the module's `module init' function is called.  This function
;; is expected to call `scm_register_module_xxx' to register the `real
;; init' function.  Later, when the module is referenced for the first
;; time, this real init function is called in the right context.  See
;; gtcltk-lib/gtcltk-module.c for an example.
;;
;; The code for the module can be in a regular shared library (so that
;; the `module init' function will be called when libguile is
;; initialized).  Or it can be dynamically linked.
;;
;; You can safely call `scm_register_module_xxx' before libguile
;; itself is initialized.  You could call it from an C++ constructor
;; of a static object, for example.
;;
;; To make your Guile extension into a dynamic linkable module, follow
;; these easy steps:
;;
;; - Find a name for your module, like (ice-9 gtcltk)
;; - Write a function with a name like
;;
;;     scm_init_ice_9_gtcltk_module
;;
;;   This is your `module init' function.  It should call
;;   
;;     scm_register_module_xxx ("ice-9 gtcltk", scm_init_gtcltk);
;;   
;;   "ice-9 gtcltk" is the C version of the module name. Slashes are
;;   replaced by spaces, the rest is untouched. `scm_init_gtcltk' is
;;   the real init function that executes the usual initializations
;;   like making new smobs, etc.
;;
;; - Make a shared library with your code and a name like
;;
;;     ice-9/libgtcltk.so
;;
;;   and put it somewhere in %load-path.
;;
;; - Then you can simply write `:use-module (ice-9 gtcltk)' and it
;;   will be linked automatically.
;;
;; This is all very experimental.

(define (split-c-module-name str)
  (let loop ((rev '())
	     (start 0)
	     (pos 0)
	     (end (string-length str)))
    (cond
     ((= pos end)
      (reverse (cons (string->symbol (substring str start pos)) rev)))
     ((eq? (string-ref str pos) #\space)
      (loop (cons (string->symbol (substring str start pos)) rev)
	    (+ pos 1)
	    (+ pos 1)
	    end))
     (else
      (loop rev start (+ pos 1) end)))))

(define (convert-c-registered-modules dynobj)
  (let ((res (map (lambda (c)
		    (list (split-c-module-name (car c)) (cdr c) dynobj))
		  (c-registered-modules))))
    (c-clear-registered-modules)
    res))

(define registered-modules (convert-c-registered-modules #f))
    
(define (init-dynamic-module modname)
  (or-map (lambda (modinfo)
	    (if (equal? (car modinfo) modname)
		(let ((mod (resolve-module modname #f)))
		  (save-module-excursion
		   (lambda ()
		     (set-current-module mod)
		     (dynamic-call (cadr modinfo) (caddr modinfo))
		     (set-module-public-interface! mod mod)))
		  (set! registered-modules (delq! modinfo registered-modules))
		  #t)
		#f))
	  registered-modules))

(define (dynamic-maybe-call name dynobj)
  (catch #t				; could use false-if-exception here
	 (lambda ()
	   (dynamic-call name dynobj))
	 (lambda args
	   #f)))

(define (dynamic-maybe-link filename)
  (catch #t				; could use false-if-exception here
	 (lambda ()
	   (dynamic-link filename))
	 (lambda args
	   #f)))

(define (find-and-link-dynamic-module module-name)
  (define (make-init-name mod-name)
    (string-append 'scm_init
		   (list->string (map (lambda (c)
					(if (or (char-alphabetic? c)
						(char-numeric? c))
					    c
					    #\_))
				      (string->list mod-name)))
		   '_module))
  (let ((libname
	 (let loop ((dirs "")
		    (syms module-name))
	   (cond
	    ((null? (cdr syms))
	     (string-append dirs "lib" (car syms) ".so"))
	    (else
	     (loop (string-append dirs (car syms) "/") (cdr syms))))))
	(init (make-init-name (apply string-append
				     (map (lambda (s)
					    (string-append "_" s))
					  module-name)))))
    ;; (pk 'libname libname 'init init)
    (or-map
     (lambda (dir)
       (let ((full (in-vicinity dir libname)))
	 ;; (pk 'trying full)
	 (if (file-exists? full)
	     (begin
	       (link-dynamic-module full init)
	       #t)
	     #f)))
     %load-path)))

(define (link-dynamic-module filename initname)
  (let ((dynobj (dynamic-link filename)))
    (dynamic-call initname dynobj)
    (set! registered-modules 
	  (append! (convert-c-registered-modules dynobj)
		   registered-modules))))
	    
(define (try-module-dynamic-link module-name)
  (or (init-dynamic-module module-name)
      (and (find-and-link-dynamic-module module-name)
	   (init-dynamic-module module-name))))



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


;; XXX - should the definition of the car really be looked up in the
;; current module?

(define (macroexpand-1 e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (local-ref (list a)))))
		(if (defmacro? val)
		    (apply (defmacro-transformer val) (cdr e))
		    e)))
   (#t e)))

(define (macroexpand e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (local-ref (list a)))))
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
  (let loop ((source (read (current-input-port))))
    (print (evaler source))
    (loop (read (current-input-port)))))

;; A provisional repl that acts like the SCM repl:
;;
(define scm-repl-silent #f)
(define (assert-repl-silence v) (set! scm-repl-silent v))

(define *unspecified* (if #f #f))
(define (unspecified? v) (eq? v *unspecified*))

(define scm-repl-print-unspecified #f)
(define (assert-repl-print-unspecified v) (set! scm-repl-print-unspecified v))

(define scm-repl-verbose #f)
(define (assert-repl-verbosity v) (set! scm-repl-verbose v))

(define scm-repl-prompt "guile> ")

(define (set-repl-prompt! v) (set! scm-repl-prompt v))

(define (default-lazy-handler key . args)
  (save-stack lazy-handler-dispatch)
  (apply throw key args))

(define apply-frame-handler default-lazy-handler)
(define exit-frame-handler default-lazy-handler)

(define (lazy-handler-dispatch key . args)
  (case key
    ((apply-frame)
     (apply apply-frame-handler key args))
    ((exit-frame)
     (apply exit-frame-handler key args))
    (else
     (apply default-lazy-handler key args))))

(define abort-hook '())

(define (error-catching-loop thunk)
  (let ((status #f))
    (define (loop first)
      (let ((next 
	     (catch #t

		    (lambda ()
		      (lazy-catch #t
				  (lambda ()
				    (dynamic-wind
				     (lambda () (unmask-signals))
				     (lambda ()
				       (first)
				       
				       ;; This line is needed because mark
				       ;; doesn't do closures quite right.
				       ;; Unreferenced locals should be
				       ;; collected.
				       ;;
				       (set! first #f)
				       (let loop ((v (thunk)))
					 (loop (thunk)))
				       #f)
				     (lambda () (mask-signals))))

				  lazy-handler-dispatch))
		    
		    (lambda (key . args)
		      (case key
			((quit)
			 (force-output)
			 (set! status args)
			 #f)

			((switch-repl)
			 (apply throw 'switch-repl args))

			((abort)
			 ;; This is one of the closures that require
			 ;; (set! first #f) above
			 ;;
			 (lambda ()
			   (run-hooks abort-hook)
			   (force-output)
			   (display "ABORT: "  (current-error-port))
			   (write args (current-error-port))
			   (newline (current-error-port))
			   (if (and (not has-shown-debugger-hint?)
				    (not (memq 'backtrace
					       (debug-options-interface)))
				    (stack? the-last-stack))
			       (begin
				 (newline (current-error-port))
				 (display
				  "Type \"(backtrace)\" to get more information.\n"
				  (current-error-port))
				 (set! has-shown-debugger-hint? #t)))
			   (set! stack-saved? #f)))

			(else
			 ;; This is the other cons-leak closure...
			 (lambda ()
			   (cond ((= (length args) 4)
				  (apply handle-system-error key args))
				 (else
				  (apply bad-throw key args))))))))))
	(if next (loop next) status)))
    (loop (lambda () #t))))

;;(define the-last-stack #f) Defined by scm_init_backtrace ()
(define stack-saved? #f)

(define (save-stack . narrowing)
  (cond (stack-saved?)
	((not (memq 'debug (debug-options-interface)))
	 (set! the-last-stack #f)
	 (set! stack-saved? #t))
	(else
	 (set! the-last-stack
	       (case (stack-id #t)
		 ((repl-stack)
		  (apply make-stack #t save-stack eval narrowing))
		 ((load-stack)
		  (apply make-stack #t save-stack gsubr-apply narrowing))
		 ((tk-stack)
		  (apply make-stack #t save-stack tk-stack-mark narrowing))
		 ((#t)
		  (apply make-stack #t save-stack 0 1 narrowing))
		 (else (let ((id (stack-id #t)))
			 (and (procedure? id)
			      (apply make-stack #t save-stack id narrowing))))))
	 (set! stack-saved? #t))))

(define before-error-hook '())
(define after-error-hook '())
(define before-backtrace-hook '())
(define after-backtrace-hook '())

(define has-shown-debugger-hint? #f)

(define (handle-system-error key . args)
  (let ((cep (current-error-port)))
    (cond ((not (stack? the-last-stack)))
	  ((memq 'backtrace (debug-options-interface))
	   (run-hooks before-backtrace-hook)
	   (newline cep)
	   (display-backtrace the-last-stack cep)
	   (newline cep)
	   (run-hooks after-backtrace-hook)))
    (run-hooks before-error-hook)
    (apply display-error the-last-stack cep args)
    (run-hooks after-error-hook)
    (force-output cep)
    (throw 'abort key)))

(define (quit . args)
  (apply throw 'quit args))

(define exit quit)

;;(define has-shown-backtrace-hint? #f) Defined by scm_init_backtrace ()

;; Replaced by C code:
;;(define (backtrace)
;;  (if the-last-stack
;;      (begin
;;	(newline)
;;	(display-backtrace the-last-stack (current-output-port))
;;	(newline)
;;	(if (and (not has-shown-backtrace-hint?)
;;		 (not (memq 'backtrace (debug-options-interface))))
;;	    (begin
;;	      (display
;;"Type \"(debug-enable 'backtrace)\" if you would like a backtrace
;;automatically if an error occurs in the future.\n")
;;	      (set! has-shown-backtrace-hint? #t))))
;;      (display "No backtrace available.\n")))

(define (error-catching-repl r e p)
  (error-catching-loop (lambda () (p (e (r))))))

(define (gc-run-time)
  (cdr (assq 'gc-time-taken (gc-stats))))

(define before-read-hook '())
(define after-read-hook '())

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

	   (consume-trailing-whitespace
	    (lambda ()
	      (let ((ch (peek-char)))
		(cond
		 ((eof-object? ch))
		 ((or (char=? ch #\space) (char=? ch #\tab))
		  (read-char)
		  (consume-trailing-whitespace))
		 ((char=? ch #\newline)
		  (read-char))))))
	   (-read (lambda ()
		    (if scm-repl-prompt
			(begin
			  (display (cond ((string? scm-repl-prompt)
					  scm-repl-prompt)
					 ((thunk? scm-repl-prompt)
					  (scm-repl-prompt))
					 (else "> ")))
			  (force-output)
			  (repl-report-reset)))
		    (run-hooks before-read-hook)
		    (let ((val (read (current-input-port))))
		      ;; As described in R4RS, the READ procedure updates the
		      ;; port to point to the first characetr past the end of
		      ;; the external representation of the object.  This
		      ;; means that it doesn't consume the newline typically
		      ;; found after an expression.  This means that, when
		      ;; debugging Guile with GDB, GDB gets the newline, which
		      ;; it often interprets as a "continue" command, making
		      ;; breakpoints kind of useless.  So, consume any
		      ;; trailing newline here, as well as any whitespace
		      ;; before it.
		      (consume-trailing-whitespace)
		      (run-hooks after-read-hook)
		      (if (eof-object? val)
			  (begin
			    (repl-report-start-timing)
			    (if scm-repl-verbose
				(begin
				  (newline)
				  (display ";;; EOF -- quitting")
				  (newline)))
			    (quit 0)))
		      val)))

	   (-eval (lambda (sourc)
		    (repl-report-start-timing)
		    (start-stack 'repl-stack (eval sourc))))

	   (-print (lambda (result)
		     (if (not scm-repl-silent)
			 (begin
			   (if (or scm-repl-print-unspecified
				   (not (unspecified? result)))
			       (begin
				 (write result)
				 (newline)))
			   (if scm-repl-verbose
			       (repl-report))
			   (force-output)))))

	   (-quit (lambda (args)
		    (if scm-repl-verbose
			(begin
			  (display ";;; QUIT executed, repl exitting")
			  (newline)
			  (repl-report)))
		    args))

	   (-abort (lambda ()
		     (if scm-repl-verbose
			 (begin
			   (display ";;; ABORT executed.")
			   (newline)
			   (repl-report)))
		     (repl -read -eval -print))))

    (let ((status (error-catching-repl -read
				       -eval
				       -print)))
      (-quit status))))
  


;;; {IOTA functions: generating lists of numbers}

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


;;; {with-fluids}

;; with-fluids is a convenience wrapper for the builtin procedure
;; `with-fluids*'.  The syntax is just like `let':
;;
;;  (with-fluids ((fluid val)
;;                ...)
;;     body)

(defmacro with-fluids (bindings . body)
  `(with-fluids* (list ,@(map car bindings)) (list ,@(map cadr bindings))
		 (lambda () ,@body)))



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

;; the guts of the use-modules macro.  add the interfaces of the named
;; modules to the use-list of the current module, in order
(define (process-use-modules module-names)
  (for-each (lambda (module-name)
	      (let ((mod-iface (resolve-interface module-name)))
		(or mod-iface
		    (error "no such module" module-name))
		(module-use! (current-module) mod-iface)))
	    (reverse module-names)))

(defmacro use-modules modules
  `(process-use-modules ',modules))

(define define-private define)

(defmacro define-public args
  (define (syntax)
    (error "bad syntax" (list 'define-public args)))
  (define (defined-name n)
    (cond
     ((symbol? n) n)
     ((pair? n) (defined-name (car n)))
     (else (syntax))))
  (cond
   ((null? args) (syntax))

   (#t (let ((name (defined-name (car args))))
	 `(begin
	    (let ((public-i (module-public-interface (current-module))))
	      ;; Make sure there is a local variable:
	      ;;
	      (module-define! (current-module)
			      ',name
			      (module-ref (current-module) ',name #f))
			       
	      ;; Make sure that local is exported:
	      ;;
	      (module-add! public-i ',name
			   (module-variable (current-module) ',name)))
			       
	    ;; Now (re)define the var normally.  Bernard URBAN
	    ;; suggests we use eval here to accomodate Hobbit; it lets
	    ;; the interpreter handle the define-private form, which
	    ;; Hobbit can't digest.
	    (eval '(define-private ,@ args)))))))



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




(define load load-module)
;(define (load . args)
;  (start-stack 'load-stack (apply load-module args)))



;;; {I/O functions for Tcl channels (disabled)}

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

;; this is just (scm-style-repl) with a wrapper to install and remove 
;; signal handlers.
(define (top-repl) 
  (let ((old-handlers #f)
	(signals `((,SIGINT . "User interrupt")
		   (,SIGFPE . "Arithmetic error")
		   (,SIGBUS . "Bad memory access (bus error)")
		   (,SIGSEGV . "Bad memory access (Segmentation violation)"))))

    (dynamic-wind

     ;; call at entry
     (lambda ()
       (let ((make-handler (lambda (msg)
			     (lambda (sig)
			       (scm-error 'signal
					  #f
					  msg
					  #f
					  (list sig))))))
	 (set! old-handlers
	       (map (lambda (sig-msg)
		      (sigaction (car sig-msg)
				 (make-handler (cdr sig-msg))))
		    signals))))

     ;; the protected thunk.
     (lambda ()
       (scm-style-repl))

     ;; call at exit.
     (lambda ()
       (map (lambda (sig-msg old-handler)
	      (if (not (car old-handler))
		  ;; restore original C handler.
		  (sigaction (car sig-msg) #f)
		  ;; restore Scheme handler, SIG_IGN or SIG_DFL.
		  (sigaction (car sig-msg)
			     (car old-handler)
			     (cdr old-handler))))
			 signals old-handlers)))))

(defmacro false-if-exception (expr)
  `(catch #t (lambda () ,expr)
	  (lambda args #f)))


;;; {Calling Conventions}
(define-module (ice-9 calling))

;;;;
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


;;; with-excursion-getter-and-setter <vars> proc
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




;;; {Implementation of COMMON LISP list functions for Scheme}

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
		(error "negative argument to butlast" n)
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


;;; {Functions for browsing modules}

(define-module (ice-9 ls)
  :use-module (ice-9 common-list))

;;;;
;;;	local-definitions-in root name
;;;		Returns a list of names defined locally in the named
;;;		subdirectory of root.
;;;	definitions-in root name
;;;		Returns a list of all names defined in the named
;;;		subdirectory of root.  The list includes alll locally
;;;		defined names as well as all names inherited from a
;;;		member of a use-list.
;;;
;;; A convenient interface for examining the nature of things:
;;;
;;;	ls . various-names
;;;
;;;		With just one argument, interpret that argument as the
;;;		name of a subdirectory of the current module and
;;;		return a list of names defined there.
;;;
;;;		With more than one argument, still compute
;;;		subdirectory lists, but return a list:
;;;			((<subdir-name> . <names-defined-there>)
;;;			 (<subdir-name> . <names-defined-there>)
;;;			 ...)
;;;

(define-public (local-definitions-in root names)
  (let ((m (nested-ref root names))
	(answer '()))
    (if (not (module? m))
	(set! answer m)
	(module-for-each (lambda (k v) (set! answer (cons k answer))) m))
    answer))

(define-public (definitions-in root names)
  (let ((m (nested-ref root names)))
    (if (not (module? m))
	m
	(reduce union
		(cons (local-definitions-in m  '())
		      (map (lambda (m2) (definitions-in m2 '()))
			   (module-uses m)))))))

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

(define-public (recursive-local-define name value)
  (let ((parent (reverse! (cdr (reverse name)))))
    (and parent (make-modules-in (current-module) parent))
    (local-define name value)))

;;; {Queues}

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
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
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

;;; q-rear q
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




;;; {The runq data structure}

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
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

;;;;
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


;;; {String Fun}

(define-module (ice-9 string-fun))

;;;;
;;;
;;; Various string funcitons, particularly those that take
;;; advantage of the "shared substring" capability.
;;;

;;; {String Fun: Dividing Strings Into Fields}
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

(define-public (separate-fields-discarding-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-rindex str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str (+ 1 w)) fields)
			     (make-shared-substring str 0 w))))
     (else (ret (cons str fields))))))

(define-public (separate-fields-after-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-rindex str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str (+ 1 w)) fields)
			     (make-shared-substring str 0 (+ 1 w)))))
     (else (ret (cons str fields))))))

(define-public (separate-fields-before-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-rindex str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str w) fields)
			     (make-shared-substring str 0 w))))
     (else (ret (cons str fields))))))


;;; {String Fun: String Prefix Predicates}
;;;
;;; Very simple:
;;;
;;; (define-public ((string-prefix-predicate pred?) prefix str)
;;;  (and (<= (length prefix) (length str))
;;;	  (pred? prefix (make-shared-substring str 0 (length prefix)))))
;;;
;;; (define-public string-prefix=? (string-prefix-predicate string=?))
;;;

(define-public ((string-prefix-predicate pred?) prefix str)
  (and (<= (length prefix) (length str))
       (pred? prefix (make-shared-substring str 0 (length prefix)))))

(define-public string-prefix=? (string-prefix-predicate string=?))


;;; {String Fun: Strippers}
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

;;; {String Fun: has-trailing-newline?}
;;;

(define-public (has-trailing-newline? str)
  (and (< 0 (string-length str))
       (char=? #\nl (string-ref str (1- (string-length str))))))



;;; {String Fun: with-regexp-parts}

;;; This relies on the older, hairier regexp interface, which we don't
;;; particularly want to implement, and it's not used anywhere, so
;;; we're just going to drop it for now.
;;; (define-public (with-regexp-parts regexp fields str return fail)
;;;   (let ((parts (regexec regexp str fields)))
;;;     (if (number? parts)
;;;         (fail parts)
;;;         (apply return parts))))


;;; {Load debug extension code if debug extensions present.}
;;;
;;; *fixme* This is a temporary solution.
;;;

(if (memq 'debug-extensions *features*)
    (define-module (guile) :use-module (ice-9 debug)))


;;; {Load session support if present.}
;;;
;;; *fixme* This is a temporary solution.
;;;

(if (%search-load-path "ice-9/session.scm")
    (define-module (guile) :use-module (ice-9 session)))


;;; {Load thread code if threads are present.}
;;;
;;; *fixme* This is a temporary solution.
;;;

(if (memq 'threads *features*)
    (define-module (guile) :use-module (ice-9 threads)))


;;; {Load emacs interface support if emacs option is given.}
;;;
;;; *fixme* This is a temporary solution.
;;;

(if (and (module-defined? the-root-module 'use-emacs-interface)
	 use-emacs-interface)
    (begin
      (if (memq 'debug-extensions *features*)
	  (debug-enable 'backtrace))
      (define-module (guile) :use-module (ice-9 emacs))))


;;; {Load regexp code if regexp primitives are available.}

(if (memq 'regex *features*)
    (define-module (guile) :use-module (ice-9 regex)))


;;; {Check that the interpreter and scheme code match up.}

(let ((show-line
       (lambda args
	 (with-output-to-port (current-error-port)
	   (lambda ()
	     (display (car (command-line)))
	     (display ": ")
	     (for-each (lambda (string) (display string))
		       args) 
	     (newline))))))

  (load-from-path "ice-9/version.scm")

  (if (not (string=?
	    (libguile-config-stamp)	; from the interprpreter
	    (ice-9-config-stamp)))	; from the Scheme code
      (begin
	(show-line "warning: different versions of libguile and ice-9:")
	(show-line "libguile: configured on " (libguile-config-stamp))
	(show-line "ice-9:    configured on " (ice-9-config-stamp)))))
    


(define-module (guile))

(append! %load-path (cons "." ()))
