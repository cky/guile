;;; "batch.scm" Group and execute commands on various systems.
;Copyright (C) 1994, 1995, 1997 Aubrey Jaffer
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

(require 'line-i/o)			;Just for write-line
(require 'parameters)
(require 'database-utilities)
(require 'string-port)
(require 'tree)

(define system
  (if (provided? 'system)
      system
      (lambda (str) 1)))
(define system:success?
  (case (software-type)
    ((VMS) (lambda (int) (eqv? 1 int)))
    (else zero?)))
;;(trace system system:success? exit quit slib:exit)

(define (batch:port parms)
  (let ((bp (parameter-list-ref parms 'batch-port)))
    (cond ((or (not (pair? bp)) (not (output-port? (car bp))))
	   (slib:warn 'batch-line "missing batch-port parameter" bp)
	   (current-output-port))
	  (else (car bp)))))

(define (batch:dialect parms)		; was batch-family
  (car (parameter-list-ref parms 'batch-dialect)))

(define (write-batch-line str line-limit port)
  (cond ((and line-limit (>= (string-length str) line-limit))
	 (slib:warn 'write-batch-line 'too-long
		    (string-length str) '> line-limit)
	 #f)
	(else (write-line str port) #t)))
(define (batch-line parms str)
  (write-batch-line str (batch:line-length-limit parms) (batch:port parms)))

;;; add a Scheme batch-dialect?

(define (batch:try-chopped-command parms . args)
  (define args-but-last (batch:flatten (butlast args 1)))
  (define line-limit (batch:line-length-limit parms))
  (let loop ((fodder (car (last-pair args))))
    (let ((str (batch:glued-line parms
				 (batch:flatten
				  (append args-but-last (list fodder))))))
      (cond ((< (string-length str) line-limit)
	     (batch:try-command parms str))
	    ((< (length fodder) 2)
	     (slib:warn 'batch:try-chopped-command "can't fit in " line-limit
			(cons proc (append args-but-last (list fodder))))
	     #f)
	    (else (let ((hlen (quotient (length fodder) 2)))
		    (and (loop (last fodder hlen))
			 (loop (butlast fodder hlen)))))))))

(define (batch:glued-line parms strings)
  (case (batch:dialect parms)
    ((vms) (apply string-join " " "$" strings))
    ((unix dos amigados system *unknown*) (apply string-join " " strings))
    (else #f)))

(define (batch:try-command parms . strings)
  (set! strings (batch:flatten strings))
  (let ((line (batch:glued-line parms strings)))
    (and line
	 (case (batch:dialect parms)
	   ((unix dos vms amigados) (batch-line parms line))
	   ((system)
	    (let ((port (batch:port parms)))
	      (write `(system ,line) port) (newline port)
	      (and (provided? 'system) (system:success? (system line)))))
	   ((*unknown*)
	    (let ((port (batch:port parms)))
	      (write `(system ,line) port) (newline port) #t))
	   (else #f)))))

(define (batch:command parms . strings)
  (cond ((apply batch:try-command parms strings))
	(else (slib:error 'batch:command 'failed strings))))

(define (batch:run-script parms name . strings)
  (case (batch:dialect parms strings)
    ((vms) (batch:command parms (string-append "@" name) strings))
    (else (batch:command parms name strings))))

(define (batch:write-comment-line dialect line port)
  (case dialect
    ((unix) (write-batch-line (string-append "# " line) #f port))
    ((dos) (write-batch-line (string-append "rem " line) #f port))
    ((vms) (write-batch-line (string-append "$! " line) #f port))
    ((amigados) (write-batch-line (string-append "; " line) #f port))
    ((system) (write-batch-line (string-append "; " line) #f port))
    ((*unknown*) (write-batch-line (string-append ";;; " line) #f port)
     ;;(newline port)
     #f)))

(define (batch:comment parms . lines)
  (define port (batch:port parms))
  (define dialect (batch:dialect parms))
  (set! lines (batch:flatten lines))
  (every (lambda (line)
	   (batch:write-comment-line dialect line port))
	 lines))

(define (batch:lines->file parms file . lines)
  (define port (batch:port parms))
  (set! lines (batch:flatten lines))
  (case (or (batch:dialect parms) '*unknown*)
    ((unix) (batch-line parms (string-append "rm -f " file))
	    (every
	     (lambda (string)
	       (batch-line parms (string-append "echo '" string "'>>" file)))
	     lines))
    ((dos) (batch-line parms (string-append "DEL " file))
	   (every
	    (lambda (string)
	      (batch-line parms
			  (string-append "ECHO" (if (equal? "" string) "." " ")
					 string ">>" file)))
	    lines))
    ((vms) (and (batch-line parms (string-append "$DELETE " file))
		(batch-line parms (string-append "$CREATE " file))
		(batch-line parms (string-append "$DECK"))
		(every (lambda (string) (batch-line parms string))
		       lines)
		(batch-line parms (string-append "$EOD"))))
    ((amigados) (batch-line parms (string-append "delete force " file))
	    (every
	     (lambda (str)
	       (letrec ((star-quote
			 (lambda (str)
			   (if (equal? "" str)
			       str
			       (let* ((ch (string-ref str 0))
				      (s (if (char=? ch #\")
					     (string #\* ch)
					     (string ch))))
				 (string-append
				  s
				  (star-quote
				   (substring str 1 (string-length str)))))))))
		 (batch-line parms (string-append "echo \"" (star-quote str)
						  "\" >> " file))))
	     lines))
    ((system) (write `(delete-file ,file) port) (newline port)
	      (delete-file file)
	      (require 'pretty-print)
	      (pretty-print `(call-with-output-file ,file
			       (lambda (fp)
				 (for-each
				  (lambda (string) (write-line string fp))
				  ',lines)))
			    port)
	      (call-with-output-file file
		(lambda (fp) (for-each (lambda (string) (write-line string fp))
				       lines)))
	      #t)
    ((*unknown*)
     (write `(delete-file ,file) port) (newline port)
     (require 'pretty-print)
     (pretty-print
      `(call-with-output-file ,file
	 (lambda (fp)
	   (for-each
	    (lambda (string)
	      (write-line string fp))
	    ,lines)))
      port)
     #f)))

(define (batch:delete-file parms file)
  (define port (batch:port parms))
  (case (batch:dialect parms)
    ((unix) (batch-line parms (string-append "rm -f " file))
	    #t)
    ((dos) (batch-line parms (string-append "DEL " file))
	   #t)
    ((vms) (batch-line parms (string-append "$DELETE " file))
	   #t)
    ((amigados) (batch-line parms (string-append "delete force " file))
	    #t)
    ((system) (write `(delete-file ,file) port) (newline port)
	      (delete-file file))	; SLIB provides
    ((*unknown*) (write `(delete-file ,file) port) (newline port)
		 #f)))

(define (batch:rename-file parms old-name new-name)
  (define port (batch:port parms))
  (case (batch:dialect parms)
    ((unix) (batch-line parms (string-join " " "mv -f" old-name new-name)))
    ;;((dos) (batch-line parms (string-join " " "REN" old-name new-name)))
    ((dos) (batch-line parms (string-join " " "MOVE" "/Y" old-name new-name)))
    ((vms) (batch-line parms (string-join " " "$RENAME" old-name new-name)))
    ((amigados) (batch-line parms (string-join " " "failat 21"))
		(batch-line parms (string-join " " "delete force" new-name))
		(batch-line parms (string-join " " "rename" old-name new-name)))
    ((system) (batch:extender 'rename-file batch:rename-file))
    ((*unknown*) (write `(rename-file ,old-name ,new-name) port)
		 (newline port)
		 #f)))

(define (batch:write-header-comment dialect name port)
  (batch:write-comment-line
   dialect
   (string-append (if (string? name)
		      (string-append "\"" name "\"")
		      (case dialect
			((system *unknown*) "Scheme")
			((vms) "VMS")
			((dos) "DOS")
			((default-for-platform) "??")
			(else (symbol->string dialect))))
		  " script created by SLIB/batch "
		  (cond ((provided? 'bignum)
			 (require 'posix-time)
			 (let ((ct (ctime (current-time))))
			   (substring ct 0 (+ -1 (string-length ct)))))
			(else "")))
   port))

(define (batch:call-with-output-script parms name proc)
  (define dialect (batch:dialect parms))
  (case dialect
    ((unix) ((cond ((and (string? name) (provided? 'system))
		    (lambda (proc)
		      (let ((ans (call-with-output-file name proc)))
			(system (string-append "chmod +x " name))
			ans)))
		   ((output-port? name) (lambda (proc) (proc name)))
		   (else (lambda (proc) (proc (current-output-port)))))
	     (lambda (port)
	       (write-line "#!/bin/sh" port)
	       (batch:write-header-comment dialect name port)
	       (proc port))))

    ((dos) ((cond ((string? name)
		   (lambda (proc)
		     (call-with-output-file (string-append name ".bat") proc)))
		  ((output-port? name) (lambda (proc) (proc name)))
		  (else (lambda (proc) (proc (current-output-port)))))
	    (lambda (port)
	      (batch:write-header-comment dialect name port)
	      (proc port))))

    ((vms) ((cond ((string? name)
		   (lambda (proc)
		     (call-with-output-file (string-append name ".COM") proc)))
		  ((output-port? name) (lambda (proc) (proc name)))
		  (else (lambda (proc) (proc (current-output-port)))))
	    (lambda (port)
	      (batch:write-header-comment dialect name port)
	      ;;(write-line "$DEFINE/USER SYS$OUTPUT BUILD.LOG" port)
	      (proc port))))

    ((amigados) ((cond ((and (string? name) (provided? 'system))
		    (lambda (proc)
		      (let ((ans (call-with-output-file name proc)))
			(system (string-append "protect " name " rswd"))
			ans)))
		   ((output-port? name) (lambda (proc) (proc name)))
		   (else (lambda (proc) (proc (current-output-port)))))
	     (lambda (port)
	       (batch:write-header-comment dialect name port)
	       (proc port))))

    ((system) ((cond ((and (string? name) (provided? 'system))
		      (lambda (proc)
			(let ((ans (call-with-output-file name
				     (lambda (port) (proc name)))))
			  (system (string-append "chmod +x " name))
			  ans)))
		     ((output-port? name) (lambda (proc) (proc name)))
		     (else (lambda (proc) (proc (current-output-port)))))
	       (lambda (port)
		 (batch:write-header-comment dialect name port)
		 (proc port))))

    ((*unknown*) ((cond ((and (string? name) (provided? 'system))
			 (lambda (proc)
			   (let ((ans (call-with-output-file name
					(lambda (port) (proc name)))))
			     (system (string-append "chmod +x " name))
			     ans)))
			((output-port? name) (lambda (proc) (proc name)))
			(else (lambda (proc) (proc (current-output-port)))))
		  (lambda (port)
		    (batch:write-header-comment dialect name port)
		    (proc port))))))

;;; This little ditty figures out how to use a Scheme extension or
;;; SYSTEM to execute a command that is not available in the batch
;;; mode chosen.

(define (batch:extender NAME BATCHER)
  (lambda (parms . args)
    (define port (batch:port parms))
    (cond
     ((provided? 'i/o-extensions)	; SCM specific
      (write `(,NAME ,@args) port)
      (newline port)
      (apply (slib:eval NAME) args))
     ((not (provided? 'system)) #f)
     (else
      (let ((pl (make-parameter-list (map car parms))))
	(adjoin-parameters!
	 pl (cons 'batch-dialect (os->batch-dialect
				  (parameter-list-ref parms 'platform))))
	(system
	 (call-with-output-string
	  (lambda (port)
	    (batch:call-with-output-script
	     port
	     (lambda (batch-port)
	       (define new-parms (copy-tree pl))
	       (adjoin-parameters! new-parms (list 'batch-port batch-port))
	       (apply BATCHER new-parms args)))))))))))

(define (truncate-up-to str chars)
  (define (tut str)
    (do ((i (string-length str) (+ -1 i)))
	((or (zero? i) (memv (string-ref str (+ -1 i)) chars))
	 (substring str i (string-length str)))))
  (cond ((char? chars) (set! chars (list chars)))
	((string? chars) (set! chars (string->list chars))))
  (if (string? str) (tut str) (map tut str)))

(define (must-be-first firsts lst)
  (append (remove-if-not (lambda (i) (member i lst)) firsts)
	  (remove-if (lambda (i) (member i firsts)) lst)))

(define (must-be-last lst lasts)
  (append (remove-if (lambda (i) (member i lasts)) lst)
	  (remove-if-not (lambda (i) (member i lst)) lasts)))

(define (string-join joiner . args)
  (if (null? args) ""
      (apply string-append
	     (car args)
	     (map (lambda (s) (string-append joiner s)) (cdr args)))))

(define (batch:flatten strings)
  (apply
   append (map
	   (lambda (obj)
	     (cond ((eq? "" obj) '())
		   ((string? obj) (list obj))
		   ((eq? #f obj) '())
		   ((null? obj) '())
		   ((list? obj) (batch:flatten obj))
		   (else (slib:error 'batch:flatten "unexpected type"
				     obj "in" strings))))
	   strings)))

(define batch:platform (software-type))
(cond ((and (eq? 'unix batch:platform) (provided? 'system))
       (let ((file-name (tmpnam)))
	 (system (string-append "uname > " file-name))
	 (set! batch:platform (call-with-input-file file-name read))
	 (delete-file file-name))))

(define batch:database #f)
(define os->batch-dialect #f)
(define batch-dialect->line-length-limit #f)

(define (batch:line-length-limit parms)
  (let ((bl (parameter-list-ref parms 'batch-line-length-limit)))
    (if bl (car bl) (batch-dialect->line-length-limit (batch:dialect parms)))))

(define (batch:initialize! database)
  (set! batch:database database)
  (define-tables database

    '(batch-dialect
      ((family atom))
      ((line-length-limit number))
      ((unix 1023)
       (dos 127)
       (vms 1023)
       (amigados 511)
       (system 1023)
       (*unknown* -1)))

    '(operating-system
      ((name symbol))
      ((os-family batch-dialect))
      (;;(3b1		*unknown*)
       (*unknown*	*unknown*)
       (acorn		*unknown*)
       (aix		unix)
       (alliant		*unknown*)
       (amiga		amigados)
       (apollo		unix)
       (apple2		*unknown*)
       (arm		*unknown*)
       (atari.st	*unknown*)
       (cdc		*unknown*)
       (celerity	*unknown*)
       (concurrent	*unknown*)
       (convex		*unknown*)
       (encore		*unknown*)
       (harris		*unknown*)
       (hp-ux		unix)
       (hp48		*unknown*)
       (irix		unix)
       (isis		*unknown*)
       (linux		unix)
       (mac		*unknown*)
       (masscomp	unix)
       (mips		*unknown*)
       (ms-dos		dos)
       (ncr		*unknown*)
       (newton		*unknown*)
       (next		unix)
       (novell		*unknown*)
       (os/2		dos)
       (osf1		unix)
       (prime		*unknown*)
       (psion		*unknown*)
       (pyramid		*unknown*)
       (sequent		*unknown*)
       (sgi		*unknown*)
       (stratus		*unknown*)
       (sunos		unix)
       (transputer	*unknown*)
       (unicos		unix)
       (unix		unix)
       (vms		vms)
       )))

  ((database 'add-domain) '(operating-system operating-system #f symbol #f))
  (set! os->batch-dialect (((batch:database 'open-table) 'operating-system #f)
			   'get 'os-family))
  (set! batch-dialect->line-length-limit
	(((batch:database 'open-table) 'batch-dialect #f)
	 'get 'line-length-limit))
  )
