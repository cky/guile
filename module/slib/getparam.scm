;;; "getparam.scm" convert getopt to passing parameters by name.
; Copyright 1995, 1996, 1997, 2001 Aubrey Jaffer
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

(require 'getopt)
(require 'coerce)

(define (getopt->parameter-list argc argv optnames arities types aliases
				. description)
  (define (can-take-arg? opt)
    (not (eq? 'boolean (list-ref arities (position opt optnames)))))
  (let ((progname (list-ref argv (+ -1 *optind*)))
	(optlist '())
	(long-opt-list '())
	(optstring #f)
	(pos-args '())
	(parameter-list (make-parameter-list optnames))
	(curopt '*unclaimed-argument*)
	(positional? (assv 0 aliases))
	(unclaimeds '()))
    (define (adjoin-val val curopt)
      (define ntyp (list-ref types (position curopt optnames)))
      (adjoin-parameters! parameter-list
			  (list curopt (case ntyp
					 ((expression) val)
					 (else (coerce val ntyp))))))
    (define (finish)
      (cond
       (positional?
	(set! unclaimeds (reverse unclaimeds))
	(do ((idx 2 (+ 1 idx))
	     (alias+ (assv 1 aliases) (assv idx aliases))
	     (alias- (assv -1 aliases) (assv (- idx) aliases)))
	    ((or (not (or alias+ alias-)) (null? unclaimeds)))
	  (set! unclaimeds (reverse unclaimeds))
	  (cond (alias-
		 (set! curopt (cadr alias-))
		 (adjoin-val (car unclaimeds) curopt)
		 (set! unclaimeds (cdr unclaimeds))))
	  (set! unclaimeds (reverse unclaimeds))
	  (cond ((and alias+ (not (null? unclaimeds)))
		 (set! curopt (cadr alias+))
		 (adjoin-val (car unclaimeds) curopt)
		 (set! unclaimeds (cdr unclaimeds)))))
	(let ((alias (assv '0 aliases)))
	  (cond (alias
		 (set! curopt (cadr alias))
		 (for-each (lambda (unc) (adjoin-val unc curopt)) unclaimeds)
		 (set! unclaimeds '()))))))
      (cond ((not (null? unclaimeds))
	     (slib:warn 'getopt->parameter-list 'arguments 'unclaimed unclaimeds)
	     (apply parameter-list->getopt-usage
		    progname optnames arities types aliases description))
	    (else parameter-list)))
    (set! aliases
	  (map (lambda (alias)
		 (cond ((string? (car alias))
			(let ((str (string-copy (car alias))))
			  (do ((i (+ -1 (string-length str)) (+ -1 i)))
			      ((negative? i) (cons str (cdr alias)))
			    (cond ((char=? #\ (string-ref str i))
				   (string-set! str i #\-))))))
		       ((number? (car alias))
			(set! positional? (car alias))
			alias)
		       (else alias)))
	       aliases))
    (for-each
     (lambda (alias)
       (define opt (car alias))
       (cond ((number? opt) (set! pos-args (cons opt pos-args)))
	     ((not (string? opt)))
	     ((< 1 (string-length opt))
	      (set! long-opt-list (cons opt long-opt-list)))
	     ((not (= 1 (string-length opt))))
	     ((can-take-arg? (cadr alias))
	      (set! optlist (cons (string-ref opt 0) (cons #\: optlist))))
	     (else (set! optlist (cons (string-ref opt 0) optlist)))))
     aliases)
    (set! optstring (list->string (cons #\: optlist)))
    (let loop ()
      (let ((opt (getopt-- argc argv optstring)))
	(case opt
	  ((#\: #\?)
	   (slib:warn 'getopt->parameter-list
		      (case opt
			((#\:) "argument missing after")
			((#\?) "unrecognized option"))
		      (string #\- getopt:opt))
	   (apply parameter-list->getopt-usage
		  progname optnames arities types aliases description))
	  ((#f)
	   (cond ((and (< *optind* argc)
		       (string=? "-" (list-ref argv *optind*)))
		  (set! *optind* (+ 1 *optind*))
		  (finish))
		 ((< *optind* argc)
		  (let ((topt (assoc curopt aliases)))
		    (if topt (set! curopt (cadr topt)))
		    (cond
		     ((and positional? (not topt))
		      (set! unclaimeds
			    (cons (list-ref argv *optind*) unclaimeds))
		      (set! *optind* (+ 1 *optind*)) (loop))
		     ((and (member curopt optnames)
			   (adjoin-val (list-ref argv *optind*) curopt))
		      (set! *optind* (+ 1 *optind*)) (loop))
		     (else (slib:error 'getopt->parameter-list curopt
				       (list-ref argv *optind*)
				       'not 'supported)))))
		 (else (finish))))
	  (else
	   (cond ((char? opt) (set! opt (string opt))))
	   (let ((topt (assoc opt aliases)))
	     (if topt (set! topt (cadr topt)))
	     (cond
	      ((not topt)
	       (slib:warn "Option not recognized -" opt)
	       (apply parameter-list->getopt-usage
		      progname optnames arities types aliases description))
	      ((not (can-take-arg? topt))
	       (adjoin-parameters! parameter-list (list topt #t))
	       (loop))
	      (*optarg* (set! curopt topt) (adjoin-val *optarg* curopt) (loop))
	      (else
;;;	       (slib:warn 'getopt->parameter-list "= missing for option--" opt)
	       (set! curopt topt) (loop))))))))))

(define (parameter-list->getopt-usage comname optnames arities types aliases
				      . description)
  (require 'printf)
  (require 'common-list-functions)
  (let ((aliast (map list optnames))
	(strlen=1? (lambda (s) (= 1 (string-length s))))
	(cep (current-error-port)))
    (for-each (lambda (alias)
		(let ((apr (assq (cadr alias) aliast)))
		  (set-cdr! apr (cons (car alias) (cdr apr)))))
	      aliases)
    (fprintf cep "Usage: %s [OPTION ARGUMENT ...] ..." comname)
    (do ((pos+ '()) (pos- '())
	 (idx 2 (+ 1 idx))
	 (alias+ (assv 1 aliases) (assv idx aliases))
	 (alias- (assv -1 aliases) (assv (- idx) aliases)))
	((not (or alias+ alias-))
	 (for-each (lambda (alias) (fprintf cep " <%s>" (cadr alias)))
		   (reverse pos+))
	 (let ((alias (assv 0 aliases)))
	   (if alias (fprintf cep " <%s> ..." (cadr alias))))
	 (for-each (lambda (alias) (fprintf cep " <%s>" (cadr alias)))
		   pos-))
      (cond (alias- (set! pos- (cons alias- pos-))))
      (cond (alias+ (set! pos+ (cons alias+ pos+)))))
    (fprintf cep "\\n\\n")
    (for-each
     (lambda (optname arity aliat)
       (let loop ((initials (remove-if-not strlen=1? (remove-if number? (cdr aliat))))
		  (longname (remove-if strlen=1? (remove-if number? (cdr aliat)))))
	 (cond ((and (null? initials) (null? longname)))
	       (else (fprintf cep
			      (case arity
				((boolean) "  %3s %s\\n")
				(else "  %3s %s<%s> %s\\n"))
			      (if (null? initials)
				  ""
				  (string-append "-" (car initials)
						 (if (null? longname) " " ",")))
			      (if (null? longname)
				  "      "
				  (string-append "--" (car longname)
						 (case arity
						   ((boolean) " ")
						   (else "="))))
			      (case arity
				((boolean) "")
				(else optname))
			      (case arity
				((nary nary1) "...")
				(else "")))
		     (loop (if (null? initials) '() (cdr initials))
			   (if (null? longname) '() (cdr longname)))))))
     optnames arities aliast)
    (for-each (lambda (desc) (fprintf cep "  %s\\n" desc)) description))
  #f)

(define (getopt->arglist argc argv optnames positions
			 arities types defaulters checks aliases . description)
  (define progname (list-ref argv (+ -1 *optind*)))
  (let* ((params (apply getopt->parameter-list
			argc argv optnames arities types aliases description))
	 (fparams (and params (fill-empty-parameters defaulters params))))
    (cond ((and (list? params)
		(check-parameters checks fparams)
		(parameter-list->arglist positions arities fparams)))
	  (params (apply parameter-list->getopt-usage
			 progname optnames arities types aliases description))
	  (else #f))))

