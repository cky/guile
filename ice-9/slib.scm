;;; installed-scm-file
(define-module #/ice-9/slib)
  


(define (eval-load <filename> evl)
  (if (not (file-exists? <filename>))
      (set! <filename> (string-append <filename> (scheme-file-suffix))))
  (call-with-input-file <filename>
    (lambda (port)
      (let ((old-load-pathname *load-pathname*))
	(set! *load-pathname* <filename>)
	(do ((o (read port #t read-sharp) (read port #t read-sharp)))
	    ((eof-object? o))
	  (evl o))
	(set! *load-pathname* old-load-pathname)))))



(define slib:exit quit)
(define slib:error error)
(define slib:eval eval)
(define defmacro:eval eval)
(define logical:logand logand)
(define logical:logior logior)
(define logical:logxor logxor)
(define logical:lognot lognot)
(define logical:ash ash)
(define logical:logcount logcount)
(define logical:integer-length integer-length)
(define logical:bit-extract bit-extract)
(define logical:integer-expt integer-expt)
(define logical:ipow-by-squaring ipow-by-squaring)
(define slib:eval-load eval-load)
(define slib:tab #\tab)
(define slib:form-feed #\page)

(define slib:features
  (append '(source
	    eval
	    abort
	    alist
	    defmacro
	    delay
	    dynamic-wind
	    full-continuation
	    hash
	    hash-table
	    line-i/o
	    logical
	    multiarg/and-
	    multiarg-apply
	    promise
	    rev2-procedures
	    rev4-optional-procedures
	    string-port
	    with-file)

	  (if (defined? getenv)
	      '(getenv)
	      '())

	  (if (defined? current-time)
	      '(current-time)
	      '())

	  (if (defined? system)
	      '(system)
	      '())

	  (if (defined? array?)
	      '(array)
	      '())

	  (if (defined? char-ready?)
	      '(char-ready?)
	      '())

	  (if (defined? array-for-each)
	      '(array-for-each)
	      '())

	  (if (and (string->number "0.0") (inexact? (string->number "0.0")))
	      '(inexact)
	      '())

	  (if (rational? (string->number "1/19"))
	      '(rational)
	      '())

	  (if (real? (string->number "0.0"))
	      '(real)
	      ())

	  (if (complex? (string->number "1+i"))
	      '(complex)
	      '())

	  (let ((n (string->number "9999999999999999999999999999999")))
	    (if (and n (exact? n))
		'(bignum)
		'()))))


(define slib-module (current-module))

(define (slib:load name)
  (save-module-excursion
   (lambda ()
     (set-current-module slib-module)
     (let* ((errinfo (catch 'system-error
			    (lambda ()
			      (basic-load name)
			      #f)
			    (lambda args args)))
	    (errinfo (and errinfo
			  (catch 'system-error
				 (lambda ()
				   (basic-load (string-append name ".scm"))
				   #f)
				 (lambda args args)))))
       (if errinfo
	   (apply throw errinfo))))))

(define slib:load-source slib:load)
(define defmacro:load slib:load)

(define slib-parent-dir
  (let* ((path (%search-load-path "slib/require.scm")))
    (make-shared-substring path 0 (- (length path) 17))))

(define-public (implementation-vicinity)
  (string-append slib-parent-dir "/"))
(define (library-vicinity)
  (string-append (implementation-vicinity) "slib/"))
(define (scheme-implementation-type) 'guile)
(define (scheme-implementation-version) "")

(define (output-port-width . arg) 80)
(define (output-port-height . arg) 24)

;;; {Time}
;;;

(define difftime -)
(define offset-time +)


(define %system-define define)

(define define
  (procedure->memoizing-macro
   (lambda (exp env)
     (if (= (length env) 1)
	 `(define-public ,@(cdr exp))
	 `(%system-define ,@(cdr exp))))))

(define (software-type) 'UNIX)

(slib:load (in-vicinity (library-vicinity) "require.scm"))

(define-public require require:require)

;; {Extensions to the require system so that the user can add new
;;  require modules easily.}

(define *vicinity-table*
  (list
   (cons 'implementation (implementation-vicinity))
   (cons 'library (library-vicinity))))

(define (install-require-vicinity name vicinity)
  (let ((entry (assq name *vicinity-table*)))
    (if entry
	(set-cdr! entry vicinity)
	(set! *vicinity-table*
	      (acons name vicinity *vicinity-table*)))))

(define (install-require-module name vicinity-name file-name)
  (let ((entry (assq name *catalog*))
	(vicinity (cdr (assq vicinity-name *vicinity-table*))))
    (let ((path-name (in-vicinity vicinity file-name)))
      (if entry
	  (set-cdr! entry path-name)
	  (set! *catalog*
		(acons name path-name *catalog*))))))
