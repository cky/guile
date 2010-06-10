;;;; Copyright (C) 2003, 2005, 2006, 2009, 2010 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;

(define-module (ice-9 deprecated)
  #:export (substring-move-left! substring-move-right!
            dynamic-maybe-call dynamic-maybe-link
            try-module-linked try-module-dynamic-link
            list* feature? eval-case unmemoize-expr
            $asinh
            $acosh
            $atanh
            $sqrt
            $abs
            $exp
            $log
            $sin
            $cos
            $tan
            $asin
            $acos
            $atan
            $sinh
            $cosh
            $tanh
            closure?
            %nil
            @bind
            scm-style-repl)

  #:replace (module-ref-submodule module-define-submodule!))


;;;; Deprecated definitions.

(define substring-move-left! substring-move!)
(define substring-move-right! substring-move!)


;; This method of dynamically linking Guile Extensions is deprecated.
;; Use `load-extension' explicitly from Scheme code instead.

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

(define registered-modules '())

(define (register-modules dynobj)
  (set! registered-modules
	(append! (convert-c-registered-modules dynobj)
		 registered-modules)))

(define (warn-autoload-deprecation modname)
  (issue-deprecation-warning
   "Autoloading of compiled code modules is deprecated."
   "Write a Scheme file instead that uses `load-extension'.")
  (issue-deprecation-warning
   (simple-format #f "(You just autoloaded module ~S.)" modname)))

(define (init-dynamic-module modname)
  ;; Register any linked modules which have been registered on the C level
  (register-modules #f)
  (or-map (lambda (modinfo)
	    (if (equal? (car modinfo) modname)
		(begin
		  (warn-autoload-deprecation modname)
		  (set! registered-modules (delq! modinfo registered-modules))
		  (let ((mod (resolve-module modname #f)))
		    (save-module-excursion
		     (lambda ()
		       (set-current-module mod)
		       (set-module-public-interface! mod mod)
		       (dynamic-call (cadr modinfo) (caddr modinfo))
		       ))
		    #t))
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
    (string-append "scm_init"
		   (list->string (map (lambda (c)
					(if (or (char-alphabetic? c)
						(char-numeric? c))
					    c
					    #\_))
				      (string->list mod-name)))
		   "_module"))

  ;; Put the subdirectory for this module in the car of SUBDIR-AND-LIBNAME,
  ;; and the `libname' (the name of the module prepended by `lib') in the cdr
  ;; field.  For example, if MODULE-NAME is the list (inet tcp-ip udp), then
  ;; SUBDIR-AND-LIBNAME will be the pair ("inet/tcp-ip" . "libudp").
  (let ((subdir-and-libname
	 (let loop ((dirs "")
		    (syms module-name))
	   (if (null? (cdr syms))
	       (cons dirs (string-append "lib" (symbol->string (car syms))))
	       (loop (string-append dirs (symbol->string (car syms)) "/")
		     (cdr syms)))))
	(init (make-init-name (apply string-append
				     (map (lambda (s)
					    (string-append "_"
							   (symbol->string s)))
					  module-name)))))
    (let ((subdir (car subdir-and-libname))
	  (libname (cdr subdir-and-libname)))

      ;; Now look in each dir in %LOAD-PATH for `subdir/libfoo.la'.  If that
      ;; file exists, fetch the dlname from that file and attempt to link
      ;; against it.  If `subdir/libfoo.la' does not exist, or does not seem
      ;; to name any shared library, look for `subdir/libfoo.so' instead and
      ;; link against that.
      (let check-dirs ((dir-list %load-path))
	(if (null? dir-list)
	    #f
	    (let* ((dir (in-vicinity (car dir-list) subdir))
		   (sharlib-full
		    (or (try-using-libtool-name dir libname)
			(try-using-sharlib-name dir libname))))
	      (if (and sharlib-full (file-exists? sharlib-full))
		  (link-dynamic-module sharlib-full init)
		  (check-dirs (cdr dir-list)))))))))

(define (try-using-libtool-name libdir libname)
  (let ((libtool-filename (in-vicinity libdir
				       (string-append libname ".la"))))
    (and (file-exists? libtool-filename)
	 libtool-filename)))

(define (try-using-sharlib-name libdir libname)
  (in-vicinity libdir (string-append libname ".so")))

(define (link-dynamic-module filename initname)
  ;; Register any linked modules which have been registered on the C level
  (register-modules #f)
  (let ((dynobj (dynamic-link filename)))
    (dynamic-call initname dynobj)
    (register-modules dynobj)))

(define (try-module-linked module-name)
  (init-dynamic-module module-name))

(define (try-module-dynamic-link module-name)
  (and (find-and-link-dynamic-module module-name)
       (init-dynamic-module module-name)))


(define (list* . args)
  (issue-deprecation-warning "'list*' is deprecated.  Use 'cons*' instead.")
  (apply cons* args))

(define (feature? sym)
  (issue-deprecation-warning
   "`feature?' is deprecated.  Use `provided?' instead.")
  (provided? sym))

(define-macro (eval-case . clauses)
  (issue-deprecation-warning
   "`eval-case' is deprecated.  Use `eval-when' instead.")
  ;; Practically speaking, eval-case only had load-toplevel and else as
  ;; conditions.
  (cond
   ((assoc-ref clauses '(load-toplevel))
    => (lambda (exps)
         ;; the *unspecified so that non-toplevel definitions will be
         ;; caught
         `(begin *unspecified* . ,exps)))
   ((assoc-ref clauses 'else)
    => (lambda (exps)
         `(begin *unspecified* . ,exps)))
   (else
    `(begin))))

;; The strange prototype system for uniform arrays has been
;; deprecated.
(read-hash-extend
 #\y
 (lambda (c port)
   (issue-deprecation-warning
    "The `#y' bitvector syntax is deprecated.  Use `#*' instead.")
   (let ((x (read port)))
     (cond
      ((list? x)
       (list->bitvector
        (map (lambda (x)
               (cond ((zero? x) #f)
                     ((eqv? x 1) #t)
                     (else (error "invalid #y element" x))))
             x)))
      (else
       (error "#y needs to be followed by a list" x))))))

(define (unmemoize-expr . args)
  (issue-deprecation-warning
   "`unmemoize-expr' is deprecated. Use `unmemoize-expression' instead.")
  (apply unmemoize-expression args))

(define ($asinh z) (asinh z))
(define ($acosh z) (acosh z))
(define ($atanh z) (atanh z))
(define ($sqrt z) (sqrt z))
(define ($abs z) (abs z))
(define ($exp z) (exp z))
(define ($log z) (log z))
(define ($sin z) (sin z))
(define ($cos z) (cos z))
(define ($tan z) (tan z))
(define ($asin z) (asin z))
(define ($acos z) (acos z))
(define ($atan z) (atan z))
(define ($sinh z) (sinh z))
(define ($cosh z) (cosh z))
(define ($tanh z) (tanh z))

(define (closure? x)
  (issue-deprecation-warning
   "`closure?' is deprecated. Use `procedure?' instead.")
  (procedure? x))

(define %nil #nil)

;;; @bind is used by the old elisp code as a dynamic scoping mechanism.
;;; Please let the Guile developers know if you are using this macro.
;;;
(define-syntax @bind
  (lambda (x)
    (define (bound-member id ids)
      (cond ((null? ids) #f)
            ((bound-identifier=? id (car ids)) #t)
            ((bound-member (car ids) (cdr ids)))))
    
    (issue-deprecation-warning
     "`@bind' is deprecated. Use `with-fluids' instead.")

    (syntax-case x ()
      ((_ () b0 b1 ...)
       #'(let () b0 b1 ...))
      ((_ ((id val) ...) b0 b1 ...)
       (and-map identifier? #'(id ...))
       (if (let lp ((ids #'(id ...)))
             (cond ((null? ids) #f)
                   ((bound-member (car ids) (cdr ids)) #t)
                   (else (lp (cdr ids)))))
           (syntax-violation '@bind "duplicate bound identifier" x)
           (with-syntax (((old-v ...) (generate-temporaries #'(id ...)))
                         ((v ...) (generate-temporaries #'(id ...))))
             #'(let ((old-v id) ...
                     (v val) ...)
                 (dynamic-wind
                   (lambda ()
                     (set! id v) ...)
                   (lambda () b0 b1 ...)
                   (lambda ()
                     (set! id old-v) ...)))))))))

(define (module-ref-submodule module name)
  (or (hashq-ref (module-submodules module) name)
      (and (module-submodule-binder module)
           ((module-submodule-binder module) module name))
      (let ((var (module-local-variable module name)))
        (and (variable-bound? var)
             (module? (variable-ref var))
             (begin
               (warn "module" module "not in submodules table")
               (variable-ref var))))))

(define (module-define-submodule! module name submodule)
  (let ((var (module-local-variable module name)))
    (if (and var (variable-bound? var) (not (module? (variable-ref var))))
        (warn "defining module" module ": not overriding local definition" var)
        (module-define! module name submodule)))
  (hashq-set! (module-submodules module) name submodule))

;; Define (%app) and (%app modules), and have (app) alias (%app). This
;; side-effects the-root-module, both to the submodules table and (through
;; module-define-submodule! above) the obarray.
;;
(let ((%app (make-module 31)))
  (set-module-name! %app '(%app))
  (module-define-submodule! the-root-module '%app %app)
  (module-define-submodule! the-root-module 'app %app)
  (module-define-submodule! %app 'modules (resolve-module '() #f)))

;; Allow code that poked %module-public-interface to keep on working.
;;
(set! module-public-interface
      (let ((getter module-public-interface))
        (lambda (mod)
          (or (getter mod)
              (cond
               ((and=> (module-local-variable mod '%module-public-interface)
                       variable-ref)
                => (lambda (iface)
                     (issue-deprecation-warning 
"Setting a module's public interface via munging %module-public-interface is
deprecated. Use set-module-public-interface! instead.")
                     (set-module-public-interface! mod iface)
                     iface))
               (else #f))))))

(set! set-module-public-interface!
      (let ((setter set-module-public-interface!))
        (lambda (mod iface)
          (setter mod iface)
          (module-define! mod '%module-public-interface iface))))

(define (scm-style-repl)
  (issue-deprecation-warning 
   "`scm-style-repl' is deprecated. Use the repl from `(system repl repl)' instead.")
  (letrec (
           (start-gc-rt #f)
           (start-rt #f)
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
                    (let ((val
                           (let ((prompt (cond ((string? scm-repl-prompt)
                                                scm-repl-prompt)
                                               ((thunk? scm-repl-prompt)
                                                (scm-repl-prompt))
                                               (scm-repl-prompt "> ")
                                               (else ""))))
                             (repl-reader prompt))))

                      ;; As described in R4RS, the READ procedure updates the
                      ;; port to point to the first character past the end of
                      ;; the external representation of the object.  This
                      ;; means that it doesn't consume the newline typically
                      ;; found after an expression.  This means that, when
                      ;; debugging Guile with GDB, GDB gets the newline, which
                      ;; it often interprets as a "continue" command, making
                      ;; breakpoints kind of useless.  So, consume any
                      ;; trailing newline here, as well as any whitespace
                      ;; before it.
                      ;; But not if EOF, for control-D.
                      (if (not (eof-object? val))
                          (consume-trailing-whitespace))
                      (run-hook after-read-hook)
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
                    (run-hook before-eval-hook sourc)
                    (let ((val (start-stack 'repl-stack
                                            ;; If you change this procedure
                                            ;; (primitive-eval), please also
                                            ;; modify the repl-stack case in
                                            ;; save-stack so that stack cutting
                                            ;; continues to work.
                                            (primitive-eval sourc))))
                      (run-hook after-eval-hook sourc)
                      val)))


           (-print (let ((maybe-print (lambda (result)
                                        (if (or scm-repl-print-unspecified
                                                (not (unspecified? result)))
                                            (begin
                                              (write result)
                                              (newline))))))
                     (lambda (result)
                       (if (not scm-repl-silent)
                           (begin
                             (run-hook before-print-hook result)
                             (maybe-print result)
                             (run-hook after-print-hook result)
                             (if scm-repl-verbose
                                 (repl-report))
                             (force-output))))))

           (-quit (lambda (args)
                    (if scm-repl-verbose
                        (begin
                          (display ";;; QUIT executed, repl exitting")
                          (newline)
                          (repl-report)))
                    args)))

    (let ((status (error-catching-repl -read
                                       -eval
                                       -print)))
      (-quit status))))
