;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011
;;;; Free Software Foundation, Inc.
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



;;; Commentary:

;;; This file is the first thing loaded into Guile.  It adds many mundane
;;; definitions and a few that are interesting.
;;;
;;; The module system (hence the hierarchical namespace) are defined in this
;;; file.
;;;

;;; Code:



;; Before compiling, make sure any symbols are resolved in the (guile)
;; module, the primary location of those symbols, rather than in
;; (guile-user), the default module that we compile in.

(eval-when (compile)
  (set-current-module (resolve-module '(guile))))



;;; {Error handling}
;;;

;; Define delimited continuation operators, and implement catch and throw in
;; terms of them.

(define make-prompt-tag
  (lambda* (#:optional (stem "prompt"))
    (gensym stem)))

(define default-prompt-tag
  ;; not sure if we should expose this to the user as a fluid
  (let ((%default-prompt-tag (make-prompt-tag)))
    (lambda ()
      %default-prompt-tag)))

(define (call-with-prompt tag thunk handler)
  (@prompt tag (thunk) handler))
(define (abort-to-prompt tag . args)
  (@abort tag args))


;; Define catch and with-throw-handler, using some common helper routines and a
;; shared fluid. Hide the helpers in a lexical contour.

(define with-throw-handler #f)
(let ()
  ;; Ideally we'd like to be able to give these default values for all threads,
  ;; even threads not created by Guile; but alack, that does not currently seem
  ;; possible. So wrap the getters in thunks.
  (define %running-exception-handlers (make-fluid))
  (define %exception-handler (make-fluid))

  (define (running-exception-handlers)
    (or (fluid-ref %running-exception-handlers)
        (begin
          (fluid-set! %running-exception-handlers '())
          '())))
  (define (exception-handler)
    (or (fluid-ref %exception-handler)
        (begin
          (fluid-set! %exception-handler default-exception-handler)
          default-exception-handler)))

  (define (default-exception-handler k . args)
    (cond
     ((eq? k 'quit)
      (primitive-exit (cond
                       ((not (pair? args)) 0)
                       ((integer? (car args)) (car args))
                       ((not (car args)) 1)
                       (else 0))))
     (else
      (format (current-error-port) "guile: uncaught throw to ~a: ~a\n" k args)
      (primitive-exit 1))))

  (define (default-throw-handler prompt-tag catch-k)
    (let ((prev (exception-handler)))
      (lambda (thrown-k . args)
        (if (or (eq? thrown-k catch-k) (eqv? catch-k #t))
            (apply abort-to-prompt prompt-tag thrown-k args)
            (apply prev thrown-k args)))))

  (define (custom-throw-handler prompt-tag catch-k pre)
    (let ((prev (exception-handler)))
      (lambda (thrown-k . args)
        (if (or (eq? thrown-k catch-k) (eqv? catch-k #t))
            (let ((running (running-exception-handlers)))
              (with-fluids ((%running-exception-handlers (cons pre running)))
                (if (not (memq pre running))
                    (apply pre thrown-k args))
                ;; fall through
                (if prompt-tag
                    (apply abort-to-prompt prompt-tag thrown-k args)
                    (apply prev thrown-k args))))
            (apply prev thrown-k args)))))

  (set! catch
        (lambda* (k thunk handler #:optional pre-unwind-handler)
          "Invoke @var{thunk} in the dynamic context of @var{handler} for
exceptions matching @var{key}.  If thunk throws to the symbol
@var{key}, then @var{handler} is invoked this way:
@lisp
 (handler key args ...)
@end lisp

@var{key} is a symbol or @code{#t}.

@var{thunk} takes no arguments.  If @var{thunk} returns
normally, that is the return value of @code{catch}.

Handler is invoked outside the scope of its own @code{catch}.
If @var{handler} again throws to the same key, a new handler
from further up the call chain is invoked.

If the key is @code{#t}, then a throw to @emph{any} symbol will
match this call to @code{catch}.

If a @var{pre-unwind-handler} is given and @var{thunk} throws
an exception that matches @var{key}, Guile calls the
@var{pre-unwind-handler} before unwinding the dynamic state and
invoking the main @var{handler}.  @var{pre-unwind-handler} should
be a procedure with the same signature as @var{handler}, that
is @code{(lambda (key . args))}.  It is typically used to save
the stack at the point where the exception occurred, but can also
query other parts of the dynamic state at that point, such as
fluid values.

A @var{pre-unwind-handler} can exit either normally or non-locally.
If it exits normally, Guile unwinds the stack and dynamic context
and then calls the normal (third argument) handler.  If it exits
non-locally, that exit determines the continuation."
          (if (not (or (symbol? k) (eqv? k #t)))
              (scm-error "catch" 'wrong-type-arg
                         "Wrong type argument in position ~a: ~a"
                         (list 1 k) (list k)))
          (let ((tag (make-prompt-tag "catch")))
            (call-with-prompt
             tag
             (lambda ()
               (with-fluids
                   ((%exception-handler
                     (if pre-unwind-handler
                         (custom-throw-handler tag k pre-unwind-handler)
                         (default-throw-handler tag k))))
                 (thunk)))
             (lambda (cont k . args)
               (apply handler k args))))))

  (set! with-throw-handler
        (lambda (k thunk pre-unwind-handler)
          "Add @var{handler} to the dynamic context as a throw handler
for key @var{key}, then invoke @var{thunk}."
          (if (not (or (symbol? k) (eqv? k #t)))
              (scm-error "with-throw-handler" 'wrong-type-arg
                         "Wrong type argument in position ~a: ~a"
                         (list 1 k) (list k)))
          (with-fluids ((%exception-handler
                         (custom-throw-handler #f k pre-unwind-handler)))
            (thunk))))

  (set! throw
        (lambda (key . args)
          "Invoke the catch form matching @var{key}, passing @var{args} to the
@var{handler}.

@var{key} is a symbol. It will match catches of the same symbol or of @code{#t}.

If there is no handler at all, Guile prints an error and then exits."
          (if (not (symbol? key))
              ((exception-handler) 'wrong-type-arg "throw"
               "Wrong type argument in position ~a: ~a" (list 1 key) (list key))
              (apply (exception-handler) key args)))))




;;; {R4RS compliance}
;;;

(primitive-load-path "ice-9/r4rs")



;;; {Simple Debugging Tools}
;;;

;; peek takes any number of arguments, writes them to the
;; current ouput port, and returns the last argument.
;; It is handy to wrap around an expression to look at
;; a value each time is evaluated, e.g.:
;;
;;      (+ 10 (troublesome-fn))
;;      => (+ 10 (pk 'troublesome-fn-returned (troublesome-fn)))
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



;;; {Features}
;;;

(define (provide sym)
  (if (not (memq sym *features*))
      (set! *features* (cons sym *features*))))

;; Return #t iff FEATURE is available to this Guile interpreter.  In SLIB,
;; provided? also checks to see if the module is available.  We should do that
;; too, but don't.

(define (provided? feature)
  (and (memq feature *features*) #t))



;;; {Structs}
;;;

(define (make-struct/no-tail vtable . args)
  (apply make-struct vtable 0 args))



;;; {and-map and or-map}
;;;
;;; (and-map fn lst) is like (and (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;; (or-map fn lst) is like (or (fn (car lst)) (fn (cadr lst)) (fn...) ...)
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



;; let format alias simple-format until the more complete version is loaded

(define format simple-format)

;; this is scheme wrapping the C code so the final pred call is a tail call,
;; per SRFI-13 spec
(define string-any
  (lambda* (char_pred s #:optional (start 0) (end (string-length s)))
    (if (and (procedure? char_pred)
             (> end start)
             (<= end (string-length s))) ;; let c-code handle range error
        (or (string-any-c-code char_pred s start (1- end))
            (char_pred (string-ref s (1- end))))
        (string-any-c-code char_pred s start end))))

;; this is scheme wrapping the C code so the final pred call is a tail call,
;; per SRFI-13 spec
(define string-every
  (lambda* (char_pred s #:optional (start 0) (end (string-length s)))
    (if (and (procedure? char_pred)
             (> end start)
             (<= end (string-length s))) ;; let c-code handle range error
        (and (string-every-c-code char_pred s start (1- end))
             (char_pred (string-ref s (1- end))))
        (string-every-c-code char_pred s start end))))

;; A variant of string-fill! that we keep for compatability
;;
(define (substring-fill! str start end fill)
  (string-fill! str fill start end))



;; Define a minimal stub of the module API for psyntax, before modules
;; have booted.
(define (module-name x)
  '(guile))
(define (module-add! module sym var)
  (hashq-set! (%get-pre-modules-obarray) sym var))
(define (module-define! module sym val)
  (let ((v (hashq-ref (%get-pre-modules-obarray) sym)))
    (if v
        (variable-set! v val)
        (module-add! (current-module) sym (make-variable val)))))
(define (module-ref module sym)
  (let ((v (module-variable module sym)))
    (if v (variable-ref v) (error "badness!" (pk module) (pk sym)))))
(define (resolve-module . args)
  #f)

;; API provided by psyntax
(define syntax-violation #f)
(define datum->syntax #f)
(define syntax->datum #f)
(define syntax-source #f)
(define identifier? #f)
(define generate-temporaries #f)
(define bound-identifier=? #f)
(define free-identifier=? #f)

;; $sc-dispatch is an implementation detail of psyntax. It is used by
;; expanded macros, to dispatch an input against a set of patterns.
(define $sc-dispatch #f)

;; Load it up!
(primitive-load-path "ice-9/psyntax-pp")
;; The binding for `macroexpand' has now been overridden, making psyntax the
;; expander now.

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x y ...) (if x (and y ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x y ...) (let ((t x)) (if t t (or y ...))))))

;; The "maybe-more" bits are something of a hack, so that we can support
;; SRFI-61. Rewrites into a standalone syntax-case macro would be
;; appreciated.
(define-syntax cond
  (syntax-rules (=> else)
    ((_ "maybe-more" test consequent)
     (if test consequent))

    ((_ "maybe-more" test consequent clause ...)
     (if test consequent (cond clause ...)))

    ((_ (else else1 else2 ...))
     (begin else1 else2 ...))

    ((_ (test => receiver) more-clause ...)
     (let ((t test))
       (cond "maybe-more" t (receiver t) more-clause ...)))

    ((_ (generator guard => receiver) more-clause ...)
     (call-with-values (lambda () generator)
       (lambda t
         (cond "maybe-more"
               (apply guard t) (apply receiver t) more-clause ...))))

    ((_ (test => receiver ...) more-clause ...)
     (syntax-violation 'cond "wrong number of receiver expressions"
                       '(test => receiver ...)))
    ((_ (generator guard => receiver ...) more-clause ...)
     (syntax-violation 'cond "wrong number of receiver expressions"
                       '(generator guard => receiver ...)))
    
    ((_ (test) more-clause ...)
     (let ((t test))
       (cond "maybe-more" t t more-clause ...)))

    ((_ (test body1 body2 ...) more-clause ...)
     (cond "maybe-more"
           test (begin body1 body2 ...) more-clause ...))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (make-promise (lambda () exp)))))

(include-from-path "ice-9/quasisyntax")

(define-syntax current-source-location
  (lambda (x)
    (syntax-case x ()
      ((_)
       (with-syntax ((s (datum->syntax x (syntax-source x))))
         #''s)))))

(define-syntax define-once
  (syntax-rules ()
    ((_ sym val)
     (define sym
       (if (module-locally-bound? (current-module) 'sym) sym val)))))



;;;
;;; Extensible exception printing.
;;;

(define set-exception-printer! #f)
;; There is already a definition of print-exception from backtrace.c
;; that we will override.

(let ((exception-printers '()))
  (define (print-location frame port)
    (let ((source (and=> frame frame-source)))
      ;; source := (addr . (filename . (line . column)))
      (if source
          (let ((filename (or (cadr source) "<unnamed port>"))
                (line (caddr source))
                (col (cdddr source)))
            (format port "~a:~a:~a: " filename line col))
          (format port "ERROR: "))))

  (set! set-exception-printer!
        (lambda (key proc)
          (set! exception-printers (acons key proc exception-printers))))

  (set! print-exception
        (lambda (port frame key args)
          (define (default-printer)
            (format port "Throw to key `~a' with args `~s'." key args))

          (if frame
              (let ((proc (frame-procedure frame)))
                (print-location frame port)
                (format port "In procedure ~a:\n"
                        (or (procedure-name proc) proc))))

          (print-location frame port)
          (catch #t
            (lambda ()
              (let ((printer (assq-ref exception-printers key)))
                (if printer
                    (printer port key args default-printer)
                    (default-printer))))
            (lambda (k . args)
              (format port "Error while printing exception.")))
          (newline port)
          (force-output port))))

;;;
;;; Printers for those keys thrown by Guile.
;;;
(let ()
  (define (scm-error-printer port key args default-printer)
    ;; Abuse case-lambda as a pattern matcher, given that we don't have
    ;; ice-9 match at this point.
    (apply (case-lambda
             ((subr msg args . rest)
              (if subr
                  (format port "In procedure ~a: " subr))
              (apply format port msg (or args '())))
             (_ (default-printer)))
           args))

  (define (syntax-error-printer port key args default-printer)
    (apply (case-lambda
             ((who what where form subform . extra)
              (format port "Syntax error:\n")
              (if where
                  (let ((file (or (assq-ref where 'filename) "unknown file"))
                        (line (and=> (assq-ref where 'line) 1+))
                        (col (assq-ref where 'column)))
                    (format port "~a:~a:~a: " file line col))
                  (format port "unknown location: "))
              (if who
                  (format port "~a: " who))
              (format port "~a" what)
              (if subform
                  (format port " in subform ~s of ~s" subform form)
                  (if form
                      (format port " in form ~s" form))))
             (_ (default-printer)))
           args))

  (set-exception-printer! 'goops-error scm-error-printer)
  (set-exception-printer! 'host-not-found scm-error-printer)
  (set-exception-printer! 'keyword-argument-error scm-error-printer)
  (set-exception-printer! 'misc-error scm-error-printer)
  (set-exception-printer! 'no-data scm-error-printer)
  (set-exception-printer! 'no-recovery scm-error-printer)
  (set-exception-printer! 'null-pointer-error scm-error-printer)
  (set-exception-printer! 'out-of-range scm-error-printer)
  (set-exception-printer! 'program-error scm-error-printer)
  (set-exception-printer! 'read-error scm-error-printer)
  (set-exception-printer! 'regular-expression-syntax scm-error-printer)
  (set-exception-printer! 'signal scm-error-printer)
  (set-exception-printer! 'stack-overflow scm-error-printer)
  (set-exception-printer! 'system-error scm-error-printer)
  (set-exception-printer! 'try-again scm-error-printer)
  (set-exception-printer! 'unbound-variable scm-error-printer)
  (set-exception-printer! 'wrong-number-of-args scm-error-printer)
  (set-exception-printer! 'wrong-type-arg scm-error-printer)

  (set-exception-printer! 'syntax-error syntax-error-printer))




;;; {Defmacros}
;;;

(define-syntax define-macro
  (lambda (x)
    "Define a defmacro."
    (syntax-case x ()
      ((_ (macro . args) doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ (macro . args) body ...)
       #'(define-macro macro #f (lambda args body ...)))
      ((_ macro doc transformer)
       (or (string? (syntax->datum #'doc))
           (not (syntax->datum #'doc)))
       #'(define-syntax macro
           (lambda (y)
             doc
             #((macro-type . defmacro)
               (defmacro-args args))
             (syntax-case y ()
               ((_ . args)
                (let ((v (syntax->datum #'args)))
                  (datum->syntax y (apply transformer v)))))))))))

(define-syntax defmacro
  (lambda (x)
    "Define a defmacro, with the old lispy defun syntax."
    (syntax-case x ()
      ((_ macro args doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ macro args body ...)
       #'(define-macro macro #f (lambda args body ...))))))

(provide 'defmacro)



;;; {Deprecation}
;;;

(define-syntax begin-deprecated
  (lambda (x)
    (syntax-case x ()
      ((_ form form* ...)
       (if (include-deprecated-features)
           #'(begin form form* ...)
           #'(begin))))))



;;; {Trivial Functions}
;;;

(define (identity x) x)

(define (compose proc . rest)
  "Compose PROC with the procedures in REST, such that the last one in
REST is applied first and PROC last, and return the resulting procedure.
The given procedures must have compatible arity."
  (if (null? rest)
      proc
      (let ((g (apply compose rest)))
        (lambda args
          (call-with-values (lambda () (apply g args)) proc)))))

(define (negate proc)
  "Return a procedure with the same arity as PROC that returns the `not'
of PROC's result."
  (lambda args
    (not (apply proc args))))

(define (const value)
  "Return a procedure that accepts any number of arguments and returns
VALUE."
  (lambda _
    value))

(define (and=> value procedure) (and value (procedure value)))
(define call/cc call-with-current-continuation)

(define-syntax false-if-exception
  (syntax-rules ()
    ((_ expr)
     (catch #t
       (lambda () expr)
       (lambda (k . args) #f)))))



;;; {General Properties}
;;;

;; Properties are a lispy way to associate random info with random objects.
;; Traditionally properties are implemented as an alist or a plist actually
;; pertaining to the object in question.
;;
;; These "object properties" have the advantage that they can be associated with
;; any object, even if the object has no plist. Object properties are good when
;; you are extending pre-existing objects in unexpected ways. They also present
;; a pleasing, uniform procedure-with-setter interface. But if you have a data
;; type that always has properties, it's often still best to store those
;; properties within the object itself.

(define (make-object-property)
  (define-syntax with-mutex
    (syntax-rules ()
      ((_ lock exp)
       (dynamic-wind (lambda () (lock-mutex lock))
                     (lambda () exp)
                     (lambda () (unlock-mutex lock))))))
  (let ((prop (make-weak-key-hash-table))
        (lock (make-mutex)))
    (make-procedure-with-setter
     (lambda (obj) (with-mutex lock (hashq-ref prop obj)))
     (lambda (obj val) (with-mutex lock (hashq-set! prop obj val))))))




;;; {Symbol Properties}
;;;

;;; Symbol properties are something you see in old Lisp code. In most current
;;; Guile code, symbols are not used as a data structure -- they are used as
;;; keys into other data structures.

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

(define (array-shape a)
  (map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
       (array-dimensions a)))



;;; {Keywords}
;;;

;;; It's much better if you can use lambda* / define*, of course.

(define (kw-arg-ref args kw)
  (let ((rem (member kw args)))
    (and rem (pair? (cdr rem)) (cadr rem))))



;;; {Structs}
;;;

(define (struct-layout s)
  (struct-ref (struct-vtable s) vtable-index-layout))



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

(define (inherit-print-state old-port new-port)
  (if (get-print-state old-port)
      (port-with-print-state new-port (get-print-state old-port))
      new-port))

;; 0: type-name, 1: fields, 2: constructor
(define record-type-vtable
  ;; FIXME: This should just call make-vtable, not make-vtable-vtable; but for
  ;; that we need to expose the bare vtable-vtable to Scheme.
  (make-vtable-vtable "prprpw" 0
                      (lambda (s p)
                        (cond ((eq? s record-type-vtable)
                               (display "#<record-type-vtable>" p))
                              (else
                               (display "#<record-type " p)
                               (display (record-type-name s) p)
                               (display ">" p))))))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

(define* (make-record-type type-name fields #:optional printer)
  ;; Pre-generate constructors for nfields < 20.
  (define-syntax make-constructor
    (lambda (x)
      (define *max-static-argument-count* 20)
      (define (make-formals n)
        (let lp ((i 0))
          (if (< i n)
              (cons (datum->syntax
                     x 
                     (string->symbol
                      (string (integer->char (+ (char->integer #\a) i)))))
                    (lp (1+ i)))
              '())))
      (syntax-case x ()
        ((_ rtd exp) (not (identifier? #'exp))
         #'(let ((n exp))
             (make-constructor rtd n)))
        ((_ rtd nfields)
         #`(case nfields
             #,@(let lp ((n 0))
                  (if (< n *max-static-argument-count*)
                      (cons (with-syntax (((formal ...) (make-formals n))
                                          (n n))
                              #'((n)
                                 (lambda (formal ...)
                                   (make-struct rtd 0 formal ...))))
                            (lp (1+ n)))
                      '()))
             (else
              (lambda args
                (if (= (length args) nfields)
                    (apply make-struct rtd 0 args)
                    (scm-error 'wrong-number-of-args
                               (format #f "make-~a" type-name)
                               "Wrong number of arguments" '() #f)))))))))

  (define (default-record-printer s p)
    (display "#<" p)
    (display (record-type-name (record-type-descriptor s)) p)
    (let loop ((fields (record-type-fields (record-type-descriptor s)))
               (off 0))
      (cond
       ((not (null? fields))
        (display " " p)
        (display (car fields) p)
        (display ": " p)
        (display (struct-ref s off) p)
        (loop (cdr fields) (+ 1 off)))))
    (display ">" p))

  (let ((rtd (make-struct record-type-vtable 0
                          (make-struct-layout
                           (apply string-append
                                  (map (lambda (f) "pw") fields)))
                          (or printer default-record-printer)
                          type-name
                          (copy-tree fields))))
    (struct-set! rtd (+ vtable-offset-user 2)
                 (make-constructor rtd (length fields)))
    ;; Temporary solution: Associate a name to the record type descriptor
    ;; so that the object system can create a wrapper class for it.
    (set-struct-vtable-name! rtd (if (symbol? type-name)
                                     type-name
                                     (string->symbol type-name)))
    rtd))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-record-type obj)))

(define* (record-constructor rtd #:optional field-names)
  (if (not field-names)
      (struct-ref rtd (+ 2 vtable-offset-user))
      (primitive-eval
       `(lambda ,field-names
          (make-struct ',rtd 0 ,@(map (lambda (f)
                                        (if (memq f field-names)
                                            f
                                            #f))
                                      (record-type-fields rtd)))))))
          
(define (record-predicate rtd)
  (lambda (obj) (and (struct? obj) (eq? rtd (struct-vtable obj)))))

(define (%record-type-error rtd obj)  ;; private helper
  (or (eq? rtd (record-type-descriptor obj))
      (scm-error 'wrong-type-arg "%record-type-check"
                 "Wrong type record (want `~S'): ~S"
                 (list (record-type-name rtd) obj)
                 #f)))

(define (record-accessor rtd field-name)
  (let ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
        (error 'no-such-field field-name))
    (lambda (obj)
      (if (eq? (struct-vtable obj) rtd)
          (struct-ref obj pos)
          (%record-type-error rtd obj)))))

(define (record-modifier rtd field-name)
  (let ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
        (error 'no-such-field field-name))
    (lambda (obj val)
      (if (eq? (struct-vtable obj) rtd)
          (struct-set! obj pos val)
          (%record-type-error rtd obj)))))

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
  (string->symbol (apply string-append (map symbol->string args))))

(define (list->symbol . args)
  (string->symbol (apply list->string args)))

(define (symbol . args)
  (string->symbol (apply string args)))



;;; {Lists}
;;;

(define (list-index l k)
  (let loop ((n 0)
             (l l))
    (and (not (null? l))
         (if (eq? (car l) k)
             n
             (loop (+ n 1) (cdr l))))))



;; Load `posix.scm' even when not (provided? 'posix) so that we get the
;; `stat' accessors.
(primitive-load-path "ice-9/posix")

(if (provided? 'socket)
    (primitive-load-path "ice-9/networking"))

;; For reference, Emacs file-exists-p uses stat in this same way.
(define file-exists?
  (if (provided? 'posix)
      (lambda (str)
        (->bool (stat str #f)))
      (lambda (str)
        (let ((port (catch 'system-error (lambda () (open-file str OPEN_READ))
                           (lambda args #f))))
          (if port (begin (close-port port) #t)
              #f)))))

(define file-is-directory?
  (if (provided? 'posix)
      (lambda (str)
        (eq? (stat:type (stat str)) 'directory))
      (lambda (str)
        (let ((port (catch 'system-error
                           (lambda () (open-file (string-append str "/.")
                                                 OPEN_READ))
                           (lambda args #f))))
          (if port (begin (close-port port) #t)
              #f)))))

(define (system-error-errno args)
  (if (eq? (car args) 'system-error)
      (car (list-ref args 4))
      #f))



;;; {Error Handling}
;;;

(define error
  (case-lambda
    (()
     (scm-error 'misc-error #f "?" #f #f))
    ((message . args)
     (let ((msg (string-join (cons "~A" (make-list (length args) "~S")))))
       (scm-error 'misc-error #f msg (cons message args) #f)))))



;;; {Time Structures}
;;;

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

(define (tms:clock obj) (vector-ref obj 0))
(define (tms:utime obj) (vector-ref obj 1))
(define (tms:stime obj) (vector-ref obj 2))
(define (tms:cutime obj) (vector-ref obj 3))
(define (tms:cstime obj) (vector-ref obj 4))



;;; {File Descriptors and Ports}
;;;

(define file-position ftell)
(define* (file-set-position port offset #:optional (whence SEEK_SET))
  (seek port offset whence))

(define (move->fdes fd/port fd)
  (cond ((integer? fd/port)
         (dup->fdes fd/port fd)
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

(define dup->port
  (case-lambda
    ((port/fd mode)
     (fdopen (dup->fdes port/fd) mode))
    ((port/fd mode new-fd)
     (let ((port (fdopen (dup->fdes port/fd new-fd) mode)))
       (set-port-revealed! port 1)
       port))))

(define dup->inport
  (case-lambda
    ((port/fd)
     (dup->port port/fd "r"))
    ((port/fd new-fd)
     (dup->port port/fd "r" new-fd))))

(define dup->outport
  (case-lambda
    ((port/fd)
     (dup->port port/fd "w"))
    ((port/fd new-fd)
     (dup->port port/fd "w" new-fd))))

(define dup
  (case-lambda
    ((port/fd)
     (if (integer? port/fd)
         (dup->fdes port/fd)
         (dup->port port/fd (port-mode port/fd))))
    ((port/fd new-fd)
     (if (integer? port/fd)
         (dup->fdes port/fd new-fd)
         (dup->port port/fd (port-mode port/fd) new-fd)))))

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

(define (unsetenv name)
  "Remove the entry for NAME from the environment."
  (putenv name))



;;; {Load Paths}
;;;

(define (in-vicinity vicinity file)
  (let ((tail (let ((len (string-length vicinity)))
                (if (zero? len)
                    #f
                    (string-ref vicinity (- len 1))))))
    (string-append vicinity
                   (if (or (not tail)
                           (eq? tail #\/))
                       ""
                       "/")
                   file)))



;;; {Help for scm_shell}
;;;
;;; The argument-processing code used by Guile-based shells generates
;;; Scheme code based on the argument list.  This page contains help
;;; functions for the code it generates.
;;;

(define (command-line) (program-arguments))

;; This is mostly for the internal use of the code generated by
;; scm_compile_shell_switches.

(define (load-user-init)
  (let* ((home (or (getenv "HOME")
                   (false-if-exception (passwd:dir (getpwuid (getuid))))
                   "/"))  ;; fallback for cygwin etc.
         (init-file (in-vicinity home ".guile")))
    (if (file-exists? init-file)
        (primitive-load init-file))))



;;; {The interpreter stack}
;;;

;; %stacks defined in stacks.c
(define (%start-stack tag thunk)
  (let ((prompt-tag (make-prompt-tag "start-stack")))
    (call-with-prompt
     prompt-tag
     (lambda ()
       (with-fluids ((%stacks (acons tag prompt-tag
                                     (or (fluid-ref %stacks) '()))))
         (thunk)))
     (lambda (k . args)
       (%start-stack tag (lambda () (apply k args)))))))
(define-syntax start-stack
  (syntax-rules ()
    ((_ tag exp)
     (%start-stack tag (lambda () exp)))))



;;; {Loading by paths}
;;;

;;; Load a Scheme source file named NAME, searching for it in the
;;; directories listed in %load-path, and applying each of the file
;;; name extensions listed in %load-extensions.
(define (load-from-path name)
  (start-stack 'load-stack
               (primitive-load-path name)))

(define %load-verbosely #f)
(define (assert-load-verbosity v) (set! %load-verbosely v))

(define (%load-announce file)
  (if %load-verbosely
      (with-output-to-port (current-error-port)
        (lambda ()
          (display ";;; ")
          (display "loading ")
          (display file)
          (newline)
          (force-output)))))

(set! %load-hook %load-announce)



;;; {Reader Extensions}
;;;
;;; Reader code for various "#c" forms.
;;;

(define read-eval? (make-fluid))
(fluid-set! read-eval? #f)
(read-hash-extend #\.
                  (lambda (c port)
                    (if (fluid-ref read-eval?)
                        (eval (read port) (interaction-environment))
                        (error
                         "#. read expansion found and read-eval? is #f."))))



;;; {Low Level Modules}
;;;
;;; These are the low level data structures for modules.
;;;
;;; Every module object is of the type 'module-type', which is a record
;;; consisting of the following members:
;;;
;;; - eval-closure: the function that defines for its module the strategy that
;;;   shall be followed when looking up symbols in the module.
;;;
;;;   An eval-closure is a function taking two arguments: the symbol to be
;;;   looked up and a boolean value telling whether a binding for the symbol
;;;   should be created if it does not exist yet.  If the symbol lookup
;;;   succeeded (either because an existing binding was found or because a new
;;;   binding was created), a variable object representing the binding is
;;;   returned.  Otherwise, the value #f is returned.  Note that the eval
;;;   closure does not take the module to be searched as an argument: During
;;;   construction of the eval-closure, the eval-closure has to store the
;;;   module it belongs to in its environment.  This means, that any
;;;   eval-closure can belong to only one module.
;;;
;;;   The eval-closure of a module can be defined arbitrarily.  However, three
;;;   special cases of eval-closures are to be distinguished: During startup
;;;   the module system is not yet activated.  In this phase, no modules are
;;;   defined and all bindings are automatically stored by the system in the
;;;   pre-modules-obarray.  Since no eval-closures exist at this time, the
;;;   functions which require an eval-closure as their argument need to be
;;;   passed the value #f.
;;;
;;;   The other two special cases of eval-closures are the
;;;   standard-eval-closure and the standard-interface-eval-closure.  Both
;;;   behave equally for the case that no new binding is to be created.  The
;;;   difference between the two comes in, when the boolean argument to the
;;;   eval-closure indicates that a new binding shall be created if it is not
;;;   found.
;;;
;;;   Given that no new binding shall be created, both standard eval-closures
;;;   define the following standard strategy of searching bindings in the
;;;   module: First, the module's obarray is searched for the symbol.  Second,
;;;   if no binding for the symbol was found in the module's obarray, the
;;;   module's binder procedure is exececuted.  If this procedure did not
;;;   return a binding for the symbol, the modules referenced in the module's
;;;   uses list are recursively searched for a binding of the symbol.  If the
;;;   binding can not be found in these modules also, the symbol lookup has
;;;   failed.
;;;
;;;   If a new binding shall be created, the standard-interface-eval-closure
;;;   immediately returns indicating failure.  That is, it does not even try
;;;   to look up the symbol.  In contrast, the standard-eval-closure would
;;;   first search the obarray, and if no binding was found there, would
;;;   create a new binding in the obarray, therefore not calling the binder
;;;   procedure or searching the modules in the uses list.
;;;
;;;   The explanation of the following members obarray, binder and uses
;;;   assumes that the symbol lookup follows the strategy that is defined in
;;;   the standard-eval-closure and the standard-interface-eval-closure.
;;;
;;; - obarray: a hash table that maps symbols to variable objects.  In this
;;;   hash table, the definitions are found that are local to the module (that
;;;   is, not imported from other modules).  When looking up bindings in the
;;;   module, this hash table is searched first.
;;;
;;; - binder: either #f or a function taking a module and a symbol argument.
;;;   If it is a function it is called after the obarray has been
;;;   unsuccessfully searched for a binding.  It then can provide bindings
;;;   that would otherwise not be found locally in the module.
;;;
;;; - uses: a list of modules from which non-local bindings can be inherited.
;;;   These modules are the third place queried for bindings after the obarray
;;;   has been unsuccessfully searched and the binder function did not deliver
;;;   a result either.
;;;
;;; - transformer: either #f or a function taking a scheme expression as
;;;   delivered by read.  If it is a function, it will be called to perform
;;;   syntax transformations (e. g. makro expansion) on the given scheme
;;;   expression. The output of the transformer function will then be passed
;;;   to Guile's internal memoizer.  This means that the output must be valid
;;;   scheme code.  The only exception is, that the output may make use of the
;;;   syntax extensions provided to identify the modules that a binding
;;;   belongs to.
;;;
;;; - name: the name of the module.  This is used for all kinds of printing
;;;   outputs.  In certain places the module name also serves as a way of
;;;   identification.  When adding a module to the uses list of another
;;;   module, it is made sure that the new uses list will not contain two
;;;   modules of the same name.
;;;
;;; - kind: classification of the kind of module.  The value is (currently?)
;;;   only used for printing.  It has no influence on how a module is treated.
;;;   Currently the following values are used when setting the module kind:
;;;   'module, 'directory, 'interface, 'custom-interface.  If no explicit kind
;;;   is set, it defaults to 'module.
;;;
;;; - duplicates-handlers: a list of procedures that get called to make a
;;;   choice between two duplicate bindings when name clashes occur.  See the
;;;   `duplicate-handlers' global variable below.
;;;
;;; - observers: a list of procedures that get called when the module is
;;;   modified.
;;;
;;; - weak-observers: a weak-key hash table of procedures that get called
;;;   when the module is modified.  See `module-observe-weak' for details.
;;;
;;; In addition, the module may (must?) contain a binding for
;;; `%module-public-interface'.  This variable should be bound to a module
;;; representing the exported interface of a module.  See the
;;; `module-public-interface' and `module-export!' procedures.
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
;;;             => [ <obj> | opt-value | an error occurs ]
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
;;;

;; This is how modules are printed.  You can re-define it.
(define (%print-module mod port)
  (display "#<" port)
  (display (or (module-kind mod) "module") port)
  (display " " port)
  (display (module-name mod) port)
  (display " " port)
  (display (number->string (object-address mod) 16) port)
  (display ">" port))

(letrec-syntax
     ;; Locally extend the syntax to allow record accessors to be defined at
     ;; compile-time. Cache the rtd locally to the constructor, the getters and
     ;; the setters, in order to allow for redefinition of the record type; not
     ;; relevant in the case of modules, but perhaps if we make this public, it
     ;; could matter.

    ((define-record-type
       (lambda (x)
         (define (make-id scope . fragments)
           (datum->syntax #'scope
                          (apply symbol-append
                                 (map (lambda (x)
                                        (if (symbol? x) x (syntax->datum x)))
                                      fragments))))
         
         (define (getter rtd type-name field slot)
           #`(define #,(make-id rtd type-name '- field)
               (let ((rtd #,rtd))
                 (lambda (#,type-name)
                   (if (eq? (struct-vtable #,type-name) rtd)
                       (struct-ref #,type-name #,slot)
                       (%record-type-error rtd #,type-name))))))

         (define (setter rtd type-name field slot)
           #`(define #,(make-id rtd 'set- type-name '- field '!)
               (let ((rtd #,rtd))
                 (lambda (#,type-name val)
                   (if (eq? (struct-vtable #,type-name) rtd)
                       (struct-set! #,type-name #,slot val)
                       (%record-type-error rtd #,type-name))))))

         (define (accessors rtd type-name fields n exp)
           (syntax-case fields ()
             (() exp)
             (((field #:no-accessors) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         exp))
             (((field #:no-setter) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(getter rtd type-name #'field n))))
             (((field #:no-getter) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(setter rtd type-name #'field n))))
             ((field field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(getter rtd type-name #'field n)
                                  #,(setter rtd type-name #'field n))))))

         (define (predicate rtd type-name fields exp)
           (accessors
            rtd type-name fields 0
            #`(begin
                #,exp
                (define (#,(make-id rtd type-name '?) obj)
                  (and (struct? obj) (eq? (struct-vtable obj) #,rtd))))))

         (define (field-list fields)
           (syntax-case fields ()
             (() '())
             (((f . opts) . rest) (identifier? #'f)
              (cons #'f (field-list #'rest)))
             ((f . rest) (identifier? #'f)
              (cons #'f (field-list #'rest)))))

         (define (constructor rtd type-name fields exp)
           (let ((ctor (make-id rtd type-name '-constructor))
                 (args (field-list fields)))
             (predicate rtd type-name fields
                        #`(begin #,exp
                                 (define #,ctor
                                   (let ((rtd #,rtd))
                                     (lambda #,args
                                       (make-struct rtd 0 #,@args))))
                                 (struct-set! #,rtd (+ vtable-offset-user 2)
                                              #,ctor)))))

         (define (type type-name printer fields)
           (define (make-layout)
             (let lp ((fields fields) (slots '()))
               (syntax-case fields ()
                 (() (datum->syntax #'here
                                    (make-struct-layout
                                     (apply string-append slots))))
                 ((_ . rest) (lp #'rest (cons "pw" slots))))))

           (let ((rtd (make-id type-name type-name '-type)))
             (constructor rtd type-name fields
                          #`(begin
                              (define #,rtd
                                (make-struct record-type-vtable 0
                                             '#,(make-layout)
                                             #,printer
                                             '#,type-name
                                             '#,(field-list fields)))
                              (set-struct-vtable-name! #,rtd '#,type-name)))))

         (syntax-case x ()
           ((_ type-name printer (field ...))
            (type #'type-name #'printer #'(field ...)))))))

  ;; module-type
  ;;
  ;; A module is characterized by an obarray in which local symbols
  ;; are interned, a list of modules, "uses", from which non-local
  ;; bindings can be inherited, and an optional lazy-binder which
  ;; is a (CLOSURE module symbol) which, as a last resort, can provide
  ;; bindings that would otherwise not be found locally in the module.
  ;;
  ;; NOTE: If you change the set of fields or their order, you also need to
  ;; change the constants in libguile/modules.h.
  ;;
  ;; NOTE: The getter `module-eval-closure' is used in libguile/modules.c.
  ;; NOTE: The getter `module-transfomer' is defined libguile/modules.c.
  ;; NOTE: The getter `module-name' is defined later, due to boot reasons.
  ;; NOTE: The getter `module-public-interface' is used in libguile/modules.c.
  ;;
  (define-record-type module
    (lambda (obj port) (%print-module obj port))
    (obarray
     uses
     binder
     eval-closure
     (transformer #:no-getter)
     (name #:no-getter)
     kind
     duplicates-handlers
     (import-obarray #:no-setter)
     observers
     (weak-observers #:no-setter)
     version
     submodules
     submodule-binder
     public-interface
     filename)))


;; make-module &opt size uses binder
;;
;; Create a new module, perhaps with a particular size of obarray,
;; initial uses list, or binding procedure.
;;
(define* (make-module #:optional (size 31) (uses '()) (binder #f))
  (define %default-import-size
    ;; Typical number of imported bindings actually used by a module.
    600)

  (if (not (integer? size))
      (error "Illegal size to make-module." size))
  (if (not (and (list? uses)
                (and-map module? uses)))
      (error "Incorrect use list." uses))
  (if (and binder (not (procedure? binder)))
      (error
       "Lazy-binder expected to be a procedure or #f." binder))

  (let ((module (module-constructor (make-hash-table size)
                                    uses binder #f macroexpand
                                    #f #f #f
                                    (make-hash-table %default-import-size)
                                    '()
                                    (make-weak-key-hash-table 31) #f
                                    (make-hash-table 7) #f #f #f)))

    ;; We can't pass this as an argument to module-constructor,
    ;; because we need it to close over a pointer to the module
    ;; itself.
    (set-module-eval-closure! module (standard-eval-closure module))

    module))




;;; {Observer protocol}
;;;

(define (module-observe module proc)
  (set-module-observers! module (cons proc (module-observers module)))
  (cons module proc))

(define* (module-observe-weak module observer-id #:optional (proc observer-id))
  ;; Register PROC as an observer of MODULE under name OBSERVER-ID (which can
  ;; be any Scheme object).  PROC is invoked and passed MODULE any time
  ;; MODULE is modified.  PROC gets unregistered when OBSERVER-ID gets GC'd
  ;; (thus, it is never unregistered if OBSERVER-ID is an immediate value,
  ;; for instance).

  ;; The two-argument version is kept for backward compatibility: when called
  ;; with two arguments, the observer gets unregistered when closure PROC
  ;; gets GC'd (making it impossible to use an anonymous lambda for PROC).
  (hashq-set! (module-weak-observers module) observer-id proc))

(define (module-unobserve token)
  (let ((module (car token))
        (id (cdr token)))
    (if (integer? id)
        (hash-remove! (module-weak-observers module) id)
        (set-module-observers! module (delq1! id (module-observers module)))))
  *unspecified*)

(define module-defer-observers #f)
(define module-defer-observers-mutex (make-mutex 'recursive))
(define module-defer-observers-table (make-hash-table))

(define (module-modified m)
  (if module-defer-observers
      (hash-set! module-defer-observers-table m #t)
      (module-call-observers m)))

;;; This function can be used to delay calls to observers so that they
;;; can be called once only in the face of massive updating of modules.
;;;
(define (call-with-deferred-observers thunk)
  (dynamic-wind
      (lambda ()
        (lock-mutex module-defer-observers-mutex)
        (set! module-defer-observers #t))
      thunk
      (lambda ()
        (set! module-defer-observers #f)
        (hash-for-each (lambda (m dummy)
                         (module-call-observers m))
                       module-defer-observers-table)
        (hash-clear! module-defer-observers-table)
        (unlock-mutex module-defer-observers-mutex))))

(define (module-call-observers m)
  (for-each (lambda (proc) (proc m)) (module-observers m))

  ;; We assume that weak observers don't (un)register themselves as they are
  ;; called since this would preclude proper iteration over the hash table
  ;; elements.
  (hash-for-each (lambda (id proc) (proc m)) (module-weak-observers m)))



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
  (let ((var (module-variable m v)))
    (and var
         (variable-bound? var))))

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
;;; (This is now written in C, see `modules.c'.)
;;;

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
    (if (and var (variable-bound? var))
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
    (if (and var (variable-bound? var))
        (variable-ref var)
        (if (not (null? opt-val))
            (car opt-val)
            (error "Unbound variable." v)))))




;;; {Adding Variables to Modules}
;;;

;; module-make-local-var! module symbol
;;
;; ensure a variable for V in the local namespace of M.
;; If no variable was already there, then create a new and uninitialzied
;; variable.
;;
;; This function is used in modules.c.
;;
(define (module-make-local-var! m v)
  (or (let ((b (module-obarray-ref (module-obarray m) v)))
        (and (variable? b)
             (begin
               ;; Mark as modified since this function is called when
               ;; the standard eval closure defines a binding
               (module-modified m)
               b)))

      ;; Create a new local variable.
      (let ((local-var (make-undefined-variable)))
        (module-add! m v local-var)
        local-var)))

;; module-ensure-local-variable! module symbol
;;
;; Ensure that there is a local variable in MODULE for SYMBOL.  If
;; there is no binding for SYMBOL, create a new uninitialized
;; variable.  Return the local variable.
;;
(define (module-ensure-local-variable! module symbol)
  (or (module-local-variable module symbol)
      (let ((var (make-undefined-variable)))
        (module-add! module symbol var)
        var)))

;; module-add! module symbol var
;;
;; ensure a particular variable for V in the local namespace of M.
;;
(define (module-add! m v var)
  (if (not (variable? var))
      (error "Bad variable to module-add!" var))
  (module-obarray-set! (module-obarray m) v var)
  (module-modified m))

;; module-remove!
;;
;; make sure that a symbol is undefined in the local namespace of M.
;;
(define (module-remove! m v)
  (module-obarray-remove! (module-obarray m) v)
  (module-modified m))

(define (module-clear! m)
  (hash-clear! (module-obarray m))
  (module-modified m))

;; MODULE-FOR-EACH -- exported
;;
;; Call PROC on each symbol in MODULE, with arguments of (SYMBOL VARIABLE).
;;
(define (module-for-each proc module)
  (hash-for-each proc (module-obarray module)))

(define (module-map proc module)
  (hash-map->list proc (module-obarray module)))

;; Submodules
;;
;; Modules exist in a separate namespace from values, because you generally do
;; not want the name of a submodule, which you might not even use, to collide
;; with local variables that happen to be named the same as the submodule.
;;
(define (module-ref-submodule module name)
  (or (hashq-ref (module-submodules module) name)
      (and (module-submodule-binder module)
           ((module-submodule-binder module) module name))))

(define (module-define-submodule! module name submodule)
  (hashq-set! (module-submodules module) name submodule))

;; It used to be, however, that module names were also present in the
;; value namespace. When we enable deprecated code, we preserve this
;; legacy behavior.
;;
;; These shims are defined here instead of in deprecated.scm because we
;; need their definitions before loading other modules.
;;
(begin-deprecated
 (define (module-ref-submodule module name)
   (or (hashq-ref (module-submodules module) name)
       (and (module-submodule-binder module)
            ((module-submodule-binder module) module name))
       (let ((var (module-local-variable module name)))
         (and var (variable-bound? var) (module? (variable-ref var))
              (begin
                (warn "module" module "not in submodules table")
                (variable-ref var))))))

 (define (module-define-submodule! module name submodule)
   (let ((var (module-local-variable module name)))
     (if (and var
              (or (not (variable-bound? var))
                  (not (module? (variable-ref var)))))
         (warn "defining module" module ": not overriding local definition" var)
         (module-define! module name submodule)))
   (hashq-set! (module-submodules module) name submodule)))



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



;;; {MODULE-REF -- exported}
;;;

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
            (car rest)                  ; default value
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
        (begin
          (variable-set! variable value)
          (module-modified module))
        (let ((variable (make-variable value)))
          (module-add! module name variable)))))

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
  (if (not (or (eq? module interface)
               (memq interface (module-uses module))))
      (begin
        ;; Newly used modules must be appended rather than consed, so that
        ;; `module-variable' traverses the use list starting from the first
        ;; used module.
        (set-module-uses! module (append (module-uses module)
                                         (list interface)))
        (hash-clear! (module-import-obarray module))
        (module-modified module))))

;; MODULE-USE-INTERFACES! module interfaces
;;
;; Same as MODULE-USE!, but only notifies module observers after all
;; interfaces are added to the inports list.
;;
(define (module-use-interfaces! module interfaces)
  (let* ((cur (module-uses module))
         (new (let lp ((in interfaces) (out '()))
                (if (null? in)
                    (reverse out)
                    (lp (cdr in)
                        (let ((iface (car in)))
                          (if (or (memq iface cur) (memq iface out))
                              out
                              (cons iface out))))))))
    (set-module-uses! module (append cur new))
    (hash-clear! (module-import-obarray module))
    (module-modified module)))



;;; {Recursive Namespaces}
;;;
;;; A hierarchical namespace emerges if we consider some module to be
;;; root, and submodules of that module to be nested namespaces.
;;;
;;; The routines here manage variable names in hierarchical namespace.
;;; Each variable name is a list of elements, looked up in successively nested
;;; modules.
;;;
;;;             (nested-ref some-root-module '(foo bar baz))
;;;             => <value of a variable named baz in the submodule bar of
;;;                 the submodule foo of some-root-module>
;;;
;;;
;;; There are:
;;;
;;;     ;; a-root is a module
;;;     ;; name is a list of symbols
;;;
;;;     nested-ref a-root name
;;;     nested-set! a-root name val
;;;     nested-define! a-root name val
;;;     nested-remove! a-root name
;;;
;;; These functions manipulate values in namespaces. For referencing the
;;; namespaces themselves, use the following:
;;;
;;;     nested-ref-module a-root name
;;;     nested-define-module! a-root name mod
;;;
;;; (current-module) is a natural choice for a root so for convenience there are
;;; also:
;;;
;;;     local-ref name                ==  nested-ref (current-module) name
;;;     local-set! name val           ==  nested-set! (current-module) name val
;;;     local-define name val         ==  nested-define! (current-module) name val
;;;     local-remove name             ==  nested-remove! (current-module) name
;;;     local-ref-module name         ==  nested-ref-module (current-module) name
;;;     local-define-module! name m   ==  nested-define-module! (current-module) name m
;;;


(define (nested-ref root names)
  (if (null? names)
      root
      (let loop ((cur root)
                 (head (car names))
                 (tail (cdr names)))
        (if (null? tail)
            (module-ref cur head #f)
            (let ((cur (module-ref-submodule cur head)))
              (and cur
                   (loop cur (car tail) (cdr tail))))))))

(define (nested-set! root names val)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-set! cur head val)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (error "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))

(define (nested-define! root names val)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-define! cur head val)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (error "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))

(define (nested-remove! root names)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-remove! cur head)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (error "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))


(define (nested-ref-module root names)
  (let loop ((cur root)
             (names names))
    (if (null? names)
        cur
        (let ((cur (module-ref-submodule cur (car names))))
          (and cur
               (loop cur (cdr names)))))))

(define (nested-define-module! root names module)
  (if (null? names)
      (error "can't redefine root module" root module)
      (let loop ((cur root)
                 (head (car names))
                 (tail (cdr names)))
        (if (null? tail)
            (module-define-submodule! cur head module)
            (let ((cur (or (module-ref-submodule cur head)
                           (let ((m (make-module 31)))
                             (set-module-kind! m 'directory)
                             (set-module-name! m (append (module-name cur)
                                                         (list head)))
                             (module-define-submodule! cur head m)
                             m))))
              (loop cur (car tail) (cdr tail)))))))


(define (local-ref names)
  (nested-ref (current-module) names))

(define (local-set! names val)
  (nested-set! (current-module) names val))

(define (local-define names val)
  (nested-define! (current-module) names val))

(define (local-remove names)
  (nested-remove! (current-module) names))

(define (local-ref-module names)
  (nested-ref-module (current-module) names))

(define (local-define-module names mod)
  (nested-define-module! (current-module) names mod))





;;; {The (guile) module}
;;;
;;; The standard module, which has the core Guile bindings. Also called the
;;; "root module", as it is imported by many other modules, but it is not
;;; necessarily the root of anything; and indeed, the module named '() might be
;;; better thought of as a root.
;;;

(define (set-system-module! m s)
  (set-procedure-property! (module-eval-closure m) 'system-module s))

;; The root module uses the pre-modules-obarray as its obarray.  This
;; special obarray accumulates all bindings that have been established
;; before the module system is fully booted.
;;
;; (The obarray continues to be used by code that has been closed over
;;  before the module system has been booted.)
;;
(define the-root-module
  (let ((m (make-module 0)))
    (set-module-obarray! m (%get-pre-modules-obarray))
    (set-module-name! m '(guile))
    (set-system-module! m #t)
    m))

;; The root interface is a module that uses the same obarray as the
;; root module.  It does not allow new definitions, tho.
;;
(define the-scm-module
  (let ((m (make-module 0)))
    (set-module-obarray! m (%get-pre-modules-obarray))
    (set-module-eval-closure! m (standard-interface-eval-closure m))
    (set-module-name! m '(guile))
    (set-module-kind! m 'interface)
    (set-system-module! m #t)

    ;; In Guile 1.8 and earlier M was its own public interface.
    (set-module-public-interface! m m)

    m))

(set-module-public-interface! the-root-module the-scm-module)



;; Now that we have a root module, even though modules aren't fully booted,
;; expand the definition of resolve-module.
;;
(define (resolve-module name . args)
  (if (equal? name '(guile))
      the-root-module
      (error "unexpected module to resolve during module boot" name)))

;; Cheat.  These bindings are needed by modules.c, but we don't want
;; to move their real definition here because that would be unnatural.
;;
(define define-module* #f)
(define process-use-modules #f)
(define module-export! #f)
(define default-duplicate-binding-procedures #f)

;; This boots the module system.  All bindings needed by modules.c
;; must have been defined by now.
;;
(set-current-module the-root-module)




;; Now that modules are booted, give module-name its final definition.
;;
(define module-name
  (let ((accessor (record-accessor module-type 'name)))
    (lambda (mod)
      (or (accessor mod)
          (let ((name (list (gensym))))
            ;; Name MOD and bind it in the module root so that it's visible to
            ;; `resolve-module'. This is important as `psyntax' stores module
            ;; names and relies on being able to `resolve-module' them.
            (set-module-name! mod name)
            (nested-define-module! (resolve-module '() #f) name mod)
            (accessor mod))))))

(define (make-modules-in module name)
  (or (nested-ref-module module name)
      (let ((m (make-module 31)))
        (set-module-kind! m 'directory)
        (set-module-name! m (append (module-name module) name))
        (nested-define-module! module name m)
        m)))

(define (beautify-user-module! module)
  (let ((interface (module-public-interface module)))
    (if (or (not interface)
            (eq? interface module))
        (let ((interface (make-module 31)))
          (set-module-name! interface (module-name module))
          (set-module-version! interface (module-version module))
          (set-module-kind! interface 'interface)
          (set-module-public-interface! module interface))))
  (if (and (not (memq the-scm-module (module-uses module)))
           (not (eq? module the-root-module)))
      ;; Import the default set of bindings (from the SCM module) in MODULE.
      (module-use! module the-scm-module)))

(define (version-matches? version-ref target)
  (define (sub-versions-match? v-refs t)
    (define (sub-version-matches? v-ref t)
      (let ((matches? (lambda (v) (sub-version-matches? v t))))
        (cond
         ((number? v-ref) (eqv? v-ref t))
         ((list? v-ref)
          (case (car v-ref)
            ((>=)  (>= t (cadr v-ref)))
            ((<=)  (<= t (cadr v-ref)))
            ((and) (and-map matches? (cdr v-ref)))
            ((or)  (or-map matches? (cdr v-ref)))
            ((not) (not (matches? (cadr v-ref))))
            (else (error "Invalid sub-version reference" v-ref))))
         (else (error "Invalid sub-version reference" v-ref)))))
    (or (null? v-refs)
        (and (not (null? t))
             (sub-version-matches? (car v-refs) (car t))
             (sub-versions-match? (cdr v-refs) (cdr t)))))

  (let ((matches? (lambda (v) (version-matches? v target))))
    (or (null? version-ref)
        (case (car version-ref)
          ((and) (and-map matches? (cdr version-ref)))
          ((or)  (or-map matches? (cdr version-ref)))
          ((not) (not (matches? (cadr version-ref))))
          (else  (sub-versions-match? version-ref target))))))

(define (make-fresh-user-module)
  (let ((m (make-module)))
    (beautify-user-module! m)
    m))

;; NOTE: This binding is used in libguile/modules.c.
;;
(define resolve-module
  (let ((root (make-module)))
    (set-module-name! root '())
    ;; Define the-root-module as '(guile).
    (module-define-submodule! root 'guile the-root-module)

    (lambda* (name #:optional (autoload #t) (version #f) #:key (ensure #t))
      (let ((already (nested-ref-module root name)))
        (cond
         ((and already
               (or (not autoload) (module-public-interface already)))
          ;; A hit, a palpable hit.
          (if (and version
                   (not (version-matches? version (module-version already))))
              (error "incompatible module version already loaded" name))
          already)
         (autoload
          ;; Try to autoload the module, and recurse.
          (try-load-module name version)
          (resolve-module name #f #:ensure ensure))
         (else
          ;; No module found (or if one was, it had no public interface), and
          ;; we're not autoloading. Make an empty module if #:ensure is true.
          (or already
              (and ensure
                   (make-modules-in root name)))))))))


(define (try-load-module name version)
  (try-module-autoload name version))

(define (reload-module m)
  "Revisit the source file corresponding to the module @var{m}."
  (let ((f (module-filename m)))
    (if f
        (save-module-excursion
         (lambda () 
           ;; Re-set the initial environment, as in try-module-autoload.
           (set-current-module (make-fresh-user-module))
           (primitive-load-path f)
           m))
        ;; Though we could guess, we *should* know it.
        (error "unknown file name for module" m))))

(define (purify-module! module)
  "Removes bindings in MODULE which are inherited from the (guile) module."
  (let ((use-list (module-uses module)))
    (if (and (pair? use-list)
             (eq? (car (last-pair use-list)) the-scm-module))
        (set-module-uses! module (reverse (cdr (reverse use-list)))))))

;; Return a module that is an interface to the module designated by
;; NAME.
;;
;; `resolve-interface' takes four keyword arguments:
;;
;;   #:select SELECTION
;;
;; SELECTION is a list of binding-specs to be imported; A binding-spec
;; is either a symbol or a pair of symbols (ORIG . SEEN), where ORIG
;; is the name in the used module and SEEN is the name in the using
;; module.  Note that SEEN is also passed through RENAMER, below.  The
;; default is to select all bindings.  If you specify no selection but
;; a renamer, only the bindings that already exist in the used module
;; are made available in the interface.  Bindings that are added later
;; are not picked up.
;;
;;   #:hide BINDINGS
;;
;; BINDINGS is a list of bindings which should not be imported.
;;
;;   #:prefix PREFIX
;;
;; PREFIX is a symbol that will be appended to each exported name.
;; The default is to not perform any renaming.
;;
;;   #:renamer RENAMER
;;
;; RENAMER is a procedure that takes a symbol and returns its new
;; name.  The default is not perform any renaming.
;;
;; Signal "no code for module" error if module name is not resolvable
;; or its public interface is not available.  Signal "no binding"
;; error if selected binding does not exist in the used module.
;;
(define* (resolve-interface name #:key
                            (select #f)
                            (hide '())
                            (prefix #f)
                            (renamer (if prefix
                                         (symbol-prefix-proc prefix)
                                         identity))
                            version)
  (let* ((module (resolve-module name #t version #:ensure #f))
         (public-i (and module (module-public-interface module))))
    (and (or (not module) (not public-i))
         (error "no code for module" name))
    (if (and (not select) (null? hide) (eq? renamer identity))
        public-i
        (let ((selection (or select (module-map (lambda (sym var) sym)
                                                public-i)))
              (custom-i (make-module 31)))
          (set-module-kind! custom-i 'custom-interface)
          (set-module-name! custom-i name)
          ;; XXX - should use a lazy binder so that changes to the
          ;; used module are picked up automatically.
          (for-each (lambda (bspec)
                      (let* ((direct? (symbol? bspec))
                             (orig (if direct? bspec (car bspec)))
                             (seen (if direct? bspec (cdr bspec)))
                             (var (or (module-local-variable public-i orig)
                                      (module-local-variable module orig)
                                      (error
                                       ;; fixme: format manually for now
                                       (simple-format
                                        #f "no binding `~A' in module ~A"
                                        orig name)))))
                        (if (memq orig hide)
                            (set! hide (delq! orig hide))
                            (module-add! custom-i
                                         (renamer seen)
                                         var))))
                    selection)
          ;; Check that we are not hiding bindings which don't exist
          (for-each (lambda (binding)
                      (if (not (module-local-variable public-i binding))
                          (error
                           (simple-format
                            #f "no binding `~A' to hide in module ~A"
                            binding name))))
                    hide)
          custom-i))))

(define (symbol-prefix-proc prefix)
  (lambda (symbol)
    (symbol-append prefix symbol)))

;; This function is called from "modules.c".  If you change it, be
;; sure to update "modules.c" as well.

(define* (define-module* name
           #:key filename pure version (duplicates '())
           (imports '()) (exports '()) (replacements '())
           (re-exports '()) (autoloads '()) transformer)
  (define (list-of pred l)
    (or (null? l)
        (and (pair? l) (pred (car l)) (list-of pred (cdr l)))))
  (define (valid-export? x)
    (or (symbol? x) (and (pair? x) (symbol? (car x)) (symbol? (cdr x)))))
  (define (valid-autoload? x)
    (and (pair? x) (list-of symbol? (car x)) (list-of symbol? (cdr x))))
  
  (define (resolve-imports imports)
    (define (resolve-import import-spec)
      (if (list? import-spec)
          (apply resolve-interface import-spec)
          (error "unexpected use-module specification" import-spec)))
    (let lp ((imports imports) (out '()))
      (cond
       ((null? imports) (reverse! out))
       ((pair? imports)
        (lp (cdr imports)
            (cons (resolve-import (car imports)) out)))
       (else (error "unexpected tail of imports list" imports)))))

  ;; We could add a #:no-check arg, set by the define-module macro, if
  ;; these checks are taking too much time.
  ;;
  (let ((module (resolve-module name #f)))
    (beautify-user-module! module)
    (if filename
        (set-module-filename! module filename))
    (if pure
        (purify-module! module))
    (if version
        (begin 
          (if (not (list-of integer? version))
              (error "expected list of integers for version"))
          (set-module-version! module version)
          (set-module-version! (module-public-interface module) version)))
    (if (pair? duplicates)
        (let ((handlers (lookup-duplicates-handlers duplicates)))
          (set-module-duplicates-handlers! module handlers)))

    (let ((imports (resolve-imports imports)))
      (call-with-deferred-observers
       (lambda ()
         (if (pair? imports)
             (module-use-interfaces! module imports))
         (if (list-of valid-export? exports)
             (if (pair? exports)
                 (module-export! module exports))
             (error "expected exports to be a list of symbols or symbol pairs"))
         (if (list-of valid-export? replacements)
             (if (pair? replacements)
                 (module-replace! module replacements))
             (error "expected replacements to be a list of symbols or symbol pairs"))
         (if (list-of valid-export? re-exports)
             (if (pair? re-exports)
                 (module-re-export! module re-exports))
             (error "expected re-exports to be a list of symbols or symbol pairs"))
         ;; FIXME
         (if (not (null? autoloads))
             (apply module-autoload! module autoloads)))))

    (if transformer
        (if (and (pair? transformer) (list-of symbol? transformer))
            (let ((iface (resolve-interface transformer))
                  (sym (car (last-pair transformer))))
              (set-module-transformer! module (module-ref iface sym)))
            (error "expected transformer to be a module name" transformer)))
    
    (run-hook module-defined-hook module)
    module))

;; `module-defined-hook' is a hook that is run whenever a new module
;; is defined.  Its members are called with one argument, the new
;; module.
(define module-defined-hook (make-hook 1))



;;; {Autoload}
;;;

(define (make-autoload-interface module name bindings)
  (let ((b (lambda (a sym definep)
             (and (memq sym bindings)
                  (let ((i (module-public-interface (resolve-module name))))
                    (if (not i)
                        (error "missing interface for module" name))
                    (let ((autoload (memq a (module-uses module))))
                      ;; Replace autoload-interface with actual interface if
                      ;; that has not happened yet.
                      (if (pair? autoload)
                          (set-car! autoload i)))
                    (module-local-variable i sym))))))
    (module-constructor (make-hash-table 0) '() b #f #f name 'autoload #f
                        (make-hash-table 0) '() (make-weak-value-hash-table 31) #f
                        (make-hash-table 0) #f #f #f)))

(define (module-autoload! module . args)
  "Have @var{module} automatically load the module named @var{name} when one
of the symbols listed in @var{bindings} is looked up.  @var{args} should be a
list of module-name/binding-list pairs, e.g., as in @code{(module-autoload!
module '(ice-9 q) '(make-q q-length))}."
  (let loop ((args args))
    (cond ((null? args)
           #t)
          ((null? (cdr args))
           (error "invalid name+binding autoload list" args))
          (else
           (let ((name     (car args))
                 (bindings (cadr args)))
             (module-use! module (make-autoload-interface module
                                                          name bindings))
             (loop (cddr args)))))))




;;; {Autoloading modules}
;;;

(define autoloads-in-progress '())

;; This function is called from "modules.c".  If you change it, be
;; sure to update "modules.c" as well.

(define* (try-module-autoload module-name #:optional version)
  (let* ((reverse-name (reverse module-name))
         (name (symbol->string (car reverse-name)))
         (dir-hint-module-name (reverse (cdr reverse-name)))
         (dir-hint (apply string-append
                          (map (lambda (elt)
                                 (string-append (symbol->string elt) "/"))
                               dir-hint-module-name))))
    (resolve-module dir-hint-module-name #f)
    (and (not (autoload-done-or-in-progress? dir-hint name))
         (let ((didit #f))
           (dynamic-wind
            (lambda () (autoload-in-progress! dir-hint name))
            (lambda ()
              (with-fluids ((current-reader #f))
                (save-module-excursion
                 (lambda () 
                   ;; The initial environment when loading a module is a fresh
                   ;; user module.
                   (set-current-module (make-fresh-user-module))
                   ;; Here we could allow some other search strategy (other than
                   ;; primitive-load-path), for example using versions encoded
                   ;; into the file system -- but then we would have to figure
                   ;; out how to locate the compiled file, do auto-compilation,
                   ;; etc. Punt for now, and don't use versions when locating
                   ;; the file.
                   (primitive-load-path (in-vicinity dir-hint name) #f)
                   (set! didit #t)))))
            (lambda () (set-autoloaded! dir-hint name didit)))
           didit))))



;;; {Dynamic linking of modules}
;;;

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



;;; {Run-time options}
;;;

(define-syntax define-option-interface
  (syntax-rules ()
    ((_ (interface (options enable disable) (option-set!)))
     (begin
       (define options
        (case-lambda
          (() (interface))
          ((arg)
           (if (list? arg)
               (begin (interface arg) (interface))
               (for-each
                (lambda (option)
                  (apply (lambda (name value documentation)
                           (display name)
                           (if (< (string-length (symbol->string name)) 8)
                               (display #\tab))
                           (display #\tab)
                           (display value)
                           (display #\tab)
                           (display documentation)
                           (newline))
                         option))
                (interface #t))))))
       (define (enable . flags)
         (interface (append flags (interface)))
         (interface))
       (define (disable . flags)
         (let ((options (interface)))
           (for-each (lambda (flag) (set! options (delq! flag options)))
                     flags)
           (interface options)
           (interface)))
       (define-syntax option-set!
         (syntax-rules ()
           ((_ opt val)
            (eval-when (eval load compile expand)
              (options (append (options) (list 'opt val)))))))))))

(define-option-interface
  (debug-options-interface
   (debug-options debug-enable debug-disable)
   (debug-set!)))

(define-option-interface
  (read-options-interface
   (read-options read-enable read-disable)
   (read-set!)))

(define-option-interface
  (print-options-interface
   (print-options print-enable print-disable)
   (print-set!)))



;;; {The Unspecified Value}
;;;
;;; Currently Guile represents unspecified values via one particular value,
;;; which may be obtained by evaluating (if #f #f). It would be nice in the
;;; future if we could replace this with a return of 0 values, though.
;;;

(define-syntax *unspecified*
  (identifier-syntax (if #f #f)))

(define (unspecified? v) (eq? v *unspecified*))




;;; {Running Repls}
;;;

(define *repl-stack* (make-fluid))

;; Programs can call `batch-mode?' to see if they are running as part of a
;; script or if they are running interactively. REPL implementations ensure that
;; `batch-mode?' returns #f during their extent.
;;
(define (batch-mode?)
  (null? (or (fluid-ref *repl-stack*) '())))

;; Programs can re-enter batch mode, for example after a fork, by calling
;; `ensure-batch-mode!'. It's not a great interface, though; it would be better
;; to abort to the outermost prompt, and call a thunk there.
;;
(define (ensure-batch-mode!)
  (set! batch-mode? (lambda () #t)))

(define (quit . args)
  (apply throw 'quit args))

(define exit quit)

(define (gc-run-time)
  (cdr (assq 'gc-time-taken (gc-stats))))

(define abort-hook (make-hook))
(define before-error-hook (make-hook))
(define after-error-hook (make-hook))
(define before-backtrace-hook (make-hook))
(define after-backtrace-hook (make-hook))

(define before-read-hook (make-hook))
(define after-read-hook (make-hook))
(define before-eval-hook (make-hook 1))
(define after-eval-hook (make-hook 1))
(define before-print-hook (make-hook 1))
(define after-print-hook (make-hook 1))

;;; This hook is run at the very end of an interactive session.
;;;
(define exit-hook (make-hook))

;;; The default repl-reader function.  We may override this if we've
;;; the readline library.
(define repl-reader
  (lambda* (prompt #:optional (reader (fluid-ref current-reader)))
    (if (not (char-ready?))
        (display (if (string? prompt) prompt (prompt))))
    (force-output)
    (run-hook before-read-hook)
    ((or reader read) (current-input-port))))




;;; {IOTA functions: generating lists of numbers}
;;;

(define (iota n)
  (let loop ((count (1- n)) (result '()))
    (if (< count 0) result
        (loop (1- count) (cons count result)))))



;;; {While}
;;;
;;; with `continue' and `break'.
;;;

;; The inliner will remove the prompts at compile-time if it finds that
;; `continue' or `break' are not used.
;;
(define-syntax while
  (lambda (x)
    (syntax-case x ()
      ((while cond body ...)
       #`(let ((break-tag (make-prompt-tag "break"))
               (continue-tag (make-prompt-tag "continue")))
           (call-with-prompt
            break-tag
            (lambda ()
              (define-syntax #,(datum->syntax #'while 'break)
                (lambda (x)
                  (syntax-case x ()
                    ((_)
                     #'(abort-to-prompt break-tag))
                    ((_ . args)
                     (syntax-violation 'break "too many arguments" x))
                    (_
                     #'(lambda ()
                         (abort-to-prompt break-tag))))))
              (let lp ()
                (call-with-prompt
                 continue-tag
                 (lambda () 
                   (define-syntax #,(datum->syntax #'while 'continue)
                     (lambda (x)
                       (syntax-case x ()
                         ((_)
                          #'(abort-to-prompt continue-tag))
                         ((_ . args)
                          (syntax-violation 'continue "too many arguments" x))
                         (_
                          #'(lambda ()
                              (abort-to-prompt continue-tag))))))
                   (do () ((not cond)) body ...))
                 (lambda (k) (lp)))))
            (lambda (k)
              #t)))))))




;;; {Module System Macros}
;;;

;; Return a list of expressions that evaluate to the appropriate
;; arguments for resolve-interface according to SPEC.

(eval-when (compile)
  (if (memq 'prefix (read-options))
      (error "boot-9 must be compiled with #:kw, not :kw")))

(define (keyword-like-symbol->keyword sym)
  (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))

(define-syntax define-module
  (lambda (x)
    (define (keyword-like? stx)
      (let ((dat (syntax->datum stx)))
        (and (symbol? dat)
             (eqv? (string-ref (symbol->string dat) 0) #\:))))
    (define (->keyword sym)
      (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))
    
    (define (parse-iface args)
      (let loop ((in args) (out '()))
        (syntax-case in ()
          (() (reverse! out))
          ;; The user wanted #:foo, but wrote :foo. Fix it.
          ((sym . in) (keyword-like? #'sym)
           (loop #`(#,(->keyword (syntax->datum #'sym)) . in) out))
          ((kw . in) (not (keyword? (syntax->datum #'kw)))
           (syntax-violation 'define-module "expected keyword arg" x #'kw))
          ((#:renamer renamer . in)
           (loop #'in (cons* #',renamer #:renamer out)))
          ((kw val . in)
           (loop #'in (cons* #'val #'kw out))))))

    (define (parse args imp exp rex rep aut)
      ;; Just quote everything except #:use-module and #:use-syntax.  We
      ;; need to know about all arguments regardless since we want to turn
      ;; symbols that look like keywords into real keywords, and the
      ;; keyword args in a define-module form are not regular
      ;; (i.e. no-backtrace doesn't take a value).
      (syntax-case args ()
        (()
         (let ((imp (if (null? imp) '() #`(#:imports `#,imp)))
               (exp (if (null? exp) '() #`(#:exports '#,exp)))
               (rex (if (null? rex) '() #`(#:re-exports '#,rex)))
               (rep (if (null? rep) '() #`(#:replacements '#,rep)))
               (aut (if (null? aut) '() #`(#:autoloads '#,aut))))
           #`(#,@imp #,@exp #,@rex #,@rep #,@aut)))
        ;; The user wanted #:foo, but wrote :foo. Fix it.
        ((sym . args) (keyword-like? #'sym)
         (parse #`(#,(->keyword (syntax->datum #'sym)) . args)
                  imp exp rex rep aut))
        ((kw . args) (not (keyword? (syntax->datum #'kw)))
         (syntax-violation 'define-module "expected keyword arg" x #'kw))
        ((#:no-backtrace . args)
         ;; Ignore this one.
         (parse #'args imp exp rex rep aut))
        ((#:pure . args)
         #`(#:pure #t . #,(parse #'args imp exp rex rep aut)))
        ((kw)
         (syntax-violation 'define-module "keyword arg without value" x #'kw))
        ((#:version (v ...) . args)
         #`(#:version '(v ...) . #,(parse #'args imp exp rex rep aut)))
        ((#:duplicates (d ...) . args)
         #`(#:duplicates '(d ...) . #,(parse #'args imp exp rex rep aut)))
        ((#:filename f . args)
         #`(#:filename 'f . #,(parse #'args imp exp rex rep aut)))
        ((#:use-module (name name* ...) . args)
         (and (and-map symbol? (syntax->datum #'(name name* ...))))
         (parse #'args (cons #'((name name* ...)) imp) exp rex rep aut))
        ((#:use-syntax (name name* ...) . args)
         (and (and-map symbol? (syntax->datum #'(name name* ...))))
         #`(#:transformer '(name name* ...)
            . #,(parse #'args (cons #'((name name* ...)) imp) exp rex rep aut)))
        ((#:use-module ((name name* ...) arg ...) . args)
         (and (and-map symbol? (syntax->datum #'(name name* ...))))
         (parse #'args
                (cons #`((name name* ...) #,@(parse-iface #'(arg ...))) imp)
                exp rex rep aut))
        ((#:export (ex ...) . args)
         (parse #'args imp #`(#,@exp ex ...) rex rep aut))
        ((#:export-syntax (ex ...) . args)
         (parse #'args imp #`(#,@exp ex ...) rex rep aut))
        ((#:re-export (re ...) . args)
         (parse #'args imp exp #`(#,@rex re ...) rep aut))
        ((#:re-export-syntax (re ...) . args)
         (parse #'args imp exp #`(#,@rex re ...) rep aut))
        ((#:replace (r ...) . args)
         (parse #'args imp exp rex #`(#,@rep r ...) aut))
        ((#:replace-syntax (r ...) . args)
         (parse #'args imp exp rex #`(#,@rep r ...) aut))
        ((#:autoload name bindings . args)
         (parse #'args imp exp rex rep #`(#,@aut name bindings)))
        ((kw val . args)
         (syntax-violation 'define-module "unknown keyword or bad argument"
                           #'kw #'val))))
    
    (syntax-case x ()
      ((_ (name name* ...) arg ...)
       (and-map symbol? (syntax->datum #'(name name* ...)))
       (with-syntax (((quoted-arg ...)
                      (parse #'(arg ...) '() '() '() '() '()))
                     ;; Ideally the filename is either a string or #f;
                     ;; this hack is to work around a case in which
                     ;; port-filename returns a symbol (`socket') for
                     ;; sockets.
                     (filename (let ((f (assq-ref (or (syntax-source x) '())
                                                  'filename)))
                                 (and (string? f) f))))
         #'(eval-when (eval load compile expand)
             (let ((m (define-module* '(name name* ...)
                        #:filename filename quoted-arg ...)))
               (set-current-module m)
               m)))))))

;; The guts of the use-modules macro.  Add the interfaces of the named
;; modules to the use-list of the current module, in order.

;; This function is called by "modules.c".  If you change it, be sure
;; to change scm_c_use_module as well.

(define (process-use-modules module-interface-args)
  (let ((interfaces (map (lambda (mif-args)
                           (or (apply resolve-interface mif-args)
                               (error "no such module" mif-args)))
                         module-interface-args)))
    (call-with-deferred-observers
     (lambda ()
       (module-use-interfaces! (current-module) interfaces)))))

(define-syntax use-modules
  (lambda (x)
    (define (keyword-like? stx)
      (let ((dat (syntax->datum stx)))
        (and (symbol? dat)
             (eqv? (string-ref (symbol->string dat) 0) #\:))))
    (define (->keyword sym)
      (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))
    
    (define (quotify-iface args)
      (let loop ((in args) (out '()))
        (syntax-case in ()
          (() (reverse! out))
          ;; The user wanted #:foo, but wrote :foo. Fix it.
          ((sym . in) (keyword-like? #'sym)
           (loop #`(#,(->keyword (syntax->datum #'sym)) . in) out))
          ((kw . in) (not (keyword? (syntax->datum #'kw)))
           (syntax-violation 'define-module "expected keyword arg" x #'kw))
          ((#:renamer renamer . in)
           (loop #'in (cons* #'renamer #:renamer out)))
          ((kw val . in)
           (loop #'in (cons* #''val #'kw out))))))

    (define (quotify specs)
      (let lp ((in specs) (out '()))
        (syntax-case in ()
          (() (reverse out))
          (((name name* ...) . in)
           (and-map symbol? (syntax->datum #'(name name* ...)))
           (lp #'in (cons #''((name name* ...)) out)))
          ((((name name* ...) arg ...) . in)
           (and-map symbol? (syntax->datum #'(name name* ...)))
           (with-syntax (((quoted-arg ...) (quotify-iface #'(arg ...))))
             (lp #'in (cons #`(list '(name name* ...) quoted-arg ...)
                            out)))))))
    
    (syntax-case x ()
      ((_ spec ...)
       (with-syntax (((quoted-args ...) (quotify #'(spec ...))))
         #'(eval-when (eval load compile expand)
             (process-use-modules (list quoted-args ...))
             *unspecified*))))))

(define-syntax use-syntax
  (syntax-rules ()
    ((_ spec ...)
     (begin
       (eval-when (eval load compile expand)
         (issue-deprecation-warning
          "`use-syntax' is deprecated. Please contact guile-devel for more info."))
       (use-modules spec ...)))))

(include-from-path "ice-9/r6rs-libraries")

(define-syntax define-private
  (syntax-rules ()
    ((_ foo bar)
     (define foo bar))))

(define-syntax define-public
  (syntax-rules ()
    ((_ (name . args) . body)
     (define-public name (lambda args . body)))
    ((_ name val)
     (begin
       (define name val)
       (export name)))))

(define-syntax defmacro-public
  (syntax-rules ()
    ((_ name args . body)
     (begin
       (defmacro name args . body)
       (export-syntax name)))))

;; And now for the most important macro.
(define-syntax λ
  (syntax-rules ()
    ((_ formals body ...)
     (lambda formals body ...))))


;; Export a local variable

;; This function is called from "modules.c".  If you change it, be
;; sure to update "modules.c" as well.

(define (module-export! m names)
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-ensure-local-variable! m internal-name)))
                  (module-add! public-i external-name var)))
              names)))

(define (module-replace! m names)
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-ensure-local-variable! m internal-name)))
                  (set-object-property! var 'replace #t)
                  (module-add! public-i external-name var)))
              names)))

;; Export all local variables from a module
;;
(define (module-export-all! mod)
  (define (fresh-interface!)
    (let ((iface (make-module)))
      (set-module-name! iface (module-name mod))
      (set-module-version! iface (module-version mod))
      (set-module-kind! iface 'interface)
      (set-module-public-interface! mod iface)
      iface))
  (let ((iface (or (module-public-interface mod)
                   (fresh-interface!))))
    (set-module-obarray! iface (module-obarray mod))))

;; Re-export a imported variable
;;
(define (module-re-export! m names)
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-variable m internal-name)))
                  (cond ((not var)
                         (error "Undefined variable:" internal-name))
                        ((eq? var (module-local-variable m internal-name))
                         (error "re-exporting local variable:" internal-name))
                        (else
                         (module-add! public-i external-name var)))))
              names)))

(define-syntax export
  (syntax-rules ()
    ((_ name ...)
     (eval-when (eval load compile expand)
       (call-with-deferred-observers
        (lambda ()
          (module-export! (current-module) '(name ...))))))))

(define-syntax re-export
  (syntax-rules ()
    ((_ name ...)
     (eval-when (eval load compile expand)
       (call-with-deferred-observers
        (lambda ()
          (module-re-export! (current-module) '(name ...))))))))

(define-syntax export!
  (syntax-rules ()
    ((_ name ...)
     (eval-when (eval load compile expand)
       (call-with-deferred-observers
        (lambda ()
          (module-replace! (current-module) '(name ...))))))))

(define-syntax export-syntax
  (syntax-rules ()
    ((_ name ...)
     (export name ...))))

(define-syntax re-export-syntax
  (syntax-rules ()
    ((_ name ...)
     (re-export name ...))))



;;; {Parameters}
;;;

(define* (make-mutable-parameter init #:optional (converter identity))
  (let ((fluid (make-fluid)))
    (fluid-set! fluid (converter init))
    (case-lambda
      (() (fluid-ref fluid))
      ((val) (fluid-set! fluid (converter val))))))




;;; {Handling of duplicate imported bindings}
;;;

;; Duplicate handlers take the following arguments:
;;
;; module  importing module
;; name    conflicting name
;; int1    old interface where name occurs
;; val1    value of binding in old interface
;; int2    new interface where name occurs
;; val2    value of binding in new interface
;; var     previous resolution or #f
;; val     value of previous resolution
;;
;; A duplicate handler can take three alternative actions:
;;
;; 1. return #f => leave responsibility to next handler
;; 2. exit with an error
;; 3. return a variable resolving the conflict
;;

(define duplicate-handlers
  (let ((m (make-module 7)))
    
    (define (check module name int1 val1 int2 val2 var val)
      (scm-error 'misc-error
                 #f
                 "~A: `~A' imported from both ~A and ~A"
                 (list (module-name module)
                       name
                       (module-name int1)
                       (module-name int2))
                 #f))
    
    (define (warn module name int1 val1 int2 val2 var val)
      (format (current-error-port)
              "WARNING: ~A: `~A' imported from both ~A and ~A\n"
              (module-name module)
              name
              (module-name int1)
              (module-name int2))
      #f)
     
    (define (replace module name int1 val1 int2 val2 var val)
      (let ((old (or (and var (object-property var 'replace) var)
                     (module-variable int1 name)))
            (new (module-variable int2 name)))
        (if (object-property old 'replace)
            (and (or (eq? old new)
                     (not (object-property new 'replace)))
                 old)
            (and (object-property new 'replace)
                 new))))
    
    (define (warn-override-core module name int1 val1 int2 val2 var val)
      (and (eq? int1 the-scm-module)
           (begin
             (format (current-error-port)
                     "WARNING: ~A: imported module ~A overrides core binding `~A'\n"
                     (module-name module)
                     (module-name int2)
                     name)
             (module-local-variable int2 name))))
     
    (define (first module name int1 val1 int2 val2 var val)
      (or var (module-local-variable int1 name)))
     
    (define (last module name int1 val1 int2 val2 var val)
      (module-local-variable int2 name))
     
    (define (noop module name int1 val1 int2 val2 var val)
      #f)
    
    (set-module-name! m 'duplicate-handlers)
    (set-module-kind! m 'interface)
    (module-define! m 'check check)
    (module-define! m 'warn warn)
    (module-define! m 'replace replace)
    (module-define! m 'warn-override-core warn-override-core)
    (module-define! m 'first first)
    (module-define! m 'last last)
    (module-define! m 'merge-generics noop)
    (module-define! m 'merge-accessors noop)
    m))

(define (lookup-duplicates-handlers handler-names)
  (and handler-names
       (map (lambda (handler-name)
              (or (module-symbol-local-binding
                   duplicate-handlers handler-name #f)
                  (error "invalid duplicate handler name:"
                         handler-name)))
            (if (list? handler-names)
                handler-names
                (list handler-names)))))

(define default-duplicate-binding-procedures
  (make-mutable-parameter #f))

(define default-duplicate-binding-handler
  (make-mutable-parameter '(replace warn-override-core warn last)
                          (lambda (handler-names)
                            (default-duplicate-binding-procedures
                              (lookup-duplicates-handlers handler-names))
                            handler-names)))



;;; {`load'.}
;;;
;;; Load is tricky when combined with relative paths, compilation, and
;;; the filesystem.  If a path is relative, what is it relative to?  The
;;; path of the source file at the time it was compiled?  The path of
;;; the compiled file?  What if both or either were installed?  And how
;;; do you get that information?  Tricky, I say.
;;;
;;; To get around all of this, we're going to do something nasty, and
;;; turn `load' into a macro.  That way it can know the path of the
;;; source file with respect to which it was invoked, so it can resolve
;;; relative paths with respect to the original source path.
;;;
;;; There is an exception, and that is that if the source file was in
;;; the load path when it was compiled, instead of looking up against
;;; the absolute source location, we load-from-path against the relative
;;; source location.
;;;

(define %auto-compilation-options
  ;; Default `compile-file' option when auto-compiling.
  '(#:warnings (unbound-variable arity-mismatch)))

(define* (load-in-vicinity dir path #:optional reader)
  ;; Returns the .go file corresponding to `name'. Does not search load
  ;; paths, only the fallback path. If the .go file is missing or out of
  ;; date, and auto-compilation is enabled, will try auto-compilation, just
  ;; as primitive-load-path does internally. primitive-load is
  ;; unaffected. Returns #f if auto-compilation failed or was disabled.
  ;;
  ;; NB: Unless we need to compile the file, this function should not cause
  ;; (system base compile) to be loaded up. For that reason compiled-file-name
  ;; partially duplicates functionality from (system base compile).
  ;;
  (define (compiled-file-name canon-path)
    (and %compile-fallback-path
         (string-append
          %compile-fallback-path
          ;; no need for '/' separator here, canon-path is absolute
          canon-path
          (cond ((or (null? %load-compiled-extensions)
                     (string-null? (car %load-compiled-extensions)))
                 (warn "invalid %load-compiled-extensions"
                       %load-compiled-extensions)
                 ".go")
                (else (car %load-compiled-extensions))))))

  (define (fresh-compiled-file-name name go-path)
    (catch #t
      (lambda ()
        (let* ((scmstat (stat name))
               (gostat  (and (not %fresh-auto-compile)
                             (stat go-path #f))))
          (if (and gostat
                   (or (> (stat:mtime gostat) (stat:mtime scmstat))
                       (and (= (stat:mtime gostat) (stat:mtime scmstat))
                            (>= (stat:mtimensec gostat)
                                (stat:mtimensec scmstat)))))
              go-path
              (begin
                (if gostat
                    (format (current-error-port)
                            ";;; note: source file ~a\n;;;       newer than compiled ~a\n"
                            name go-path))
                (cond
                 (%load-should-auto-compile
                  (%warn-auto-compilation-enabled)
                  (format (current-error-port) ";;; compiling ~a\n" name)
                  (let ((cfn
                         ((module-ref
                               (resolve-interface '(system base compile))
                               'compile-file)
                              name
                              #:opts %auto-compilation-options
                              #:env (current-module))))
                    (format (current-error-port) ";;; compiled ~a\n" cfn)
                    cfn))
                 (else #f))))))
      (lambda (k . args)
        (format (current-error-port)
                ";;; WARNING: compilation of ~a failed:\n;;; key ~a, throw_args ~s\n"
                name k args)
        #f)))

  (define (absolute-path? path)
    (string-prefix? "/" path))

  (define (load-absolute abs-path)
    (let ((cfn (let ((canon (false-if-exception (canonicalize-path abs-path))))
                 (and canon
                      (let ((go-path (compiled-file-name canon)))
                        (and go-path
                             (fresh-compiled-file-name abs-path go-path)))))))
      (if cfn
          (load-compiled cfn)
          (start-stack 'load-stack
                       (primitive-load abs-path)))))
  
  (save-module-excursion
   (lambda ()
     (with-fluids ((current-reader reader)
                   (%file-port-name-canonicalization 'relative))
       (cond
        ((or (absolute-path? path))
         (load-absolute path))
        ((absolute-path? dir)
         (load-absolute (in-vicinity dir path)))
        (else
         (load-from-path (in-vicinity dir path))))))))

(define-syntax load
  (make-variable-transformer
   (lambda (x)
     (let* ((src (syntax-source x))
            (file (and src (assq-ref src 'filename)))
            (dir (and (string? file) (dirname file))))
       (syntax-case x ()
         ((_ arg ...)
          #`(load-in-vicinity #,(or dir #'(getcwd)) arg ...))
         (id
          (identifier? #'id)
          #`(lambda args
              (apply load-in-vicinity #,(or dir #'(getcwd)) args))))))))



;;; {`cond-expand' for SRFI-0 support.}
;;;
;;; This syntactic form expands into different commands or
;;; definitions, depending on the features provided by the Scheme
;;; implementation.
;;;
;;; Syntax:
;;;
;;; <cond-expand>
;;;   --> (cond-expand <cond-expand-clause>+)
;;;     | (cond-expand <cond-expand-clause>* (else <command-or-definition>))
;;; <cond-expand-clause>
;;;   --> (<feature-requirement> <command-or-definition>*)
;;; <feature-requirement>
;;;   --> <feature-identifier>
;;;     | (and <feature-requirement>*)
;;;     | (or <feature-requirement>*)
;;;     | (not <feature-requirement>)
;;; <feature-identifier>
;;;   --> <a symbol which is the name or alias of a SRFI>
;;;
;;; Additionally, this implementation provides the
;;; <feature-identifier>s `guile' and `r5rs', so that programs can
;;; determine the implementation type and the supported standard.
;;;
;;; Currently, the following feature identifiers are supported:
;;;
;;;   guile r5rs srfi-0 srfi-4 srfi-6 srfi-13 srfi-14 srfi-55 srfi-61
;;;
;;; Remember to update the features list when adding more SRFIs.
;;;

(define %cond-expand-features
  ;; Adjust the above comment when changing this.
  '(guile
    guile-2
    r5rs
    srfi-0   ;; cond-expand itself
    srfi-4   ;; homogenous numeric vectors
    srfi-6   ;; open-input-string etc, in the guile core
    srfi-13  ;; string library
    srfi-23  ;; `error` procedure
    srfi-14  ;; character sets
    srfi-55  ;; require-extension
    srfi-61  ;; general cond clause
    ))

;; This table maps module public interfaces to the list of features.
;;
(define %cond-expand-table (make-hash-table 31))

;; Add one or more features to the `cond-expand' feature list of the
;; module `module'.
;;
(define (cond-expand-provide module features)
  (let ((mod (module-public-interface module)))
    (and mod
         (hashq-set! %cond-expand-table mod
                     (append (hashq-ref %cond-expand-table mod '())
                             features)))))

(define-syntax cond-expand
  (lambda (x)
    (define (module-has-feature? mod sym)
      (or-map (lambda (mod)
                (memq sym (hashq-ref %cond-expand-table mod '())))
              (module-uses mod)))

    (define (condition-matches? condition)
      (syntax-case condition (and or not)
        ((and c ...)
         (and-map condition-matches? #'(c ...)))
        ((or c ...)
         (or-map condition-matches? #'(c ...)))
        ((not c)
         (if (condition-matches? #'c) #f #t))
        (c
         (identifier? #'c)
         (let ((sym (syntax->datum #'c)))
           (if (memq sym %cond-expand-features)
               #t
               (module-has-feature? (current-module) sym))))))

    (define (match clauses alternate)
      (syntax-case clauses ()
        (((condition form ...) . rest)
         (if (condition-matches? #'condition)
             #'(begin form ...)
             (match #'rest alternate)))
        (() (alternate))))

    (syntax-case x (else)
      ((_ clause ... (else form ...))
       (match #'(clause ...)
         (lambda ()
           #'(begin form ...))))
      ((_ clause ...)
       (match #'(clause ...)
         (lambda ()
           (syntax-violation 'cond-expand "unfulfilled cond-expand" x)))))))

;; This procedure gets called from the startup code with a list of
;; numbers, which are the numbers of the SRFIs to be loaded on startup.
;;
(define (use-srfis srfis)
  (process-use-modules
   (map (lambda (num)
          (list (list 'srfi (string->symbol
                             (string-append "srfi-" (number->string num))))))
        srfis)))



;;; srfi-55: require-extension
;;;

(define-syntax require-extension
  (lambda (x)
    (syntax-case x (srfi)
      ((_ (srfi n ...))
       (and-map integer? (syntax->datum #'(n ...)))
       (with-syntax
           (((srfi-n ...)
             (map (lambda (n)
                    (datum->syntax x (symbol-append 'srfi- n)))
                  (map string->symbol
                       (map number->string (syntax->datum #'(n ...)))))))
         #'(use-modules (srfi srfi-n) ...)))
      ((_ (type arg ...))
       (identifier? #'type)
       (syntax-violation 'require-extension "Not a recognized extension type"
                         x)))))


;;; Defining transparently inlinable procedures
;;;

(define-syntax define-inlinable
  ;; Define a macro and a procedure such that direct calls are inlined, via
  ;; the macro expansion, whereas references in non-call contexts refer to
  ;; the procedure.  Inspired by the `define-integrable' macro by Dybvig et al.
  (lambda (x)
    ;; Use a space in the prefix to avoid potential -Wunused-toplevel
    ;; warning
    (define prefix (string->symbol "% "))
    (define (make-procedure-name name)
      (datum->syntax name
                     (symbol-append prefix (syntax->datum name)
                                    '-procedure)))

    (syntax-case x ()
      ((_ (name formals ...) body ...)
       (identifier? #'name)
       (with-syntax ((proc-name  (make-procedure-name #'name))
                     ((args ...) (generate-temporaries #'(formals ...))))
         #`(begin
             (define (proc-name formals ...)
               body ...)
             (define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   ((_ args ...)
                    #'((lambda (formals ...)
                         body ...)
                       args ...))
                   (_
                    (identifier? x)
                    #'proc-name))))))))))



(define using-readline?
  (let ((using-readline? (make-fluid)))
     (make-procedure-with-setter
      (lambda () (fluid-ref using-readline?))
      (lambda (v) (fluid-set! using-readline? v)))))



;;; {Deprecated stuff}
;;;

(begin-deprecated
 (module-use! the-scm-module (resolve-interface '(ice-9 deprecated))))



;;; Place the user in the guile-user module.
;;;

;; FIXME:
(module-use! the-scm-module (resolve-interface '(srfi srfi-4)))

;; Set filename to #f to prevent reload.
(define-module (guile-user)
  #:autoload (system base compile) (compile compile-file)
  #:filename #f)

;; Remain in the `(guile)' module at compilation-time so that the
;; `-Wunused-toplevel' warning works as expected.
(eval-when (compile) (set-current-module the-root-module))

;;; boot-9.scm ends here
