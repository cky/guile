(define-module (lang elisp transform)
  #:use-module (lang elisp internals trace)
  #:use-module (lang elisp internals fset)
  #:use-module (lang elisp internals evaluation)
  #:use-module (ice-9 session)
  #:export (transformer transform))

;;; {S-expressions}
;;;

(define (syntax-error x)
  (error "Syntax error in expression" x))

;; Should be made mutating instead of constructing
;;
(define (transformer x)
  (cond ((eq? x 'nil) %nil)
	((eq? x 't) #t)
	((null? x) %nil)
	((not (pair? x)) x)
	((and (pair? (car x))
	      (eq? (caar x) 'quasiquote))
	 (transformer (car x)))
	((symbol? (car x))
	 (case (car x)
	   ((@fop @bind define-module use-modules use-syntax) x)
	   ; Escape to Scheme syntax
	   ((scheme) (cons begin (cdr x)))
	   ; Should be handled in reader
	   ((quote function) `(,quote ,@(cars->nil (cdr x))))
	   ((quasiquote) (m-quasiquote x '()))
	   ;((nil-cond) (transform-1 x))
	   ;((let) (m-let x '()))
	   ;((let*) (m-let* x '()))
	   ;((if) (m-if x '()))
	   ;((and) (m-and x '()))
	   ;((or) (m-or x '()))
	   ;((while) (m-while x '()))
	   ;((while) (cons macro-while (cdr x)))
	   ;((prog1) (m-prog1 x '()))
	   ;((prog2) (m-prog2 x '()))
	   ;((progn) (cons 'begin (map transformer (cdr x))))
	   ;((cond) (m-cond x '()))
	   ;((lambda) (transform-lambda/interactive x '<elisp-lambda>))
	   ;((defun) (m-defun x '()))
	   ;((defmacro) (m-defmacro x '()))
	   ;((setq) (m-setq x '()))
	   ;((interactive) (fluid-set! interactive-spec x) #f)
	   ;((unwind-protect) (m-unwind-protect x '()))
	   (else (transform-application x))))
	(else (syntax-error x))))

(define (m-quasiquote exp env)
  (cons quasiquote
	(map transform-inside-qq (cdr exp))))

(define (transform-inside-qq x)
  (trc 'transform-inside-qq x)
  (cond ((not (pair? x)) x)
	((symbol? (car x))
	 (case (car x)
	   ((unquote) (list 'unquote (transformer (cadr x))))
	   ((unquote-splicing) (list 'unquote-splicing (transformer (cadr x))))
	   (else (cons (car x) (map transform-inside-qq (cdr x))))))
	(else
	 (cons (transform-inside-qq (car x)) (transform-inside-qq (cdr x))))))

(define (transform-application x)
  (cons-source x
	       '@fop
	       `(,(car x) (,transformer-macro ,@(cdr x)))))

(define transformer-macro
  (procedure->memoizing-macro
   (let ((cdr cdr))
     (lambda (exp env)
       (cons 'list (map transformer (cdr exp)))))))

(define (cars->nil ls)
  (cond ((not (pair? ls)) ls)
	((null? (car ls)) (cons '() (cars->nil (cdr ls))))
	(else (cons (cars->nil (car ls))
		    (cars->nil (cdr ls))))))

(define transform transformer)
