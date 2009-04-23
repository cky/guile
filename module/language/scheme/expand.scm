;;; Guile Scheme specification

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language scheme expand)
  #:use-module (language scheme amatch)
  #:use-module (ice-9 expand-support)
  #:use-module (ice-9 optargs)
  #:use-module ((system base compile) #:select (syntax-error))
  #:export (expand *expand-table* define-scheme-expander))

(define (aref x) (if (annotation? x) (annotation-expression x) x))
(define (apair? x) (pair? (aref x)))
(define (acar x) (car (aref x)))
(define (acdr x) (cdr (aref x)))
(define (acaar x) (acar (acar x)))
(define (acdar x) (acdr (acar x)))
(define (acadr x) (acar (acdr x)))
(define (acddr x) (acdr (acdr x)))
(define (aloc x) (and (annotation? x) (annotation-source x)))
(define (re-annotate x y)
  (if (and (annotation? x) (not (annotation? y)))
      (make-annotation y (annotation-source x))
      y))
(define-macro (-> exp) `(re-annotate x ,exp))

(define* (expand x #:optional (mod (current-module)) (once? #f))
  (define re-expand
    (if once?
        (lambda (x) x)
        (lambda (x) (expand x mod once?))))
  (let ((exp (if (annotation? x) (annotation-expression x) x)))
    (cond
     ((pair? exp)
      (let ((head (car exp)) (tail (cdr exp))) 
        (cond
         ;; allow macros to be unquoted into the output of a macro
         ;; expansion
         ((or (symbol? head) (macro? head))
          (let ((val (cond
                      ((macro? head) head)
                      ((module-variable mod head)
                       => (lambda (var)
                            ;; unbound vars can happen if the module
                            ;; definition forward-declared them
                            (and (variable-bound? var) (variable-ref var))))
                      (else #f))))
            (cond
             ((hashq-ref *expand-table* val)
              => (lambda (expand1) (expand1 x re-expand)))

             ((defmacro? val)
              (re-expand (-> (apply (defmacro-transformer val)
                                    (deannotate tail)))))
             
             ((eq? val sc-macro)
              ;; syncase!
              (let* ((eec (@@ (ice-9 syncase) expansion-eval-closure))
                     (sc-expand3 (@@ (ice-9 syncase) sc-expand3)))
                (re-expand
                 (with-fluids ((eec (module-eval-closure mod)))
                   ;; fixme -- use ewes fluid?
                   (sc-expand3 exp 'c '(compile load eval))))))

             ((primitive-macro? val)
              (syntax-error (aloc x) "unhandled primitive macro" head))
             
             ((macro? val)
              (syntax-error (aloc x) "unknown kind of macro" head))

             (else
              (-> (cons head (map re-expand tail)))))))

         (else
          (-> (map re-expand exp))))))
          
     (else x))))


(define *expand-table* (make-hash-table))

(define-macro (define-scheme-expander sym . clauses)
  `(hashq-set! (@ (language scheme expand) *expand-table*)
               ,sym
               (lambda (x re-expand)
                 (define syntax-error (@ (system base compile) syntax-error))
                 (amatch (acdr x)
                   ,@clauses
                   ,@(if (assq 'else clauses) '()
                         `((else
                            (syntax-error (aloc x) (format #f "bad ~A" ',sym) x))))))))

(define-scheme-expander quote
  ;; (quote OBJ)
  ((,obj) x))
    
(define-scheme-expander quasiquote
  ;; (quasiquote OBJ)
  ((,obj)
   (-> `(,'quasiquote
         ,(let lp ((x obj) (level 0))
            (cond ((not (apair? x)) x)
                  ;; FIXME: hygiene regarding imported , / ,@ rebinding
                  ((memq (acar x) '(unquote unquote-splicing))
                   (amatch (acdr x)
                     ((,obj)
                      (cond
                       ((zero? level) 
                        (-> `(,(acar x) ,(re-expand obj))))
                       (else
                        (-> `(,(acar x) ,(lp obj (1- level)))))))
                     (else (syntax-error (aloc x) (format #f "bad ~A" (acar x)) x))))
                  ((eq? (acar x) 'quasiquote)
                   (amatch (acdr x)
                     ((,obj) (-> `(,'quasiquote ,(lp obj (1+ level)))))
                     (else (syntax-error (aloc x) "bad quasiquote" x))))
                  (else (-> (cons (lp (acar x) level) (lp (acdr x) level))))))))))

(define-scheme-expander define
  ;; (define NAME VAL)
  ((,name ,val) (guard (symbol? name))
   (-> `(define ,name ,(re-expand val))))
  ;; (define (NAME FORMALS...) BODY...)
  (((,name . ,formals) . ,body) (guard (symbol? name))
   ;; -> (define NAME (lambda FORMALS BODY...))
   (re-expand (-> `(define ,name (lambda ,formals . ,body))))))

(define-scheme-expander set!
  ;; (set! (NAME ARGS...) VAL)
  (((,name . ,args) ,val) (guard (symbol? name)
                                 (not (eq? name '@)) (not (eq? name '@@)))
   ;; -> ((setter NAME) ARGS... VAL)
   (re-expand (-> `((setter ,name) ,@args ,val))))

  ;; (set! NAME VAL)
  ((,name ,val) (guard (symbol? name))
   (-> `(set! ,name ,(re-expand val)))))

(define-scheme-expander if
  ;; (if TEST THEN [ELSE])
  ((,test ,then)
   (-> `(if ,(re-expand test) ,(re-expand then))))
  ((,test ,then ,else)
   (-> `(if ,(re-expand test) ,(re-expand then) ,(re-expand else)))))

(define-scheme-expander and
  ;; (and EXPS...)
  (,tail
   (-> `(and . ,(map re-expand tail)))))

(define-scheme-expander or
  ;; (or EXPS...)
  (,tail
   (-> `(or . ,(map re-expand tail)))))

(define-scheme-expander begin
  ;; (begin EXPS...)
  ((,single-exp)
   (-> (re-expand single-exp)))
  (,tail
   (-> `(begin . ,(map re-expand tail)))))

(define (valid-bindings? bindings . it-is-for-do)
  (define (valid-binding? b)
    (amatch b 
      ((,sym ,var) (guard (symbol? sym)) #t)
      ((,sym ,var ,update) (guard (pair? it-is-for-do) (symbol? sym)) #t)
      (else #f)))
  (and (list? (aref bindings))
       (and-map valid-binding? (aref bindings))))

(define-scheme-expander let
  ;; (let NAME ((SYM VAL) ...) BODY...)
  ((,name ,bindings . ,body) (guard (symbol? name)
                                    (valid-bindings? bindings))
   ;; -> (letrec ((NAME (lambda (SYM...) BODY...))) (NAME VAL...))
   (re-expand (-> `(letrec ((,name (lambda ,(map acar (aref bindings))
                                     . ,body)))
                     (,name . ,(map acadr (aref bindings)))))))

  ((() . ,body)
   (re-expand (expand-internal-defines body)))

  ;; (let ((SYM VAL) ...) BODY...)
  ((,bindings . ,body) (guard (valid-bindings? bindings))
   (-> `(let ,(map (lambda (x)
                     ;; nb, relies on -> non-hygiene
                     (-> `(,(acar x) ,(re-expand (acadr x)))))
                   (aref bindings))
          ,(expand-internal-defines (map re-expand body))))))

(define-scheme-expander let*
  ;; (let* ((SYM VAL) ...) BODY...)
  ((() . ,body)
   (re-expand (-> `(let () . ,body))))
  ((((,sym ,val) . ,rest) . ,body) (guard (symbol? sym))
   (re-expand (-> `(let ((,sym ,val)) (let* ,rest . ,body))))))

(define-scheme-expander letrec
  ;; (letrec ((SYM VAL) ...) BODY...)
  ((,bindings . ,body) (guard (valid-bindings? bindings))
   (-> `(letrec ,(map (lambda (x)
                        ;; nb, relies on -> non-hygiene
                        (-> `(,(acar x) ,(re-expand (acadr x)))))
                      (aref bindings))
          ,(expand-internal-defines (map re-expand body))))))

(define-scheme-expander cond
  ;; (cond (CLAUSE BODY...) ...)
  (() (-> '(begin)))
  (((else . ,body)) (re-expand (-> `(begin ,@body))))
  (((,test) . ,rest) (re-expand (-> `(or ,test (cond ,@rest)))))
  (((,test => ,proc) . ,rest)
   ;; FIXME hygiene!
   (re-expand (-> `(let ((_t ,test)) (if _t (,proc _t) (cond ,@rest))))))
  (((,test . ,body) . ,rest)
   (re-expand (-> `(if ,test (begin ,@body) (cond ,@rest))))))

(define-scheme-expander case
  ;; (case EXP ((KEY...) BODY...) ...)
  ((,exp . ,clauses)
    ;; FIXME hygiene!
   (re-expand 
    (->`(let ((_t ,exp))
          ,(let loop ((ls clauses))
             (cond ((null? ls) '(begin))
                   ((eq? (acaar ls) 'else) `(begin ,@(acdar ls)))
                   (else `(if (memv _t ',(acaar ls))
                              (begin ,@(acdar ls))
                              ,(loop (acdr ls)))))))))))

(define-scheme-expander do
  ;; (do ((SYM VAL [UPDATE]) ...) (TEST RESULT...) BODY...)
  ((,bindings (,test . ,result) . ,body) (guard (valid-bindings? bindings #t))
   (let ((sym (map acar (aref bindings)))
         (val (map acadr (aref bindings)))
         (update (map acddr (aref bindings))))
     (define (next s x) (if (pair? x) (car x) s))
     (re-expand
      ;; FIXME hygiene!
      (-> `(letrec ((_l (lambda ,sym
                          (if ,test
                              (begin ,@result)
                              (begin ,@body
                                     (_l ,@(map next sym update)))))))
             (_l ,@val)))))))

(define-scheme-expander lambda
  ;; (lambda FORMALS BODY...)
  ((,formals ,docstring ,body1 . ,body) (guard (string? docstring))
   (-> `(lambda ,formals ,docstring ,(expand-internal-defines
                                      (map re-expand (cons body1 body))))))
  ((,formals . ,body)
   (-> `(lambda ,formals ,(expand-internal-defines (map re-expand body))))))

(define-scheme-expander delay
  ;; FIXME not hygienic
  ((,expr)
   (re-expand `(make-promise (lambda () ,expr)))))

(define-scheme-expander @
  ((,modname ,sym)
   x))

(define-scheme-expander @@
  ((,modname ,sym)
   x))

(define-scheme-expander eval-when
  ((,when . ,body) (guard (list? when) (and-map symbol? when))
   (if (memq 'compile when)
       (primitive-eval `(begin . ,body)))
   (if (memq 'load when)
       (-> `(begin . ,body))
       (-> `(begin)))))

;;; Hum, I don't think this takes imported modifications to `define'
;;; properly into account. (Lexical bindings are OK because of alpha
;;; renaming.)
(define (expand-internal-defines body)
  (let loop ((ls body) (ds '()))
    (amatch ls
      (() (syntax-error l "bad body" body))
      (((define ,name ,val) . _)
       (loop (acdr ls) (cons (list name val) ds)))
      (else
       (if (null? ds)
           (if (null? (cdr ls)) (car ls) `(begin ,@ls))
           `(letrec ,ds ,(if (null? (cdr ls)) (car ls) `(begin ,@ls))))))))
