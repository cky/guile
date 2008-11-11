;;; High-level compiler interface

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

(define-module (system base compile)
  #:use-syntax (system base syntax)
  #:use-module (system base language)
  #:use-module ((system il compile) #:select ((compile . compile-il)))
  #:use-module (system il ghil)
  #:use-module (system il glil)
  #:use-module (system vm objcode)
  #:use-module (system vm assemble)
  #:use-module (system vm vm) ;; for compile-time evaluation
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (syntax-error compile-file load-source-file load-file
            *current-language*
            compiled-file-name
            compile-time-environment
            compile read-file-in compile-in
            load/compile)
  #:export-syntax (call-with-compile-error-catch))

;;;
;;; Compiler environment
;;;

(define (syntax-error loc msg exp)
  (throw 'syntax-error-compile-time loc msg exp))

(define-macro (call-with-compile-error-catch thunk)
  `(catch 'syntax-error-compile-time
	 ,thunk
	 (lambda (key loc msg exp)
	   (if (pair? loc)
	       (format (current-error-port)
                       "~A:~A: ~A: ~A~%" (car loc) (cdr loc) msg exp)
	       (format (current-error-port)
                       "unknown location: ~A: ~A~%" msg exp)))))


;;;
;;; Compiler
;;;

(define *current-language* (make-fluid))

;; This is basically to avoid mucking with the backtrace.
(define (call-with-nonlocal-exit-protect thunk on-nonlocal-exit)
  (let ((success #f) (entered #f))
    (dynamic-wind
        (lambda ()
          (if entered
              (error "thunk may only be entered once: ~a" thunk))
          (set! entered #t))
        (lambda ()
          (thunk)
          (set! success #t))
        (lambda ()
          (if (not success)
              (on-nonlocal-exit))))))
                        
(define (call-with-output-file/atomic filename proc)
  (let* ((template (string-append filename ".XXXXXX"))
         (tmp (mkstemp! template)))
    (call-with-nonlocal-exit-protect
     (lambda ()
       (with-output-to-port tmp
         (lambda () (proc (current-output-port))))
       (rename-file template filename))
     (lambda ()
       (delete-file template)))))

(define (compile-file file . opts)
  (let ((comp (compiled-file-name file))
        (lang (fluid-ref *current-language*)))
    (catch 'nothing-at-all
      (lambda ()
	(call-with-compile-error-catch
	 (lambda ()
	   (call-with-output-file/atomic comp
	     (lambda (port)
	       (let* ((source (read-file-in file lang))
		      (objcode (apply compile-in source (current-module)
				      lang opts)))
		 (if (memq #:c opts)
		   (pprint-glil objcode port)
		   (uniform-vector-write (objcode->u8vector objcode) port)))))
	   (format #t "wrote `~A'\n" comp))))
      (lambda (key . args)
	(format #t "ERROR: during compilation of ~A:\n" file)
	(display "ERROR: ")
	(apply format #t (cadr args) (caddr args))
	(newline)
	(format #t "ERROR: ~A ~A ~A\n" key (car args) (cadddr args))
	(delete-file comp)))))

; (let ((c-f compile-file))
;   ;; XXX:  Debugging output
;   (set! compile-file
; 	(lambda (file . opts)
; 	  (format #t "compile-file: ~a ~a~%" file opts)
; 	  (let ((result (apply c-f (cons file opts))))
; 	    (format #t "compile-file: returned ~a~%" result)
; 	    result))))

(define (load-source-file file . opts)
  (let ((lang (fluid-ref *current-language*)))
    (let ((source (read-file-in file lang)))
      (apply compile-in source (current-module) lang opts))))

(define (load-file file . opts)
  (let ((comp (compiled-file-name file)))
    (if (file-exists? comp)
	(load-objcode comp)
	(apply load-source-file file opts))))

(define (compiled-file-name file)
  (let ((base (basename file))
        (cext (cond ((or (null? %load-compiled-extensions)
                         (string-null? (car %load-compiled-extensions)))
                     (warn "invalid %load-compiled-extensions"
                           %load-compiled-extensions)
                     ".go")
                    (else (car %load-compiled-extensions)))))
    (let lp ((exts %load-extensions))
      (cond ((null? exts) (string-append base cext))
            ((string-null? (car exts)) (lp (cdr exts)))
            ((string-suffix? (car exts) base)
             (string-append
              (substring base 0
                         (- (string-length base) (string-length (car exts))))
              cext))
            (else (lp (cdr exts)))))))

;;; environment := #f
;;;                | MODULE
;;;                | COMPILE-ENV
;;; compile-env := (MODULE LEXICALS . EXTERNALS)
(define (cenv-module env)
  (cond ((not env) #f)
        ((module? env) env)
        ((and (pair? env) (module? (car env))) (car env))
        (else (error "bad environment" env))))

(define (cenv-ghil-env env)
  (cond ((not env) (make-ghil-toplevel-env))
        ((module? env) (make-ghil-toplevel-env))
        ((pair? env)
         (ghil-env-dereify (cadr env)))
        (else (error "bad environment" env))))

(define (cenv-externals env)
  (cond ((not env) '())
        ((module? env) '())
        ((pair? env) (cddr env))
        (else (error "bad environment" env))))

(define (compile-time-environment)
  "A special function known to the compiler that, when compiled, will
return a representation of the lexical environment in place at compile
time. Useful for supporting some forms of dynamic compilation. Returns
#f if called from the interpreter."
  #f)

(define* (compile x #:optional env)
  (let ((thunk (objcode->program
                (compile-in x env (fluid-ref *current-language*))
                (cenv-externals env))))
    (if (not env)
        (thunk)
        (save-module-excursion
         (lambda ()
           (set-current-module (cenv-module env))
           (thunk))))))


;;;
;;; Scheme compiler interface
;;;

(define (read-file-in file lang)
  (call-with-input-file file (or (language-read-file lang)
                                 (error "language has no #:read-file" lang))))

;;; FIXME: fold run-pass x (compile-passes lang opts) 
(define (compile-passes lang opts)
  (let lp ((passes (list
                    (language-expander lang)
                    (language-translator lang)
                    (lambda (x e) (apply compile-il x e opts))
                    (lambda (x e) (apply assemble x e opts))))
           (keys '(#f #:e #:t #:c))
           (out '()))
    (if (or (null? keys)
            (and (car keys) (memq (car keys) opts)))
        (reverse! out)
        (lp (cdr passes) (cdr keys)
            (if (car passes)
                (cons (car passes) out)
                out)))))

(define (compile-in x e lang . opts)
  (save-module-excursion
   (lambda ()
     (and=> (cenv-module e) set-current-module)
     (let ((env (cenv-ghil-env e)))
       (fold (lambda (pass exp)
               (pass exp env))
             x
             (compile-passes lang opts))))))

;;;
;;;
;;;

(define (compile-and-load file . opts)
  (let ((comp (object-file-name file)))
    (if (or (not (file-exists? comp))
	    (> (stat:mtime (stat file)) (stat:mtime (stat comp))))
	(compile-file file))
    (load-compiled-file comp)))

(define (load/compile file . opts)
  (let* ((file (file-full-name file))
	 (compiled (object-file-name file)))
    (if (or (not (file-exists? compiled))
	    (> (stat:mtime (stat file)) (stat:mtime (stat compiled))))
	(apply compile-file file #f opts))
    (if (memq #:b opts)
	(apply vm-trace (the-vm) (load-objcode compiled) opts)
	((the-vm) (load-objcode compiled)))))

(define (file-full-name filename)
  (let* ((port (current-load-port))
	 (oldname (and port (port-filename port))))
    (if (and oldname
	     (> (string-length filename) 0)
	     (not (char=? (string-ref filename 0) #\/))
	     (not (string=? (dirname oldname) ".")))
	(string-append (dirname oldname) "/" filename)
	filename)))

(fluid-set! *current-language* (lookup-language 'scheme))
