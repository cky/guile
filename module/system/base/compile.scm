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
  #:use-module (system base syntax)
  #:use-module (system base language)
  #:use-module (system vm vm) ;; FIXME: there's a reason for this, can't remember why tho
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:export (syntax-error 
            *current-language*
            compiled-file-name compile-file compile-and-load
            compile compile-time-environment
            decompile)
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
(fluid-set! *current-language* 'scheme)
(define (current-language)
  (fluid-ref *current-language*))

(define (call-once thunk)
  (let ((entered #f))
    (dynamic-wind
        (lambda ()
          (if entered
              (error "thunk may only be entered once: ~a" thunk))
          (set! entered #t))
        thunk
        (lambda () #t))))

(define (call-with-output-file/atomic filename proc)
  (let* ((template (string-append filename ".XXXXXX"))
         (tmp (mkstemp! template)))
    (call-once
     (lambda ()
       (with-throw-handler #t
         (lambda ()
           (with-output-to-port tmp
             (lambda () (proc (current-output-port))))
           (rename-file template filename))
         (lambda args
           (delete-file template)))))))

(define (ensure-language x)
  (if (language? x)
      x
      (lookup-language x)))

(define* (compile-file file #:key (to 'objcode) (opts '()))
  (let ((comp (compiled-file-name file))
        (lang (ensure-language (current-language)))
        (to (ensure-language to)))
    (catch 'nothing-at-all
      (lambda ()
	(call-with-compile-error-catch
	 (lambda ()
	   (call-with-output-file/atomic comp
	     (lambda (port)
               (let ((print (language-printer to)))
                 (print (compile (read-file-in file lang)
                                 #:from lang #:to to #:opts opts)
                        port))))
	   (format #t "wrote `~A'\n" comp))))
      (lambda (key . args)
	(format #t "ERROR: during compilation of ~A:\n" file)
	(display "ERROR: ")
	(apply format #t (cadr args) (caddr args))
	(newline)
	(format #t "ERROR: ~A ~A ~A\n" key (car args) (cadddr args))
	(delete-file comp)))))

(define* (compile-and-load file #:key (to 'value) (opts '()))
  (let ((lang (ensure-language (current-language))))
    (compile (read-file-in file lang) #:to 'value #:opts opts)))

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


;;;
;;; Compiler interface
;;;

(define (read-file-in file lang)
  (call-with-input-file file
    (or (language-read-file lang)
        (error "language has no #:read-file" lang))))

(define (compile-passes from to opts)
  (map cdr
       (or (lookup-compilation-order from to)
           (error "no way to compile" from "to" to))))

(define (compile-fold passes exp env opts)
  (if (null? passes)
      exp
      (receive (exp env) ((car passes) exp env opts)
        (compile-fold (cdr passes) exp env opts))))

(define (compile-time-environment)
  "A special function known to the compiler that, when compiled, will
return a representation of the lexical environment in place at compile
time. Useful for supporting some forms of dynamic compilation. Returns
#f if called from the interpreter."
  #f)

(define* (compile x #:key
                  (env #f)
                  (from (current-language))
                  (to 'value)
                  (opts '()))
  (compile-fold (compile-passes from to opts)
                x
                env
                opts))


;;;
;;; Decompiler interface
;;;

(define (decompile-passes from to opts)
  (map cdr
       (or (lookup-decompilation-order from to)
           (error "no way to decompile" from "to" to))))

(define* (decompile x #:key
                    (env #f)
                    (from 'value)
                    (to 'assembly)
                    (opts '()))
  (compile-fold (decompile-passes from to opts)
                x
                env
                opts))
