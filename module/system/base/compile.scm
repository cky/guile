;;; High-level compiler interface

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.

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
            compile
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
               (let ((file (or (assq-ref loc 'filename) "unknown file"))
                     (line (assq-ref loc 'line))
                     (col (assq-ref loc 'column)))
                 (format (current-error-port)
                         "~A:~A:~A: ~A: ~A~%" file line col msg exp))
               (format (current-error-port)
                       "unknown location: ~A: ~S~%" msg exp)))))


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
           (proc tmp)
           (chmod tmp (logand #o0666 (lognot (umask))))
           (close-port tmp)
           (rename-file template filename))
         (lambda args
           (delete-file template)))))))

(define (ensure-language x)
  (if (language? x)
      x
      (lookup-language x)))

(define* (compile-file file #:key
                       (output-file #f)
                       (env #f)
                       (from (current-language))
                       (to 'objcode)
                       (opts '()))
  (let ((comp (or output-file (compiled-file-name file)))
        (in (open-input-file file)))
    (call-with-output-file/atomic comp
      (lambda (port)
        ((language-printer (ensure-language to))
         (read-and-compile in #:env env #:from from #:to to #:opts opts)
         port)))
    comp))

(define* (compile-and-load file #:key (to 'value) (opts '()))
  (read-and-compile (open-input-port file)
                    #:from lang #:to to #:opts opts))

(define (compiled-file-name file)
  (let ((base (basename file))
        (cext (cond ((or (null? %load-compiled-extensions)
                         (string-null? (car %load-compiled-extensions)))
                     (warn "invalid %load-compiled-extensions"
                           %load-compiled-extensions)
                     ".go")
                    (else (car %load-compiled-extensions)))))
    (let lp ((exts %load-extensions))
      (cond ((null? exts) (string-append file cext))
            ((string-null? (car exts)) (lp (cdr exts)))
            ((string-suffix? (car exts) base)
             (string-append
              (dirname file) "/"
              (substring base 0
                         (- (string-length base) (string-length (car exts))))
              cext))
            (else (lp (cdr exts)))))))


;;;
;;; Compiler interface
;;;

(define (compile-passes from to opts)
  (map cdr
       (or (lookup-compilation-order from to)
           (error "no way to compile" from "to" to))))

(define (compile-fold passes exp env opts)
  (let lp ((passes passes) (x exp) (e env) (cenv env) (first? #t))
    (if (null? passes)
        (values x e cenv)
        (receive (x e new-cenv) ((car passes) x e opts)
          (lp (cdr passes) x e (if first? new-cenv cenv) #f)))))

(define (find-language-joint from to)
  (let lp ((in (reverse (or (lookup-compilation-order from to)
                            (error "no way to compile" from "to" to))))
           (lang to))
    (cond ((null? in)
           (error "don't know how to join expressions" from to))
          ((language-joiner lang) lang)
          (else
           (lp (cdr in) (caar in))))))

(define* (read-and-compile port #:key
                           (env #f)
                           (from (current-language))
                           (to 'objcode)
                           (opts '()))
  (let ((from (ensure-language from))
        (to (ensure-language to)))
    (let ((joint (find-language-joint from to)))
      (with-fluids ((*current-language* from))
        (let lp ((exps '()) (env #f) (cenv env))
          (let ((x ((language-reader (current-language)) port)))
            (cond
             ((eof-object? x)
              (compile ((language-joiner joint) (reverse exps) env)
                       #:from joint #:to to #:env env #:opts opts))
             (else
              ;; compile-fold instead of compile so we get the env too
              (receive (jexp jenv jcenv)
                  (compile-fold (compile-passes (current-language) joint opts)
                                x cenv opts)
                (lp (cons jexp exps) jenv jcenv))))))))))

(define* (compile x #:key
                  (env #f)
                  (from (current-language))
                  (to 'value)
                  (opts '()))
  (receive (exp env cenv)
      (compile-fold (compile-passes from to opts) x env opts)
    exp))


;;;
;;; Decompiler interface
;;;

(define (decompile-passes from to opts)
  (map cdr
       (or (lookup-decompilation-order from to)
           (error "no way to decompile" from "to" to))))

(define (decompile-fold passes exp env opts)
  (if (null? passes)
      (values exp env)
      (receive (exp env) ((car passes) exp env opts)
        (decompile-fold (cdr passes) exp env opts))))

(define* (decompile x #:key
                    (env #f)
                    (from 'value)
                    (to 'assembly)
                    (opts '()))
  (decompile-fold (decompile-passes from to opts)
                  x
                  env
                  opts))
