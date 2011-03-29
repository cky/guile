;;; High-level compiler interface

;; Copyright (C) 2001, 2009, 2010, 2011 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system base compile)
  #:use-module (system base syntax)
  #:use-module (system base language)
  #:use-module (system base message)
  #:use-module (system vm vm) ;; FIXME: there's a reason for this, can't remember why tho
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:export (compiled-file-name
            compile-file
            compile-and-load
            read-and-compile
            compile
            decompile))


;;;
;;; Compiler
;;;

(define (call-once thunk)
  (let ((entered #f))
    (dynamic-wind
        (lambda ()
          (if entered
              (error "thunk may only be entered once: ~a" thunk))
          (set! entered #t))
        thunk
        (lambda () #t))))

;; (put 'call-with-output-file/atomic 'scheme-indent-function 1)
(define* (call-with-output-file/atomic filename proc #:optional reference)
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

;; Throws an exception if `dir' is not writable.  The mkdir occurs
;; before the check, so that we avoid races (possibly due to parallel
;; compilation).
;;
(define (ensure-writable-dir dir)
  (catch 'system-error
    (lambda ()
      (mkdir dir))
    (lambda (k subr fmt args rest)
      (let ((errno (and (pair? rest) (car rest))))
        (cond
         ((eqv? errno EEXIST)
          (let ((st (stat dir)))
            (if (or (not (eq? (stat:type st) 'directory))
                    (not (access? dir W_OK)))
                (error "directory not writable" dir))))
         ((eqv? errno ENOENT)
          (ensure-writable-dir (dirname dir))
          (ensure-writable-dir dir))
         (else
          (throw k subr fmt args rest)))))))

;;; This function is among the trickiest I've ever written. I tried many
;;; variants. In the end, simple is best, of course.
;;;
;;; After turning this around a number of times, it seems that the the
;;; desired behavior is that .go files should exist in a path, for
;;; searching. That is orthogonal to this function. For writing .go
;;; files, either you know where they should go, in which case you tell
;;; compile-file explicitly, as in the srcdir != builddir case; or you
;;; don't know, in which case this function is called, and we just put
;;; them in your own ccache dir in ~/.guile-ccache.
;;;
;;; See also boot-9.scm:load.
(define (compiled-file-name file)
  (define (compiled-extension)
    (cond ((or (null? %load-compiled-extensions)
               (string-null? (car %load-compiled-extensions)))
           (warn "invalid %load-compiled-extensions"
                 %load-compiled-extensions)
           ".go")
          (else (car %load-compiled-extensions))))
  (and %compile-fallback-path
       (let ((f (string-append
                 %compile-fallback-path
                 ;; no need for '/' separator here, canonicalize-path
                 ;; will give us an absolute path
                 (canonicalize-path file)
                 (compiled-extension))))
         (and (false-if-exception (ensure-writable-dir (dirname f)))
              f))))

(define* (compile-file file #:key
                       (output-file #f)
                       (from (current-language))
                       (to 'objcode)
                       (env (default-environment from))
                       (opts '())
                       (canonicalization 'relative))
  (with-fluids ((%file-port-name-canonicalization canonicalization))
    (let* ((comp (or output-file (compiled-file-name file)
                     (error "failed to create path for auto-compiled file"
                            file)))
           (in (open-input-file file))
           (enc (file-encoding in)))
      ;; Choose the input encoding deterministically.
      (set-port-encoding! in (or enc "UTF-8"))

      (ensure-writable-dir (dirname comp))
      (call-with-output-file/atomic comp
        (lambda (port)
          ((language-printer (ensure-language to))
           (read-and-compile in #:env env #:from from #:to to #:opts opts)
           port))
        file)
      comp)))

(define* (compile-and-load file #:key (from 'scheme) (to 'value)
                           (env (current-module)) (opts '())
                           (canonicalization 'relative))
  (with-fluids ((%file-port-name-canonicalization canonicalization))
    (read-and-compile (open-input-file file)
                      #:from from #:to to #:opts opts
                      #:env env)))


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
                           (from (current-language))
                           (to 'objcode)
                           (env (default-environment from))
                           (opts '()))
  (let ((from (ensure-language from))
        (to (ensure-language to)))
    (let ((joint (find-language-joint from to)))
      (with-fluids ((*current-language* from))
        (let lp ((exps '()) (env #f) (cenv env))
          (let ((x ((language-reader (current-language)) port cenv)))
            (cond
             ((eof-object? x)
              (compile ((language-joiner joint) (reverse exps) env)
                       #:from joint #:to to
                       ;; env can be false if no expressions were read.
                       #:env (or env (default-environment joint))
                       #:opts opts))
             (else
              ;; compile-fold instead of compile so we get the env too
              (receive (jexp jenv jcenv)
                  (compile-fold (compile-passes (current-language) joint opts)
                                x cenv opts)
                (lp (cons jexp exps) jenv jcenv))))))))))

(define* (compile x #:key
                  (from (current-language))
                  (to 'value)
                  (env (default-environment from))
                  (opts '()))

  (let ((warnings (memq #:warnings opts)))
    (if (pair? warnings)
        (let ((warnings (cadr warnings)))
          ;; Sanity-check the requested warnings.
          (for-each (lambda (w)
                      (or (lookup-warning-type w)
                          (warning 'unsupported-warning #f w)))
                    warnings))))

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
