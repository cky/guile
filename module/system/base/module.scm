;;; Module system

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

(define-module (system base module)
  :use-module (oop goops)
  :use-syntax (system base syntax)
  :use-module (system base language)
  :use-module (ice-9 regex)
  :use-module (ice-9 common-list))

(define (expand-file-name file dir)
  (string-append dir "/" file))


;;;
;;; Environment
;;;

(dynamic-call "scm_init_envs" (dynamic-link "libguilevm.so"))

(export env-identifier)
(define-generic env-identifier)
(define-generic env-bound?)
(define-generic env-ref)
(define-generic env-set!)
(define-generic env-define)

(export global-ref)

(define (global-ref identifier)
  (let loop ((e *root-package*) (l (identifier->list identifier)))
    (cond ((null? l) e)
	  (else (loop (env-ref e (car l)) (cdr l))))))

(define (load-env identifier)
  (let ((x (global-ref identifier))) x.env))

(define (identifier->list identifier)
  (let loop ((s (symbol->string identifier)) (l '()))
    (let ((m (string-match "::" s)))
      (if m
	  (loop (match:suffix m) (cons (string->symbol (match:prefix m)) l))
	  (reverse! (cons (string->symbol s) l))))))

(define-public (identifier-split identifier)
  (let ((m (string-match "::([^:]*)$" (symbol->string identifier))))
    (if m
	(values (string->symbol (match:prefix m))
		(string->symbol (match:substring m 1)))
	(values #f identifier))))


;;;
;;; Modules
;;;

(define-vm-class <vmodule> (<env>)
  (env (make-env))
  (bootcode))

(export make-vmodule)

(define (make-vmodule)
  (make <vmodule>))

(define-method (env-identifier (m <vmodule>))
  (env-identifier m.env))

(define-method (env-define (m <vmodule>) (s <symbol>) v)
  (env-define m.env s v))


;;;
;;; Packages
;;;

(define *package-def* "GPKG.def")

(define-vm-class <package> (<env>)
  (env (make-env)))

(define-method (env-bound? (p <package>) (s <symbol>))
  (if (not (env-bound? p.env s))
      (cond ((package-lookup p s) => (lambda (v) (env-define p.env s v) #t))
	    (else #f))))

(define-method (env-ref (p <package>) (s <symbol>))
  (env-bound? p s)
  (env-ref p.env s))

(define-method (env-define (p <package>) (s <symbol>) (c <env>))
  (env-define p.env s c)
  (let ((id (cond ((env-identifier p.env) =>
		   (lambda (id)
		     (string->symbol (format #f "~A::~A" id s))))
		  (else s))))
    (set-env-identifier! c.env id)))

(define (try-load-package dir)
  (if (and (file-exists? dir) (file-is-directory? dir))
      (if (file-exists? (expand-file-name *package-def* dir))
	  (make-custom-package dir)
	  (make-plain-package dir))
      #f))

;; plain package

(define-vm-class <plain-package> (<package>)
  directory)

(define (make-plain-package dir)
  (make <plain-package> :directory dir))

(define-method (package-lookup (p <plain-package>) (s <symbol>))
  (let ((file (expand-file-name (string-downcase! (symbol->string s))
				p.directory)))
    (or (try-load-package file)
	(try-load-vmodule file))))

(define (try-load-vmodule file)
  (or (try-load-compiled-vmodule file)
      (try-load-source-vmodule file)))

(define (try-load-compiled-vmodule file) #f)

(define (try-load-source-vmodule file) #f)

;; custom package

(define-vm-class <custom-package> (<package>)
  directory name category version author modules)

(define (make-custom-package dir)
  (call-with-input-file (expand-file-name *package-def* dir)
    (lambda (p)
      (apply make <custom-package> :directory dir :name (cdr (read p))))))

(define-method (package-lookup (p <custom-package>) (s <symbol>))
  (and-let* ((entry (assq-ref p.modules s)))
    (let ((module (make-vmodule)))
      (env-define p s module)
      (let* ((file (expand-file-name (car entry) p.directory))
	     (code (load-file-in file module (lookup-language (cadr entry)))))
	(set! module.bootcode code))
      module)))

;; multi package

(define-vm-class <multi-package> (<package>)
  packages)

(define (make-multi-package dirs)
  (let ((packages (pick id (map try-load-package dirs))))
    (make <multi-package> :packages packages)))

(define-method (package-lookup (p <multi-package>) (s <symbol>))
  (list-fold (lambda (p d)
	       (let ((c (and (env-bound? p s) (env-ref p s))))
		 (if c (if d (error "Module name conflict" d c) c) d)))
	     #f p.packages))


;;;
;;; Guile old module
;;;

(define (import-old-module! m module)
  (hash-fold (lambda (k v d) (env-define m k (variable-ref v)))
	     #f (module-obarray module)))


;;;
;;; Current modules
;;;

(export current-vmodule set-current-vmodule!
	current-evaluator set-current-evaluator!)

(define *current-module* #f)
(define (current-vmodule) *current-module*)
(define (set-current-vmodule! m) (set! *current-module* m))

(define *current-evaluator* #f)
(define (current-evaluator) *current-evaluator*)
(define (set-current-evaluator! e) (set! *current-evaluator* e))


;;;
;;; Standard modules/packages
;;;

(define *root-package*
  (make-multi-package '("/usr/local/share/guile/site")))

(let ((user (make-vmodule)))
  (env-define *root-package* 'user user))

(let ((core (make-vmodule)))
  (env-define *root-package* 'core core)
  (hash-fold (lambda (s v d) (env-define core s v)) #f (builtin-bindings)))
