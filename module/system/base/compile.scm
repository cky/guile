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
  :use-syntax (system base syntax)
  :use-module (system base language)
  :use-module (system il compile)
  :use-module (system il glil)
  :use-module (system vm core)
  :use-module (system vm assemble)
  :use-module (ice-9 regex))

;;;
;;; Compiler environment
;;;

(define-record (<cenv> vm language module))

(define-public (make-cenv . rest)
  (apply <cenv> rest))

(define-public (syntax-error loc msg exp)
  (throw 'syntax-error loc msg exp))

(define-public (call-with-compile-error-catch thunk)
  (try (thunk)
    ((syntax-error loc msg exp)
     (format #t "~A:~A: ~A: ~A" (car loc) (cdr loc) msg exp))))


;;;
;;; Compiler
;;;

(define scheme (lookup-language 'scheme))

(define-public (compile-file file . opts)
  (let ((comp (compiled-file-name file)))
    (catch #t
      (lambda ()
	(call-with-compile-error-catch
	 (lambda ()
	   (call-with-output-file comp
	     (lambda (port)
	       (let* ((source (read-file-in file scheme))
		      (objcode (apply compile-in source (current-module)
				      scheme opts)))
		 (if (memq :c opts)
		   (pprint-glil objcode port)
		   (uniform-array-write (objcode->string objcode) port)))))
	   (format #t "Wrote ~A\n" comp))))
      (lambda (key . args)
	(format #t "ERROR: During compiling ~A:\n" file)
	(display "ERROR: ")
	(apply format #t (cadr args) (caddr args))
	(newline)
	(format #t "ERROR: ~A ~A ~A\n" key (car args) (cadddr args))
	(delete-file comp)))))

(define-public (load-source-file file . opts)
  (let ((source (read-file-in file scheme)))
    (apply compile-in source (current-module) scheme opts)))

(define-public (load-file file . opts)
  (let ((comp (compiled-file-name file)))
    (if (file-exists? comp)
	(load-objcode comp)
	(apply load-source-file file opts))))

(define-public (compiled-file-name file)
  (let ((m (string-match "\\.[^.]*$" file)))
    (string-append (if m (match:prefix m) file) ".go")))

(define-public (scheme-eval x e)
  (vm-load (the-vm) (compile-in x e scheme)))


;;;
;;; Scheme compiler interface
;;;

(define-public (read-file-in file lang)
  (call-with-input-file file lang.read-file))

(define-public (compile-in x e lang . opts)
  (catch 'result
    (lambda ()
      ;; expand
      (set! x (lang.expander x e))
      (if (memq :e opts) (throw 'result x))
      ;; translate
      (set! x (lang.translator x e))
      (if (memq :t opts) (throw 'result x))
      ;; compile
      (set! x (apply compile x e opts))
      (if (memq :c opts) (throw 'result x))
      ;; assemble
      (apply assemble x e opts))
    (lambda (key val) val)))

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
