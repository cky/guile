;;; Multi-language support

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

(define-module (system base language)
  :use-module (oop goops)
  :use-syntax (system base syntax)
  :use-module (system il compile)
  :use-module (system vm core)
  :use-module (system vm assemble)
  :use-module (ice-9 regex)
  :export (define-language lookup-language
	    read-in compile-in print-in compile-file-in load-file-in))


;;;
;;; Language class
;;;

(define-vm-class <language> ()
  name title version environment
  (reader)
  (expander (lambda (x) x))
  (translator (lambda (x) x))
  (evaler #f)
  (printer)
  )

(define-method (write (lang <language>) port)
  (display "#<language " port)
  (display lang.name port)
  (display ">"))

(define-macro (define-language name . spec)
  `(define ,name (,make ,<language> :name ',name ,@spec)))

(define (lookup-language name)
  (let ((m (resolve-module `(language ,name spec))))
    (if (module-bound? m name)
	(module-ref m name)
	(error "No such language:" name))))


;;;
;;; Evaluation interface
;;;

(define (read-in lang . port)
  (lang.reader (if (null? port) (current-input-port) (car port))))

(define (compile-in form env lang . opts)
  (catch 'result
    (lambda ()
      ;; expand
      (set! form (lang.expander form))
      (if (memq :e opts) (throw 'result form))
      ;; translate
      (set! form (lang.translator form))
      (if (memq :t opts) (throw 'result form))
      ;; compile
      (set! form (apply compile form env opts))
      (if (memq :c opts) (throw 'result form))
      ;; assemble
      (apply assemble form env opts))
    (lambda (key val) val)))

(define (print-in val lang . port)
  (lang.printer val (if (null? port) (current-output-port) (car port))))

(define (compile-file-in file env lang . opts)
  (let* ((code (call-with-input-file file
		 (lambda (in)
		   (do ((x (read-in lang in) (read-in lang in))
			(l '() (cons (lang.translator (lang.expander x)) l)))
		       ((eof-object? x) (reverse! l))))))
	 (asm (apply compile (cons '@begin code) env opts))
	 (bytes (apply assemble asm env opts)))
    (call-with-output-file (object-file-name file)
      (lambda (out) (uniform-vector-write bytes out)))))

(define (load-file-in file env lang . opts)
  (let ((compiled (object-file-name file)))
    (if (or (not (file-exists? compiled))
	    (> (stat:mtime (stat file)) (stat:mtime (stat compiled))))
	(compile-file-in file env lang))
    (call-with-input-file compiled
      (lambda (p)
	(let ((bytes (make-uniform-vector (stat:size (stat compiled)) #\a)))
	  (uniform-vector-read! bytes p)
	  bytes)))))

(define (object-file-name file)
  (let ((m (string-match "\\.[^.]*$" file)))
    (string-append (if m (match:prefix m) file) ".go")))
