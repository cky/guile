;;; run-vm-tests.scm -- Run Guile-VM's test suite.
;;;
;;; Copyright 2005, 2009, 2010 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA


(use-modules (system vm vm)
             (system vm program)
	     (system base compile)
	     (system base language)
             (language scheme spec)
             (language objcode spec)
	     (srfi srfi-1)
	     (ice-9 r5rs))


(define (fetch-sexp-from-file file)
  (with-input-from-file file
    (lambda ()
      (let loop ((sexp (read))
		 (result '()))
	(if (eof-object? sexp)
	    (cons 'begin (reverse result))
	    (loop (read) (cons sexp result)))))))

(define (compile-to-objcode sexp)
  "Compile the expression @var{sexp} into a VM program and return it."
  (compile sexp #:from scheme #:to objcode))

(define (run-vm-program objcode)
  "Run VM program contained into @var{objcode}."
  ((make-program objcode)))

(define (compile/run-test-from-file file)
  "Run test from source file @var{file} and return a value indicating whether
it succeeded."
  (run-vm-program (compile-to-objcode (fetch-sexp-from-file file))))


(define-macro (watch-proc proc-name str)
  `(let ((orig-proc ,proc-name))
     (set! ,proc-name
	   (lambda args
	     (format #t (string-append ,str "...  "))
	     (apply orig-proc args)))))

(watch-proc fetch-sexp-from-file  "reading")
(watch-proc compile-to-objcode    "compiling")
(watch-proc run-vm-program        "running")


;; The program.

(define (run-vm-tests files)
  "For each file listed in @var{files}, load it and run it through both the
interpreter and the VM (after having it compiled).  Both results must be
equal in the sense of @code{equal?}."
  (let* ((res (map (lambda (file)
		     (format #t "running `~a'...  " file)
		     (if (catch #t
				(lambda ()
				  (equal? (compile/run-test-from-file file)
					  (primitive-eval (fetch-sexp-from-file file))))
				(lambda (key . args)
				  (format #t "[~a/~a] " key args)
				  #f))
			 (format #t "ok~%")
			 (begin (format #t "FAILED~%") #f)))
		   files))
	 (total (length files))
	 (failed (length (filter not res))))

    (if (= 0 failed)
        (exit 0)
	(begin
	  (format #t "~%~a tests failed out of ~a~%"
		  failed total)
	  (exit failed)))))

