;;; guile-emacs.scm --- Guile Emacs interface

;; Copyright (C) 2001 Keisuke Nishida <kxn30@po.cwru.edu>

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(use-modules (ice-9 regex))
(use-modules (ice-9 channel))
(use-modules (ice-9 session))
(use-modules (ice-9 documentation))


;;;
;;; Emacs Lisp channel
;;;

(define (emacs-lisp-channel)

  (define (native-type? x)
    (or (integer? x) (symbol? x) (string? x) (pair? x) (vector? x)))

  (define (emacs-lisp-print ch val)
    (cond
      ((unspecified? val))
      ((eq? val #t) (channel-print-value ch 't))
      ((or (eq? val #f) (null? val)) (channel-print-value ch 'nil))
      ((native-type? val) (channel-print-value ch val))
      (else (channel-print-token ch val))))

  (channel-open (make-object-channel emacs-lisp-print)))


;;;
;;; Scheme channel
;;;

(define (emacs-scheme-channel)
  (define (print ch val) (channel-print-value ch (object->string val)))
  (channel-open (make-object-channel print)))


;;;
;;; for guile-import and guile-use-modules
;;;

(define (guile-emacs-export-procedure name proc docs)
  (define (procedure-arity proc)
    (assq-ref (procedure-properties proc) 'arity))

  (define (procedure-args proc)
    (let ((source (procedure-source proc)))
      (if source
	;; formals -> emacs args
	(let loop ((formals (cadr source)))
	  (cond
	    ((null? formals) '())
	    ((symbol? formals) `(&rest ,formals))
	    (else (cons (car formals) (loop (cdr formals))))))
	;; arity -> emacs args
	(let* ((arity (procedure-arity proc))
	       (nreqs (car arity))
	       (nopts (cadr arity))
	       (restp (caddr arity)))
	  (define (nsyms n)
	    (if (= n 0) '() (cons (gensym "a") (nsyms (1- n)))))
	  (append! (nsyms nreqs)
		   (if (> nopts 0) (cons '&optional (nsyms nopts)) '())
		   (if restp (cons '&rest (nsyms 1)) '()))))))

  (define (procedure-call name args)
    (let ((restp (memq '&rest args))
	  (args (delq '&rest (delq '&optional args))))
      (if restp
	`('apply ',name ,@args)
	`(',name ,@args))))

  (let ((args (procedure-args proc))
	(docs (and docs (object-documentation proc))))
    `(defun ,name ,args
       ,@(if docs (list docs) '())
       (guile-lisp-flat-eval ,@(procedure-call (procedure-name proc) args)))))

(define (guile-emacs-export proc-name func-name docs)
  (let ((proc (module-ref (current-module) proc-name)))
    (guile-emacs-export-procedure func-name proc docs)))

(define (guile-emacs-export-procedures module-name docs)
  (define (module-public-procedures name)
    (hash-fold (lambda (s v d)
		 (let ((val (variable-ref v)))
		   (if (procedure? val) (acons s val d) d)))
	       '() (module-obarray (resolve-interface name))))
  `(progn ,@(map (lambda (n+p)
		   (guile-emacs-export-procedure (car n+p) (cdr n+p) docs))
		 (module-public-procedures module-name))))


;;;
;;; for guile-emacs-complete-symbol
;;;

(define (guile-emacs-complete-alist str)
  (sort! (apropos-fold (lambda (module name val data)
			 (cons (list (symbol->string name)
				     (cond ((procedure? val) " <p>")
					   ((macro? val)     " <m>")
					   (else "")))
			       data))
		       '() (string-append "^" (regexp-quote str))
		       apropos-fold-all)
	 (lambda (p1 p2) (string<? (car p1) (car p2)))))

;;;
;;; Guile 1.4 compatibility
;;;

(define object->string
  (if (defined? 'object->string)
    object->string
    (lambda (x) (format #f "~S" x))))

;;; guile-emacs.scm ends here
