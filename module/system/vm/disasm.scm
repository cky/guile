;;; Guile VM Disassembler

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

(define-module (system vm disasm)
  #:use-module (system base pmatch)
  #:use-module (system vm objcode)
  #:use-module (system vm program)
  #:use-module (system vm conv)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:export (disassemble-objcode disassemble-program disassemble-bytecode))

(define (disassemble-objcode objcode . opts)
  (let* ((prog  (objcode->program objcode))
	 (arity (program-arity prog))
	 (nlocs (arity:nlocs arity))
	 (nexts (arity:nexts arity))
	 (bytes (program-bytecode prog)))
    (format #t "Disassembly of ~A:\n\n" objcode)
    (format #t "nlocs = ~A  nexts = ~A\n\n" nlocs nexts)
    (disassemble-bytecode bytes #f 0 #f #f '())))

(define (disassemble-program prog . opts)
  (let* ((arity (program-arity prog))
	 (nargs (arity:nargs arity))
	 (nrest (arity:nrest arity))
	 (nlocs (arity:nlocs arity))
	 (nexts (arity:nexts arity))
	 (bytes (program-bytecode prog))
	 (objs  (program-objects prog))
	 (meta  (program-meta prog))
	 (exts  (program-external prog))
         (binds (program-bindings prog))
         (blocs (and binds
                     (filter (lambda (x) (not (binding:extp x))) binds)))
         (bexts (and binds
                     (filter binding:extp binds)))
         (srcs  (program-sources prog)))
    ;; Disassemble this bytecode
    (format #t "Disassembly of ~A:\n\n" prog)
    (format #t "nargs = ~A  nrest = ~A  nlocs = ~A  nexts = ~A\n\n"
	    nargs nrest nlocs nexts)
    (format #t "Bytecode:\n\n")
    (disassemble-bytecode bytes objs nargs blocs bexts srcs)
    (if (> (vector-length objs) 0)
	(disassemble-objects objs))
    (if (pair? exts)
	(disassemble-externals exts))
    (if meta
	(disassemble-meta prog (meta)))
    ;; Disassemble other bytecode in it
    (for-each
     (lambda (x)
       (if (program? x)
	   (begin (display "----------------------------------------\n")
		  (apply disassemble-program x opts))))
     (vector->list objs))))

(define (disassemble-bytecode bytes objs nargs blocs bexts sources)
  (let ((decode (make-byte-decoder bytes))
        (programs '()))
    (define (lp start end code)
      (pmatch code
       (#f (newline))
       ((load-program ,x)
        (let ((sym (gensym "")))
          (set! programs (acons sym x programs))
          (print-info start `(load-program ,sym) #f #f)))
       (else
        (print-info start code
                    (code-annotation end code objs nargs blocs bexts)
                    (and=> (assq end sources) source->string))))
      (if code (call-with-values decode lp)))
    (call-with-values decode lp)
    (for-each (lambda (sym+bytes)
                (format #t "Bytecode #~A:\n\n" (car sym+bytes))
                (disassemble-bytecode (cdr sym+bytes) #f 0 #f #f '()))
              (reverse! programs))))

(define (disassemble-objects objs)
  (display "Objects:\n\n")
  (let ((len (vector-length objs)))
    (do ((n 0 (1+ n)))
	((= n len) (newline))
      (print-info n (vector-ref objs n) #f #f))))

(define (disassemble-externals exts)
  (display "Externals:\n\n")
  (let ((len (length exts)))
    (do ((n 0 (1+ n))
	 (l exts (cdr l)))
	((null? l) (newline))
      (print-info n (car l) #f))))

(define-macro (unless test . body)
  `(if (not ,test) (begin ,@body)))

(define *uninteresting-props* '(name))

(define (disassemble-meta program meta)
  (let ((sources (cadr meta))
        (props (filter (lambda (x)
                         (not (memq (car x) *uninteresting-props*)))
                       (cddr meta))))
    (unless (null? props)
            (display "Properties:\n\n")
            (for-each (lambda (x) (print-info #f x #f #f)) props)
            (newline))))

(define (source->string src)
  (format #f "~a:~a:~a" (or (source:file src) "(unknown file)")
          (source:line src) (source:column src)))

(define (make-int16 byte1 byte2)
  (+ (* byte1 256) byte2))

(define (code-annotation end-addr code objs nargs blocs bexts)
  (let* ((code (code-unpack code))
         (inst (car code))
         (args (cdr code)))
    (case inst
      ((list vector) 
       (list "~a element~:p" (apply make-int16 args)))
      ((br br-if br-if-eq br-if-not br-if-not-eq br-if-not-null br-if-null)
       (list "-> ~A" (+ end-addr (apply make-int16 args))))
      ((object-ref)
       (and objs (list "~s" (vector-ref objs (car args)))))
      ((local-ref local-set)
       (and blocs
            (let ((b (list-ref blocs (car args))))
              (list "`~a'~@[ (arg)~]"
                    (binding:name b) (< (binding:index b) nargs)))))
      ((external-ref external-set)
       (and bexts
            (let ((b (list-ref bexts (car args))))
              (list "`~a'~@[ (arg)~]"
                    (binding:name b) (< (binding:index b) nargs)))))
      ((mv-call)
       (list "MV -> ~A" (+ end-addr (apply make-int16 args))))
      (else
       (and=> (code->object code)
              (lambda (obj) (list "~s" obj)))))))

;; i am format's daddy.
(define (print-info addr info extra src)
  (format #t "~4@S    ~32S~@[;; ~1{~@?~}~]~@[~61t at ~a~]\n" addr info extra src))

(define (simplify x)
  (cond ((string? x)
	 (cond ((string-index x #\newline) =>
		(lambda (i) (set! x (substring x 0 i)))))
	 (cond ((> (string-length x) 16)
		(set! x (string-append (substring x 0 13) "..."))))))
  x)
