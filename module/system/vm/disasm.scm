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
    (disassemble-bytecode bytes #f)))

(define (disassemble-program prog . opts)
  (let* ((arity (program-arity prog))
	 (nargs (arity:nargs arity))
	 (nrest (arity:nrest arity))
	 (nlocs (arity:nlocs arity))
	 (nexts (arity:nexts arity))
	 (bytes (program-bytecode prog))
	 (objs  (program-objects prog))
	 (meta  (program-meta prog))
	 (exts  (program-external prog)))
    ;; Disassemble this bytecode
    (format #t "Disassembly of ~A:\n\n" prog)
    (format #t "nargs = ~A  nrest = ~A  nlocs = ~A  nexts = ~A\n\n"
	    nargs nrest nlocs nexts)
    (format #t "Bytecode:\n\n")
    (disassemble-bytecode bytes objs)
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

(define (disassemble-bytecode bytes objs)
  (let ((decode (make-byte-decoder bytes))
        (programs '()))
    (define (lp addr code)
      (pmatch code
       (#f (newline))
       ((load-program ,x)
        (let ((sym (gensym "")))
          (set! programs (acons sym x programs))
          (print-info addr (format #f "(load-program #~A)" sym) #f)))
       (else
        (print-info addr (list->info code)
                    (original-value addr code objs))))
      (if code (call-with-values decode lp)))
    (call-with-values decode lp)
    (for-each (lambda (sym+bytes)
                (format #t "Bytecode #~A:\n\n" (car sym+bytes))
                (disassemble-bytecode (cdr sym+bytes) #f))
              (reverse! programs))))

(define (disassemble-objects objs)
  (display "Objects:\n\n")
  (let ((len (vector-length objs)))
    (do ((n 0 (1+ n)))
	((= n len) (newline))
      (let ((info (object->string (vector-ref objs n))))
	(print-info n info #f)))))

(define (disassemble-externals exts)
  (display "Externals:\n\n")
  (let ((len (length exts)))
    (do ((n 0 (1+ n))
	 (l exts (cdr l)))
	((null? l) (newline))
      (let ((info (object->string (car l))))
	(print-info n info #f)))))

(define-macro (unless test . body)
  `(if (not ,test) (begin ,@body)))

(define (disassemble-bindings prog bindings)
  (let* ((nargs (arity:nargs (program-arity prog)))
         (args (if (zero? nargs) '() (cdar bindings)))
         (nonargs (if (zero? nargs) bindings (cdr bindings))))
    (unless (null? args)
            (display "Arguments:\n\n")
            (for-each (lambda (bind n)
                        (print-info n
                                    (format #f "~a[~a]: ~a"
                                            (if (cadr bind) 'external 'local)
                                            (caddr bind) (car bind))
                                    #f))
                      args
                      (iota nargs))
            (newline))
    (unless (null? nonargs)
            (display "Bindings:\n\n")
            (for-each (lambda (start binds end)
                        (for-each (lambda (bind)
                                    (print-info (format #f "~a-~a" start end)
                                                (format #f "~a[~a]: ~a"
                                                        (if (cadr bind) 'external 'local)
                                                        (caddr bind) (car bind))
                                                #f))
                                  binds))
                      (map car (filter cdr nonargs))
                      (map cdr (filter cdr nonargs))
                      (map car (filter (lambda (x) (not (cdr x))) nonargs)))
            (newline))))

(define (disassemble-meta program meta)
  (let ((bindings (car meta))
        (sources (cadr meta))
        (props (cddr meta)))
    (unless (null? bindings)
            (disassemble-bindings program bindings))
    (unless (null? sources)
            (display "Sources:\n\n")
            (for-each (lambda (x)
                        (print-info (car x) (list->info (cdr x)) #f))
                      sources)
            (newline))
    (unless (null? props)
            (display "Properties:\n\n")
            (for-each (lambda (x) (print-info #f x #f)) props)
            (newline))))

(define (original-value addr code objs)
  (let* ((code (code-unpack code))
         (inst (car code))
         (args (cdr code)))
    (case inst
      ((list vector) 
       (let ((len (+ (* (cadr code) 256) (caddr code))))
         (format #f "~a element~a" len (if (> len 1) "s" ""))))
      ((br br-if br-if-eq br-if-not br-if-not-eq br-if-not-null br-if-null)
       (let ((offset (+ (* (car args) 256) (cadr args))))
         (format #f "-> ~A" (+ addr offset 3))))
      ((object-ref)
       (if objs (object->string (vector-ref objs (car args))) #f))
      ((mv-call)
       (let ((offset (+ (* (caddr code) 256) (cadddr code))))
         (format #f "MV -> ~A" (+ addr offset 4))))
      (else
       (and=> (code->object code) object->string)))))

(define (list->info list)
  (object->string list))

;   (define (u8vector->string vec)
;     (list->string (map integer->char (u8vector->list vec))))

;   (case (car list)
;     ((link)
;      (object->string `(link ,(u8vector->string (cadr list)))))
;     (else
;      (object->string list))))

(define (print-info addr info extra)
  (if extra
      (format #t "~4@A    ~32A;; ~A\n" addr info extra)
      (format #t "~4@A    ~A\n" addr info)))

(define (simplify x)
  (cond ((string? x)
	 (cond ((string-index x #\newline) =>
		(lambda (i) (set! x (substring x 0 i)))))
	 (cond ((> (string-length x) 16)
		(set! x (string-append (substring x 0 13) "..."))))))
  x)
