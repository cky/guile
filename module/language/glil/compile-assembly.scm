;;; Guile VM assembler

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language glil compile-assembly)
  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (language glil)
  #:use-module (language assembly)
  #:use-module (system vm instruction)
  #:use-module ((system vm program) #:select (make-binding))
  #:use-module (ice-9 receive)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (rnrs bytevector)
  #:export (compile-assembly))

;; Variable cache cells go in the object table, and serialize as their
;; keys. The reason we wrap the keys in these records is so they don't
;; compare as `equal?' to other objects in the object table.
;;
;; `key' is either a symbol or the list (MODNAME SYM PUBLIC?)

(define-record <variable-cache-cell> key)

;; Subprograms can be loaded into an object table as well. We need a
;; disjoint type here too. (Subprograms have their own object tables --
;; though probably we should just make one table per compilation unit.)

(define-record <subprogram> table prog)


(define (limn-sources sources)
  (let lp ((in sources) (out '()) (filename #f))
    (if (null? in)
        (reverse! out)
        (let ((addr (caar in))
              (new-filename (assq-ref (cdar in ) 'filename))
              (line (assq-ref (cdar in) 'line))
              (column (assq-ref (cdar in) 'column)))
          (cond
           ((not (equal? new-filename filename))
            (lp (cdr in)
                `((,addr . (,line . ,column))
                  (filename . ,new-filename)
                  . ,out)
                new-filename))
           ((or (null? out) (not (equal? (cdar out) `(,line . ,column))))
            (lp (cdr in)
                `((,addr . (,line . ,column))
                  . ,out)
                filename))
           (else
            (lp (cdr in) out filename)))))))

(define (make-meta bindings sources tail)
  (if (and (null? bindings) (null? sources) (null? tail))
      #f
      (compile-assembly
       (make-glil-program 0 0 0 0 '()
                          (list
                           (make-glil-const `(,bindings ,sources ,@tail))
                           (make-glil-call 'return 1))))))

;; A functional stack of names of live variables.
(define (make-open-binding name ext? index)
  (list name ext? index))
(define (make-closed-binding open-binding start end)
  (make-binding (car open-binding) (cadr open-binding)
                (caddr open-binding) start end))
(define (open-binding bindings vars start)
  (cons
   (acons start
          (map
           (lambda (v)
             (pmatch v
               ((,name local ,i) (make-open-binding name #f i))
               ((,name external ,i) (make-open-binding name #t i))
               (else (error "unknown binding type" v))))
           vars)
          (car bindings))
   (cdr bindings)))
(define (close-binding bindings end)
  (pmatch bindings
    ((((,start . ,closing) . ,open) . ,closed)
     (cons open
           (fold (lambda (o tail)
                   ;; the cons is for dsu sort
                   (acons start (make-closed-binding o start end)
                          tail))
                 closed
                 closing)))
    (else (error "broken bindings" bindings))))
(define (close-all-bindings bindings end)
  (if (null? (car bindings))
      (map cdr
           (stable-sort (reverse (cdr bindings))
                        (lambda (x y) (< (car x) (car y)))))
      (close-all-bindings (close-binding bindings end) end)))

;; A functional object table.
(define *module* 1)
(define (assoc-ref-or-acons alist x make-y)
  (cond ((assoc-ref alist x)
         => (lambda (y) (values y alist)))
        (else
         (let ((y (make-y x alist)))
           (values y (acons x y alist))))))
(define (object-index-and-alist x alist)
  (assoc-ref-or-acons alist x
                      (lambda (x alist)
                        (+ (length alist) *module*))))

(define (compile-assembly glil)
  (receive (code . _)
      (glil->assembly glil '() '(()) '() '() #f -1)
    (car code)))
(define (make-object-table objects)
  (and (not (null? objects))
       (list->vector (cons #f objects))))

(define (glil->assembly glil nexts-stack bindings
                        source-alist label-alist object-alist addr)
  (define (emit-code x)
    (values (map assembly-pack x) bindings source-alist label-alist object-alist))
  (define (emit-code/object x object-alist)
    (values (map assembly-pack x) bindings source-alist label-alist object-alist))

  (record-case glil
    ((<glil-program> nargs nrest nlocs nexts meta body closure-level)
     (let ((toplevel? (null? nexts-stack)))
       (define (process-body)
         (let ((nexts-stack (cons nexts nexts-stack)))
           (let lp ((body body) (code '()) (bindings '(())) (source-alist '())
                    (label-alist '()) (object-alist (if toplevel? #f '())) (addr 0))
             (cond
              ((null? body)
               (values (reverse code)
                       (close-all-bindings bindings addr)
                       (limn-sources (reverse! source-alist))
                       (reverse label-alist)
                       (and object-alist (map car (reverse object-alist)))
                       addr))
              (else
               (receive (subcode bindings source-alist label-alist object-alist)
                   (glil->assembly (car body) nexts-stack bindings
                                   source-alist label-alist object-alist addr)
                 (lp (cdr body) (append (reverse subcode) code)
                     bindings source-alist label-alist object-alist
                     (addr+ addr subcode))))))))

       (receive (code bindings sources labels objects len)
           (process-body)
         (let ((prog `(load-program ,nargs ,nrest ,nlocs ,nexts ,labels
                                    ,len
                                    ,(make-meta bindings sources meta)
                                    . ,code)))
           (cond
            (toplevel?
             ;; toplevel bytecode isn't loaded by the vm, no way to do
             ;; object table or closure capture (not in the bytecode,
             ;; anyway)
             (emit-code (align-program prog addr)))
            (else
             (let ((table (dump-object (make-object-table objects) addr))
                   (closure (if (> closure-level 0) '((make-closure)) '())))
               (cond
                (object-alist
                 ;; if we are being compiled from something with an object
                 ;; table, cache the program there
                 (receive (i object-alist)
                     (object-index-and-alist (make-subprogram table prog)
                                             object-alist)
                   (emit-code/object `(,(if (< i 256)
                                            `(object-ref ,i)
                                            `(long-object-ref ,(quotient i 256)
                                                              ,(modulo i 256)))
                                       ,@closure)
                                     object-alist)))
                (else
                 ;; otherwise emit a load directly
                 (emit-code `(,@table ,@(align-program prog (addr+ addr table))
                                      ,@closure)))))))))))
    
    ((<glil-bind> vars)
     (values '()
             (open-binding bindings vars addr)
             source-alist
             label-alist
             object-alist))

    ((<glil-mv-bind> vars rest)
     (values `((truncate-values ,(length vars) ,(if rest 1 0)))
             (open-binding bindings vars addr)
             source-alist
             label-alist
             object-alist))

    ((<glil-unbind>)
     (values '()
             (close-binding bindings addr)
             source-alist
             label-alist
             object-alist))
             
    ((<glil-source> props)
     (values '()
             bindings
             (acons addr props source-alist)
             label-alist
             object-alist))

    ((<glil-void>)
     (emit-code '((void))))

    ((<glil-const> obj)
     (cond
      ((object->assembly obj)
       => (lambda (code)
            (emit-code (list code))))
      ((not object-alist)
       (emit-code (dump-object obj addr)))
      (else
       (receive (i object-alist)
           (object-index-and-alist obj object-alist)
         (emit-code/object (if (< i 256)
                               `((object-ref ,i))
                               `((long-object-ref ,(quotient i 256)
                                                  ,(modulo i 256))))
                           object-alist)))))

    ((<glil-local> op index)
     (emit-code (if (eq? op 'ref)
                    `((local-ref ,index))
                    `((local-set ,index)))))

    ((<glil-external> op depth index)
     (emit-code (let lp ((d depth) (n 0) (stack nexts-stack))
                  (if (> d 0)
                      (lp (1- d) (+ n (car stack)) (cdr stack))
                      (if (eq? op 'ref)
                          `((external-ref ,(+ n index)))
                          `((external-set ,(+ n index))))))))

    ((<glil-toplevel> op name)
     (case op
       ((ref set)
        (cond
         ((not object-alist)
          (emit-code `(,@(dump-object name addr)
                       (link-now)
                       ,(case op 
                          ((ref) '(variable-ref))
                          ((set) '(variable-set))))))
         (else
          (receive (i object-alist)
              (object-index-and-alist (make-variable-cache-cell name)
                                      object-alist)
            (emit-code/object (if (< i 256)
                                  `((,(case op
                                        ((ref) 'toplevel-ref)
                                        ((set) 'toplevel-set))
                                     ,i))
                                  `((,(case op
                                        ((ref) 'long-toplevel-ref)
                                        ((set) 'long-toplevel-set))
                                     ,(quotient i 256)
                                     ,(modulo i 256))))
                              object-alist)))))
       ((define)
        (emit-code `((define ,(symbol->string name))
                     (variable-set))))
       (else
        (error "unknown toplevel var kind" op name))))

    ((<glil-module> op mod name public?)
     (let ((key (list mod name public?)))
       (case op
         ((ref set)
          (cond
           ((not object-alist)
            (emit-code `(,@(dump-object key addr)
                         (link-now)
                         ,(case op 
                            ((ref) '(variable-ref))
                            ((set) '(variable-set))))))
           (else
            (receive (i object-alist)
                (object-index-and-alist (make-variable-cache-cell key)
                                        object-alist)
              (emit-code/object (case op
                                  ((ref) `((toplevel-ref ,i)))
                                  ((set) `((toplevel-set ,i))))
                                object-alist)))))
         (else
          (error "unknown module var kind" op key)))))

    ((<glil-label> label)
     (values '()
             bindings
             source-alist
             (acons label addr label-alist)
             object-alist))

    ((<glil-branch> inst label)
     (emit-code `((,inst ,label))))

    ;; nargs is number of stack args to insn. probably should rename.
    ((<glil-call> inst nargs)
     (if (not (instruction? inst))
         (error "Unknown instruction:" inst))
     (let ((pops (instruction-pops inst)))
       (cond ((< pops 0)
              (case (instruction-length inst)
                ((1) (emit-code `((,inst ,nargs))))
                ((2) (emit-code `((,inst ,(quotient nargs 256)
                                         ,(modulo nargs 256)))))
                (else (error "Unknown length for variable-arg instruction:"
                             inst (instruction-length inst)))))
             ((= pops nargs)
              (emit-code `((,inst))))
             (else
              (error "Wrong number of stack arguments to instruction:" inst nargs)))))

    ((<glil-mv-call> nargs ra)
     (emit-code `((mv-call ,nargs ,ra))))))

(define (dump-object x addr)
  (define (too-long x)
    (error (string-append x " too long")))

  (cond
   ((object->assembly x) => list)
   ((variable-cache-cell? x) (dump-object (variable-cache-cell-key x) addr))
   ((subprogram? x)
    `(,@(subprogram-table x)
      ,@(align-program (subprogram-prog x)
                       (addr+ addr (subprogram-table x)))))
   ((number? x)
    `((load-number ,(number->string x))))
   ((string? x)
    `((load-string ,x)))
   ((symbol? x)
    `((load-symbol ,(symbol->string x))))
   ((keyword? x)
    `((load-keyword ,(symbol->string (keyword->symbol x)))))
   ((list? x)
    (let ((tail (let ((len (length x)))
                  (if (>= len 65536) (too-long "list"))
                  `((list ,(quotient len 256) ,(modulo len 256))))))
      (let dump-objects ((objects x) (codes '()) (addr addr))
        (if (null? objects)
            (fold append tail codes)
            (let ((code (dump-object (car objects) addr)))
              (dump-objects (cdr objects) (cons code codes)
                            (addr+ addr code)))))))
   ((pair? x)
    (let ((kar (dump-object (car x) addr)))
      `(,@kar
        ,@(dump-object (cdr x) (addr+ addr kar))
        (cons))))
   ((vector? x)
    (let* ((len (vector-length x))
           (tail (if (>= len 65536)
                     (too-long "vector")
                     `((vector ,(quotient len 256) ,(modulo len 256))))))
      (let dump-objects ((i 0) (codes '()) (addr addr))
        (if (>= i len)
            (fold append tail codes)
            (let ((code (dump-object (vector-ref x i) addr)))
              (dump-objects (1+ i) (cons code codes)
                            (addr+ addr code)))))))
   ((and (array? x) (symbol? (array-type x)))
    (let* ((type (dump-object (array-type x) addr))
           (shape (dump-object (array-shape x) (addr+ addr type))))
      `(,@type
        ,@shape
        ,@(align-code
           `(load-array ,(uniform-array->bytevector x))
           (addr+ (addr+ addr type) shape)
           8
           4))))
   (else
    (error "assemble: unrecognized object" x))))

