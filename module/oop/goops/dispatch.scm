;;;; 	Copyright (C) 1999, 2000, 2001, 2003, 2006, 2009 Free Software Foundation, Inc.
;;;;
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
;;;;


;; There are circularities here; you can't import (oop goops compile)
;; before (oop goops). So when compiling, make sure that things are
;; kosher.
(eval-when (compile) (resolve-module '(oop goops)))

(define-module (oop goops dispatch)
  :use-module (oop goops)
  :use-module (oop goops util)
  :use-module (oop goops compile)
  :export (memoize-method!)
  :no-backtrace
  )

(define *dispatch-module* (current-module))

;;;
;;; Generic functions have an applicable-methods cache associated with
;;; them. Every distinct set of types that is dispatched through a
;;; generic adds an entry to the cache. This cache gets compiled out to
;;; a dispatch procedure. In steady-state, this dispatch procedure is
;;; never recompiled; but during warm-up there is some churn, both to
;;; the cache and to the dispatch procedure.
;;;
;;; So what is the deal if warm-up happens in a multithreaded context?
;;; There is indeed a window between missing the cache for a certain set
;;; of arguments, and then updating the cache with the newly computed
;;; applicable methods. One of the updaters is liable to lose their new
;;; entry.
;;;
;;; This is actually OK though, because a subsequent cache miss for the
;;; race loser will just cause memoization to try again. The cache will
;;; eventually be consistent. We're not mutating the old part of the
;;; cache, just consing on the new entry.
;;;
;;; It doesn't even matter if the dispatch procedure and the cache are
;;; inconsistent -- most likely the type-set that lost the dispatch
;;; procedure race will simply re-trigger a memoization, but since the
;;; winner isn't in the effective-methods cache, it will likely also
;;; re-trigger a memoization, and the cache will finally be consistent.
;;; As you can see there is a possibility for ping-pong effects, but
;;; it's unlikely given the shortness of the window between slot-set!
;;; invocations. We could add a mutex, but it is strictly unnecessary,
;;; and would add runtime cost and complexity.
;;;

(define (emit-linear-dispatch gf-sym nargs methods free rest?)
  (define (gen-syms n stem)
    (let lp ((n (1- n)) (syms '()))
      (if (< n 0)
          syms
          (lp (1- n) (cons (gensym stem) syms)))))
  (let* ((args (gen-syms nargs "a"))
         (types (gen-syms nargs "t")))
    (let lp ((methods methods)
             (free free)
             (exp `(cache-miss ,gf-sym
                               ,(if rest?
                                    `(cons* ,@args rest)
                                    `(list ,@args)))))
      (cond
       ((null? methods)
        (values `(,(if rest? `(,@args . rest) args)
                  (let ,(map (lambda (t a)
                               `(,t (class-of ,a)))
                             types args)
                    ,exp))
                free))
       (else
        ;; jeez
        (let preddy ((free free)
                     (types types)
                     (specs (vector-ref (car methods) 1))
                     (checks '()))
          (if (null? types)
              (let ((m-sym (gensym "p")))
                (lp (cdr methods)
                    (acons (vector-ref (car methods) 3)
                           m-sym
                           free)
                    `(if (and . ,checks)
                         ,(if rest?
                              `(apply ,m-sym ,@args rest)
                              `(,m-sym . ,args))
                         ,exp)))
              (let ((var (assq-ref free (car specs))))
                (if var
                    (preddy free
                            (cdr types)
                            (cdr specs)
                            (cons `(eq? ,(car types) ,var)
                                  checks))
                    (let ((var (gensym "c")))
                      (preddy (acons (car specs) var free)
                              (cdr types)
                              (cdr specs)
                              (cons `(eq? ,(car types) ,var)
                                    checks))))))))))))

(define (compute-dispatch-procedure gf cache)
  (define (scan)
    (let lp ((ls cache) (nreq -1) (nrest -1))
      (cond
       ((null? ls)
        (collate (make-vector (1+ nreq) '())
                 (make-vector (1+ nrest) '())))
       ((vector-ref (car ls) 2)         ; rest
        (lp (cdr ls) nreq (max nrest (vector-ref (car ls) 0))))
       (else                            ; req
        (lp (cdr ls) (max nreq (vector-ref (car ls) 0)) nrest)))))
  (define (collate req rest)
    (let lp ((ls cache))
      (cond
       ((null? ls)
        (emit req rest))
       ((vector-ref (car ls) 2)         ; rest
        (let ((n (vector-ref (car ls) 0)))
          (vector-set! rest n (cons (car ls) (vector-ref rest n)))
          (lp (cdr ls))))
       (else                            ; req
        (let ((n (vector-ref (car ls) 0)))
          (vector-set! req n (cons (car ls) (vector-ref req n)))
          (lp (cdr ls)))))))
  (define (emit req rest)
    (let ((gf-sym (gensym "g")))
      (define (emit-rest n clauses free)
        (if (< n (vector-length rest))
            (let ((methods (vector-ref rest n)))
              (cond
               ((null? methods)
                (emit-rest (1+ n) clauses free))
               ;; FIXME: hash dispatch
               (else
                (call-with-values
                    (lambda ()
                      (emit-linear-dispatch gf-sym n methods free #t))
                  (lambda (clause free)
                    (emit-rest (1+ n) (cons clause clauses) free))))))
            (emit-req (1- (vector-length req)) clauses free)))
      (define (emit-req n clauses free)
        (if (< n 0)
            (comp `(lambda ,(map cdr free)
                     (case-lambda ,@clauses))
                  (map car free))
            (let ((methods (vector-ref req n)))
              (cond
               ((null? methods)
                (emit-req (1- n) clauses free))
               ;; FIXME: hash dispatch
               (else
                (call-with-values
                    (lambda ()
                      (emit-linear-dispatch gf-sym n methods free #f))
                  (lambda (clause free)
                    (emit-req (1- n) (cons clause clauses) free))))))))

      (emit-rest 0
                 (if (or (zero? (vector-length rest))
                         (null? (vector-ref rest 0)))
                     (list `(args (cache-miss ,gf-sym args)))
                     '())
                 (acons gf gf-sym '()))))
  (define (comp exp vals)
    (let ((p ((@ (system base compile) compile) exp #:env *dispatch-module*)))
      (apply p vals)))
  
  ;; kick it.
  (scan))

;; o/~  ten, nine, eight
;;        sometimes that's just how it goes
;;          three, two, one
;;
;;            get out before it blows    o/~
;;
(define timer-init 10)
(define (delayed-compile gf)
  (let ((timer timer-init))
    (lambda args
      (set! timer (1- timer))
      (cond
       ((zero? timer)
        (let ((dispatch (compute-dispatch-procedure
                         gf (slot-ref gf 'effective-methods))))
          (slot-set! gf 'procedure dispatch)
          (apply dispatch args)))
       (else
        ;; interestingly, this catches recursive compilation attempts as
        ;; well; in that case, timer is negative
        (cache-dispatch gf args))))))

(define (cache-dispatch gf args)
  (define (map-until n f ls)
    (if (or (zero? n) (null? ls))
        '()
        (cons (f (car ls)) (map-until (1- n) f (cdr ls)))))
  (define (equal? x y) ; can't use the stock equal? because it's a generic...
    (cond ((pair? x) (and (pair? y)
                          (eq? (car x) (car y))
                          (equal? (cdr x) (cdr y))))
          ((null? x) (null? y))
          (else #f)))
  (if (slot-ref gf 'n-specialized)
      (let ((types (map-until (slot-ref gf 'n-specialized) class-of args)))
        (let lp ((cache (slot-ref gf 'effective-methods)))
          (cond ((null? cache)
                 (cache-miss gf args))
                ((equal? (vector-ref (car cache) 1) types)
                 (apply (vector-ref (car cache) 3) args))
                (else (lp (cdr cache))))))
      (cache-miss gf args)))

(define (cache-miss gf args)
  (apply (memoize-method! gf args (slot-ref gf '%cache)) args))

(define (memoize-effective-method! gf args applicable)
  (define (first-n ls n)
    (if (or (zero? n) (null? ls))
        '()
        (cons (car ls) (first-n (cdr ls) (- n 1)))))
  (define (parse n ls)
    (cond ((null? ls)
           (memoize n #f (map class-of args)))
          ((= n (slot-ref gf 'n-specialized))
           (memoize n #t (map class-of (first-n args n))))
          (else
           (parse (1+ n) (cdr ls)))))
  (define (memoize len rest? types)
    (let* ((cmethod (compute-cmethod applicable types))
           (cache (cons (vector len types rest? cmethod)
                        (slot-ref gf 'effective-methods))))
      (slot-set! gf 'effective-methods cache)
      (slot-set! gf 'procedure (delayed-compile gf))
      cmethod))
  (parse 0 args))


;;;
;;; This file implements method memoization.  It will finally be
;;; implemented on C level in order to obtain fast generic function
;;; application also during the first pass through the code.
;;;

;;;
;;; Constants
;;;

(define hashsets 8)
(define hashset-index 9)

(define hash-threshold 3)
(define initial-hash-size 4) ;must be a power of 2 and >= hash-threshold

(define initial-hash-size-1 (- initial-hash-size 1))

(define the-list-of-no-method '(no-method))

;;;
;;; Method cache
;;;

;; (#@dispatch args N-SPECIALIZED #((TYPE1 ... . CMETHOD) ...) GF)
;; (#@dispatch args N-SPECIALIZED HASHSET MASK
;;             #((TYPE1 ... . CMETHOD) ...)
;;             GF)

;;; Representation

;; non-hashed form

(define method-cache-entries cadddr)

(define (set-method-cache-entries! mcache entries)
  (set-car! (cdddr mcache) entries))

(define (method-cache-n-methods exp)
  (n-cache-methods (method-cache-entries exp)))

(define (method-cache-methods exp)
  (cache-methods (method-cache-entries exp)))

;; hashed form

(define (set-hashed-method-cache-hashset! exp hashset)
  (set-car! (cdddr exp) hashset))

(define (set-hashed-method-cache-mask! exp mask)
  (set-car! (cddddr exp) mask))

(define (hashed-method-cache-entries exp)
  (list-ref exp 5))

(define (set-hashed-method-cache-entries! exp entries)
  (set-car! (list-cdr-ref exp 5) entries))

;; either form

(define (method-cache-generic-function exp)
  (list-ref exp (if (method-cache-hashed? exp) 6 4)))

;;; Predicates

(define (method-cache-hashed? x)
  (integer? (cadddr x)))

(define max-non-hashed-index (- hash-threshold 2))

(define (passed-hash-threshold? exp)
  (and (> (vector-length (method-cache-entries exp)) max-non-hashed-index)
       (struct? (car (vector-ref (method-cache-entries exp)
				 max-non-hashed-index)))))

;;; Converting a method cache to hashed form

(define (method-cache->hashed! exp)
  (set-cdr! (cddr exp) (cons 0 (cons initial-hash-size-1 (cdddr exp))))
  exp)

;;;
;;; Cache entries
;;;

(define (n-cache-methods entries)
  (do ((i (- (vector-length entries) 1) (- i 1)))
      ((or (< i 0) (struct? (car (vector-ref entries i))))
       (+ i 1))))

(define (cache-methods entries)
  (do ((i (- (vector-length entries) 1) (- i 1))
       (methods '() (let ((entry (vector-ref entries i)))
		      (if (or (not (pair? entry)) (struct? (car entry)))
			  (cons entry methods)
			  methods))))
      ((< i 0) methods)))

;;;
;;; Method insertion
;;;

(define (method-cache-insert! exp entry)
  (let* ((entries (method-cache-entries exp))
	 (n (n-cache-methods entries)))
    (if (>= n (vector-length entries))
	;; grow cache
	(let ((new-entries (make-vector (* 2 (vector-length entries))
					the-list-of-no-method)))
	  (do ((i 0 (+ i 1)))
	      ((= i n))
	    (vector-set! new-entries i (vector-ref entries i)))
	  (vector-set! new-entries n entry)
	  (set-method-cache-entries! exp new-entries))
	(vector-set! entries n entry))))

(define (hashed-method-cache-insert! exp entry)
  (let* ((cache (hashed-method-cache-entries exp))
	 (size (vector-length cache)))
    (let* ((entries (cons entry (cache-methods cache)))
	   (size (if (<= (length entries) size)
		     size
		     ;; larger size required
		     (let ((new-size (* 2 size)))
		       (set-hashed-method-cache-mask! exp (- new-size 1))
		       new-size)))
	   (min-misses size)
	   (best #f))
      (do ((hashset 0 (+ 1 hashset)))
	  ((= hashset hashsets))
	(let* ((test-cache (make-vector size the-list-of-no-method))
	       (misses (cache-try-hash! min-misses hashset test-cache entries)))
	  (cond ((zero? misses)
		 (set! min-misses 0)
		 (set! best hashset)
		 (set! cache test-cache)
		 (set! hashset (- hashsets 1)))
		((< misses min-misses)
		 (set! min-misses misses)
		 (set! best hashset)
		 (set! cache test-cache)))))
      (set-hashed-method-cache-hashset! exp best)
      (set-hashed-method-cache-entries! exp cache))))

;;;
;;; Caching
;;;

(define (cache-hashval hashset entry)
  (let ((hashset-index (+ hashset-index hashset)))
    (do ((sum 0)
	 (classes entry (cdr classes)))
	((not (and (pair? classes) (struct? (car classes))))
         sum)
      (set! sum (+ sum (struct-ref (car classes) hashset-index))))))

(define (cache-try-hash! min-misses hashset cache entries)
  (let ((mask (- (vector-length cache) 1)))
    (let outer ((in entries) (max-misses 0))
      (if (null? in)
          max-misses
          (let inner ((i (logand mask (cache-hashval hashset (car in))))
                      (misses 0))
            (cond
             ((and (pair? (vector-ref cache i))
                   (eq? (car (vector-ref cache i)) 'no-method))
              (vector-set! cache i (car in))
              (outer (cdr in) (if (> misses max-misses) misses max-misses)))
             (else
              (let ((misses (+ 1 misses)))
                (if (>= misses min-misses)
                    misses ;; this is a return, yo.
                    (inner (logand mask (+ i 1)) misses))))))))))

;;;
;;; Memoization
;;;

(define (memoize-method! gf args exp)
  (let ((applicable ((if (eq? gf compute-applicable-methods)
			 %compute-applicable-methods
			 compute-applicable-methods)
		     gf args)))
    (cond (applicable
           (memoize-effective-method! gf args applicable)
    	   ;; *fixme* dispatch.scm needs rewriting Since the current
	   ;; code mutates the method cache, we have to work on a
	   ;; copy.  Otherwise we might disturb another thread
	   ;; currently dispatching on the cache.  (No need to copy
	   ;; the vector.)
	   (let* ((new (list-copy exp))
		  (res
		   (cond ((method-cache-hashed? new)
			  (method-cache-install! hashed-method-cache-insert!
						 new args applicable))
			 ((passed-hash-threshold? new)
			  (method-cache-install! hashed-method-cache-insert!
						 (method-cache->hashed! new)
						 args
						 applicable))
			 (else
			  (method-cache-install! method-cache-insert!
						 new args applicable)))))
	     (set-cdr! (cdr exp) (cddr new))
	     res))
	  (else
	   (no-applicable-method gf args)))))

(set-procedure-property! memoize-method! 'system-procedure #t)

(define method-cache-install!
  (letrec ((first-n
	    (lambda (ls n)
	      (if (or (zero? n) (null? ls))
		  '()
		  (cons (car ls) (first-n (cdr ls) (- n 1)))))))
    (lambda (insert! exp args applicable)
      (let* ((specializers (method-specializers (car applicable)))
	     (n-specializers
	      (if (list? specializers)
		  (length specializers)
		  (+ 1 (slot-ref (method-cache-generic-function exp)
				 'n-specialized)))))
	(let* ((types (map class-of (first-n args n-specializers)))
	       (cmethod (compute-cmethod applicable types)))
	  (insert! exp (append types cmethod)) ; entry = types + cmethod
	  cmethod))))) ; cmethod
