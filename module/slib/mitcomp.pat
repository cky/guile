;"mitcomp.pat", patch file of definitions for compiling SLIB with MitScheme.
;;; Copyright (C) 1993 Matthew McDonald.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

From: mafm@cs.uwa.edu.au (Matthew MCDONALD)

	Added declarations to files providing these:
dynamic alist hash hash-table logical random random-inexact modular
prime charplot common-list-functions format generic-write pprint-file
pretty-print-to-string object->string string-case printf line-i/o
synchk priority-queue process red-black-tree sort

(for-each cf
 '("dynamic.scm" "alist.scm" "hash.scm" "hashtab.scm" "logical.scm"
   "random.scm" "randinex.scm" "modular.scm" "prime.scm" "charplot.scm"
   "comlist.scm" "format.scm" "genwrite.scm" "ppfile.scm" "pp2str.scm"
   "obj2str.scm" "strcase.scm" "printf.scm" "lineio.scm" "synchk.scm"
   "priorque.scm" "process.scm" "rbtree.scm" "sort.scm))

while in the SLIB directory will compile all of these.

	They all appear to still be working... They should be
everything CScheme currently uses (except [1] below.)

NOTES:

[1] Not altered:
	debug		   Not worth optimising
	test		   "   "     "
	fluid-let          compiler chokes over
				(lambda () . body)
	scmacro		   Fails when compiled, not immediately obvious why
	synclo             " " "
	r4rsyn		   " " "
	yasos              requires the macros
	collect		   "        "   "

[2] removed 'sort from list of MIT features. The library version is
more complete (and needed for charplot.)

[3] Remember that mitscheme.init gets the .bin put in the wrong place
by the compiler and thus doesn't get recognised by LOAD.
======================================================================
diff -c slib/alist.scm nlib/alist.scm
*** slib/alist.scm	Thu Jan 21 00:01:34 1993
--- nlib/alist.scm	Tue Feb  9 00:21:07 1993
***************
*** 44,50 ****
  ;(define rem (alist-remover string-ci=?))
  ;(set! alist (rem alist "fOO"))
  
! (define (predicate->asso pred)
    (cond ((eq? eq? pred) assq)
  	((eq? = pred) assv)
  	((eq? eqv? pred) assv)
--- 44,53 ----
  ;(define rem (alist-remover string-ci=?))
  ;(set! alist (rem alist "fOO"))
  
! ;;; Declarations for CScheme
! (declare (usual-integrations))
! 
! (define-integrable (predicate->asso pred)
    (cond ((eq? eq? pred) assq)
  	((eq? = pred) assv)
  	((eq? eqv? pred) assv)
***************
*** 57,69 ****
  			((pred key (caar al)) (car al))
  			(else (l (cdr al)))))))))
  
! (define (alist-inquirer pred)
    (let ((assofun (predicate->asso pred)))
      (lambda (alist key)
        (let ((pair (assofun key alist)))
  	(and pair (cdr pair))))))
  
! (define (alist-associator pred)
    (let ((assofun (predicate->asso pred)))
      (lambda (alist key val)
        (let* ((pair (assofun key alist)))
--- 60,72 ----
  			((pred key (caar al)) (car al))
  			(else (l (cdr al)))))))))
  
! (define-integrable (alist-inquirer pred)
    (let ((assofun (predicate->asso pred)))
      (lambda (alist key)
        (let ((pair (assofun key alist)))
  	(and pair (cdr pair))))))
  
! (define-integrable (alist-associator pred)
    (let ((assofun (predicate->asso pred)))
      (lambda (alist key val)
        (let* ((pair (assofun key alist)))
***************
*** 71,77 ****
  		    alist)
  	      (else (cons (cons key val) alist)))))))
  
! (define (alist-remover pred)
    (lambda (alist key)
      (cond ((null? alist) alist)
  	  ((pred key (caar alist)) (cdr alist))
--- 74,80 ----
  		    alist)
  	      (else (cons (cons key val) alist)))))))
  
! (define-integrable (alist-remover pred)
    (lambda (alist key)
      (cond ((null? alist) alist)
  	  ((pred key (caar alist)) (cdr alist))
diff -c slib/charplot.scm nlib/charplot.scm
*** slib/charplot.scm	Sat Nov 14 21:50:54 1992
--- nlib/charplot.scm	Tue Feb  9 00:21:07 1993
***************
*** 7,12 ****
--- 7,24 ----
  ;are strings with names to label the x and y axii with.
  
  ;;;;---------------------------------------------------------------
+ 
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate-external "sort"))
+ (declare (integrate
+ 	  rows
+ 	  columns
+ 	  charplot:height
+ 	  charplot:width
+ 	  charplot:plot
+ 	  plot!))
+ 
  (require 'sort)
  
  (define rows 24)
***************
*** 27,39 ****
  	 (write-char char)
  	 (charplot:printn! (+ n -1) char))))
  
! (define (charplot:center-print! str width)
    (let ((lpad (quotient (- width (string-length str)) 2)))
      (charplot:printn! lpad #\ )
      (display str)
      (charplot:printn! (- width (+ (string-length  str) lpad)) #\ )))
  
! (define (scale-it z scale)
    (if (and (exact? z) (integer? z))
        (quotient (* z (car scale)) (cadr scale))
        (inexact->exact (round (/ (* z (car scale)) (cadr scale))))))
--- 39,51 ----
  	 (write-char char)
  	 (charplot:printn! (+ n -1) char))))
  
! (define-integrable (charplot:center-print! str width)
    (let ((lpad (quotient (- width (string-length str)) 2)))
      (charplot:printn! lpad #\ )
      (display str)
      (charplot:printn! (- width (+ (string-length  str) lpad)) #\ )))
  
! (define-integrable (scale-it z scale)
    (if (and (exact? z) (integer? z))
        (quotient (* z (car scale)) (cadr scale))
        (inexact->exact (round (/ (* z (car scale)) (cadr scale))))))
diff -c slib/comlist.scm nlib/comlist.scm
*** slib/comlist.scm	Wed Jan 27 11:08:44 1993
--- nlib/comlist.scm	Tue Feb  9 00:21:08 1993
***************
*** 6,11 ****
--- 6,14 ----
  
  ;;;; LIST FUNCTIONS FROM COMMON LISP
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ 
  ;;;From: hugh@ear.mit.edu (Hugh Secker-Walker)
  (define (make-list k . init)
    (set! init (if (pair? init) (car init)))
***************
*** 13,21 ****
         (result '() (cons init result)))
        ((<= k 0) result)))
  
! (define (copy-list lst) (append lst '()))
  
! (define (adjoin e l) (if (memq e l) l (cons e l)))
  
  (define (union l1 l2)
    (cond ((null? l1) l2)
--- 16,24 ----
         (result '() (cons init result)))
        ((<= k 0) result)))
  
! (define-integrable (copy-list lst) (append lst '()))
  
! (define-integrable (adjoin e l) (if (memq e l) l (cons e l)))
  
  (define (union l1 l2)
    (cond ((null? l1) l2)
***************
*** 33,39 ****
  	((memv (car l1) l2) (set-difference (cdr l1) l2))
  	(else (cons (car l1) (set-difference (cdr l1) l2)))))
  
! (define (position obj lst)
    (letrec ((pos (lambda (n lst)
  		  (cond ((null? lst) #f)
  			((eqv? obj (car lst)) n)
--- 36,42 ----
  	((memv (car l1) l2) (set-difference (cdr l1) l2))
  	(else (cons (car l1) (set-difference (cdr l1) l2)))))
  
! (define-integrable (position obj lst)
    (letrec ((pos (lambda (n lst)
  		  (cond ((null? lst) #f)
  			((eqv? obj (car lst)) n)
***************
*** 45,51 ****
        init
        (reduce-init p (p init (car l)) (cdr l))))
  
! (define (reduce p l)
    (cond ((null? l) l)
  	((null? (cdr l)) (car l))
  	(else (reduce-init p (car l) (cdr l)))))
--- 48,54 ----
        init
        (reduce-init p (p init (car l)) (cdr l))))
  
! (define-integrable (reduce p l)
    (cond ((null? l) l)
  	((null? (cdr l)) (car l))
  	(else (reduce-init p (car l) (cdr l)))))
***************
*** 58,64 ****
    (or (null? l)
        (and (pred (car l)) (every pred (cdr l)))))
  
! (define (notevery pred l) (not (every pred l)))
  
  (define (find-if t l)
    (cond ((null? l) #f)
--- 61,67 ----
    (or (null? l)
        (and (pred (car l)) (every pred (cdr l)))))
  
! (define-integrable (notevery pred l) (not (every pred l)))
  
  (define (find-if t l)
    (cond ((null? l) #f)
***************
*** 121,141 ****
  (define (nthcdr n lst)
    (if (zero? n) lst (nthcdr (+ -1 n) (cdr lst))))
  
! (define (last lst n)
    (nthcdr (- (length lst) n) lst))
  
  ;;;; CONDITIONALS
  
! (define (and? . args)
    (cond ((null? args) #t)
  	((car args) (apply and? (cdr args)))
  	(else #f)))
  
! (define (or? . args)
    (cond ((null? args) #f)
  	((car args) #t)
  	(else (apply or? (cdr args)))))
  
! (define (identity x) x)
  
  (require 'rev3-procedures)
--- 124,144 ----
  (define (nthcdr n lst)
    (if (zero? n) lst (nthcdr (+ -1 n) (cdr lst))))
  
! (define-integrable (last lst n)
    (nthcdr (- (length lst) n) lst))
  
  ;;;; CONDITIONALS
  
! (define-integrable (and? . args)
    (cond ((null? args) #t)
  	((car args) (apply and? (cdr args)))
  	(else #f)))
  
! (define-integrable (or? . args)
    (cond ((null? args) #f)
  	((car args) #t)
  	(else (apply or? (cdr args)))))
  
! (define-integrable (identity x) x)
  
  (require 'rev3-procedures)
diff -c slib/dynamic.scm nlib/dynamic.scm
*** slib/dynamic.scm	Thu Sep 17 23:35:46 1992
--- nlib/dynamic.scm	Tue Feb  9 00:21:08 1993
***************
*** 31,36 ****
--- 31,43 ----
  ;
  ;There was also a DYNAMIC-BIND macro which I haven't implemented.
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ 
+ (declare (integrate-external "record"))
+ (declare (integrate-external "dynwind"))
+ (declare (integrate dynamic:errmsg))
+ 
  (require 'record)
  (require 'dynamic-wind)
  
***************
*** 48,60 ****
    (record-accessor dynamic-environment-rtd 'parent))
  
  (define *current-dynamic-environment* #f)
! (define (extend-current-dynamic-environment dynamic obj)
    (set! *current-dynamic-environment*
  	(make-dynamic-environment dynamic obj
  				  *current-dynamic-environment*)))
  
  (define dynamic-rtd (make-record-type "dynamic" '()))
! (define make-dynamic
    (let ((dynamic-constructor (record-constructor dynamic-rtd)))
      (lambda (obj)
        (let ((dynamic (dynamic-constructor)))
--- 55,69 ----
    (record-accessor dynamic-environment-rtd 'parent))
  
  (define *current-dynamic-environment* #f)
! 
! (define-integrable (extend-current-dynamic-environment dynamic obj)
    (set! *current-dynamic-environment*
  	(make-dynamic-environment dynamic obj
  				  *current-dynamic-environment*)))
  
  (define dynamic-rtd (make-record-type "dynamic" '()))
! 
! (define-integrable make-dynamic
    (let ((dynamic-constructor (record-constructor dynamic-rtd)))
      (lambda (obj)
        (let ((dynamic (dynamic-constructor)))
***************
*** 61,68 ****
  	(extend-current-dynamic-environment dynamic obj)
  	dynamic))))
  
! (define dynamic? (record-predicate dynamic-rtd))
! (define (guarantee-dynamic dynamic)
    (or (dynamic? dynamic)
        (slib:error "Not a dynamic" dynamic)))
  
--- 70,78 ----
  	(extend-current-dynamic-environment dynamic obj)
  	dynamic))))
  
! (define-integrable dynamic? (record-predicate dynamic-rtd))
! 
! (define-integrable (guarantee-dynamic dynamic)
    (or (dynamic? dynamic)
        (slib:error "Not a dynamic" dynamic)))
  
***************
*** 69,75 ****
  (define dynamic:errmsg
    "No value defined for this dynamic in the current dynamic environment")
  
! (define (dynamic-ref dynamic)
    (guarantee-dynamic dynamic)
    (let loop ((env *current-dynamic-environment*))
      (cond ((not env)
--- 79,85 ----
  (define dynamic:errmsg
    "No value defined for this dynamic in the current dynamic environment")
  
! (define-integrable (dynamic-ref dynamic)
    (guarantee-dynamic dynamic)
    (let loop ((env *current-dynamic-environment*))
      (cond ((not env)
***************
*** 79,85 ****
  	  (else
  	   (loop (dynamic-environment:parent env))))))
  
! (define (dynamic-set! dynamic obj)
    (guarantee-dynamic dynamic)
    (let loop ((env *current-dynamic-environment*))
      (cond ((not env)
--- 89,95 ----
  	  (else
  	   (loop (dynamic-environment:parent env))))))
  
! (define-integrable (dynamic-set! dynamic obj)
    (guarantee-dynamic dynamic)
    (let loop ((env *current-dynamic-environment*))
      (cond ((not env)
diff -c slib/format.scm nlib/format.scm
*** slib/format.scm	Tue Jan  5 14:56:48 1993
--- nlib/format.scm	Tue Feb  9 00:21:09 1993
***************
*** 78,84 ****
  ;   * removed C-style padding support
  ;
  
! ;;; SCHEME IMPLEMENTATION DEPENDENCIES ---------------------------------------
  
  ;; To configure the format module for your scheme system, set the variable
  ;; format:scheme-system to one of the symbols of (slib elk any). You may add
--- 78,88 ----
  ;   * removed C-style padding support
  ;
  
! ;;; SCHEME IMPLEMENTATION DEPENDENCIES
! ;;; ---------------------------------------
! 
! ;;; (minimal) Declarations for CScheme
! (declare (usual-integrations))
  
  ;; To configure the format module for your scheme system, set the variable
  ;; format:scheme-system to one of the symbols of (slib elk any). You may add
diff -c slib/genwrite.scm nlib/genwrite.scm
*** slib/genwrite.scm	Mon Oct 19 14:49:06 1992
--- nlib/genwrite.scm	Tue Feb  9 00:21:10 1993
***************
*** 26,31 ****
--- 26,34 ----
  ;
  ; where display-string = (lambda (s) (for-each write-char (string->list s)) #t)
  
+ ;;; (minimal) Declarations for CScheme
+ (declare (usual-integrations))
+ 
  (define (generic-write obj display? width output)
  
    (define (read-macro? l)
diff -c slib/hash.scm nlib/hash.scm
*** slib/hash.scm	Thu Sep 10 00:05:52 1992
--- nlib/hash.scm	Tue Feb  9 00:21:10 1993
***************
*** 23,35 ****
  ;the equality predicate pred.  Pred should be EQ?, EQV?, EQUAL?, =,
  ;CHAR=?, CHAR-CI=?, STRING=?, or STRING-CI=?.
   
! (define (hash:hash-char char n)
    (modulo (char->integer char) n))
  
! (define (hash:hash-char-ci char n)
    (modulo (char->integer (char-downcase char)) n))
  
! (define (hash:hash-symbol sym n)
    (hash:hash-string (symbol->string sym) n))
  
  ;;; I am trying to be careful about overflow and underflow here.
--- 23,40 ----
  ;the equality predicate pred.  Pred should be EQ?, EQV?, EQUAL?, =,
  ;CHAR=?, CHAR-CI=?, STRING=?, or STRING-CI=?.
   
! 
! ;;; Declarations for CScheme
! (declare (usual-integrations))
! (declare (integrate hash))
! 
! (define-integrable (hash:hash-char char n)
    (modulo (char->integer char) n))
  
! (define-integrable (hash:hash-char-ci char n)
    (modulo (char->integer (char-downcase char)) n))
  
! (define-integrable (hash:hash-symbol sym n)
    (hash:hash-string (symbol->string sym) n))
  
  ;;; I am trying to be careful about overflow and underflow here.
***************
*** 173,179 ****
  
  (define hashq hashv)
  
! (define (predicate->hash pred)
    (cond ((eq? pred eq?) hashq)
  	((eq? pred eqv?) hashv)
  	((eq? pred equal?) hash)
--- 178,184 ----
  
  (define hashq hashv)
  
! (define-integrable (predicate->hash pred)
    (cond ((eq? pred eq?) hashq)
  	((eq? pred eqv?) hashv)
  	((eq? pred equal?) hash)
diff -c slib/hashtab.scm nlib/hashtab.scm
*** slib/hashtab.scm	Mon Oct 19 14:49:44 1992
--- nlib/hashtab.scm	Tue Feb  9 00:21:11 1993
***************
*** 36,47 ****
  ;Returns a procedure of 2 arguments, hashtab and key, which modifies
  ;hashtab so that the association whose key is key removed.
  
  (require 'hash)
  (require 'alist)
  
! (define (make-hash-table k) (make-vector k '()))
  
! (define (predicate->hash-asso pred)
    (let ((hashfun (predicate->hash pred))
  	(asso (predicate->asso pred)))
      (lambda (key hashtab)
--- 36,53 ----
  ;Returns a procedure of 2 arguments, hashtab and key, which modifies
  ;hashtab so that the association whose key is key removed.
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ 
+ (declare (integrate-external "hash"))
+ (declare (integrate-external "alist"))
+ 
  (require 'hash)
  (require 'alist)
  
! (define-integrable (make-hash-table k) (make-vector k '()))
  
! (define-integrable (predicate->hash-asso pred)
    (let ((hashfun (predicate->hash pred))
  	(asso (predicate->asso pred)))
      (lambda (key hashtab)
***************
*** 48,54 ****
        (asso key
  	    (vector-ref hashtab (hashfun key (vector-length hashtab)))))))
  
! (define (hash-inquirer pred)
    (let ((hashfun (predicate->hash pred))
  	(ainq (alist-inquirer pred)))
      (lambda (hashtab key)
--- 54,60 ----
        (asso key
  	    (vector-ref hashtab (hashfun key (vector-length hashtab)))))))
  
! (define-integrable (hash-inquirer pred)
    (let ((hashfun (predicate->hash pred))
  	(ainq (alist-inquirer pred)))
      (lambda (hashtab key)
***************
*** 55,61 ****
        (ainq (vector-ref hashtab (hashfun key (vector-length hashtab)))
  	    key))))
  
! (define (hash-associator pred)
    (let ((hashfun (predicate->hash pred))
  	(asso (alist-associator pred)))
      (lambda (hashtab key val)
--- 61,67 ----
        (ainq (vector-ref hashtab (hashfun key (vector-length hashtab)))
  	    key))))
  
! (define-integrable (hash-associator pred)
    (let ((hashfun (predicate->hash pred))
  	(asso (alist-associator pred)))
      (lambda (hashtab key val)
***************
*** 64,70 ****
  		     (asso (vector-ref hashtab num) key val)))
        hashtab)))
  
! (define (hash-remover pred)
    (let ((hashfun (predicate->hash pred))
  	(arem (alist-remover pred)))
      (lambda (hashtab key)
--- 70,76 ----
  		     (asso (vector-ref hashtab num) key val)))
        hashtab)))
  
! (define-integrable (hash-remover pred)
    (let ((hashfun (predicate->hash pred))
  	(arem (alist-remover pred)))
      (lambda (hashtab key)
diff -c slib/lineio.scm nlib/lineio.scm
*** slib/lineio.scm	Sun Oct 25 01:40:38 1992
--- nlib/lineio.scm	Tue Feb  9 00:21:11 1993
***************
*** 28,33 ****
--- 28,36 ----
  ;unspecified value.  Port may be ommited, in which case it defaults to
  ;the value returned by current-input-port.
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ 
  (define (read-line . arg)
    (let* ((char (apply read-char arg)))
      (if (eof-object? char)
***************
*** 56,61 ****
  			(+ 1 i) #f))))
  	  (string-set! str i char)))))
  
! (define (write-line str . arg)
    (apply display str arg)
    (apply newline arg))
--- 59,64 ----
  			(+ 1 i) #f))))
  	  (string-set! str i char)))))
  
! (define-integrable (write-line str . arg)
    (apply display str arg)
    (apply newline arg))
diff -c slib/logical.scm nlib/logical.scm
*** slib/logical.scm	Mon Feb  1 22:22:04 1993
--- nlib/logical.scm	Tue Feb  9 00:21:11 1993
***************
*** 48,53 ****
--- 48,66 ----
  ;
  ;;;;------------------------------------------------------------------
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate logand		; Exported functions
+ 		    logor
+ 		    logxor
+ 		    lognot
+ 		    ash
+ 		    logcount
+ 		    integer-length
+ 		    bit-extract
+ 		    ipow-by-squaring
+ 		    integer-expt))
+ 
  (define logical:integer-expt
    (if (provided? 'inexact)
        expt
***************
*** 61,67 ****
  					(quotient k 2)
  					(if (even? k) acc (proc acc x))
  					proc))))
- 
  (define (logical:logand n1 n2)
    (cond ((= n1 n2) n1)
  	((zero? n1) 0)
--- 74,79 ----
***************
*** 90,102 ****
  	    (vector-ref (vector-ref logical:boole-xor (modulo n1 16))
  			(modulo n2 16))))))
  
! (define (logical:lognot n) (- -1 n))
  
! (define (logical:bit-extract n start end)
    (logical:logand (- (logical:integer-expt 2 (- end start)) 1)
  		  (logical:ash n (- start))))
  
! (define (logical:ash int cnt)
    (if (negative? cnt)
        (let ((n (logical:integer-expt 2 (- cnt))))
  	(if (negative? int)
--- 102,114 ----
  	    (vector-ref (vector-ref logical:boole-xor (modulo n1 16))
  			(modulo n2 16))))))
  
! (define-integrable (logical:lognot n) (- -1 n))
  
! (define-integrable (logical:bit-extract n start end)
    (logical:logand (- (logical:integer-expt 2 (- end start)) 1)
  		  (logical:ash n (- start))))
  
! (define-integrable (logical:ash int cnt)
    (if (negative? cnt)
        (let ((n (logical:integer-expt 2 (- cnt))))
  	(if (negative? int)
***************
*** 104,110 ****
  	    (quotient int n)))
        (* (logical:integer-expt 2 cnt) int)))
  
! (define (logical:ash-4 x)
    (if (negative? x)
        (+ -1 (quotient (+ 1 x) 16))
        (quotient x 16)))
--- 116,122 ----
  	    (quotient int n)))
        (* (logical:integer-expt 2 cnt) int)))
  
! (define-integrable (logical:ash-4 x)
    (if (negative? x)
        (+ -1 (quotient (+ 1 x) 16))
        (quotient x 16)))
diff -c slib/mitscheme.init nlib/mitscheme.init
*** slib/mitscheme.init	Fri Jan 22 00:52:04 1993
--- nlib/mitscheme.init	Tue Feb  9 00:21:12 1993
***************
*** 48,55 ****
  
  ;;; FORCE-OUTPUT flushes any pending output on optional arg output port
  ;;; use this definition if your system doesn't have such a procedure.
! ;(define (force-output . arg) #t)
! (define force-output flush-output)
  
  ;;; CHAR-CODE-LIMIT is one greater than the largest integer which can
  ;;; be returned by CHAR->INTEGER.  It is defined by MITScheme.
--- 47,54 ----
  
  ;;; FORCE-OUTPUT flushes any pending output on optional arg output port
  ;;; use this definition if your system doesn't have such a procedure.
! (define (force-output . arg) #t)
! ;(define force-output flush-output)
  
  ;;; CHAR-CODE-LIMIT is one greater than the largest integer which can
  ;;; be returned by CHAR->INTEGER.  It is defined by MITScheme.
diff -c slib/modular.scm nlib/modular.scm
*** slib/modular.scm	Sun Feb  2 12:53:26 1992
--- nlib/modular.scm	Tue Feb  9 00:21:13 1993
***************
*** 36,41 ****
--- 36,48 ----
  ;Returns (k2 ^ k3) mod k1.
  ;
  ;;;;--------------------------------------------------------------
+ 
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ 
+ (declare (integrate-external "logical"))
+ (declare (integrate modular:negate  extended-euclid))
+ 
  (require 'logical)
  
  ;;; from:
***************
*** 51,57 ****
  	      (caddr res)
  	      (- (cadr res) (* (quotient a b) (caddr res)))))))
  
! (define (modular:invert m a)
    (let ((d (modular:extended-euclid a m)))
      (if (= 1 (car d))
  	(modulo (cadr d) m)
--- 58,64 ----
  	      (caddr res)
  	      (- (cadr res) (* (quotient a b) (caddr res)))))))
  
! (define-integrable (modular:invert m a)
    (let ((d (modular:extended-euclid a m)))
      (if (= 1 (car d))
  	(modulo (cadr d) m)
***************
*** 59,67 ****
  
  (define modular:negate -)
  
! (define (modular:+ m a b) (modulo (+ (- a m) b) m))
  
! (define (modular:- m a b) (modulo (- a b) m))
  
  ;;; See: L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
  ;;; with Splitting Facilities." ACM Transactions on Mathematical
--- 66,74 ----
  
  (define modular:negate -)
  
! (define-integrable (modular:+ m a b) (modulo (+ (- a m) b) m))
  
! (define-integrable (modular:- m a b) (modulo (- a b) m))
  
  ;;; See: L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
  ;;; with Splitting Facilities." ACM Transactions on Mathematical
***************
*** 98,104 ****
  	  (modulo (+ (if (positive? p) (- p m) p)
  		     (* a0 (modulo b q))) m)))))
  
! (define (modular:expt m a b)
    (cond ((= a 1) 1)
  	((= a (- m 1)) (if (odd? b) a 1))
  	((zero? a) 0)
--- 105,111 ----
  	  (modulo (+ (if (positive? p) (- p m) p)
  		     (* a0 (modulo b q))) m)))))
  
! (define-integrable (modular:expt m a b)
    (cond ((= a 1) 1)
  	((= a (- m 1)) (if (odd? b) a 1))
  	((zero? a) 0)
diff -c slib/obj2str.scm nlib/obj2str.scm
*** slib/obj2str.scm	Mon Oct 19 14:49:08 1992
--- nlib/obj2str.scm	Tue Feb  9 00:21:13 1993
***************
*** 2,13 ****
  
  (require 'generic-write)
  
  ; (object->string obj) returns the textual representation of 'obj' as a
  ; string.
  ;
  ; Note: (write obj) = (display (object->string obj))
  
! (define (object->string obj)
    (let ((result '()))
      (generic-write obj #f #f (lambda (str) (set! result (cons str result)) #t))
      (reverse-string-append result)))
--- 2,17 ----
  
  (require 'generic-write)
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate-external "genwrite"))
+ 
  ; (object->string obj) returns the textual representation of 'obj' as a
  ; string.
  ;
  ; Note: (write obj) = (display (object->string obj))
  
! (define-integrable (object->string obj)
    (let ((result '()))
      (generic-write obj #f #f (lambda (str) (set! result (cons str result)) #t))
      (reverse-string-append result)))
diff -c slib/pp2str.scm nlib/pp2str.scm
*** slib/pp2str.scm	Mon Oct 19 14:49:08 1992
--- nlib/pp2str.scm	Tue Feb  9 00:21:13 1993
***************
*** 2,11 ****
  
  (require 'generic-write)
  
  ; (pretty-print-to-string obj) returns a string with the pretty-printed
  ; textual representation of 'obj'.
  
! (define (pp:pretty-print-to-string obj)
    (let ((result '()))
      (generic-write obj #f 79 (lambda (str) (set! result (cons str result)) #t))
      (reverse-string-append result)))
--- 2,16 ----
  
  (require 'generic-write)
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate-external "genwrite"))
+ (declare (integrate pretty-print-to-string))
+ 
  ; (pretty-print-to-string obj) returns a string with the pretty-printed
  ; textual representation of 'obj'.
  
! (define-integrable (pp:pretty-print-to-string obj)
    (let ((result '()))
      (generic-write obj #f 79 (lambda (str) (set! result (cons str result)) #t))
      (reverse-string-append result)))
diff -c slib/ppfile.scm nlib/ppfile.scm
*** slib/ppfile.scm	Mon Oct 19 14:49:08 1992
--- nlib/ppfile.scm	Tue Feb  9 00:21:14 1993
***************
*** 10,15 ****
--- 10,19 ----
  ;
  (require 'pretty-print)
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate-external "pp"))
+ 
  (define (pprint-file ifile . optarg)
    (let ((lst (call-with-input-file ifile
  	       (lambda (iport)
diff -c slib/prime.scm nlib/prime.scm
*** slib/prime.scm	Mon Feb  8 20:49:46 1993
--- nlib/prime.scm	Tue Feb  9 00:24:16 1993
***************
*** 24,29 ****
--- 24,39 ----
  ;(sort! (factor k) <)
  
  ;;;;--------------------------------------------------------------
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate-external "random"))
+ (declare (integrate-external "modular"))
+ (declare (integrate
+ 	  jacobi-symbol 
+ 	  prime?
+ 	  factor))
+ 
+ 
  (require 'random)
  (require 'modular)
  
***************
*** 56,62 ****
  ;;;     choosing prime:trials=30 should be enough
  (define prime:trials 30)
  ;;; prime:product is a product of small primes.
! (define prime:product
    (let ((p 210))
      (for-each (lambda (s) (set! p (or (string->number s) p)))
        '("2310" "30030" "510510" "9699690" "223092870"
--- 66,72 ----
  ;;;     choosing prime:trials=30 should be enough
  (define prime:trials 30)
  ;;; prime:product is a product of small primes.
! (define-integrable prime:product
    (let ((p 210))
      (for-each (lambda (s) (set! p (or (string->number s) p)))
        '("2310" "30030" "510510" "9699690" "223092870"
***************
*** 86,92 ****
  ;                  |  f(u,v,2b,n/2) or f(u+b,v+b,2b,(n-u-v-b)/2) if n even
  
  ;Thm: f(1,1,2,(m-1)/2) = (p,q) iff pq=m for odd m.
!  
  ;It may be illuminating to consider the relation of the Lankinen function in
  ;a `computational hierarchy' of other factoring functions.*  Assumptions are
  ;made herein on the basis of conventional digital (binary) computers.  Also,
--- 96,102 ----
  ;                  |  f(u,v,2b,n/2) or f(u+b,v+b,2b,(n-u-v-b)/2) if n even
  
  ;Thm: f(1,1,2,(m-1)/2) = (p,q) iff pq=m for odd m.
! 
  ;It may be illuminating to consider the relation of the Lankinen function in
  ;a `computational hierarchy' of other factoring functions.*  Assumptions are
  ;made herein on the basis of conventional digital (binary) computers.  Also,
***************
*** 94,100 ****
  ;be factored is prime).  However, all algorithms would probably perform to
  ;the same constant multiple of the given orders for complete composite
  ;factorizations.
!  
  ;Thm: Eratosthenes' Sieve is very roughtly O(ln(n)/n) in time and
  ;     O(n*log2(n)) in space.
  ;Pf: It works with all prime factors less than n (about ln(n)/n by the prime
--- 104,110 ----
  ;be factored is prime).  However, all algorithms would probably perform to
  ;the same constant multiple of the given orders for complete composite
  ;factorizations.
! 
  ;Thm: Eratosthenes' Sieve is very roughtly O(ln(n)/n) in time and
  ;     O(n*log2(n)) in space.
  ;Pf: It works with all prime factors less than n (about ln(n)/n by the prime
diff -c slib/priorque.scm nlib/priorque.scm
*** slib/priorque.scm	Mon Oct 19 14:49:42 1992
--- nlib/priorque.scm	Tue Feb  9 00:21:15 1993
***************
*** 22,41 ****
  ;;; 1989 MIT Press.
  
  (require 'record)
  (define heap-rtd (make-record-type "heap" '(array size heap<?)))
! (define make-heap
    (let ((cstr (record-constructor heap-rtd)))
      (lambda (pred<?)
        (cstr (make-vector 4) 0 pred<?))))
! (define heap-ref
    (let ((ra (record-accessor heap-rtd 'array)))
      (lambda (a i)
        (vector-ref (ra a) (+ -1 i)))))
! (define heap-set!
    (let ((ra (record-accessor heap-rtd 'array)))
      (lambda (a i v)
        (vector-set! (ra a) (+ -1 i) v))))
! (define heap-exchange
    (let ((aa (record-accessor heap-rtd 'array)))
      (lambda (a i j)
        (set! i (+ -1 i))
--- 22,53 ----
  ;;; 1989 MIT Press.
  
  (require 'record)
+ 
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ 
+ (declare (integrate
+ 	  heap-size
+ 	  heap<?))
+ 
  (define heap-rtd (make-record-type "heap" '(array size heap<?)))
! 
! (define-integrable make-heap
    (let ((cstr (record-constructor heap-rtd)))
      (lambda (pred<?)
        (cstr (make-vector 4) 0 pred<?))))
! 
! (define-integrable heap-ref
    (let ((ra (record-accessor heap-rtd 'array)))
      (lambda (a i)
        (vector-ref (ra a) (+ -1 i)))))
! 
! (define-integrable heap-set!
    (let ((ra (record-accessor heap-rtd 'array)))
      (lambda (a i v)
        (vector-set! (ra a) (+ -1 i) v))))
! 
! (define-integrable heap-exchange
    (let ((aa (record-accessor heap-rtd 'array)))
      (lambda (a i j)
        (set! i (+ -1 i))
***************
*** 44,51 ****
--- 56,66 ----
  	     (tmp (vector-ref ra i)))
  	(vector-set! ra i (vector-ref ra j))
  	(vector-set! ra j tmp)))))
+ 
  (define heap-size (record-accessor heap-rtd 'size))
+ 
  (define heap<? (record-accessor heap-rtd 'heap<?))
+ 
  (define heap-set-size
    (let ((aa (record-accessor heap-rtd 'array))
  	(am (record-modifier heap-rtd 'array))
***************
*** 59,68 ****
  		(vector-set! nra i (vector-ref ra i)))))
  	(sm a s)))))
  
! (define (heap-parent i) (quotient i 2))
! (define (heap-left i) (* 2 i))
! (define (heap-right i) (+ 1 (* 2 i)))
  
  (define (heapify a i)
    (define l (heap-left i))
    (define r (heap-right i))
--- 74,85 ----
  		(vector-set! nra i (vector-ref ra i)))))
  	(sm a s)))))
  
! (define-integrable (heap-parent i) (quotient i 2))
  
+ (define-integrable (heap-left i) (* 2 i))
+ 
+ (define-integrable (heap-right i) (+ 1 (* 2 i)))
+ 
  (define (heapify a i)
    (define l (heap-left i))
    (define r (heap-right i))
***************
*** 99,104 ****
--- 116,122 ----
      max))
  
  (define heap #f)
+ 
  (define (heap-test)
    (set! heap (make-heap char>?))
    (heap-insert! heap #\A)
diff -c slib/process.scm nlib/process.scm
*** slib/process.scm	Wed Nov  4 12:26:50 1992
--- nlib/process.scm	Tue Feb  9 00:21:15 1993
***************
*** 21,30 ****
  ;
  ;;;;----------------------------------------------------------------------
  
  (require 'full-continuation)
  (require 'queue)
  
! (define (add-process! thunk1)
    (cond ((procedure? thunk1)
  	 (defer-ints)
  	 (enqueue! process:queue thunk1)
--- 21,33 ----
  ;
  ;;;;----------------------------------------------------------------------
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ 
  (require 'full-continuation)
  (require 'queue)
  
! (define-integrable (add-process! thunk1)
    (cond ((procedure? thunk1)
  	 (defer-ints)
  	 (enqueue! process:queue thunk1)
***************
*** 55,63 ****
  (define ints-disabled #f)
  (define alarm-deferred #f)
  
! (define (defer-ints) (set! ints-disabled #t))
  
! (define (allow-ints)
    (set! ints-disabled #f)
    (cond (alarm-deferred
  	  (set! alarm-deferred #f)
--- 58,66 ----
  (define ints-disabled #f)
  (define alarm-deferred #f)
  
! (define-integrable (defer-ints) (set! ints-disabled #t))
  
! (define-integrable (allow-ints)
    (set! ints-disabled #f)
    (cond (alarm-deferred
  	  (set! alarm-deferred #f)
***************
*** 66,72 ****
  ;;; Make THE process queue.
  (define process:queue (make-queue))
  
! (define (alarm-interrupt)
    (alarm 1)
    (if ints-disabled (set! alarm-deferred #t)
        (process:schedule!)))
--- 69,75 ----
  ;;; Make THE process queue.
  (define process:queue (make-queue))
  
! (define-integrable (alarm-interrupt)
    (alarm 1)
    (if ints-disabled (set! alarm-deferred #t)
        (process:schedule!)))
diff -c slib/randinex.scm nlib/randinex.scm
*** slib/randinex.scm	Wed Nov 18 22:59:20 1992
--- nlib/randinex.scm	Tue Feb  9 00:21:16 1993
***************
*** 47,52 ****
--- 47,59 ----
  ;For an exponential distribution with mean U use (* U (random:exp)).
  ;;;;-----------------------------------------------------------------
  
+ 
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate-external "random"))
+ (declare (integrate
+ 	  random:float-radix))
+ 
  (define random:float-radix
    (+ 1 (exact->inexact random:MASK)))
  
***************
*** 56,61 ****
--- 63,69 ----
    (if (= 1.0 (+ 1 x))
        l
        (random:size-float (+ l 1) (/ x random:float-radix))))
+ 
  (define random:chunks/float (random:size-float 1 1.0))
  
  (define (random:uniform-chunk n state)
***************
*** 67,73 ****
  	 random:float-radix)))
  
  ;;; Generate an inexact real between 0 and 1.
! (define (random:uniform state)
    (random:uniform-chunk random:chunks/float state))
  
  ;;; If x and y are independent standard normal variables, then with
--- 75,81 ----
  	 random:float-radix)))
  
  ;;; Generate an inexact real between 0 and 1.
! (define-integrable (random:uniform state)
    (random:uniform-chunk random:chunks/float state))
  
  ;;; If x and y are independent standard normal variables, then with
***************
*** 89,95 ****
  	  (do! n (* r (cos t)))
  	  (if (positive? n) (do! (- n 1) (* r (sin t)))))))))
  
! (define random:normal
    (let ((vect (make-vector 1)))
      (lambda args 
        (apply random:normal-vector! vect args)
--- 97,103 ----
  	  (do! n (* r (cos t)))
  	  (if (positive? n) (do! (- n 1) (* r (sin t)))))))))
  
! (define-integrable random:normal
    (let ((vect (make-vector 1)))
      (lambda args 
        (apply random:normal-vector! vect args)
***************
*** 98,104 ****
  ;;; For the uniform distibution on the hollow sphere, pick a normal
  ;;; family and scale.
  
! (define (random:hollow-sphere! vect . args)
    (let ((ms (sqrt (apply random:normal-vector! vect args))))
      (do ((n (- (vector-length vect) 1) (- n 1)))
  	((negative? n))
--- 106,112 ----
  ;;; For the uniform distibution on the hollow sphere, pick a normal
  ;;; family and scale.
  
! (define-integrable (random:hollow-sphere! vect . args)
    (let ((ms (sqrt (apply random:normal-vector! vect args))))
      (do ((n (- (vector-length vect) 1) (- n 1)))
  	((negative? n))
***************
*** 117,123 ****
  	((negative? n))
        (vector-set! vect n (* r (vector-ref vect n))))))
  
! (define (random:exp . args)
    (let ((state (if (null? args) *random-state* (car args))))
      (- (log (random:uniform state)))))
  
--- 125,131 ----
  	((negative? n))
        (vector-set! vect n (* r (vector-ref vect n))))))
  
! (define-integrable (random:exp . args)
    (let ((state (if (null? args) *random-state* (car args))))
      (- (log (random:uniform state)))))
  
diff -c slib/random.scm nlib/random.scm
*** slib/random.scm	Tue Feb  2 00:02:58 1993
--- nlib/random.scm	Tue Feb  9 00:21:18 1993
***************
*** 35,40 ****
--- 35,50 ----
  ;procedures for generating inexact distributions.
  ;;;;------------------------------------------------------------------
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate-external "logical"))
+ (declare (integrateb
+ 	  random:tap-1
+ 	  random:size
+ 	  random:chunk-size
+ 	  random:MASK
+ 	  random))
+ 
  (require 'logical)
  
  (define random:tap 24)
***************
*** 45,50 ****
--- 55,61 ----
    (if (and (exact? trial) (>= most-positive-fixnum trial))
        l
        (random:size-int (- l 1)))))
+ 
  (define random:chunk-size (* 4 (random:size-int 8)))
  
  (define random:MASK
***************
*** 107,113 ****
  ;;;random:uniform is in randinex.scm.  It is needed only if inexact is
  ;;;supported.
  
! (define (random:make-random-state . args)
    (let ((state (if (null? args) *random-state* (car args))))
      (list->vector (vector->list state))))
  
--- 118,124 ----
  ;;;random:uniform is in randinex.scm.  It is needed only if inexact is
  ;;;supported.
  
! (define-integrable (random:make-random-state . args)
    (let ((state (if (null? args) *random-state* (car args))))
      (list->vector (vector->list state))))
  
diff -c slib/rbtree.scm nlib/rbtree.scm
*** slib/rbtree.scm	Sat Jan  9 13:40:56 1993
--- nlib/rbtree.scm	Tue Feb  9 00:21:18 1993
***************
*** 5,11 ****
--- 5,24 ----
  ;;;; PGS, 6 Jul 1990
  ;;; jaffer@ai.mit.edu Ported to SLIB, 1/6/93
  
+ 
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ (declare (integrate 
+ 	  rb-tree-root
+ 	  set-rb-tree-root!
+ 	  rb-tree-left-rotation-field-maintainer
+ 	  rb-tree-right-rotation-field-maintainer
+ 	  rb-tree-insertion-field-maintainer
+ 	  rb-tree-deletion-field-maintainer
+ 	  rb-tree-prior?))
+ 
  (require 'record)
+ 
  (define rb-tree
    (make-record-type
     "rb-tree"
***************
*** 227,233 ****
  	   y)
  	(set! x y)
  	(set! y (rb-node-parent y)))))
- 
  
  ;;;; Deletion.  We do not entirely follow Cormen, Leiserson and Rivest's lead
  ;;;; here, because their use of sentinels is in rather obscenely poor taste.
--- 240,245 ----
diff -c slib/sort.scm nlib/sort.scm
*** slib/sort.scm	Wed Nov  6 00:50:38 1991
--- nlib/sort.scm	Tue Feb  9 00:22:03 1993
***************
*** 118,123 ****
--- 118,125 ----
  ;   in Scheme.
  ;;; --------------------------------------------------------------------
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))		; Honestly, nothing defined here clashes!
  
  ;;; (sorted? sequence less?)
  ;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
diff -c slib/printf.scm nlib/printf.scm
*** slib/printf.scm	Mon Oct 19 14:48:58 1992
--- nlib/printf.scm	Tue Feb  9 00:22:03 1993
***************
*** 3,8 ****
--- 3,19 ----
  
  ;;; Floating point is not handled yet.  It should not be hard to do.
  
+ ;;; Declarations for CScheme
+ (declare (usual-integrations))
+ 
+ (declare (integrate 
+ 	  printf
+ 	  fprintf
+ 	  sprintf
+ 	  stdin
+ 	  stdout
+ 	  stderr))
+ 
  (define (stdio:iprintf out format . args)
    (let loop ((pos 0) (args args))
      (if (< pos (string-length format))
***************
*** 96,105 ****
  	  (else (out (string-ref format pos))
  		(loop (+ pos 1) args))))))
  
! (define (stdio:printf format . args)
    (apply stdio:iprintf display format args))
  
! (define (stdio:fprintf port format . args)
    (if (equal? port (current-output-port))
        (apply stdio:iprintf display format args)
        (apply stdio:iprintf (lambda (x) (display x port)) format args)))
--- 107,116 ----
  	  (else (out (string-ref format pos))
  		(loop (+ pos 1) args))))))
  
! (define-integrable (stdio:printf format . args)
    (apply stdio:iprintf display format args))
  
! (define-integrable (stdio:fprintf port format . args)
    (if (equal? port (current-output-port))
        (apply stdio:iprintf display format args)
        (apply stdio:iprintf (lambda (x) (display x port)) format args)))
diff -c slib/strcase.scm nlib/strcase.scm
*** slib/strcase.scm	Wed Nov 18 14:15:18 1992
--- nlib/strcase.scm	Tue Feb  9 00:22:03 1993
***************
*** 8,27 ****
  ;string-upcase!, string-downcase!, string-capitalize!
  ; are destructive versions.
  
! (define (string-upcase! str)
    (do ((i (- (string-length str) 1) (- i 1)))
        ((< i 0) str)
      (string-set! str i (char-upcase (string-ref str i)))))
  
! (define (string-upcase str)
    (string-upcase! (string-copy str)))
    
! (define (string-downcase! str)
    (do ((i (- (string-length str) 1) (- i 1)))
        ((< i 0) str)
      (string-set! str i (char-downcase (string-ref str i)))))
  
! (define (string-downcase str)
    (string-downcase! (string-copy str)))
  
  (define (string-capitalize! str)	; "hello" -> "Hello"
--- 8,30 ----
  ;string-upcase!, string-downcase!, string-capitalize!
  ; are destructive versions.
  
! ;;; Declarations for CScheme
! (declare (usual-integrations))
! 
! (define-integrable (string-upcase! str)
    (do ((i (- (string-length str) 1) (- i 1)))
        ((< i 0) str)
      (string-set! str i (char-upcase (string-ref str i)))))
  
! (define-integrable (string-upcase str)
    (string-upcase! (string-copy str)))
    
! (define-integrable (string-downcase! str)
    (do ((i (- (string-length str) 1) (- i 1)))
        ((< i 0) str)
      (string-set! str i (char-downcase (string-ref str i)))))
  
! (define-integrable (string-downcase str)
    (string-downcase! (string-copy str)))
  
  (define (string-capitalize! str)	; "hello" -> "Hello"
***************
*** 38,42 ****
  		  (string-set! str i (char-upcase c))))
  	    (set! non-first-alpha #f))))))
  
! (define (string-capitalize str)
    (string-capitalize! (string-copy str)))
--- 41,45 ----
  		  (string-set! str i (char-upcase c))))
  	    (set! non-first-alpha #f))))))
  
! (define-integrable (string-capitalize str)
    (string-capitalize! (string-copy str)))
diff -c slib/synchk.scm nlib/synchk.scm
*** slib/synchk.scm	Mon Jan 27 09:28:48 1992
--- nlib/synchk.scm	Tue Feb  9 00:22:03 1993
***************
*** 35,45 ****
  ;;; written by Alan Bawden
  ;;; modified by Chris Hanson
  
! (define (syntax-check pattern form)
    (if (not (syntax-match? (cdr pattern) (cdr form)))
        (syntax-error "ill-formed special form" form)))
  
! (define (ill-formed-syntax form)
    (syntax-error "ill-formed special form" form))
  
  (define (syntax-match? pattern object)
--- 35,48 ----
  ;;; written by Alan Bawden
  ;;; modified by Chris Hanson
  
! ;;; Declarations for CScheme
! (declare (usual-integrations))
! 
! (define-integrable (syntax-check pattern form)
    (if (not (syntax-match? (cdr pattern) (cdr form)))
        (syntax-error "ill-formed special form" form)))
  
! (define-integrable (ill-formed-syntax form)
    (syntax-error "ill-formed special form" form))
  
  (define (syntax-match? pattern object)
