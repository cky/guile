;;; "glob.scm" String matching for filenames (a la BASH).
;;; Copyright (C) 1998 Radey Shouman.
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

;;$Header: /home/ludo/src/guile/gitification/guile-cvs/guile/guile/guile-vm/module/slib/glob.scm,v 1.1 2001/04/14 11:24:45 kei Exp $
;;$Name:  $

(define (glob:pattern->tokens pat)
  (cond
   ((string? pat)
    (let loop ((i 0)
	       (toks '()))
      (if (>= i (string-length pat))
	  (reverse toks)
	  (let ((pch (string-ref pat i)))
	    (case pch
	      ((#\? #\*)
	       (loop (+ i 1)
		     (cons (substring pat i (+ i 1)) toks)))
	      ((#\[)
	       (let ((j
		      (let search ((j (+ i 2)))
			(cond
			 ((>= j (string-length pat))
			  (slib:error 'glob:make-matcher
				      "unmatched [" pat))
			 ((char=? #\] (string-ref pat j))
			  (if (and (< (+ j 1) (string-length pat))
				   (char=? #\] (string-ref pat (+ j 1))))
			      (+ j 1)
			      j))
			 (else (search (+ j 1)))))))
		 (loop (+ j 1) (cons (substring pat i (+ j 1)) toks))))
	      (else
	       (let search ((j (+ i 1)))
		 (cond ((= j (string-length pat))
			(loop j (cons (substring pat i j) toks)))
		       ((memv (string-ref pat j) '(#\? #\* #\[))
			(loop j (cons (substring pat i j) toks)))
		       (else (search (+ j 1)))))))))))
   ((pair? pat)
    (for-each (lambda (elt) (or (string? elt)
				(slib:error 'glob:pattern->tokens
					    "bad pattern" pat)))
	      pat)
    pat)
   (else (slib:error 'glob:pattern->tokens "bad pattern" pat))))

(define (glob:make-matcher pat ch=? ch<=?)
  (define (match-end str k kmatch)
    (and (= k (string-length str)) (reverse (cons k kmatch))))
  (define (match-str pstr nxt)
    (let ((plen (string-length pstr)))
      (lambda (str k kmatch)
	(and (<= (+ k plen) (string-length str))
	     (let loop ((i 0))
	       (cond ((= i plen)
		      (nxt str (+ k plen) (cons k kmatch)))
		     ((ch=? (string-ref pstr i)
			    (string-ref str (+ k i)))
		      (loop (+ i 1)))
		     (else #f)))))))
  (define (match-? nxt)
    (lambda (str k kmatch)
      (and (< k (string-length str))
	   (nxt str (+ k 1) (cons k kmatch)))))
  (define (match-set1 chrs)
    (let recur ((i 0))
      (cond ((= i (string-length chrs))
	     (lambda (ch) #f))
	    ((and (< (+ i 2) (string-length chrs))
		  (char=? #\- (string-ref chrs (+ i 1))))
	     (let ((nxt (recur (+ i 3))))
	       (lambda (ch)
		 (or (and (ch<=? ch (string-ref chrs (+ i 2)))
			  (ch<=? (string-ref chrs i) ch))
		     (nxt ch)))))
	    (else
	     (let ((nxt (recur (+ i 1)))
		   (chrsi (string-ref chrs i)))
	       (lambda (ch)
		 (or (ch=? chrsi ch) (nxt ch))))))))
  (define (match-set tok nxt)
    (let ((chrs (substring tok 1 (- (string-length tok) 1))))
      (if (and (positive? (string-length chrs))
	       (memv (string-ref chrs 0) '(#\^ #\!)))
	  (let ((pred (match-set1 (substring chrs 1 (string-length chrs)))))
	    (lambda (str k kmatch)
	      (and (< k (string-length str))
		   (not (pred (string-ref str k)))
		   (nxt str (+ k 1) (cons k kmatch)))))
	  (let ((pred (match-set1 chrs)))
	    (lambda (str k kmatch)
	      (and (< k (string-length str))
		   (pred (string-ref str k))
		   (nxt str (+ k 1) (cons k kmatch))))))))
  (define (match-* nxt)
    (lambda (str k kmatch)
      (let ((kmatch (cons k kmatch)))
	(let loop ((kk (string-length str)))
	  (and (>= kk k)
	       (or (nxt str kk kmatch)
		   (loop (- kk 1))))))))

  (let ((matcher
	 (let recur ((toks (glob:pattern->tokens pat)))
	   (if (null? toks)
	       match-end
	       (let ((pch (or (string=? (car toks) "")
			      (string-ref (car toks) 0))))
		 (case pch
		   ((#\?) (match-? (recur (cdr toks))))
		   ((#\*) (match-* (recur (cdr toks))))
		   ((#\[) (match-set (car toks) (recur (cdr toks))))
		   (else (match-str (car toks) (recur (cdr toks))))))))))
    (lambda (str) (matcher str 0 '()))))

(define (glob:caller-with-matches pat proc ch=? ch<=?)
  (define (glob:wildcard? pat)
    (cond ((string=? pat "") #f)
	  ((memv (string-ref pat 0) '(#\* #\? #\[)) #t)
	  (else #f)))
  (let* ((toks (glob:pattern->tokens pat))
	 (wild? (map glob:wildcard? toks))
	 (matcher (glob:make-matcher toks ch=? ch<=?)))
    (lambda (str)
      (let loop ((inds (matcher str))
		 (wild? wild?)
		 (res '()))
	(cond ((not inds) #f)
	      ((null? wild?)
	       (apply proc (reverse res)))
	      ((car wild?)
	       (loop (cdr inds)
		     (cdr wild?)
		     (cons (substring str (car inds) (cadr inds)) res)))
	      (else
	       (loop (cdr inds) (cdr wild?) res)))))))

(define (glob:make-substituter pattern template ch=? ch<=?)
  (define (wildcard? pat)
    (cond ((string=? pat "") #f)
	  ((memv (string-ref pat 0) '(#\* #\? #\[)) #t)
	  (else #f)))
  (define (countq val lst)
    (do ((lst lst (cdr lst))
	 (c 0 (if (eq? val (car lst)) (+ c 1) c)))
	((null? lst) c)))
  (let ((tmpl-literals (map (lambda (tok)
			      (if (wildcard? tok) #f tok))
			    (glob:pattern->tokens template)))
	(pat-wild? (map wildcard? (glob:pattern->tokens pattern)))
	(matcher (glob:make-matcher pattern ch=? ch<=?)))
    (or (= (countq #t pat-wild?) (countq #f tmpl-literals))
	(slib:error 'glob:make-substituter
		    "number of wildcards doesn't match" pattern template))
    (lambda (str)
      (let ((indices (matcher str)))
	(and indices
	     (let loop ((inds indices)
			(wild? pat-wild?)
			(lits tmpl-literals)
			(res '()))
	       (cond
		((null? lits)
		 (apply string-append (reverse res)))
		((car lits)
		 (loop inds wild? (cdr lits) (cons (car lits) res)))
		((null? wild?)		;this should never happen.
		 (loop '() '() lits res))
		((car wild?)
		 (loop (cdr inds) (cdr wild?) (cdr lits)
		       (cons (substring str (car inds) (cadr inds))
			     res)))
		(else
		 (loop (cdr inds) (cdr wild?) lits res)))))))))


(define (glob:match?? pat)
  (glob:make-matcher pat char=? char<=?))
(define (glob:match-ci?? pat)
  (glob:make-matcher pat char-ci=? char-ci<=?))
(define filename:match?? glob:match??)
(define filename:match-ci?? glob:match-ci??)

(define (glob:substitute?? pat templ)
  (cond ((procedure? templ)
	 (glob:caller-with-matches pat templ char=? char<=?))
	((string? templ)
	 (glob:make-substituter pat templ char=? char<=?))
	(else
	 (slib:error 'glob:substitute "bad second argument" templ))))
(define (glob:substitute-ci?? pat templ)
  (cond ((procedure? templ)
	 (glob:caller-with-matches pat templ char-ci=? char-ci<=?))
	((string? templ)
	 (glob:make-substituter pat templ char-ci=? char-ci<=?))
	(else
	 (slib:error 'glob:substitute "bad second argument" templ))))
(define filename:substitute?? glob:substitute??)
(define filename:substitute-ci?? glob:substitute-ci??)

(define (replace-suffix str old new)
  (let* ((f (glob:make-substituter (list "*" old) (list "*" new)
				   char=? char<=?))
	 (g (lambda (st)
	      (or (f st)
		  (slib:error 'replace-suffix "suffix doesn't match:"
			      old st)))))
    (if (pair? str)
	(map g str)
	(g str))))
