;;; "MISCIO" Search for string from port.
; Written 1995, 1996 by Oleg Kiselyov (oleg@ponder.csci.unt.edu)
; Modified 1996, 1997, 1998 by A. Jaffer (jaffer@ai.mit.edu)
;
; This code is in the public domain.

;;; Return the index of the first occurence of a-char in str, or #f
(define (string-index str a-char)
  (let loop ((pos 0))
    (cond
     ;; whole string has been searched, in vain
     ((>= pos (string-length str)) #f)
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (+ 1 pos))))))

(define (string-index-ci str a-char)
  (let loop ((pos 0))
    (cond
     ;; whole string has been searched, in vain
     ((>= pos (string-length str)) #f)
     ((char-ci=? a-char (string-ref str pos)) pos)
     (else (loop (+ 1 pos))))))

(define (string-reverse-index str a-char)
  (let loop ((pos (- (string-length str) 1)))
    (cond ((< pos 0) #f)
	  ((char=? (string-ref str pos) a-char) pos)
	  (else (loop (- pos 1))))))

(define (string-reverse-index-ci str a-char)
  (let loop ((pos (- (string-length str) 1)))
    (cond ((< pos 0) #f)
	  ((char-ci=? (string-ref str pos) a-char) pos)
	  (else (loop (- pos 1))))))

(define (miscio:substring? pattern str char=?)
  (let* ((pat-len (string-length pattern))
	 (search-span (- (string-length str) pat-len))
	 (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
	 (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
     ((not c1) 0)			; empty pattern, matches upfront
     ((not c2) (string-index str c1))	; one-char pattern
     (else				; matching pattern of > two chars
      (let outer ((pos 0))
	(cond
	 ((> pos search-span) #f)	; nothing was found thru the whole str
	 ((not (char=? c1 (string-ref str pos)))
	  (outer (+ 1 pos)))		; keep looking for the right beginning
	 ((not (char=? c2 (string-ref str (+ 1 pos))))
	  (outer (+ 1 pos)))		; could've done pos+2 if c1 == c2....
	 (else				; two char matched: high probability
					; the rest will match too
	  (let inner ((i-pat 2) (i-str (+ 2 pos)))
	    (if (>= i-pat pat-len) pos	; the whole pattern matched
		(if (char=? (string-ref pattern i-pat)
			    (string-ref str i-str))
		    (inner (+ 1 i-pat) (+ 1 i-str))
		    ;; mismatch after partial match
		    (outer (+ 1 pos))))))))))))

(define (substring? pattern str) (miscio:substring? pattern str char=?))
(define (substring-ci? pattern str) (miscio:substring? pattern str char-ci=?))

(define (find-string-from-port? str <input-port> . max-no-char)
  (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
  (letrec
      ((no-chars-read 0)
       (peeked? #f)
       (my-peek-char			; Return a peeked char or #f
	(lambda () (and (or (not (number? max-no-char))
			    (< no-chars-read max-no-char))
			(let ((c (peek-char <input-port>)))
			  (cond (peeked? c)
				((eof-object? c) #f)
				((procedure? max-no-char)
				 (set! peeked? #t)
				 (if (max-no-char c) #f c))
				((eqv? max-no-char c) #f)
				(else c))))))
       (next-char (lambda () (set! peeked? #f) (read-char <input-port>)
			  (set! no-chars-read  (+ 1 no-chars-read))))
       (match-1st-char			; of the string str
	(lambda ()
	  (let ((c (my-peek-char)))
	    (and c
		 (begin (next-char)
			(if (char=? c (string-ref str 0))
			    (match-other-chars 1)
			    (match-1st-char)))))))
       ;; There has been a partial match, up to the point pos-to-match
       ;; (for example, str[0] has been found in the stream)
       ;; Now look to see if str[pos-to-match] for would be found, too
       (match-other-chars
	(lambda (pos-to-match)
	  (if (>= pos-to-match (string-length str))
	      no-chars-read		; the entire string has matched
	      (let ((c (my-peek-char)))
		(and c
		     (if (not (char=? c (string-ref str pos-to-match)))
			 (backtrack 1 pos-to-match)
			 (begin (next-char)
				(match-other-chars (+ 1 pos-to-match)))))))))

       ;; There had been a partial match, but then a wrong char showed up.
       ;; Before discarding previously read (and matched) characters, we check
       ;; to see if there was some smaller partial match. Note, characters read
       ;; so far (which matter) are those of str[0..matched-substr-len - 1]
       ;; In other words, we will check to see if there is such i>0 that
       ;; substr(str,0,j) = substr(str,i,matched-substr-len)
       ;; where j=matched-substr-len - i
       (backtrack
	(lambda (i matched-substr-len)
	  (let ((j (- matched-substr-len i)))
	    (if (<= j 0)
		;; backed off completely to the begining of str
		(match-1st-char)
		(let loop ((k 0))
		  (if (>= k j)
		      (match-other-chars j) ; there was indeed a shorter match
		      (if (char=? (string-ref str k)
				  (string-ref str (+ i k)))
			  (loop (+ 1 k))
			  (backtrack (+ 1 i) matched-substr-len))))))))
       )
    (match-1st-char)))

(define (string-subst text old new . rest)
  (define sub
    (lambda (text)
      (set! text
	    (cond ((equal? "" text) text)
		  ((substring? old text)
		   => (lambda (idx)
			(string-append
			 (substring text 0 idx)
			 new
			 (sub (substring
			       text (+ idx (string-length old))
			       (string-length text))))))
		  (else text)))
      (if (null? rest)
	  text
	  (apply string-subst text rest))))
  (sub text))

