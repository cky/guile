; "lineio.scm", line oriented input/output functions for Scheme.
; Copyright (c) 1992, 1993 Aubrey Jaffer
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


;;@args
;;@args port
;;Returns a string of the characters up to, but not including a
;;newline or end of file, updating @var{port} to point to the
;;character following the newline.  If no characters are available, an
;;end of file object is returned.  The @var{port} argument may be
;;omitted, in which case it defaults to the value returned by
;;@code{current-input-port}.
(define (read-line . port)
  (let* ((char (apply read-char port)))
    (if (eof-object? char)
	char
	(do ((char char (apply read-char port))
	     (clist '() (cons char clist)))
	    ((or (eof-object? char) (char=? #\newline char))
	     (list->string (reverse clist)))))))

;;@args string
;;@args string port
;;Fills @1 with characters up to, but not including a newline or end
;;of file, updating the @var{port} to point to the last character read
;;or following the newline if it was read.  If no characters are
;;available, an end of file object is returned.  If a newline or end
;;of file was found, the number of characters read is returned.
;;Otherwise, @code{#f} is returned.  The @var{port} argument may be
;;omitted, in which case it defaults to the value returned by
;;@code{current-input-port}.
(define (read-line! str . port)
  (let* ((char (apply read-char port))
	 (midx (+ -1 (string-length str))))
    (if (eof-object? char)
	char
	(do ((char char (apply read-char port))
	     (i 0 (+ 1 i)))
	    ((or (eof-object? char)
		 (char=? #\newline char)
		 (> i midx))
	     (if (> i midx) #f i))
	  (string-set! str i char)))))

;;@args string
;;@args string port
;;Writes @1 followed by a newline to the given @var{port} and returns
;;an unspecified value.  The @var{Port} argument may be omitted, in
;;which case it defaults to the value returned by
;;@code{current-input-port}.@refill
(define (write-line str . port)
  (apply display str port)
  (apply newline port))

;;@args path
;;@args path port
;;Displays the contents of the file named by @1 to @var{port}.  The
;;@var{port} argument may be ommited, in which case it defaults to the
;;value returned by @code{current-output-port}.
(define (display-file path . port)
  (set! port (if (null? port) (current-output-port) (car port)))
  (call-with-input-file path
    (lambda (inport)
      (do ((line (read-line inport) (read-line inport)))
	  ((eof-object? line))
	(write-line line port)))))
