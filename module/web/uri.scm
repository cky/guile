;;; www/url.scm --- URL manipulation tools

;; 	Copyright (C) 1997,2001,2002 Free Software Foundation, Inc.
;;
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
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA
;;

;;; Commentary:

;; This module exports the following procedures:
;;   (url:scheme url)
;;   (url:address url)
;;   (url:unknown url)
;;   (url:user url)
;;   (url:host url)
;;   (url:port url)
;;   (url:path url)
;;   (url:make scheme . args)
;;   (url:make-http host port path)
;;   (url:make-ftp user host port path)
;;   (url:make-mailto address)
;;   (url:parse url)
;;   (url:unparse url)
;;   (url:decode str)
;;   (url:encode str reserved-chars)

;;; Code:


;; TODO:
;;   * support `user:password@' strings where appropriate in URLs.
;;   * make URL parsing smarter.  This is good for most TCP/IP-based
;;	 URL schemes, but parsing is actually specific to each URL scheme.
;;   * fill out url:encode, include facilities for URL-scheme-specific
;;     encoding methods (e.g. a url-scheme-reserved-char-alist)

(define-module (tekuti url)
  #:use-module ((srfi srfi-1) #:select (filter))
  #:use-module (ice-9 regex))

;; `url:scheme' is an unfortunate term, but it is the technical
;; name for that portion of the URL according to RFC 1738. Sigh.

(define-public (url:scheme url)  (vector-ref url 0))
(define-public (url:address url) (vector-ref url 1))
(define-public (url:unknown url) (vector-ref url 1))
(define-public (url:user url)    (vector-ref url 1))
(define-public (url:host url)    (vector-ref url 2))
(define-public (url:port url)    (vector-ref url 3))
(define-public (url:path url)    (vector-ref url 4))

(define-public (url:make scheme . args)
  (apply vector scheme args))
(define-public (url:make-http host port path)
  (vector 'http #f host port path))
(define-public (url:make-ftp user host port path)
  (vector 'ftp user host port path))
(define-public (url:make-mailto address)
  (vector 'mailto address))

(define http-regexp (make-regexp "^http://([^:/]+)(:([0-9]+))?(/(.*))?$"))
(define ftp-regexp
  (make-regexp "^ftp://(([^@:/]+)@)?([^:/]+)(:([0-9]+))?(/(.*))?$"))
(define mailto-regexp (make-regexp "^mailto:(.*)$"))

(define-public (url:parse url)
  (cond
   ((regexp-exec http-regexp url)
    => (lambda (m)
	 (url:make-http (match:substring m 1)
			(cond ((match:substring m 3) => string->number)
			      (else #f))
			(match:substring m 5))))

   ((regexp-exec ftp-regexp url)
    => (lambda (m)
	 (url:make-ftp (match:substring m 2)
		       (match:substring m 3)
		       (cond ((match:substring m 5) => string->number)
			     (else #f))
		       (match:substring m 7))))

   ((regexp-exec mailto-regexp url)
    => (lambda (m)
	 (url:make-mailto (match:substring m 1))))

   (else
    (url:make 'unknown url))))


(define-public (url:unparse url)
  (define (pathy scheme username url)   ; username not used!
    (format #f "~A://~A~A~A"
            scheme
            (url:host url)
            (cond ((url:port url) => (lambda (port) (format #f ":~A" port)))
                  (else ""))
            (cond ((url:path url) => (lambda (path) (format #f "/~A" path)))
                  (else ""))))
  (case (url:scheme url)
    ((http) (pathy 'http #f url))
    ((ftp)  (pathy 'ftp (url:user url) url))
    ((mailto) (format #f "mailto:~A" (url:address url)))
    ((unknown) (url:unknown url))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (url-decode STR)
;; Turn + into space, and hex-encoded %XX strings into their
;; eight-bit characters.  Is a regexp faster than character
;; scanning?  Does it incur more overhead (which may be more
;; important for code that frequently gets restarted)?

(define-public (url:decode str)
  (regexp-substitute/global
   #f "\\+|%([0-9A-Fa-f][0-9A-Fa-f])" str
   'pre
   (lambda (m)
     (cond ((string=? "+" (match:substring m 0)) " ")
           (else (integer->char
                  (string->number
                   (match:substring m 1)
                   16)))))
   'post))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (url-encode STR)
;; The inverse of url-decode.  Can't be done easily with
;; a regexp: we would have to construct a regular expression
;; like "[\277-\377]", for example, and Guile strings don't
;; let you interpolate character literals.  Pity.
;;   URL-encode any characters in STR that are not safe: these
;; include any character not in the SAFE-CHARS list and any
;; character that *is* in the RESERVED-CHARS list argument.

(define-public (url:encode str)
  (with-output-to-string
    (lambda ()
      (for-each (lambda (ch)
		  (if (safe-char? ch)
		      (display ch)
		      (begin
			(display #\%)
			(display (number->string (char->integer ch) 16)))))
		(string->list str)))))

(define special-chars
  (string->list "$-_.+!*'()"))
(define reserved-chars
  (string->list ";/?:@&="))

(define (safe-char? ch)
  ;; ``Thus, only alphanumerics, the special characters "$-_.+!*'(),", and
  ;; reserved characters used for their reserved purposes may be used
  ;; unencoded within a URL.'' RFC 1738, #2.2.
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (memv ch special-chars)))

(define-public (url:path-part path)
  (substring path 0 (or (string-index path #\?) (string-length path))))

(define-public (url:query-part path)
  (let ((q (string-index path #\?)))
    (if q (substring path (1+ q)) #f)))

(define-public (url:path-split path)
  (filter (lambda (x) (not (string-null? x)))
          (map url:decode (string-split (url:path-part path) #\/))))

(define-public (url:path-join path)
  (string-join (map url:encode path) "/"))

;;; www/url.scm ends here
