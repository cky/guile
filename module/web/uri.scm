;;;; (web uri) --- URI manipulation tools
;;;;
;;;; Copyright (C) 1997,2001,2002,2010 Free Software Foundation, Inc.
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

;;; Commentary:

;; Based on (www url). To be documented.

;;; Code:

(define-module (web uri)
  #:export (uri?
            uri-scheme uri-userinfo uri-host uri-port
            uri-path uri-query uri-fragment

            build-uri
            parse-uri unparse-uri
            uri-decode uri-encode
            split-and-decode-uri-path
            encode-and-join-uri-path)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 control)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports))

(define-record-type <uri>
  (make-uri scheme userinfo host port path query fragment)
  uri?
  (scheme uri-scheme)
  (userinfo uri-userinfo)
  (host uri-host)
  (port uri-port)
  (path uri-path)
  (query uri-query)
  (fragment uri-fragment))

(define (positive-exact-integer? port)
  (and (number? port) (exact? port) (integer? port) (positive? port)))

(define (validate-uri scheme userinfo host port path query fragment)
  (cond
   ((not (symbol? scheme))
    (error "expected a symbol for the URI scheme" scheme))
   ((and (or userinfo port) (not host))
    (error "expected host, given userinfo or port"))
   ((and port (not (positive-exact-integer? port)))
    (error "expected integer port" port))
   ((and host (or (not (string? host)) (not (valid-host? host))))
    (error "expected valid host" host))
   ((and userinfo (not (string? userinfo)))
    (error "expected string for userinfo" userinfo))
   ((not (string? path))
    (error "expected string for path" path))
   ((and host (not (string-null? path))
         (not (eqv? (string-ref path 0) #\/)))
    (error "expected path of absolute URI to start with a /" path))))

(define* (build-uri scheme #:key userinfo host port (path "") query fragment
                    (validate? #t))
  (if validate?
      (validate-uri scheme userinfo host port path query fragment))
  (make-uri scheme userinfo host port path query fragment))

;; See RFC 3986 #3.2.2 for comments on percent-encodings, IDNA (RFC
;; 3490), and non-ASCII host names.
;;
(define ipv4-regexp
  (make-regexp "^([0-9.]+)"))
(define ipv6-regexp
  (make-regexp "^\\[([0-9a-fA-F:]+)\\]+"))
(define domain-label-regexp
  (make-regexp "^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$"))
(define top-label-regexp
  (make-regexp "^[a-zA-Z]([a-zA-Z0-9-]*[a-zA-Z0-9])?$"))

(define (valid-host? host)
  (cond
   ((regexp-exec ipv4-regexp host)
    => (lambda (m)
         (false-if-exception (inet-pton AF_INET (match:substring m 1)))))
   ((regexp-exec ipv6-regexp host)
    => (lambda (m)
         (false-if-exception (inet-pton AF_INET6 (match:substring m 1)))))
   (else
    (let ((labels (reverse (string-split host #\.))))
      (and (pair? labels)
           (regexp-exec top-label-regexp (car labels))
           (and-map (lambda (label)
                      (regexp-exec domain-label-regexp label))
                    (cdr labels)))))))

(define userinfo-pat
  "[a-zA-Z0-9_.!~*'();:&=+$,-]+")
(define host-pat
  "[a-zA-Z0-9.-]+")
(define port-pat
  "[0-9]*")
(define authority-regexp
  (make-regexp
   (format #f "^//((~a)@)?(~a)(:(~a))?$"
           userinfo-pat host-pat port-pat)))

(define (parse-authority authority fail)
  (let ((m (regexp-exec authority-regexp authority)))
    (if (and m (valid-host? (match:substring m 3)))
        (values (match:substring m 2)
                (match:substring m 3)
                (let ((port (match:substring m 5)))
                  (and port (not (string-null? port))
                       (string->number port))))
        (fail))))


;;; RFC 3986, #3.
;;;
;;;   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
;;;
;;;   hier-part   = "//" authority path-abempty
;;;               / path-absolute
;;;               / path-rootless
;;;               / path-empty

(define scheme-pat
  "[a-zA-Z][a-zA-Z0-9+.-]*")
(define authority-pat
  "[^/?#]*")
(define path-pat
  "[^?#]*")
(define query-pat
  "[^#]*")
(define fragment-pat
  ".*")
(define uri-pat
  (format #f "^(~a):(//~a)?(~a)(\\?(~a))?(#(~a))?$"
          scheme-pat authority-pat path-pat query-pat fragment-pat))
(define uri-regexp
  (make-regexp uri-pat))

(define (parse-uri string)
  (% (let ((m (regexp-exec uri-regexp string)))
       (if (not m) (abort))
       (let ((scheme (string->symbol
                      (string-downcase (match:substring m 1))))
             (authority (match:substring m 2))
             (path (match:substring m 3))
             (query (match:substring m 5))
             (fragment (match:substring m 7)))
         (call-with-values
             (lambda ()
               (if authority
                   (parse-authority authority abort)
                   (values #f #f #f)))
           (lambda (userinfo host port)
             (make-uri scheme userinfo host port path query fragment)))))
     (lambda (k)
       #f)))

(define (unparse-uri uri)
  (let* ((scheme-str (string-append
                      (symbol->string (uri-scheme uri)) ":"))
         (userinfo (uri-userinfo uri))
         (host (uri-host uri))
         (port (uri-port uri))
         (path (uri-path uri))
         (query (uri-query uri))
         (fragment (uri-fragment uri)))
    (string-append
     scheme-str
     (if host
         (string-append "//"
                        (if userinfo (string-append userinfo "@")
                            "")
                        host
                        (if port
                            (string-append ":" (number->string port))
                            ""))
         "")
     path
     (if query
         (string-append "?" query)
         "")
     (if fragment
         (string-append "#" fragment)
         ""))))


;; A note on characters and bytes: URIs are defined to be sequences of
;; characters in a subset of ASCII. Those characters may encode a
;; sequence of bytes (octets), which in turn may encode sequences of
;; characters in other character sets.
;;

;; Return a new string made from uri-decoding @var{str}.  Specifically,
;; turn @code{+} into space, and hex-encoded @code{%XX} strings into
;; their eight-bit characters.
;;
(define hex-chars
  (string->char-set "0123456789abcdefABCDEF"))

(define* (uri-decode str #:key (charset 'utf-8))
  (let ((len (string-length str)))
    (call-with-values open-bytevector-output-port
      (lambda (port get-bytevector)
        (let lp ((i 0))
          (if (= i len)
              ((case charset
                 ((utf-8) utf8->string)
                 ((#f) (lambda (x) x)) ; raw bytevector
                 (else (error "unknown charset" charset)))
               (get-bytevector))
              (let ((ch (string-ref str i)))
                (cond
                 ((eqv? ch #\+)
                  (put-u8 port (char->integer #\space))
                  (lp (1+ i)))
                 ((and (< (+ i 2) len) (eqv? ch #\%)
                       (let ((a (string-ref str (+ i 1)))
                             (b (string-ref str (+ i 2))))
                         (and (char-set-contains? hex-chars a)
                              (char-set-contains? hex-chars b)
                              (string->number (string a b) 16))))
                  => (lambda (u8)
                       (put-u8 port u8)
                       (lp (+ i 3))))
                 ((< (char->integer ch) 128)
                  (put-u8 port (char->integer ch))
                  (lp (1+ i)))
                 (else
                  (error "invalid character in encoded URI" str ch))))))))))
  
(define ascii-alnum-chars
  (string->char-set
   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

;; RFC 3986, #2.2.
(define gen-delims
  (string->char-set ":/?#[]@"))
(define sub-delims
  (string->char-set "!$&'()*+,l="))
(define reserved-chars
  (char-set-union gen-delims sub-delims))

;; RFC 3986, #2.3
(define unreserved-chars
  (char-set-union ascii-alnum-chars
                  (string->char-set "-._~")))

;; Return a new string made from uri-encoding @var{str}, unconditionally
;; transforming any characters not in @var{unescaped-chars}.
;;
(define* (uri-encode str #:key (charset 'utf-8)
                     (unescaped-chars unreserved-chars))
  (define (put-utf8 binary-port str)
    (put-bytevector binary-port (string->utf8 str)))

  ((case charset
     ((utf-8) utf8->string)
     ((#f) (lambda (x) x)) ; raw bytevector
     (else (error "unknown charset" charset)))
   (call-with-values open-bytevector-output-port
     (lambda (port get-bytevector)
       (string-for-each
        (lambda (ch)
          (if (char-set-contains? unescaped-chars ch)
              (put-utf8 port (string ch))
              (let* ((utf8 (string->utf8 (string ch)))
                     (len (bytevector-length utf8)))
                ;; Encode each byte.
                (let lp ((i 0))
                  (if (< i len)
                      (begin
                        (put-utf8 port (string #\%))
                        (put-utf8 port
                                  (number->string (bytevector-u8-ref utf8 i) 16))
                        (lp (1+ i))))))))
        str)
       (get-bytevector)))))

(define (split-and-decode-uri-path path)
  (filter (lambda (x) (not (string-null? x)))
          (map uri-decode (string-split path #\/))))

(define (encode-and-join-uri-path parts)
  (string-join (map uri-encode parts) "/"))
