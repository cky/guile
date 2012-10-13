;;; Web client

;; Copyright (C) 2011, 2012 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:
;;;
;;; (web client) is a simple HTTP URL fetcher for Guile.
;;;
;;; In its current incarnation, (web client) is synchronous.  If you
;;; want to fetch a number of URLs at once, probably the best thing to
;;; do is to write an event-driven URL fetcher, similar in structure to
;;; the web server.
;;;
;;; Another option, good but not as performant, would be to use threads,
;;; possibly via a thread pool.
;;;
;;; Code:

(define-module (web client)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (open-socket-for-uri
            http-get))

(define (open-socket-for-uri uri)
  "Return an open input/output port for a connection to URI."
  (define addresses
    (let ((port (uri-port uri)))
      (getaddrinfo (uri-host uri)
                   (cond (port => number->string)
                         (else (symbol->string (uri-scheme uri))))
                   (if port
                       AI_NUMERICSERV
                       0))))

  (let loop ((addresses addresses))
    (let* ((ai (car addresses))
           (s  (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                       (addrinfo:protocol ai))))
      (set-port-encoding! s "ISO-8859-1")

      (catch 'system-error
        (lambda ()
          (connect s (addrinfo:addr ai))

          ;; Buffer input and output on this port.
          (setvbuf s _IOFBF)
          ;; Enlarge the receive buffer.
          (setsockopt s SOL_SOCKET SO_RCVBUF (* 12 1024))
          s)
        (lambda args
          ;; Connection failed, so try one of the other addresses.
          (close s)
          (if (null? addresses)
              (apply throw args)
              (loop (cdr addresses))))))))

(define (decode-string bv encoding)
  (if (string-ci=? encoding "utf-8")
      (utf8->string bv)
      (let ((p (open-bytevector-input-port bv)))
        (set-port-encoding! p encoding)
        (let ((res (read-delimited "" p)))
          (close-port p)
          res))))

(define (text-type? type)
  (let ((type (symbol->string type)))
    (or (string-prefix? "text/" type)
        (string-suffix? "/xml" type)
        (string-suffix? "+xml" type))))

;; Logically the inverse of (web server)'s `sanitize-response'.
;;
(define (decode-response-body response body)
  ;; `body' is either #f or a bytevector.
  (cond
   ((not body) body)
   ((bytevector? body)
    (let ((rlen (response-content-length response))
          (blen (bytevector-length body)))
      (cond
       ((and rlen (not (= rlen blen)))
        (error "bad content-length" rlen blen))
       ((response-content-type response)
        => (lambda (type)
             (cond
              ((text-type? (car type))
               (decode-string body (or (assq-ref (cdr type) 'charset)
                                       "iso-8859-1")))
              (else body))))
       (else body))))
   (else
    (error "unexpected body type" body))))

(define* (http-get uri #:key (port (open-socket-for-uri uri))
                   (version '(1 . 1)) (keep-alive? #f) (extra-headers '())
                   (decode-body? #t))
  (let ((req (build-request uri #:version version
                            #:headers (if keep-alive?
                                          extra-headers
                                          (cons '(connection close)
                                                extra-headers)))))
    (write-request req port)
    (force-output port)
    (if (not keep-alive?)
        (shutdown port 1))
    (let* ((res (read-response port))
           (body (read-response-body res)))
      (if (not keep-alive?)
          (close-port port))
      (values res
              (if decode-body?
                  (decode-response-body res body)
                  body)))))
