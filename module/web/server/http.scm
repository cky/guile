;;; Web I/O: HTTP

;; Copyright (C)  2010 Free Software Foundation, Inc.

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

;;; Code:

(define-module (web server http)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (system repl error-handling))


(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock family addr port)
    sock))

;; -> server
(define* (http-open #:key
                      (host #f)
                      (family AF_INET)
                      (addr (if host
                                (inet-pton family host)
                                INADDR_LOOPBACK))
                      (port 8080)
                      (socket (make-default-socket family addr port)))
  (listen socket 5)
  (sigaction SIGPIPE SIG_IGN)
  socket)

;; -> (keep-alive client request body | keep-alive #f #f #f)
(define (http-read server keep-alive)
  (call-with-values (lambda ()
                      (let ((ports (cons server keep-alive)))
                        (apply values (select ports '() ports))))
    (lambda (readable writable except)
      (cond
       ((pair? except)
        (values (fold (lambda (p keep-alive)
                        (close-port p)
                        (if (eq? p server)
                            (throw 'interrupt)
                            (delq p keep-alive)))
                      keep-alive
                      except)
                #f #f #f))
       ((memq server readable)
        ;; FIXME: meta to read-request
        (let* ((client (let ((pair (accept server)))
                         ;; line buffered for request
                         (setvbuf (car pair) _IOLBF)
                         pair))
               (req (read-request (car client)))
               (body-str (begin
                           ;; block buffered for body and response
                           (setvbuf (car client) _IOFBF)
                           (read-request-body/latin-1 req))))
          (values keep-alive (car client) req body-str)))
       ((pair? readable)
        ;; FIXME: preserve meta for keep-alive
        (let* ((p (car readable))
               (keep-alive (delq p keep-alive)))
          (if (eof-object? (peek-char p))
              (begin
                (close-port p)
                (values keep-alive #f #f #f))
              (call-with-error-handling
               (lambda ()
                 ;; http-write already left p in line-buffered state
                 (let* ((req (read-request p))
                        (body-str (begin
                                    ;; block buffered for body and response
                                    (setvbuf p _IOFBF)
                                    (read-request-body/latin-1 req))))
                   (values keep-alive p req body-str)))
               #:pass-keys '(quit interrupt)
               #:on-error (if (batch-mode?) 'pass 'debug)
               #:post-error
               (lambda (k . args)
                 (warn "Error while reading request" k args)
                 (values keep-alive #f #f #f #f))))))
       (else
        (values keep-alive #f #f #f))))))

(define (keep-alive? response)
  (let ((v (response-version response)))
    (case (car v)
      ((1)
       (case (cdr v)
         ((1) #t)
         ((0) (memq 'keep-alive (response-connection response)))))
      (else #f))))

;; -> (#f | client)
(define (http-write server client response body)
  (let ((response (write-response response client)))
    (cond
     ((not body))                       ; pass
     ((string? body)
      (write-response-body/latin-1 response body))
     ((bytevector? body)
      (write-response-body/bytevector response body))
     (else
      (error "Expected a string or bytevector for body" body)))
    (force-output (response-port response))
    (if (keep-alive? response)
        (let ((p (response-port response)))
          ;; back to line buffered
          (setvbuf p _IOLBF)
          p)
        (begin
          (close-port (response-port response))
          #f))))

;; -> unspecified values
(define (http-close server)
  (shutdown server 2)
  (close-port server))

(define-server-impl http
  http-open
  http-read
  http-write
  http-close)
