;;; Web server

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

;;; Commentary:
;;;
;;; (web server) is a generic web server interface, along with a main
;;; loop implementation for web servers controlled by Guile.
;;;
;;; The lowest layer is the <server-impl> object, which defines a set of
;;; hooks to open a server, read a request from a client, write a
;;; response to a client, and close a server.  These hooks -- open,
;;; read, write, and close, respectively -- are bound together in a
;;; <server-impl> object.  Procedures in this module take a
;;; <server-impl> object, if needed.
;;;
;;; A <server-impl> may also be looked up by name.  If you pass the
;;; `http' symbol to `run-server', Guile looks for a variable named
;;; `http' in the `(web server http)' module, which should be bound to a
;;; <server-impl> object.  Such a binding is made by instantiation of
;;; the `define-server-impl' syntax.  In this way the run-server loop can
;;; automatically load other backends if available.
;;;
;;; The life cycle of a server goes as follows:
;;;
;;;   * The `open' hook is called, to open the server. `open' takes 0 or
;;;     more arguments, depending on the backend, and returns an opaque
;;;     server socket object, or signals an error.
;;;
;;;   * The `read' hook is called, to read a request from a new client.
;;;     The `read' hook takes one arguments, the server socket.  It
;;;     should return three values: an opaque client socket, the
;;;     request, and the request body. The request should be a
;;;     `<request>' object, from `(web request)'.  The body should be a
;;;     string or a bytevector, or `#f' if there is no body.
;;;
;;;     If the read failed, the `read' hook may return #f for the client
;;;     socket, request, and body.
;;;
;;;   * A user-provided handler procedure is called, with the request
;;;     and body as its arguments.  The handler should return two
;;;     values: the response, as a `<response>' record from `(web
;;;     response)', and the response body as a string, bytevector, or
;;;     `#f' if not present.  We also allow the reponse to be simply an
;;;     alist of headers, in which case a default response object is
;;;     constructed with those headers.
;;;
;;;   * The `write' hook is called with three arguments: the client
;;;     socket, the response, and the body.  The `write' hook returns no
;;;     values.
;;;
;;;   * At this point the request handling is complete. For a loop, we
;;;     loop back and try to read a new request.
;;;
;;;   * If the user interrupts the loop, the `close' hook is called on
;;;     the server socket.
;;;
;;; Code:

(define-module (web server)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (system repl error-handling)
  #:use-module (ice-9 control)
  #:export (define-server-impl
            lookup-server-impl
            open-server
            read-client
            handle-request
            sanitize-response
            write-client
            close-server
            serve-one-client
            run-server))

(define *timer* (gettimeofday))
(define (print-elapsed who)
  (let ((t (gettimeofday)))
    (pk who (+ (* (- (car t) (car *timer*)) 1000000)
               (- (cdr t) (cdr *timer*))))
    (set! *timer* t)))

(eval-when (expand)
  (define *time-debug?* #f))

(define-syntax debug-elapsed
  (lambda (x)
    (syntax-case x ()
      ((_ who)
       (if *time-debug?*
           #'(print-elapsed who)
           #'*unspecified*)))))

(define-record-type server-impl
  (make-server-impl name open read write close)
  server-impl?
  (name server-impl-name)
  (open server-impl-open)
  (read server-impl-read)
  (write server-impl-write)
  (close server-impl-close))

(define-syntax define-server-impl
  (syntax-rules ()
    ((_ name open read write close)
     (define name
       (make-server-impl 'name open read write close)))))

(define (lookup-server-impl impl)
  (cond
   ((server-impl? impl) impl)
   ((symbol? impl)
    (let ((impl (module-ref (resolve-module `(web server ,impl)) impl)))
      (if (server-impl? impl)
          impl
          (error "expected a server impl in module" `(web server ,impl)))))
   (else
    (error "expected a server-impl or a symbol" impl))))

;; -> server
(define (open-server impl open-params)
  (apply (server-impl-open impl) open-params))

;; -> (client request body | #f #f #f)
(define (read-client impl server)
  (call-with-error-handling
   (lambda ()
     ((server-impl-read impl) server))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'pass 'debug)
   #:post-error
   (lambda (k . args)
     (warn "Error while accepting client" k args)
     (values #f #f #f))))

(define (call-with-encoded-output-string charset proc)
  (if (and (string-ci=? charset "utf-8") #f)
      ;; I don't know why, but this appears to be faster; at least for
      ;; examples/debug-sxml.scm (650 reqs/s versus 510 reqs/s).
      (string->utf8 (call-with-output-string proc))
      (call-with-values
          (lambda ()
            (open-bytevector-output-port))
        (lambda (port get-bytevector)
          (set-port-encoding! port charset)
          (proc port)
          (get-bytevector)))))

(define (encode-string str charset)
  (if (string-ci=? charset "utf-8")
      (string->utf8 str)
      (call-with-encoded-output-string charset
                                       (lambda (port)
                                         (display str port)))))

;; -> response body
(define (sanitize-response request response body)
  (cond
   ((list? response)
    (sanitize-response request
                       (build-response #:version (request-version request)
                                       #:headers response)
                       body))
   ((not (equal? (request-version request) (response-version response)))
    (sanitize-response request
                       (adapt-response-version response
                                               (request-version request))
                       body))
   ((not body)
    (values response #vu8()))
   ((string? body)
    (let* ((type (response-content-type response
                                        '("text/plain")))
           (declared-charset (assoc-ref (cdr type) "charset"))
           (charset (or declared-charset "utf-8")))
      (sanitize-response
       request
       (if declared-charset
           response
           (extend-response response 'content-type
                            `(,@type ("charset" . ,charset))))
       (encode-string body charset))))
   ((procedure? body)
    (let* ((type (response-content-type response
                                        '("text/plain")))
           (declared-charset (assoc-ref (cdr type) "charset"))
           (charset (or declared-charset "utf-8")))
      (sanitize-response
       request
       (if declared-charset
           response
           (extend-response response 'content-type
                            `(,@type ("charset" . ,charset))))
       (call-with-encoded-output-string charset body))))
   ((bytevector? body)
    ;; check length; assert type; add other required fields?
    (values (let ((rlen (response-content-length response))
                  (blen (bytevector-length body)))
              (cond
               (rlen (if (= rlen blen)
                         response
                         (error "bad content-length" rlen blen)))
               ((zero? blen) response)
               (else (extend-response response 'content-length blen))))
            body))
   (else
    (error "unexpected body type"))))

;; -> response body state
(define (handle-request handler request body state)
  (call-with-error-handling
   (lambda ()
     (call-with-values (lambda ()
                         (with-stack-and-prompt
                          (lambda ()
                            (apply handler request body state))))
       (lambda (response body . state)
         (call-with-values (lambda ()
                             (debug-elapsed 'handler)
                             (sanitize-response request response body))
           (lambda (response body)
             (debug-elapsed 'sanitize)
             (values response body state))))))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'pass 'debug)
   #:post-error
   (lambda (k . args)
     (warn "Error handling request" k args)
     (values (build-response #:code 500) #f state))))

;; -> unspecified values
(define (write-client impl server client response body)
  (call-with-error-handling
   (lambda ()
     ((server-impl-write impl) server client response body))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'pass 'debug)
   #:post-error
   (lambda (k . args)
     (warn "Error while writing response" k args)
     (values))))

;; -> unspecified values
(define (close-server impl server)
  ((server-impl-close impl) server))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk handler-thunk) (thunk))
      (lambda (thunk handler-thunk)
        (let ((handler #f))
          (catch 'interrupt
            (lambda ()
              (dynamic-wind
                (lambda ()
                  (set! handler
                        (sigaction SIGINT (lambda (sig) (throw 'interrupt)))))
                thunk
                (lambda ()
                  (if handler
                      ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                      (sigaction SIGINT (car handler) (cdr handler))
                      ;; restore original C handler.
                      (sigaction SIGINT #f)))))
            (lambda (k . _) (handler-thunk)))))))

(define (with-stack-and-prompt thunk)
  (call-with-prompt (default-prompt-tag)
                    (lambda () (start-stack #t (thunk)))
                    (lambda (k proc)
                      (with-stack-and-prompt (lambda () (proc k))))))
  
;; -> new-state
(define (serve-one-client handler impl server state)
  (debug-elapsed 'serve-again)
  (call-with-values
      (lambda ()
        (read-client impl server))
    (lambda (client request body)
      (debug-elapsed 'read-client)
      (if client
          (call-with-values
              (lambda ()
                (handle-request handler request body state))
            (lambda (response body state)
              (debug-elapsed 'handle-request)
              (write-client impl server client response body)
              (debug-elapsed 'write-client)
              state))
          state))))

(define* (run-server handler #:optional (impl 'http) (open-params '())
                     . state)
  (let* ((impl (lookup-server-impl impl))
         (server (open-server impl open-params)))
    (call-with-sigint
     (lambda ()
       (let lp ((state state))
         (lp (serve-one-client handler impl server state))))
     (lambda ()
       (close-server impl server)
       (values)))))
