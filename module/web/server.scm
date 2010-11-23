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
;;;     The `read' hook takes two arguments: the server socket, and a
;;;     list of keep-alive clients.  It should return four values:  the
;;;     new list of keep-alive clients, an opaque client socket, the
;;;     request, and the request body. The request should be a
;;;     `<request>' object, from `(web request)'.  The body should be a
;;;     string or a bytevector, or `#f' if there is no body.
;;;
;;;     The keep-alive list is used when selecting a new request.  You
;;;     can either serve an old client or serve a new client; and some
;;;     old clients might close their connections while you are waiting.
;;;     The `read' hook returns a new keep-alive set to account for old
;;;     clients going away, and for read errors on old clients.
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
;;;     socket, the response, and the body.  The `write' hook may return
;;;     #f to indicate that the connection was closed.  If `write'
;;;     returns a true value, it will be consed onto the keep-alive
;;;     list.
;;;
;;;   * At this point the request handling is complete. For a loop, we
;;;     loop back with the new keep-alive list, and try to read a new
;;;     request.
;;;
;;;   * If the user interrupts the loop, the `close' hook is called on
;;;     the server socket.
;;;
;;; Code:

(define-module (web server)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
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

;; -> (keep-alive client request body | keep-alive #f #f #f)
(define (read-client impl server keep-alive)
  (call-with-error-handling
   (lambda ()
     ((server-impl-read impl) server keep-alive))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'pass 'debug)
   #:post-error
   (lambda (k . args)
     (warn "Error while accepting client" k args)
     (values keep-alive #f #f #f #f))))

;; -> response body state ...
(define (handle-request handler request body . state)
  (call-with-error-handling
   (lambda ()
     (with-stack-and-prompt
      (lambda ()
        (apply handler request body state))))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'pass 'debug)
   #:post-error
   (lambda (k . args)
     (warn "Error handling request" k args)
     (apply values (build-response #:code 500) #f state))))

(define (encode-string str charset)
  (case charset
    ((utf-8) (string->utf8 str))
    (else (error "unknown charset" charset))))

;; -> response body
(define (sanitize-response request response body)
  (cond
   ((list? response)
    (sanitize-response request (build-response #:headers response) body))
   ((not body)
    (values response #vu8()))
   ((string? body)
    (let* ((type (response-content-type response
                                        '("text/plain")))
           (declared-charset (assoc-ref (cdr type) "charset"))
           (charset (if declared-charset
                        (string->symbol 
                         (string-downcase declared-charset))
                        'utf-8)))
      (sanitize-response
       request
       (if declared-charset
           response
           (extend-response response 'content-type
                            `(,@type ("charset" . ,(symbol->string charset)))))
       (encode-string body charset))))
   ((procedure? body)
    (sanitize-response request response (call-with-output-string body)))
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

;; -> (#f | client)
(define (write-client impl server client response body)
  (call-with-error-handling
   (lambda ()
     ((server-impl-write impl) server client response body))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'pass 'debug)
   #:post-error
   (lambda (k . args)
     (warn "Error while writing response" k args)
     #f)))

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
  
(define (and-cons x xs)
  (if x (cons x xs) xs))

;; -> new keep-alive new-state
(define (serve-one-client handler impl server keep-alive state)
  (call-with-values
      (lambda ()
        (read-client impl server keep-alive))
    (lambda (keep-alive client request body)
      (if client
          (call-with-values
              (lambda ()
                (apply handle-request handler request body state))
            (lambda (response body . state)
              (call-with-values (lambda ()
                                  (sanitize-response request response body))
                (lambda (response body)
                  (values
                   (and-cons (write-client impl server client response body)
                             keep-alive)
                   state)))))
          (values keep-alive state)))))

(define* (run-server handler #:optional (impl 'http) (open-params '())
                     . state)
  (let* ((impl (lookup-server-impl impl))
         (server (open-server impl open-params)))
    (call-with-sigint
     (lambda ()
       (let lp ((keep-alive '()) (state state))
         (call-with-values
             (lambda ()
               (serve-one-client handler impl server keep-alive state))
           (lambda (new-keep-alive new-state)
             (lp new-keep-alive new-state)))))
     (lambda ()
       (close-server impl server)
       (values)))))
