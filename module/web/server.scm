;;; Web server

;; Copyright (C)  2010, 2011 Free Software Foundation, Inc.

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
  #:use-module (ice-9 binary-ports)
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
  "Look up a server implementation.  If @var{impl} is a server
implementation already, it is returned directly.  If it is a symbol, the
binding named @var{impl} in the @code{(web server @var{impl})} module is
looked up.  Otherwise an error is signaled.

Currently a server implementation is a somewhat opaque type, useful only
for passing to other procedures in this module, like
@code{read-client}."
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
  "Open a server for the given implementation.  Returns one value, the
new server object.  The implementation's @code{open} procedure is
applied to @var{open-params}, which should be a list."
  (apply (server-impl-open impl) open-params))

;; -> (client request body | #f #f #f)
(define (read-client impl server)
  "Read a new client from @var{server}, by applying the implementation's
@code{read} procedure to the server.  If successful, returns three
values: an object corresponding to the client, a request object, and the
request body.  If any exception occurs, returns @code{#f} for all three
values."
  (call-with-error-handling
   (lambda ()
     ((server-impl-read impl) server))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'pass 'debug)
   #:post-error
   (lambda (k . args)
     (warn "Error while accepting client" k args)
     (values #f #f #f))))

;; like call-with-output-string, but actually closes the port (doh)
(define (call-with-output-string* proc)
  (let ((port (open-output-string)))
    (proc port)
    (let ((str (get-output-string port)))
      (close-port port)
      str)))

(define (call-with-output-bytevector* proc)
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (port get-bytevector)
      (proc port)
      (let ((bv (get-bytevector)))
        (close-port port)
        bv))))

(define (call-with-encoded-output-string charset proc)
  (if (string-ci=? charset "utf-8")
      ;; I don't know why, but this appears to be faster; at least for
      ;; examples/debug-sxml.scm (1464 reqs/s versus 850 reqs/s).
      (string->utf8 (call-with-output-string* proc))
      (call-with-output-bytevector*
       (lambda (port)
         (set-port-encoding! port charset)
         (proc port)))))

(define (encode-string str charset)
  (if (string-ci=? charset "utf-8")
      (string->utf8 str)
      (call-with-encoded-output-string charset
                                       (lambda (port)
                                         (display str port)))))

(define (extend-response r k v . additional)
  (let ((r (build-response #:version (response-version r)
                           #:code (response-code r)
                           #:headers
                           (assoc-set! (copy-tree (response-headers r))
                                       k v)
                           #:port (response-port r))))
    (if (null? additional)
        r
        (apply extend-response r additional))))

;; -> response body
(define (sanitize-response request response body)
  "\"Sanitize\" the given response and body, making them appropriate for
the given request.

As a convenience to web handler authors, @var{response} may be given as
an alist of headers, in which case it is used to construct a default
response.  Ensures that the response version corresponds to the request
version.  If @var{body} is a string, encodes the string to a bytevector,
in an encoding appropriate for @var{response}.  Adds a
@code{content-length} and @code{content-type} header, as necessary.

If @var{body} is a procedure, it is called with a port as an argument,
and the output collected as a bytevector.  In the future we might try to
instead use a compressing, chunk-encoded port, and call this procedure
later, in the write-client procedure.  Authors are advised not to rely
on the procedure being called at any particular time."
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
                                        '(text/plain)))
           (declared-charset (assq-ref (cdr type) 'charset))
           (charset (or declared-charset "utf-8")))
      (sanitize-response
       request
       (if declared-charset
           response
           (extend-response response 'content-type
                            `(,@type (charset . ,charset))))
       (encode-string body charset))))
   ((procedure? body)
    (let* ((type (response-content-type response
                                        '(text/plain)))
           (declared-charset (assq-ref (cdr type) 'charset))
           (charset (or declared-charset "utf-8")))
      (sanitize-response
       request
       (if declared-charset
           response
           (extend-response response 'content-type
                            `(,@type (charset . ,charset))))
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
  "Handle a given request, returning the response and body.

The response and response body are produced by calling the given
@var{handler} with @var{request} and @var{body} as arguments.

The elements of @var{state} are also passed to @var{handler} as
arguments, and may be returned as additional values.  The new
@var{state}, collected from the @var{handler}'s return values, is then
returned as a list.  The idea is that a server loop receives a handler
from the user, along with whatever state values the user is interested
in, allowing the user's handler to explicitly manage its state."
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
  "Write an HTTP response and body to @var{client}.  If the server and
client support persistent connections, it is the implementation's
responsibility to keep track of the client thereafter, presumably by
attaching it to the @var{server} argument somehow."
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
  "Release resources allocated by a previous invocation of
@code{open-server}."
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
  "Read one request from @var{server}, call @var{handler} on the request
and body, and write the response to the client.  Returns the new state
produced by the handler procedure."
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
  "Run Guile's built-in web server.

@var{handler} should be a procedure that takes two or more arguments,
the HTTP request and request body, and returns two or more values, the
response and response body.

For example, here is a simple \"Hello, World!\" server:

@example
 (define (handler request body)
   (values '((content-type . (text/plain)))
           \"Hello, World!\"))
 (run-server handler)
@end example

The response and body will be run through @code{sanitize-response}
before sending back to the client.

Additional arguments to @var{handler} are taken from
@var{state}. Additional return values are accumulated into a new
@var{state}, which will be used for subsequent requests.  In this way a
handler can explicitly manage its state.

The default server implementation is @code{http}, which accepts
@var{open-params} like @code{(#:port 8081)}, among others.  See \"Web
Server\" in the manual, for more information."
  (let* ((impl (lookup-server-impl impl))
         (server (open-server impl open-params)))
    (call-with-sigint
     (lambda ()
       (let lp ((state state))
         (lp (serve-one-client handler impl server state))))
     (lambda ()
       (close-server impl server)
       (values)))))
