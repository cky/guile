;;; Toy web server

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

(define-module (web toy-server)
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:use-module (web response)
  #:export (run-server simple-get-handler))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock family addr port)
    sock))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk) (thunk))
      (lambda (thunk)
        (let ((handler #f))
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
                  (sigaction SIGINT #f))))))))

(define (accept-new-client server-socket)
  (catch #t
    (lambda () (call-with-sigint (lambda () (accept server-socket))))
    (lambda (k . args)
      (cond
       ((port-closed? server-socket)
        ;; Shutting down.
        #f)
       ((eq? k 'interrupt)
        ;; Interrupt.
        (close-port server-socket)
        #f)
       (else
        (warn "Error accepting client" k args)
        ;; Retry after a timeout.
        (sleep 1)
        (accept-new-client server-socket))))))
  
(define* (simple-get-handler handler #:optional (content-type '("text" "plain")))
  (lambda (request request-body)
    (if (eq? (request-method request) 'GET)
        (let* ((x (handler (request-absolute-uri request)))
               (bv (cond ((bytevector? x) x)
                         ((string? x) (string->utf8 x))
                         (else
                          (error "unexpected val from simple get handler" x)))))
          (values (build-response
                   #:headers `((content-type . ,content-type)
                               (content-length . ,(bytevector-length bv))))
                  bv))
        (build-response #:code 405))))

;; This abuses the definition of "toy", because it's really
;; terrible. Not even fit for children. The FIXME is to handle errors
;; while reading the request and writing the response, not only errors
;; in the handler.
;;
(define (serve-client handler sock addr)
  (let* ((req (read-request sock))
         (body-str (read-request-body/latin-1 req)))
    (call-with-values (lambda ()
                        (catch #t
                          (lambda ()
                            (handler req body-str))
                          (lambda (k . args)
                            (if (eq? k 'interrupt)
                                (apply throw k args)
                                (begin
                                  (warn "Error while serving client" k args)
                                  (build-response #:code 500))))))
      (lambda* (response #:optional body)
        (let ((response (write-response response sock)))
          (cond
           ((not body)) ; pass
           ((string? body)
            (write-response-body/latin-1 response body))
           ((bytevector? body)
            (write-response-body/bytevector response body))
           (else
            (error "Expected a string or bytevector for body" body)))))))
  (close-port sock)) ; FIXME: keep socket alive. requires select?

(define* (run-server handler
                     #:key
                     (host #f)
                     (family AF_INET)
                     (addr (if host
                               (inet-pton family host)
                               INADDR_LOOPBACK))
                     (port 8080)
                     (server-socket (make-default-socket family addr port)))
  (listen server-socket 5)
  (let lp ((client (accept-new-client server-socket)))
    ;; If client is false, we are shutting down.
    (if client
        (let ((client-socket (car client))
              (client-addr (cdr client)))
          (catch 'interrupt
            (lambda ()
              (call-with-sigint
               (lambda ()
                 (serve-client handler client-socket client-addr))))
            (lambda (k . args)
              (warn "Interrupt while serving client")
              (close-port client-socket)
              #f))
          (lp (accept-new-client server-socket))))))
