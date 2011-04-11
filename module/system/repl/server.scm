;;; Repl server

;; Copyright (C)  2003, 2010, 2011 Free Software Foundation, Inc.

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

(define-module (system repl server)
  #:use-module (system repl repl)
  #:use-module (ice-9 threads)
  #:export (make-tcp-server-socket
            make-unix-domain-server-socket
            run-server
            spawn-server
            stop-server-and-clients!))

(define *open-sockets* '())

(define sockets-lock (make-mutex))

(define (close-socket! s)
  (with-mutex sockets-lock
    (set! *open-sockets* (delq! s *open-sockets*)))
  ;; Close-port could block or raise an exception flushing buffered
  ;; output.  Hmm.
  (close-port s))

(define (add-open-socket! s)
  (with-mutex sockets-lock
    (set! *open-sockets* (cons s *open-sockets*))))

(define (stop-server-and-clients!)
  (cond
   ((with-mutex sockets-lock
      (and (pair? *open-sockets*)
           (car *open-sockets*)))
    => (lambda (s)
         (close-socket! s)
         (stop-server-and-clients!)))))

(define* (make-tcp-server-socket #:key
                          (host #f)
                          (addr (if host (inet-aton host) INADDR_LOOPBACK))
                          (port 37146))
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock AF_INET addr port)
    sock))

(define* (make-unix-domain-server-socket #:key (path "/tmp/guile-socket"))
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock AF_UNIX path)
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

(define* (run-server #:optional (server-socket (make-tcp-server-socket)))
  (define (accept-new-client)
    (catch #t
      (lambda () (call-with-sigint (lambda () (accept server-socket))))
      (lambda (k . args)
        (cond
         ((port-closed? server-socket)
          ;; Shutting down.
          #f)
         ((eq? k 'interrupt)
          ;; Interrupt.
          (close-socket! server-socket)
          #f)
         (else
          (warn "Error accepting client" k args)
          ;; Retry after a timeout.
          (sleep 1)
          (accept-new-client))))))
  
  (sigaction SIGPIPE SIG_IGN)
  (add-open-socket! server-socket)
  (listen server-socket 5)
  (let lp ((client (accept-new-client)))
    ;; If client is false, we are shutting down.
    (if client
        (let ((client-socket (car client))
              (client-addr (cdr client)))
          (add-open-socket! client-socket)
          (make-thread serve-client client-socket client-addr)
          (lp (accept-new-client))))))

(define* (spawn-server #:optional (server-socket (make-tcp-server-socket)))
  (make-thread run-server server-socket))

(define (serve-client client addr)
  (with-continuation-barrier
   (lambda ()
     (with-input-from-port client
       (lambda ()
         (with-output-to-port client
           (lambda ()
             (with-error-to-port client
               (lambda ()
                 (with-fluids ((*repl-stack* '()))
                   (start-repl))))))))))
  (close-socket! client))
