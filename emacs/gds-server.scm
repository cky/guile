;;;; Guile Debugger UI server

;;; Copyright (C) 2003 Free Software Foundation, Inc.
;;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(define-module (emacs gds-server)
  #:use-module (emacs gds-client)
  #:export (run-server))

;; UI is normally via a pipe to Emacs, so make sure to flush output
;; every time we write.
(define (write-to-ui form)
  (write form)
  (newline)
  (force-output))

(define (trc . args)
  (write-to-ui (cons '* args)))

(define (with-error->eof proc port)
  (catch #t
	 (lambda () (proc port))
	 (lambda args the-eof-object)))

(define (run-server . ignored-args)

  (let ((server (socket PF_INET SOCK_STREAM 0)))

    ;; Initialize server socket.
    (setsockopt server SOL_SOCKET SO_REUSEADDR 1)
    (bind server AF_INET INADDR_ANY gds-port-number)
    (listen server 5)

    (let loop ((clients '()) (readable-sockets '()))

      (define (do-read port)
	(cond ((eq? port (current-input-port))
	       (do-read-from-ui))
	      ((eq? port server)
	       (accept-new-client))
	      (else
	       (do-read-from-client port))))

      (define (do-read-from-ui)
	(trc "reading from ui")
	(let* ((form (with-error->eof read (current-input-port)))
	       (client (assq-ref (map (lambda (port)
					(cons (fileno port) port))
				      clients)
				 (car form))))
	  (with-error->eof read-char (current-input-port))
	  (if client
	      (begin
		(write (cdr form) client)
		(newline client))
	      (trc "client not found")))	
	clients)

      (define (accept-new-client)
	(cons (car (accept server)) clients))

      (define (do-read-from-client port)
	(trc "reading from client")
	(let ((next-char (with-error->eof peek-char port)))
	  ;;(trc 'next-char next-char)
	  (cond ((eof-object? next-char)
		 (write-to-ui (list (fileno port) 'closed))
		 (close port)
		 (delq port clients))
		((char=? next-char #\()
		 (write-to-ui (cons (fileno port) (with-error->eof read port)))
		 clients)
		(else
		 (with-error->eof read-char port)
		 clients))))

      ;;(trc 'clients clients)
      ;;(trc 'readable-sockets readable-sockets)

      (if (null? readable-sockets)
	  (loop clients (car (select (cons (current-input-port)
					   (cons server clients))
				     '()
				     '())))
	  (loop (do-read (car readable-sockets)) (cdr readable-sockets))))))
