;;;; ports.scm --- R6RS port API

;;;;	Copyright (C) 2009 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Author: Ludovic Courtès <ludo@gnu.org>

;;; Commentary:
;;;
;;; The I/O port API of the R6RS is provided by this module.  In many areas
;;; it complements or refines Guile's own historical port API.  For instance,
;;; it allows for binary I/O with bytevectors.
;;;
;;; Code:

(define-module (rnrs io ports)
  :re-export (eof-object? port? input-port? output-port?)
  :export (eof-object

           ;; input & output ports
           port-transcoder binary-port? transcoded-port
           port-position set-port-position!
           port-has-port-position? port-has-set-port-position!?
           call-with-port

           ;; input ports
           open-bytevector-input-port
           make-custom-binary-input-port

           ;; binary input
           get-u8 lookahead-u8
           get-bytevector-n get-bytevector-n!
           get-bytevector-some get-bytevector-all

           ;; output ports
           open-bytevector-output-port
           make-custom-binary-output-port

           ;; binary output
           put-u8 put-bytevector))

(load-extension "libguile" "scm_init_r6rs_ports")



;;;
;;; Input and output ports.
;;;

(define (port-transcoder port)
  (error "port transcoders are not supported" port))

(define (binary-port? port)
  ;; So far, we don't support transcoders other than the binary transcoder.
  #t)

(define (transcoded-port port)
  (error "port transcoders are not supported" port))

(define (port-position port)
  "Return the offset (an integer) indicating where the next octet will be
read from/written to in @var{port}."

  ;; FIXME: We should raise an `&assertion' error when not supported.
  (seek port 0 SEEK_CUR))

(define (set-port-position! port offset)
  "Set the position where the next octet will be read from/written to
@var{port}."

  ;; FIXME: We should raise an `&assertion' error when not supported.
  (seek port offset SEEK_SET))

(define (port-has-port-position? port)
  "Return @code{#t} is @var{port} supports @code{port-position}."
  (and (false-if-exception (port-position port)) #t))

(define (port-has-set-port-position!? port)
  "Return @code{#t} is @var{port} supports @code{set-port-position!}."
  (and (false-if-exception (set-port-position! port (port-position port)))
       #t))

(define (call-with-port port proc)
  "Call @var{proc}, passing it @var{port} and closing @var{port} upon exit of
@var{proc}.  Return the return values of @var{proc}."
  (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc port))
      (lambda ()
        (close-port port))))

;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; ports.scm ends here
