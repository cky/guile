;;; HTTP response objects

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

;;; Code:

(define-module (web response)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:use-module (web http)
  #:export (response?
            response-version
            response-code
            response-reason-phrase
            response-headers
            response-port
            read-response
            build-response
            adapt-response-version
            write-response

            read-response-body
            write-response-body

            ;; General headers
            ;;
            response-cache-control
            response-connection
            response-date
            response-pragma
            response-trailer
            response-transfer-encoding
            response-upgrade
            response-via
            response-warning

            ;; Entity headers
            ;;
            response-allow
            response-content-encoding
            response-content-language
            response-content-length
            response-content-location
            response-content-md5
            response-content-range
            response-content-type
            response-expires
            response-last-modified

            ;; Response headers
            ;;
            response-accept-ranges
            response-age
            response-etag
            response-location
            response-proxy-authenticate
            response-retry-after
            response-server
            response-vary
            response-www-authenticate))


(define-record-type <response>
  (make-response version code reason-phrase headers port)
  response?
  (version response-version)
  (code response-code)
  (reason-phrase %response-reason-phrase)
  (headers response-headers)
  (port response-port))

(define (bad-response message . args)
  (throw 'bad-response message args))

(define (non-negative-integer? n)
  (and (number? n) (>= n 0) (exact? n) (integer? n)))
                                    
(define (validate-headers headers)
  (if (pair? headers)
      (let ((h (car headers)))
        (if (pair? h)
            (let ((k (car h)) (v (cdr h)))
              (if (valid-header? k v)
                  (validate-headers (cdr headers))
                  (bad-response "Bad value for header ~a: ~s" k v)))
            (bad-response "Header not a pair: ~a" h)))
      (if (not (null? headers))
          (bad-response "Headers not a list: ~a" headers))))

(define* (build-response #:key (version '(1 . 1)) (code 200) reason-phrase
                         (headers '()) port (validate-headers? #t))
  "Construct an HTTP response object. If @var{validate-headers?} is true,
the headers are each run through their respective validators."
  (cond
   ((not (and (pair? version)
              (non-negative-integer? (car version))
              (non-negative-integer? (cdr version))))
    (bad-response "Bad version: ~a" version))
   ((not (and (non-negative-integer? code) (< code 600)))
    (bad-response "Bad code: ~a" code))
   ((and reason-phrase (not (string? reason-phrase)))
    (bad-response "Bad reason phrase" reason-phrase))
   (else
    (if validate-headers?
        (validate-headers headers))))
  (make-response version code reason-phrase headers port))

(define *reason-phrases*
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (203 . "Non-Authoritative Information")
    (204 . "No Content")
    (205 . "Reset Content")
    (206 . "Partial Content")
    (300 . "Multiple Choices")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (305 . "Use Proxy")
    (307 . "Temporary Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment Required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (407 . "Proxy Authentication Required")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (410 . "Gone")
    (411 . "Length Required")
    (412 . "Precondition Failed")
    (413 . "Request Entity Too Large")
    (414 . "Request-URI Too Long")
    (415 . "Unsupported Media Type")
    (416 . "Requested Range Not Satisfiable")
    (417 . "Expectation Failed")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout")
    (505 . "HTTP Version Not Supported")))

(define (code->reason-phrase code)
  (or (assv-ref *reason-phrases* code)
      "(Unknown)"))

(define (response-reason-phrase response)
  "Return the reason phrase given in @var{response}, or the standard
reason phrase for the response's code."
  (or (%response-reason-phrase response)
      (code->reason-phrase (response-code response))))

(define (read-response port)
  "Read an HTTP response from @var{port}.

As a side effect, sets the encoding on @var{port} to
ISO-8859-1 (latin-1), so that reading one character reads one byte.  See
the discussion of character sets in \"HTTP Responses\" in the manual,
for more information."
  (set-port-encoding! port "ISO-8859-1")
  (call-with-values (lambda () (read-response-line port))
    (lambda (version code reason-phrase)
      (make-response version code reason-phrase (read-headers port) port))))

(define (adapt-response-version response version)
  "Adapt the given response to a different HTTP version.  Returns a new
HTTP response.

The idea is that many applications might just build a response for the
default HTTP version, and this method could handle a number of
programmatic transformations to respond to older HTTP versions (0.9 and
1.0).  But currently this function is a bit heavy-handed, just updating
the version field."
  (build-response #:code (response-code response)
                  #:version version
                  #:headers (response-headers response)
                  #:port (response-port response)))

(define (write-response r port)
  "Write the given HTTP response to @var{port}.

Returns a new response, whose @code{response-port} will continue writing
on @var{port}, perhaps using some transfer encoding."
  (write-response-line (response-version r) (response-code r)
                       (response-reason-phrase r) port)
  (write-headers (response-headers r) port)
  (display "\r\n" port)
  (if (eq? port (response-port r))
      r
      (make-response (response-version r) (response-code r)
                     (response-reason-phrase r) (response-headers r) port)))

(define (read-response-body r)
  "Reads the response body from @var{r}, as a bytevector.  Returns
@code{#f} if there was no response body."
  (let ((nbytes (response-content-length r)))
    (and nbytes
         (let ((bv (get-bytevector-n (response-port r) nbytes)))
           (if (= (bytevector-length bv) nbytes)
               bv
               (bad-response "EOF while reading response body: ~a bytes of ~a"
                            (bytevector-length bv) nbytes))))))

(define (write-response-body r bv)
  "Write @var{body}, a bytevector, to the port corresponding to the HTTP
response @var{r}."
  (put-bytevector (response-port r) bv))

(define-syntax define-response-accessor
  (lambda (x)
    (syntax-case x ()
      ((_ field)
       #'(define-response-accessor field #f))
      ((_ field def) (identifier? #'field)
       #`(define* (#,(datum->syntax
                      #'field
                      (symbol-append 'response- (syntax->datum #'field)))
                   response
                   #:optional (default def))
           (cond
            ((assq 'field (response-headers response)) => cdr)
            (else default)))))))

;; General headers
;;
(define-response-accessor cache-control '())
(define-response-accessor connection '())
(define-response-accessor date #f)
(define-response-accessor pragma '())
(define-response-accessor trailer '())
(define-response-accessor transfer-encoding '())
(define-response-accessor upgrade '())
(define-response-accessor via '())
(define-response-accessor warning '())

;; Entity headers
;;
(define-response-accessor allow '())
(define-response-accessor content-encoding '())
(define-response-accessor content-language '())
(define-response-accessor content-length #f)
(define-response-accessor content-location #f)
(define-response-accessor content-md5 #f)
(define-response-accessor content-range #f)
(define-response-accessor content-type #f)
(define-response-accessor expires #f)
(define-response-accessor last-modified #f)

;; Response headers
;;
(define-response-accessor accept-ranges #f)
(define-response-accessor age #f)
(define-response-accessor etag #f)
(define-response-accessor location #f)
(define-response-accessor proxy-authenticate #f)
(define-response-accessor retry-after #f)
(define-response-accessor server #f)
(define-response-accessor vary '())
(define-response-accessor www-authenticate #f)
