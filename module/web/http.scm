;;; HTTP messages

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

(define-module (web http)
  #:use-module ((srfi srfi-1) #:select (append-map! map!))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (web uri)
  #:export (header-decl?
            make-header-decl
            header-decl-sym
            header-decl-name
            header-decl-multiple?
            header-decl-parser
            header-decl-validator
            header-decl-writer
            lookup-header-decl
            declare-header!

            read-header
            parse-header
            valid-header?
            write-header

            read-headers
            write-headers

            parse-http-method
            parse-http-version
            parse-request-uri

            read-request-line
            write-request-line
            read-response-line
            write-response-line))


;;; TODO
;;;
;;; Look at quality lists with more insight.
;;; Think about `accept' a bit more.
;;; 


(define-record-type <header-decl>
  (make-header-decl sym name multiple? parser validator writer)
  header-decl?
  (sym header-decl-sym)
  (name header-decl-name)
  (multiple? header-decl-multiple?)
  (parser header-decl-parser)
  (validator header-decl-validator)
  (writer header-decl-writer))

;; sym -> header
(define *declared-headers* (make-hash-table))
;; downcased name -> header
(define *declared-headers-by-name* (make-hash-table))

(define* (declare-header! sym name #:key 
                          multiple?
                          parser
                          validator
                          writer)
  (if (and (symbol? sym) (string? name) parser validator writer)
      (let ((decl (make-header-decl sym name
                                    multiple? parser validator writer)))
        (hashq-set! *declared-headers* sym decl)
        (hash-set! *declared-headers-by-name* (string-downcase name) decl)
        decl)
      (error "bad header decl" sym name multiple? parser validator writer)))

(define (read-line* port)
  (let* ((pair (%read-line port))
         (line (car pair))
         (delim (cdr pair)))
    (if (and (string? line) (char? delim))
        (let ((orig-len (string-length line)))
          (let lp ((len orig-len))
            (if (and (> len 0)
                     (char-whitespace? (string-ref line (1- len))))
                (lp (1- len))
                (if (= len orig-len)
                    line
                    (substring line 0 len)))))
        (bad-header '%read line))))

(define (read-continuation-line port val)
  (if (or (eqv? (peek-char port) #\space)
          (eqv? (peek-char port) #\tab))
      (read-continuation-line port
                              (string-append val
                                             (begin
                                               (read-line* port))))
      val))

(define (read-header port)
  (let ((line (read-line* port)))
    (if (or (string-null? line)
            (string=? line "\r"))
        (values #f #f)
        (let ((delim (or (string-index line #\:)
                         (bad-header '%read line))))
          (parse-header
           (substring line 0 delim)
           (read-continuation-line
            port
            (string-trim-both line char-whitespace? (1+ delim))))))))

(define (lookup-header-decl name)
  (if (string? name)
      (hash-ref *declared-headers-by-name* (string-downcase name))
      (hashq-ref *declared-headers* name)))

(define (parse-header name val)
  (let* ((down (string-downcase name))
         (decl (hash-ref *declared-headers-by-name* down)))
    (if decl
        (values (header-decl-sym decl)
                ((header-decl-parser decl) val))
        (values down val))))

(define (valid-header? sym val)
  (let ((decl (hashq-ref *declared-headers* sym)))
    (if (not decl)
        (error "Unknown header" sym)
        ((header-decl-validator decl) val))))

(define (write-header name val port)
  (if (string? name)
      ;; assume that it's a header we don't know about...
      (begin
        (display name port)
        (display ": " port)
        (display val port)
        (display "\r\n" port))
      (let ((decl (hashq-ref *declared-headers* name)))
        (if (not decl)
            (error "Unknown header" name)
            (begin
              (display (header-decl-name decl) port)
              (display ": " port)
              ((header-decl-writer decl) val port)
              (display "\r\n" port))))))

(define (read-headers port)
  (let lp ((headers '()))
    (call-with-values (lambda () (read-header port))
      (lambda (k v)
        (if k
            (lp (acons k v headers))
            (reverse! headers))))))

;; Doesn't write the final \r\n, as the user might want to add another
;; header.
(define (write-headers headers port)
  (let lp ((headers headers))
    (if (pair? headers)
        (begin
          (write-header (caar headers) (cdar headers) port)
          (lp (cdr headers))))))




;;;
;;; Utilities
;;;

(define (bad-header sym val)
  (throw 'bad-header sym val))
(define (bad-header-component sym val)
  (throw 'bad-header sym val))

(define (parse-opaque-string str)
  str)
(define (validate-opaque-string val)
  (string? val))
(define (write-opaque-string val port)
  (display val port))

(define not-separator
  "[^][()<>@,;:\\\"/?= \t]")
(define media-type-re
  (make-regexp (format #f "^(~a+)/(~a+)$" not-separator not-separator)))
(define (parse-media-type str)
  (let ((m (regexp-exec media-type-re str)))
    (if m
        (values (match:substring m 1) (match:substring m 2))
        (bad-header-component 'media-type str))))

(define* (skip-whitespace str #:optional (start 0) (end (string-length str)))
  (let lp ((i start))
    (if (and (< i end) (char-whitespace? (string-ref str i)))
        (lp (1+ i))
        i)))

(define* (trim-whitespace str #:optional (start 0) (end (string-length str)))
  (let lp ((i end))
    (if (and (< start i) (char-whitespace? (string-ref str (1- i))))
        (lp (1- i))
        i)))

(define* (split-and-trim str #:optional (delim #\,)
                         (start 0) (end (string-length str)))
  (let lp ((i start))
    (if (< i end)
        (let* ((idx (string-index str delim i end))
               (tok (string-trim-both str char-whitespace? i (or idx end))))
          (cons tok (split-and-trim str delim (if idx (1+ idx) end) end)))
        '())))

(define (collect-escaped-string from start len escapes)
  (let ((to (make-string len)))
    (let lp ((start start) (i 0) (escapes escapes))
      (if (null? escapes)
          (begin
            (substring-move! from start (+ start (- len i)) to i)
            to)
          (let* ((e (car escapes))
                 (next-start (+ start (- e i) 2)))
            (substring-move! from start (- next-start 2) to i)
            (string-set! to e (string-ref from (- next-start 1)))
            (lp next-start (1+ e) (cdr escapes)))))))

;; in incremental mode, returns two values: the string, and the index at
;; which the string ended
(define* (parse-qstring str #:optional
                        (start 0) (end (trim-whitespace str start))
                        #:key incremental?)
  (if (and (< start end) (eqv? (string-ref str start) #\"))
      (let lp ((i (1+ start)) (qi 0) (escapes '()))
        (if (< i end)
            (case (string-ref str i)
              ((#\\)
               (lp (+ i 2) (1+ qi) (cons qi escapes)))
              ((#\")
               (let ((out (collect-escaped-string str (1+ start) qi escapes)))
                 (if incremental?
                     (values out (1+ i))
                     (if (= (1+ i) end)
                         out
                         (bad-header-component 'qstring str)))))
              (else
               (lp (1+ i) (1+ qi) escapes)))
            (bad-header-component 'qstring str)))
      (bad-header-component 'qstring str)))

(define (write-list l port write-item delim)
  (if (pair? l)
      (let lp ((l l))
        (write-item (car l) port)
        (if (pair? (cdr l))
            (begin
              (display delim port)
              (lp (cdr l)))))))

(define (write-qstring str port)
  (display #\" port)
  (if (string-index str #\")
      ;; optimize me
      (write-list (string-split str #\") port display "\\\"")
      (display str port))
  (display #\" port))

(define* (parse-quality str #:optional (start 0) (end (string-length str)))
  (define (char->decimal c)
    (let ((i (- (char->integer c) (char->integer #\0))))
      (if (and (<= 0 i) (< i 10))
          i
          (bad-header-component 'quality str))))
  (cond
   ((not (< start end))
    (bad-header-component 'quality str))
   ((eqv? (string-ref str start) #\1)
    (if (or (string= str "1" start end)
            (string= str "1." start end)
            (string= str "1.0" start end)
            (string= str "1.00" start end)
            (string= str "1.000" start end))
        1000
        (bad-header-component 'quality str)))
   ((eqv? (string-ref str start) #\0)
    (if (or (string= str "0" start end)
            (string= str "0." start end))
        0
        (if (< 2 (- end start) 6)
            (let lp ((place 1) (i (+ start 4)) (q 0))
              (if (= i (1+ start))
                  (if (eqv? (string-ref str (1+ start)) #\.)
                      q
                      (bad-header-component 'quality str))
                  (lp (* 10 place) (1- i)
                      (if (< i end)
                          (+ q (* place (char->decimal (string-ref str i))))
                          q))))
            (bad-header-component 'quality str))))
   (else
    (bad-header-component 'quality str))))

(define (valid-quality? q)
  (and (non-negative-integer? q) (<= 1000 q)))

(define (write-quality q port)
  (define (digit->char d)
    (integer->char (+ (char->integer #\0) d)))
  (display (digit->char (modulo (quotient q 1000) 10)) port)
  (display #\. port)
  (display (digit->char (modulo (quotient q 100) 10)) port)
  (display (digit->char (modulo (quotient q 10) 10)) port)
  (display (digit->char (modulo q 10)) port))

(define (list-of? val pred)
  (or (null? val)
      (and (pair? val)
           (pred (car val))
           (list-of? (cdr val) pred))))

(define* (parse-quality-list str)
  (map (lambda (part)
         (cond
          ((string-rindex part #\;)
           => (lambda (idx)
                (let ((qpart (string-trim-both part char-whitespace? (1+ idx))))
                  (if (string-prefix? "q=" qpart)
                      (cons (parse-quality qpart 2)
                            (string-trim-both part char-whitespace? 0 idx))
                      (bad-header-component 'quality qpart)))))
          (else
           (cons 1000 (string-trim-both part char-whitespace?)))))
       (string-split str #\,)))

(define (validate-quality-list l)
  (list-of? l
            (lambda (elt)
              (and (pair? elt)
                   (valid-quality? (car elt))
                   (string? (cdr elt))))))

(define (write-quality-list l port)
  (write-list l port
              (lambda (x port)
                (let ((q (car x))
                      (str (cdr x)))
                  (display str port)
                  (if (< q 1000)
                      (begin
                        (display ";q=" port)
                        (write-quality q port)))))
              ","))

(define* (parse-non-negative-integer val #:optional (start 0)
                                     (end (string-length val)))
  (define (char->decimal c)
    (let ((i (- (char->integer c) (char->integer #\0))))
      (if (and (<= 0 i) (< i 10))
          i
          (bad-header-component 'non-negative-integer val))))
  (if (not (< start end))
      (bad-header-component 'non-negative-integer val)
      (let lp ((i start) (out 0))
        (if (< i end)
            (lp (1+ i)
                (+ (* out 10) (char->decimal (string-ref val i))))
            out))))

(define (non-negative-integer? code)
  (and (number? code) (>= code 0) (exact? code) (integer? code)))
                                    
(define (default-kons k val)
  (if val
      (cons k val)
      k))

(define (default-kv-validator k val)
  #t)

(define (default-val-writer k val port)
  (if (or (string-index val #\;)
          (string-index val #\,)
          (string-index val #\"))
      (write-qstring val port)
      (display val port)))

(define* (parse-key-value-list str #:optional (kproc identity)
                               (kons default-kons)
                               (start 0) (end (string-length str)))
  (let lp ((i start) (out '()))
    (if (not (< i end))
        (reverse! out)
        (let* ((i (skip-whitespace str i end))
               (eq (string-index str #\= i end))
               (comma (string-index str #\, i end))
               (delim (min (or eq end) (or comma end)))
               (k (kproc (substring str i (trim-whitespace str i delim)))))
          (call-with-values
              (lambda ()
                (if (and eq (or (not comma) (< eq comma)))
                    (let ((i (skip-whitespace str (1+ eq) end)))
                      (if (and (< i end) (eqv? (string-ref str i) #\"))
                          (parse-qstring str i end #:incremental? #t)
                          (values (substring str i
                                             (trim-whitespace str i
                                                              (or comma end)))
                                  (or comma end))))
                    (values #f delim)))
            (lambda (v-str next-i)
              (let ((i (skip-whitespace str next-i end)))
                (if (or (= i end) (eqv? (string-ref str i) #\,))
                    (lp (1+ i) (cons (kons k v-str) out))
                    (bad-header-component 'key-value-list
                                          (substring str start end))))))))))

(define* (key-value-list? list #:optional
                          (valid? default-kv-validator))
  (list-of? list
            (lambda (elt)
              (cond
               ((pair? elt)
                (let ((k (car elt))
                      (v (cdr elt)))
                  (and (or (string? k) (symbol? k))
                       (valid? k v))))
               ((or (string? elt) (symbol? elt))
                (valid? elt #f))
               (else #f)))))

(define* (write-key-value-list list port #:optional
                               (val-writer default-val-writer) (delim ", "))
  (write-list
   list port
   (lambda (x port)
     (let ((k (if (pair? x) (car x) x))
           (v (if (pair? x) (cdr x) #f)))
       (display k port)
       (if v
           (begin
             (display #\= port)
             (val-writer k v port)))))
   delim))

;; param-component = token [ "=" (token | quoted-string) ] \
;;    *(";" token [ "=" (token | quoted-string) ])
;;
(define* (parse-param-component str #:optional (kproc identity)
                                (kons default-kons)
                                (start 0) (end (string-length str)))
  (let lp ((i start) (out '()))
    (if (not (< i end))
        (values (reverse! out) end)
        (let ((delim (string-index str
                                   (lambda (c) (memq c '(#\, #\; #\=)))
                                   i)))
          (let ((k (kproc
                    (substring str i (trim-whitespace str i (or delim end)))))
                (delimc (and delim (string-ref str delim))))
            (case delimc
              ((#\=)
               (call-with-values
                   (lambda ()
                     (let ((i (skip-whitespace str (1+ delim) end)))
                       (if (and (< i end) (eqv? (string-ref str i) #\"))
                           (parse-qstring str i end #:incremental? #t)
                           (let ((delim
                                  (or (string-index
                                       str
                                       (lambda (c)
                                         (or (eqv? c #\;)
                                             (eqv? c #\,)
                                             (char-whitespace? c)))
                                       i end)
                                      end)))
                             (values (substring str i delim)
                                     delim)))))
                 (lambda (v-str next-i)
                   (let ((x (kons k v-str))
                         (i (skip-whitespace str next-i end)))
                     (case (and (< i end) (string-ref str i))
                       ((#f)
                        (values (reverse! (cons x out)) end))
                       ((#\;)
                        (lp (skip-whitespace str (1+ i) end)
                            (cons x out)))
                       (else            ; including #\,
                        (values (reverse! (cons x out)) i)))))))
              ((#\;)
               (lp (skip-whitespace str (1+ delim) end)
                   (cons (kons k #f) out)))
             
              (else ;; either the end of the string or a #\,
               (values (reverse! (cons (kons k #f) out))
                       (or delim end)))))))))

(define* (parse-param-list str #:optional
                           (kproc identity) (kons default-kons)
                           (start 0) (end (string-length str)))
  (let lp ((i start) (out '()))
    (call-with-values
        (lambda () (parse-param-component str kproc kons i end))
      (lambda (item i)
        (if (< i end)
            (if (eqv? (string-ref str i) #\,)
                (lp (skip-whitespace str (1+ i) end)
                    (cons item out))
                (bad-header-component 'param-list str))
            (reverse! (cons item out)))))))

(define* (validate-param-list list #:optional
                              (valid? default-kv-validator))
  (list-of? list
            (lambda (elt)
              (key-value-list? list valid?))))

(define* (write-param-list list port #:optional
                           (val-writer default-val-writer))
  (write-list
   list port
   (lambda (item port)
     (write-key-value-list item port val-writer ";"))
   ","))

(define (list-of-strings? val)
  (list-of? val string?))

(define (write-list-of-strings val port)
  (write-list val port display ", "))

(define (parse-date str)
  ;; Unfortunately, there is no way to make string->date parse out the
  ;; "GMT" bit, so we play string games to append a format it will
  ;; understand (the +0000 bit).
  (string->date
   (if (string-suffix? " GMT" str)
       (string-append (substring str 0 (- (string-length str) 4))
                      " +0000")
       (bad-header-component 'date str))
   "~a, ~d ~b ~Y ~H:~M:~S ~z"))

(define (write-date date port)
  (display (date->string date "~a, ~d ~b ~Y ~H:~M:~S GMT") port))

(define (write-uri uri port)
  (display (unparse-uri uri) port))

(define (parse-entity-tag val)
  (if (string-prefix? "W/" val)
      (cons (parse-qstring val 2) #f)
      (cons (parse-qstring val) #t)))

(define (entity-tag? val)
  (and (pair? val)
       (string? (car val))))

(define (write-entity-tag val port)
  (if (cdr val)
      (display "W/" port))
  (write-qstring (car val) port))

(define* (parse-entity-tag-list val #:optional
                                (start 0) (end (string-length val)))
  (let ((strong? (not (string-prefix? "W/" val 0 2 start end))))
    (call-with-values (lambda ()
                        (parse-qstring val (if strong? start (+ start 2))
                                       end #:incremental? #t))
      (lambda (tag next)
        (acons tag strong?
               (let ((next (skip-whitespace val next end)))
                  (if (< next end)
                      (if (eqv? (string-ref val next) #\,)
                          (parse-entity-tag-list
                           val
                           (skip-whitespace val (1+ next) end)
                           end)
                          (bad-header-component 'entity-tag-list val))
                      '())))))))

(define (entity-tag-list? val)
  (list-of? val entity-tag?))

(define (write-entity-tag-list val port)
  (write-list val port write-entity-tag  ", "))




;;;
;;; Request-Line and Response-Line
;;;

;; Hmm.
(define (bad-request message . args)
  (throw 'bad-request message args))
(define (bad-response message . args)
  (throw 'bad-response message args))

(define *known-versions* '())

(define* (parse-http-version str #:optional (start 0) (end (string-length str)))
  (or (let lp ((known *known-versions*))
        (and (pair? known)
             (if (string= str (caar known) start end)
                 (cdar known)
                 (lp (cdr known)))))
      (let ((dot-idx (string-index str #\. start end)))
        (if (and (string-prefix? "HTTP/" str 0 5 start end)
                 dot-idx
                 (= dot-idx (string-rindex str #\. start end)))
            (cons (parse-non-negative-integer str (+ start 5) dot-idx)
                  (parse-non-negative-integer str (1+ dot-idx) end))
            (bad-header-component 'http-version (substring str start end))))))

(define (write-http-version val port)
  (display "HTTP/" port)
  (display (car val) port)
  (display #\. port)
  (display (cdr val) port))

(for-each
 (lambda (v)
   (set! *known-versions*
         (acons v (parse-http-version v 0 (string-length v))
                *known-versions*)))
 '("HTTP/1.0" "HTTP/1.1"))


;; Request-URI = "*" | absoluteURI | abs_path | authority
;;
;; The `authority' form is only permissible for the CONNECT method, so
;; because we don't expect people to implement CONNECT, we save
;; ourselves the trouble of that case, and disallow the CONNECT method.
;;
(define* (parse-http-method str #:optional (start 0) (end (string-length str)))
  (cond
   ((string= str "GET" start end) 'GET)
   ((string= str "HEAD" start end) 'HEAD)
   ((string= str "POST" start end) 'POST)
   ((string= str "PUT" start end) 'PUT)
   ((string= str "DELETE" start end) 'DELETE)
   ((string= str "OPTIONS" start end) 'OPTIONS)
   ((string= str "TRACE" start end) 'TRACE)
   (else (bad-request "Invalid method: ~a" (substring str start end)))))

(define* (parse-request-uri str #:optional (start 0) (end (string-length str)))
  (cond
   ((= start end)
    (bad-request "Missing Request-URI"))
   ((string= str "*" start end)
    #f)
   ((eq? (string-ref str start) #\/)
    (let* ((q (string-index str #\? start end))
           (f (string-index str #\# start end))
           (q (and q (or (not f) (< q f)) q)))
      (build-uri 'http
                 #:path (substring str start (or q f end))
                 #:query (and q (substring str (1+ q) (or f end)))
                 #:fragment (and f (substring str (1+ f) end)))))
   (else
    (or (parse-uri (substring str start end))
        (bad-request "Invalid URI: ~a" (substring str start end))))))

(define (read-request-line port)
  (let* ((line (read-line* port))
         (d0 (string-index line char-whitespace?)) ; "delimiter zero"
         (d1 (string-rindex line char-whitespace?)))
    (if (and d0 d1 (< d0 d1))
        (values (parse-http-method line 0 d0)
                (parse-request-uri line (skip-whitespace line (1+ d0) d1) d1)
                (parse-http-version line (1+ d1) (string-length line)))
        (bad-request "Bad Request-Line: ~s" line))))

(define (write-uri uri port)
  (if (uri-host uri)
      (begin
        (display (uri-scheme uri) port)
        (display "://" port)
        (if (uri-userinfo uri)
            (begin
              (display (uri-userinfo uri) port)
              (display #\@ port)))
        (display (uri-host uri) port)
        (let ((p (uri-port uri)))
          (if (and p (not (eqv? p 80)))
              (begin
                (display #\: port)
                (display p port))))))
  (let* ((path (uri-path uri))
         (len (string-length path)))
    (cond
     ((and (> len 0) (not (eqv? (string-ref path 0) #\/)))
      (bad-request "Non-absolute URI path: ~s" path))
     ((and (zero? len) (not (uri-host uri)))
      (bad-request "Empty path and no host for URI: ~s" uri))
     (else
      (display path port))))
  (if (uri-query uri)
      (begin
        (display #\? port)
        (display (uri-query uri) port))))

(define (write-request-line method uri version port)
  (display method port)
  (display #\space port)
  (write-uri uri port)
  (display #\space port)
  (write-http-version version port)
  (display "\r\n" port))

(define (read-response-line port)
  (let* ((line (read-line* port))
         (d0 (string-index line char-whitespace?)) ; "delimiter zero"
         (d1 (and d0 (string-index line char-whitespace?
                                   (skip-whitespace line d0)))))
    (if (and d0 d1)
        (values (parse-http-version line 0 d0)
                (parse-non-negative-integer line (skip-whitespace line d0 d1)
                                            d1)
                (string-trim-both line char-whitespace? d1))
        (bad-response "Bad Response-Line: ~s" line))))

(define (write-response-line version code reason-phrase port)
  (write-http-version version port)
  (display #\space port)
  (display code port)
  (display #\space port)
  (display reason-phrase port)
  (display "\r\n" port))




;;;
;;; Syntax for declaring headers
;;;

;; emacs: (put 'declare-header 'scheme-indent-function 1)
(define-syntax declare-header
  (syntax-rules ()
    ((_ sym name parser validator writer arg ...)
     (declare-header!
      'sym name
      #:parser parser #:validator validator #:writer writer
      arg ...))))

;; emacs: (put 'declare-opaque-header 'scheme-indent-function 1)
(define-syntax declare-opaque-header
  (syntax-rules ()
    ((_ sym name)
     (declare-header sym
       name
       parse-opaque-string validate-opaque-string write-opaque-string))))

;; emacs: (put 'declare-date-header 'scheme-indent-function 1)
(define-syntax declare-date-header
  (syntax-rules ()
    ((_ sym name)
     (declare-header sym
       name
       parse-date date? write-date))))

;; emacs: (put 'declare-string-list-header 'scheme-indent-function 1)
(define-syntax declare-string-list-header
  (syntax-rules ()
    ((_ sym name)
     (declare-header sym
       name
       split-and-trim list-of-strings? write-list-of-strings))))

;; emacs: (put 'declare-integer-header 'scheme-indent-function 1)
(define-syntax declare-integer-header
  (syntax-rules ()
    ((_ sym name)
     (declare-header sym
       name
       parse-non-negative-integer non-negative-integer? display))))

;; emacs: (put 'declare-uri-header 'scheme-indent-function 1)
(define-syntax declare-uri-header
  (syntax-rules ()
    ((_ sym name)
     (declare-header sym
       name
       (lambda (str) (or (parse-uri str) (bad-header-component 'uri str)))
       uri?
       write-uri))))

;; emacs: (put 'declare-quality-list-header 'scheme-indent-function 1)
(define-syntax declare-quality-list-header
  (syntax-rules ()
    ((_ sym name)
     (declare-header sym
       name
       parse-quality-list validate-quality-list write-quality-list))))

;; emacs: (put 'declare-param-list-header 'scheme-indent-function 1)
(define-syntax declare-param-list-header
  (syntax-rules ()
    ((_ sym name)
     (declare-param-list-header sym name identity default-kons
                                default-kv-validator default-val-writer))
    ((_ sym name kproc)
     (declare-param-list-header sym name kproc default-kons
                                default-kv-validator default-val-writer))
    ((_ sym name kproc kons val-validator val-writer)
     (declare-header sym
       name
       (lambda (str) (parse-param-list str kproc kons))
       (lambda (val) (validate-param-list val val-validator))
       (lambda (val port) (write-param-list val port val-writer))))))

;; emacs: (put 'declare-key-value-list-header 'scheme-indent-function 1)
(define-syntax declare-key-value-list-header
  (syntax-rules ()
    ((_ sym name)
     (declare-key-value-list-header sym name identity default-kons
                                    default-kv-validator default-val-writer))
    ((_ sym name kproc)
     (declare-key-value-list-header sym name kproc default-kons
                                    default-kv-validator default-val-writer))
    ((_ sym name kproc kons val-validator val-writer)
     (declare-header sym
       name
       (lambda (str) (parse-key-value-list str kproc kons))
       (lambda (val) (key-value-list? val val-validator))
       (lambda (val port) (write-key-value-list val port val-writer))))))

;; emacs: (put 'declare-entity-tag-list-header 'scheme-indent-function 1)
(define-syntax declare-entity-tag-list-header
  (syntax-rules ()
    ((_ sym name)
     (declare-header sym
       name
       (lambda (str) (if (string=? str "*") '* (parse-entity-tag-list str)))
       (lambda (val) (or (eq? val '*) (entity-tag-list? val)))
       (lambda (val port)
         (if (eq? val '*)
             (display "*" port)
             (write-entity-tag-list val port)))))))




;;;
;;; General headers
;;;

;; Cache-Control   = 1#(cache-directive)
;; cache-directive = cache-request-directive | cache-response-directive
;; cache-request-directive =
;;        "no-cache"                          ; Section 14.9.1
;;      | "no-store"                          ; Section 14.9.2
;;      | "max-age" "=" delta-seconds         ; Section 14.9.3, 14.9.4
;;      | "max-stale" [ "=" delta-seconds ]   ; Section 14.9.3
;;      | "min-fresh" "=" delta-seconds       ; Section 14.9.3
;;      | "no-transform"                      ; Section 14.9.5
;;      | "only-if-cached"                    ; Section 14.9.4
;;      | cache-extension                     ; Section 14.9.6
;;  cache-response-directive =
;;        "public"                               ; Section 14.9.1
;;      | "private" [ "=" <"> 1#field-name <"> ] ; Section 14.9.1
;;      | "no-cache" [ "=" <"> 1#field-name <"> ]; Section 14.9.1
;;      | "no-store"                             ; Section 14.9.2
;;      | "no-transform"                         ; Section 14.9.5
;;      | "must-revalidate"                      ; Section 14.9.4
;;      | "proxy-revalidate"                     ; Section 14.9.4
;;      | "max-age" "=" delta-seconds            ; Section 14.9.3
;;      | "s-maxage" "=" delta-seconds           ; Section 14.9.3
;;      | cache-extension                        ; Section 14.9.6
;; cache-extension = token [ "=" ( token | quoted-string ) ]
;;
(declare-key-value-list-header cache-control
  "Cache-Control"
  (let ((known-directives (make-hash-table)))
    (for-each (lambda (s) 
                (hash-set! known-directives s (string->symbol s)))
              '("no-cache" "no-store" "max-age" "max-stale" "min-fresh"
                "no-transform" "only-if-cached" "public" "private"
                "must-revalidate" "proxy-revalidate" "s-maxage"))
    (lambda (k-str)
      (hash-ref known-directives k-str k-str)))
  (lambda (k v-str)
    (case k
      ((max-age max-stale min-fresh s-maxage)
       (cons k (parse-non-negative-integer v-str)))
      ((private no-cache)
       (cons k (if v-str (split-and-trim v-str) #t)))
      (else (if v-str (cons k v-str) k))))
  default-kv-validator
  (lambda (k v port)
    (cond
     ((string? v) (display v port))
     ((pair? v)
      (write-qstring (string-join v ", ") port))
     ((integer? v)
      (display v port))
     (else
      (bad-header-component 'cache-control v)))))

;; Connection = "Connection" ":" 1#(connection-token)
;; connection-token  = token
;; e.g.
;;     Connection: close, foo-header
;; 
(declare-string-list-header connection
  "Connection")

;; Date  = "Date" ":" HTTP-date
;; e.g.
;;     Date: Tue, 15 Nov 1994 08:12:31 GMT
;;
(declare-date-header date
  "Date")

;; Pragma            = "Pragma" ":" 1#pragma-directive
;; pragma-directive  = "no-cache" | extension-pragma
;; extension-pragma  = token [ "=" ( token | quoted-string ) ]
;;
(declare-key-value-list-header pragma
  "Pragma"
  (lambda (k) (if (equal? k "no-cache") 'no-cache k)))

;; Trailer  = "Trailer" ":" 1#field-name
;;
(declare-string-list-header trailer
  "Trailer")

;; Transfer-Encoding = "Transfer-Encoding" ":" 1#transfer-coding
;;
(declare-param-list-header transfer-encoding
  "Transfer-Encoding"
  (lambda (k)
    (if (equal? k "chunked") 'chunked k)))

;; Upgrade = "Upgrade" ":" 1#product
;;
(declare-string-list-header upgrade
  "Upgrade")

;; Via =  "Via" ":" 1#( received-protocol received-by [ comment ] )
;; received-protocol = [ protocol-name "/" ] protocol-version
;; protocol-name     = token
;; protocol-version  = token
;; received-by       = ( host [ ":" port ] ) | pseudonym
;; pseudonym         = token
;;
(declare-header via
  "Via"
  split-and-trim
  list-of-strings?
  write-list-of-strings
  #:multiple? #t)

;; Warning    = "Warning" ":" 1#warning-value
;;
;; warning-value = warn-code SP warn-agent SP warn-text
;;                                       [SP warn-date]
;;
;; warn-code  = 3DIGIT
;; warn-agent = ( host [ ":" port ] ) | pseudonym
;;                 ; the name or pseudonym of the server adding
;;                 ; the Warning header, for use in debugging
;; warn-text  = quoted-string
;; warn-date  = <"> HTTP-date <">
(declare-header warning
  "Warning"
  (lambda (str)
    (let ((len (string-length str)))
      (let lp ((i (skip-whitespace str 0)))
        (let* ((idx1 (string-index str #\space i))
               (idx2 (string-index str #\space (1+ idx1))))
          (if (and idx1 idx2)
              (let ((code (parse-non-negative-integer str i idx1))
                    (agent (substring str (1+ idx1) idx2)))
                (call-with-values
                    (lambda () (parse-qstring str (1+ idx2) #:incremental? #t))
                  (lambda (text i)
                    (call-with-values
                        (lambda ()
                          (let ((c (and (< i len) (string-ref str i))))
                            (case c
                              ((#\space)
                               ;; we have a date.
                               (call-with-values
                                   (lambda () (parse-qstring str (1+ i)
                                                             #:incremental? #t))
                                 (lambda (date i)
                                   (values text (parse-date date) i))))
                              (else
                               (values text #f i)))))
                      (lambda (text date i)
                        (let ((w (list code agent text date))
                              (c (and (< i len) (string-ref str i))))
                          (case c
                            ((#f) (list w))
                            ((#\,) (cons w (lp (skip-whitespace str (1+ i)))))
                            (else (bad-header 'warning str))))))))))))))
  (lambda (val)
    (list-of? val
              (lambda (elt)
                (and (list? elt)
                     (= (length elt) 4)
                     (apply (lambda (code host text date)
                              (and (non-negative-integer? code) (< code 1000)
                                   (string? host)
                                   (string? text)
                                   (or (not date) (date? date))))
                            elt)))))
  (lambda (val port)
    (write-list
     val port
     (lambda (w port)
       (apply
        (lambda (code host text date)
          (display code port)
          (display #\space port)
          (display host port)
          (display #\space port)
          (write-qstring text port)
          (if date
              (begin
                (display #\space port)
                (write-date date port))))
        w))
     ", "))
  #:multiple? #t)




;;;
;;; Entity headers
;;;

;; Allow = #Method
;;
(declare-string-list-header allow
  "Allow")

;; Content-Encoding = 1#content-coding
;;
(declare-string-list-header content-encoding
  "Content-Encoding")

;; Content-Language = 1#language-tag
;;
(declare-string-list-header content-language
  "Content-Language")

;; Content-Length = 1*DIGIT
;;
(declare-integer-header content-length
  "Content-Length")

;; Content-Location = ( absoluteURI | relativeURI )
;;
(declare-uri-header content-location
  "Content-Location")

;; Content-MD5 = <base64 of 128 bit MD5 digest as per RFC 1864>
;;
(declare-opaque-header content-md5
  "Content-MD5")

;; Content-Range = content-range-spec
;; content-range-spec      = byte-content-range-spec
;; byte-content-range-spec = bytes-unit SP
;;                           byte-range-resp-spec "/"
;;                           ( instance-length | "*" )
;; byte-range-resp-spec = (first-byte-pos "-" last-byte-pos)
;;                                | "*"
;; instance-length           = 1*DIGIT
;;
(declare-header content-range
  "Content-Range"
  (lambda (str)
    (let ((dash (string-index str #\-))
          (slash (string-index str #\/)))
      (if (and (string-prefix? "bytes " str) slash)
          (list 'bytes
                (cond
                 (dash
                  (cons
                   (parse-non-negative-integer str 6 dash)
                   (parse-non-negative-integer str (1+ dash) slash)))
                 ((string= str "*" 6 slash)
                  '*)
                 (else
                  (bad-header 'content-range str)))
                (if (string= str "*" (1+ slash))
                    '*
                    (parse-non-negative-integer str (1+ slash))))
          (bad-header 'content-range str))))
  (lambda (val)
    (and (list? val) (= (length val) 3)
         (symbol? (car val))
         (let ((x (cadr val)))
           (or (eq? x '*)
               (and (pair? x)
                    (non-negative-integer? (car x))
                    (non-negative-integer? (cdr x)))))
         (let ((x (caddr val)))
           (or (eq? x '*)
               (non-negative-integer? x)))))
  (lambda (val port)
    (display (car val) port)
    (display #\space port)
    (if (eq? (cadr val) '*)
        (display #\* port)
        (begin
          (display (caadr val) port)
          (display #\- port)
          (display (caadr val) port)))
    (if (eq? (caddr val) '*)
        (display #\* port)
        (display (caddr val) port))))

;; Content-Type = media-type
;;
(declare-header content-type
  "Content-Type"
  (lambda (str)
    (let ((parts (string-split str #\;)))
      (call-with-values (lambda () (parse-media-type (car parts)))
        (lambda (type subtype)
          (cons* type subtype
                 (map (lambda (x)
                        (let ((eq (string-index x #\=)))
                          (if (and eq (= eq (string-rindex x #\=)))
                              (cons (string-trim x 0 eq)
                                    (string-trim-right x (1+ eq)))
                              (bad-header 'content-type str))))
                      (cdr parts)))))))
  (lambda (val)
    (and (list-of? val string?)
         (let ((len (length val)))
           (and (>= len 2)
                (even? len)))))
  (lambda (val port)
    (display (car val) port)
    (display #\/ port)
    (display (cadr val) port)
    (write-list
     (cddr val) port
     (lambda (pair port)
       (display (car pair) port)
       (display #\= port)
       (display (cdr pair) port))
     ";")))

;; Expires = HTTP-date
;;
(declare-date-header expires
  "Expires")

;; Last-Modified = HTTP-date
;;
(declare-date-header last-modified
  "Last-Modified")




;;;
;;; Request headers
;;;

;; Accept = #( media-range [ accept-params ] )
;; media-range = ( "*/*" | ( type "/" "*" ) | ( type "/" subtype ) )
;;               *( ";" parameter )
;; accept-params = ";" "q" "=" qvalue *( accept-extension )
;; accept-extension = ";" token [ "=" ( token | quoted-string ) ]
;;
(declare-param-list-header accept
  "Accept"
  ;; -> ("type/subtype" (str-prop . str-val) ...) ...)
  ;;
  ;; with the exception of prop = "q", in which case the prop will be
  ;; the symbol 'q, and the val will be a valid quality value
  ;;
  (lambda (k) (if (string=? k "q") 'q k))
  (lambda (k v)
    (if (eq? k 'q)
        (cons k (parse-quality v))
        (default-kons k v)))
  (lambda (k v)
    (if (eq? k 'q)
        (valid-quality? v)
        (default-kv-validator k v)))
  (lambda (k v port)
    (if (eq? k 'q)
        (write-quality v port)
        (default-val-writer k v port))))

;; Accept-Charset = 1#( ( charset | "*" )[ ";" "q" "=" qvalue ] )
;;
(declare-quality-list-header accept-charset
  "Accept-Charset")

;; Accept-Encoding = 1#( codings [ ";" "q" "=" qvalue ] )
;; codings = ( content-coding | "*" )
;;
(declare-quality-list-header accept-encoding
  "Accept-Encoding")

;; Accept-Language = 1#( language-range [ ";" "q" "=" qvalue ] )
;; language-range  = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
;;
(declare-quality-list-header accept-language
  "Accept-Language")

;; Authorization = credentials
;;
;; Authorization is basically opaque to this HTTP stack, we just pass
;; the string value through.
;; 
(declare-opaque-header authorization
  "Authorization")

;; Expect = 1#expectation
;; expectation = "100-continue" | expectation-extension
;; expectation-extension = token [ "=" ( token | quoted-string )
;;                         *expect-params ]
;; expect-params = ";" token [ "=" ( token | quoted-string ) ]
;;
(declare-param-list-header expect
  "Expect"
  (lambda (k)
    (if (equal? k "100-continue")
        '100-continue
        k)))

;; From = mailbox
;;
;; Should be an email address; we just pass on the string as-is.
;;
(declare-opaque-header from
  "From")

;; Host = host [ ":" port ]
;; 
(declare-header host
  "Host"
  (lambda (str)
    (let ((colon (string-index str #\:)))
      (if colon
          (cons (substring str 0 colon)
                (parse-non-negative-integer str (1+ colon)))
          (cons str #f))))
  (lambda (val)
    (and (pair? val)
         (string? (car val))
         (or (not (cdr val))
             (non-negative-integer? (cdr val)))))
  (lambda (val port)
    (display (car val) port)
    (if (cdr val)
        (begin
          (display #\: port)
          (display (cdr val) port)))))

;; If-Match = ( "*" | 1#entity-tag )
;;
(declare-entity-tag-list-header if-match
  "If-Match")

;; If-Modified-Since = HTTP-date
;;
(declare-date-header if-modified-since
  "If-Modified-Since")

;; If-None-Match = ( "*" | 1#entity-tag )
;;
(declare-entity-tag-list-header if-none-match
  "If-None-Match")

;; If-Range = ( entity-tag | HTTP-date )
;;
(declare-header if-range
  "If-Range"
  (lambda (str)
    (if (or (string-prefix? "\"" str)
            (string-prefix? "W/" str))
        (parse-entity-tag str)
        (parse-date str)))
  (lambda (val)
    (or (date? val) (entity-tag? val)))
  (lambda (val port)
    (if (date? val)
        (write-date val port)
        (write-entity-tag val port))))

;; If-Unmodified-Since = HTTP-date
;;
(declare-date-header if-unmodified-since
  "If-Unmodified-Since")

;; Max-Forwards = 1*DIGIT
;;
(declare-integer-header max-forwards
  "Max-Forwards")

;; Proxy-Authorization = credentials
;;
(declare-opaque-header proxy-authorization
  "Proxy-Authorization")

;; Range = "Range" ":" ranges-specifier
;; ranges-specifier = byte-ranges-specifier
;; byte-ranges-specifier = bytes-unit "=" byte-range-set
;; byte-range-set = 1#( byte-range-spec | suffix-byte-range-spec )
;; byte-range-spec = first-byte-pos "-" [last-byte-pos]
;; first-byte-pos = 1*DIGIT
;; last-byte-pos = 1*DIGIT
;; suffix-byte-range-spec = "-" suffix-length
;; suffix-length = 1*DIGIT
;;
(declare-header range
  "Range"
  (lambda (str)
    (if (string-prefix? "bytes=" str)
        (cons
         'bytes
         (map (lambda (x)
                (let ((dash (string-index x #\-)))
                  (cond
                   ((not dash)
                    (bad-header 'range str))
                   ((zero? dash)
                    (cons #f (parse-non-negative-integer x 1)))
                   ((= dash (1- (string-length x)))
                    (cons (parse-non-negative-integer x 0 dash) #f))
                   (else
                    (cons (parse-non-negative-integer x 0 dash)
                          (parse-non-negative-integer x (1+ dash)))))))
              (string-split (substring str 6) #\,)))
        (bad-header 'range str)))
  (lambda (val)
    (and (pair? val)
         (symbol? (car val))
         (list-of? (cdr val)
                   (lambda (elt)
                     (and (pair? elt)
                          (let ((x (car elt)) (y (cdr elt)))
                            (and (or x y)
                                 (or (not x) (non-negative-integer? x))
                                 (or (not y) (non-negative-integer? y)))))))))
  (lambda (val port)
    (display (car val) port)
    (display #\= port)
    (write-list
     (cdr val) port
     (lambda (pair port)
       (if (car pair)
           (display (car pair) port))
       (display #\- port)
       (if (cdr pair)
           (display (cdr pair) port)))
     ",")))

;; Referer = ( absoluteURI | relativeURI )
;;
(declare-uri-header referer
  "Referer")

;; TE = #( t-codings )
;; t-codings = "trailers" | ( transfer-extension [ accept-params ] )
;;
(declare-param-list-header te
  "TE"
  (lambda (k) (if (equal? k "trailers") 'trailers k)))

;; User-Agent = 1*( product | comment )
;;
(declare-opaque-header user-agent
  "User-Agent")




;;;
;;; Reponse headers
;;;

;; Accept-Ranges = acceptable-ranges
;; acceptable-ranges = 1#range-unit | "none"
;;
(declare-string-list-header accept-ranges
  "Accept-Ranges")

;; Age = age-value
;; age-value = delta-seconds
;;
(declare-integer-header age
  "Age")

;; ETag = entity-tag
;;
(declare-header etag
  "ETag"
  parse-entity-tag
  entity-tag?
  write-entity-tag)

;; Location = absoluteURI
;; 
(declare-uri-header location
  "Location")

;; Proxy-Authenticate = 1#challenge
;;
;; FIXME: split challenges ?
(declare-opaque-header proxy-authenticate
  "Proxy-Authenticate")

;; Retry-After  = ( HTTP-date | delta-seconds )
;;
(declare-header retry-after
  "Retry-After"
  (lambda (str)
    (if (and (not (string-null? str))
             (char-numeric? (string-ref str 0)))
        (parse-non-negative-integer str)
        (parse-date str)))
  (lambda (val)
    (or (date? val) (non-negative-integer? val)))
  (lambda (val port)
    (if (date? val)
        (write-date val port)
        (display val port))))

;; Server = 1*( product | comment )
;;
(declare-opaque-header server
  "Server")

;; Vary = ( "*" | 1#field-name )
;;
(declare-header vary
  "Vary"
  (lambda (str)
    (if (equal? str "*")
        '*
        (split-and-trim str)))
  (lambda (val)
    (or (eq? val '*) (list-of-strings? val)))
  (lambda (val port)
    (if (eq? val '*)
        (display "*" port)
        (write-list-of-strings val port))))

;; WWW-Authenticate = 1#challenge
;;
;; Hum.
(declare-opaque-header www-authenticate
  "WWW-Authenticate")
