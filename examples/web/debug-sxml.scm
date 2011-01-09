;;; Commentary:

;;; A simple debugging server that responds to all responses with a
;;; table containing the headers given in the request.
;;;
;;; As a novelty, this server uses a little micro-framework to build up
;;; the response as SXML. Instead of a string, the `respond' helper
;;; returns a procedure for the body, which allows the `(web server)'
;;; machinery to collect the output as a bytevector in the desired
;;; encoding, instead of building an intermediate output string.
;;;
;;; In the future this will also allow for chunked transfer-encoding,
;;; for HTTP/1.1 clients.

;;; Code:

(use-modules (web server)
             (web request)
             (web response)
             (sxml simple))

(define html5-doctype "<!DOCTYPE html>\n")
(define default-title "Hello hello!")

(define* (templatize #:key (title "No title") (body '((p "No body"))))
  `(html (head (title ,title))
         (body ,@body)))

(define* (respond #:optional body #:key
                  (status 200)
                  (title default-title)
                  (doctype html5-doctype)
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '())
                  (sxml (and body (templatize #:title title #:body body))))
  (values (build-response
           #:code status
           #:headers `((content-type . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port)
            (if sxml
                (begin
                  (if doctype (display doctype port))
                  (sxml->xml sxml port))))))

(define (debug-page request body)
  (respond `((h1 "hello world!")
             (table
              (tr (th "header") (th "value"))
              ,@(map (lambda (pair)
                       `(tr (td (tt ,(with-output-to-string
                                       (lambda () (display (car pair))))))
                            (td (tt ,(with-output-to-string
                                       (lambda ()
                                         (write (cdr pair))))))))
                     (request-headers request))))))

(run-server debug-page)
