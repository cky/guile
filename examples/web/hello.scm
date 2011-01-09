;;; Commentary:

;;; A simple web server that responds to all requests with the eponymous
;;; string. Visit http://localhost:8080 to test.

;;; Code:

(use-modules (web server))

;; A handler receives two values as arguments: the request object, and
;; the request body.  It returns two values also: the response object,
;; and the response body.
;;
;; In this simple example we don't actually access the request object,
;; but if we wanted to, we would use the procedures from the `(web
;; request)' module.  If there is no body given in the request, the body
;; argument will be false.
;;
;; To create a response object, use the `build-response' procedure from
;; `(web response)'.  Here we take advantage of a shortcut, in which we
;; return an alist of headers for the response instead of returning a
;; proper response object. In this case, a response object will be made
;; for us with a 200 OK status.
;;
(define (handler request body)
  (values '((content-type . (text/plain)))
          "Hello, World!"))

(run-server handler)
