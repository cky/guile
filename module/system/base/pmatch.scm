(define-module (system base pmatch)
  #:export (pmatch))
;; FIXME: shouldn't have to export ppat...

;; Originally written by Oleg Kiselyov. Taken from:
;; αKanren: A Fresh Name in Nominal Logic Programming
;; by William E. Byrd and Daniel P. Friedman
;; Proceedings of the 2007 Workshop on Scheme and Functional Programming
;; Université Laval Technical Report DIUL-RT-0701

;; Licensing unclear. Probably need to ask Oleg for a disclaimer.

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ (op arg ...) cs ...)
     (let ((v (op arg ...)))
       (pmatch v cs ...)))
    ((_ v) (if #f #f))
    ((_ v (else e0 e ...)) (let () e0 e ...))
    ((_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat
             (if (and g ...) (let () e0 e ...) (fk))
             (fk))))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (let () e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (_ quote unquote)
    ((_ v _ kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (quote lit) kt kf)
     (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (ppat vx x (ppat vy y kt kf) kf))
         kf))
    ((_ v lit kt kf) (if (eq? v (quote lit)) kt kf))))
