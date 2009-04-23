(define-module (language scheme amatch)
  #:export (amatch))

;; This is exactly the same as pmatch except that it unpacks annotations
;; as needed.

(define-syntax amatch
  (syntax-rules (else guard)
    ((_ (op arg ...) cs ...)
     (let ((v (op arg ...)))
       (amatch v cs ...)))
    ((_ v) (if #f #f))
    ((_ v (else e0 e ...)) (begin e0 e ...))
    ((_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (amatch v cs ...))))
       (apat v pat
             (if (and g ...) (begin e0 e ...) (fk))
             (fk))))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (amatch v cs ...))))
       (apat v pat (begin e0 e ...) (fk))))))

(define-syntax apat
  (syntax-rules (_ quote unquote)
    ((_ v _ kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (quote lit) kt kf)
     (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (apair? v)
         (let ((vx (acar v)) (vy (acdr v)))
           (apat vx x (apat vy y kt kf) kf))
         kf))
    ((_ v lit kt kf) (if (eq? v (quote lit)) kt kf))))
