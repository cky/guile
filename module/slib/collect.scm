;"collect.scm" Sample collection operations
; COPYRIGHT (c) Kenneth Dickey 1992
;
;               This software may be used for any purpose whatever
;               without warrantee of any kind.
; AUTHOR        Ken Dickey
; DATE          1992 September 1
; LAST UPDATED  1992 September 2
; NOTES         Expository (optimizations & checks elided).
;               Requires YASOS (Yet Another Scheme Object System).

(require 'yasos)

(define-operation (collect:collection? obj)
 ;; default
  (cond
    ((or (list? obj) (vector? obj) (string? obj)) #t)
    (else #f)
) )

(define (collect:empty? collection) (zero? (yasos:size collection)))

(define-operation (collect:gen-elts <collection>) ;; return element generator
  ;; default behavior
  (cond                      ;; see utilities, below, for generators
    ((vector? <collection>) (collect:vector-gen-elts <collection>))
    ((list?   <collection>) (collect:list-gen-elts   <collection>))
    ((string? <collection>) (collect:string-gen-elts <collection>))
    (else
     (slib:error "Operation not supported: GEN-ELTS " (yasos:print obj #f)))
) )

(define-operation (collect:gen-keys collection)
  (if (or (vector? collection) (list? collection) (string? collection))
      (let ( (max+1 (yasos:size collection)) (index 0) )
	 (lambda ()
            (cond
	      ((< index max+1)
	       (set! index (collect:add1 index))
	       (collect:sub1 index))
	      (else (slib:error "no more keys in generator"))
      ) ) )
      (slib:error "Operation not handled: GEN-KEYS " collection)
) )

(define (collect:do-elts <proc> . <collections>)
  (let ( (max+1 (yasos:size (car <collections>)))
         (generators (map collect:gen-elts <collections>))
       )
    (let loop ( (counter 0) )
       (cond
          ((< counter max+1)
           (apply <proc> (map (lambda (g) (g)) generators))
           (loop (collect:add1 counter))
          )
          (else 'unspecific)  ; done
    )  )
) )

(define (collect:do-keys <proc> . <collections>)
  (let ( (max+1 (yasos:size (car <collections>)))
         (generators (map collect:gen-keys <collections>))
       )
    (let loop ( (counter 0) )
       (cond
          ((< counter max+1)
           (apply <proc> (map (lambda (g) (g)) generators))
           (loop (collect:add1 counter))
          )
          (else 'unspecific)  ; done
    )  )
) )

(define (collect:map-elts <proc> . <collections>)
  (let ( (max+1 (yasos:size (car <collections>)))
         (generators (map collect:gen-elts <collections>))
         (vec (make-vector (yasos:size (car <collections>))))
       )
    (let loop ( (index 0) )
       (cond
          ((< index max+1)
           (vector-set! vec index (apply <proc> (map (lambda (g) (g)) generators)))
           (loop (collect:add1 index))
          )
          (else vec)  ; done
    )  )
) )

(define (collect:map-keys <proc> . <collections>)
  (let ( (max+1 (yasos:size (car <collections>)))
         (generators (map collect:gen-keys <collections>))
	 (vec (make-vector (yasos:size (car <collections>))))
       )
    (let loop ( (index 0) )
       (cond
          ((< index max+1)
           (vector-set! vec index (apply <proc> (map (lambda (g) (g)) generators)))
           (loop (collect:add1 index))
          )
          (else vec)  ; done
    )  )
) )

(define-operation (collect:for-each-key <collection> <proc>)
   ;; default
   (collect:do-keys <proc> <collection>)  ;; talk about lazy!
)

(define-operation (collect:for-each-elt <collection> <proc>)
   (collect:do-elts <proc> <collection>)
)

(define (collect:reduce <proc> <seed> . <collections>)
   (let ( (max+1 (yasos:size (car <collections>)))
          (generators (map collect:gen-elts <collections>))
        )
     (let loop ( (count 0) )
       (cond
          ((< count max+1)
           (set! <seed>
                 (apply <proc> <seed> (map (lambda (g) (g)) generators)))
           (loop (collect:add1 count))
          )
          (else <seed>)
     ) )
)  )



;; pred true for every elt?
(define (collect:every? <pred?> . <collections>)
   (let ( (max+1 (yasos:size (car <collections>)))
          (generators (map collect:gen-elts <collections>))
        )
     (let loop ( (count 0) )
       (cond
          ((< count max+1)
           (if (apply <pred?> (map (lambda (g) (g)) generators))
               (loop (collect:add1 count))
               #f)
          )
          (else #t)
     ) )
)  )

;; pred true for any elt?
(define (collect:any? <pred?> . <collections>)
   (let ( (max+1 (yasos:size (car <collections>)))
          (generators (map collect:gen-elts <collections>))
        )
     (let loop ( (count 0) )
       (cond
          ((< count max+1)
           (if (apply <pred?> (map (lambda (g) (g)) generators))
               #t
               (loop (collect:add1 count))
          ))
          (else #f)
     ) )
)  )


;; MISC UTILITIES

(define (collect:add1 obj)  (+ obj 1))
(define (collect:sub1 obj)  (- obj 1))

;; Nota Bene:  list-set! is bogus for element 0

(define (collect:list-set! <list> <index> <value>)

  (define (set-loop last this idx)
     (cond
        ((zero? idx)
         (set-cdr! last (cons <value> (cdr this)))
         <list>
        )
        (else (set-loop (cdr last) (cdr this) (collect:sub1 idx)))
  )  )

  ;; main
  (if (zero? <index>)
      (cons <value> (cdr <list>))  ;; return value
      (set-loop <list> (cdr <list>) (collect:sub1 <index>)))
)

(add-setter list-ref collect:list-set!)  ; for (setter list-ref)


;; generator for list elements
(define (collect:list-gen-elts <list>)
  (lambda ()
     (if (null? <list>)
         (slib:error "No more list elements in generator")
         (let ( (elt (car <list>)) )
           (set! <list> (cdr <list>))
           elt))
) )

;; generator for vector elements
(define (collect:make-vec-gen-elts <accessor>)
  (lambda (vec)
    (let ( (max+1 (yasos:size vec))
           (index 0)
         )
      (lambda ()
         (cond ((< index max+1)
                (set! index (collect:add1 index))
                (<accessor> vec (collect:sub1 index))
               )
               (else #f)
      )  )
  ) )
)

(define collect:vector-gen-elts (collect:make-vec-gen-elts vector-ref))

(define collect:string-gen-elts (collect:make-vec-gen-elts string-ref))

;;; exports:

(define collection? collect:collection?)
(define empty? collect:empty?)
(define gen-keys collect:gen-keys)
(define gen-elts collect:gen-elts)
(define do-elts collect:do-elts)
(define do-keys collect:do-keys)
(define map-elts collect:map-elts)
(define map-keys collect:map-keys)
(define for-each-key collect:for-each-key)
(define for-each-elt collect:for-each-elt)
(define reduce collect:reduce)		; reduce is also in comlist.scm
(define every? collect:every?)
(define any? collect:any?)

;;                        --- E O F "collect.oo" ---                    ;;
