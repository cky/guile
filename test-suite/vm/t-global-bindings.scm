;; Are global bindings reachable at run-time?  This relies on the
;; `object-ref' and `object-set' instructions.

(begin

  (define the-binding "hello")

  ((lambda () the-binding))

  ((lambda () (set! the-binding "world")))

  ((lambda () the-binding)))

