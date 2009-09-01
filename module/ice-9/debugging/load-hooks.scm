
(define-module (ice-9 debugging load-hooks)
  #:export (before-load-hook
            after-load-hook
            install-load-hooks
            uninstall-load-hooks))

;; real-primitive-load: holds the real (C-implemented) definition of
;; primitive-load, when the load hooks are installed.
(define real-primitive-load #f)

;; The load hooks themselves.  These are called with one argument, the
;; name of the file concerned.
(define before-load-hook (make-hook 1))
(define after-load-hook (make-hook 1))

;; primitive-load-with-hooks: our new definition for primitive-load.
(define (primitive-load-with-hooks filename)
  (run-hook before-load-hook filename)
  (real-primitive-load filename)
  (run-hook after-load-hook filename))

(define (install-load-hooks)
  (if real-primitive-load
      (error "load hooks are already installed"))
  (set! real-primitive-load primitive-load)
  (set! primitive-load primitive-load-with-hooks))

(define (uninstall-load-hooks)
  (or real-primitive-load
      (error "load hooks are not installed"))
  (set! primitive-load real-primitive-load)
  (set! real-primitive-load #f))
