;; Per-directory local variables for GNU Emacs 23 and later.

((nil             . ((fill-column . 72)
                     (tab-width   .  8)))
 (c-mode          . ((c-file-style . "gnu")))
 (scheme-mode
  . ((indent-tabs-mode . nil)
     (eval . (put 'pass-if-equal 'scheme-indent-function 2))))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)))
 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 72))))
