;;; examples/box-dynamic-module/box-module.scm -- Scheme part of the
;;;   dynamic module (box-module)

;;; Commentary:

;;; This is the Scheme part of the dynamic library module (box-module).
;;; When you do a (use-modules (box-module)) in this directory,
;;; this file gets loaded and will load the compiled extension.

;;; Code:

;;; Author: Martin Grabmueller
;;; Date: 2001-06-06

(define-module (box-module))

(load-extension "libbox-module" "scm_init_box")
