;;; guile-scheme.el --- Guile Scheme editing mode

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Put the following lines in your ~/.emacs:
;; 
;;   (require 'guile-scheme)
;;   (setq initial-major-mode 'scheme-interaction-mode)

;;; Code:

(require 'guile)
(require 'scheme)

(defgroup guile-scheme nil
  "Editing Guile-Scheme code"
  :group 'lisp)

(defvar guile-scheme-syntax-keywords
  '((begin 0) (if 1) (cond 0) (case 1) (do 2)
    quote syntax lambda and or else delay receive use-modules
    (match 1) (match-lambda 0) (match-lambda* 0)
    (let scheme-let-indent) (let* 1) (letrec 1) (and-let* 1)
    (let-syntax 1) (letrec-syntax 1) (syntax-rules 1) (syntax-case 2)))

(defvar guile-scheme-special-procedures
  '((catch 1) (lazy-catch 1) (stack-catch 1)
    map for-each (dynamic-wind 3)))

;; set indent functions
(dolist (x (append guile-scheme-syntax-keywords
		   guile-scheme-special-procedures))
  (when (consp x)
    (put (car x) 'scheme-indent-function (cadr x))))

(defconst guile-scheme-font-lock-keywords
  (eval-when-compile
    (list
     (list (concat "(\\(define\\*?\\("
		   ;; Function names.
		   "\\(\\|-public\\|-method\\|-generic\\)\\|"
		   ;; Macro names, as variable names.
		   "\\(-syntax\\|-macro\\)\\|"
		   ;; Others
		   "-\\sw+\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "\\s *(?\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(5 (cond ((match-beginning 3) font-lock-function-name-face)
		     ((match-beginning 4) font-lock-variable-name-face)
		     (t font-lock-type-face)) nil t))
     (list (concat
	    "(" (regexp-opt
		 (mapcar (lambda (e)
			   (prin1-to-string (if (consp e) (car e) e)))
			 (append guile-scheme-syntax-keywords
				 guile-scheme-special-procedures)) 'words))
	   '(1 font-lock-keyword-face))
     '("<\\sw+>" . font-lock-type-face)
     '("\\<:\\sw+\\>" . font-lock-builtin-face)
     ))
  "Expressions to highlight in Guile Scheme mode.")


;;;
;;; Guile Scheme mode
;;;

(defvar guile-scheme-mode-map nil
  "Keymap for Guile Scheme mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(unless guile-scheme-mode-map
  (let ((map (make-sparse-keymap "Guile-Scheme")))
    (setq guile-scheme-mode-map map)
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar guile-scheme] (cons "Guile-Scheme" map))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (define-key map "\e\C-i" 'guile-scheme-complete-symbol)
    (define-key map "\e\C-x" 'guile-scheme-eval-define)
    (define-key map "\C-x\C-e" 'guile-scheme-eval-last-sexp)
    (define-key map "\C-c\C-b" 'guile-scheme-eval-buffer)
    (define-key map "\C-c\C-r" 'guile-scheme-eval-region)
    (define-key map "\C-c:" 'guile-scheme-eval-expression)
    (define-key map "\C-c\C-a" 'guile-scheme-apropos)
    (define-key map "\C-c\C-d" 'guile-scheme-describe)

    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)))

(defcustom guile-scheme-mode-hook nil
  "Normal hook run when entering `guile-scheme-mode'."
  :type 'hook
  :group 'guile-scheme)

;;;###autoload
(defun guile-scheme-mode ()
  "Major mode for editing Guile Scheme code.
Editing commands are similar to those of `scheme-mode'.

\\{scheme-mode-map}
Entry to this mode calls the value of `scheme-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Guile Scheme")
  (setq major-mode 'guile-scheme-mode)
  (use-local-map guile-scheme-mode-map)
  (scheme-mode-variables)
  (setq mode-line-process
	'(:eval (if (processp guile-scheme-adapter)
		    (format " [%s]" guile-scheme-command)
		  "")))
  (setq font-lock-defaults
        '((guile-scheme-font-lock-keywords)
          nil t (("+-*/.<>=!?$%_&~^:@" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))
  (run-hooks 'guile-scheme-mode-hook))


;;;
;;; Scheme interaction mode
;;;

(defvar scheme-interaction-mode-map ()
  "Keymap for Scheme Interaction mode.
All commands in `guile-scheme-mode-map' are inherited by this map.")

(unless scheme-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (setq scheme-interaction-mode-map map)
    (set-keymap-parent map guile-scheme-mode-map)
    (define-key map "\C-j" 'guile-scheme-eval-print-last-sexp)
    ))

(defvar scheme-interaction-mode-hook nil
  "Normal hook run when entering `scheme-interaction-mode'.")

(defun scheme-interaction-mode ()
  "Major mode for evaluating Scheme expressions with Guile.

\\{scheme-interaction-mode-map}"
  (interactive)
  (guile-scheme-mode)
  (use-local-map scheme-interaction-mode-map)
  (setq major-mode 'scheme-interaction-mode)
  (setq mode-name "Scheme Interaction")
  (run-hooks 'scheme-interaction-mode-hook))


;;;
;;; Guile Scheme adapter
;;;

(defvar guile-scheme-command "guile")
(defvar guile-scheme-adapter nil)

(defun guile-scheme-adapter ()
  (if (and (processp guile-scheme-adapter)
	   (eq (process-status guile-scheme-adapter) 'run))
      guile-scheme-adapter
    (setq guile-scheme-adapter
	  (guile:make-adapter guile-scheme-command 'emacs-scheme-channel))))

(defun guile-scheme-set-module ()
  "Set the current module based on buffer contents.
If there is a (define-module ...) form, evaluate it.
Otherwise, choose module (guile-user)."
  (save-excursion
    (guile:eval
     (if (re-search-backward "^(define-module " nil t)
	 (let ((start (match-beginning 0)))
	   (goto-char start)
	   (forward-sexp)
	   (buffer-substring-no-properties start (point)))
       "(define-module (emacs-user))")
     (guile-scheme-adapter))))

(defun guile-scheme-eval-string (string)
  (guile-scheme-set-module)
  (guile:eval string (guile-scheme-adapter)))

(defun guile-scheme-display-result (value flag)
  (if (string= value "#<unspecified>")
      (setq value "done"))
  (if flag
      (insert value)
    (message "%s" value)))


;;;
;;; Interactive commands
;;;

(defun guile-scheme-eval-expression (string)
  "Evaluate the expression in STRING and show value in echo area."
  (interactive "SGuile Scheme Eval: ")
  (guile-scheme-display-result (guile-scheme-eval-string string) nil))

(defun guile-scheme-eval-region (start end)
  "Evaluate the region as Guile Scheme code."
  (interactive "r")
  (guile-scheme-eval-expression (buffer-substring-no-properties start end)))

(defun guile-scheme-eval-buffer ()
  "Evaluate the current buffer as Guile Scheme code."
  (interactive)
  (guile-scheme-eval-expression (buffer-string)))

(defun guile-scheme-eval-last-sexp (arg)
  "Evaluate sexp before point; show value in echo area.
With argument, print output into current buffer."
  (interactive "P")
  (guile-scheme-display-result
   (guile-scheme-eval-string
    (buffer-substring-no-properties
     (point) (save-excursion (backward-sexp) (point)))) arg))

(defun guile-scheme-eval-print-last-sexp ()
  "Evaluate sexp before point; print value into current buffer."
  (interactive)
  (insert "\n")
  (guile-scheme-eval-last-sexp t)
  (insert "\n"))

(defun guile-scheme-eval-define ()
  (interactive)
  (guile-scheme-eval-region (save-excursion (end-of-defun) (point))
			    (save-excursion (beginning-of-defun) (point))))

(defun guile-scheme-load-file (file)
  "Load a Guile Scheme file."
  (interactive "fGuile Scheme load file: ")
  (guile-scheme-eval-string (format "(load %s)" (expand-file-name file)))
  (message "done"))

(defun guile-scheme-complete-symbol ()
  (interactive)
  (unless (boundp 'guile-emacs-complete-alist)
    (guile-import guile-emacs-complete-alist))
  (let* ((end (point))
	 (start (save-excursion (skip-syntax-backward "w_") (point)))
	 (pattern (buffer-substring-no-properties start end))
	 (alist (guile-emacs-complete-alist pattern)))
    (goto-char end)
    (let ((completion (try-completion pattern alist)))
      (cond ((eq completion t))
	    ((not completion)
	     (message "Can't find completion for \"%s\"" pattern)
	     (ding))
	    ((not (string= pattern completion))
	     (delete-region start end)
	     (insert completion))
	    (t
	     (message "Making completion list...")
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list alist))
	     (message "Making completion list...done"))))))

;; (define-command (guile-scheme-apropos regexp)
;;   (interactive "sGuile-Scheme apropos (regexp): ")
;;   (guile-scheme-set-module)
;;   (let ((old #^guile-scheme-output-buffer))
;;     (dynamic-wind
;; 	(lambda () (set! #^guile-scheme-output-buffer #f))
;; 	(lambda ()
;; 	  (with-output-to-temp-buffer "*Help*"
;; 	    (lambda ()
;; 	      (apropos regexp))))
;; 	(lambda () (set! #^guile-scheme-output-buffer old)))))
;; 
;; (define (guile-scheme-input-symbol prompt)
;;   (let* ((symbol (thing-at-point 'symbol))
;; 	 (table (map (lambda (sym) (list (symbol->string sym)))
;; 		     (apropos-list "")))
;; 	 (default (if (assoc symbol table)
;; 		      (string-append " (default " symbol ")")
;; 		      "")))
;;     (string->symbol (completing-read (string-append prompt default ": ")
;; 				     table #f #t #f #f symbol))))
;; 
;; (define-command (guile-scheme-describe symbol)
;;   "Display the value and documentation of SYMBOL."
;;   (interactive (list (guile-scheme-input-symbol "Describe Guile-Scheme variable")))
;;   (guile-scheme-set-module)
;;   (let ((old #^guile-scheme-output-buffer))
;;     (dynamic-wind
;; 	(lambda () (set! #^guile-scheme-output-buffer #f))
;; 	(lambda ()
;; 	  (begin-with-output-to-temp-buffer "*Help*"
;; 	    (describe symbol)))
;; 	(lambda () (set! #^guile-scheme-output-buffer old)))))
;; 
;; (define-command (guile-scheme-find-definition symbol)
;;   (interactive (list (guile-scheme-input-symbol "Guile-Scheme find definition")))
;;   (guile-scheme-set-module)
;;   )


;;;
;;; Turn on guile-scheme-mode for .scm files by default.
;;;

(setq auto-mode-alist
      (cons '("\\.scm\\'" . guile-scheme-mode) auto-mode-alist))

(provide 'guile-scheme)

;;; guile-scheme.el ends here
