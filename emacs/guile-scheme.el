;;; guile-scheme.el --- Guile Scheme editing mode.

;; Copyright (C) 2001 Keisuke Nishida <kxn30@po.cwru.edu>

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

;;; Code:

(require 'scheme)

(defgroup guile-scheme nil
  "Editing Guile-Scheme code"
  :group 'lisp)

(defvar guile-scheme-syntax-keywords
  '((begin 0) (if 1) (cond 0) (case 1) (do 2) (try 1)
    quote syntax lambda and or else delay receive
    (match 1) (match-lambda 0) (match-lambda* 0)
    (let scheme-let-indent) (let* 1) (letrec 1) (and-let* 1)
    (let-syntax 1) (letrec-syntax 1) (syntax-rules 1) (syntax-case 2)))

(defvar guile-scheme-special-procedures
  '((catch 1) (lazy-catch 1) (stack-catch 1)
    map for-each (dynamic-wind 3)))

(dolist (x (append guile-scheme-syntax-keywords
		   guile-scheme-special-procedures))
  (when (consp x)
    (put (car x) 'scheme-indent-function (cadr x))))

;; This is shared by cmuscheme and xscheme.
(defcustom guile-scheme-program-name "guile"
  "*Program invoked by the `run-scheme' command."
  :type 'string
  :group 'guile-scheme)

(defconst guile-scheme-font-lock-keywords
  (eval-when-compile
    (list
     (list (concat "(\\(define\\*?\\("
		   ;; Function names.
		   "\\(\\|-public\\|-method\\|-generic\\)\\|"
		   ;; Macro names, as variable names.  A bit dubious, this.
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
  "Expressions to highlight in Guile-Scheme modes.")

(defvar guile-scheme-mode-syntax-table nil)
(unless guile-scheme-mode-syntax-table
  (let ((i 0))
    (setq guile-scheme-mode-syntax-table (make-syntax-table))
    (set-syntax-table guile-scheme-mode-syntax-table)

    ;; Default is atom-constituent.
    (while (< i 256)
      (modify-syntax-entry i "_   ")
      (setq i (1+ i)))

    ;; Word components.
    (setq i ?0)
    (while (<= i ?9)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))
    (setq i ?A)
    (while (<= i ?Z)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))
    (setq i ?a)
    (while (<= i ?z)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    ")
    (modify-syntax-entry ?\n ">   ")
    (modify-syntax-entry ?\f "    ")
    (modify-syntax-entry ?\r "    ")
    (modify-syntax-entry ?  "    ")

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  ")
    (modify-syntax-entry ?\] ")[  ")
    (modify-syntax-entry ?{ "(}  ")
    (modify-syntax-entry ?} "){  ")
    (modify-syntax-entry ?\| "  23")

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  ")
    (modify-syntax-entry ?\) ")(  ")
    (modify-syntax-entry ?\; "<   ")
    (modify-syntax-entry ?\" "\"    ")
    (modify-syntax-entry ?' "  p")
    (modify-syntax-entry ?` "  p")
    (modify-syntax-entry ?. "  p")

    ;; Special characters
    (modify-syntax-entry ?, "_ p")
    (modify-syntax-entry ?# "_ p14")
    (modify-syntax-entry ?\\ "\\   ")))

(defvar guile-scheme-mode-line-process "")

(defvar guile-scheme-mode-map nil
  "Keymap for Guile Scheme mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(unless guile-scheme-mode-map
  (let ((map (make-sparse-keymap "Guile-Scheme")))
    (setq guile-scheme-mode-map (make-sparse-keymap))
    (set-keymap-parent guile-scheme-mode-map lisp-mode-shared-map)
    (define-key guile-scheme-mode-map [menu-bar] (make-sparse-keymap))
    (define-key guile-scheme-mode-map [menu-bar guile-scheme]
      (cons "Guile Scheme" map))
    (define-key map [run-guile-scheme]
      '("Run Inferior Guile-Scheme" . run-guile-scheme))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)))

(defcustom guile-scheme-mode-hook nil
  "Normal hook run when entering `guile-scheme-mode'.
See `run-hooks'."
  :type 'hook
  :group 'guile-scheme)


;;;
;;; Guile Scheme mode
;;;

;;;###autoload
(defun guile-scheme-mode ()
  "Major mode for editing Guile-Scheme code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior Scheme process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Scheme buffers.  The names of commands that interact
with the Scheme process start with \"xscheme-\" if you use the MIT
Scheme-specific `xscheme' package; for more information see the
documentation for `xscheme-interaction-mode'.  Use \\[run-scheme] to
start an inferior Scheme using the more general `cmuscheme' package.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entry to this mode calls the value of `scheme-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Guile Scheme")
  (setq major-mode 'guile-scheme-mode)
  (use-local-map guile-scheme-mode-map)
  (guile-scheme-mode-variables)
  (run-hooks 'guile-scheme-mode-hook))

(defun guile-scheme-mode-variables ()
  (set-syntax-table guile-scheme-mode-syntax-table)
  (setq local-abbrev-table scheme-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'lisp-indent-function)
  (set lisp-indent-function 'scheme-indent-function)
  (setq mode-line-process '("" guile-scheme-mode-line-process))
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-syntax-alist)
	'(("+-*/.<>=?!$%_&~^:" . "w")))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((guile-scheme-font-lock-keywords)
          nil t (("+-*/.<>=!?$%_&~^:@" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun))))

(provide 'guile-scheme)

;;; guile-scheme.el ends here
