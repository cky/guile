;;; guile-c.el --- Guile C editing commands

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; (add-hook 'c-mode-hook
;;   (lambda ()
;;     (require 'guile-c)
;;     (define-key c-mode-map "\C-c\C-g\C-p" 'guile-c-insert-define)
;;     (define-key c-mode-map "\C-c\C-g\C-e" 'guile-c-edit-docstring)
;;     (define-key c-mode-map "\C-c\C-g\C-d" 'guile-c-deprecate-region)
;;     ))

;;; Code:

(require 'cc-mode)

(defvar guile-c-prefix "scm_")


;;;
;;; Insert templates
;;;

(defun guile-c-insert-define ()
  "Insert a template of a Scheme procedure.

  M-x guile-c-insert-define RET foo arg , opt . rest =>

  SCM_DEFINE (scm_foo, \"foo\", 1, 1, 1,
              (SCM arg, SCM opt, SCM rest),
              \"\")
  #define FUNC_NAME s_scm_foo
  {
  
  }
  #undef FUNC_NAME"
  (interactive)
  (let ((tokens (split-string (read-string "Procedure: ")))
	name args opts rest)
    ;; Get procedure name
    (if (not tokens) (error "No procedure name"))
    (setq name (car tokens) tokens (cdr tokens))
    ;; Get requisite arguments
    (while (and tokens (not (member (car tokens) '("," "."))))
      (setq args (cons (car tokens) args) tokens (cdr tokens)))
    (setq args (nreverse args))
    ;; Get optional arguments
    (when (string= (car tokens) ",")
      (setq tokens (cdr tokens))
      (while (and tokens (not (string= (car tokens) ".")))
	(setq opts (cons (car tokens) opts) tokens (cdr tokens)))
      (setq opts (nreverse opts)))
    ;; Get rest argument
    (when (string= (car tokens) ".")
      (setq rest (list (cadr tokens))))
    ;; Insert template
    (let ((c-name (guile-c-name-from-scheme-name name)))
      (insert (format "SCM_DEFINE (%s, \"%s\", %d, %d, %d,\n"
		      c-name name (length args) (length opts) (length rest))
	      "\t    ("
	      (mapconcat (lambda (a) (concat "SCM " a))
			 (append args opts rest) ", ")
	      "),\n"
	      "\t    \"\")\n"
	      "#define FUNC_NAME s_" c-name "\n"
	      "{\n\n}\n"
	      "#undef FUNC_NAME\n\n")
      (previous-line 4)
      (indent-for-tab-command))))

(defun guile-c-name-from-scheme-name (name)
  (while (string-match "\\?$" name) (setq name (replace-match "_p" t t name)))
  (while (string-match "!$" name) (setq name (replace-match "_x" t t name)))
  (while (string-match "^%" name) (setq name (replace-match "sys_" t t name)))
  (while (string-match "->" name) (setq name (replace-match "_to_" t t name)))
  (while (string-match "[-:]" name) (setq name (replace-match "_" t t name)))
  (concat guile-c-prefix name))


;;;
;;; Edit docstrings
;;;

(defvar guile-c-window-configuration nil)

(defun guile-c-edit-docstring ()
  (interactive)
  (let* ((region (guile-c-find-docstring))
	 (doc (if region (buffer-substring (car region) (cdr region)))))
    (if (not doc)
	(error "No docstring!")
      (setq guile-c-window-configuration (current-window-configuration))
      (with-current-buffer (get-buffer-create "*Guile Docstring*")
	(erase-buffer)
	(insert doc)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "[ \t]*\"")
	      (delete-region (match-beginning 0) (match-end 0)))
	  (end-of-line)
	  (if (eq (char-before (point)) ?\")
	      (delete-backward-char 1))
	  (if (and (eq (char-before (point)) ?n)
		   (eq (char-before (1- (point))) ?\\))
	      (delete-backward-char 2))
	  (forward-line))
	(goto-char (point-min))
	(texinfo-mode)
	(if global-font-lock-mode
	    (font-lock-fontify-buffer))
	(local-set-key "\C-c\C-c" 'guile-c-edit-finish)
	(setq fill-column 63)
	(switch-to-buffer-other-window (current-buffer))
        (message "Type `C-c C-c' to finish")))))

(defun guile-c-edit-finish ()
  (interactive)
  (goto-char (point-max))
  (while (eq (char-before) ?\n) (backward-delete-char 1))
  (goto-char (point-min))
  (if (eobp)
      (insert "\"\"")
    (while (not (eobp))
      (insert "\t    \"")
      (end-of-line)
      (insert (if (eobp) "\"" "\\n\""))
      (forward-line 1)))
  (let ((doc (buffer-string)))
    (kill-buffer (current-buffer))
    (set-window-configuration guile-c-window-configuration)
    (let ((region (guile-c-find-docstring)))
      (goto-char (car region))
      (delete-region (car region) (cdr region)))
    (insert doc)))

(defun guile-c-find-docstring ()
  (save-excursion
    (if (re-search-backward "^SCM_DEFINE" nil t)
	(let ((start (progn (forward-line 2) (point))))
	  (while (looking-at "[ \t]*\"")
	    (forward-line 1))
	  (cons start (- (point) 2))))))


;;;
;;; Others
;;;

(defun guile-c-deprecate-region (start end)
  (interactive "r")
  (save-excursion
    (let ((marker (make-marker)))
      (set-marker marker end)
      (goto-char start)
      (insert "#if (SCM_DEBUG_DEPRECATED == 0)\n\n")
      (goto-char marker)
      (insert "\n#endif /* (SCM_DEBUG_DEPRECATED == 0) */\n"))))

(provide 'guile-c)

;; guile-c.el ends here
