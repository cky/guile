;;; guile.el --- Emacs Guile interface

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

(require 'cl)

;;;
;;; Low level interface
;;;

(defvar gulie-emacs-file
  (catch 'return
    (mapc (lambda (dir)
	    (let ((file (expand-file-name "guile-emacs.scm" dir)))
	      (if (file-exists-p file) (throw 'return file))))
	  load-path)
    (error "Cannot find guile-emacs.scm")))

(defvar gulie-channel-file
  (catch 'return
    (mapc (lambda (dir)
	    (let ((file (expand-file-name "channel.scm" dir)))
	      (if (file-exists-p file) (throw 'return file))))
	  load-path)))

(defvar guile-libs
  (nconc (if gulie-channel-file (list "-l" gulie-channel-file) '())
	 (list "-l" gulie-emacs-file)))

;;;###autoload
(defun guile:make-adapter (command channel)
  (let* ((buff (generate-new-buffer " *guile object channel*"))
	 (libs (if gulie-channel-file (list "-l" gulie-channel-file) nil))
	 (proc (apply 'start-process "guile-oa" buff command "-q" guile-libs)))
    (process-kill-without-query proc)
    (accept-process-output proc)
    (guile-process-require proc (format "(%s)\n" channel) "channel> ")
    proc))

(put 'guile-error 'error-conditions '(guile-error error))
(put 'guile-error 'error-message "Guile error")

(defvar guile-token-tag "<guile>")

(defun guile-tokenp (x) (and (consp x) (eq (car x) guile-token-tag)))

;;;###autoload
(defun guile:eval (string adapter)
  (let ((output (guile-process-require adapter (concat "eval " string "\n")
				       "channel> ")))
    (cond
     ((string= output "") nil)
     ((string-match "^\\(\\(value\\)\\|\\(token\\)\\|\\(exception\\)\\) = "
		    output)
      (cond
       ;; value
       ((match-beginning 2)
	(car (read-from-string (substring output (match-end 0)))))
       ;; token
       ((match-beginning 3)
	(cons guile-token-tag
	      (car (read-from-string (substring output (match-end 0))))))
       ;; exception
       ((match-beginning 4)
	(signal 'guile-error
		(car (read-from-string (substring output (match-end 0))))))))
     (t
      (error "Unsupported result" output)))))


;;;
;;; Guile Lisp adapter
;;;

(defvar guile-lisp-command "guile")
(defvar guile-lisp-adapter nil)

(defvar true "#t")
(defvar false "#f")

(defun guile-lisp-adapter ()
  (if (and (processp guile-lisp-adapter)
	   (eq (process-status guile-lisp-adapter) 'run))
      guile-lisp-adapter
    (setq guile-lisp-adapter
	  (guile:make-adapter guile-lisp-command 'emacs-lisp-channel))))

(defun guile-lisp-convert (x)
  (cond
   ((or (eq x true) (eq x false)) x)
   ((null x) "'()")
   ((keywordp x) (concat "#" (prin1-to-string x)))
   ((stringp x) (prin1-to-string x))
   ((guile-tokenp x) (cadr x))
   ((consp x)
    (if (null (cdr x))
	(list (guile-lisp-convert (car x)))
      (cons (guile-lisp-convert (car x)) (guile-lisp-convert (cdr x)))))
   (t x)))

;;;###autoload
(defun guile-lisp-eval (form)
  (guile:eval (format "%s" (guile-lisp-convert form)) (guile-lisp-adapter)))

(defun guile-lisp-flat-eval (&rest form)
  (let ((args (mapcar (lambda (x)
			(if (guile-tokenp x) (cadr x) (list 'quote x)))
		      (cdr form))))
    (guile-lisp-eval (cons (car form) args))))

;;;###autoload
(defmacro guile-import (name &optional new-name &rest opts)
  `(guile-process-import ',name ',new-name ',opts))

(defun guile-process-import (name new-name opts)
  (let ((real (or new-name name))
	(docs (if (memq :with-docs opts) true false)))
    (eval (guile-lisp-eval `(guile-emacs-export ',name ',real ,docs)))))

;;;###autoload
(defmacro guile-import-module (name &rest opts)
  `(guile-process-use-module ',name ',opts))

(defun guile-process-use-module (name opts)
  (unless (boundp 'guile-emacs-export-procedures)
    (guile-import guile-emacs-export-procedures))
  (let ((docs (if (memq :with-docs opts) true false)))
    (guile-lisp-eval `(use-modules ,name))
    (eval (guile-emacs-export-procedures name docs))
    name))


;;;
;;; Process handling
;;;

(defvar guile-process-output-start nil)
(defvar guile-process-output-value nil)
(defvar guile-process-output-finished nil)
(defvar guile-process-output-separator nil)

(defun guile-process-require (process string separator)
  (setq guile-process-output-value nil)
  (setq guile-process-output-finished nil)
  (setq guile-process-output-separator separator)
  (let (temp-buffer)
    (unless (process-buffer process)
      (setq temp-buffer (guile-temp-buffer))
      (set-process-buffer process temp-buffer))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert string)
      (setq guile-process-output-start (point))
      (set-process-filter process 'guile-process-filter)
      (process-send-string process string)
      (while (not guile-process-output-finished)
	(unless (accept-process-output process 3)
	  (when (> (point) guile-process-output-start)
	    (display-buffer (current-buffer))
	    (error "BUG in Guile object channel!!")))))
    (when temp-buffer
      (set-process-buffer process nil)
      (kill-buffer temp-buffer)))
  guile-process-output-value)

(defun guile-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (insert string)
    (forward-line -1)
    (if (< (point) guile-process-output-start)
	(goto-char guile-process-output-start))
    (when (re-search-forward guile-process-output-separator nil 0)
      (goto-char (match-beginning 0))
      (setq guile-process-output-value
	    (buffer-substring guile-process-output-start (point)))
      (setq guile-process-output-finished t))))

(defun guile-process-kill (process)
  (set-process-filter process nil)
  (delete-process process)
  (if (process-buffer process)
      (kill-buffer (process-buffer process))))

(provide 'guile)

;;; guile.el ends here
