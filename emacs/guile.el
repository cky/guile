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

(defun guile:make-adapter (command)
  (let* ((buff (generate-new-buffer " *guile object channel*"))
	 (proc (start-process "guile-oa" buff command "-q")))
    (process-kill-without-query proc)
    (process-send-string proc "(use-modules (guile channel))\n")
    (process-send-string proc "(open-object-channel)\n")
    (accept-process-output proc)
    proc))

(defun guile:eval (exp adapter)
  (let ((str (format "eval %S\n" exp)))
    (guile-process-require adapter str "channel> " 'guile:eval-filter)))

(defun guile:eval-filter (proc)
  (cond
   ((looking-at "value = ")
    (car (read-from-string (buffer-substring (match-end 0) (point-max)))))
   ((looking-at "token = ")
    (caar (read-from-string (buffer-substring (match-end 0) (point-max)))))
   ((looking-at "exception = ")
    (apply 'signal (car (read-from-string
			 (buffer-substring (match-end 0) (point-max))))))
   (t
    (error "Unsupported result"))))

;;;
;;; Process handling
;;;

(defvar guile-process-output-start nil)
(defvar guile-process-output-value nil)
(defvar guile-process-output-filter nil)
(defvar guile-process-output-finished nil)
(defvar guile-process-output-separator nil)
(defvar guile-process-output-separator-lines 2)

(defun guile-process-require (process string separator &optional filter)
  (setq guile-process-output-value nil)
  (setq guile-process-output-filter filter)
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
	(unless (accept-process-output process 5)
	  (when (> (point) guile-process-output-start)
	    (display-buffer (current-buffer))
	    (error "BUG in the filter!!")))))
    (when temp-buffer
      (set-process-buffer process nil)
      (kill-buffer temp-buffer)))
  guile-process-output-value)

(defun guile-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (insert string)
    (forward-line (- guile-process-output-separator-lines))
    (if (< (point) guile-process-output-start)
	(goto-char guile-process-output-start))
    (when (re-search-forward guile-process-output-separator nil 0)
      (goto-char (match-beginning 0))
      (if guile-process-output-filter
	  (save-current-buffer
	    (narrow-to-region guile-process-output-start (point))
	    (goto-char (point-min))
	    (setq guile-process-output-value
		  (funcall guile-process-output-filter process))
	    (widen))
	(setq guile-process-output-value
	      (buffer-substring guile-process-output-start (point))))
      (setq guile-process-output-finished t))))

(defun guile-process-kill (process)
  (set-process-filter process nil)
  (delete-process process)
  (if (process-buffer process)
      (kill-buffer (process-buffer process))))

(provide 'guile)

;;; guile.el ends here
