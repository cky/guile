;;; @(#) guile-init.el -- 
;;; @(#) $Keywords:  $

;; Copyright (C) 1995 Mikael Djurfeldt

;; LCD Archive Entry:
;; guile-init|djurfeldt@nada.kth.se| 
;; A GNU Emacs extension which |
;; $Date: 2003-08-20 19:00:44 $|$Revision: 1.1 $|~/misc/<package>.el.Z|

;; Author:  Mikael Djurfeldt <djurfeldt@nada.kth.se>
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Requirements:
;;
;; Usage:
;;
;; Bugs:
;;
;;

(defvar guile-init-load-hook nil
  "*Hook run when file is loaded")

(require 'guile)

;;; Misc. interactivity
;;;
;;;
(defun inda-barf-at-modifications (&rest args)
  (or inhibit-read-only
      (error "Attempt to modify read-only text")))

(defun inda-boldify-previous-character ()
  ;; Must check this so that we don't point outside buffer...
  (if (> (point) (point-min))
      (let ((inhibit-read-only t))
	(put-text-property (1- (point)) (point) 'face 'bold))))

(defun inda-make-input-memory (string)
  ;; If input consists of many lines, the read-only overlay will
  ;; cover the previous line, so we have to disable the protection.
  (let ((inhibit-read-only t))
      ;(setq n (1+ n)
      ;      l (append l (list (list n 'input-filter string))))
    (if (marker-position guile-last-output-end)
	(add-text-properties guile-last-output-end (1- (point))
			     '(input-memory t rear-nonsticky t mouse-face highlight)))))

(defun inda-reset-guile-last-output (string)
  ;(setq n (1+ n)
  ;      l (append l (list (list n 'output-filter string))))
  (if (not scheme-ready-p)
      (set-marker guile-last-output-end nil)))

(define-key inferior-scheme-mode-map [mouse-2] 'inda-mouse-yank-at-click)
(define-key inferior-scheme-mode-map [S-mouse-2] 'inda-mouse-yank-at-click)

;; Should rather be implemented with advice.
(defun inda-mouse-yank-at-click (click arg)
  "Insert the last stretch of killed text at the position clicked on.
Also move point to one end of the text thus inserted (normally the end).
Prefix arguments are interpreted as with \\[yank].
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e\nP")
  (if (get-char-property (posn-point (event-start click)) 'input-memory)
      (if (memq 'shift (event-modifiers (car click)))
	  (inda-insert-input-memory click)
	(inda-insert-input-memory-and-send click))
    ;; Give temporary modes such as isearch a chance to turn off.
    (run-hooks 'mouse-leave-buffer-hook)
    (or mouse-yank-at-point (mouse-set-point click))
    (setq this-command 'yank)
    (yank arg)))

(defun inda-insert-input-memory (event)
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
	 (beg (previous-single-property-change (1+ pos) 'mouse-face))
	 (end (next-single-property-change pos 'mouse-face)))
    (goto-char (point-max))
    (let ((input-start (point)))
      (comint-kill-input)
      (insert (buffer-substring beg end))
      (add-text-properties input-start (point)
			   '(mouse-face nil
					rear-nonsticky nil
					input-memory nil)))))

(defun inda-insert-input-memory-and-send (event)
  (interactive "e")
  (inda-insert-input-memory event)
  (guile-send-input))

(defun inda-boldify (string)
  (put-text-property comint-last-input-start (point) 'face 'bold))

(defun inda-extend-read-only-overlay (string)
  (if guile-input-sent-p
      (let ((inhibit-read-only t))
	(move-overlay inda-read-only-overlay (point-min) (point)))))

;;; Misc. utilities
;;;
(defun scheme-send-buffer ()
  "Send the current buffer to the inferior Scheme process."
  (interactive)
   (let (begin end)
     (save-excursion
       (goto-char (point-max))
       (setq end (point))
       (goto-char (point-min))
       (setq begin (point)))
     (scheme-send-region begin end)))

(defun indent-buffer ()
  "Indent entire buffer."
  (interactive)
  (save-excursion
   (end-of-buffer)
   (let ((end (point)))
     (beginning-of-buffer)
     (indent-region (point) end nil))))

(defun indent-defun ()
  "Indent lisp definition."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (indent-region (point) end nil))))

(provide 'guile-init)
(run-hooks 'guile-init-load-hook)
