;;; multistring.el --- editing multiline strings.

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;;; Author: Mikael Djurfeldt <djurfeldt@nada.kth.se>

;;; Commentary:

;; Commands for editing multiline ANSI C compatible string literals.

;;; Code:

(defun ms-ansify-string (arg)
    "Convert a string literal spanning multiple lines into multiple literals.
With no argument, convert the single string at point to multiple strings.
With an argument, convert multiple strings to a single one.

ANSI C doesn't allow a string literal to span multiple lines.  This
function makes editing of multiline strings easier.

The programmer can edit a string spanning multiple lines and then use
this function to convert it into multiple literals representing the
original string through the ANSI C string concatenation feature:

  \"A string consisting of
  multiple
  lines.\"

is converted into

  \"A string consisting of\n\"
  \"multiple\n\"
  \"lines\""
  (interactive "*P")
  (save-excursion
    (let (beg end)
      (save-restriction
	(ms-narrow-canonicalize-ansi-c-string)
	(if (not arg)
	    (ms-break-string))
	(setq beg (point-min))
	(setq end (point-max)))
      (if (not arg)
	  (c-indent-region beg end)))))

(defun ms-pack-region (from to &optional unpack-flag)
  "Pack paragraphs into single lines and remove one newline after paragraphs.
With no argument, do the conversion.
With an argument, do the reverse.

When doing the reverse conversion, \\[fill-region] is used to break up
the text into multiple lines."
  (interactive "*r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (if (not unpack-flag)
	  (ms-pack-buffer)
	(ms-unpack-buffer)))))

(defun ms-pack-ansify-string (arg)
  "Pack text in a string literal and convert into multiple literals.
With no argument, do the conversion.
With an argument, do the reverse.

This command has the combined effect of \\[ms-pack-region] and
\\[ms-ansify-string].  It is typically used if you want to store
entire paragraphs without newlines in an ANSI C literal, but want to
split it into multiple literals in order for the program text to look
sensible.  Using the reverse command, you can \"unpack\" the text,
edit it and repack the text using the forward conversion."
  (interactive "*P")
  (save-excursion
    (let (beg end)
      (save-restriction
	(ms-narrow-canonicalize-ansi-c-string)
	(if (not arg)
	    (ms-break-string " " "" "\\n")
	  (ms-unpack-buffer))
	(setq beg (point-min))
	(setq end (point-max)))
      (if (not arg)
	  (c-indent-region beg end)))))

(defun ms-pack-buffer ()
  "Pack paragraphs into single lines and remove one newline after paragraphs."
  (interactive "*")
  (goto-char (point-min))
  (skip-chars-forward "^\n")
  (while (not (eobp))
    (delete-char 1)
    (skip-chars-forward "\n")
    (skip-chars-forward "^\n")))

(defun ms-unpack-buffer ()
  "Break single lines into paragraphs and add an extra newline between each."
  (interactive "*")
  (goto-char (point-min))
  (skip-chars-forward "^\n")
  (while (not (eobp))
    (insert ?\n)
    (skip-chars-forward "\n")
    (skip-chars-forward "^\n"))
  (fill-region (point-min) (point-max) nil t))

(defconst ms-whitespace " \t\n")
(defconst ms-string-beginning "\"")
(defconst ms-string-end "\\(\\\\*\\)\"")
(defconst ms-quoted-newline "\\(\\\\*\\)\\(\\\\n\\)")

(defun ms-in-string-p ()
  (eq (c-in-literal) 'string))

(defun ms-narrow-canonicalize-ansi-c-string ()
  ;; Find and check reference point
  (cond ((ms-in-string-p))
	((eq (char-after) ?\") (forward-char))
	(t (error "Not in string.")))
  (set-mark (point))
  ;; Find beginning
  (ms-beginning-of-string)
  (let ((beg (point)))
    ;; Extend string backwards
    (while (ms-extend-backwards)
      (setq beg (point)))
    (goto-char (mark))
    ;; Find end
    (ms-end-of-string)
    (let ((end (point)))
      ;; Extend string forwards
      (while (ms-extend-forwards)
	(setq end (point)))
      ;; Narrow
      (narrow-to-region beg end)
      ;; Convert \n into explicit newlines
      (ms-convert-quoted-newlines))))

(defun ms-beginning-of-string ()
  (let ((pos (search-backward ms-string-beginning nil t)))
    (while (and pos
		(char-before)
		(eq (char-before) ?\\))
      (setq pos (search-backward ms-string-beginning nil t)))
    (if pos
	(progn
	  (forward-char)
	  (1+ pos)))))

(defun ms-extend-backwards ()
  (let ((end (point)))
    (backward-char)
    (skip-chars-backward ms-whitespace)
    (if (eq (char-before) ?\")
	(progn
	  (backward-char)
	  (delete-region (point) end)
	  (ms-beginning-of-string)))))

(defun ms-end-of-string ()
  (let ((pos (search-forward-regexp ms-string-end nil t)))
    (while (and pos (= (logand (- (match-end 1) (match-beginning 1)) 1) 1))
      (setq pos (search-forward-regexp ms-string-end nil t)))
    (if pos
	(progn
	  (backward-char)
	  (match-end 1)))))

(defun ms-extend-forwards ()
  (let ((start (point)))
    (forward-char)
    (skip-chars-forward ms-whitespace)
    (if (eq (char-after) ?\")
	(progn
	  (forward-char)
	  (delete-region start (point))
	  (ms-end-of-string)))))

(defun ms-convert-quoted-newlines ()
  (goto-char (point-min))
  (while (search-forward-regexp ms-quoted-newline nil t)
    (if (= (logand (- (match-end 1) (match-beginning 1)) 1) 0)
	(replace-match "\n" nil t nil 2))))

(defun ms-break-string (&optional single-term multi-term-1 multi-term-n)
  (let ((single-term (or single-term "\\n"))
	(multi-term-1 (or multi-term-1 "\\n"))
	(multi-term-n (or multi-term-n "\\n")))
    (goto-char (point-min))
    (skip-chars-forward "^\n")
    (while (not (eobp))
      (delete-char 1)
      (if (not (eq (char-after) ?\n))
	  (insert single-term)
	(insert multi-term-1)
	(while (eq (char-after) ?\n)
	  (delete-char 1)
	  (insert multi-term-n)))
      (insert "\"\n\"")
      (skip-chars-forward "^\n"))))

(eval-after-load "cc-mode"
  (progn
    (define-key c-mode-base-map "\C-ca" 'ms-ansify-string)
    (define-key c-mode-base-map "\C-cd" 'ms-pack-ansify-string)
    ))
