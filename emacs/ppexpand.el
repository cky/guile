;;; ppexpand.el --- temporarily expanding macros in a pretty way.

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
(require 'cmacexp)

(defvar if-mark "#if 0 /* PPEXPAND */")
(defvar else-mark "#else /* PPEXPAND */")
(defvar endif-mark "#endif /* PPEXPAND */")

(define-key c-mode-map "\C-ce" 'ppexpand)

(defun ppexpand (start end &optional undo)
  "Expand C macros in the region, using the C preprocessor.
The expanded code is run through the `indent' command and inserted
into the program next to the original code, using an #if/#else/#endif
construct.

Given a prefix argument, it reverts the change, removing the
#if/#else/#endif construct and the expanded code.

`c-macro-preprocessor' specifies the preprocessor to use.
Prompt for arguments to the preprocessor \(e.g. `-DDEBUG -I ./include')
if the user option `c-macro-prompt-flag' is non-nil.

Noninteractive args are START, END, UNDO.
For use inside Lisp programs, see also `c-macro-expansion'."

  (interactive (if current-prefix-arg
		   (list nil nil t)
		 (let ((pos1 (point))
		       (pos2 (mark)))
		   (if (< pos1 pos2)
		       (list pos1 pos2 nil)
		     (list pos2 pos1 nil)))))
  (let ((inbuf (current-buffer)))
    ;; Build the command string.
    (if c-macro-prompt-flag
	(setq c-macro-cppflags
	      (read-string "Preprocessor arguments: "
			   c-macro-cppflags)))
    (if undo
	(let ((pos (point)) if-pos else-pos endif-pos)
	  (save-excursion
	    (end-of-line)
	    (if (not (and (setq if-pos (search-backward if-mark nil t))
			  (setq else-pos (search-forward else-mark nil t))
			  (setq endif-pos (search-forward endif-mark nil t))
			  (<= if-pos pos)
			  (< pos endif-pos)))
		(error "Not in ppexpanded region"))
	    (let ((orig (buffer-substring (+ if-pos (length if-mark) 1)
					  (- else-pos (length else-mark)))))
	      (delete-region if-pos (+ endif-pos 1))
	      (insert orig))))
      ;; Expand the macro.
      (let* ((expansion (c-macro-expansion start end
					   (concat c-macro-preprocessor " "
						   c-macro-cppflags) t))
	     (orig (buffer-substring start end)))
	(setq expansion
	      (with-temp-buffer
		(insert expansion)
		(call-process-region (point-min) (point-max) "indent"
				     t		;delete the text
				     t		;output --> current buffer
				     )
		(buffer-string)))
	(delete-region start end)
	(insert if-mark ?\n orig else-mark ?\n expansion endif-mark ?\n)))))
