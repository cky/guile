;;; update-changelog.el --- stitch rcs2log output to ChangeLog

;;; Copyright (C) 2001 Free Software Foundation, Inc.

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

;; Usage: emacs -batch -l update-changelog.el
;;
;; This program is basically a wrapper around rcs2log, and inherits rcs2log's
;; weaknesses, namely, the requirement that there be a checked out (working
;; directory) copy.  It would be nice if rcs2log grokked with the repository
;; directly, but until then, we work around it by requiring the environment
;; var `LOCAL_WORK_ROOT' to be defined.  This should be a directory under
;; which cvs modules are checked out.
;;
;; Flash!  Newer versions of rcs2log do indeed understand the repository,
;; and can be invoked with "-R" therein.  We infer this if `LOCAL_WORK_ROOT'
;; is not set, and use instead `CVSROOT'.  At least one of these must be set.
;;
;; You can pass additional options to rcs2log using env var `RCS2LOG_OPTS'.
;;
;; Usage from a Lisp program:
;;   (ucl-update filename) -- Update FILENAME, a Change Log file

;;; Code:

;;;---------------------------------------------------------------------------
;;; Variables

(defvar ucl-o (or (getenv "RCS2LOG_OPTS") "")
  "Additional options to pass to rcs2log.")

;;;---------------------------------------------------------------------------
;;; Cleanup functions

(defun ucl-stitch-new-old (new-old &rest ignore)
  "In a changelog buffer, remove redundancy around NEW-OLD point.
The new text is before NEW-OLD point, and the old after."
  (goto-char new-old)
  (or (= new-old (point-max))		; no old
      (let ((last-new
	     (save-excursion
	       (buffer-substring (re-search-backward "^[0-9]+") new-old))))
	(let ((has-diff (string-match "\n\tdiff.*-r" last-new))) ; ugh
	  (and has-diff (setq last-new (substring last-new 0 has-diff))))
	(let ((overlap (search-forward last-new (point-max) t)))
	  (and overlap (delete-region new-old overlap))))))

;; Sometimes wannabe developers append diffs to their log entries.
(defun ucl-omit-diffs (&rest ignore)
  "In a changelog buffer, delete diffs (assumed at end of entry)."
  (goto-char (point-min))
  (while (re-search-forward "^\tdiff .*-r" (point-max) t)
    (beginning-of-line)
    (delete-region (point)
		   (save-excursion
		     (if (re-search-forward "^[0-9]+" (point-max))
			 (- (point) 4)
		       (point-max))))))

(defun ucl-space-out-entries (&rest ignore)
  "In a changelog buffer, ensure proper spacing between entries."
  (goto-char (point-max))
  (while (re-search-backward "^[0-9]+" (point-min) t)
    (unless (= (point) (point-min))
      (open-line 3)			; yuk
      (delete-blank-lines))))

(defun ucl-kill-eol-white-space (&rest ignore)
  "In a changelog buffer, delete end-of-line white space."
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" (point-max) t)
    (delete-region
     (match-beginning 0) (match-end 0))))

(defvar ucl-cleanup-hook '(ucl-stitch-new-old
			   ucl-omit-diffs
			   ucl-space-out-entries
			   ucl-kill-eol-white-space)
  "Hook run after combining the new fragment with the old changelog.  These
are called with the argument NEW-OLD, which is the buffer position at the
boundary of the two pieces of text.  This is suboptimal; we should use a
marker so that munges on the text do not lose this position.  The result is
that currently, `ucl-stitch-new-old' must be called first because it depends
on NEW-OLD, while the other cleanup funcs ignore it.  (Sigh.)")

;;;---------------------------------------------------------------------------
;;; Update functions

(defun ucl-root ()
  (let ((lwr (getenv "LOCAL_WORK_ROOT"))
	(cr  (getenv "CVSROOT")))
    (concat (or lwr
		(and cr (progn
			  (setq ucl-o (concat "-R " ucl-o))	; hmm
			  cr))
		(error "Must set env var LOCAL_WORK_ROOT or CVSROOT"))
	    "/")))

(defun ucl-update (filename)
  (interactive "fChangeLog: ")
  (let* ((ofile (expand-file-name filename))
         (cmd   (concat "rcs2log " ucl-o " -c " ofile))
         (obuf  "*ucl-work*"))
    (when (and (file-exists-p ofile)
	       (progn
		 (shell-command cmd obuf)
		 (get-buffer obuf)))
      (save-excursion			; prevent default-directory hosing
	(set-buffer obuf)
	(unless (= 0 (buffer-size))
	  (let ((new-old-boundary (point-max)))
	    (goto-char new-old-boundary)
	    (insert-file ofile)
	    (run-hook-with-args 'ucl-cleanup-hook new-old-boundary))
	  (or (= (buffer-size) (nth 7 (file-attributes ofile)))
	      (let (make-backup-files)	; less clutter
		(write-file ofile))))
	(kill-buffer (current-buffer))))))

;;;---------------------------------------------------------------------------
;;; Load-time actions

(when noninteractive			; only when `-batch'
  (or (ucl-update "ChangeLog")
      (message "Sorry, could not update ChangeLog in %s" default-directory)))

(provide 'update-changelog)

;;; update-changelog.el ends here
