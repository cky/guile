;;; docstring.el --- utilities for Guile docstring maintenance
;;;
;;; Copyright (C) 2001 Neil Jerram
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The basic premise of these utilities is that - at least in the
;; short term - we can get a lot of reference manual mileage by
;; co-opting the docstrings that are snarfed automatically from
;; Guile's C and Scheme source code.  But this leads to problems of
;; synchronization...  How do you track when a docstring has been
;; updated in the source and so needs updating in the reference
;; manual.  What if a procedure is removed from the Guile source?  And
;; so on.  To complicate matters, the exact snarfed docstring text
;; will probably need to be modified so that it fits into the flow of
;; the manual section in which it appears.  Can we design solutions to
;; synchronization problems that continue to work even when the manual
;; text has been enhanced in this way?
;;
;; This file implements an approach to this problem that I have found
;; useful.  It involves keeping track of three copies of each
;; docstring:
;;
;; "MANUAL"   = the docstring as it appears in the reference manual.
;;
;; "SNARFED"  = the docstring as snarfed from the current C or Scheme
;;              source.
;;
;; "TRACKING" = the docstring as it appears in a tracking file whose
;;              purpose is to record the most recent snarfed docstrings
;;              that are known to be in sync with the reference manual.
;;
;; The approaches are as follows.
;;
;; 1. Comparison of MANUAL-DOC, SOURCE-DOC and TRACK-DOC, to produce a
;; summary output buffer in which keystrokes are defined to bring up
;; detailed comparisons.
;;
;; 2. Comparison of MANUAL-DOC, SOURCE-DOC and TRACK-DOC using Ediff.

;;; Code:

(defvar guile-core-dir (or (getenv "GUILE_MAINTAINER_GUILE_CORE_DIR")
                           "~/Guile/cvs/guile-core"))

(defvar docstring-manual-directory (expand-file-name "doc/ref" guile-core-dir)
  "*The directory containing the Texinfo source for the Guile reference manual.")

(defvar docstring-tracking-root (expand-file-name "doc/maint" guile-core-dir)
  "*Root directory for docstring tracking files.  The tracking file
for module (a b c) is expected to be in the file
<docstring-tracking-root>/a/b/c.texi.")

(defvar docstring-snarfed-roots (mapcar
                                 #'(lambda (frag)
                                     (expand-file-name frag guile-core-dir))
                                 '("libguile" "ice-9" "oop"))
  "*List of possible root directories for snarfed docstring files.
For each entry in this list, the snarfed docstring file for module (a
b c) is looked for in the file <entry>/a/b/c.texi.")

(defvar docstring-manual-files
  (directory-files docstring-manual-directory nil "\\.texi$" t)
  "List of Texinfo source files that comprise the Guile reference manual.")

(defvar docstring-new-docstrings-file "new-docstrings.texi"
  "The name of a file in the Guile reference manual source directory
to which new docstrings should be added.")

;; Apply FN in turn to each element in the list CANDIDATES until the
;; first application that returns non-nil.
(defun or-map (fn candidates args)
  (let ((result nil))
    (while candidates
      (setq result (apply fn (car candidates) args))
      (if result
          (setq result (cons (car candidates) result)
                candidates nil)
        (setq candidates (cdr candidates))))
    result))

;; Return t if the current buffer position is in the scope of the
;; specified MODULE, as determined by "@c module ..." comments in the
;; buffer.  DEFAULT-OK specifies the return value in the case that
;; there are no preceding module comments at all.
(defun docstring-in-module (module default-ok)
  (save-excursion
    (if (re-search-backward "^@c module " nil t)
        (progn
          (search-forward "@c module ")
          (equal module (read (current-buffer))))
      default-ok)))

;; Find a docstring in the specified FILE-NAME for the item in module
;; MODULE and with description DESCRIPTION.  MODULE should be a list
;; of symbols, Guile-style, for example: '(ice-9 session).
;; DESCRIPTION should be the string that is expected after the @deffn,
;; for example "primitive acons" or "syntax let*".
(defun find-docstring (file-name module description)
  (and (file-exists-p file-name)
       (let ((buf (find-file-noselect file-name))
             (deffn-regexp (concat "^@deffnx? "
                                   (regexp-quote description)
                                   "[ \n\t]"))
             found
             result)
         (save-excursion
           (set-buffer buf)
           (goto-char (point-min))
           (while (and (not found)
                       (re-search-forward deffn-regexp nil t))
             (save-excursion
               (goto-char (match-beginning 0))
               (beginning-of-line)
               (if (docstring-in-module module t)
                   (setq found t))))
           (if found
               (setq result
                     (list (current-buffer)
                           (progn
                             (re-search-backward "^@deffn ")
                             (beginning-of-line)
                             (point))
                           (progn
                             (re-search-forward "^@end deffn")
                             (forward-line 1)
                             (point))))))
         result)))

;; Find the reference manual version of the specified docstring.
;; MODULE and DESCRIPTION specify the docstring as per
;; `find-docstring'.  The set of files that `find-manual-docstring'
;; searches is determined by the value of the `docstring-manual-files'
;; variable.
(defun find-manual-docstring (module description)
  (let* ((result
          (or-map 'find-docstring
                  (mapcar (function (lambda (file-name)
                                      (concat docstring-manual-directory
                                              "/"
                                              file-name)))
                          (cons docstring-new-docstrings-file
                                docstring-manual-files))
                  (list module
                        description)))
         (matched-file-name (and (cdr result)
                                 (file-name-nondirectory (car result)))))
    (if matched-file-name
        (setq docstring-manual-files
              (cons matched-file-name
                    (delete matched-file-name docstring-manual-files))))
    (cdr result)))

;; Convert MODULE to a directory subpath.
(defun module-to-path (module)
  (mapconcat (function (lambda (component)
                         (symbol-name component)))
             module
             "/"))

;; Find the current snarfed version of the specified docstring.
;; MODULE and DESCRIPTION specify the docstring as per
;; `find-docstring'.  The file that `find-snarfed-docstring' looks in
;; is automatically generated from MODULE.
(defun find-snarfed-docstring (module description)
  (let ((modpath (module-to-path module)))
    (cdr (or-map (function (lambda (root)
                             (find-docstring (concat root
                                                     "/"
                                                     modpath
                                                     ".texi")
                                             module
                                             description)))
                 docstring-snarfed-roots
                 nil))))

;; Find the tracking version of the specified docstring.  MODULE and
;; DESCRIPTION specify the docstring as per `find-docstring'.  The
;; file that `find-tracking-docstring' looks in is automatically
;; generated from MODULE.
(defun find-tracking-docstring (module description)
  (find-docstring (concat docstring-tracking-root
                          "/"
                          (module-to-path module)
                          ".texi")
                  module
                  description))

;; Extract an alist of modules and descriptions from the current
;; buffer.
(defun make-module-description-list ()
  (let ((alist nil)
        (module '(guile)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(@c module \\|@deffnx? \\({[^}]+}\\|[^ ]+\\) \\([^ \n]+\\)\\)"
                                nil
                                t)
        (let ((matched (buffer-substring (match-beginning 1)
                                         (match-end 1))))
          (if (string-equal matched "@c module ")
              (setq module (read (current-buffer)))
	    (let ((type (buffer-substring (match-beginning 2)
					  (match-end 2))))
	      (if (string-equal type "{C Function}")
		  nil
		(setq matched
		      (concat type
			      " "
			      (buffer-substring (match-beginning 3)
						(match-end 3))))
		(message "Found docstring: %S: %s" module matched)
		(let ((descriptions (assoc module alist)))
		  (setq alist
			(cons (cons module (cons matched (cdr-safe descriptions)))
			      (if descriptions
				  (delete descriptions alist)
				alist))))))))))
    alist))

;; Return the docstring from the specified LOCATION.  LOCATION is a
;; list of three elements: buffer, start position and end position.
(defun location-to-docstring (location)
  (and location
       (save-excursion
         (set-buffer (car location))
         (buffer-substring (cadr location) (caddr location)))))

;; Perform a comparison of the specified docstring.  MODULE and
;; DESCRIPTION are as per usual.
(defun docstring-compare (module description)
  (let* ((manual-location (find-manual-docstring module description))
         (snarf-location (find-snarfed-docstring module description))
         (track-location (find-tracking-docstring module description))

         (manual-docstring (location-to-docstring manual-location))
         (snarf-docstring (location-to-docstring snarf-location))
         (track-docstring (location-to-docstring track-location))

         action
         issue)

    ;; Decide what to do.
    (cond ((null snarf-location)
           (setq action nil
                 issue (if manual-location
                           'consider-removal
                         nil)))

          ((null manual-location)
           (setq action 'add-to-manual issue nil))

          ((null track-location)
           (setq action nil
                 issue (if (string-equal manual-docstring snarf-docstring)
                           nil
                         'check-needed)))

          ((string-equal track-docstring snarf-docstring)
           (setq action nil issue nil))

          ((string-equal track-docstring manual-docstring)
           (setq action 'auto-update-manual issue nil))

          (t
           (setq action nil issue 'update-needed)))

    ;; Return a pair indicating any automatic action that can be
    ;; taken, and any issue for resolution.
    (cons action issue)))

;; Add the specified docstring to the manual.
(defun docstring-add-to-manual (module description)
  (let ((buf (find-file-noselect (concat docstring-manual-directory
                                         "/"
                                         docstring-new-docstrings-file))))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-max))
      (or (docstring-in-module module nil)
          (insert "\n@c module " (prin1-to-string module) "\n"))
      (insert "\n" (location-to-docstring (find-snarfed-docstring module
                                                                  description))))))

;; Auto-update the specified docstring in the manual.
(defun docstring-auto-update-manual (module description)
  (let ((manual-location (find-manual-docstring module description))
        (track-location (find-tracking-docstring module description)))
    (save-excursion
      (set-buffer (car manual-location))
      (goto-char (cadr manual-location))
      (delete-region (cadr manual-location) (caddr manual-location))
      (insert (location-to-docstring (find-snarfed-docstring module
                                                             description))))))

;; Process an alist of modules and descriptions, and produce a summary
;; buffer describing actions taken and issues to be resolved.
(defun docstring-process-alist (alist)
  (let (check-needed-list
        update-needed-list
        consider-removal-list
        added-to-manual-list
        auto-updated-manual-list)

    (mapcar
     (function (lambda (module-list)
                 (let ((module (car module-list)))
                   (message "Module: %S" module)
                   (mapcar
                    (function (lambda (description)
                                (message "Comparing docstring: %S: %s" module description)
                                (let* ((ai (docstring-compare module description))
                                       (action (car ai))
                                       (issue (cdr ai)))

                                  (cond ((eq action 'add-to-manual)
                                         (docstring-add-to-manual module description)
                                         (setq added-to-manual-list
                                               (cons (cons module description)
                                                     added-to-manual-list)))

                                        ((eq action 'auto-update-manual)
                                         (docstring-auto-update-manual module description)
                                         (setq auto-updated-manual-list
                                               (cons (cons module description)
                                                     auto-updated-manual-list))))

                                  (cond ((eq issue 'check-needed)
                                         (setq check-needed-list
                                               (cons (cons module description)
                                                     check-needed-list)))

                                        ((eq issue 'update-needed)
                                         (setq update-needed-list
                                               (cons (cons module description)
                                                     update-needed-list)))

                                        ((eq issue 'consider-removal)
                                         (setq consider-removal-list
                                               (cons (cons module description)
                                                     consider-removal-list)))))))
                    (cdr module-list)))))
     alist)

    ;; Prepare a buffer describing the results.
    (set-buffer (get-buffer-create "*Docstring Results*"))
    (erase-buffer)

    (insert "
The following items have been automatically added to the manual in
file `" docstring-manual-directory "/" docstring-new-docstrings-file "'.\n\n")
    (if added-to-manual-list
	(mapcar (function (lambda (moddesc)
			    (insert (prin1-to-string (car moddesc))
                                    ": "
                                    (cdr moddesc)
                                    "\n")))
		added-to-manual-list)
      (insert "(none)\n"))

    (insert "
The following items have been automatically updated in the manual.\n\n")
    (if auto-updated-manual-list
	(mapcar (function (lambda (moddesc)
			    (insert (prin1-to-string (car moddesc))
                                    ": "
                                    (cdr moddesc)
                                    "\n")))
		auto-updated-manual-list)
      (insert "(none)\n"))

    (insert "
The following items are already documented in the manual but are not
mentioned in the reference copy of the snarfed docstrings file.
You should check that the manual documentation matches the docstring
in the current snarfed docstrings file.\n\n")
    (if check-needed-list
	(mapcar (function (lambda (moddesc)
			    (insert (prin1-to-string (car moddesc))
                                    ": "
                                    (cdr moddesc)
                                    "\n")))
		check-needed-list)
      (insert "(none)\n"))

    (insert "
The following items have manual documentation that is different from
the docstring in the reference copy of the snarfed docstrings file,
and the snarfed docstring has changed.  You need to update the manual
documentation by hand with reference to the snarfed docstring changes.\n\n")
    (if update-needed-list
	(mapcar (function (lambda (moddesc)
			    (insert (prin1-to-string (car moddesc))
                                    ": "
                                    (cdr moddesc)
                                    "\n")))
		update-needed-list)
      (insert "(none)\n"))

    (insert "
The following items are documented in the manual but are no longer
present in the snarfed docstrings file.  You should consider whether
the existing manual documentation is still pertinent.  If it is, its
docstring module comment may need updating, to connect it with a
new snarfed docstring file.\n\n")
    (if consider-removal-list
	(mapcar (function (lambda (moddesc)
			    (insert (prin1-to-string (car moddesc))
                                    ": "
                                    (cdr moddesc)
                                    "\n")))
		consider-removal-list)
      (insert "(none)\n"))
    (insert "\n")

    (goto-char (point-min))
    (local-set-key "d" 'docstring-ediff-this-line)

    ;; Popup the issues buffer.
    (let ((pop-up-frames t))
      (set-window-point (display-buffer (current-buffer))
			(point-min)))))

(defun docstring-process-current-buffer ()
  (interactive)
  (docstring-process-alist (make-module-description-list)))

(defun docstring-process-current-region (beg end)
  (interactive "r")
  (narrow-to-region beg end)
  (unwind-protect
      (save-excursion
        (docstring-process-alist (make-module-description-list)))
    (widen)))

(defun docstring-process-module (module)
  (interactive "xModule: ")
  (let ((modpath (module-to-path module))
        (mdlist nil))
    (mapcar (function (lambda (root)
                        (let ((fn (concat root
                                          "/"
                                          modpath
                                          ".texi")))
                          (if (file-exists-p fn)
                              (save-excursion
                                (find-file fn)
                                (message "Getting docstring list from %s" fn)
                                (setq mdlist
                                      (append mdlist
                                              (make-module-description-list))))))))
            docstring-snarfed-roots)
    (docstring-process-alist mdlist)))

(defun docstring-ediff-this-line ()
  (interactive)
  (let (module
        description)
    (save-excursion
      (beginning-of-line)
      (setq module (read (current-buffer)))
      (forward-char 2)
      (setq description (buffer-substring (point)
                                          (progn
                                            (end-of-line)
                                            (point)))))

    (message "Ediff docstring: %S: %s" module description)

    (let ((track-location (or (find-tracking-docstring module description)
                              (docstring-temp-location "No docstring in tracking file")))
          (snarf-location (or (find-snarfed-docstring module description)
                              (docstring-temp-location "No docstring in snarfed file")))
          (manual-location (or (find-manual-docstring module description)
                               (docstring-temp-location "No docstring in manual"))))

      (setq docstring-ediff-buffers
            (list (car track-location)
                  (car snarf-location)
                  (car manual-location)))

      (docstring-narrow-to-location track-location)
      (docstring-narrow-to-location snarf-location)
      (docstring-narrow-to-location manual-location)

      (add-hook 'ediff-quit-hook 'docstring-widen-ediff-buffers)

    (ediff-buffers3 (nth 0 docstring-ediff-buffers)
		    (nth 1 docstring-ediff-buffers)
		    (nth 2 docstring-ediff-buffers)))))

(defun docstring-narrow-to-location (location)
  (save-excursion
    (set-buffer (car location))
    (narrow-to-region (cadr location) (caddr location))))

(defun docstring-temp-location (str)
  (let ((buf (generate-new-buffer "*Docstring Temp*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert str "\n")
      (list buf (point-min) (point-max)))))

(require 'ediff)

(defvar docstring-ediff-buffers '())

(defun docstring-widen-ediff-buffers ()
  (remove-hook 'ediff-quit-hook 'docstring-widen-ediff-buffers)
  (save-excursion
    (mapcar (function (lambda (buffer)
			(set-buffer buffer)
			(widen)))
	    docstring-ediff-buffers)))


;;; Tests:

;(find-docstring "/home/neil/Guile/cvs/guile-core/doc/maint/guile.texi" nil "primitive sloppy-assq")
;(find-manual-docstring '(guile) "primitive sloppy-assq")
;(find-tracking-docstring '(guile) "primitive sloppy-assq")
;(find-snarfed-docstring '(guile) "primitive sloppy-assq")

(provide 'docstring)

;;; docstring.el ends here
