;;; patch.el --- mail/apply a patch

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

;;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;;; Version: 1
;;; Favorite-Favorite: Favorite-Favorite

;;; Commentary:

;; This file has two symmetrical usage modes, for patch creation and
;; application, respectively.  The details are somewhat tuned for Guile
;; maintenance; probably we should generalize it a bit and add it to
;; Emacs proper at some point in the future.  Long live free software!
;;
;; On the patch creation side of things, there are various version
;; control systems that are happy to write a diff to stdout (and
;; numerous Emacs interfaces to them all).  Thus, we provide only a
;; simple `patch-send' that composes mail from the current buffer;
;; the contents of that buffer are left as an exercise for the patch
;; creator.  When preparing the mail, `patch-send' scans the patch
;; for standard filename headers and sets up a skeleton change log --
;; filling this in is a good way to earn respect from maintainers (hint
;; hint).  Type `C-c C-c' to send the mail when you are done.  (See
;; `compose-mail' for more info.)
;;
;; TODO: Write/document patch-apply side of things.
;; TODO: Integrate w/ `ediff-patch-buffer' et al.

;;; Code:

(require 'cl)
(require 'update-changelog)             ; for stitching

;; outgoing

(defvar patch-greeting "hello guile maintainers,\n\n"
  "*String to insert at beginning of patch mail.")

(defun patch-scan-files ()
  (let (files)
    (save-excursion
      (while (re-search-forward "^[+][+][+] \\(\\S-+\\)" (point-max) t)
        (setq files (cons (cons (match-string 1)
                                (match-beginning 0))
                          files))))
    (reverse files)))

(defun patch-common-prefix (filenames)
  (let* ((first-file (car filenames))
         (prefix (and first-file (file-name-directory first-file))))
    (while (and prefix
                (not (string= "" prefix))
                (not (every (lambda (filename)
                              (string-match (concat "^" prefix) filename))
                            filenames)))
      (setq prefix (file-name-directory (substring prefix 0 -1))))
    prefix))

(defun patch-changelog-skeleton ()
  (let* ((file-info (patch-scan-files))
         (fullpath-files (mapcar 'car file-info))
         (cut (length (patch-common-prefix fullpath-files)))
         (files (mapcar (lambda (fullpath-file)
                          (substring fullpath-file cut))
                        fullpath-files)))
    (mapconcat
     (lambda (file)
       (concat (make-string (length file) ?_) "\n" file "\n[writeme]"))
     files
     "\n")))

(defun patch-send (buffer subject)
  (interactive "bBuffer: \nsSubject: ")
  (when (string= "" subject)
    (error "(empty subject)"))
  (compose-mail "bug-guile@gnu.org" subject)
  (insert (with-current-buffer buffer (buffer-string)))
  (mail-text)
  (insert patch-greeting)
  (save-excursion
    (insert "here is a patch ... [overview/observations/etc]\n\n"
            (patch-changelog-skeleton) "\n\n\n"
            (make-string 72 ?_) "\n")))

;; incoming


  

;;; patch.el ends here
