;;;; hacks.el --- a few functions to help me work on the manual
;;;; Jim Blandy <jimb@red-bean.com> --- October 1998

(defun jh-exemplify-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)

      ;; Texinfo doesn't handle tabs well.
      (untabify (point-min) (point-max))

      ;; Quote any characters special to texinfo.
      (goto-char (point-min))
      (while (re-search-forward "[{}@]" nil t)
	(replace-match "@\\&")))))
