;;; gud-guile.el --- Support for debugging guile internals

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

;; This is a grab bag of stuff for doing "gdb guile" in Emacs.
;; The var `gdb-guile-suggested-gdbinit' has a string that is
;; snarfed from ../HACKING.  (todo: Write `gdb-guile-init' to
;; send it to gdb...)

;;; Code:

(require 'cl)

(defun gdb-guile-display-scm ()
  (interactive)
  (save-excursion
    (let ((sym (thing-at-point 'symbol))
          (proc (get-buffer-process
                 (find-if (lambda (buf)
                            (string-match "^.gud-." (buffer-name buf)))
                          (buffer-list)))))
      (mapc (lambda (template)
              (process-send-string proc (format template sym)))
            (list
             "set gdb_print(%s)\n"
             "printf \"%s: %%s\\n\", gdb_output\n")))))

(defvar gdb-guile-suggested-gdbinit "
define gp
set gdb_print($arg0)
print gdb_output
end
document gp
Executes (object->string arg)
end

define ge
call gdb_read($arg0)
call gdb_eval(gdb_result)
set gdb_print(gdb_result)
print gdb_output
end
document ge
Executes (print (eval (read arg))): ge \"(+ 1 2)\" => 3
end

define gh
call g_help(scm_str2symbol($arg0), 20)
set gdb_print($1)
print gdb_output
end
document gh
Prints help string for arg: gh \"enved-target\"
end
"
  "A useful .gdbinit")

(provide 'gud-guile)

;;; gud-guile.el ends here
