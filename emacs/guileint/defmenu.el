;;; @(#) defmenu.el -- A GNU Emacs extension which helps building menus
;;; @(#) $Keywords: X, menu $

;; Copyright (C) 1995 Mikael Djurfeldt

;; LCD Archive Entry:
;; defmenu|djurfeldt@nada.kth.se| 
;; A GNU Emacs extension which helps building menus|
;; $Date: 2003-08-20 19:00:44 $|$Revision: 1.1 $|~/misc/defmenu.el.Z|

;; Author:	 Mikael Djurfeldt <djurfeldt@nada.kth.se>
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

(defun define-menu (keymap key name entries)
  "Define a menu in KEYMAP on fake function key KEY with label NAME.
Every entry in the list ENTRIES defines a menu item and looks like this:

  (LABEL DEFINITION [ENABLE-EXP])

LABEL is a string which will appear in the menu.
DEFINITION is either a symbol, in which case it will be used both as
fake function key and binding, or a pair, where the car is the fake
function key and the cdr is the binding.
The optional ENABLE-EXP is an expression which will be evaluated every
time the menu is displayed.  If it returns nil the menu item will
be disabled.

You can get a separator by including nil in the ENTRIES list."
  (define-key keymap
    (vector 'menu-bar key)
    (cons name (make-menu name entries))))

(defun make-menu (name entries)
  "Make a menu with label NAME.
Every entry in the list ENTRIES defines a menu item and looks like this:

  (LABEL DEFINITION [ENABLE-EXP])

LABEL is a string which will appear in the menu.
DEFINITION is either a symbol, in which case it will be used both as
fake function key and binding, or a pair, where the car is the fake
function key and the cdr is the binding.
The optional ENABLE-EXP is an expression which will be evaluated every
time the menu is displayed.  If it returns nil the menu item will
be disabled.

You can get a separator by including nil in the ENTRIES list."
  (let ((menu (make-sparse-keymap name))
	(entries (reverse entries)))
    (while entries
      (let ((entry (car entries)))
	(if (null entry)
	    (define-key menu (vector (defmenu-gensym "separator")) '("--"))
	  (if (symbolp (nth 1 entry))
	      (define-key menu (vector (nth 1 entry))
		(cons (car entry) (nth 1 entry)))
	    (define-key menu (vector (car (nth 1 entry)))
	      (cons (car entry) (cdr (nth 1 entry)))))
	  (if (not (null (nthcdr 2 entry)))
	      (put (nth 1 entry) 'menu-enable (nth 2 entry)))))
      (setq entries (cdr entries)))
    menu))

(defun defmenu-gensym (prefix)
  (let ((counter (intern (concat "defmenu-" prefix "count"))))
    (if (boundp counter) (set counter (1+ (symbol-value counter)))
      (set counter 0))
    (intern (concat prefix (int-to-string (symbol-value counter))))))

(provide 'defmenu)
