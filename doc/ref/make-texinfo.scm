;; make-texinfo.scm -- document a set of scheme modules as texinfo
;; Copyright (C) 2006,2007,2009  Andy Wingo <wingo at pobox dot com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (texinfo reflection)
             (texinfo serialize))

(define (main config-scm)
  (load config-scm)
  (display
   (stexi->texi
    (package-stexi-documentation-for-include
     (map car *modules*)
     (map cdr *modules*)))))

(apply main (cdr (command-line)))
