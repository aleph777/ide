;;; tjf-sort.el --- Sort submenu definition and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2024 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   15-Dec-1999

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software",
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; Except as contained in this notice, the name(s of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.

;; The software is provided "As Is", without warranty of any kind, express or
;; implied, including but not limited to the warranties of merchantability,
;; fitness for a particular purpose and noninfringement. In no event shall
;; the authors or copyright holders be liable for any claim, damages or other
;; liability, whether in an action of contract, tort or otherwise, arising
;; from, out of or in connection with the software or the use or other
;; dealings in the software.

;;; Commentary:

;; Revision: 02-Feb-2010 added exit message
;;           29-Feb-2016 changed from `usr-' to `u-'
;;           03-Feb-2021 ‘tjf’ overhaul
;;           28-Jun-2023 fixed missing ‘interactive’
;;

;;; Code:

(message "Loading tjf-sort...")
(eval-when-compile
  (require 'sort)
  (require 'tjf-macro))

;;
(defvar tjf:sort/order-list (list  '"Sort in which order?"
                                   '("Ascending" . nil)
                                   '("Descending" . t)))

(defvar tjf:sort/field-list (list  '"Sort by which field?"
                                   '("1"  .  1) '("2"  .  2) '("3"  .  3) '("4"  .  4)
                                   '("-4" . -4) '("-3" . -3) '("-2" . -2) '("-1" . -1)))

(defun tjf:sort/do (field)
  "Sort by FIELD."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (tjf:sort/fields field beg end)))

(defun tjf:sort/do-numeric (field)
  "Sort numerically by FIELD."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (tjf:sort/fields-numeric field beg end)))

(defun tjf:sort/fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each
line.  Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are four arguments: REVERSE, FIELD, BEG
and END.  BEG and END specify region to sort."
  (require 'sort)
  (if (zerop field)
      (setq field (x-popup-dialog t tjf:sort/field-list)))
  (tjf:sort/fields-helper (x-popup-dialog t tjf:sort/order-list) beg end
                   (function (lambda () (sort-skip-fields field) nil))
                   (function (lambda () (skip-chars-forward "^ \t\n")))))

(defun tjf:sort/fields-helper (reverse beg end startkeyfun endkeyfun)
  "Sort helper function."
  (let ((tbl (syntax-table)))
    (unwind-protect
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-min))
            (set-syntax-table sort-fields-syntax-table)
            (sort-subr reverse 'forward-line 'end-of-line startkeyfun endkeyfun)))
      (set-syntax-table tbl))))

(defun tjf:sort/fields-numeric (field beg end)
  "Sort lines in region numericically by the ARGth field of each
line.  Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are four arguments: REVERSE, FIELD, BEG
and END.  BEG and END specify region to sort."
  (require 'sort)
  (if (zerop field)
      (setq field (x-popup-dialog t tjf:sort/field-list)))
  (tjf:sort/fields-helper (x-popup-dialog t tjf:sort/order-list) beg end
                   (function (lambda () (sort-skip-fields field)
                               (string-to-number (buffer-substring (point) (save-excursion (forward-sexp 1) (point))))))
                   (function (lambda () (skip-chars-forward "^ \t\n")
                               (string-to-number (buffer-substring (point) (save-excursion (forward-sexp 1) (point))))))))

(defvar tjf:sort/menu
  '("Sort"
    ["Sort Buffer"                   (tjf:sort/do         1) :key-sequence nil]
    ["Sort Buffer Numeric"           (tjf:sort/do-numeric 1) :key-sequence nil]
    ["Sort Buffer by Fields"         (tjf:sort/do         0) :key-sequence nil]
    ["Sort Buffer by Fields Numeric" (tjf:sort/do-numeric 0) :key-sequence nil]
    ["Reverse Buffer"                (reverse-region      (point-min) (point-max)) :key-sequence nil]
    "---"
    ["Sort Region"                   (tjf:sort/do         1) :active mark-active :key-sequence nil]
    ["Sort Region Numeric"           (tjf:sort/do-numeric 1) :active mark-active :key-sequence nil]
    ["Sort Region by Fields"         (tjf:sort/do         0) :active mark-active :key-sequence nil]
    ["Sort Region by Fields Numeric" (tjf:sort/do-numeric 0) :active mark-active :key-sequence nil]
    ["Reverse Region"                reverse-region          :active mark-active :key-sequence nil]))

;;
(message "Loading tjf-sort...done")
(provide 'tjf-sort)

;;; tjf-sort.el ends here
