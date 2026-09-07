;;; tjf-sort.el --- Sort submenu definition and associated functions -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2026 Tom Fontaine

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

(defvar tjf:sort/menu)
(setq tjf:sort/menu
  '("Sort"
    ["Sort"                   tjf:sort/alpha         :key-sequence nil :enable '(tjf:flags/is-rw?)]
    ["Sort by Fields"         tjf:sort/alpha-field   :key-sequence nil :enable '(tjf:flags/is-rw?)]
    ["---" nil :visible t :enable nil]
    ["Sort Numeric"           tjf:sort/numeric       :key-sequence nil :enable '(tjf:flags/is-rw?)]
    ["Sort by Fields Numeric" tjf:sort/numeric-field :key-sequence nil :enable '(tjf:flags/is-rw?)]
    ["---" nil :visible t :enable nil]
    ["Reverse"                tjf:sort/reverse       :key-sequence nil :enable '(tjf:flags/is-rw?)]))

(defun tjf:sort/reverse ()
  "Reverse lines in buffer or region"
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (reverse-region beg end)))

(defun tjf:sort/alpha ()
  "Sort alpabetically."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (tjf:sort/fields 1 beg end)))

(defun tjf:sort/alpha-field ()
  "Sort alpabetically by field."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (tjf:sort/fields 0 beg end)))

(defun tjf:sort/numeric ()
  "Sort numerically by field."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (tjf:sort/fields-numeric 1 beg end)))

(defun tjf:sort/numeric-field ()
  "Sort numerically by field."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (tjf:sort/fields-numeric 0 beg end)))

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

;;
(message "Loading tjf-sort...done")
(provide 'tjf-sort)

;;; tjf-sort.el ends here
