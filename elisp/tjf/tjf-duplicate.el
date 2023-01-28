;;; tjf-duplicate.el --- Duplicate text from surrounding lines -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2005-2023 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   12-Jan-2005

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

;; Revision: 02-Apr-2015 Added loading messages
;;           06-May-2015 Removed ‘duplicate-previous-N’, ‘duplicate-next-N’ functions
;;           26-Feb-2016 Refactored ‘duplicate’ to better deal with TABs
;;                       Added ‘duplicate-line-or-region’
;;           03-Mar-2016 Fixed bug in ‘duplicate-line-or-region’
;;           29-Apr-2016 Added ‘duplicate-as-comment’
;;           03-Feb-2021 ‘tjf’ overhaul
;;           28-Apr-2022 Added ‘tjf:duplicate/tabs’
;;

;;; Code:

(message "Loading tjf-duplicate...")
(require 'tjf-flags)
(require 'tjf-tools)
(require 'undo-tree)

;;
(defun tjf:duplicate/as-comment ()
  "Create a commented copy of the current region (or line
if no region is active). This is handy for testing out temporary
changes to a section of code while keeping the original version
as a comment for reference."
  (interactive)
  (let* ((begin (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end   (if (use-region-p) (region-end)       (line-beginning-position 2)))
         (text  (buffer-substring begin end)))
    (save-excursion
      (goto-char begin)
      (insert text))
    (if (equal (point) begin)
        (goto-char end))
    (comment-region begin end)))

(defun tjf:duplicate/line-or-region ()
  "Duplicate the current line or region."
  (interactive "*")
  (with-line-or-region (beg end)
   (let ((reg (buffer-substring-no-properties beg end))
         (deactivate-mark nil))
     (goto-char end)
     (insert reg))))

(defun tjf:duplicate/previous ()
  "Duplicates a word of the previous line's text starting from the current
column.  There is no effect when the current column exceeds the width of the
previous line.  This function does not work on the first line of a buffer.  This
function does not work well when the duplicated line contains TABs."
  (interactive "*")
  (if tjf:flags/using-tabs
      (tjf:duplicate/tabs -1)
    (tjf:duplicate/syntax -1)))

(defun tjf:duplicate/next ()
  "Duplicates a word of the next line's text starting from the current column.
There is no effect when the current column exceeds the width of the next line.
This function does not work on the last line of a buffer.  This function does
not work well when the duplicated line contains TABs."
  (interactive "*")
  (if tjf:flags/using-tabs
      (tjf:duplicate/tabs 1)
    (tjf:duplicate/syntax 1)))

(defun tjf:duplicate/syntax (dup-offset)
  "Duplicate a word of the DUP-OFFSET line's text starting from the current
column.  There is no effect when the current column exceeds the width of the
duplicated line.  This function does not work when the ‘dup-offset’ would be
prior to ‘point-min’ or after ‘point-max’.  This function does not replicate
TABs but will re-create the equivalent amount of whitespace."
  (if (< dup-offset 0)
      (if (< (+ (tjf:tools/current-line) dup-offset) 0)
          (error "Can't go back that many lines"))
    (let* ((beg-line (line-number-at-pos nil))
           (dup-line (+ beg-line dup-offset))
           (end-line (save-excursion (goto-char (point-max)) (line-number-at-pos nil))))
      (if (>= dup-line end-line)
          (error "Can't go forward that many lines"))))
  (let ((column-limit (current-column))
        (dup-string   nil)
        (tabs?        nil)
        (dupbeg-point nil))
    (save-excursion
      (forward-line dup-offset)
      (setq tabs? (tjf:flags/is-tabs-on-line?))
      (when tabs?
        (untabify (line-beginning-position) (line-end-position)))
      (setq column-limit (+ (point) column-limit))
      (skip-chars-forward "^\n" column-limit)
      (setq dupbeg-point (point))
      (skip-syntax-forward "w")
      (skip-syntax-forward "^w" (line-end-position))
      (setq dup-string (buffer-substring-no-properties dupbeg-point (point)))
      (when tabs?
        (undo-tree-undo)))
    (insert dup-string)))

(defun tjf:duplicate/tabs (dup-offset)
  "Duplicate a word of the DUP-OFFSET line's text starting from the current
column using TABs."
  (interactive "P*")
  (if (< dup-offset 0)
      (if (< (+ (tjf:tools/current-line) dup-offset) 0)
          (error "Can't go back that many lines"))
    (let* ((beg-line (line-number-at-pos nil))
           (dup-line (+ beg-line dup-offset))
           (end-line (save-excursion (goto-char (point-max)) (line-number-at-pos nil))))
      (if (>= dup-line end-line)
          (error "Can't go forward that many lines"))))
  (let ((column-limit (current-column))
        (dup-string   nil)
        (dupbeg-point nil))
    (save-excursion
      (forward-line dup-offset)
      (move-to-column column-limit)
      (setq dupbeg-point (point))
      (skip-syntax-forward "w")
      (skip-syntax-forward "^w" (line-end-position))
      (setq dup-string (buffer-substring-no-properties dupbeg-point (point))))
    (insert dup-string)))

;;
(message "Loading tjf-duplicate...done")
(provide 'tjf-duplicate)

;;; tjf-duplicate.el ends here
