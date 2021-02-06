;;; tjf-navigate.el --- Navigation menu and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2021 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   25-Feb-2016

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

;; Revision: 03-Mar-2016 Added ‘u-forward-symbol’ and ‘backward-symbol’
;;           16-Jan-2017 Added ‘search-word-forward’ and ‘search-word-backward’
;;           13-Jun-2018 Changed ‘atim-unscroll’ requirement to use ‘eval-when-compile’
;;           22-Jul-2019 Added Bookmarks menu
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-navigate...")
(require 'tjf-bookmark)

;;
(defvar tjf:navigate/saved-point nil "Saved value of point for current buffer.")
(make-variable-buffer-local 'tjf:navigate/saved-point)

;; these should go in tjf-search
(defalias 'search-word-forward  'tinysearch-search-word-forward)
(defalias 'search-word-backward 'tinysearch-search-word-backward)

(defun tjf:navigate/backward-symbol ()
  "Move the cursor backward to the beginning of the previous symbol."
  (interactive)
  (forward-symbol -1))

(defun tjf:navigate/forward-symbol ()
  "Move the cursor forward to the beginning of the next symbol."
  (interactive)
  (re-search-forward "\\(\\sw\\|\\s_\\)+" nil 'move)
  (skip-syntax-forward "^w"))

(defun tjf:navigate/forward-word ()
  "Move the cursor forward to the beginning of the next word."
  (interactive)
  (skip-syntax-forward "w")
  (skip-syntax-forward "^w"))

(defun tjf:navigate/goto-beginning-of-line ()
  "Go to the `beginning-of-line'."
  (interactive)
  (unless (bolp)
    (let ((orig-point (point)))         ; go to first non-whitespace character
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1)))))  ; then go to bol

(defun tjf:navigate/goto-end-of-line ()
  "Go to the ‘end-of-line’ position."
  (interactive)
  (unless (eolp)
    (let ((orig-point (point)))
      (move-end-of-line nil)
      (re-search-backward "^\\|[^[:space:]]") ; go to last non-whitespace character
      (forward-char 1)
      (when (= orig-point (point))
        (move-end-of-line 1)))))              ; then go to eol

(defun tjf:navigate/goto-line (n)
  "Go to the line N."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun tjf:navigate/goto-saved-point ()
  "Go to the saved value of point."
  (interactive)
  (if tjf:navigate/saved-point
      (goto-char tjf:navigate/saved-point)
    (error "No point saved in this window!!!")))

(defun tjf:navigate/menu ()
  "Top portion of ‘Navigate’ menu."
  (if (not (eq major-mode 'fundamental-mode))
      (progn
        (easy-menu-add-item nil '("Navigate") "---"                                                  "*Rescan*")
        (easy-menu-add-item nil '("Navigate") ["Goto saved point" tjf:navigate/goto-saved-point   t] "---")
        (easy-menu-add-item nil '("Navigate") ["Save point"       tjf:navigate/save-point         t] "Goto Saved Point")
        (easy-menu-add-item nil '("Navigate") ["Go to line..."    goto-line                       t] "Undo scroll")
        (easy-menu-add-item nil '("Navigate") tjf:bookmark/menu                                      "---"))
    ))

(defun tjf:navigate/save-point ()
  "Save current value of point."
  (interactive)
  (setq tjf:navigate/saved-point (point)))

;;
(message "Loading tjf-navigate...done")
(provide 'tjf-navigate)

;;; tjf-navigate.el ends here
