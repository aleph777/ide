;;; u-navigate.el --- Navigation menu and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

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
;;

;;; Code:

(message "Loading u-navigate...")

(require 'u-bookmarks)

(eval-when-compile
  (require 'atim-unscroll))

;;
(defun u-navigate-menu ()
  "Top portion of ‘Navigate’ menu."
  (if (not (eq major-mode 'fundamental-mode))
      (progn
        (easy-menu-add-item nil '("Navigate") "---"                                     "*Rescan*")
        (easy-menu-add-item nil '("Navigate") ["Goto saved point" goto-saved-point   t] "---")
        (easy-menu-add-item nil '("Navigate") ["Save point"       save-point         t] "Goto Saved Point")
        (easy-menu-add-item nil '("Navigate") ["Redo scroll"      atim-unscroll-down t] "Save Point")
        (easy-menu-add-item nil '("Navigate") ["Undo scroll"      atim-unscroll-up   t] "Redo scroll")
        (easy-menu-add-item nil '("Navigate") ["Go to line..."    goto-line          t] "Undo scroll")
        (easy-menu-add-item nil '("Navigate") ["Bookmarks"        u-bookmark-menu    t] "Go to line...")
        (easy-menu-add-item nil '("Navigate") u-bookmarks-menu                          "---"))
    ))

(defvar u-navigate-saved-point nil "Saved value of point for current buffer.")
(make-variable-buffer-local 'u-navigate-saved-point)

(defun save-point ()
  "Save current value of point."
  (interactive)
  (setq u-navigate-saved-point (point)))

(defun goto-saved-point ()
  "Goto the saved value of point."
  (interactive)
  (if u-navigate-saved-point
      (goto-char u-navigate-saved-point)
    (error "No point saved in this window!!!")))

(defun u-end-of-line ()
  "Go to the first whitespace at ‘end-of-line’.  On second try go to
actual eol."
  (interactive)
  (unless (eolp)
    (let ((orig-point (point)))
      (move-end-of-line nil)
      (re-search-backward "^\\|[^[:space:]]")
      (forward-char 1)
      (when (= orig-point (point))
        (move-end-of-line 1)))))

(defun u-beginning-of-line ()
  "Go to first non-whitespace at beginning-of-line. On second try
go to actual bol."
  (interactive)
  (unless (bolp)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1)))))

(defun u-goto-line (N)
  "Go to the line N."
  (goto-char (point-min))
  (forward-line (1- N)))

(defun u-forward-word ()
  "Move the cursor forward to the beginning of the next word."
  (interactive)
  (skip-syntax-forward "w")
  (skip-syntax-forward "^w"))

(defun u-forward-symbol ()
  "Move the cursor forward to the beginning of the next symbol."
  (interactive)
  (re-search-forward "\\(\\sw\\|\\s_\\)+" nil 'move)
  (skip-syntax-forward "^w"))

(defun backward-symbol ()
  "Move the cursor backward to the beginning of the previous symbol."
  (interactive)
  (forward-symbol -1))

(defalias 'search-word-forward  'tinysearch-search-word-forward)
(defalias 'search-word-backward 'tinysearch-search-word-backward)

;;
(message "Loading u-navigate...done")
(provide 'u-navigate)

;;; u-navigate.el ends here
