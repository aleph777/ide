;;; u-clipboard.el --- Local clipboard implementation -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2019 Tom Fontaine

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

;; Revision: 22-Jun-2000 Fixed interactive bug in `usr-copy-clipboard'
;;           07-Mar-2001 Added Xemacs support
;;           29-Dec-2004 Modified `usr-select-clipboard' to use format statements
;;           02-Feb-2010 Added exit message
;;           17-Jun-2014 Removed `byte-compile-dynamic'
;;           29-Jan-2016 Changed menu order
;;           29-Feb-2016 Changed from `usr-' to `u-'

;;; Code:

(message "Loading u-clipboard...")
;;
(defvar u-clipboard-number       0)
(defvar u-clipboard-buffer-name  " *clipboard 0*")
(defvar u-clipboard-select-title "Select Local Clipboard [0]")

(defvar u-clipboard-menu
  '("Local Clipboard"
    (u-clipboard-select-title
     ["0" (u-select-clipboard 0) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 0)]
     ["1" (u-select-clipboard 1) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 1)]
     ["2" (u-select-clipboard 2) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 2)]
     ["3" (u-select-clipboard 3) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 3)]
     ["4" (u-select-clipboard 4) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 4)]
     ["5" (u-select-clipboard 5) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 5)]
     ["6" (u-select-clipboard 6) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 6)]
     ["7" (u-select-clipboard 7) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 7)]
     ["8" (u-select-clipboard 8) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 8)]
     ["9" (u-select-clipboard 9) :active t :key-sequence nil :style toggle :selected (= u-clipboard-number 9)])
    "---"
    ["Cut to Local Clipboard"     u-cut-clipboard     :enable (and mark-active (not buffer-read-only))]
    ["Copy to Local Clipboard"    u-copy-clipboard    :enable mark-active]
    ["Paste from Local Clipboard" u-paste-clipboard   :enable (and (not buffer-read-only) (get-buffer u-clipboard-buffer-name))]
    "---"
    ["Append to Local Clipboard"  u-append-clipboard  :enable mark-active]
    ["Prepend to Local Clipboard" u-prepend-clipboard :enable mark-active]
    ))

(defun u-select-clipboard (clipboard-number)
  "Changes the selected clipboard buffer."
  (setq u-clipboard-number       clipboard-number
        u-clipboard-buffer-name  (format " *clipboard %d*" clipboard-number)
	u-clipboard-select-title (format "Select Clipboard [%d]" clipboard-number)))

(defun u-cut-clipboard ()
  "Cuts the selected text to the current clipboard buffer."
  (interactive "*")
  (copy-to-buffer (get-buffer-create u-clipboard-buffer-name) (region-beginning) (region-end))
  (kill-region (region-beginning) (region-end)))

(defun u-copy-clipboard ()
  "Copies the selected text to the current clipboard buffer."
  (interactive)
  (copy-to-buffer (get-buffer-create u-clipboard-buffer-name) (region-beginning) (region-end)))

(defun u-append-clipboard ()
  "Appends the selected text to the current clipboard buffer."
  (interactive "*")
  (append-to-buffer (get-buffer-create u-clipboard-buffer-name) (region-beginning) (region-end)))

(defun u-prepend-clipboard ()
  "Prepends the selected text to the current clipboard buffer."
  (interactive "*")
  (prepend-to-buffer (get-buffer-create u-clipboard-buffer-name) (region-beginning) (region-end)))

(defun u-paste-clipboard ()
  "Pastes from the current clipboard buffer."
  (interactive "*")
  (let ((clipboard (get-buffer u-clipboard-buffer-name))
        (transient-mark-mode nil))
    (if clipboard
        (insert-buffer-substring clipboard)
      (error "Clipboard not available!!!"))))

(defun u-view-clipboard ()
  "View the contenets of the clipboard buffer."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create u-clipboard-buffer-name)))
;;
(message "Loading u-clipboard...done")
(provide 'u-clipboard)

;;; u-clipboard.el ends here
