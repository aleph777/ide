;;; clipboard.el --- Local clipboard implementation -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

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
;;           27-Jun-2019 Added ‘u/paste-clipboard-n’
;;                       Added ‘u/clipboard-buffer’

;;; Code:

(message "Loading clipboard...")
;;
(defconst clipboard-buffer-open  " *clipboard ")
(defconst clipboard-buffer-close "*")

(defvar clipboard-number 0)

(defsubst clipboard-buffer (number)
  "Return the clipboard buffer name from NUMBER."
  (format "%s%d%s" clipboard-buffer-open number clipboard-buffer-close))

(defvar clipboard-buffer-name  (clipboard-buffer 0))
(defvar clipboard-select-title "Select Clipboard [0]")

(defvar clipboard-menu
  '("Clipboard"
    (clipboard-select-title
     ["0" (select-clipboard 0) :active t :key-sequence nil :style toggle :selected (= clipboard-number 0)]
     ["1" (select-clipboard 1) :active t :key-sequence nil :style toggle :selected (= clipboard-number 1)]
     ["2" (select-clipboard 2) :active t :key-sequence nil :style toggle :selected (= clipboard-number 2)]
     ["3" (select-clipboard 3) :active t :key-sequence nil :style toggle :selected (= clipboard-number 3)]
     ["4" (select-clipboard 4) :active t :key-sequence nil :style toggle :selected (= clipboard-number 4)]
     ["5" (select-clipboard 5) :active t :key-sequence nil :style toggle :selected (= clipboard-number 5)]
     ["6" (select-clipboard 6) :active t :key-sequence nil :style toggle :selected (= clipboard-number 6)]
     ["7" (select-clipboard 7) :active t :key-sequence nil :style toggle :selected (= clipboard-number 7)]
     ["8" (select-clipboard 8) :active t :key-sequence nil :style toggle :selected (= clipboard-number 8)]
     ["9" (select-clipboard 9) :active t :key-sequence nil :style toggle :selected (= clipboard-number 9)])
    "---"
    ["Cut to Clipboard"     cut-clipboard     :enable (and mark-active (not buffer-read-only))]
    ["Copy to Clipboard"    copy-clipboard    :enable mark-active]
    ["Paste from Clipboard" paste-clipboard   :enable (and (not buffer-read-only) (get-buffer clipboard-buffer-name))]
    "---"
    ["Append to Clipboard"  append-clipboard  :enable mark-active]
    ["Prepend to Clipboard" prepend-clipboard :enable mark-active]
    ))

(defun select-clipboard (number)
  "Change the selected clipboard buffer to NUMBER."
  (setq clipboard-number       number
        clipboard-buffer-name  (clipboard-buffer clipboard-number)
	clipboard-select-title (format "Select Clipboard [%d]" clipboard-number)))

(defun cut-clipboard ()
  "Cuts the selected text to the current clipboard buffer."
  (interactive "*")
  (copy-to-buffer (get-buffer-create clipboard-buffer-name) (region-beginning) (region-end))
  (kill-region (region-beginning) (region-end)))

(defun copy-clipboard ()
  "Copies the selected text to the current clipboard buffer."
  (interactive)
  (copy-to-buffer (get-buffer-create clipboard-buffer-name) (region-beginning) (region-end)))

(defun append-clipboard ()
  "Append the selected text to the current clipboard buffer."
  (interactive "*")
  (append-to-buffer (get-buffer-create clipboard-buffer-name) (region-beginning) (region-end)))

(defun prepend-clipboard ()
  "Prepend the selected text to the current clipboard buffer."
  (interactive "*")
  (prepend-to-buffer (get-buffer-create clipboard-buffer-name) (region-beginning) (region-end)))

(defun paste-clipboard ()
  "Pastes from the current clipboard buffer."
  (interactive "*")
  (let ((clipboard (get-buffer clipboard-buffer-name))
        (transient-mark-mode nil))
    (if clipboard
        (insert-buffer-substring clipboard)
      (error "Clipboard not available!!!"))))

(defun copy-clipboard-n (n)
  "Copy to clipboard buffer N."
  (interactive "*")
  (copy-to-buffer (get-buffer-create (clipboard-buffer n)) (region-beginning) (region-end)))

(defun paste-clipboard-n (n)
  "Paste from clipboard buffer N."
  (interactive "*")
  (let ((clipboard (get-buffer (clipboard-buffer n)))
        (transient-mark-mode nil))
    (if clipboard
        (insert-buffer-substring clipboard)
      (error "Clipboard not available!!!"))))

(defun view-clipboard ()
  "View the contenets of the clipboard buffer."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create clipboard-buffer-name)))
;;
(message "Loading clipboard...done")
(provide 'clipboard)

;;; clipboard.el ends here
