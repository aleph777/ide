;;; tjf-clipboard.el --- Local clipboard implementation -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2021 Tom Fontaine

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
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-clipboard...")
(require 'tjf-flags)

;;
(defvar tjf:clipboard/number 0 "The number of the selected clipboard buffer.")

(defvar tjf:clipboard/name nil "The name of the selected clipboard buffer.")

(defvar tjf:clipboard/select-title nil "The menu title of the selected clipboard buffer.")

(defun tjf:clipboard/append ()
  "Append the selected text to the current clipboard buffer."
  (interactive "*")
  (append-to-buffer (get-buffer-create tjf:clipboard/name) (region-beginning) (region-end)))

(defun tjf:clipboard/copy ()
  "Copies the selected text to the current clipboard buffer."
  (interactive)
  (copy-to-buffer (get-buffer-create tjf:clipboard/name) (region-beginning) (region-end)))

(defun tjf:clipboard/copy-n (n)
  "Copy to clipboard buffer N."
  (interactive)
  (copy-to-buffer (get-buffer-create (tjf:clipboard/get-name n)) (region-beginning) (region-end)))

(defun tjf:clipboard/cut ()
  "Cuts the selected text to the current clipboard buffer."
  (interactive "*")
  (copy-to-buffer (get-buffer-create tjf:clipboard/name) (region-beginning) (region-end))
  (kill-region (region-beginning) (region-end)))

(defun tjf:clipboard/enable-paste? ()
  "Boolean: should paste from clipboard be enabled?"
  (and (not buffer-read-only) (get-buffer tjf:clipboard/name)))

(defun tjf:clipboard/get-name (number)
  "Return the clipboard buffer name from NUMBER."
  (format " *clipboard %d*" number))

(defun tjf:clipboard/get-select-title (number)
  "Return the clipboard select menu title from NUMBER."
  (format "Select Clipboard [%d]" number))

(defun tjf:clipboard/paste ()
  "Pastes from the current clipboard buffer."
  (interactive "*")
  (let ((clipboard (get-buffer tjf:clipboard/name))
        (transient-mark-mode nil))
    (if clipboard
        (insert-buffer-substring clipboard)
      (error "Clipboard not available!!!"))))

(defun tjf:clipboard/paste-n (n)
  "Paste from clipboard buffer N."
  (interactive "*")
  (let ((clipboard (get-buffer (clipboard-buffer n)))
        (transient-mark-mode nil))
    (if clipboard
        (insert-buffer-substring clipboard)
      (error "Clipboard not available!!!"))))

(defun tjf:clipboard/prepend ()
  "Prepend the selected text to the current clipboard buffer."
  (interactive "*")
  (prepend-to-buffer (get-buffer-create tjf:clipboard/name) (region-beginning) (region-end)))

(defun tjf:clipboard/select (number)
  "Change the selected clipboard buffer to NUMBER."
  (setq tjf:clipboard/number       number)
  (setq tjf:clipboard/name         (tjf:clipboard/get-name number))
  (setq tjf:clipboard/select-title (tjf:clipboard/get-select-title tjf:clipboard/number)))

(defun tjf:clipboard/selected? (number)
  "Boolean: is NUMBER the selected clipboard?"
  (= tjf:clipboard/number number))

(defun tjf:clipboard/view ()
  "View the contents of the clipboard buffer."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create tjf:clipboard/name)))

(defvar tjf:clipboard/menu
  '("Clipboard"
    (tjf:clipboard/select-title
     ["0" (tjf:clipboard/select 0) :style toggle :selected (tjf:clipboard/selected? 0)]
     ["1" (tjf:clipboard/select 1) :style toggle :selected (tjf:clipboard/selected? 1)]
     ["2" (tjf:clipboard/select 2) :style toggle :selected (tjf:clipboard/selected? 2)]
     ["3" (tjf:clipboard/select 3) :style toggle :selected (tjf:clipboard/selected? 3)]
     ["4" (tjf:clipboard/select 4) :style toggle :selected (tjf:clipboard/selected? 4)]
     ["5" (tjf:clipboard/select 5) :style toggle :selected (tjf:clipboard/selected? 5)]
     ["6" (tjf:clipboard/select 6) :style toggle :selected (tjf:clipboard/selected? 6)]
     ["7" (tjf:clipboard/select 7) :style toggle :selected (tjf:clipboard/selected? 7)]
     ["8" (tjf:clipboard/select 8) :style toggle :selected (tjf:clipboard/selected? 8)]
     ["9" (tjf:clipboard/select 9) :style toggle :selected (tjf:clipboard/selected? 9)])
    "---"
    ["Cut to Clipboard"     tjf:clipboard/cut   :enable (tjf:flags/enable-modify-region?)]
    ["Copy to Clipboard"    tjf:clipboard/copy  :enable mark-active]
    ["Paste from Clipboard" tjf:clipboard/paste :enable (tjf:clipboard/enable-paste?)]
    "---"
    ["Append to Clipboard"  tjf:clipboard/append  :enable mark-active]
    ["Prepend to Clipboard" tjf:clipboard/prepend :enable mark-active]
    "---"
    ["View Clipboard" tjf:clipboard/view :enable (get-buffer tjf:clipboard/name)]
    ))

(setq tjf:clipboard/name         (tjf:clipboard/get-name tjf:clipboard/number))
(setq tjf:clipboard/select-title (tjf:clipboard/get-select-title tjf:clipboard/number))

;;
(message "Loading tjf-clipboard...done")
(provide 'tjf-clipboard)

;;; tjf-clipboard.el ends here
