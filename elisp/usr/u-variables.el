;;; u-variables.el --- Local overrides for global variables -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2020 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   15-Apr-2016

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

;; Revision: 10-Jan-2017 Added setting of ‘gc-cons-threshold’
;;                       Added setting of ‘gnutls-min-prime-bits’
;;                       Added ‘flow’ to ‘u-host-type-alist’
;;           21-Mar-2017 Added ‘user-copyright-holder’
;;           27-Mar-2017 Added ‘msb-max-menu-items’
;;           29-Jun-2017 Removed unnecessary variables
;;           03-Jul-2017 Moved ‘user-dir-home’ to .emacs
;;           24-Aug-2017 Moved ‘gc-cons-threshold’ to .emacs
;;           19-Apr-2018 Removed ‘u-initial-major-mode’
;;           15-Dec-2020 Moved defvars to .emacs
;;

;;; Code:

(message "Loading u-variables...")
;;
(eval-when-compile
 (require 'u-macro))

(let ((autosave-dir (concat user-dir-home ".autosave/")))
  (setq auto-save-file-name-transforms `((".*"   ,autosave-dir t))))

(let ((backup-dir (concat user-dir-home ".backup/")))
  (setq backup-directory-alist         `((".*" . ,backup-dir))))

(fset 'yes-or-no-p #'y-or-n-p)

(setq-default cursor-type '(bar . 2))

(setq-default ediff-split-window-function 'split-window-horizontally)

(setq-default font-lock-maximum-decoration t)

(setq-default frame-title-format "%b")

(setq-default indent-tabs-mode nil)

(setq blink-cursor-blinks 0)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(setq disabled-command-function nil)

(setq fill-column 8192)

(setq gnutls-min-prime-bits 80)

(setq max-image-size 256)

(setq msb-max-menu-items 32)

(setq mouse-drag-copy-region t)

(setq mouse-yank-at-point t)

(setq recenter-positions '(top middle bottom))

(setq recentf-auto-cleanup 'never)

(setq recentf-max-menu-items 25)

(setq recentf-menu-before "Open in New Window...")

(setq ring-bell-function '(lambda () (let ((visible-bell t)) (beep)) (beep) (beep)))

(setq save-interprogram-paste-before-kill t)

(setq sentence-end-double-space nil)

(setq which-func-modes '(emacs-lisp-mode c-mode c++-mode cperl-mode python-mode diff-mode))

(on-gnu/linux-gui
 (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
 (setq x-pointer-shape x-pointer-center-ptr))

;;
(message "Loading u-variables...done")
(provide 'u-variables)

;;; u-variables.el ends here
