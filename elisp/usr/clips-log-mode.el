;;; clips-log-mode.el --- Major mode for CLIPS log files -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2020 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   16-Aug-2016

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

;; Revision:
;;

;;; Code:

(message "Loading clips-log-mode...")

(defface clips-log-deactivate-face
  '((t :inherit default))
  "Face for CLIPS log Deactivation."
  :group 'font-lock-faces)

(defvar clips-log-deactivate-face (make-face 'clips-log-deactivate-face))

(defface clips-log-activate-face
  '((t :inherit default))
  "Face for CLIPS log Activation."
  :group 'font-lock-faces)

(defvar clips-log-activate-face (make-face 'clips-log-activate-face))

(defface clips-log-unmake-instance-face
  '((t :inherit default))
  "Face for CLIPS log unmake instance."
  :group 'font-lock-faces)

(defvar clips-log-unmake-instance-face (make-face 'clips-log-unmake-instance-face))

(defface clips-log-make-instance-face
  '((t :inherit default))
  "Face for CLIPS log make instance."
  :group 'font-lock-faces)

(defvar clips-log-make-instance-face (make-face 'clips-log-make-instance-face))

(defface clips-log-fire-face
  '((t (:foreground "blue3" :weight bold)))
  "Face for CLIPS log FIRE records."
  :group 'font-lock-faces)

(defvar clips-log-fire-face (make-face 'clips-log-fire-face))

(defface clips-log-retract-fact-face
  '((t (:inherit default)))
  "Face for CLIPS log retract fact."
  :group 'font-lock-faces)

(defvar clips-log-retract-fact-face (make-face 'clips-log-retract-fact-face))

(defface clips-log-assert-fact-face
  '((t (:inherit default)))
  "Face for CLIPS log assert fact."
  :group 'font-lock-faces)

(defvar clips-log-assert-fact-face (make-face 'clips-log-assert-fact-face))

(defface clips-log-error-face
  '((t (:inherit default)))
  "Face for CLIPS log FIRE records."
  :group 'font-lock-faces)

(defvar clips-log-error-face (make-face 'clips-log-error-face))

(defface clips-log-fatal-face
  '((t (:inherit default)))
  "Face for CLIPS log make instance."
  :group 'font-lock-faces)

(defvar clips-log-fatal-face (make-face 'clips-log-fatal-face))

(defface clips-log-pass-face '((t (:inherit default)))
  "Face for CLIPS log PASS records."
  :group 'font-lock-faces)

(defvar clips-log-pass-face (make-face 'clips-log-pass-face))

(defconst clips-log-font-lock-keywords-1
  (list
   '("^==> Activation .+" . clips-log-activate-face)
   '("^<== Activation .+" . clips-log-deactivate-face)
   '("^==> instance .+"   . clips-log-make-instance-face)
   '("^<== instance .+"   . clips-log-unmake-instance-face)
   '("^==> fact .+"       . clips-log-assert-fact-face)
   '("^<== fact .+"       . clips-log-retract-fact-face)
   '("^FIRE .+"           . clips-log-fire-face)
   '("^GRE-EXCEPTION .+"  . clips-log-fatal-face)
   ))

(defvar clips-log-font-lock-keywords clips-log-font-lock-keywords-1
  "Default expressions to highlight in Log modes.")

(defvar clips-log-mode-map (make-sparse-keymap)
  "The keymap used in `clips-log-mode'.")

(define-derived-mode clips-log-mode fundamental-mode
  "CLIPS Log mode"
  "Major mode for viewing CLIPS log files."
  (kill-all-local-variables)
  (setq major-mode 'clips-log-mode)
  (setq mode-name "CLIPS Log")
  (use-local-map clips-log-mode-map)
  (electric-indent-local-mode -1)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'which-function-mode)
  (setq which-function-mode nil)
  (setq font-lock-defaults '(clips-log-font-lock-keywords))
  (view-buffer (current-buffer))
  )
;;
(message "Loading clips-log-mode...done")
(provide 'clips-log-mode)

;;; clips-log-mode.el ends here
