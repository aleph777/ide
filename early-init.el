;;; early-init.el --- initialization file -*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:

;; Do not initialise installed packages
(setq package-enable-at-startup nil)

;; Do not allow loading from the package cache
(setq package-quickstart nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(setq inhibit-splash-screen t)
(setq use-dialog-box t)               ; only for mouse events

;;; early-init.el ends here
