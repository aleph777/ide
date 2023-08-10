;;; early-init.el --- initialization file -*-Emacs-Lisp-*-

;;; Commentary:

;; This file is loaded before the package system and GUI is
;; initialized, so in it you can customize variables that affect frame
;; appearance as well as the package initialization process, such as
;; ‘package-enable-at-startup’, ‘package-load-list’, and
;; ‘package-user-dir’. Note that variables like ‘package-archives’
;; which only affect the installation of new packages, and not the
;; process of making already-installed packages available, may be
;; customized in the regular init file. *NotPackage Installation::.

;; We do not recommend that you move into ‘early-init.el’
;; customizations that can be left in the normal init files. That is
;; because the early init file is read before the GUI is initialized,
;; so customizations related to GUI features will not work reliably in
;; ‘early-init.el’.

;;; Code:

;; Do not initialise installed packages
(setq package-enable-at-startup nil)

;; Do not allow loading from the package cache
(setq package-quickstart nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

(setq inhibit-splash-screen       t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen      t)

(setq use-dialog-box              t)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq gc-cons-threshold  (* 8 1024 1024))
              (setq gc-cons-percentage 0.1)
              (garbage-collect)))

;;; early-init.el ends here
