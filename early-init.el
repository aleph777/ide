;;; early-init.el --- initialization file -*- lexical-binding: t; -*- ; -*-Emacs-Lisp-*-

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

;; (message "Loading early-init.el...")


;; garbage collection
;;
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq gc-cons-threshold  (* 16 1024 1024))
              (setq gc-cons-percentage 0.1)
              (garbage-collect)))

;; frames and windows
;;
(setq frame-inhibit-implied-resize t) ;; Do not resize the frame at this early stage.
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

(setq inhibit-splash-screen       t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen      t)
(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-x-resources         t)
(setq initial-buffer-choice       nil)

(setq-default cursor-in-non-selected-windows nil)

(advice-add #'display-startup-screen :override #'ignore)

(setq highlight-nonselected-windows nil)

(setq auto-mode-case-fold nil)

;; bidirectional text
;;
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right)

;; misc
;;
(setq ad-redefinition-action 'accept)

(setq idle-update-delay 1.0)

(setq lexical-binding nil)

(setq package-enable-at-startup nil)

(setq read-process-output-max (* 256 1024))

(setq use-dialog-box t)

(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; -----------------------------------------------------------------------------

;; (if (and (featurep 'native-compile)
;;          (fboundp 'native-comp-available-p)
;;          (native-comp-available-p))
;;     ;; Activate `native-compile'
;;     (progn
;;       (setq native-comp-deferred-compilation t)
;;       (setq native-comp-speed                2)
;;       ;; (setq package-native-compile           t)
;;       (setq native-comp-async-report-warnings-errors 'silent)
;;     )
;;   ;; Deactivate the `native-compile' feature if it is not available
;;   (setq features (delq 'native-compile features)))

;; Suppress compiler warnings and don't inundate users with their popups.
;; (setq native-comp-async-report-warnings-errors
;; (or minimal-emacs-debug 'silent))
;; (setq native-comp-warning-on-missing-source minimal-emacs-debug)

;; (setq debug-on-error minimal-emacs-debug
;;       jka-compr-verbose minimal-emacs-debug)

(setq byte-compile-warnings '(not free-vars obsolete unresolved noruntime lexical make-local))
;; (setq byte-compile-warnings '(not obsolete))
;; (setq byte-compile-verbose minimal-emacs-debug)

;;
;; (message "Loading early-init.el...done")
;;; early-init.el ends here
