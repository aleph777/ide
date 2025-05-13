;;; init.el --- Global initialization -*-lexical-binding: t-*- ;; -*-no-byte-compile: t ;; -*-Emacs-Lisp-*-

(message "Loading early-init.el...")

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
(setq warning-suppress-types     '((defvaralias) (lexical-binding)))

(setq byte-compile-warnings '(not free-vars obsolete unresolved noruntime lexical make-local))

;; -----------------------------------------------------------------------------

(message "Loading .emacs.el...")

;;
(require 'cl-lib)

(defvar is-daemon?    (daemonp))
(defvar is-gui?       (display-graphic-p))

(defvar is-cygwin?    (eq system-type 'cygwin))
(defvar is-linux?     (eq system-type 'gnu/linux))
(defvar is-macos?     (eq system-type 'darwin))
(defvar is-windows?   (eq system-type 'windows-nt))

(defvar is-linux-gui? (and is-linux? is-gui?))

(defvar tjf:user/windows-id       "tfontaine")
(defvar tjf:user/copyright-holder user-full-name)
(defvar tjf:user/dir-home         (concat (getenv "HOME") "/"))

;;
(defvar tjf:user/dir-bin      (concat tjf:user/dir-home "bin/"))
(defvar tjf:user/dir-config   (concat tjf:user/dir-home ".config/emacs/"))
(defvar tjf:user/dir-elisp    (concat tjf:user/dir-home "elisp/"))
(defvar tjf:user/dir-emacsd   (concat tjf:user/dir-home ".emacs.d/"))

(defvar tjf:user/dir-autosave (concat tjf:user/dir-config "autosave/"))
(defvar tjf:user/dir-backup   (concat tjf:user/dir-config "backup/"))
(defvar tjf:user/dir-bookmark (concat tjf:user/dir-config "bookmark/"))

(defvar tjf:user/dir-themes   (concat tjf:user/dir-elisp "themes/"))

(defvar tjf:user/dir-elisp-ext    (concat tjf:user/dir-elisp "ext/"))
(defvar tjf:user/dir-elisp-images (concat tjf:user/dir-elisp "images/"))
(defvar tjf:user/dir-elisp-tjf    (concat tjf:user/dir-elisp "tjf/"))

(defvar tjf:user/dir-initialize (concat tjf:user/dir-elisp "00_initialize/"))
(defvar tjf:user/dir-experiment (concat tjf:user/dir-elisp "00_experiment/"))

(defvar tjf:user/dir-tenbeauty  (concat tjf:user/dir-home "Workspace/tenbeauty/"))

;;
(cl-pushnew tjf:user/dir-elisp-images image-load-path :test 'string=)
(cl-pushnew tjf:user/dir-elisp-ext  load-path :test 'string=)
(cl-pushnew tjf:user/dir-elisp-tjf  load-path :test 'string=)

(setq user-emacs-directory tjf:user/dir-config)
(setq custom-file (concat tjf:user/dir-elisp "custom.el"))
(setq custom-theme-directory tjf:user/dir-themes)

(load custom-file)

;; -----------------------------------------------------------------------------

(defun message--with-timestamp (format-string &rest args)
  "Add FORMAT-STRING timestamp (using ARGS) to `*Messages*' buffer."
  (when (and (>   (length  format-string) 0)
             (not (string= format-string " ")))
    (let ((deactivate-mark nil))
      (save-mark-and-excursion
        (with-current-buffer "*Messages*"
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (when (not (bolp)) (newline))
            (insert (format-time-string "[%T.%3N] " (current-time)))))))))

(advice-add 'message :before 'message--with-timestamp)


;;
(enable-theme 'fontaine)

(package-initialize)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")       t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("non-gnu"      . "https://elpa.nongnu.org/nongnu/")   t)

(setq package-archive-priorities '(("gnu"          . 10)
                                   ("non-gnu"      . 20)
                                   ("melpa-stable" . 30)
                                   ("melpa"        . 40)))

;; =============================================================================

(use-package cua-base             :ensure nil :defer t
  :config
  (cua-mode))

(global-set-key [undo] 'undo)

(message "Configuring from stub.el...done")
