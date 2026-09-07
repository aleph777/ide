;;; .emacs.el --- initialization file  -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:

(message "Loading .emacs.el...")

;;
(require 'cl-lib)

(defconst is-daemon?    (daemonp))
(defconst is-gui?       (display-graphic-p))

(defconst is-cygwin?    (eq system-type 'cygwin))
(defconst is-linux?     (eq system-type 'gnu/linux))
(defconst is-macos?     (eq system-type 'darwin))
(defconst is-windows?   (eq system-type 'windows-nt))
(defconst is-wsl?       (equal (system-name) "HON-366GKK4"))
(defconst is-linux-gui? (and is-linux? is-gui?))

(defvar tjf:user/windows-id       "H679869")
(defvar tjf:user/copyright-holder user-full-name)
(defvar tjf:user/dir-home         (concat (getenv "HOME") "/"))
(defvar tjf:user/dir-emacs.d      user-emacs-directory)

;;
(defvar tjf:user/dir-bin      (concat tjf:user/dir-home "bin/"))
(defvar tjf:user/dir-config   (concat tjf:user/dir-home ".config/emacs/"))
(defvar tjf:user/dir-elisp    (concat tjf:user/dir-home "elisp/"))
(defvar tjf:user/dir-emacsd   (concat tjf:user/dir-home ".emacs.d/"))

(defvar tjf:user/dir-autosave (concat tjf:user/dir-config "autosave/"))
(defvar tjf:user/dir-backup   (concat tjf:user/dir-config "backup/"))

(defvar tjf:user/dir-themes   (concat tjf:user/dir-elisp "themes/"))

(defvar tjf:user/dir-elisp-ext     (concat tjf:user/dir-elisp "ext/"))
(defvar tjf:user/dir-elisp-images  (concat tjf:user/dir-elisp "images/"))
(defvar tjf:user/dir-elisp-toolbar (concat tjf:user/dir-elisp "images/toolbar/"))
(defvar tjf:user/dir-elisp-tjf     (concat tjf:user/dir-elisp "tjf/"))

(if is-linux?
    (let ((cmd-distro  "lsb_release -is | tr [:upper:] [:lower:] | tr -d '\n'")
          (cmd-version "lsb_release -rs | cut -d. -f1 | tr -d '\n'"))
      (setq tjf:user/os-distro  (shell-command-to-string cmd-distro))
      (setq tjf:user/os-version (shell-command-to-string cmd-version))
      (setq tjf:user/os-distro-version (concat tjf:user/os-distro " " tjf:user/os-version))))

;;
(cl-pushnew tjf:user/dir-elisp-images  image-load-path :test 'string=)
(cl-pushnew tjf:user/dir-elisp-toolbar image-load-path :test 'string=)
(cl-pushnew tjf:user/dir-elisp-ext     load-path       :test 'string=)
(cl-pushnew tjf:user/dir-elisp-tjf     load-path       :test 'string=)

(setq user-emacs-directory tjf:user/dir-config)
(setq custom-file (concat tjf:user/dir-elisp "custom.el"))
(setq custom-theme-directory tjf:user/dir-themes)

(load custom-file)

;;
(message "Loading .emacs.el...done")
(provide 'init)

;;; .emacs.el ends here
