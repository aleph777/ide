;;; .emacs.el --- initialization file -*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:


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

(defvar tjf:user/dir-autosave (concat tjf:user/dir-config "autosave/"))
(defvar tjf:user/dir-backup   (concat tjf:user/dir-config "backup/"))
(defvar tjf:user/dir-bookmark (concat tjf:user/dir-config "bookmark/"))

(defvar tjf:user/dir-themes   (concat tjf:user/dir-elisp "themes/"))

(defvar tjf:user/dir-elisp-ext    (concat tjf:user/dir-elisp "ext/"))
(defvar tjf:user/dir-elisp-images (concat tjf:user/dir-elisp "images/"))
(defvar tjf:user/dir-elisp-tjf    (concat tjf:user/dir-elisp "tjf/"))

;;
(cl-pushnew tjf:user/dir-elisp-ext load-path :test 'string=)
(cl-pushnew tjf:user/dir-elisp-tjf load-path :test 'string=)

(cl-pushnew tjf:user/dir-elisp-images image-load-path :test 'string=)

(setq user-emacs-directory tjf:user/dir-config)
(setq custom-file (concat tjf:user/dir-elisp "custom.el"))
(setq custom-theme-directory tjf:user/dir-themes)

(load custom-file)

;;
(message "Loading .emacs.el...done")

;;; .emacs.el ends here
