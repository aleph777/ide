;;; .emacs --- initialization file -*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:


(message "Loading .emacs...")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)

(defvar user-windows-id "tfontaine")
(defvar user-dir-home (concat (getenv "HOME") "/"))

(setq gc-cons-threshold (* 16 1024 1024))

(let ((elisp (concat user-dir-home "elisp/")))
  (pushnew (concat elisp "packages/tiny-tools/lisp/tiny")  load-path :test 'string=)
  (pushnew (concat elisp "packages/tiny-tools/lisp/other") load-path :test 'string=)

  (pushnew (concat elisp "ext") load-path :test 'string=)
  (pushnew (concat elisp "usr") load-path :test 'string=)

  (pushnew (concat elisp "images") image-load-path :test 'string=)

  (pushnew (concat elisp "themes") custom-theme-load-path :test 'string=)

  (setq custom-file (concat elisp "custom.el"))
  )

(put 'inhibit-startup-echo-area-message 'saved-value (setq inhibit-startup-echo-area-message (user-login-name)))
(load custom-file)

;; ======================================================================

;
;(global-set-key [ns-drag-file] 'ns-find-file)
;
;
;;
(message "Loading .emacs...done")
;;; .emacs ends here
