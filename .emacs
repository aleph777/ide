;;; .emacs --- initialization file -*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:


(message "Loading .emacs...")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; (require 'cl)
(eval-when-compile (require 'cl))

(defconst is-cygwin?    (eq system-type 'cygwin))
(defconst is-daemon?    (daemonp))
(defconst is-gui?       (display-graphic-p))
(defconst is-linux?     (eq system-type 'gnu/linux))
(defconst is-linux-gui? (and is-linux? is-gui?))
(defconst is-windows?   (eq system-type 'windows-nt))

(defconst user-windows-id       "tfontaine")
(defconst user-dir-home         (concat (getenv "HOME") "/"))
(defconst user-dir-bin          (concat user-dir-home "bin/")) ;; this should be evaluated at run time
(defconst user-copyright-holder user-full-name)

(defalias 'user/home-directory 'user-dir-home)

(setq gc-cons-threshold (* 16 1024 1024))

(let* ((elisp    (concat user-dir-home "elisp/"))
       (packages (concat elisp "packages/"))
       (tiny     (concat packages "project--emacs-tiny-tools/lisp/")))

  (cl-pushnew (concat tiny "tiny")  load-path :test 'string=)
  (cl-pushnew (concat tiny "other") load-path :test 'string=)

  (cl-pushnew (concat packages "declutter") load-path :test 'string=)

  (cl-pushnew (concat elisp "ext") load-path :test 'string=)
  (cl-pushnew (concat elisp "usr") load-path :test 'string=)

  (cl-pushnew (concat elisp "images") image-load-path :test 'string=)

  (cl-pushnew (concat elisp "themes") custom-theme-load-path :test 'string=)

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
