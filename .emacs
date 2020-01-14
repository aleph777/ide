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

(defconst user-windows-id "tfontaine")
(defconst user-dir-home (concat (getenv "HOME") "/"))

(defalias 'user/home-directory 'user-dir-home)

(setq gc-cons-threshold (* 16 1024 1024))

(let* ((elisp    (concat user-dir-home "elisp/"))
       (packages (concat elisp "packages/"))
       (tiny     (concat packages "project--emacs-tiny-tools/lisp/")))

  (pushnew (concat tiny "tiny")  load-path :test 'string=)
  (pushnew (concat tiny "other") load-path :test 'string=)

  (pushnew (concat packages "declutter") load-path :test 'string=)

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
