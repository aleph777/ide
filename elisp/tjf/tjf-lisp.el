;;; tjf-lisp.el --- Support for various Lisp modes -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2001-2021 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   26-Nov-2001

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

;; Revision: 02-Feb-2010 Major overhaul
;;           25-Mar-2015 Changed obsolete eval-current-buffer to eval-buffer
;;           14-Apr-2015 Added menu "-defun" functions to menu
;;           20-Oct-2015 Changed obsolete lisp-complete-symbol to completion-at-point
;;           02-Feb-2016 Added neotree
;;                       Fixed byte-recompile-directory menu entry
;;           28-Feb-2016 Refactored as ‘u-lisp’
;;           03-Mar-2016 Updated to use ‘yasnippet’
;;           18-Apr-2016 Updated for ‘use-package’
;;           23-Jun-2016 Removed globally set ‘semantic-mode’
;;           30-Dec-2016 Modified header line in ‘lisp-insert-skeleton’
;;           02-Jan-2017 Added ‘u-lisp-imenu-generic-expression’
;;           18-Jan-2017 Updated ‘lisp-insert-skeleton’
;;           13-Jun-2020 Added FORCE to byte recompile directory menu entry
;;           03-Feb-2021 ‘tjf’ overhaul
;;           07-Apr-2021 Updated ‘tjf:lisp/insert-skeleton’
;;

;;; Code:

(message "Loading tjf-lisp...")
(require 'easymenu)
(require 'tjf-date)
(require 'tjf-edit)

(eval-when-compile
  (require 'cape))

;;
(defconst tjf:lisp/imenu-generic-expression
  '(("Functions etc." "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
 ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
 ("Variables" "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]" 1)
 ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)))

(defun tjf:lisp/byte-recompile ()
  "Recompile directory."
  (byte-recompile-directory 0))

(defun tjf:lisp/insert-skeleton ()
  "Insert a Lisp module skeleton."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat tjf:user/dir-elisp "templates/lisp-skeleton.el")))
  (let ((name   (substring (file-name-nondirectory (buffer-file-name)) 0 -3))
        (year   (format-time-string "%Y-%Y"))
        (author (user-full-name))
        (date   (tjf:date/today tjf:date/dd-mon-yyyy)))
    (tjf:edit/fill-skeleton "<<<NAME>>>"   name)
    (tjf:edit/fill-skeleton "<<<YEAR>>>"   year)
    (tjf:edit/fill-skeleton "<<<AUTHOR>>>" author)
    (tjf:edit/fill-skeleton "<<<DATE>>>"   date)))

(defun tjf:lisp/setup ()
  "Set up ‘lisp-mode’."
  (add-hook 'completion-at-point-functions #'cape-keyword nil 'local)
  (add-hook 'completion-at-point-functions #'cape-symbol nil 'local)
  (imenu-add-to-menubar "Navigate"))

(defvar tjf:lisp/build-menu-text
  '("Build"
    ["Byte Compile This File" emacs-lisp-byte-compile          :enable (buffer-file-name)]
    ["Byte Compile And Load"  emacs-lisp-byte-compile-and-load :enable (buffer-file-name)]
    "---"
    ["Byte Compile File..."      byte-compile-file]
    ["Byte Recompile Directory" (byte-recompile-directory "." 0 t)]
))

(defvar tjf:lisp/mode-menu-text
  '("Lisp"
    ["Beginning Of Defun" beginning-of-defun :active t]
    ["End Of Defun"       end-of-defun       :active t]
    ["Mark Defun"         mark-defun         :active t]
    "---"
    ["Evaluate Defun"  eval-defun  :active t          ]
    ["Evaluate Buffer" eval-buffer :active t          ]
    ["Evaluate Region" eval-region :active mark-active]
    "---"
    ["Insert Skeleton"   tjf:lisp/insert-skeleton :enable (buffer-file-name)]
    ))

;;
(message "Loading tjf-lisp...setting up minor mode menus...")
(define-key lisp-mode-map             [menu-bar] nil)
(define-key lisp-mode-shared-map      [menu-bar] nil)
(define-key lisp-interaction-mode-map [menu-bar] nil)
(define-key emacs-lisp-mode-map       [menu-bar] nil)
;;
(easy-menu-define tjf:lisp/menu       emacs-lisp-mode-map       "Lisp"  tjf:lisp/mode-menu-text)
(easy-menu-define tjf:lisp/build-menu emacs-lisp-mode-map       "Build" tjf:lisp/build-menu-text)
(easy-menu-define tjf:lisp/menu       lisp-interaction-mode-map "Lisp"  tjf:lisp/mode-menu-text)
(easy-menu-define tjf:lisp/build-menu lisp-interaction-mode-map "Build" tjf:lisp/build-menu-text)

(setq lisp-imenu-generic-expression tjf:lisp/imenu-generic-expression)

;;
(message "Loading tjf-lisp...done")
(provide 'tjf-lisp)

;;; tjf-lisp.el ends here
