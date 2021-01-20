;;; u-csharp.el --- CSharp major mode -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2015-2021 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   12-May-2015

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

;; Revision: 18-Jan-2016 Updated for new user interface
;;           02-Feb-2016 Added `neotree'
;;           29-Feb-2016 Changed from `usr-' to `u-'
;;                       Removed`neotree'
;;           23-Jun-2016 Removed globally set `semantic-mode'
;;           10-Mar-2017 Fixed ‘imenu’ handling
;;

;;; Code:

(message "Loading u-csharp...")

(eval-when-compile
  (require 'csharp-mode)
  (require 'flycheck))

(defvar csharp-build-menu-text) ;; !!! solution && projects
(defvar csharp-search-path (list "./" "../" "../../"))

(defvar csharp-build-menu-text
  '("Build"
    ["Refresh" beginning-of-defun :active t]
    "---"
    ))

(defvar csharp-build-menu nil)

(defsubst csharp-search-path (glob)
  "Return the search results for GLOB in ‘csharp-search-path’."
  (let ((dir-list csharp-search-path)
        (list))
    (dolist (element dir-list list)
      (setq list (append list (file-expand-wildcards (concat element glob)))))
    list))

(defun csharp-find-sln ()
  "Return a list of local solution files for this buffer."
  (csharp-search-path "*.sln"))

(defun csharp-find-proj ()
  "Return a list of local project files for this buffer."
  (csharp-search-path "*.csproj"))

(defun csharp-build-menu ()
  "Create a Build menu."
  (let ((sln-list  (csharp-find-sln))
        (proj-list (csharp-find-proj))
        
        ))
  )
(defun u-csharp-setup()
  "C#-mode setup function"
  ;; (easy-menu-define csharp-build-menu csharp-mode-map "Build" csharp-build-menu-text)
  (flycheck-select-checker 'csharp)
  (setq imenu-create-index-function #'csharp--imenu-create-index-function)
  (imenu-add-to-menubar "Navigate"))

(eval-after-load "flycheck"
  (flycheck-define-checker csharp
    "A C# syntax checker for Mono."
    :command ("csbuild" source-inplace)
    :error-patterns
    ;; WinFormsHello.cs(17,9): error CS0246: The type or namespace blah blah. Are you missing an assembly reference?
    ((error   line-start (file-name) "(" line "," column "): error "   (message) line-end)
     (warning line-start (file-name) "(" line "," column "): warning " (message) line-end))
    :modes (csharp-mode)))

(easy-menu-define u-csharp-menu csharp-mode-map "C#"
  '("C#"
    ["Beginning Of Function" beginning-of-defun t]
    ["End Of Function"       end-of-defun       t]
    ["Mark Function"         c-mark-defun       t]
    "---"
    ["Backward Statement" c-beginning-of-statement t]
    ["Forward  Statement" c-end-of-statement       t]
    "---"
    ["Beginning Of Class" csharp-move-back-to-beginning-of-class t]
    ["End Of Class"       csharp-move-fwd-to-end-of-class        t]
    "---"
    ["Beginning Of Namespace" csharp-move-back-to-beginning-of-namespace t]
    ))

(setq csharp-want-imenu nil)

(add-hook 'csharp-mode-hook 'u-csharp-setup)
(add-hook 'csharp-mode-hook 'rainbow-delimiters-mode)
;;
(provide 'u-csharp)
(message "Loading u-csharp...done")

;;; u-csharp.el ends here
