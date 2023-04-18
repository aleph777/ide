;;; tjf-csharp.el --- CSharp major mode -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2015-2023 Tom Fontaine

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
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-csharp...")

(eval-when-compile
  (require 'csharp-mode)
  (require 'flycheck))

(defvar tjf:csharp/search-path (list "./" "../" "../../"))

(defvar tjf:csharp/build-menu-text
  '("Build"
    ["Refresh" beginning-of-defun :active t]
    "---"
    ))

(defvar tjf:csharp/build-menu nil)

(defsubst tjf:csharp/search-path (glob)
  "Return the search results for GLOB in ‘tjf:csharp/search-path’."
  (let ((dir-list tjf:csharp/search-path)
        (list))
    (dolist (element dir-list list)
      (setq list (append list (file-expand-wildcards (concat element glob)))))
    list))

(defun tjf:csharp/find-sln ()
  "Return a list of local solution files for this buffer."
  (tjf:csharp/search-path "*.sln"))

(defun tjf:csharp/find-proj ()
  "Return a list of local project files for this buffer."
  (tjf:csharp/search-path "*.csproj"))

;; (defun tjf:csharp/build-menu ()
;;   "Create a Build menu."
;;   (let ((sln-list  (tjf:csharp/find-sln))
;;         (proj-list (tjf:csharp/find-proj)))
;;     ))

(defun tjf:csharp/setup()
  "C#-mode setup function"
  ;; (easy-menu-define tjf:csharp/build-menu tjf:csharp/mode-map "Build" tjf:csharp/build-menu-text)
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

(easy-menu-define tjf:csharp/menu csharp-mode-map "C#"
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
    ["Beginning Of Namespace" csharp-move-back-to-beginning-of-namespace t]))

(setq csharp-want-imenu nil)

;;
(provide 'tjf-csharp)
(message "Loading tjf-csharp...done")

;;; tjf-csharp.el ends here
