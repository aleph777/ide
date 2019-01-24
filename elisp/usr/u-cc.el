;;; u-cc.el --- cperl-mode support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2018 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   15-Dec-1999

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

;; Revision: 23-Jun-2000 Changed substatement-open to 0
;;                       Added `usr-cc-menu'
;;           26-Jun-2000 Added define-key to remove C menu from `c-mode-map'
;;           02-Feb-2010 Added exit message
;;           30-Aug-2012 Fixed for modern cc-mode
;;           26-Mar-2015 Added `usr-c-setup', `usr-c++-setup', and `usr-java-setup'
;;                       Now using (c-add-style c-style-alist) instead of directly setting `c-style-alist'
;;           06-May-2015 Added `usr-doc-string'
;;                       Added explicit `usr-java-setup'
;;           18-Aug-2015 Removed style "tjf"; changed to "bsd" with `c-basic-offset' of 4
;;           28-Sep-2015 Added `usr-cc-reformat-buffer' functionality
;;           20-Oct-2015 Added `usr-cpp-comment-region'
;;           04-Dec-2015 Fixed `usr-cpp-comment-region'
;;           15-Jan-2016 Added semantic/imenu support to `usr-c-setup', `usr-c++-setup', and `usr-java-setup'
;;           17-Jan-2016 Updated for new standard user interface
;;           02-Feb-2016 Added `neotree'
;;           12-Feb-2016 Updated C and C++ Build menus
;;           29-Feb-2016 Changed from `usr-' to `u-'
;;                       Removed`neotree'
;;           03-Mar-2016 Changed `setq' to `setq-local' in `u-c-setup'
;;                       Updated to use `yasnippet'
;;           09-Mar-2016 Fixed `u-c-setup' and `u-java-setup' to sort `imenu'
;;           18-Apr-2016 Updated for `use-package'
;;           03-May-2016 Unbound local setting for `C-d'
;;           23-Jun-2016 Removed globally set `semantic-mode'
;;           07-Sep-2016 Added support for `ac-clang'
;;           14-Jan-2017 Removed support for `ac-clang'
;;           21-Mar-2017 Added ‘u-cc-insert-boilerplate’
;;                       Added ‘u-cc-insert-header-skeleton’
;;                       Added ‘u-cc-insert-source-skeleton’
;;           28-Apr-2017 Updated ‘u-cc-insert-boilerplate’, ‘u-cc-insert-header-skeleton’, ‘u-cc-insert-source-skeleton’
;;                       Updated ‘u-cc-mode-menu-text’
;;           02-May-2017 Added ‘u-cc-insert-summary’
;;                       Updated ‘u-cc-mode-menu-text’
;;           24-Jan-2019 Added ‘Go to Definition’ to ‘u-cc-mode-menu-text’
;;                       Added ‘Format File’ to ‘u-cc-mode-menu-text’
;;                       Removed ‘u-cc-reformat-buffer’ in favor of ‘eglot-format’
;;

;;; Code:

(message "Loading u-cc...")
(require 'cc-mode)
(require 'flycheck)
(require 'eglot)
(require 'u-macro)
;; (require 'ac-clang)

(defvar u-cc-indent-program "indent ")

(defvar u-cc-indent-options (concat
                             "-i4 "   ; set indentation level to 2 spaces
                             "-cli4 " ; case label indent of 2 spaces
                             "-ci4 "  ; Continuation indent of 2 spaces
                             "-st "   ; write to STDOUT
                             "-bad "  ; blank lines after declarations
                             "-bap "  ; blank lines after procedures
                             "-bli0 " ; indent braces 0 spaces
                             "-ncs "  ; do not put a space after cast operators
                             "-npcs " ; do not put space after the function in function calls
                             "-nut "  ; use spaces instead of tabs
                             "-npsl " ; put the type of a procedure on the same line as its name
                             "-fca "  ; format all comments
                             "-lc79 " ; set maximum line length for comment formatting to 79
                             "-fc1 "  ; format comments in the first column
                             "-sob "  ; swallow optional blank lines
                             "-lp "   ; line up continued lines at parentheses
                             "-nsai " ; do not put a space after every if
                             "-nsaf " ; do not put a space after every for
                             "-nsaw " ; do not put a space after every while
                             ))

(defvar u-cc-indent-command (concat u-cc-indent-program u-cc-indent-options))

(defvar u-cc-mode-menu-text
  '(
    ["Insert Header File Skeleton" u-cc-insert-header-skeleton   :active (is-rw?)]
    ["Insert Source File Skeleton" u-cc-insert-source-skeleton   :active (is-rw?)]
    ["Insert Boilerplate"          u-cc-insert-boilerplate       :active (is-rw?)]
    ["Insert Summary"              u-cc-insert-summary           :active (is-rw?)]
    ["Format File"                 eglot-format                  :active (is-rw?)]
    ["Flush Region Pragmas"       (flush-lines "pragma.+region") :active (is-rw?)] ;; what was this for?
    "---"
    ["Go to Definition" xref-find-definitions]
    "---"
    ["Beginning Of Function" beginning-of-defun :active t]
    ["End Of Function"       end-of-defun       :active t]
    ["Mark Function"         c-mark-function    :active t]
    "---"
    ["Fill Comment Paragraph"c-fill-paragraph :active t]
    ;;    ["Convert comment to docstring" u-docstring    :enable (or c++-mode java-mode)]
    "---"
    ["Backward Statement" c-beginning-of-statement :active t]
    ["Forward  Statement" c-end-of-statement       :active t]
    "---"
    ["Up Conditional"       c-up-conditional       :active t]
    ["Backward Conditional" c-backward-conditional :active t]
    ["Forward  Conditional" c-forward-conditional  :active t]
    ))

;; (defvar tutorial--point-before-chkeys 0
;;   "Point before display of key changes.")
;; (make-variable-buffer-local 'tutorial--point-before-chkeys)

(defvar u-cc-cflags "-Wall -g")
(make-variable-buffer-local 'u-cc-cflags)

(defvar u-cc-ldflags "-lm -pthread")
(make-variable-buffer-local 'u-cc-ldflags)

(defvar u-cc-makeflags "")
(make-variable-buffer-local 'u-cc-makeflags)

(defun u-c-setup()
  "C-mode setup function"
  (abbrev-mode -1)
  (flycheck-mode)
  (imenu-add-to-menubar "Navigate")
  ;;
  (setq flycheck-gcc-language-standard "c11"
        u-cc-cflags (concat " -std=c11 " u-cc-cflags))
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

(defun u-c++-setup ()
  "C++-mode setup function."
  (abbrev-mode -1)
  (flycheck-mode)
  (imenu-add-to-menubar "Navigate")
  ;;
  (setq flycheck-gcc-language-standard "c++11"
        u-cc-cflags (concat "-std=c++11 " u-cc-cflags)))

(defun u-java-setup ()
  "Java-mode setup function."
  (abbrev-mode -1)
  (flycheck-mode -1)
  (imenu-add-to-menubar "Navigate")
  ;;
  )

(defun u-docstring ()
  "Convert C++-style comments '^ *//' to a docstring."
  (interactive "*")
  (let* ((exp "^ *// *")
         (beg (progn
                (beginning-of-line)
                (and (looking-at exp) (progn (while (looking-at exp) (forward-line -1)) (forward-line 1) (point)))))
         (end (and beg (progn (while (looking-at exp) (forward-line 1)) (point)))))
    (if beg
        (let ((opn " /**\n")
              (com "  * ")
              (cls "  */\n"))
          (goto-char end)
          (insert cls)
          (save-excursion
            (goto-char end)
            (while (re-search-backward exp beg t)
              (replace-match com nil nil))
            (goto-char beg)
            (insert opn))
          (indent-region beg (point)))
      (error "Not in an eligible comment"))))

(defun u-cc-set-cflags ()
  "Allow the user to set `CFLAGS'."
  (interactive)
  (let ((flags (read-shell-command "Compiler Flags: " u-cc-cflags)))
    (unless (equal flags u-cc-cflags)
      (setq u-cc-cflags flags))))

(defun u-cc-set-ldflags ()
  "Allow the user to set `LDFLAGS'."
  (interactive)
  (let ((flags (read-shell-command "Linker Flags: " u-cc-ldflags)))
    (unless (equal flags u-cc-ldflags)
      (setq u-cc-ldflags flags))))

(defun u-cc-set-makeflags ()
  "Allow the user to set `command-line' arguments to `make'."
  (interactive)
  (let ((flags (read-shell-command "Make Flags: " u-cc-makeflags)))
    (unless (equal flags u-cc-makeflags)
      (setq u-cc-makeflags flags))))

(defun whitespace-respace-function()
  "Replaces all (non-line changing) series of whitespaces by a single space, then reindent"
  (interactive)
  (save-excursion
    ; from https://www.emacswiki.org/emacs/BasicNarrowing
    (save-restriction
      ; narrows regexp replace on region
      (c-mark-function)
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (while (re-search-forward "[[:space:]]+" nil t)
        (replace-match " " nil nil)
        )
      )
    (c-mark-function)
    (indent-region (region-beginning) (region-end) nil)))

(defun u-cc-insert-boilerplate (filename)
  "Insert a C/C++/Java module boilerplate for FILENAME."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat user-dir-home "elisp/templates/cc-skeleton.cpp")))
  (let ((year (format-time-string "%Y-%Y")))
    (save-excursion
      (search-forward "<<<FILENAME>>>" (point-max) t)
      (replace-match filename t)
      (search-forward "<<<CHOLDER>>>")
      (replace-match user-copyright-holder t)
      (search-forward "<<<YEAR>>>")
      (replace-match year t)
      (search-forward "<<<CHOLDER>>>") (replace-match user-copyright-holder t))))

(defun u-cc-insert-header-skeleton ()
  "Insert a header skeleton."
  (interactive "*")
  (goto-char (point-min))
  (let ((basename-h (concat (upcase (basename)) "_H")))
    (insert "#ifndef " basename-h "\n")
    (insert "#define " basename-h "\n\n")
    (insert "#endif\n")
    (u-cc-insert-boilerplate (basename-full))))

(defun u-cc-insert-source-skeleton ()
  "Insert a source file skeleton."
  (interactive "*")
  (goto-char (point-min))
  (let* ((dir        (basename (substring (dirname) 0 -1)))
         (basename-h (concat (basename) ".h"))
         (inc-file   (concat dir "/" basename-h)))
    (insert (concat "#include \"" inc-file "\"\n\n"))
    (u-cc-insert-boilerplate (basename-full))))

(defun u-cc-insert-summary ()
  "Insert a summary block at POINT."
  (interactive "*")
  (let ((end-point))
    (save-excursion
      (insert " /// <summary>\n")
      (insert " ///   \n")
      (insert " /// </summary>\n")
      (setq end-point (point)))
    (indent-region (point) end-point)))

(define-key c-mode-map    [menu-bar] nil)
(define-key c++-mode-map  [menu-bar] nil)
(define-key java-mode-map [menu-bar] nil)

(define-key c-mode-map    [(control d)] nil)
(define-key c++-mode-map  [(control d)] nil)
(define-key java-mode-map [(control d)] nil)

(easy-menu-define u-cpp-menu  c++-mode-map  "C++"  (append '("C++")  u-cc-mode-menu-text))
(easy-menu-define u-c-menu    c-mode-map    "C"    (append '("C")    u-cc-mode-menu-text))
(easy-menu-define u-java-menu java-mode-map "Java" (append '("Java") u-cc-mode-menu-text))

(easy-menu-define u-java-menu java-mode-map "Java Build"
  '("Build"
    ["Compile File" (compile (concat "javac -Xlint " (file-name-nondirectory (buffer-file-name)))) t]
    ))

(easy-menu-define u-cpp-build-menu c++-mode-map "C++ Build"
  '("Build"
    ["Syntax Check" (compile (concat "g++ " u-cc-cflags " -fsyntax-only " (file-name-nondirectory (buffer-file-name)))) t]
    ["Compile File" (let* ((fnn  (file-name-nondirectory   (buffer-file-name)))
                           (fnse (file-name-sans-extension fnn)))
                      (compile (concat "g++ " u-cc-cflags  " -c " fnn " -o " fnse ".o"))) t]
    ["Compile Program" (let* ((fnn  (file-name-nondirectory   (buffer-file-name)))
                              (fnse (file-name-sans-extension fnn)))
                         (compile (concat "g++ " u-cc-cflags " " u-cc-ldflags " " fnn " -o " fnse))) t]
    "---"
    ["Make"    (compile (concat "make " u-cc-makeflags)) t]
    ["Make..." compile                                     t]
    "---"
    ["Set Compiler Flags..." (u-cc-set-cflags)    t]
    ["Set Linker Flags..."   (u-cc-set-ldflags)   t]
    ["Set Make Flags..."     (u-cc-set-makeflags) t]
    ))

(easy-menu-define u-cpp-build-menu c-mode-map "C Build"
  '("Build"
    ["Syntax Check" (compile (concat "gcc " u-cc-cflags " -fsyntax-only " (file-name-nondirectory (buffer-file-name)))) t]
    ["Compile File" (let* ((fnn  (file-name-nondirectory   (buffer-file-name)))
                           (fnse (file-name-sans-extension fnn)))
                      (compile (concat "gcc " u-cc-cflags  " -c " fnn " -o " fnse ".o"))) t]
    ["Compile Program" (let* ((fnn  (file-name-nondirectory   (buffer-file-name)))
                              (fnse (file-name-sans-extension fnn)))
                         (compile (concat "gcc " u-cc-cflags " " u-cc-ldflags " " fnn " -o " fnse))) t]
    "---"
    ["Make"    (compile (concat "make " u-cc-makeflags)) t]
    ["Make..." compile                                     t]
    "---"
    ["Set Compiler Flags..." (u-cc-set-cflags)    t]
    ["Set Linker Flags..."   (u-cc-set-ldflags)   t]
    ["Set Make Flags..."     (u-cc-set-makeflags) t]
    ))
                                        ;
(setq c-default-style "bsd" c-basic-offset  4)
(c-set-offset 'case-label '+)

;; (ac-clang-initialize)

(add-hook 'c++-mode-hook  'u-c++-setup)
(add-hook 'c-mode-hook    'u-c-setup)
(add-hook 'java-mode-hook 'u-java-setup)

;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (when (featurep 'filladapt)
;; 	      (c-setup-filladapt))))

;; ("tjf"
;;   (c-basic-offset . 4)
;;   (c-comment-only-line-offset . 0)
;;   (c-offsets-alist
;;    (statement-block-intro . +)
;;    (knr-argdecl-intro . +)
;;    (substatement-open . 0)
;;    (label . -)
;;    (statement-cont . +)))
                                        ;
(message "Loading u-cc...done")
(provide 'u-cc)

;;; u-cc.el ends here

  
