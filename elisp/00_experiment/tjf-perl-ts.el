;;; tjf-perl-ts.el --- cperl-mode support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2025 Tom Fontaine

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

;; Revision: 22-Jun-2000 deleted ‘usr-perl-menu’ defvar and used ‘easy-menu-define’ instead of ‘easy-menu-add-item’
;;           23-Jun-2000 added ‘usr-perl-shebang’, ‘usr-perl-insert-shebang’, changed defun names to ‘usr-perl-...’
;;           26-Jun-2000 modified ‘usr-perl-insert-script-version’ to use trailing \n for used modules
;;           17-Feb-2005 modified ‘usr-perl-insert-script-version’ to use TJF instead of ACC
;;           18-Apr-2006 changed ‘usr-perl-shebang’ to use /local/bin/perl
;;           01-Jun-2006 changed ‘usr-perl-insert-script-version’ to use MDC instead of TJF
;;           22-Jun-2006 added ‘usr-perl-bin’
;;                       changed ‘usr-perl-shebang’ to use ‘usr-perl-bin’
;;           25-Aug-2006 changed ‘usr-perl-insert-shebang’ to detect for remote file name and adjust shebang
;;           02-Feb-2010 added exit message
;;           25-Mar-2015 updated ‘usr-perl-insert’ functions and associated menu entries
;;           03-Apr-2015 added ‘usr-perl-insert-me’ and associated menu entry
;;                       updated ‘usr-perl-insert-usage’ to use ‘usr-perl-insert-me’
;;           14-Apr-2015 added ‘usr-perl-insert-oo-module-template’ and ‘usr-perl-insert-fn-module-template’
;;                       revised associated menu entries
;;           21-Apr-2015 fixed regex in ‘usr-perl-insert-usage’
;;                       updated ‘usr-perl-insert-me’ to use ‘$__ME__’
;;           09-Dec-2015 set TAB to ‘indent-for-tab-command’
;;           17-Jan-2016 updated for new standard user interface
;;           02-Feb-2016 added ‘neotree’
;;           23-Feb-2016 added ‘cperl-init-faces’
;;                       removed ‘neotree’
;;           28-Feb-2016 refactored as ‘tjf-perl’
;;           03-Mar-2016 updated to use ‘yasnippet’
;;           09-Mar-2016 added use ‘v5.10’ to ‘perl-insert-script-header’
;;           18-Apr-2016 updated for ‘use-package’
;;           23-Jun-2016 removed globally set ‘semantic-mode’
;;           07-Sep-2016 added ‘plsense’ support
;;           14-Nov-2016 added ‘perl-init-faces’ to fix syntax highlighting
;;           18-Jan-2017 removed ‘plsense’ support
;;                       updated header formats
;;           19-Jan-2017 disabled ‘abbrev-mode’
;;           03-Jul-2017 set ‘flycheck-perl-include-path’ in ‘perl-setup’
;;           05-Oct-2020 removed ‘perl-init-faces’
;;           03-Feb-2021 ‘tjf’ overhaul
;;           07-Apr-2021 updated ‘tjf:perl/fill-out-template’
;;           24-Sep-2022 fixed ‘tjf:perl/which’
;;           02-Jun-2023 major clean up
;;           06-Jun-2023 changed from ‘tjf:perl/setup’ to ‘tjf:perl/hook’ and ‘tjf:perl/config’
;;           12-Jun-2023 fixed ‘tjf:perl/shebang’
;;           22-Mar-2025 ‘perl-ts-mode’
;;

;;; Code:

(message "Loading tjf-perl-ts...")
(require 'flycheck)
(require 's)
(require 'tjf-edit)
(require 'tjf-frame)

(eval-when-compile
  (require 'cape))

;;; overload
(defun cperl-define-key () nil)

(defvar tjf:perl-ts/lib)
(setq   tjf:perl-ts/lib (getenv "PERLLIB"))

(defvar tjf:perl-ts/me)
(setq   tjf:perl-ts/me "use constant _ME_ => $0 =~ m=([^/]+)$=;")

(defvar tjf:perl-ts/shebang)
(setq   tjf:perl-ts/shebang "#!/usr/bin/env -S perl   # -*-Perl-*-")

(defvar tjf:perl-ts/template-file-script-header-home)
(setq   tjf:perl-ts/template-file-script-header-home (concat tjf:user/dir-elisp "templates/perl-script-header-home.pl"))

(defvar tjf:perl-ts/template-file-script-header-work)
(setq   tjf:perl-ts/template-file-script-header-work (concat tjf:user/dir-elisp "templates/perl-script-header-work.pl"))

(defvar tjf:perl-ts/template-file-fn-module)
(defvar tjf:perl-ts/template-file-fn-module (concat tjf:user/dir-elisp "templates/perl-fn-module.pm"))

(defvar tjf:perl-ts/template-file-oo-module)
(defvar tjf:perl-ts/template-file-oo-module (concat tjf:user/dir-elisp "templates/perl-oo-module.pm"))

;; (defvar tjf:perl-ts/template-script-usage)
;; (setq   tjf:perl-ts/template-script-usage (concat tjf:user/dir-elisp "templates/perl-script-usage.pl"))

(defvar tjf:perl-ts/which)
(setq   tjf:perl-ts/which (string-trim-right (shell-command-to-string "which perl")))

(defun tjf:perl-ts/check-minimum-version ()
  "Check the minimum required version of the current file."
  (interactive)
  (message (substring (shell-command-to-string (concat "check-minimum-version " (buffer-file-name))) 0 -1)))

;;;###autoload
(defun tjf:perl-ts/convert ()
  "Convert the current file into a Perl script."
  (interactive "*")
  (message "tjf:perl-ts/convert")
  (tjf:perl-ts/insert-script-skeleton tjf:perl-ts/template-file-script-header-work)
  (set-auto-mode))

(defun tjf:perl-ts/insert-fn-module-template ()
  "Insert a template for a Functional module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents tjf:perl-ts/template-file-fn-module))
  (tjf:perl-ts/fill-out-template))

(defun tjf:perl-ts/insert-me ()
  "Insert the `_ME_' variable declaration."
  (interactive "*")
  (insert (concat tjf:perl-ts/me "\n\n")))

(defun tjf:perl-ts/insert-oo-module-template ()
  "Insert a template for an Object-Oriented module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents tjf:perl-ts/template-file-oo-module))
  (tjf:perl-ts/fill-out-template))

(defun tjf:perl-ts/insert-shebang ()
  "Insert the perl shebang at the top of the file."
  (interactive "*")
  (goto-char (point-min))
  (insert (concat tjf:perl-ts/shebang "\n")))

(defun tjf:perl-ts/insert-script-skeleton (header)
  "Insert HEADER script boilerplate at point."
  (interactive "*")
  (insert-file-contents header)
  (let ((author (user-full-name))
        (title  (file-name-nondirectory (buffer-name)))
        (date   (format-time-string "%d-%b-%Y"))
        (year   (format-time-string "%Y")))
    (tjf:edit/fill-skeleton "<<<SHEBANG>>>" tjf:perl-ts/shebang)
    (tjf:edit/fill-skeleton "<<<YEAR>>>"    year)
    (tjf:edit/fill-skeleton "<<<AUTHOR>>>"  (user-full-name))
    (tjf:edit/fill-skeleton "<<<TITLE>>>"   title)
    (tjf:edit/fill-skeleton "<<<DATE>>>"    date)))

;; (defun tjf:perl-ts/insert-usage ()
;;   "Insert the script usage code."
;;   (interactive "*")
;;   (insert-file-contents tjf:perl-ts/template-script-usage))

(defun tjf:perl-ts/hook ()
  "Perl mode hook function."
  (abbrev-mode -1)
  ;; (eglot-ensure)
  ;; (setq-local completion-at-point-functions (cons #'eglot-completion-at-point completion-at-point-functions))
  (imenu-add-to-menubar "Navigate"))

(defun tjf:perl-ts/config ()
  "Perl mode config function."
  (define-key cperl-mode-map [(control ?h) ?f] nil)
  (define-key cperl-mode-map [(control ?h) ?v] nil)

  (if tjf:perl-ts/lib
      (setq flycheck-perl-include-path (split-string tjf:perl-ts/lib ":"))
    (let ((lib-home  (concat tjf:user/dir-home "lib"))
          (lib-local (concat tjf:user/dir-home "local/lib")))
      (setq flycheck-perl-include-path '(lib-home lib-local))
      (setq tjf:perl-ts/lib (join ":" `(lib-home lib-local)))))
  ;; (setq perl-ts-indent-settings
  ;;       `((perl
  ;;          ((parent-is "source_file") column-0 0)

  ;;          ;; We want indenting like:
  ;;          ;; my ($a,
  ;;          ;;     $b) = @_

  ;;          ((lambda (node parent _)
  ;;             (and (string= "variable_declaration" (treesit-node-type parent))
  ;;                  (treesit-node-prev-sibling node t)))
  ;;           prev-sibling 0)

  ;;          ((parent-is "list_expression\\|anonymous_hash_expression") parent 0)
  ;;          ((node-is "}")                   standalone-parent 0)
  ;;          ((node-is ")")                   standalone-parent 2)
  ;;          ((parent-is "heredoc_content\\|string_content") (lambda (_ _ bol) bol) 0)
  ;;          ((node-is "'\\|\"")              column-0 0)
  ;;          ((parent-is "block")             standalone-parent 2)
  ;;          ((parent-is "binary_expression") standalone-parent 0)
  ;;          (catch-all                       parent 0))))
  )

;;
(easy-menu-define tjf:perl-ts/menu perl-ts-mode-map "tjf-perl-ts"
  '("Perl"
    ["Beginning Of Function" beginning-of-defun]
    ["End Of Function"       end-of-defun      ]
    ["Mark Function"         mark-defun        ]
    "---"
    ["Insert Home Script Skeleton" (tjf:perl-ts/insert-script-skeleton tjf:perl-ts/template-file-script-header-home)]
    ["Insert Work Script Skeleton" (tjf:perl-ts/insert-script-skeleton tjf:perl-ts/template-file-script-header-work)]
    ["Insert Shebang"      tjf:perl-ts/insert-shebang]
    ["Insert _ME_"         tjf:perl-ts/insert-me     ]
    ;; ["Insert Script Usage" tjf:perl-ts/insert-usage  ]
    "---"
    ["Insert OO Module Template" tjf:perl-ts/insert-oo-module-template]
    ["Insert FN Module Template" tjf:perl-ts/insert-fn-module-template]
    "---"
    ["Check Minimum Perl Version" tjf:perl-ts/check-minimum-version]
    ))

(easy-menu-define tjf:perl-ts/menu-build perl-ts-mode-map "Perl Build"
  '("Build"
    ["Syntax Check" (compile (concat "export PERLLIB=" tjf:perl-ts/lib ";" tjf:perl-ts/which " -c " (file-name-nondirectory (buffer-file-name))))]
    ["Critique"     (compile (concat "critique " (file-name-nondirectory (buffer-file-name))))]
    ))

;;
(message "Loading tjf-perl-ts...done")
(provide 'tjf-perl-ts)

;;; tjf-perl-ts.el ends here
