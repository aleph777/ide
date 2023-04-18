;;; tjf-perl.el --- cperl-mode support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2023 Tom Fontaine

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

;; Revision: 22-Jun-2000 Deleted ‘usr-perl-menu’ defvar and used ‘easy-menu-define’ instead of ‘easy-menu-add-item’
;;           23-Jun-2000 Added ‘usr-perl-shebang’, ‘usr-perl-insert-shebang’, changed defun names to ‘usr-perl-...’
;;           26-Jun-2000 Modified ‘usr-perl-insert-script-version’ to use trailing \n for used modules
;;           17-Feb-2005 Modified ‘usr-perl-insert-script-version’ to use TJF instead of ACC
;;           18-Apr-2006 Changed ‘usr-perl-shebang’ to use /local/bin/perl
;;           01-Jun-2006 Changed ‘usr-perl-insert-script-version’ to use MDC instead of TJF
;;           22-Jun-2006 Added ‘usr-perl-bin’
;;                       Changed ‘usr-perl-shebang’ to use ‘usr-perl-bin’
;;           25-Aug-2006 Changed ‘usr-perl-insert-shebang’ to detect for remote file name and adjust shebang
;;           02-Feb-2010 Added exit message
;;           25-Mar-2015 Updated ‘usr-perl-insert’ functions and associated menu entries
;;           03-Apr-2015 Added ‘usr-perl-insert-me’ and associated menu entry
;;                       Updated ‘usr-perl-insert-usage’ to use ‘usr-perl-insert-me’
;;           14-Apr-2015 Added ‘usr-perl-insert-oo-module-template’ and ‘usr-perl-insert-fn-module-template’
;;                       Revised associated menu entries
;;           21-Apr-2015 Fixed regex in ‘usr-perl-insert-usage’
;;                       Updated ‘usr-perl-insert-me’ to use ‘$__ME__’
;;           09-Dec-2015 Set TAB to ‘indent-for-tab-command’
;;           17-Jan-2016 Updated for new standard user interface
;;           02-Feb-2016 Added ‘neotree’
;;           23-Feb-2016 Added ‘cperl-init-faces’
;;                       Removed ‘neotree’
;;           28-Feb-2016 Refactored as ‘tjf-perl’
;;           03-Mar-2016 Updated to use ‘yasnippet’
;;           09-Mar-2016 Added use ‘v5.10’ to ‘perl-insert-script-header’
;;           18-Apr-2016 Updated for ‘use-package’
;;           23-Jun-2016 Removed globally set ‘semantic-mode’
;;           07-Sep-2016 Added ‘plsense’ support
;;           14-Nov-2016 Added ‘perl-init-faces’ to fix syntax highlighting
;;           18-Jan-2017 Removed ‘plsense’ support
;;                       Updated header formats
;;           19-Jan-2017 Disabled ‘abbrev-mode’
;;           03-Jul-2017 Set ‘flycheck-perl-include-path’ in ‘perl-setup’
;;           05-Oct-2020 Removed ‘perl-init-faces’
;;           03-Feb-2021 ‘tjf’ overhaul
;;           07-Apr-2021 Updated ‘tjf:perl/fill-out-template’
;;           24-Sep-2022 Fixed ‘tjf:perl/which’
;;

;;; Code:

(message "Loading tjf-perl...")
(require 'cperl-mode)
(require 'tjf-date)
(require 'tjf-edit)
(require 'tjf-frame)

(eval-when-compile
  (require 'cape))

;;; overload
(defun cperl-define-key () nil)

(define-key cperl-mode-map [(control ?h) ?f] nil)
(define-key cperl-mode-map [(control ?h) ?v] nil)

(defvar tjf:perl/lib         (getenv "PERLLIB"))
(defvar tjf:perl/me          "use constant _ME_ => $0 =~ m=([^/]+)$=;\n\n")
(defvar tjf:perl/min-version "5.010")
(defvar tjf:perl/which       (shell-command-to-string "which perl"))
(defvar tjf:perl/shebang     (concat "#!" tjf:perl/which " -w  # -*-Perl-*-\n"))

;;;###autoload
(defun tjf:perl/convert ()
  "Convert the current file into a Perl script."
  (interactive "*")
  (tjf:perl/insert-script-skeleton)
  (set-auto-mode))

(defun tjf:perl/fill-out-template ()
  "Replace template fields with the associated data."
  (let* ((dirbase (file-name-nondirectory (directory-file-name default-directory)))
         (modbase (replace-regexp-in-string ".pm$" "" (file-name-nondirectory (buffer-file-name))))
         (package (if (not (string= dirbase "lib")) (concat dirbase "::" modbase) modbase))
         (year    (format-time-string "%Y-%Y"))
         (author  (user-full-name))
         (date    (tjf:date/today tjf:date/dd-mon-yyyy)))
    (tjf:edit/fill-skeleton "<<<PACKAGE>>>" package)
    (tjf:edit/fill-skeleton "<<<YEAR>>>"    year)
    (tjf:edit/fill-skeleton "<<<AUTHOR>>>"  author)
    (tjf:edit/fill-skeleton "<<<DATE>>>"    date)))

(defun tjf:perl/indent-function ()
  "Indentation function for `perl-mode'."
  (interactive "*")
  (save-excursion
    (beginning-of-defun)
    (cperl-indent-exp)))

(defun tjf:perl/insert-me ()
  "Insert the `_ME_' variable declaration."
  (interactive "*")
  (insert "use constant _ME_ => $0 =~ m=([^/]+)$=;\n\n"))

(defun tjf:perl/insert-fn-module-template ()
  "Insert a template for an Functional module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat tjf:user/dir-elisp "templates/perl-fn-module.pm")))
  (tjf:perl/fill-out-template))

(defun tjf:perl/insert-module-header ()
  "Insert a header for a Perl module."
  (interactive "*")
  (save-excursion
    (insert-file-contents (concat tjf:user/dir-elisp "templates/perl-module-header.pm")))
  (tjf:perl/fill-out-template))

(defun tjf:perl/insert-oo-module-template ()
  "Insert a template for an Object-Oriented module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat tjf:user/dir-elisp "templates/perl-oo-module.pm")))
  (tjf:perl/fill-out-template))

(defun tjf:perl/insert-script-header ()
  "Insert script boilerplate at point."
  (interactive "*")
  (insert-file-contents (concat tjf:user/dir-elisp "templates/perl-script-header.pl"))
  (let* ((year   (format-time-string "%Y-%Y"))
         (author (user-full-name))
         (title  (file-name-nondirectory (buffer-name)))
         (date   (tjf:date/today tjf:date/dd-mon-yyyy)))
    (save-excursion
      (search-forward "<<<YEAR>>>")
      (replace-match year t)
      (search-forward "<<<AUTHOR>>>")
      (replace-match author t)
      (search-forward "<<<TITLE>>>")
      (replace-match title t)
      (search-forward "<<<DATE>>>")
      (replace-match date t))))

(defun tjf:perl/insert-script-skeleton ()
  "Insert the perl shebang and script boilerplate at the top of the file."
  (interactive "*")
  (save-excursion
    (tjf:perl/insert-shebang)
    (tjf:perl/insert-script-header)))

(defun tjf:perl/insert-shebang ()
  "Insert the perl shebang at the top of the file."
  (interactive "*")
  (goto-char (point-min))
  (insert tjf:perl/shebang))

(defun tjf:perl/insert-usage ()
  "Insert the script usage code."
  (interactive "*")
  (tjf:perl/insert-me)
  (insert-file-contents (concat tjf:user/dir-elisp "templates/perl-script-usage.pl")))

(defun tjf:perl/setup()
  "Set up a buffer for ‘perl-mode’."
  (abbrev-mode -1)
  (add-hook 'completion-at-point-functions #'cape-keyword nil 'local)
  (imenu-add-to-menubar "Navigate")
  (unless tjf:perl/lib
    (setq tjf:perl/lib (concat tjf:user/dir-home "lib:" tjf:user/dir-home "local/lib")))
  (setq flycheck-perl-include-path (split-string tjf:perl/lib ":"))
  (flycheck-mode))

;;
(easy-menu-define tjf:perl/menu cperl-mode-map "tjf-Perl"
  '("Perl"
    ["Beginning Of Function" beginning-of-defun]
    ["End Of Function"       end-of-defun      ]
    ["Mark Function"         mark-defun        ]
    "---"
    ["Insert Shebang"         tjf:perl/insert-shebang        ]
    ["Insert Script Header"   tjf:perl/insert-script-header  ]
    ["Insert Script Skeleton" tjf:perl/insert-script-skeleton]
    ["Insert _ME_"            tjf:perl/insert-me             ]
    ["Insert Script Usage"    tjf:perl/insert-usage          ]
    "---"
    ["Insert OO Module Template" tjf:perl/insert-oo-module-template]
    ["Insert FN Module Template" tjf:perl/insert-fn-module-template]
    ["Insert Module Header"      tjf:perl/insert-module-header     ]
    "---"
    ["Check Minimum Perl Version" (message (substring (shell-command-to-string (concat "check-minimum-version " (buffer-file-name))) 0 -1))]
    ))

(easy-menu-define tjf:perl/menu-build cperl-mode-map "Perl Build"
  '("Build"
    ["Syntax Check" (compile (concat "export PERLLIB=" tjf:perl/lib ";" tjf:perl/which " -c " (file-name-nondirectory (buffer-file-name))))]
    ["Critique"     (compile (concat "critique " (file-name-nondirectory (buffer-file-name))))]
    ))

;;
(message "Loading tjf-perl...done")
(provide 'tjf-perl)

;;; tjf-perl.el ends here
