;;; u-perl.el --- cperl-mode support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2021 Tom Fontaine

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
;;           28-Feb-2016 Refactored as ‘u-perl’
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
;;

;;; Code:


(message "Loading u-perl...")
(require 'cperl-mode)
(require 'u-frame)
(require 'u-date)
(require 'u-variables)

(defvar perl-which       "/usr/bin/perl")
(defvar perl-shebang     (concat "#!" perl-which " -w    # -*-Perl-*-\n"))
(defvar perl-min-version "5.010")

(defun cperl-define-key () nil)

(defun perl-setup()
  "`eval-after-load' target for perl-mode."
  (imenu-add-to-menubar "Navigate")
  (abbrev-mode -1)
  (let ((perllib (getenv "PERLLIB")))
    (unless perllib
      (setq perllib (concat user-dir-home "lib:" user-dir-home "local/lib")))
    (setq flycheck-perl-include-path (split-string perllib ":")))
  (flycheck-mode))

(defun perl-insert-shebang ()
  "Insert the perl shebang at the top of the file."
  (interactive "*")
  (goto-char (point-min))
  (insert perl-shebang))

(defun perl-insert-script-header ()
  "Insert script boilerplate at point."
  (interactive "*")
  (insert-file-contents (concat user-dir-home "elisp/templates/perl-script-header.pl"))
  (let* ((year   (format-time-string "%Y-%Y"))
         (author (user-full-name))
         (title  (file-name-nondirectory (buffer-name)))
         (date   (get-dd-mon-yyyy)))
    (save-excursion
      (search-forward "<<<YEAR>>>")
      (replace-match year t)
      (search-forward "<<<AUTHOR>>>")
      (replace-match author t)
      (search-forward "<<<TITLE>>>")
      (replace-match title t)
      (search-forward "<<<AUTHOR>>>")
      (replace-match author t)
      (search-forward "<<<DATE>>>")
      (replace-match date t))))

(defun perl-insert-script-skeleton ()
  "Insert the perl shebang and script boilerplate at the top of the file."
  (interactive "*")
  (save-excursion
    (perl-insert-shebang)
    (perl-insert-script-header)))

(defun perl-insert-me ()
  "Insert the `_ME_' variable declaration."
  (interactive "*")
  (insert "use constant _ME_ => $0 =~ m=([^/]+)$=;\n\n"))

(defun perl-insert-usage ()
  "Insert the script usage code."
  (interactive "*")
  (perl-insert-me)
  (insert "if((@ARGV == 0) || ($ARGV[0] =~ /(?^:^(?:-(?:(?:-(?:h(?:elp)?|\\?)|h(?:elp)?)$|\\?)|\\?$))/))\n")
  (insert "{\n")
  (insert "  print \"\\nUsage: $me\\n\\n\";\n")
  (insert "  exit;\n")
  (insert "}\n"))

;;;###autoload
(defun convert-to-perl ()
  "Convert the current file into a Perl script."
  (interactive "*")
  (perl-insert-script-skeleton)
  (set-auto-mode)
  (reset-frame-size))

(defun perl-fill-out-template ()
  "Replace template fields with the associated data."
  (let* ((dirbase   (file-name-nondirectory (directory-file-name default-directory)))
         (modbase   (replace-regexp-in-string ".pm$" "" (file-name-nondirectory (buffer-file-name))))
         (package   (if (not (equal dirbase "lib")) (concat dirbase "::" modbase) modbase))
         (year   (format-time-string "%Y-%Y"))
         (author (user-full-name))
         (date   (get-dd-mon-yyyy)))
    (search-forward "<<<PACKAGE>>>")
    (replace-match package t)
    (search-forward "<<<YEAR>>>")
    (replace-match year t)
    (search-forward "<<<AUTHOR>>>")
    (replace-match author t)
    (search-forward "<<<AUTHOR>>>")
    (replace-match author t)
    (search-forward "<<<DATE>>>")
    (replace-match date t)
    (if (search-forward "<<<PACKAGE>>>" (point-max) t)
        (replace-match package t))
    ))

(defun perl-insert-oo-module-template ()
  "Insert a template for an Object-Oriented module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat user-dir-home "elisp/templates/perl-oo-module.pm")))
  (perl-fill-out-template))

(defun perl-insert-fn-module-template ()
  "Insert a template for an Functional module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat user-dir-home "elisp/templates/perl-fn-module.pm")))
  (perl-fill-out-template))

(defun perl-insert-module-header ()
  "Insert a header for a Perl module."
  (interactive "*")
  (save-excursion
    (insert-file-contents (concat user-dir-home "elisp/templates/perl-module-header.pm")))
  (perl-fill-out-template))

(defun perl-indent-function ()
  "Indentation function for `perl-mode'."
  (interactive "*")
  (save-excursion
    (beginning-of-defun)
    (cperl-indent-exp)))

(setq cperl-indent-region-fix-constructs nil cperl-hairy t
      cperl-style-alist (append cperl-style-alist '(("TJF"
                                                     (cperl-indent-level               .  2)
                                                     (cperl-brace-offset               .  0)
                                                     (cperl-continued-brace-offset     . -2)
                                                     (cperl-label-offset               . -2)
                                                     (cperl-extra-newline-before-brace .  t)
                                                     (cperl-merge-trailing-else        .  nil)
                                                     (cperl-continued-statement-offset .  2)))))

(cperl-set-style "TJF")
(cperl-init-faces)

(define-key cperl-mode-map [menu-bar] nil)
(define-key cperl-mode-map [?\t] '(lambda nil
                                    (interactive)
                                    (if mark-active
                                        (indent-region
                                         (region-beginning)
                                         (region-end))
                                      (indent-for-tab-command))))

(easy-menu-define u-perl-menu cperl-mode-map "U-Perl"
  '("Perl"
    ["Beginning Of Function" beginning-of-defun :active t]
    ["End Of Function"       end-of-defun       :active t]
    ["Mark Function"         mark-defun         :active t]
    "---"
    ["Insert Shebang"         perl-insert-shebang         :active t]
    ["Insert Script Header"   perl-insert-script-header   :active t]
    ["Insert Script Skeleton" perl-insert-script-skeleton :active t]
    ["Insert _ME_"            perl-insert-me              :active t]
    ["Insert Script Usage"    perl-insert-usage           :active t]
    "---"
    ["Insert OO Module Template" perl-insert-oo-module-template :active t]
    ["Insert FN Module Template" perl-insert-fn-module-template :active t]
    ["Insert Module Header"      perl-insert-module-header      :active t]
    "---"
    ["Check Minimum Perl Version" (message (substring (shell-command-to-string (concat "check-minimum-version " (buffer-file-name))) 0 -1))]
    ))

(easy-menu-define u-perl-build-menu cperl-mode-map "Perl Build"
  '("Build"
    ["Syntax Check" (compile (concat "export PERLLIB=$HOME/lib:$HOME/local/lib;/usr/bin/perl -c "  (file-name-nondirectory (buffer-file-name)))) :active t]
    ["Critique"     (compile (concat "critique " (file-name-nondirectory (buffer-file-name)))) :active t]
    ))

(add-hook 'cperl-mode-hook 'perl-setup)
;;
(message "Loading u-perl...done")
(provide 'u-perl)

;;; u-perl.el ends here
