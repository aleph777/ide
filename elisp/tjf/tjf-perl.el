;;; tjf-perl.el --- perl-mode support for GNU Emacs -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2026 Tom Fontaine

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

;;; Code:

(message "Loading tjf-perl...")
(require 'perl-ts-mode)
(require 'flycheck)
(require 's)
(require 'tjf-edit)
(require 'tjf-macro)

(eval-when-compile
  (require 'cape))

;;
(defvar tjf:perl/menu)
(setq   tjf:perl/menu
   '("Perl"
     ["Beginning Of Function" beginning-of-defun]
     ["End Of Function"       end-of-defun      ]
     ["Mark Function"         mark-defun        ]
     ["---" nil :visible t :enable nil]
     ["Insert Home Script Skeleton" (tjf:perl/insert-script-skeleton tjf:perl/template-file-script-header-home)]
     ["Insert Work Script Skeleton" (tjf:perl/insert-script-skeleton tjf:perl/template-file-script-header-work)]
     ["Insert Shebang"      tjf:perl/insert-shebang]
     ["Insert _ME_"         tjf:perl/insert-me     ]
     ;; ["Insert Script Usage" tjf:perl/insert-usage  ]
     ["---" nil :visible t :enable nil]
     ["Insert OO Module Template" tjf:perl/insert-oo-module-template]
     ["Insert FN Module Template" tjf:perl/insert-fn-module-template]
     ["---" nil :visible t :enable nil]
     ["Check Minimum Perl Version" tjf:perl/check-minimum-version]
     ["---" nil :visible t :enable nil]
     ["Check Mode Readiness" tjf:perl/ready]
     ))

(defvar tjf:perl/menu-build)
(setq   tjf:perl/menu-build
  '("Build"
    ["Syntax Check" tjf:perl/syntax-check]
    ["Critique"     (compile (concat "critique " (file-name-nondirectory (buffer-file-name))))]
    ))

(defvar tjf:perl/lib)
(setq   tjf:perl/lib (getenv "PERL5LIB"))

(defvar tjf:perl/shebang)
(setq   tjf:perl/shebang "#!/usr/bin/env -S perl   # -*-Perl-*-")

(defvar tjf:perl/me)
(setq   tjf:perl/me "use constant _ME_ => $0 =~ m=([^/]+)$=;")

(defvar tjf:perl/language-server)
(setq   tjf:perl/language-server "perl-lsp")

(defvar tjf:perl/template-file-script-header-home)
(setq   tjf:perl/template-file-script-header-home (concat tjf:user/dir-elisp "templates/perl-script-header-home.pl"))

(defvar tjf:perl/template-file-script-header-work)
(setq   tjf:perl/template-file-script-header-work (concat tjf:user/dir-elisp "templates/perl-script-header-work.pl"))

(defvar tjf:perl/template-file-fn-module)
(setq   tjf:perl/template-file-fn-module (concat tjf:user/dir-elisp "templates/perl-fn-module.pm"))

(defvar tjf:perl/template-file-oo-module)
(setq   tjf:perl/template-file-oo-module (concat tjf:user/dir-elisp "templates/perl-oo-module.pm"))

(defun tjf:perl/syntax-check ()
  "Check the syntax of the current buffer."
  (interactive)
  (compile (concat "PERL5LIB=" tjf:perl/lib ";perl -c " (file-name-nondirectory (buffer-file-name)))))

;; (defvar tjf:perl/template-script-usage)
;; (setq   tjf:perl/template-script-usage (concat tjf:user/dir-elisp "templates/perl-script-usage.pl"))

(defun tjf:perl/check-minimum-version ()
  "Check the minimum required version of the current file."
  (interactive)
  (message (get-shell-output (concat "check-minimum-version " (buffer-file-name)))))

;;;###autoload
(defun tjf:perl/convert ()
  "Convert the current file into a Perl script."
  (interactive "*")
  (message "tjf:perl/convert")
  (tjf:perl/insert-script-skeleton tjf:perl/template-file-script-header-work)
  (set-auto-mode))

(defun tjf:perl/fill-template ()
  "Complete the template in the current buffer."
  (let* ((dir     (basename (dirname buffer-file-name)))
         (base    (basename-no-ext buffer-file-name))
         (package (concat dir "::" base))
         (author  (user-full-name))
         (date    (format-time-string "%d-%b-%Y"))
         (year    (format-time-string "%Y")))
    (tjf:edit/fill-skeleton "<<<PACKAGE>>>" package)
    (tjf:edit/fill-skeleton "<<<YEAR>>>"    year)
    (tjf:edit/fill-skeleton "<<<AUTHOR>>>"  author)
    (tjf:edit/fill-skeleton "<<<DATE>>>"    date)))

(defun tjf:perl/insert-fn-module-template ()
  "Insert a template for a Functional module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents tjf:perl/template-file-fn-module))
  (tjf:perl/fill-template))

(defun tjf:perl/insert-me ()
  "Insert the `_ME_' variable declaration."
  (interactive "*")
  (insert (concat tjf:perl/me "\n\n")))

(defun tjf:perl/insert-oo-module-template ()
  "Insert a template for an Object-Oriented module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents tjf:perl/template-file-oo-module))
  (tjf:perl/fill-template))

(defun tjf:perl/insert-shebang ()
  "Insert the perl shebang at the top of the file."
  (interactive "*")
  (goto-char (point-min))
  (insert (concat tjf:perl/shebang "\n")))

(defun tjf:perl/insert-script-skeleton (header)
  "Insert HEADER script boilerplate at point."
  (interactive "*")
  (insert-file-contents header)
  (let ((author (user-full-name))
        ;; (title  (file-name-nondirectory (buffer-name)))
        (date   (format-time-string "%d-%b-%Y"))
        (year   (format-time-string "%Y")))
    (tjf:edit/fill-skeleton "<<<SHEBANG>>>" tjf:perl/shebang)
    (tjf:edit/fill-skeleton "<<<YEAR>>>"    year)
    (tjf:edit/fill-skeleton "<<<AUTHOR>>>"  author)
    (tjf:edit/fill-skeleton "<<<DATE>>>"    date)))

;; (defun tjf:perl/insert-usage ()
;;   "Insert the script usage code."
;;   (interactive "*")
;;   (insert-file-contents tjf:perl/template-script-usage))

(defun tjf:perl/ready ()
  "Display readiness of treesitter."
  (interactive)
  (message (if (treesit-ready-p 'perl) "Ready" "NOT ready")))

(defun tjf:perl/hook ()
  "Perl mode hook function."
  (abbrev-mode -1)
  (eglot-ensure)
  (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                 #'eglot-completion-at-point
                                 #'cape-keyword
                                 #'cape-dabbrev
                                 #'cape-file)))
  (imenu-add-to-menubar "Navigate"))

(defun tjf:perl/config ()
  "Perl mode config function."
  (if (eq major-mode 'cperl-mode)
      (progn
        (define-key cperl-mode-map [(control ?h) ?f] nil)
        (define-key cperl-mode-map [(control ?h) ?v] nil)

        ;; (add-to-list 'eglot-server-programs '(cperl-mode . (tjf:perl/language-server)))

        (easy-menu-define tjf-perl-menu   cperl-mode-map "Perl"  tjf:perl/menu)
        (easy-menu-define perl-build-menu cperl-mode-map "Build" tjf:perl/menu-build))

    (define-key perl-ts-mode-map [(control ?h) ?f] nil)
    (define-key perl-ts-mode-map [(control ?h) ?v] nil)

    ;; (add-to-list 'eglot-server-programs '(perl-ts-mode . ,tjf:perl/language-server))

    (easy-menu-define tjf-perl-menu   perl-ts-mode-map "Perl"  tjf:perl/menu)
    (easy-menu-define perl-build-menu perl-ts-mode-map "Build" tjf:perl/menu-build))

  ;; (treesit-install-language-grammar 'perl)

  (if tjf:perl/lib
      (setq flycheck-perl-include-path (split-string tjf:perl/lib ":"))
    (let ((lib-home  (concat tjf:user/dir-home "lib"))
          (lib-local (concat tjf:user/dir-home "local/lib"))
          (lib-perl5 (concat tjf:user/dir-home "perl5/lib/perl5")))  ;; /home/fontaine/perl5/lib/perl5
      (setq flycheck-perl-include-path '(lib-home lib-local lib-perl5))
      (setq tjf:perl/lib (join ":" (list lib-home lib-local lib-perl5)))))
  (add-to-list 'eglot-server-programs '((perl-ts-mode cperl-mode) . ("perl-lsp"))))

;;
(message "Loading tjf-perl...done")
(provide 'tjf-perl)

;;; tjf-perl.el ends here
