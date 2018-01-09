;;; default.el --- Global initialization for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2000-2018 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   19-Sep-2000

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

;; Revision: 01-Mar-2001 Revised to support Emacs and Xemacs
;;           05-Apr-2001 Changed autoload for new version of ‘tinysearch’
;;           29-Nov-2001 Changed ‘message-log-max’ from 50 to 512
;;           03-Dec-2001 Added ‘usr-toolbar’
;;           06-Dec-2001 Removed ‘ishl’ (now included as ‘isearch-lazy-highlight’) and fixed ‘isearch’ faces
;;                       Removed ‘show-paren-mode’ that conflicted with ‘mic-paren’
;;           04-Feb-2005 Added comint hook for ‘usr-comint-setup’
;;                       Added ‘eval-after-load’ for ‘comint’ (usr-comint)
;;           21-Mar-2007 Reworked mouse set up
;;           09-Sep-2009 Added ‘develock’
;;           02-Feb-2010 Removed ‘develock’
;;           11-Feb-2011 Removed ‘redo’ - added ‘redo+’
;;                       Added ‘recentf-mode’
;;                       Updated default setting for Emacs 22 changes (many obsolete settings removed)
;;           30-Aug-2012 Removed ‘c-mode-hook’ and ‘c++-mode-hook’ (usr-cc-setup)
;;                       Added ".cpp" to ‘auto-mode-alist’
;;           26-Dec-2012 Added ‘eshell-mode-hook’
;;           13-Aug-2013 Added ‘csharp-mode’ support
;;           26-Sep-2013 Moved mouse button definitions to ‘keys.el’
;;           21-Oct-2013 Added ‘csharp-mode’ to ‘msb-menu-cond’
;;           19-Feb-2014 Added ‘matlab-mode’ support
;;                       Added ‘matlab-mode’ to ‘msb-menu-cond’
;;                       Added loading messages for elisp/ext files
;;                       Added xz support to ‘jka-compr-compression-info-list’
;;           03-Mar-2014 Added ‘clips-mode’ support
;;                       Added ‘clips-mode’ to ‘msb-menu-cond’
;;           09-Apr-2014 Added .proto to c++ ‘auto-mode-alist’ entry
;;           02-May-2014 Added python to ‘msb-menu-cond’, sorted mode specific buffers
;;           06-May-2014 Added ‘log-mode’ support
;;                       Added ‘log-mode’ to ‘msb-menu-cond’
;;           09-May-2014 Added ‘nxml-mode’ to ‘msb-menu-cond’
;;           23-May-2014 Added ‘cua-mode’ minor mode
;;                       Removed .pl, .pm and sh-mode entries from ‘auto-mode-alist’
;;                       Added ‘undo-tree’
;;           24-May-2014 Added ‘rainbow-delimiters’
;;           25-May-2014 Added ‘uniquify’
;;           27-May-2014 Removed ‘scroll-in-place’ in favor of setting ‘scroll-preserve-screen-position’
;;                       Added ‘electric-pair-mode’
;;                       Added ‘scroll-error-top-bottom’
;;                       Added ‘smooth-scrolling’
;;           30-May-2014 Changed ‘rainbow-delimiters’ to use specific mode hooks
;;           13-Jun-2014 Changed to ‘usr-clips-mode’
;;           07-Jul-2014 Removed ‘rainbow-delimiters’ from cperl-mode-hook
;;                       Added .debug as a suffix for log-mode in ‘auto-mode-alist’
;;           31-Jul-2014 Added .proto as a suffix for c++-mode in ‘auto-mode-alist’
;;           01-Aug-2014 Added ‘before-save-hook’ to untabify buffer
;;           17-Sep-2014 Added .xaml to nxml ‘auto-mode-alist’ entry
;;                       Added ‘eval-after-load’ for ‘flymake’ to ‘csharp-mode’
;;           13-Nov-2014 Added ‘atim-unscroll’
;;           26-Mar-2015 Added Java entry to ‘msb-menu-cond’
;;           27-Mar-2015 Now using aliases in place of ‘usr-cc-setup’
;;           01-Apr-2015 Set ‘left-fringe’ and ‘right-fringe’ to 1 in ‘after-make-frame-functions’
;;           02-Apr-2015 Cleaned up loading messages
;;           10-Apr-2015 Added Ruby support
;;                       Added ‘folding-mode’ support
;;           17-Apr-2015 Added ‘haskell-mode’ but Haskell support is incomplete
;;           22-Apr-2015 Updated ‘after-make-frame-functions’
;;           12-May-2015 Changed ‘eval-after-load’ for ‘csharp-mode’ to require ‘usr-csharp’
;;           01-Jul-2015 Added ‘js-mode’ to ‘msb-menu-cond’
;;           18-Aug-2015 Added ‘tabbar’ support
;;           12-Jan-2016 Added ‘eval-after-load’ for ‘smart-compile’
;;           13-Jan-2016 Added ‘eval-after-load’ for ‘whitespace’
;;                       Added ‘eval-after-load’ for ‘hideshow’
;;           03-Feb-2016 Reorganized variables - used available customizations
;;                       Fixed errors in ‘msb-menu-cond’
;;                       Added ".tpp" to c++ ‘auto-mode-alist’
;;                       Added ‘isearch-mode-map’ keys
;;                       Removed ‘ffap-file-finder setting’
;;           08-Feb-2016 Reworked auto-mode code
;;                       Added ‘php-mode’
;;           22-Feb-2016 Fixed error with sh-script autoload/add-hook wrt ‘rainbow-delimiters’
;;           25-Feb-2016 Used ‘u-require’
;;           28-Feb-2016 Fixed ‘msb-menu-cond’
;;           29-Feb-2016 Changed from ‘usr-’ to ‘u-’
;;           01-Mar-2016 Removed face proprty settings in favor of customization set in .emacs
;;           02-Mar-2016 Added autoloads for ‘buffer-face-mode’, ‘text-scale-mode’, and ‘ergoemacs-select-text-in-quote’
;;           03-Mar-2016 Added HTML modes to ‘msb-menu-cond’
;;                       Added more javascript modes to ‘msb-menu-cond’
;;                       Removed ‘filladapt’
;;           30-Mar-2016 Removed ‘u-mode-line’ in favor of u-powerline'
;;           08-Apr-2016 Major revision for ‘use-package’
;;           14-Apr-2016 Added "(fset 'yes-or-no-p #'y-or-n-p)"
;;           15-Apr-2016 Fixed package usage
;;           19-Apr-2016 Added ‘flycheck’
;;                       Added ‘smartparens’
;;           26-Apr-2016 Added ‘shift-number’
;;           27-Apr-2016 Added ‘flycheck-pos-tip’
;;           28-Apr-2016 Added ‘jedi’ (python autocomplete)
;;           01-May-2016 Added ‘paradox’ support
;;           22-Jun-2016 Removed ‘lib-complete’
;;                       Using ‘global-hl-line-mode’
;;                       Added ‘u-msb’
;;           23-Jun-2016 Converted to rigorous usage of ‘use-package’
;;           14-Aug-2016 Added ‘ac-lang’
;;           12-Sep-2016 Removed ‘hideshow’
;;                       Added ‘origami’
;;           14-Sep-2016 Added ‘async’
;;           15-Sep-2016 Added ‘ac-dabbrev’
;;           27-Sep-2016 Added org-mode hook for ‘visual-line-mode’
;;           16-Nov-2016 Added ‘cperl-mode’ to ‘autopair’
;;           05-Dec-2016 Added ‘linum-relative’
;;           12-Jan-2017 Added ‘plsense-server-start’ to ‘cperl’ config
;;                       Added ‘winner-mode’
;;           13-Jan-2017 Added ‘fancy-narrow’ and ‘loccur’
;;                       Added ‘bm’
;;           14-Jan-2017 Added check for ‘window-system’
;;                       Added ‘global-auto-revert-mode’
;;           16-Jan-2017 Added ‘langtool’
;;                       Removed ‘tinysearch-*’
;;           17-Jan-2017 Added ‘web-mode’
;;           19-Jan-2017 Disabled ‘fancy-narrow’ and ‘origami’
;;                       Added ‘hideshow’
;;                       Added ‘flycheck-emacs-lisp-load-path’ to flycheck
;;           21-Jan-2017 Added ‘ssh’
;;           29-Jun-2017 Added ‘ycmd’, ‘company-ycmd’, and ‘flycheck-ycmd’
;;           19-Sep-2017 Added ‘sdcv’
;;           09-Jan-2018 Removed ‘timestamp’
;;

;;; Code:

(message "Configuring from default.el...")
;;
(message "Initializing package system...")
(let ((file-name-handler-alist nil))
  (require 'package)

  (setq package-enable-at-startup nil)

  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/"))

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (setq use-package-always-ensure t)

  (eval-when-compile
    (require 'u-macro)
    (require 'use-package))
;;
(message "Loading packages...")

(use-package ac-python :disabled
  :defer t
  :init (with-eval-after-load 'python '(require 'ac-python)))

(use-package anzu
  :diminish anzu-mode
  :config (progn
            (defun anzu--update-mode-line-local (here total)
              (when anzu--state
                (let ((status (cl-case anzu--state
                                (search (format " (%s/%d%s) "
                                                (anzu--format-here-position here total)
                                                total (if anzu--overflow-p "+" "")))
                                (replace-query (format " (%d items) " total))
                                (replace (format " (%d/%d) " here total)))))
                  (propertize status 'face 'anzu-mode-line))))
            (setq anzu-mode-line-update-function #'anzu--update-mode-line-local)
            (setq anzu-cons-mode-line-p nil)
            (global-anzu-mode +1)))

(use-package async
  :defer t)

(use-package atim-unscroll
  :ensure nil
  :config (atim-unscroll-global-mode 1))

(progn
  (use-package auto-complete :disabled
    :diminish auto-complete-mode
    :config (ac-config-default))

  (use-package ac-clang :disabled
    :defer t
    :config (when (ac-clang-initialize)
              (add-hook 'c-mode-common-hook '(lambda ()
                                               ;;(setq ac-clang-cflags CFLAGS)
                                               (ac-clang-activate-after-modify)))))

  (use-package ac-dabbrev :disabled
    :config (add-to-list 'ac-sources 'ac-source-dabbrev)))

(use-package autopair :disabled
  :ensure nil
  :diminish autopair-mode
  :config (hook-into-modes #'autopair-mode
                           'c++-mode-hook
                           'c-mode-hook
                           'clips-mode-hook
                           'cperl-mode-hook
                           'csharp-mode-hook
                           'emacs-lisp-mode-hook
                           'java-mode-hook
                           'json-mode-hook
                           'lisp-interaction-mode-hook
                           'matlab-mode-hook
                           'perl6-mode
                           'php-mode-hook
                           'python-mode-hook
                           'sh-mode-hook
                           'yaml-mode-hook))

(use-package bm
  :init
  (setq bm-highlight-style 'bm-highlight-only-fringe
        bm-cycle-all-buffers t)
  :config
  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; Make a more bookmarky symbol for a 'mark':
  (define-fringe-bitmap 'bm-marker-left [0 0 15 15 15 15 0 0] 8 4 'center)
  (let ((material/fg-orange-800  "#ef6c00"))
    (set-face-attribute 'bm-fringe-persistent-face nil :foreground "white" :background material/fg-orange-800))

  (define-key bm-show-mode-map [mouse-2] 'bm-show-goto-bookmark-1))

(use-package cc-mode
  :ensure nil
  :mode "\\.\\(proto\\|tpp\\)\\'")

(use-package clips-log-mode
  :ensure nil
  :mode "\\.log\\'")

(use-package clips-mode
  :mode "\\.clp\\'")

(on-gnu/linux
 (use-package company
   :defer 10
   :diminish company-mode
   :init
   (hook-into-modes #'company-mode
                    'c++-mode-hook
                    'c-mode-hook
                    'csharp-mode-hook
                    'python-mode-hook
                    'emacs-lisp-mode-hook)
   :config (progn
             (define-key company-active-map [return] 'company-complete-common)
             (define-key company-active-map (kbd "RET") 'company-complete-common)
             (define-key company-active-map [tab] 'company-complete-selection)
             (define-key company-active-map (kbd "TAB") 'company-complete-selection)
             (setq company-minimum-prefix-length 2
                   company-idle-delay 0.2
                   company-show-numbers t
                   company-tooltip-align-annotations t)
             (add-to-list 'company-transformers #'company-sort-by-backend-importance)))

 (use-package ycmd
   :after company
   :diminish ycmd-mode
   :init (progn
           (set-variable 'ycmd-server-command (list "python" (substitute-in-file-name "$HOME/elisp/packages/ycmd/ycmd/__main__.py")))
           (set-variable 'ycmd-global-config (substitute-in-file-name "$HOME/elisp/packages/ycmd/.ycm_extra_conf.py"))
           (add-hook 'after-init-hook #'global-ycmd-mode))
   )

 (use-package company-ycmd
   :after ycmd
   :config (progn
             (company-ycmd-setup)
             (push '(company-ycmd :with company-yasnippet company-dabbrev-code) company-backends))
   )

 (use-package flycheck-ycmd
   :after ycmd
   :init (flycheck-ycmd-setup)
   )
)
;; (setq url-show-status nil)              ; make ycmd more quiet

(use-package cperl-mode
  :ensure nil
  :defer t
  :commands (convert-to-perl)
  :init (defalias 'perl-mode 'cperl-mode)
  ;; :config (progn
  ;;           (plsense-server-start)
  ;;           (require 'u-perl))
  )

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package cuda-mode :disabled
  :mode "\\.cu\\'")

(use-package diffview :disabled
  :commands (diffview-current diffview-region diffview-message))

(use-package eldoc :disabled
  :ensure nil
  :diminish eldoc-mode
  :init (with-eval-after-load 'lisp-mode
          (eldoc-mode))
  :config (hook-into-modes #'eldoc-mode
                           'emacs-lisp-mode-hook
                           'lisp-interaction-mode-hook))

(use-package ergoemacs-functions
  :defer t
  :commands (ergoemacs-backward-open-bracket
             ergoemacs-forward-open-bracket
             ergoemacs-move-text-down
             ergoemacs-move-text-up
             ergoemacs-select-text-in-quote
             ergoemacs-shrink-whitespaces))

(use-package f
  :defer t) ;; tjf - file API !!!

(use-package face-remap
  :ensure nil
  :defer t
  :commands (buffer-face-mode
             text-scale-mode))

(use-package fancy-narrow :disabled
  :ensure nil
  :defer t
  :commands (fancy-narrow-to-defun
             fancy-narrow-to-region
             fancy-widen)
  :preface
  (defun fancy-narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and fancy-narrow--beginning fancy-narrow--end (not p)) (fancy-widen))
          ((region-active-p)
           (fancy-narrow-to-region (region-beginning) (region-end)))
          ((derived-mode-p 'org-mode)
           ;; `org-edit-src-code' is not a real narrowing
           ;; command. Remove this first conditional if
           ;; you don't want it.
           (cond ((ignore-errors (org-edit-src-code) t)
                  (delete-other-windows))
                 ((ignore-errors (org-fancy-narrow-to-block) t))
                 (t (org-fancy-narrow-to-subtree))))
          (t (fancy-narrow-to-defun))))
  )

(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config (progn
            (setq flycheck-emacs-lisp-load-path 'inherit)
            (setq flycheck-mode-line
                  '(:eval
                    (pcase flycheck-last-status-change
                      (`not-checked nil)
                      (`no-checker (propertize " -" 'face 'warning))
                      (`running    (propertize " ✷" 'face 'success))
                      (`errored    (propertize " !" 'face 'error))
                      (`finished
                       (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                              (no-errors    (cdr (assq 'error   error-counts)))
                              (no-warnings  (cdr (assq 'warning error-counts)))
                              (face (cond (no-errors   'error)
                                          (no-warnings 'warning)
                                          (t           'success))))
                         (propertize (format " %s/%s" (or no-errors 0) (or no-warnings 0))
                                     'face face)))
                      (`interrupted " -")
                      (`suspicious '(propertize " ?" 'face 'warning)))))))

(use-package flycheck-pos-tip
  :init (with-eval-after-load 'flycheck
          (flycheck-pos-tip-mode)))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :commands hs-minor-mode
  :config (hook-into-modes #'hs-minor-mode
                           'c++-mode-hook
                           'c-mode-hook
                           'clips-mode-hook
                           'cperl-mode-hook
                           'csharp-mode-hook
                           'emacs-lisp-mode-hook
                           'java-mode-hook
                           'json-mode-hook
                           'lisp-interaction-mode-hook
                           'matlab-mode-hook
                           'perl6-mode
                           'php-mode-hook
                           'python-mode-hook
                           'sh-mode-hook
                           'yaml-mode-hook))

(use-package highlight-escape-sequences :disabled
  :config (progn
            (put 'hes-escape-backslash-face 'face-alias 'hes-escape-face)
            (put 'hes-escape-sequence-face  'face-alias 'hes-escape-face)
            (hook-into-modes #'hes-mode
                             'c++-mode-hook
                             'c-mode-hook
                             'clips-mode-hook
                             'cperl-mode-hook
                             'csharp-mode-hook
                             'emacs-lisp-mode-hook
                             'java-mode-hook
                             'json-mode-hook
                             'lisp-interaction-mode-hook
                             'matlab-mode-hook
                             'perl6-mode
                             'php-mode-hook
                             'python-mode-hook
                             'sh-mode-hook
                             'yaml-mode-hook)))

(use-package highlight-operators :disabled
  :diminish highlight-operators-mode
  :config (hook-into-modes #'highlight-operators-mode
                           'c++-mode-hook
                           'c-mode-hook
                           'clips-mode-hook
                           ;; 'cperl-mode-hook
                           'csharp-mode-hook
                           'java-mode-hook
                           ;; 'json-mode-hook
                           'matlab-mode-hook
                           'perl6-mode
                           'php-mode-hook
                           'python-mode-hook
                           'sh-mode-hook
                           'yaml-mode-hook))

(use-package jedi
  :defer t
  :init (with-eval-after-load 'python
          '(progn
             (add-hook 'python-mode-hook 'jedi:setup)
             ;; (add-hook 'python-mode-hook 'jedi:ac-setup)
             ))
  :config
  (setq jedi:complete-on-dot t))

(use-package json-mode
  :mode "\\.json\\'")

(use-package langtool
  :defer t
  :config (setq langtool-language-tool-jar "~/Documents/LanguageTool-3.6/languagetool-commandline.jar")
  :commands (langtool-check))

(use-package lisp-mode
  :ensure nil
  :config (require 'u-lisp))

(use-package loccur
  :defer t
  :commands (loccur-current))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package manage-minor-mode :disabled)

(use-package matlab-mode
  :mode "\\.m\\'"
  ;; :init (with-eval-after-load 'matlab '(require 'u-matlab))
  )

(use-package modern-cpp-font-lock
  :defer t
  :diminish modern-c++-font-lock-mode
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package mic-paren
  :config (paren-activate))

(use-package name-this-color :disabled)

(use-package neotree
  :defer t
  :functions (neo-buffer--unlock-width
              neo-buffer--lock-width))

(use-package nlinum
  :config (make-variable-buffer-local 'nlinum-mode))

(use-package nlinum-relative
  :config (progn
            (setq nlinum-relative--format-function
                  (lambda (line width)
                    (let* ((line-display (- line nlinum-relative--current-line) )
                           (is-current-line? (eq line-display 0))
                           (line-display (if is-current-line?
                                             nlinum-relative--current-line
                                           (+ nlinum-relative-offset line-display)))
                           (str (if (and (not (string-equal nlinum-relative-current-symbol "")) is-current-line?)
                                    nlinum-relative-current-symbol (format nlinum-format line-display)))
                           )
                      (when (< (length str) width)
                        (setq str (concat (make-string (- width (length str)) ?\ ) str)))
                      (if is-current-line?
                          (put-text-property 0 width 'face 'nlinum-relative-current-face str)
                        (put-text-property 0 width 'face 'linum str))
                      str)))
            (make-variable-buffer-local 'nlinum-relative-mode)))

(use-package nxml-mode :disabled
  :mode "\\.xaml\\'")

(use-package org-cua-dwim
  :defer t
  :init (with-eval-after-load 'org-mode '(org-cua-dwim-activate)))

(use-package origami :disabled
  :ensure nil
  :config (global-origami-mode))

(use-package perl6-mode
  :mode "\\.p[lm]?6\\'")

(use-package php-mode :disabled
  :mode "\\.php[s345t]?\\'")

(use-package plsense :disabled
  :defer t)

(use-package popwin
  :config (popwin-mode))

(use-package powerline
  ;; :config (require 'u-powerline)
  )

(use-package pretty-column
  :ensure nil
  :defer t
  :commands (pretty-column
             pretty-rectangle)
  :config (setq pcol-column-separator "[ \t]+" pcol-str-separator " "))

(use-package python
  :defer t)

(use-package rainbow-delimiters
  :config (hook-into-modes #'rainbow-delimiters-mode
                           'c++-mode-hook
                           'c-mode-hook
                           'clips-mode-hook
                           'cperl-mode-hook
                           'csharp-mode-hook
                           'emacs-lisp-mode-hook
                           'java-mode-hook
                           'json-mode-hook
                           'lisp-interaction-mode-hook
                           'matlab-mode-hook
                           'perl6-mode
                           'php-mode-hook
                           'python-mode-hook
                           'sh-mode-hook
                           'yaml-mode-hook))

(use-package s
  :defer t)

(use-package sdcv
  :defer t
  :commands sdcv-search)

(use-package shift-number
  :defer t
  :commands (shift-number-up
             shift-number-down))

(use-package smartparens
  :diminish smartparens-mode
  :config (progn
            (sp-local-pair sp-lisp-modes "'" nil :actions nil)
            (sp-local-pair sp-lisp-modes "`" nil :actions nil)
            (hook-into-modes #'smartparens-mode
                             'c++-mode-hook
                             'c-mode-hook
                             'clips-mode-hook
                             'csharp-mode-hook
                             'emacs-lisp-mode-hook
                             'java-mode-hook
                             'json-mode-hook
                             'lisp-interaction-mode-hook
                             'matlab-mode-hook
                             'perl6-mode
                             'php-mode-hook
                             'python-mode-hook
                             'sh-mode-hook
                             'shell-mode-hook
                             'text-mode-hook
                             'yaml-mode-hook)))

(use-package smooth-scrolling)

(use-package spu :disabled
  :defer 5
  :config (spu-package-upgrade-daily))

(use-package sr-speedbar :disabled
  :defer t)

(use-package ssh
  :defer t
  :config (progn
            (setq ssh-directory-tracking-mode t)
            (shell-dirtrack-mode t)
            (setq dirtrackp nil)))

(use-package tabbar
  :config (tabbar-mode 1))

(use-package tetris
  :defer t
  :config (setq tetris-score-file "/dev/null"))

(use-package tinyeat
  :ensure nil
  :defer t
  :commands (tinyeat-backward-preserve
             tinyeat-delete-paragraph
             tinyeat-delete-whole-word
             tinyeat-forward-preserve))

(use-package tinysearch
  :ensure nil
  :defer t
  :commands (tinysearch-search-word-forward
             tinysearch-search-word-backward))

(use-package u-cc
  :ensure nil
  :after cc-mode)

(use-package u-clips
  :ensure nil
  :after clips-mode)

(use-package u-csharp
  :ensure nil
  :after csharp-mode)

(use-package u-matlab
  :ensure nil
  :after matlab)

(use-package u-perl
  :ensure nil
  :after cperl-mode)

(use-package u-powerline
  :ensure nil
  :after powerline)

(use-package u-python
  :ensure nil
  :after python)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (progn
            (remove-hook 'menu-bar-update-hook 'undo-tree-update-menu-bar)
            (global-undo-tree-mode 1)))

(use-package uniquify
  :ensure nil)

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(use-package web-mode
  :mode "\\.\\(p?html?\\|php\\|[agj]sp\\|as[cp]x\\)\\'")

(use-package whitespace
  :ensure nil
  :defer t
  :diminish whitespace-mode
  :config (setq whitespace-style (quote (tabs spaces space-before-tab newline indentation empty space-after-tab space-mark
                                              tab-mark newline-mark))
                whitespace-display-mappings '((space-mark 32 [183] [46])                     ; 32 SPACE     => "·" "."
                                              (newline-mark 10 [182 10])                     ; 10 LINE FEED => "¶ <LINE FEED>"
                                              (tab-mark 9 [9654 32 91 84 65 66 93 9] [92 9]) ;  9 TAB       => "▶ [TAB]<TAB>"
                                              )))

(use-package writegood-mode :disabled)

(use-package yaml-mode
  :mode "\\.e?ya?ml\\'")

;;
(cua-mode)
(global-hl-line-mode)
;;
(message "Loading libraries...")

(require 'u-overload)
(require 'u-variables)
(require 'u-flags)
(require 'u-date)
(require 'u-msb)
(require 'u-frame)
(require 'duplicate)
(require 'u-search)
(require 'u-file)
(require 'u-navigate)
(require 'u-edit)
(require 'keys)
(require 'u-menubar)
(require 'u-toolbar)
)
;;
(autoload 'live-mode      "live-mode" nil t)
(autoload 'pquery-replace "pquery"    nil t)
;;
(blink-cursor-mode)
(winner-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'occur-mode-hook '(lambda () (make-local-variable 'which-function-mode) (setq which-function-mode nil)))

(diminish 'auto-fill-function)
(diminish 'isearch-mode)
;;
(eval-after-load "semantic" '(define-key semantic-mode-map [menu-bar] nil))
(eval-after-load "semantic/db-file" '(abbrev-mode 0))
(semantic-mode)
(global-auto-revert-mode)
;;
(random t)
(set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
;;
;; also "(if (not interactive) ..."
;;
(on-gui
  (set-random-background-color)
  (on-gnu/linux
   (shell))
  (reset-frame-size)
  (delete-other-windows))
;;
(message "default.el ...done")

;;; default.el ends here
