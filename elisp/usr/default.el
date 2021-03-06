;;; default.el --- Global initialization for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2000-2021 Tom Fontaine

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
;;           09-Feb-2018 Added ‘treemacs’
;;           16-Mar-2018 Removed ‘jedi’
;;                       Fixed ‘cperl-mode’/‘smartparens’ conflict
;;           02-Apr-2018 Disabled ‘nlinum’ and ‘nlinum-relative’
;;                       Added ‘display-line-numbers’
;;           12-Apr-2018 Added ‘helpful’
;;           17-Apr-2018 Changed ‘lisp-mode’ to ‘elisp-mode’
;;           13-Jun-2018 Updated for performance
;;           20-Jun-2018 Changed some minor-modes to use :hook
;;           16-Jan-2019 Added ‘clang-format’
;;           24-Jan-2019 Changed from ‘ycmd’ to ‘eglot’ (still need omnisharp server for C#?)
;;           30-Apr-2019 Added ‘treemacs-magit’
;;           18-Jun-2019 Added ‘rtags’, ‘company-rtags’, and ‘flycheck-rtags’
;;           24-Jun-2019 Added ‘groovy’
;;           10-Jul-2019 Fixed compiler warnings
;;                       Added ‘clean-aindent’
;;           13-Jul-2019 Added ‘centaur-tabs’
;;           20-Jul-2019 Added conditional for Emacs 27
;;                       Added ‘filladapt’
;;           09-Aug-2019 Added ‘git-gutter’, ‘git-gutter-fringe’, ‘fringe-helper’, and ‘ws-butler’
;;           28-Aug-2019 Added ‘u-cpp’
;;           03-Sep-2019 Added ‘minions’
;;           05-Oct-2020 Added ‘rainbow-mode’ and ‘display-line-numbers’
;;           28-Oct-2020 Fixed ‘tabbar' and ‘powerline' initialization relationship
;;           10-Nov-2020 Added ‘emojify' and ‘unicode-fonts'
;;           15-Dec-2020 Moved defconts to .emacs
;;                       Removed defer constants (use let variables)
;;           24-Jan-2021 Added ‘dash'
;;

;;; Code:

(message "Configuring from default.el...")

;;
(message "Initializing package system...")

;; (let ((file-name-handler-alist nil))
  (require 'package)

  (setq package-enable-at-startup nil)

  (if (> emacs-major-version 26)
      (enable-theme 'fontaine)
    (package-initialize))

  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (setq use-package-always-ensure t)

  (eval-when-compile
    (require 'cl-lib)
    (require 'u-macro)
    (require 'use-package))
  ;;
  (message "Loading packages...")


(cl-pushnew (concat user-dir-home "elisp/tjf") load-path :test 'string=)
(load-library "with-editor")

(use-package anzu
    :diminish anzu-mode
    :preface
    (eval-when-compile
      (declare-function global-anzu-mode "anzu" (arg1)))
    (defun anzu--update-mode-line-local (here total)
      (when anzu--state
        (let ((status (cl-case anzu--state
                        (search (format " (%s/%d%s) "
                                        (anzu--format-here-position here total)
                                        total (if anzu--overflow-p "+" "")))
                        (replace-query (format " (%d items) " total))
                        (replace (format " (%d/%d) " here total)))))
          (propertize status 'face 'anzu-mode-line))))
    :config
    (progn
      (setq anzu-mode-line-update-function #'anzu--update-mode-line-local)
      (setq anzu-cons-mode-line-p nil)
      (global-anzu-mode +1)))

  (use-package atim-unscroll :defer
    :ensure nil
    :preface
    (eval-when-compile
      (declare-function atim-unscroll-global-mode "atim-unscroll" (arg1)))
    :config
    (atim-unscroll-global-mode 1))

  (use-package autorevert :defer
    :ensure nil
    :diminish auto-revert-mode
    :preface
    (eval-when-compile
      (declare-function global-auto-revert-mode "autorevert" ()))
    :config
    (global-auto-revert-mode))

  (use-package async :defer defer-3)

  (use-package bm    ;; :defer nil
    :preface
    (eval-when-compile
      (declare-function bm-buffer-save-all "bm" ())
      (declare-function bm-repository-load "bm" ())
      (declare-function bm-repository-save "bm" ()))
    :hook
    (after-save       . bm-buffer-save)
    (after-revert     . bm-buffer-restore)
    (find-file        . bm-buffer-restore)
    (kill-buffer      . bm-buffer-save)
    (kill-emacs-hook  . (lambda nil (bm-buffer-save-all) (bm-repository-save)))
    :config
    (setq bm-highlight-style 'bm-highlight-only-fringe
          bm-cycle-all-buffers t)

    ;; where to store persistant files
    (setq bm-repository-file "~/.emacs.d/bm-repository")

    ;; save bookmarks
    (setq-default bm-buffer-persistence t)

    ;; Loading the repository from file when on start up.
    (bm-repository-load)

    ;; Make a more bookmarky symbol for a 'mark':
    (define-fringe-bitmap 'bm-marker-left [0 0 15 15 15 15 0 0] 8 4 'center)
    (let ((material/fg-orange-800  "#ef6c00"))
      (set-face-attribute 'bm-fringe-persistent-face nil :foreground "white" :background material/fg-orange-800))

    (define-key bm-show-mode-map [mouse-2] 'bm-show-goto-bookmark-1))

  (use-package clean-aindent-mode    ;; :defer nil
    :preface
    (eval-when-compile
      (declare-function clean-aindent-mode "clean-aindent-mode" (arg1)))
    :config
    (electric-indent-mode -1)  ; no electric indent, auto-indent is sufficient
    (setq clean-aindent-is-simple-indent t)
    (define-key global-map (kbd "RET") 'newline-and-indent))

  (use-package cc-mode :commands (c-mode c++-mode)
    :ensure nil
    :init
    (add-to-list 'auto-mode-alist '("\\.\\(C\\|H\\)\\'"       . c-mode))
    (add-to-list 'auto-mode-alist '("\\.\\(proto\\|tpp\\)\\'" . c++-mode)))

  (use-package ccls :after cc-mode
    :if is-linux?
    :config
    (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
    ;; (setq ccls-sem-highlight-method   'font-lock)
    (setq ccls-sem-highlight-method 'overlay)
    (message "ccls loaded..."))

  (use-package clang-format :disabled)

  (use-package clips-log-mode :commands clips-log-mode :disabled
    :ensure nil
    :mode "\\.log\\'")

  (use-package clips-mode :commands clips-mode)

  (use-package company :defer
    :if is-linux?
    :diminish company-mode
    :preface
    (eval-when-compile
      (declare-function company-sort-by-backend-importance "company" ()))
    :hook
    (prog-mode . company-mode)
    :config
    (progn
      (define-key company-active-map [return]    'company-complete-common)
      (define-key company-active-map [tab]       'company-complete-selection)
      (define-key company-active-map (kbd "RET") 'company-complete-common)
      (define-key company-active-map (kbd "TAB") 'company-complete-selection)
      (setq company-minimum-prefix-length 2
            company-idle-delay 0.2
            company-show-numbers t
            company-tooltip-align-annotations t
            company-clang-executable "/usr/bin/clang")
      (add-to-list 'company-transformers #'company-sort-by-backend-importance)))

  (use-package company-jedi :after company :disabled
    :if is-linux?
    :config
    (add-to-list 'company-backends '(company-jedi company-files)))

  (use-package company-plsense :disabled
    :if is-linux?
    :after plsense
    :config
    (add-to-list 'company-backends 'company-plsense))

  (use-package cperl-mode :commands (convert-to-perl cperl-mode perl-mode)
    :ensure nil
    :init
    (defalias 'perl-mode 'cperl-mode)
    :config
    (progn
      (define-key cperl-mode-map "{" nil)
      (define-key cperl-mode-map "(" nil)
      (define-key cperl-mode-map "[" nil))
    ;;           (plsense-server-start)
    )

  (use-package dash)

  (use-package declutter :commands declutter :disabled
    :if is-linux?
    :ensure nil)

   (use-package eglot :after company
     :if is-linux?
     :init
     (with-eval-after-load 'project
       (add-to-list 'project-find-functions
                    '(lambda (dir) (let ((root (projectile-project-root dir))) (and root (cons 'transient root))))))
     :hook
     ((c++-mode . eglot-ensure)
      ;; (c++-mode . flycheck-mode)
      )
     :config
     (message "eglot loaded..."))

   (use-package emojify :defer
     :commands emojify-mode)

  (use-package csharp-mode :commands csharp-mode)

  (use-package cuda-mode :commands cuda-mode)

  (use-package dash) ;; :defer nil

  (use-package diminish ;; :defer nil
    :preface
    (eval-when-compile
      (declare-function diminish "diminish" (arg1))))

  (use-package display-line-numbers
    :ensure nil
    :config
    (hook-into-modes #'display-line-numbers-mode
                     'org-mode-hook
                     'prog-mode-hook
                     'text-mode-hook))

  (use-package eglot :after company
    :hook
    (c++-mode . eglot-ensure)
    :config
    (setq company-backends
          (cons 'company-capf
                (remove 'company-capf company-backends))))

  (use-package eldoc :defer
    :ensure nil
    :diminish eldoc-mode
    :hook (emacs-lisp-mode-hook . eldoc-mode))

  (use-package ergoemacs-functions :commands (ergoemacs-backward-open-bracket
                                              ergoemacs-extend-selection
                                              ergoemacs-forward-open-bracket
                                              ergoemacs-move-text-down
                                              ergoemacs-move-text-up
                                              ergoemacs-select-text-in-quote
                                              ergoemacs-shrink-whitespaces)
    :ensure nil
    :no-require t)

  (use-package f) ;; :defer nil

  (use-package face-remap :commands (buffer-face-mode text-scale-mode)
    :ensure nil)

  (use-package filladapt :commands filladapt-mode)

  (use-package flycheck :defer
    :preface
    (eval-when-compile
      (declare-function global-flycheck-mode "flycheck" ()))
    :config
    (progn
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
                (`suspicious '(propertize " ?" 'face 'warning)))))
      (global-flycheck-mode)))

  (use-package flycheck-pos-tip :after flycheck
    :preface
    (eval-when-compile
      (declare-function flycheck-pos-tip-mode "flycheck-pos-tip" ()))
    :config
    (flycheck-pos-tip-mode))

  (use-package git-gutter
    :config
    (global-git-gutter-mode t))

  (use-package groovy-mode :commands groovy-mode
    :mode "\\.grv\\'")

  (use-package helpful ;; :defer nil
    :preface
    (eval-when-compile
      (declare-function helpful-callable "helpful" ())
      (declare-function helpful-variable "helpful" ())
      (declare-function helpful-key      "helpful" ()))
    :config
    (progn
      (global-set-key (kbd "C-h   f") #'helpful-callable)
      (global-set-key (kbd "C-h   v") #'helpful-variable)
      (global-set-key (kbd "C-h   k") #'helpful-key)
      (global-set-key (kbd "C-h C-k") #'describe-key)))

  (use-package hideshow :commands hs-minor-mode
    :ensure nil
    :diminish hs-minor-mode
    :hook
    (prog-mode . hs-minor-mode))

  (use-package highlight-escape-sequences :disabled
    :hook
    (prog-mode . hes-mode)
    :config
    (progn
      (put 'hes-escape-backslash-face  'face-alias 'hes-escape-face)
      (put 'hehes-escape-sequence-face 'face-alias 'hes-escape-face)))

  (use-package highlight-operators :defer
    :diminish highlight-operators-mode
    :preface
    (eval-when-compile
      (declare-function highlight-operators-mode "highlight-operators" ()))
    :config
    (hook-into-modes #'highlight-operators-mode
                     ;; 'c++-mode-hook
                     'c-mode-hook
                     'clips-mode-hook
                     ;; 'cperl-mode-hook
                     'csharp-mode-hook
                     'java-mode-hook
                     ;; 'json-mode-hook
                     'matlab-mode-hook
                     'php-mode-hook
                     'python-mode-hook
                     'sh-mode-hook
                     'yaml-mode-hook))

  (use-package json-mode :commands json-mode)

  (use-package langtool :commands langtool-check
    :config
    (setq langtool-language-tool-jar (concat user-dir-home "Documents/LanguageTool-4.1/languagetool-commandline.jar")))

  (use-package loccur :commands loccur-current)

  (use-package lua-mode :commands lua-mode)

  (use-package make-mode :commands makefile-gmake-mode
    :ensure nil
    :init
    (add-to-list 'auto-mode-alist '("\\.\\(pro\\|pro\\.sav\\)\\'" . makefile-gmake-mode)))

  (use-package magit :commands magit-status)

  (use-package matlab-mode :commands matlab-mode)

  (use-package minions
    :config
    (minions-mode 1))

  (use-package modern-cpp-font-lock :after cc-mode
    :diminish modern-c++-font-lock-mode
    :hook
    (c++-mode . modern-c++-font-lock-mode)
    :config
    (message "modern-cpp-font-lock loaded..."))

  (use-package mic-paren ;; :defer nil
    :preface
    (eval-when-compile
      (declare-function paren-activate "mic-paren" ()))
    :config
    (paren-activate))

  (use-package msb ;; :defer nil
    :ensure nil)

  (use-package org-mode :commands org-mode
    :ensure nil
    :hook
    (org-mode . visual-line-mode))

  (use-package paradox :commands paradox-list-packages)

  (use-package perl6-mode :commands perl6-mode :disabled)

  (use-package plsense :disabled
    :if is-linux?
    :after cperl-mode
    :config
    (progn
      (plsense-config-default)
      (plsense-server-start)))

  (use-package popwin :defer
    :preface
    (eval-when-compile
      (declare-function popwin-mode "popwin" ()))
    :config (popwin-mode))

  (use-package powerline)

  (use-package powerthesaurus :commands powerthesaurus-lookup-word)

  (use-package pquery :commands pquery-replace
    :ensure nil)

  (use-package pretty-column :commands (pretty-column pretty-rectangle)
    :ensure nil
    :config
    (setq pcol-column-separator "[ \t]+" pcol-str-separator " "))

  (use-package projectile :after project
    :hook
    (prog-mode . projectile-mode)
    :diminish projectile-mode
    :config
    (add-to-list 'project-find-functions '(lambda (dir)
                                            (let ((root (projectile-project-root dir)))
                                              (and root (cons 'transient root))))))

  (use-package python :commands python-mode)

  (use-package rainbow-delimiters :defer
    :hook
    (prog-mode . rainbow-delimiters-mode))

  (use-package rainbow-mode)

  (use-package s) ;; :defer nil

  (use-package sdcv :commands sdcv-search)

  (use-package shift-number :commands (shift-number-up shift-number-down))

  (use-package smartparens :defer
    :diminish smartparens-mode
    :preface
    (eval-when-compile
      (declare-function sp-local-pair "smartparens" (arg1 arg2 arg3 arg4 arg5)))
    :hook
    ((prog-mode shell-mode lisp-interaction-mode) . smartparens-mode)
    :config
    (progn
      (sp-local-pair sp-lisp-modes "'" nil :actions nil)
      (sp-local-pair sp-lisp-modes "`" nil :actions nil)))

  (use-package smooth-scrolling :defer)

  (use-package ssh :disabled
    :config
    (progn
      (setq ssh-directory-tracking-mode t)
      (shell-dirtrack-mode t)
      (setq dirtrackp nil)))

(use-package tjf-tabline
  :after powerline
  :config
  (tjf:tabline/mode 1)
  (setq tjf:tabline/tab-label-function #'tjf:tabline/label-function))

  ;; (use-package tabbar :after powerline
 ;;    :preface
 ;;    (eval-when-compile
 ;;      (declare-function tabbar-mode "tabbar" (ARG1)))
 ;;    :init
 ;;    (defvar u/tabbar-height 20)
 ;;    :config
 ;;    (defun tabbar-add-tab (tabset object &optional _append_ignored)
 ;;      "Add to TABSET a tab with value OBJECT if there isn't one there yet.
 ;; If the tab is added, it is added at the beginning of the tab list,
 ;; unless the optional argument APPEND is non-nil, in which case it is
 ;; added at the end."
 ;;      (let ((tabs (tabbar-tabs tabset)))
 ;;        (if (tabbar-get-tab object tabset)
 ;;            tabs
 ;;          (let ((tab (tabbar-make-tab object tabset)))
 ;;            (tabbar-set-template tabset nil)
 ;;            (set tabset (sort (cons tab tabs)
 ;;                              (lambda (a b) (string< (buffer-name (car a))
 ;;                                                     (buffer-name (car b))))))))))

 ;;    (defvar u/tabbar-left  (powerline-wave-right 'tabbar-default nil u/tabbar-height))
 ;;    (defvar u/tabbar-right (powerline-wave-left  nil 'tabbar-default u/tabbar-height))

 ;;    (defun u/tabbar-tab-label-function (tab)
 ;;      (powerline-render (list u/tabbar-left (format " %s  " (car tab)) u/tabbar-right)))


 ;;    (customize-set-variable 'tabbar-separator '(0.0))
 ;;    (customize-set-variable 'tabbar-use-images nil)
 ;;    (tabbar-mode 1)

 ;;    ;; tabbar-tab-label-function will be reset after enabling tabbar-mode

 ;;    (setq tabbar-tab-label-function #'u/tabbar-tab-label-function)
 ;;    (customize-set-variable 'tabbar-use-images nil))

  (use-package tetris :commands tetris
    :config
    (setq tetris-score-file "/dev/null"))

  (use-package text-mode :commands text-mode
    :ensure nil
    :hook
    (text-mode . turn-on-auto-fill))

  (use-package tinyeat :commands (tinyeat-backward-preserve
                                  tinyeat-delete-paragraph
                                  tinyeat-delete-whole-word
                                  tinyeat-forward-preserve)
    :ensure nil)

  (use-package tinysearch :commands (tinysearch-search-word-forward
                                     tinysearch-search-word-backward)
    :ensure nil)

  (use-package treemacs :commands treemacs
    :preface
    (eval-when-compile
      (declare-function treemacs-follow-mode    "treemacs" (arg1))
      (declare-function treemacs-filewatch-mode "treemacs" (arg1))
      (declare-function treemacs-git-mode       "treemacs" (arg1)))
    :config
    (progn
      (setq treemacs-change-root-without-asking nil
            treemacs-collapse-dirs              (if (executable-find "python") 3 0)
            treemacs-file-event-delay           5000
            treemacs-follow-after-init          t
            treemacs-recenter-distance          0.1
            treemacs-goto-tag-strategy          'refetch-index
            treemacs-indentation                2
            treemacs-indentation-string         " "
            treemacs-is-never-other-window      nil
            treemacs-never-persist              nil
            treemacs-no-png-images              nil
            treemacs-recenter-after-file-follow nil
            treemacs-recenter-after-tag-follow  nil
            treemacs-show-hidden-files          t
            treemacs-silent-filewatch           nil
            treemacs-silent-refresh             nil
            treemacs-sorting                    'alphabetic-case-insensitive-asc
            treemacs-tag-follow-cleanup         t
            treemacs-tag-follow-delay           1.5
            treemacs-width                      35)
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (pcase (cons (not (null (executable-find "git")))
                   (not (null (executable-find "python3"))))
        (`(t . t)
         (treemacs-git-mode 'extended))
        (`(t . _)
         (treemacs-git-mode 'simple))))
    )

  (use-package treemacs-magit :after treemacs)

  (use-package treemacs-projectile :after treemacs)

  (use-package u-c :after c-mode :disabled
    :ensure nil)

  (use-package u-cc :after cc-mode :disabled
    :ensure nil)

  (use-package u-cpp :after c++-mode
    :ensure nil)

  (use-package u-clips :after clips-mode
    :ensure nil)

  (use-package u-csharp :after csharp-mode
    :ensure nil)

  (use-package u-lisp :after elisp-mode
    :ensure nil)

  (use-package u-matlab :after matlab
    :ensure nil)

  (use-package u-msb :after msb
    :ensure nil)

  (use-package u-perl :after cperl-mode
    :ensure nil
    :preface
    (eval-when-compile
      (declare-function convert-to-perl "u-perl" (arg1))))

  (use-package u-powerline :after powerline
    :ensure nil)

  (use-package u-python :after python
    :ensure nil)

  (use-package u-ssh :after ssh
    :ensure nil)

  (use-package undo-tree
    :diminish undo-tree-mode
    :preface
    (eval-when-compile
      (declare-function global-undo-tree-mode "undo-tree" (arg1)))
    :config
    (progn
      (global-undo-tree-mode 1)))

  (use-package unicode-fonts :disabled
    :ensure t
    :init
    (defun u/replace-unicode-font-mapping (block-name old-font new-font)
      (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
             (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
             (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
        (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
              `(,updated-block))))
    :custom
    (unicode-fonts-skip-font-groups '(low-quality-glyphs))
    :config
    ;; Fix the font mappings to use the right emoji font
    (mapc
     (lambda (block-name)
       (u/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
     '("Dingbats"
       "Emoticons"
       "Miscellaneous Symbols and Pictographs"
       "Transport and Map Symbols"))
    (unicode-fonts-setup))

  (use-package uniquify :defer
    :ensure nil
    :config
    (setq uniquify-ignore-buffers-re "^\\*"))

  (use-package volatile-highlights
    :diminish volatile-highlights-mode
    :preface
    (eval-when-compile
      (declare-function volatile-highlights-mode "volatile-highlights" (arg1)))
    :config
    (volatile-highlights-mode t))

  (use-package web-mode :disabled)

  (use-package whitespace :commands whitespace-mode
    :ensure nil
    :diminish whitespace-mode
    :config (setq whitespace-style (quote (tabs spaces space-before-tab newline indentation empty space-after-tab space-mark
                                                tab-mark newline-mark))
                  whitespace-display-mappings '((space-mark 32 [183] [46])                     ; 32 SPACE     => "·" "."
                                                (newline-mark 10 [182 10])                     ; 10 LINE FEED => "¶ <LINE FEED>"
                                                (tab-mark 9 [9654 32 91 84 65 66 93 9] [92 9]) ;  9 TAB       => "▶ [TAB]<TAB>"
                                                )))

   (use-package ws-butler
    :diminish ws-butler-mode
    :config
    (ws-butler-global-mode))

  (use-package writegood-mode :disabled)

  (use-package yaml-mode :commands yaml-mode)

  ;; --------------------------------------------------------------------------------

  (message "Loading libraries...")

  (require 'u-overload)
  (require 'u-variables)
  (require 'u-flags)
  (require 'u-menubar)
  (require 'u-toolbar)
  (require 'keys)
  ;;
  (cua-mode)
  (global-hl-line-mode)
  ;;
  (blink-cursor-mode)
  (winner-mode 1)

  (add-hook 'occur-mode-hook '(lambda () (make-local-variable 'which-function-mode) (setq which-function-mode nil)))

  (diminish 'auto-fill-function)
  (diminish 'isearch-mode)
  ;;
  (random t)
  (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
  ;;
  ;; also "(if (not interactive) ..."
  ;;
  (on-gui
   (set-background-color "gray95")
   ;; (set-random-background-color)
   ;; (on-gnu/linux
   ;;  (shell))
   (reset-frame-size)
   (delete-other-windows))

  ;; )
;; ================================================================================
;;
(message "default.el ...done")
(emacs-init-time)
;;; default.el ends here
