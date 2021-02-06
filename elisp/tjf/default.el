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
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Configuring from default.el...")

;;
(if (> emacs-major-version 26)
    (enable-theme 'fontaine))

(setq straight-use-package-by-default nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  (setq use-package-hook-name-suffix nil)) ;; Write hooks using their real name instead of a shorter version: after-init ==> `after-init-hook'

(use-package straight-x)
;; provides `straight-x-clean-unused-repos' (part of `straight.el')

(eval-when-compile
  (require 'tjf-macro))

;;
(use-package diminish
  :straight t
  :after use-package
  :functions
  diminish)

(use-package f
  :straight t)

(use-package emacs
  :preface
  (eval-when-compile
    (defvar gnutls-min-prime-bits)
    (defvar which-func-modes))
  :config
  (setq         auto-save-file-name-transforms      `((".*"   ,tjf:user/dir-autosave t)))
  (setq         backup-directory-alist              `((".*" . ,tjf:user/dir-backup)))
  (setq         blink-cursor-blinks                 0)
  (setq         byte-compile-warnings               '(not free-vars unresolved noruntime lexical make-local))
  (setq         colon-double-space                  nil)
  (setq-default cursor-type                         '(bar . 2))
  (setq         disabled-command-function           nil)
  (setq         echo-keystrokes                     0.25)
  (setq         fill-column                         8192)
  (setq-default font-lock-maximum-decoration        t)
  (setq-default frame-title-format                  "%b")
  (setq         gnutls-min-prime-bits               80)
  (setq-default indent-tabs-mode                    nil)
  (setq         inhibit-startup-buffer-menu         t)
  (setq         max-image-size                      256)
  (setq         mode-require-final-newline          'visit-save)
  (setq         mouse-drag-copy-region              t)
  (setq         mouse-yank-at-point                 t)
  (setq         mouse-wheel-scroll-amount           '(1 ((shift) . 5) ((meta)) ((control))))
  (setq         recenter-positions                  '(top middle bottom))
  (setq         ring-bell-function                  '(lambda () (let ((visible-bell t)) (beep)) (beep) (beep)))
  (setq         save-interprogram-paste-before-kill t)
  (setq-default scroll-preserve-screen-position     t)
  (setq-default scroll-conservatively               0)
  (setq-default scroll-margin                       0)
  (setq         sentence-end-double-space           nil)
  (setq         sentence-end-without-period         nil)
  (setq-default tab-always-indent                   'complete)
  (setq         use-hard-newlines                   nil)
  (setq         which-func-modes                    '(emacs-lisp-mode c-mode c++-mode cperl-mode python-mode diff-mode))

  (defalias 'yes-or-no-p 'y-or-n-p)

  (put 'downcase-region  'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'overwrite-mode   'disabled t)
  (put 'upcase-region    'disabled nil)

  (random t))

;; =============================================================================

(use-package anzu
  :straight t
  :diminish anzu-mode
  :functions (anzu--format-here-position global-anzu-mode)
  :preface
  (eval-when-compile
    (defvar anzu--overflow-p)
    (defvar anzu--state)
    (defvar anzu-cons-mode-line-p)
    (defvar anzu-mode-line-update-function))
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
  (setq anzu-mode-line-update-function #'anzu--update-mode-line-local)
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode +1))

(use-package async
  :straight t
  :defer    t)

(use-package autorevert
  :diminish autorevert-mode
  :config
  (setq auto-revert-verbose t)
  (global-auto-revert-mode))

(use-package bm    ;; :defer nil
  :straight t
  :functions (bm-buffer-save-all bm-repository-load bm-repository-save)
  :preface
  (eval-when-compile
    (defvar bm-cycle-all-buffers)
    (defvar bm-highlight-style)
    (defvar bm-repository-file)
    (defvar bm-show-mode-map)
    (defvar tjf:user/dir-config))
  :init
  (add-hook 'after-save-hook   #'bm-buffer-save)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'find-file-hook    #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook  #'bm-buffer-save)
  (add-hook 'kill-emacs-hook   #'bm-buffer-save-all)
  (add-hook 'kill-emacs-hook   #'bm-repository-save)
  ;; :hook
  ;; (after-save       . bm-buffer-save)
  ;; (after-revert     . bm-buffer-restore)
  ;; (find-file        . bm-buffer-restore)
  ;; (kill-buffer      . bm-buffer-save)
  ;; (kill-emacs-hook  . (lambda nil (bm-buffer-save-all) (bm-repository-save)))
  :config
  (setq bm-highlight-style 'bm-highlight-only-fringe
        bm-cycle-all-buffers t)

  ;; where to store persistant files
  (setq bm-repository-file (concat tjf:user/dir-config "bm-repository"))

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (bm-repository-load)

  ;; Make a more bookmarky symbol for a 'mark':
  (define-fringe-bitmap 'bm-marker-left [0 0 15 15 15 15 0 0] 8 4 'center)
  (let ((material/fg-orange-800  "#ef6c00"))
    (set-face-attribute 'bm-fringe-persistent-face nil :foreground "white" :background material/fg-orange-800)) ;; theme???

  (define-key bm-show-mode-map [mouse-2] 'bm-show-goto-bookmark-1))

(use-package cc-mode
  :mode
  :commands (c-mode c++-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(C\\|H\\)\\'"       . c-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(proto\\|tpp\\)\\'" . c++-mode)))

(use-package cl-lib)

(use-package cl-macs)

(use-package clean-aindent-mode
  :straight t
  :functions clean-aindent-mode
  :preface
  (eval-when-compile
    (defvar clean-aindent-is-simple-indent))
  :config
  (electric-indent-mode -1)  ; no electric indent, auto-indent is sufficient
  (setq clean-aindent-is-simple-indent t)
  (define-key global-map (kbd "RET") 'newline-and-indent))

(use-package clips-mode
  :straight t
  :mode
  :commands clips-mode
  :config
  (message "Loading clips-mode...done"))

(use-package color)

(use-package company
  :if is-linux?
  :straight t
  :delight
  :functions company-sort-by-backend-importance
  :preface
  (eval-when-compile
    (defvar company-active-map)
    (defvar company-backends)
    (defvar company-clang-executable)
    (defvar company-dabbrev-downcase)
    (defvar company-echo-delay)
    (defvar company-idle-delay)
    (defvar company-ispell-dictionary)
    (defvar company-minimum-prefix-length)
    (defvar company-show-numbers)
    (defvar company-tooltip-align-annotations)
    (defvar company-tooltip-limit)
    (defvar company-transformers)
    (defvar tychoish-config-path))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode)
  :config
  (eval-after-load 'c-mode
    '(define-key c-mode-map (kbd "[tab]") 'company-complete))
  (define-key company-active-map [return]    'company-complete-common)
  (define-key company-active-map [tab]       'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-common)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)

  (setq company-backends '(company-capf
                           company-keywords
                           company-semantic
                           company-files
                           company-etags
                           company-elisp
                           company-clang
                           company-jedi))
  (setq company-clang-executable "/usr/bin/clang")
  (setq company-dabbrev-downcase nil)
  (setq company-echo-delay 0)
  (setq company-idle-delay 0)
  (setq company-ispell-dictionary (f-join tychoish-config-path "aspell-pws"))
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 20)

  (add-to-list 'company-transformers #'company-sort-by-backend-importance))

(use-package company-jedi
  :if is-linux?
  :straight t
  :commands (company-jedi)
  :after (company python-mode))

(use-package company-plsense :disabled
  :if is-linux?
  :straight t
  :after plsense
  :config
  (add-to-list 'company-backends 'company-plsense))

(use-package cperl-mode
  :mode
  :commands (convert-to-perl cperl-mode perl-mode)
  :init
  (defalias 'perl-mode 'cperl-mode)
  :config
  ;;           (plsense-server-start)
  (define-key cperl-mode-map "{" nil)
  (define-key cperl-mode-map "(" nil)
  (define-key cperl-mode-map "[" nil)
  (message "Loading cperl-mode...done"))

(use-package csharp-mode
  :straight t
  :mode
  :commands csharp-mode
  :config
  (message "Loading cperl-mode...done"))

(use-package cua-base
  :config
  (cua-mode))

(use-package cuda-mode
  :straight t
  :mode
  :commands cuda-mode)

(use-package dash)

(use-package display-line-numbers
  :ensure nil
  :config
  (hook-into-modes #'display-line-numbers-mode
                   'org-mode-hook
                   'prog-mode-hook
                   'text-mode-hook))

(use-package easymenu)

(use-package ediff
  :preface
  (eval-when-compile
    (defvar ediff-merge-revisions-with-ancestor))
  :config
  (setq ediff-keep-variants                    nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor    t)
  (setq ediff-show-clashes-only                t)
  (setq ediff-split-window-function            'split-window-horizontally)
  (setq ediff-window-setup-function            'ediff-setup-windows-plain))

(use-package eglot
  :if is-linux?
  :straight t
  :after company
  :init
  (hook-into-modes #'eglot-ensure 'c++-mode)

  (with-eval-after-load 'project
    (add-to-list 'project-find-functions
                 #'(lambda (dir) (let ((root (projectile-project-root dir))) (and root (cons 'transient root)))))))
  ;; :hook
  ;; ((c++-mode . eglot-ensure)
  ;;  (c++-mode . flycheck-mode)
  ;;  ))

(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))
  ;; :hook (emacs-lisp-mode-hook . eldoc-mode))

(use-package emojify
  :straight t
  :commands emojify-mode)

(use-package ergoemacs-functions
  :commands
  (ergoemacs-backward-open-bracket
   ergoemacs-extend-selection
   ergoemacs-forward-open-bracket
   ergoemacs-move-text-down
   ergoemacs-move-text-up
   ergoemacs-select-text-in-quote
   ergoemacs-shrink-whitespaces)
  :no-require t)

(use-package ergoemacs-mode
  :defer t
  :straight t)

(use-package face-remap :commands (buffer-face-mode text-scale-mode)
  :diminish face-remap-mode
  buffer-face-mode)

(use-package filladapt :commands filladapt-mode)

(use-package flycheck
  :straight t
  :functions global-flycheck-mode
  :preface
  (eval-when-compile
    (defvar flycheck-emacs-lisp-load-path)
    (defvar flycheck-mode-line))
  :config
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
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :straight t
  :after flycheck
  :functions flycheck-pos-tip-mode
  :config
  (flycheck-pos-tip-mode))

(use-package frame
  :config
  (blink-cursor-mode)
  (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
  (on-gui
   (set-background-color "gray95")))

(use-package git-gutter
  :straight t
  :functions global-git-gutter-mode
  :config
  (global-git-gutter-mode t))

(use-package groovy-mode
  :straight t
  :mode
  :commands groovy-mode
  :mode "\\.grv\\'")

(use-package helpful
  :straight t
  :functions (helpful-callable helpful-variable helpful-key)
  :config
  (global-set-key (kbd "C-h   f") #'helpful-callable)
  (global-set-key (kbd "C-h   v") #'helpful-variable)
  (global-set-key (kbd "C-h   k") #'helpful-key)
  (global-set-key (kbd "C-h C-k") #'describe-key))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package hideshow
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))
  ;; :hook (prog-mode . hs-minor-mode))

(use-package isearch
  :diminish isearch-mode
  :config
  (setq isearch-allow-scroll          'unlimited)
  (setq isearch-lax-whitespace        t)
  (setq isearch-lazy-count            nil)
  (setq isearch-lazy-highlight        t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-yank-on-move          'shift)
  ;; (setq lazy-count-prefix-format      nil)
  ;; (setq lazy-count-suffix-format      " (%s/%s)")
  (setq search-highlight              t)
  (setq search-whitespace-regexp      ".*?")
  :bind (:map isearch-mode-map
              ("C-g" . isearch-cancel)
              ("M-/" . isearch-complete)))

(use-package json-mode
  :straight t
  :mode
  :commands json-mode)

(use-package langtool
  :straight t
  :commands langtool-check
  :preface
  (eval-when-compile
    (defvar langtool-language-tool-jar)
    (defvar user-dir-home))
  :config
  (setq langtool-language-tool-jar (concat user-dir-home "Documents/LanguageTool-4.1/languagetool-commandline.jar")))

(use-package loccur
  :straight t
  :commands loccur-current)

(use-package lua-mode
  :straight t
  :mode
  :commands lua-mode)

(use-package magit
  :straight t
  :commands magit-status)

(use-package make-mode
  :mode
  :commands makefile-gmake-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(pro\\|pro\\.sav\\)\\'" . makefile-gmake-mode)))

(use-package mapreplace)

(use-package matlab-mode
  :straight t
  :mode
  :commands matlab-mode)

(use-package mic-paren
  :straight t
  :functions paren-activate
  :config
  (paren-activate))

(use-package minions
  :straight t
  :functions minions-mode
  :config
  (minions-mode 1))

(use-package modern-cpp-font-lock
  :straight t
  :after cc-mode
  :diminish modern-c++-font-lock-mode
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))
  ;; :hook (c++-mode . modern-c++-font-lock-mode))

(use-package msb
  :config
  (setq msb-max-menu-items 32))

(use-package org-mode
  :mode
  :commands org-mode
  :init
  (add-hook 'org-mode-hook #'visual-line-mode))
  ;; :hook (org-mode . visual-line-mode))

(use-package paradox
  :straight t
  :commands paradox-list-packages)

(use-package plsense
  :disabled t
  :if is-linux?
  :straight t
  :after cperl-mode
  :config
  (plsense-config-default)
  (plsense-server-start))

(use-package powerline
  :straight t)

(use-package powerthesaurus
  :straight t
  :commands powerthesaurus-lookup-word)

(use-package pretty-column
  :commands (pretty-column pretty-rectangle)
  :config
  (setq pcol-column-separator "[ \t]+" pcol-str-separator " "))

(use-package projectile
  :straight t
  :after project
  :diminish projectile-mode
  ;; :preface
  ;; (eval-when-compile
  ;;   (defun projectile-project-root))
  :init
  (add-hook 'prog-mode-hook #'projectile-mode)
  :config
  (add-to-list 'project-find-functions #'(lambda (dir)
                                           (let ((root (projectile-project-root dir)))
                                             (and root (cons 'transient root))))))
  ;; )

(use-package python
  :mode
  :commands python-mode)

(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :init
  (hook-into-modes #'smartparens-mode
                   'prog-mode-hook
                   'lisp-interaction-mode-hook))
;; :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :straight t
  :commands rainbow-mode)

(use-package recentf
  :demand
  :config
  (setq recentf-auto-cleanup    'never)
  (setq recentf-save-file       (concat tjf:user/dir-config "recentf"))
  (setq recentf-max-menu-items  32)
  (setq recentf-max-saved-items 200)
  (setq recentf-menu-before     "Open in New Window...")
  (setq recentf-exclude         '(".gz" ".xz" ".zip" "/elpa/"))

  (add-hook 'after-init-hook #'recentf-mode))
  ;; :hook (after-init-hook . recentf-mode))

(use-package replace
  (add-hook 'occur-mode-hook #'(lambda ()
                                 (make-local-variable 'which-function-mode)
                                 (setq which-function-mode nil))))
  ;; :hook
  ;; (occur-mode-hook . (lambda () (make-local-variable 'which-function-mode) (setq which-function-mode nil))))

(use-package s
  :straight t)

(use-package sdcv
  :straight t
  :commands sdcv-search)

(use-package shift-number
  :straight t
  :commands (shift-number-up shift-number-down))

(use-package simple
  :diminish auto-fill-function)

(use-package smartparens
  :straight t
  :diminish smartparens-mode
  :functions sp-local-pair
  :preface
  (eval-when-compile
    (defvar sp-lisp-modes))
  :init
  (hook-into-modes #'smartparens-mode
                   'prog-mode-hook
                   'shell-mode-hook
                   'lisp-interaction-mode-hook)
  ;; :hook ((prog-mode shell-mode lisp-interaction-mode) . smartparens-mode)
  :config
  (sp-local-pair sp-lisp-modes "'" nil :actions nil)
  (sp-local-pair sp-lisp-modes "`" nil :actions nil))

(use-package smerge-mode
  :commands
  smerge-mode)

(use-package smooth-scrolling
  :straight t)

(use-package so-long
  :config
  (global-so-long-mode 1))

;; (use-package tabbar
;;   :straight t
;;   :after powerline
;;   :functions (powerline-render powerline-wave-left powerline-wave-right tabbar-get-tab tabbar-make-tab tabbar-mode tabbar-set-template tabbar-tabs tjf:tabbar/label-function)
;;   :init
;;   (defvar tjf:tabbar/height)
;;   (setq   tjf:tabbar/height 20)
;;   :preface
;;   (eval-when-compile
;;     (defvar tabbar-tab-label-function))
;;   :config
;;   (defun tabbar-add-tab (tabset object &optional _append_ignored)
;;     "Add to TABSET a tab with value OBJECT if there isn't one there yet.
;;  If the tab is added, it is added at the beginning of the tab list,
;;  unless the optional argument APPEND is non-nil, in which case it is
;;  added at the end."
;;     (let ((tabs (tabbar-tabs tabset)))
;;       (if (tabbar-get-tab object tabset)
;;           tabs
;;         (let ((tab (tabbar-make-tab object tabset)))
;;           (tabbar-set-template tabset nil)
;;           (set tabset (sort (cons tab tabs)
;;                             (lambda (a b) (string< (buffer-name (car a))
;;                                                    (buffer-name (car b))))))))))

;;   (defvar tjf:tabbar/left)
;;   (setq   tjf:tabbar/left  (powerline-wave-right 'tabbar-default nil tjf:tabbar/height))
;;   (defvar tjf:tabbar/right)
;;   (setq   tjf:tabbar/right (powerline-wave-left  nil 'tabbar-default tjf:tabbar/height))

;;   (defun tjf:tabbar/label-function (tab)
;;     (powerline-render (list tjf:tabbar/left (format " %s " (car tab)) tjf:tabbar/right)))

;;   (tabbar-mode 1)

;;   ;; tabbar-tab-label-function will be reset after enabling tabbar-mode

;;   (setq tabbar-tab-label-function #'tjf:tabbar/label-function)
;;   (customize-set-variable 'tabbar-separator '(0.0))
;;   (customize-set-variable 'tabbar-use-images nil))

(use-package tetris
  :commands tetris
  :config
  (setq tetris-score-file "/dev/null"))

(use-package text-mode
  :commands text-mode
  :init
  (add-hook 'text-mode-hook #'turn-on-auto-fill))
  ;; :hook (text-mode . turn-on-auto-fill))

(use-package tinyeat
  :commands
  (tinyeat-backward-preserve
   tinyeat-delete-paragraph
   tinyeat-delete-whole-word
   tinyeat-forward-preserve))

(use-package tinysearch
  :commands (tinysearch-search-word-forward tinysearch-search-word-backward))

(use-package tjf-bookmark
  :after (bm tjf-flags))

(use-package tjf-clips
  :after clips-mode
  :functions tjf:clips/setup
  :init
  (add-hook 'clips-mode-hook #'tjf:clips/setup))
  ;; :hook (clips-mode-hook . tjf:clips/setup))

(use-package tjf-color
  :after (cl-macs color))

(use-package tjf-clipboard
  :after tjf-flags)

(use-package tjf-csharp
  :after csharp-mode
  :init
  (add-hook 'csharp-mode-hook #'tjf:csharp/setup))
  ;; :hook (csharp-mode . tjf:csharp/setup))

(use-package tjf-date)

(use-package tjf-duplicate
  :after (tjf-flags tjf-tools undo-tree))

(use-package tjf-edit
  :after (tjf-flags tjf-navigate xah))

(use-package tjf-flags)

(use-package tjf-file
  :after (tjf-flags tjf-frame))

(use-package tjf-keys
  :after (anzu bm tjf-clipboard tjf-duplicate tjf-edit tjf-file tjf-navigate tjf-search tjf-tools undo-tree xah) )

(use-package tjf-frame
  :after (frame tjf-color tjf-flags)
  :functions tjf:frame/reset-size
  :config
  (tjf:frame/reset-size))

(use-package tjf-lisp
  :after elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'tjf:lisp/setup))

(use-package tjf-macro
  :after (diminish f s))

(use-package tjf-matlab
  :after matlab)

(use-package tjf-menubar
  :after (easymenu tjf-clipboard tjf-edit tjf-file tjf-flags tjf-navigate tjf-search tjf-sort tjf-tools tjf-view))

(use-package tjf-msb
  :after msb
  :config
  (msb-mode))

(use-package tjf-navigate
  :after tjf-bookmark)

(use-package tjf-perl
  :after cperl-mode
  :functions convert-to-perl)

(use-package tjf-powerline
  :after powerline)

(use-package tjf-python
  :after python
  :functions tjf:python/setup
  :init
  (add-hook 'python-mode-hook #'tjf:python/setup))
  ;; :hook (python-mode-hook . tjf:python/setup))

(use-package tjf-query-replace
  :commands tjf:query-replace/do)

(use-package tjf-search
  :after (mapreplace tjf-flags))

(use-package tjf-sort)

(use-package tjf-tabline
  :after powerline
  :config
  (tjf:tabline/mode 1)
  (setq tjf:tabline/tab-label-function #'tjf:tabline/label-function))

(use-package tjf-toolbar
  :after (tjf-color tjf-flags tjf-frame tjf-navigate tjf-search))

(use-package tjf-tools
  :after tjf-flags)

(use-package tjf-view
  :after (tjf-clipboard tjf-color tjf-flags tjf-frame))

(use-package treemacs
  :straight t
  :commands treemacs
  :functions (treemacs-follow-mode treemacs-filewatch-mode treemacs-git-mode)
  :preface
  (eval-when-compile
    (defvar treemacs-change-root-without-asking)
    (defvar treemacs-collapse-dirs)
    (defvar treemacs-file-event-delay)
    (defvar treemacs-follow-after-init)
    (defvar treemacs-goto-tag-strategy)
    (defvar treemacs-indentation)
    (defvar treemacs-indentation-string)
    (defvar treemacs-is-never-other-window)
    (defvar treemacs-never-persist)
    (defvar treemacs-no-png-images)
    (defvar treemacs-recenter-after-file-follow)
    (defvar treemacs-recenter-distance)
    (defvar treemacs-show-hidden-files)
    (defvar treemacs-silent-filewatch)
    (defvar treemacs-silent-refresh)
    (defvar treemacs-sorting)
    (defvar treemacs-tag-follow-cleanup)
    (defvar treemacs-tag-follow-delay)
    (defvar treemacs-width))
  :config
  (setq treemacs-change-root-without-asking nil)
  (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0))
  (setq treemacs-file-event-delay           5000)
  (setq treemacs-follow-after-init          t)
  (setq treemacs-goto-tag-strategy          'refetch-index)
  (setq treemacs-indentation                2)
  (setq treemacs-indentation-string         " ")
  (setq treemacs-is-never-other-window      nil)
  (setq treemacs-never-persist              nil)
  (setq treemacs-no-png-images              nil)
  (setq treemacs-recenter-after-file-follow nil)
  (setq treemacs-recenter-distance          0.1)
  (setq treemacs-show-hidden-files          t)
  (setq treemacs-silent-filewatch           nil)
  (setq treemacs-silent-refresh             nil)
  (setq treemacs-sorting                    'alphabetic-case-insensitive-asc)
  (setq treemacs-tag-follow-cleanup         t)
  (setq treemacs-tag-follow-delay           1.5)
  (setq treemacs-width                      35)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-magit
  :straight t
  :after treemacs)

(use-package treemacs-projectile
  :straight t
  :after treemacs)

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :functions global-undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package unicode-fonts
  :straight t
  :functions unicode-fonts-setup
  :preface
  (eval-when-compile
    (defvar unicode-fonts-block-font-mapping))
  (defun tjf:unicode/replace-font-mapping (block-name old-font new-font)
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
     (tjf:unicode/replace-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
   '("Dingbats"
     "Emoticons"
     "Miscellaneous Symbols and Pictographs"
     "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style   'post-forward)
  (setq uniquify-ignore-buffers-re   "^\\*")
  (setq uniquify-strip-common-suffix t))

(use-package vc
  :config
  (setq vc-follow-symlinks t))

(use-package volatile-highlights
  :straight t
  :diminish volatile-highlights-mode
  :functions volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package whitespace
  :commands whitespace-mode
  :diminish whitespace-mode
  :config
  (setq whitespace-style (quote (tabs spaces space-before-tab newline indentation empty space-after-tab space-mark
                                      tab-mark newline-mark))
        whitespace-display-mappings '((space-mark 32 [183] [46])                     ; 32 SPACE     => "·" "."
                                      (newline-mark 10 [182 10])                     ; 10 LINE FEED => "¶ <LINE FEED>"
                                      (tab-mark 9 [9654 32 91 84 65 66 93 9] [92 9]) ;  9 TAB       => "▶ [TAB]<TAB>"
                                      )))

(use-package window
  :config
  (delete-other-windows))

(use-package winner
  :config
  (winner-mode 1))

(use-package ws-butler
  :straight t
  :diminish ws-butler-mode
  :functions ws-butler-global-mode
  :config
  (ws-butler-global-mode))

(use-package yaml-mode
  :straight t
  :mode
  :commands yaml-mode)

(use-package xah)

(use-package xref
  :functions xref-show-definitions-completing-read
  :config
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function       #'xref-show-definitions-completing-read)
  (setq xref-file-name-display          'project-relative)
  (setq xref-search-program             'grep))

;; ================================================================================

;;
(message "default.el ...done")

(emacs-init-time)

;;; default.el ends here
