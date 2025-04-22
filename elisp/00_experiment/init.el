;;; init.el --- Global initialization -*-lexical-binding: t-*- ;; -*-no-byte-compile: t ;; -*-Emacs-Lisp-*-

;;         Copyright © 2000-2025 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   19-Sep-2000

;;; Commentary:

;;; Code:

(defun message--with-timestamp (format-string &rest args)
  "Add FORMAT-STRING timestamp (using ARGS) to `*Messages*' buffer."
  (when (and (>   (length  format-string) 0)
             (not (string= format-string " ")))
    (let ((deactivate-mark nil))
      (save-mark-and-excursion
        (with-current-buffer "*Messages*"
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (when (not (bolp)) (newline))
            (insert (format-time-string "[%T.%3N] " (current-time)))))))))

(advice-add 'message :before 'message--with-timestamp)

(message "Configuring from init.el...")

;;
(enable-theme 'fontaine)

(package-initialize)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")       t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("non-gnu"      . "https://elpa.nongnu.org/nongnu/")   t)

(setq package-archive-priorities '(("gnu"          . 10)
                                   ("non-gnu"      . 20)
                                   ("melpa-stable" . 30)
                                   ("melpa"        . 40)))

;; ================================================================================

(use-package diminish             :ensure t
  :functions diminish
  :config
  (message "Loading diminish...done"))

(use-package dash                 :ensure t   :defer t)

(use-package f                    :ensure t   :defer t
  :config
  (defun basename (&optional filename)
    (if filename
        (f-filename filename)
      (f-filename (buffer-file-name))))
  (defun basename-no-ext (&optional filename)
    (if filename
        (file-name-base filename)
      (file-name-base (buffer-file-name))))
  (defun dirname (&optional filename)
    (if filename
        (f-dirname filename)
      (f-dirname (buffer-file-name))))
  (defun file-extension (&optional filename)
    (if filename
        (f-ext filename)
      (f-ext (buffer-file-name))))
  (message "Loading f...done"))

(use-package s                    :ensure t   :defer t
  :config
  (message "Loading s...done"))

(use-package tjf-macro            :ensure nil)
(use-package tjf-flags            :ensure nil)

;; ================================== completion ==================================

(use-package consult              :ensure t   :after minibuffer
  :config
  (message "Loading consult...done"))

(use-package orderless            :ensure t   :after consult
  :custom
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-styles             '(orderless basic))
  :config
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles   '(orderless-literal orderless-regexp)))
  (message "Loading orderless...done"))

(use-package eglot                :ensure t   :after orderless
  :custom
  (eglot-send-changes-idle-time 0.1)
  :config
  (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode c++-mode c-mode) .
                                        ("clangd"
                                         "-j=8"
                                         "--compile-commands-dir=~/Workspace/tenbeauty/build"
                                         "--log=error"
                                         "--malloc-trim"
                                         "--background-index"
                                         "--clang-tidy"
                                         "--completion-style=detailed"
                                         "--pch-storage=memory"
                                         "--header-insertion=iwyu"
                                         "--header-insertion-decorators=0")))
  (message "Loading eglot...done"))

(use-package consult-eglot        :ensure t   :after eglot
  :config
  (message "Loading consult-eglot...done"))

(use-package cape                 :ensure t   :after consult-eglot
  :config
  (setq completion-at-point-functions
        '(cape-symbol cape-keyword cape-dabbrev cape-file consult-history))
  (message "Loading cape...done"))

(use-package vertico              :ensure t   :after cape
  :config
  (vertico-mode +1)
  (message "Loading vertico...done"))

(use-package vertico-prescient    :ensure t   :after vertico
  :custom
  (prescient-history-length           1000)
  (vertico-prescient-enable-filtering t)
  :config
  (prescient-persist-mode +1)
  (vertico-prescient-mode +1)
  (message "Loading vertico-prescient...done"))

(use-package corfu                :ensure t
  :demand
  :custom
  (corfu-auto               t)
  (corfu-auto-delay         0.25)
  (corfu-auto-prefix        2)
  (corfu-count              8)
  (corfu-cycle              t)
  (corfu-min-width          corfu-max-width)
  (corfu-quit-no-match      'separator)
  :config
  (global-corfu-mode +1)
  (message "Loading corfu...done"))

(use-package corfu-prescient      :ensure t   :after vertico-prescient
  :config
  (message "Loading corfu-prescient...done"))

(use-package marginalia           :ensure t   :after corfu-prescient
  :config
  (marginalia-mode 1)
  (message "Loading marginalia...done"))

(use-package kind-icon            :ensure t   :after marginalia
  :if
  (display-graphic-p)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (message "kind-icon...done"))

(use-package clang-capf           :ensure t   :after (cape cc-mode)
  :hook
  (c++-ts-mode . (lambda ()
                   (setq-local clang-capf-clang "g++")
                   (setq-local clang-capf-include-paths '("/usr/include" "."
                                                          (concat tjf:user/dir-tenbeauty "vision_hardware_control")
                                                          (concat tjf:user/dir-tenbeauty "vision_agent/src")
                                                          (concat tjf:user/dir-tenbeauty "path_planner/src/utils")
                                                          (concat tjf:user/dir-tenbeauty "tenbeauty_api")
                                                          (concat tjf:user/dir-tenbeauty "tenbeauty_ui_qt")))
                   (setq-local clang-capf-extra-flags '("-std=c++20"))
                   (setq-local completion-at-point-functions (cons #'clang-capf completion-at-point-functions))))
  :config
  (message "clang-capf...done"))

;; =================================== modeline ===================================

(use-package powerline            :ensure t
  :custom
   (powerline-gui-use-vcs-glyph t)
  :config
  (message "Loading powerline...done"))

(use-package tjf-powerline        :ensure nil :after powerline
  :hook
  (post-command . tjf:powerline/update-modeline-vars)
  :config
  (alias-face powerline-red-face fontaine/powerline-red)
  (setq powerline-default-separator 'arrow)
  (tjf:powerline/theme))

(use-package anzu                 :ensure t   :after tjf-powerline
  :diminish anzu-mode
  :init
  (defun anzu--update-mode-line-local (here total)
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "I-search: (%s/%d%s) "
                                      (anzu--format-here-position here total)
                                      total (if anzu--overflow-p "+" "")))
                      (replace-query (format "Query Replace: (%d matches) " total))
                      (replace (format "Query Replace:  (%d/%d) " here total)))))
        (propertize status 'face 'anzu-mode-line))))
  :custom
  (anzu-cons-mode-line-p           nil)
  (anzu-mode-lighter               " ")
  (anzu-mode-line-update-function 'anzu--update-mode-line-local)
  :config
  (global-anzu-mode +1)
  (message "Loading anzu...done"))

(use-package minions              :ensure t   :after anzu
  :functions minions-mode
  :config
  (minions-mode 1)
  (message "Loading minions...done"))

;; ==================================== basics ====================================

(use-package emacs                :ensure nil
  :custom
  (buffers-menu-max-size           nil)
  (case-fold-search                t)
  (colon-double-space              nil)
  (completion-cycle-threshold      3)
  (create-lockfiles                nil)
  (cursor-type                     '(bar . 2))
  (disabled-command-function       nil)
  (echo-keystrokes                 0.25)
  (fast-but-imprecise-scrolling    t)
  (font-lock-maximum-decoration    t)
  (fill-column                     8192)
  (gnutls-min-prime-bits           80)
  (initial-scratch-message         nil)
  (line-spacing                    0)
  (mode-require-final-newline      nil)
  (mouse-drag-copy-region          t)
  (mouse-wheel-scroll-amount       '(1 ((shift) . 5) ((meta)) ((control))))
  (scroll-bar-mode                 'right)
  (sentence-end-double-space       nil)
  (sentence-end-without-period     nil)
  (use-hard-newlines               nil)
  (tab-always-indent               'complete)
  (which-func-modes                '(emacs-lisp-mode c-mode c++-mode cperl-mode python-mode diff-mode))
  (x-underline-at-descent-line     t)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'perl-mode   'cperl-mode)

  (set-language-environment    'utf-8)
  (set-default-coding-systems  'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system        'utf-8)

  (setq max-image-size 1024)

  (random t))

(use-package async                :ensure t   :defer t)

(use-package autorevert           :ensure nil
  :diminish autorevert-mode
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t)
  (auto-revert-interval      5)
  (auto-revert-verbose       t)
  :config
  (global-auto-revert-mode))

(use-package cl-macs              :ensure nil)

(use-package comint               :ensure nil :commands (shell-mode eshell-mode tjf:tools/open-new-shell)
  :custom
  (comint-input-ignoredups t)
  (comint-input-ring-size  64)
  :hook
  (comint-mode . (lambda ()
                   (setq-local completion-at-point-functions (cons #'comint-completion-at-point completion-at-point-functions))
                   (corfu-mode -1))))

(use-package cua-base             :ensure nil
  :config
  (cua-mode))

(use-package easymenu             :ensure nil)

(use-package files                :ensure nil
  :custom
  (auto-save-file-name-transforms `((".*"   ,tjf:user/dir-autosave t)))
  (backup-directory-alist         `((".*" . ,tjf:user/dir-backup)))
  :config
  (setq auto-mode-alist
        '(("\\.\\(deb\\|[oi]pk\\)\\'" . archive-mode)
          ("\\.[sS]\\'" . asm-mode)
          ("\\.asm\\'"  . asm-mode)
          ("\\(acinclude\\|aclocal\\|acsite\\)\\.m4\\'" . autoconf-mode)
          ("configure\\.\\(ac\\|in\\)\\'"               . autoconf-mode)
          ("\\.awk\\'"             . awk-mode)
          ("\\.bz\\'"              . bazel-mode)
          ("\\.c\\'"               . c-mode)
          ("\\.\\(C\\|H\\)\\'"     . c-mode)
          ("\\.xs\\'"              . c-mode)
          ("\\.h\\'"               . c-or-c++-mode)
          ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
          ("\\.\\(CC\\|HH\\)\\'"              . c++-mode)
          ("\\.\\(cc\\|hh\\)\\'"              . c++-mode)
          ("\\.\\(proto\\|tpp\\)\\'"          . c++-mode)
          ("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)
          ("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode)
          ("/\\.?\\(?:gitconfig\\|gnokiirc\\|hgrc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
          ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'"                   . conf-mode-maybe)
          ("/\\.\\(?:gtk\\|net\\|nvidia-settings-\\|screen\\|xmp\\)rc\\'"                  . conf-mode)
          ("\\.cs'"         . csharp-mode)
          ("\\.css\\'"      . css-mode)
          ("\\.csv\\'"      . csv-mode)
          ("\\.el\\'"       . emacs-lisp-mode)
          ("\\.emacs\\'"    . emacs-lisp-mode)
          ("\\.f9[05]\\'"   . f90-mode)
          ("\\.f0[38]\\'"   . f90-mode)
          ("\\.[fF]\\'"     . fortran-mode)
          ("\\.for\\'"      . fortran-mode)
          ("\\.bmp\\'"      . image-mode)
          ("\\.cmyka?\\'"   . image-mode)
          ("\\.gif\\'"      . image-mode)
          ("\\.icon?\\'"    . image-mode)
          ("\\.jpe?g\\'"    . image-mode)
          ("\\.p[bpgn]m\\'" . image-mode)
          ("\\.png\\'"      . image-mode)
          ("\\.rgba?\\'"    . image-mode)
          ("\\.svgz?\\'"    . image-mode)
          ("\\.tga\\'"      . image-mode)
          ("\\.tiff?\\'"    . image-mode)
          ("\\.webp\\'"     . image-mode)
          ("\\.x[bp]m\\'"   . image-mode)
          ("\\.xcf\\'"      . image-mode)
          ("\\.java\\'"     . java-mode)
          ("\\.zst\\'"      . jka-compr)
          ("\\.dz\\'"       . jka-compr)
          ("\\.xz\\'"       . jka-compr)
          ("\\.lzma\\'"     . jka-compr)
          ("\\.lz\\'"       . jka-compr)
          ("\\.g?z\\'"      . jka-compr)
          ("\\.bz2\\'"      . jka-compr)
          ("\\.Z\\'"        . jka-compr)
          ("\\.json\\'"     . json-ts-mode)
          ("\\.ltx\\'"      . latex-mode)
          ("\\.l\\'"        . lisp-mode)
          ("\\.li?sp\\'"    . lisp-mode)
          ("\\.am\\'"                    . makefile-automake-mode)
          ("\\.mk\\'"                    . makefile-gmake-mode)
          ("\\.make\\'"                  . makefile-gmake-mode)
          ("[Mm]akefile\\'"              . makefile-gmake-mode)
          ("Imakefile\\'"                . makefile-imake-mode)
          ("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode)
          ("\\.makepp\\'"                . makefile-makepp-mode)
          ("\\.mk\\'"                    . makefile-gmake-mode)
          ("\\.man\\'"     . nroff-mode)
          ("\\.[1-9]\\'"   . nroff-mode)
          ("\\.org\\'"     . org)
          ("\\.py[iw]?\\'" . python-mode)
          ("\\.p\\'"       . pascal-mode)
          ("\\.pas\\'"     . pascal-mode)
          ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . cperl-mode)
          ("\\.[eE]?[pP][sS]\\'" . ps-mode)
          ("\\.rb\\'"            . ruby-mode)
          ("\\.[ckz]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'"                                                  . sh-mode)
          ("\\.bash\\'"                                                                                      . sh-mode)
          ("\\(/\\|\\`\\)\\.\\(bash_\\(profile\\|history\\|log\\(in\\|out\\)\\)\\|z?log\\(in\\|out\\)\\)\\'" . sh-mode)
          ("\\(/\\|\\`\\)\\.\\(shrc\\|zshrc\\|m?kshrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'"                       . sh-mode)
          ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'"                              . sh-mode)
          ("\\.sql\\'"       . sql-mode)
          ("\\.tar\\'"       . tar-mode)
          ("\\.tgz\\'"       . tar-mode)
          ("\\.tbz2?\\'"     . tar-mode)
          ("\\.txz\\'"       . tar-mode)
          ("\\.tzst\\'"      . tar-mode)
          ("\\.[tT]e[xX]\\'" . tex-mode)
          ("\\.texinfo\\'"   . texinfo-mode)
          ("\\.te?xi\\'"     . texinfo-mode)
          ("\\.te?xt\\'"     . text-mode)
          ("\\.ya?ml\\'"      . yaml-mode))))

(use-package tjf-file             :ensure nil)

(use-package ffap                 :ensure nil :commands ffap-at-mouse)

(use-package frame                :ensure nil
  :custom
  (frame-title-format  "%b")
  (blink-cursor-blinks 0)
  :config
  (blink-cursor-mode)
  (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
  (on-gui
   (set-background-color "gray95")))

(use-package tjf-frame            :ensure nil :after frame
  :functions tjf:frame/reset-size
  :config
  (tjf:frame/reset-size))

(use-package hl-line              :ensure nil
  :config
  (global-hl-line-mode)
  (message "Loading hl-line...done"))

(use-package isearch              :ensure nil
  :diminish isearch-mode
  :custom
  (isearch-allow-scroll     'unlimited)
  (isearch-lax-whitespace   t)
  (isearch-lazy-count       nil)
  (isearch-lazy-highlight   t)
  (isearch-yank-on-move     'shift)
  (search-highlight         t)
  (search-whitespace-regexp "\s+?")
  :config
  (setq isearch-regexp-lax-whitespace nil))

(use-package msb                  :ensure nil
  :custom
  (msb-display-invisible-buffers-p t)
  (msb-max-menu-items              nil))

(use-package recentf              :ensure nil
  :demand t
  :custom
  (recentf-auto-cleanup    'never)
  (recentf-save-file       (concat tjf:user/dir-config "recentf"))
  (recentf-max-menu-items  32)
  (recentf-max-saved-items 200)
  (recentf-menu-before     "Open in New Window...")
  (recentf-exclude         '(".gz" ".xz" ".zip"))
  :hook
  (after-init . recentf-mode))

(use-package replace              :ensure nil
  :hook
  (occur-mode . (lambda ()
                  (make-local-variable 'which-function-mode)
                  (setq which-function-mode nil))))

(use-package simple               :ensure nil
  :diminish auto-fill-function
  :custom
  (kill-do-not-save-duplicates t)
  (indent-tabs-mode            nil)
)

(use-package so-long              :ensure nil :after tjf-menubar
  :config
  (global-so-long-mode 1))

(use-package tetris               :ensure nil :commands tetris
  :config
  (setq tetris-score-file "/dev/null"))

(use-package tjf-mode             :ensure nil :after msb)

(use-package tjf-msb              :ensure nil :after tjf-mode
  :config
  (msb-mode))

(use-package uniquify             :ensure nil :defer t
  :custom
  (uniquify-buffer-name-style   'post-forward)
  (uniquify-ignore-buffers-re   "^\\*")
  (uniquify-strip-common-suffix t))

(use-package vc                   :ensure nil :defer tjf-menubar
  :custom
  (vc-follow-symlinks t))

(use-package window               :ensure nil
  :custom
  (scroll-error-top-bottom         t)
  (scroll-preserve-screen-position t)
  :config
  (delete-other-windows))

(use-package winner               :ensure nil :defer t
  :config
  (winner-mode 1))

(use-package xref                 :ensure nil :defer t
  :functions xref-show-definitions-completing-read
  :custom
  (xref-show-definitions-function 'xref-show-definitions-completing-read)
  (xref-show-xrefs-function       'xref-show-definitions-completing-read)
  (xref-file-name-display         'project-relative)
  (xref-search-program            'grep))

;; ================================== appearance ==================================

(use-package color                :ensure nil)

(use-package tjf-color            :ensure nil)

(use-package display-line-numbers :ensure nil
  :config
  (hook-into-modes #'display-line-numbers-mode
                   'prog-mode-hook
                   'csv-mode-hook
                   'org-mode-hook
                   'text-mode-hook))

(use-package emojify              :ensure t   :commands emojify-mode
  :config
  (message "Loading emojify...done"))

(use-package face-remap           :ensure nil :commands (buffer-face-mode text-scale-mode)
  :diminish face-remap-mode
  buffer-face-mode)

(use-package tjf-tabline          :ensure nil :after tjf-powerline :disabled
  :custom
  (tjf:tabline/separator  '(0.0))
  (tjf:tabline/use-images nil)
  :config
  (setq tjf:tabline/tab-label-function 'tjf:tabline/label-function)
  (tjf:tabline/mode 1))

(use-package textsize             :ensure t   :commands textsize-mode
  :config
  (message "Loading textsize...done"))

(use-package unicode-fonts        :ensure t   :after tjf-menubar
  :init
  (defun tjf:unicode/emoji-fonts ()
    (set-fontset-font t1'symbol "Noto Color Emoji")
    (set-fontset-font t 'symbol "Symbola" nil 'append))
  :config
  (tjf:unicode/emoji-fonts)
  (message "Loading unicode-fonts...done"))

(use-package volatile-highlights  :ensure t   :after tjf-menubar
  :diminish  volatile-highlights-mode
  :functions volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (message "Loading volatile-highlights...done"))

;; ================================== convenience =================================

(use-package bash-completion      :ensure t   :after shell
  :config
  (bash-completion-setup)
  (message "Loading bash-completion...done"))

(use-package bm                   :ensure t
  :functions (bm-buffer-save-all bm-repository-load bm-repository-save)
  :hook
  (after-save . bm-buffer-save)
  ;; (add-hook 'after-revert-hook 'bm-buffer-restore)
  ;; (add-hook 'find-file-hook    'bm-buffer-restore)
  ;; (add-hook 'kill-buffer-hook  'bm-buffer-save)
  ;; (add-hook 'kill-emacs-hook   'bm-buffer-save-all)
  ;; (add-hook 'kill-emacs-hook   'bm-repository-save)
  :custom
  (bm-buffer-persistence t)
  (bm-cycle-all-buffers  t)
  (bm-highlight-style    'bm-highlight-only-fringe)
  (bm-repository-file    (concat tjf:user/dir-config "bookmarks"))
  :config
  (bm-repository-load)
  ;; Make a more bookmarky symbol for a 'mark':
  (define-fringe-bitmap 'bm-marker-left [0 0 15 15 15 15 0 0] 8 4 'center)
  (define-key bm-show-mode-map [mouse-1] 'bm-show-goto-bookmark)
  (define-key bm-show-mode-map [mouse-2] 'bm-show-goto-bookmark)
  (message "Loading bm...done"))

(use-package clean-aindent-mode   :ensure t   :after tjf-menubar
  :functions clean-aindent-mode
  :custom
  (clean-aindent-is-simple-indent t)
  :config
  (electric-indent-mode -1)
  (message "Loading clean-aindent-mode...done"))

(use-package ctrlf                :ensure t   :defer t
  :config
  (ctrlf-mode +1)
  (message "Loading ctrlf...done"))

(use-package tjf-date             :ensure nil)

(use-package ergoemacs-mode       :ensure t   :defer t
  :config
  (message "Loading ergoemacs-mode...done"))

(use-package ergoemacs-functions  :ensure nil
  :commands
  (ergoemacs-backward-open-bracket
   ergoemacs-extend-selection
   ergoemacs-forward-open-bracket
   ergoemacs-move-text-down
   ergoemacs-move-text-up
   ergoemacs-select-text-in-quote
   ergoemacs-shrink-whitespaces)
  :no-require t
 :config
  (message "Loading ergoemacs-functions...done"))

(use-package ediff                :ensure nil :commands ediff
  :custom
  (ediff-keep-variants                    nil)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-show-clashes-only                t)
  (ediff-split-window-function            'split-window-horizontally)
  (ediff-window-setup-function            'ediff-setup-windows-plain))

(use-package executable           :ensure nil :commands shell-script-mode
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package helpful              :ensure t   :after tjf-menubar
  :commands  (helpful-callable helpful-variable helpful-key)
  :functions (helpful-callable helpful-variable helpful-key)
  :config
  (global-set-key (kbd "C-h   f") 'helpful-callable)
  (global-set-key (kbd "C-h   v") 'helpful-variable)
  (global-set-key (kbd "C-h   k") 'helpful-key)
  (global-set-key (kbd "C-h C-k") 'describe-key)
  (message "Loading helpful...done"))

(use-package indent-bars          :ensure t :after treesit
  :custom
  (indent-bars-treesit-support nil)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope 
   '((c++ function_definition class_definition for_statement if_statement while_statement)))
  :hook
  (paragraph-indent-text-mode . indent-bars-mode)
  (prog-mode                  . indent-bars-mode)
  (text-mode                  . indent-bars-mode)
  :config
  (message "Loading indent-bars...done"))

(use-package jinx                 :ensure t   :commands jinx-mode
  :hook
  (markdown-mode              . jinx-mode)
  (paragraph-indent-text-mode . jinx-mode)
  (text-mode                  . jinx-mode))

(use-package langtool             :ensure t   :commands langtool-check
  :custom
  (langtool-language-tool-jar (concat tjf:user/dir-home "Documents/LanguageTool-4.1/languagetool-commandline.jar")))

(use-package loccur               :ensure t   :commands loccur-current)

(use-package org                  :ensure t   :commands org-mode
  :hook
  (org-mode . visual-line-mode))

(use-package paradox              :ensure t   :commands paradox-list-packages)

(use-package powerthesaurus       :ensure t   :commands powerthesaurus-lookup-dwim)

(use-package rainbow-mode         :ensure nil :commands rainbow-mode)

(use-package sdcv                 :ensure nil :commands sdcv-search)

(use-package smooth-scrolling     :ensure t   :after tjf-menubar
  :config
  (message "Loading smooth-scrolling...done"))

(use-package tjf-bookmark         :ensure nil :after bm)

(use-package tjf-clipboard        :ensure nil)

(use-package tjf-keys             :ensure nil   :after tjf-menubar)

(use-package tjf-menubar          :ensure nil
  :custom
  (recentf-menu-before "Open in New Window...")
  :hook
  (menu-bar-update-hook . tjf:navigate/menu)
  :config
  (recentf-mode))

(use-package tjf-navigate         :ensure nil)

(use-package tjf-search           :ensure nil)

(use-package tjf-toolbar          :ensure nil)

(use-package tjf-tools            :ensure nil)

(use-package tjf-view             :ensure nil)

(use-package treemacs             :ensure t   :commands treemacs
  :functions (treemacs-follow-mode treemacs-filewatch-mode treemacs-git-mode)
  :custom
  (treemacs-collapse-dirs              (if (executable-find "python") 3 0))
  (treemacs-file-event-delay           5000)
  (treemacs-follow-after-init          t)
  (treemacs-goto-tag-strategy          'refetch-index)
  (treemacs-indentation                2)
  (treemacs-indentation-string         " ")
  (treemacs-is-never-other-window      nil)
  (treemacs-no-png-images              nil)
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-distance          0.1)
  (treemacs-show-hidden-files          t)
  (treemacs-silent-filewatch           nil)
  (treemacs-silent-refresh             nil)
  (treemacs-sorting                    'alphabetic-case-insensitive-asc)
  (treemacs-tag-follow-cleanup         t)
  (treemacs-tag-follow-delay           1.5)
  (treemacs-width                      35)
  :config
  (setq treemacs-never-persist nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-magit       :ensure t   :after (treemacs magit)
  :config
  (message "Loading treemacs-magit...done"))

(use-package treemacs-projectile  :ensure t   :after (treemacs projectile) :disabled)

(use-package undo-fu              :ensure t :commands (undo-fu-only-undo undo-fu-only-redo))

(use-package whitespace           :ensure nil :commands whitespace-mode
  :diminish whitespace-mode
  :custom
  (whitespace-display-mappings 
   '((space-mark 32 [183] [46])                       ; 32 SPACE     => "·" "."
     (newline-mark 10 [182 10])                       ; 10 LINE FEED => "¶ <LINE FEED>"
     (tab-mark 9 [9654 32 91 84 65 66 93 9] [92 9]))) ;  9 TAB       => "▶ [TAB]<TAB>"
  (whitespace-style (quote (tabs spaces space-before-tab newline indentation empty 
                                 space-after-tab space-mark tab-mark newline-mark))))

(use-package ws-butler            :ensure t   :after tjf-menubar
  :diminish ws-butler-mode
  :functions ws-butler-global-mode
  :config
  (ws-butler-global-mode)
  (message "Loading ws-butler...done"))

;; ==================================== coding ====================================

(use-package bazel                :ensure t   :commands bazel-mode
  :hook
  (bazel-mode . (lambda ()
                  (setq-local completion-at-point-functions
                              (cons #'bazel-completion-at-point completion-at-point-functions))))
  :config
  (message "Loading bazel...done"))

(use-package blamer               :ensure t   :defer t
  :custom
 (blamer-avatar-folder      "~/.config/emacs/blamer/avatars/")
 (blamer-smart-background-p nil)  
  :config
  (global-blamer-mode -1)
  (message "Loading blamer...done"))

(use-package cc-mode              :ensure nil :commands (c-ts-mode c-mode c++-mode)
  :config
  (message "!!!!!!! HOLA !!!!!!"))

(use-package cperl-mode           :ensure nil :commands (cperl-mode perl-mode)
  :init
  (mapc (lambda (pair)
          (if (eq (cdr pair) 'perl-mode)
              (setcdr pair 'cperl-mode)))
        (append auto-mode-alist interpreter-mode-alist))
  :custom
  (cperl-hairy                        t)
  (cperl-indent-region-fix-constructs nil)
  ;;
  :config
  (setq cperl-style-alist (append cperl-style-alist '(("TJF"
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
  (define-key cperl-mode-map [?\t]      #'(lambda nil (interactive) (if mark-active (indent-region (region-beginning) (region-end)) (indent-for-tab-command))))
  (define-key cperl-mode-map "{"        nil)
  (define-key cperl-mode-map "("        nil)
  (define-key cperl-mode-map "["        nil)
  (message "Loading cperl-mode...done"))

(use-package perl-ts-mode         :ensure t   :commands perl-ts-mode)

(use-package tjf-perl             :ensure nil :after cperl-mode :commands tjf:perl/convvert
  :hook
  (cperl-mode . tjf:perl/hook)
  :config
  (tjf:perl/config)

  (add-to-list 'eglot-server-programs '(cperl-mode . ("perl" "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run"))))

(use-package cpp-auto-include     :ensure t   :after tjf-cpp
  :config
  (cpp-auto-include)
  (message "Loading cpp-auto-include...done"))

(use-package csharp-mode          :ensure nil :commands csharp-mode)

(use-package csv-mode             :ensure t :commands csv-mode
  :init
  (defun csv-highlight (&optional separator)
    (interactive (list (when current-prefix-arg (read-char "Separator: "))))
    (font-lock-mode 1)
    (let* ((separator (or separator ?\,))
           (n (count-matches (string separator) (line-beginning-position) (line-end-position)))
           (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                         collect (apply 'color-rgb-to-hex
                                        (color-hsl-to-rgb i 0.3 0.5)))))
      (loop for i from 2 to n by 2
            for c in colors
            for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
            do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
  :hook
  (csv-mode . csv-guess-set-separator)
  (csv-mode . csv-highlight))

(use-package c-ts-mode            :ensure nil :after cc-mode
  :config
  (message "Loading c-ts-mode...done"))

(use-package c++-ts-mode            :ensure nil :after cc-mode
  :config
  (message "Loading c++-ts-mode...done"))

(use-package eldoc                :ensure nil
  :diminish eldoc-mode
  :hook
  (emacs-lisp-mode . eldoc-mode))

(use-package flycheck             :ensure t   :after tjf-menubar
  :functions global-flycheck-mode
  :custom
  (flycheck-mode-line
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
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode)
  (message "Loading flycheck...done"))

(use-package flycheck-pos-tip     :ensure t   :after (flycheck pos-tip) :disabled
  :functions flycheck-pos-tip-mode
  :config
  (flycheck-pos-tip-mode)
  (message "Loading flycheck-pos-tip...done"))

(use-package git-gutter           :ensure t   :after tjf-menubar
  :functions global-git-gutter-mode
  :config
  (global-git-gutter-mode t)
  (message "Loading git-gutter...done"))

(use-package imenu                :ensure nil
  :custom
  (custom-enabled-themes '(fontaine))
  (custom-safe-themes t))

(use-package json-ts-mode         :ensure nil :commands json-ts-mode)

(use-package magit                :ensure t   :commands magit-status)

(use-package make-mode            :ensure nil :commands makefile-gmake-mode
  :hook
  (makefile-gmake-mode . (lambda () 
                           (setq-local completion-at-point-functions
                                       (cons #'makefile-completions-at-point completion-at-point-functions)))))
 
(use-package markdown-mode        :ensure nil :commands markdown-mode
  :hook
  (markdown-mode . (lambda () 
                     (setq-local completion-at-point-functions (cons #'markdown-complete-at-point completion-at-point-functions)))))

(use-package modern-cpp-font-lock :ensure t   :after cc-mode
  :diminish modern-c++-font-lock-mode
  :hook
  (c++-ts-mode . modern-c++-font-lock-mode)
  (c++-mode    . modern-c++-font-lock-mode)
  :config
  (message "Loading modern-cpp-font-lock...done"))

(use-package modern-sh            :ensure t
  :hook
  (sh-mode . modern-sh-mode)
  :config
  (message "Loading modern-sh...done"))

(use-package pos-tip              :ensure t   :after flycheck
  :config
  (message "Loading pos-tip...done"))

(use-package python               :ensure nil :commands python-mode
  :hook
  (python-mode . (lambda () 
                   (setq-local completion-at-point-functions (cons #'python-completion-at-point completion-at-point-functions))))
  (python-ts-mode . (lambda () 
                      (setq-local completion-at-point-functions (cons #'python-completion-at-point completion-at-point-functions))))
  :custom
  (python-indent-guess-indent-offset-verbose nil))

(use-package anaconda-mode        :ensure t   :after python
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  :config
  (message "Loading anaconda-mode...done"))

(use-package tjf-python           :ensure nil :after python
  :hook
  (python-mode . tjf:python/hook)
  :config
  (tjf:python/config))

(use-package shell                :ensure nil 
  :custom
  (explicit-shell-file-name "/bin/bash"))

(use-package tex-mode             :ensure nil :commands tex-mode
  :config
  (define-key latex-mode-map [(control return)] 'tjf:edit/insert-newline-after))

(use-package text-mode            :ensure nil :commands text-mode
  :hook
  (text-mode . turn-on-auto-fill))

(use-package tjf-c                :ensure nil :after tjf-cc
  :hook
  (c-ts-mode . tjf:c/hook)
  :config
  (tjf:c/config))

(use-package tjf-cc               :ensure nil :after cc-mode)

(use-package tjf-clips            :ensure nil :after clips-mode :disabled
  :functions tjf:clips/setup
  :hook
  (clips-mode-hook . tjf:clips/setup))

(use-package tjf-lisp             :ensure nil :defer 1
  :hook
  (emacs-lisp-mode . tjf:lisp/hook)
  :config
  (tjf:lisp/config))

(use-package treesit              :ensure nil :after files
  :custom
  (treesit-font-lock-level 4)

  :config
  (add-to-list 'major-mode-remap-alist '(c-mode        . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode      . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(cmake-mode    . cmake-ts-mode))
  (add-to-list 'major-mode-remap-alist '(cperl-mode    . perl-ts-mode))
  (add-to-list 'major-mode-remap-alist '(dockfile-mode . dockerfile-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-mode       . go-ts-mode))
  (add-to-list 'major-mode-remap-alist '(html-mode     . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(java-mode     . java-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode       . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode  . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(json-mode     . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode     . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(rust-mode     . rust-ts-mode))
  (add-to-list 'major-mode-remap-alist '(yaml-mode     . yaml-ts-mode))
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"       "v0.20.2"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"          "v0.20.5"))
          (c-sharp    . ("https://github.com/tree-sitter/tree-sitter-c-sharp"    "v0.20.0"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake"             "v0.4.1"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"        "v0.20.3"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"        "v0.20.0"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"          "1.3.0"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"       "v0.19.0"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "release"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"       "v0.20.1"))
          (latex      . ("https://github.com/latex-lsp/tree-sitter-latex"        "v0.3.0"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"         "release"))
          (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown"        "v0.1.7"))
          (pod        . ("https://github.com/tree-sitter-perl/tree-sitter-pod"   "release"))
          (perl       . ("https://github.com/tree-sitter-perl/tree-sitter-perl"  "release"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"     "v0.20.4"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"          "v0.5.0"))))
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  (message "Loading treesit...done"))

;; Commands for using ts-fold.
;; Commands                 Description
;; ts-fold-close            fold the current syntax node.
;; ts-fold-open             open the outermost fold of the current syntax node. Keep the sub-folds close.
;; ts-fold-open-recursively open all folds inside the current syntax node.
;; ts-fold-close-all        close all foldable syntax nodes in the current buffer.
;; ts-fold-open-all         open all folded syntax nodes in the current buffer.
;; ts-fold-toggle           toggle the syntax node at `point'.
;;
(use-package treesit-fold         :ensure t   :after treesit
  :config
  (global-treesit-fold-indicators-mode)
  (message "Loading treesit-fold...done"))

(use-package yaml-mode            :ensure t   :commands yaml-mode)

;; ==================================== parens ====================================

(use-package mic-paren            :ensure t   :after tjf-menubar
  :functions paren-activate
  :config
  (paren-activate))

(use-package rainbow-delimiters   :ensure t   :commands rainbow-delimiters-mode
  :hook
  (eshell-mode . rainbow-delimiters-mode)
  (latex-mode  . rainbow-delimiters-mode)
  (prog-mode   . rainbow-delimiters-mode)
  (shell-mode  . rainbow-delimiters-mode))

(use-package smartparens          :ensure t   :commands smartparens-mode
  :diminish smartparens-mode
  :functions sp-local-pair
  :hook
  (eshell-mode . smartparens-mode)
  (latex-mode  . smartparens-mode)
  (prog-mode   . smartparens-mode)
  (shell-mode  . smartparens-mode)  
  :custom
  (sp-highlight-pair-overlay     nil)
  (sp-highlight-wrap-overlay     nil)
  (sp-highlight-wrap-tag-overlay nil)
  :config
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length   4)

  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))
  (sp-local-pair sp-lisp-modes "'" nil :actions nil)
  (sp-local-pair sp-lisp-modes "`" nil :actions nil))

;; =================================== editing ====================================

(use-package pretty-column        :ensure nil :commands (pretty-column pretty-rectangle)
  :config
  (setq pcol-column-separator "[ \t]+")
  (setq pcol-str-separator    " "))

(use-package mapreplace           :ensure nil :commands (mapreplace-regexp mapreplace-string query-mapreplace query-mapreplace-regexp))

(use-package shift-number         :ensure t   :commands (shift-number-up shift-number-down))

(use-package tjf-duplicate        :ensure nil :after undo-fu)

(use-package tjf-edit             :ensure nil)

(use-package tjf-query-replace    :ensure nil :commands tjf:query-replace/do)

(use-package tjf-sort             :ensure nil)

;;
(provide 'init)

;;; init.el ends here
