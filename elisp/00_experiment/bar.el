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

(message "Loading  init.el...")

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

;; ==================================== emacs =====================================

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

;; ================================================================================

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

(use-package async                :ensure t   :defer t)

(use-package autorevert           :ensure nil :defer t
  :diminish autorevert-mode
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t)
  (auto-revert-interval      5)
  (auto-revert-verbose       t)
  :config
  (global-auto-revert-mode))

(use-package cape                 :ensure t   :after consult-eglot
  :config
  (setq completion-at-point-functions
        '(cape-symbol cape-keyword cape-dabbrev cape-file consult-history))
  (message "Loading cape...done"))

(use-package cl-macs              :ensure nil)

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

(use-package comint               :ensure nil :commands (shell-mode eshell-mode tjf:tools/open-new-shell)
  :custom
  (comint-input-ignoredups t)
  (comint-input-ring-size  64)
  :hook
  (comint-mode . (lambda ()
                   (setq-local completion-at-point-functions (cons #'comint-completion-at-point completion-at-point-functions))
                   (corfu-mode -1))))

(use-package consult              :ensure t   :after minibuffer
  :config
  (message "Loading consult...done"))

(use-package consult-eglot        :ensure t   :after eglot
  :config
  (message "Loading consult-eglot...done"))

(use-package corfu                :ensure t
  :demand
  :custom
  (corfu-auto               t)
  (corfu-auto-delay         0.25)
  (corfu-auto-prefix        2)
  (corfu-count              8)
  (corfu-cycle              t)
  (corfu-quit-no-match      'separator)
  :config
  (global-corfu-mode +1)
  (message "Loading corfu...done"))

(use-package corfu-prescient      :ensure t   :after vertico-prescient
  :config
  (message "Loading corfu-prescient...done"))

(use-package cua-base             :ensure nil
  :config
  (cua-mode))

(use-package diminish             :ensure t   :defer t
  :functions diminish
  :config
  (message "Loading diminish...done"))

(use-package easymenu             :ensure nil)

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

(use-package ffap                 :ensure nil :commands ffap-at-mouse)

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

(use-package frame                :ensure nil
  :custom
  (frame-title-format  "%b")
  (blink-cursor-blinks 0)
  :config
  (blink-cursor-mode)
  (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
  (on-gui
   (set-background-color "gray95")))

(use-package kind-icon            :ensure t   :after marginalia
  :if
  (display-graphic-p)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (message "kind-icon...done"))

(use-package orderless            :ensure t   :after consult
  :custom
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-styles             '(orderless basic))
  :config
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles   '(orderless-literal orderless-regexp)))
  (message "Loading orderless...done"))

(use-package marginalia           :ensure t   :after corfu-prescient
  :config
  (marginalia-mode 1)
  (message "Loading marginalia...done"))

(use-package s                    :ensure t   :defer t
  :config
  (message "Loading s...done"))

(use-package tjf-flags            :ensure nil)

(use-package tjf-file             :ensure nil)

(use-package tjf-frame            :ensure nil :after frame
  :functions tjf:frame/reset-size
  :config
  (tjf:frame/reset-size))

(use-package tjf-macro            :ensure nil)

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

(message "Loading compeletion...done")


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

(use-package powerline            :ensure t
  :custom
   (powerline-gui-use-vcs-glyph t)
  :config
  (message "Loading powerline...done"))

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
  (indent-tabs-mode            nil))

(use-package so-long              :ensure nil :defer t
  :config
  (global-so-long-mode 1))

(use-package tetris               :ensure nil :commands tetris
  :config
  (setq tetris-score-file "/dev/null"))

(use-package tjf-mode             :ensure nil :after msb)

(use-package tjf-msb              :ensure nil :after tjf-mode
  :config
  (msb-mode))

(use-package tjf-powerline        :ensure nil :after powerline
  :hook
  (post-command . tjf:powerline/update-modeline-vars)
  :config
  (alias-face powerline-red-face fontaine/powerline-red)
  (setq powerline-default-separator 'arrow)
  (tjf:powerline/theme))

(use-package uniquify             :ensure nil :defer t
  :custom
  (uniquify-buffer-name-style   'post-forward)
  (uniquify-ignore-buffers-re   "^\\*")
  (uniquify-strip-common-suffix t))

(use-package vc                   :ensure nil :defer t
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

;; =================================== modeline ===================================

(use-package minions              :ensure t   :after anzu
  :functions minions-mode
  :config
  (minions-mode 1)
  (message "Loading minions...done"))

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

(use-package tjf-tabline          :ensure nil :after tjf-powerline
  :custom
  (tjf:tabline/separator  '(0.0))
  (tjf:tabline/use-images nil)
  :config
  (setq tjf:tabline/tab-label-function 'tjf:tabline/label-function)
  (tjf:tabline/mode 1))

(use-package textsize             :ensure t   :commands textsize-mode
  :config
  (message "Loading textsize...done"))

(use-package unicode-fonts        :ensure t   :defer t
  :init
  (defun tjf:unicode/emoji-fonts ()
    (set-fontset-font t1'symbol "Noto Color Emoji")
    (set-fontset-font t 'symbol "Symbola" nil 'append))
  :config
  (tjf:unicode/emoji-fonts)
  (message "Loading unicode-fonts...done"))

(use-package volatile-highlights  :ensure t
  :diminish  volatile-highlights-mode
  :functions volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (message "Loading volatile-highlights...done"))
