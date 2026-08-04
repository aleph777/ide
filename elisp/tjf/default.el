;;; default.el --- Global initialization -*-lexical-binding: t-*- ;; --*-no-byte-compile: t ;; *-Emacs-Lisp-*-

;;         Copyright © 2000-2026 Tom Fontaine

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

;;; Code:

(message "Configuring from default.el...")

;;
(enable-theme 'fontaine)

(defvar tjf:default/package-initialize nil
  "Package initialization guard.")

(unless tjf:default/package-initialize
  (package-initialize)
  (add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")       t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("non-gnu"      . "https://elpa.nongnu.org/nongnu/")   t)

  (setq package-archive-priorities '(("gnu"				.	10)
									 ("non-gnu"			.	20)
									 ("melpa-stable"	.	30)
									 ("melpa"			.	40)))
  (setq tjf:default/package-initialize t))

;; ================================================================================

(use-package diminish             :ensure t   :demand
  :config
  (message "Config diminish...done"))

(use-package f                    :ensure t  :demand
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
      (f-ext (buffer-file-name)))))

(use-package s                    :ensure t  :demand
  :config
  (defalias 'join  's-join)
  (defalias 'split 's-split))

;; ================================================================================

(use-package anzu                 :ensure t
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
  (message "Config anzu...done"))

(use-package bm                   :ensure t
  :hook
  (after-save . bm-buffer-save)
  (kill-emacs . bm-buffer-save-all)
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
  (message "Config bm...done"))

(use-package cua-base             :ensure nil
  :custom
  (cua-rectangle-mark-key [(control meta return)])
  :config
  (cua-mode)
  (message "Config cua...done"))

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
  (mouse-yank-at-point             t)
  (scroll-bar-mode                 'right)
  (sentence-end-double-space       nil)
  (sentence-end-without-period     nil)
  (use-hard-newlines               nil)
  (tab-always-indent               'complete)
  (text-mode-ispell-word-completion nil)
  (which-func-modes                '(emacs-lisp-mode c-mode c++-mode cperl-mode python-mode diff-mode))
  (x-underline-at-descent-line     t)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'perl-mode   'perl-ts-mode)

  (set-language-environment    'utf-8)
  (set-default-coding-systems  'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system        'utf-8)

  (setq max-image-size 1024)

  (random t))

(use-package tjf-keys             :ensure nil)

(use-package tjf-menubar          :ensure nil :demand
  ;; :hook
  :config
  (tjf:menubar/config))

(use-package tjf-tabline          :ensure nil
  :custom
  (tjf:tabline/separator  '(0.0))
  (tjf:tabline/use-images nil)
  :config
  (tjf:tabline/mode 1)
  (message "tjf-tabline config...done"))

(use-package tjf-toolbar          :ensure nil)

(use-package undo-fu              :ensure t)

;; ================================================================================

(use-package clean-aindent-mode   :ensure t
  :custom
  (clean-aindent-is-simple-indent t)
  :config
  (electric-indent-mode -1)
  (clean-aindent-mode    t)
  (message "Config clean-aindent-mode...done"))

(use-package display-line-numbers :ensure nil :commands display-line-numbers-mode
  :hook
  (prog-mode . display-line-numbers-mode)
  (csv-mode  . display-line-numbers-mode)
  (org-mode  . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  :config
  (message "display-line-numbers config...done"))

(use-package git-gutter           :ensure t
  :config
  (global-git-gutter-mode t)
  (message "Config git-gutter...done"))

(use-package indent-bars          :ensure t   :after treesit
  :custom
  (indent-bars-treesit-support nil)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope
   '((c    function_definition for_statement if_statement while_statement)
	 (c++  function_definition class_definition for_statement if_statement while_statement)
	 (perl function_definition class_definition for_statement if_statement while_statement)
	 ))
  :hook
  (paragraph-indent-text-mode . indent-bars-mode)
  (prog-mode                  . indent-bars-mode)
  (text-mode                  . indent-bars-mode)
  :config
  (message "Config indent-bars...done"))

(use-package mic-paren            :ensure t
  :config
  (paren-activate)
  (message "Config mic-paren...done"))

(use-package minions              :ensure t   :after anzu
  :config
  (minions-mode 1)
  (message "Config minions...done"))

(use-package powerline            :ensure t
  :custom
   (powerline-gui-use-vcs-glyph t)
  :config
  (message "powerline config...done"))

(use-package rainbow-delimiters   :ensure t   :commands rainbow-delimiters-mode
  :hook
  (eshell-mode . rainbow-delimiters-mode)
  (latex-mode  . rainbow-delimiters-mode)
  (prog-mode   . rainbow-delimiters-mode)
  (shell-mode  . rainbow-delimiters-mode)
  :config
  (message "Config rainbow-delimiters...done"))

(use-package tjf-powerline        :ensure nil :after powerline
  :hook
  (post-command . tjf:powerline/update-modeline-vars)
  :init
  (require 'tjf-powerline)
  :config
  (alias-face powerline-red-face fontaine/powerline-red)
  (setq powerline-default-separator 'arrow)
  (tjf:powerline/theme)
  (message "tjf-powerline config...done"))

(use-package volatile-highlights  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (message "Config volatile-highlights...done"))

;; ================================= completion =======================================

(use-package cape                 :ensure t   :after eglot
  :config
  (setq completion-at-point-functions
        '(cape-symbol cape-keyword cape-dabbrev cape-file consult-history))
  (message "Config cape...done"))

(use-package clang-capf           :ensure t   :after cc-mode
  :hook
  (c-ts-mode . (lambda ()
                 (setq-local clang-capf-clang "clang-20")
                 (setq-local clang-capf-extra-flags '("-std=c18"))
                 (setq-local completion-at-point-functions (cons #'clang-capf completion-at-point-functions))))
  (c++-ts-mode . (lambda ()
                   (setq-local clang-capf-clang "clang++-20")
                   (setq-local clang-capf-extra-flags '("-std=c++23"))
                   (setq-local completion-at-point-functions (cons #'clang-capf completion-at-point-functions))))
  :config
  (message "clang-capf...done"))

(use-package consult              :ensure t   :demand
  :config
  (message "Config consult...done"))

(use-package corfu                :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  :hook
  (prog-mode . (lambda ()
				 (corfu-mode            1)
				 (corfu-popupinfo-mode -1)
				 (corfu-history-mode    1)))
				 
  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  ;; (defvar corfu-mode--set-explicitly)
  ;; Enable optional extension modes:
  :config
  (message "Config corfu...done"))

(use-package consult-eglot        :ensure t   :after eglot :disabled
  :config
  (message "Config consult-eglot...done"))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/"   . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  (message "Config dabbrev...done"))

;; Configure Eldoc for hover-only (no echo area messages)
(use-package eldoc                :ensure nil
  :custom
  (eldoc-idle-delay             0.2)        ; Time before doc appears
  (eldoc-message-function       #'ignore)   ; Prevent echo area flicker
  (eldoc-documentation-strategy #'ignore)	; Let eldoc-box handle rendering
  :hook
  (emacs-lisp-mode . eldoc-mode)
  :config
  (message "Config eldoc...done"))

(use-package eldoc-box            :ensure t
  :hook 
  (eglot-managed-mode . eldoc-box-hover-mode) ; Enable in LSP buffers
  :config
  (when (require 'markdown-mode nil t)
    (setq eldoc-box-hover-render-function
          (lambda (contents)
            (with-temp-buffer
              (insert contents)
              (markdown-mode)
              (font-lock-ensure)
              (buffer-string)))))
  (message "Config eldoc-box...done"))   

(use-package eglot                :ensure t   :after orderless
  :custom
  (eglot-send-changes-idle-time 0.1)
  :hook
  (find-file          . (lambda () (flymake-mode -1)))
  (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  :config
  (add-to-list 'eglot-server-programs '((perl-ts-mode cperl-mode) . ("pls")))
  (add-to-list 'eglot-server-programs '((c-ts-mode cc-mode) .
                                        ("clangd"
                                         "-j=8"
                                         "--compile-commands-dir=~/ide/cpp"
                                         "--log=error"
                                         "--malloc-trim"
                                         "--background-index"
                                         "--clang-tidy"
                                         "--completion-style=detailed"
                                         "--pch-storage=memory"
                                         "--header-insertion=iwyu"
                                         "--header-insertion-decorators=0")))
  (message "Config eglot...done"))

(use-package kind-icon            :ensure t   :after marginalia
  :if
  (display-graphic-p)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (message "kind-icon...done"))

(use-package orderless            :ensure t   :after corfu
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)  ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  :hook
  (corfu-mode . (lambda ()
				  (setq-local completion-styles '(orderless-literal-only basic)
							  completion-category-overrides nil
							  completion-category-defaults nil)))
  :config
  (orderless-define-completion-style orderless-literal-only
	(orderless-style-dispatchers nil)
	(orderless-matching-styles   '(orderless-literal)))
  (message "Config orderless...done"))
;; (use-package orderless            :ensure t   :after corfu
;;   :custom
;;   (completion-category-defaults  nil)
;;   (completion-category-overrides '((file (styles . (partial-completion)))))
;;   (completion-styles             '(orderless basic))
;;   :config
;;   (orderless-define-completion-style orderless-fast
;;     (orderless-style-dispatchers '(orderless-fast-dispatch))
;;     (orderless-matching-styles   '(orderless-literal orderless-regexp)))
;;   (message "Config orderless...done"))

(use-package marginalia           :ensure t   :after prescient
  :init
  (marginalia-mode 1) ;; because the doc said so
  :config
  (message "Config marginalia...done"))

(use-package prescient            :ensure t   :after vertico
  :custom
  (prescient-history-length           1000)
  (vertico-prescient-enable-filtering t)
  :config
  (prescient-persist-mode +1)
  (corfu-prescient-mode   +1)
  (vertico-prescient-mode +1)
  (message "Config prescient...done"))

(use-package vertico              :ensure t   :after cape
  :config
  (vertico-mode 1)
  (message "Config vertico...done"))

;; ================================= TJF =========================================

(use-package tjf-bookmark         :ensure nil)

(use-package tjf-clipboard        :ensure nil)

(use-package tjf-color            :ensure nil)

(use-package tjf-date             :ensure nil)

(use-package tjf-duplicate        :ensure nil)

(use-package tjf-edit             :ensure nil)

(use-package tjf-file             :ensure nil)

(use-package tjf-flags            :ensure nil)

(use-package tjf-frame            :ensure nil
  :config
  (tjf:frame/config))

(use-package tjf-lisp             :ensure nil
  :init
  (message "tjf-lisp[1] init!!!!!")
  :hook
  (emacs-lisp-mode       . tjf:lisp/hook)
  (lisp-interaction-mode . tjf:lisp/hook)
  :config
  (tjf:lisp/config)
  (message "tjf-lisp config...done"))

(use-package tjf-macro            :ensure nil)

(use-package tjf-mode             :ensure nil)

(use-package tjf-navigate         :ensure nil
  :hook
  (menu-bar-update . tjf:navigate/hook))

(use-package tjf-query-replace    :ensure nil)

(use-package tjf-search           :ensure nil)

(use-package tjf-sort             :ensure nil)

(use-package tjf-tools            :ensure nil)

(use-package tjf-view             :ensure nil)

;; ================================= EXT =========================================

(use-package mapreplace           :ensure nil :commands (query-mapreplace query-mapreplace-regexp))

(use-package xah                  :ensure nil)

;; ================================================================================

(use-package autorevert           :ensure nil
  :diminish autorevert-mode
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t)
  (auto-revert-interval      5)
  (auto-revert-verbose       t)
  :config
  (global-auto-revert-mode)
  (message "Config autorevert...done"))

(use-package cl-macs              :ensure nil
  :config
  (message "Config cl-macs...done"))

(use-package color                :ensure nil
  :config
  (message "Config color...done"))

(use-package delim-col            :ensure nil 
  :commands
  (delimit-columns-region 
   delimit-columns-rectangle)
  :custom
  (delimit-columns-separator     "[ \t]+")
  (delimit-columns-str-separator " "))

(use-package ediff                :ensure nil :commands ediff
  :custom
  (ediff-keep-variants                    nil)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-show-clashes-only                t)
  (ediff-split-window-function            'split-window-horizontally)
  (ediff-window-setup-function            'ediff-setup-windows-plain))

(use-package ffap                 :ensure nil :commands ffap-at-mouse)

(use-package files                :ensure nil
  :custom
  (auto-save-file-name-transforms `((".*"   ,tjf:user/dir-autosave t)))
  (backup-directory-alist         `((".*" . ,tjf:user/dir-backup)))
  (auto-mode-alist
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
     ("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-mode)
     ("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode)
     ("/\\.?\\(?:gitconfig\\|gnokiirc\\|hgrc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
     ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'"                   . conf-mode-maybe)
     ("/\\.\\(?:gtk\\|net\\|nvidia-settings-\\|screen\\|xmp\\)rc\\'"                  . conf-mode)
     ("\\.ini\\'"      . conf-mode)
     ("\\.cs\\'"       . csharp-mode)
     ("\\.css\\'"      . css-mode)
     ("\\.csv\\'"      . csv-mode)
     ("\\.el\\'"       . emacs-lisp-mode)
     ("\\.emacs\\'"    . emacs-lisp-mode)
     ("\\.f9[05]\\'"   . f90-mode)
     ("\\.f0[38]\\'"   . f90-mode)
     ("\\.[fF]\\'"     . fortran-mode)
     ("\\.for\\'"      . fortran-mode)
     ("\\.go\\'"       . go-mode)
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
     ("\\.json\\'"     . json-mode)
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
     ("\\.r\\'"             . rust-mode)
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
     ("\\.ya?ml\\'"     . yaml-mode)))
  :config
  (message "Config files...done"))

(use-package frame                :ensure nil
  :custom
  (frame-title-format  "%b")
  (blink-cursor-blinks 0)
  :config
  (blink-cursor-mode)
  (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
  (on-gui
   (set-background-color "gray95"))
  (message "Config frame...done"))

(use-package hl-line              :ensure nil
  :config
  (global-hl-line-mode)
  (message "Config hl-line...done"))

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
  (setq isearch-regexp-lax-whitespace nil)
  (message "Config isearch...done"))

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

(use-package so-long              :ensure nil
  :config
  (global-so-long-mode 1))

(use-package uniquify             :ensure nil
  :custom
  (uniquify-buffer-name-style   'post-forward)
  (uniquify-ignore-buffers-re   "^\\*")
  (uniquify-strip-common-suffix t))

(use-package vc                   :ensure nil
  :custom
  (vc-follow-symlinks t))

(use-package window               :ensure nil
  :config
  (delete-other-windows))

(use-package winner               :ensure nil
  :config
  (winner-mode 1)
  (message "Config winner...done"))

(use-package xref                 :ensure nil
  :custom
  (xref-show-definitions-function 'xref-show-definitions-completing-read)
  (xref-show-xrefs-function       'xref-show-definitions-completing-read)
  (xref-file-name-display         'project-relative)
  (xref-search-program            'grep))

;; ================================= PKG =========================================

(use-package async                :ensure t
  :config
  (message "Config async...done"))

(use-package blamer               :ensure t
  :custom
 (blamer-avatar-folder      "~/.config/emacs/blamer/avatars/")
 (blamer-smart-background-p nil)
  :config
  (global-blamer-mode -1)
  (message "Config blamer...done"))

(use-package ergoemacs-mode       :ensure t
  :commands
  (ergoemacs-backward-open-bracket
   ergoemacs-extend-selection
   ergoemacs-forward-open-bracket
   ergoemacs-move-text-down
   ergoemacs-move-text-up
   ergoemacs-select-text-in-quote
   ergoemacs-shrink-whitespaces)
  ;; :no-require t
  :config
  (ergoemacs-mode 0)
  (message "Config ergoemacs-mode...done"))

(use-package flycheck             :ensure t   :commands flycheck-mode
  :hook ((prog-mode . flycheck-mode)
         ;; Show diagnostics inline, next to the code (Error Lens style)
         (prog-mode . flycheck-annotate-mode))
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
  ;; Report Eglot's LSP diagnostics through Flycheck
  (flycheck-eglot-mode 1))

(use-package flymake              :ensure nil :commands flymake-mode
  :hook
  (prog-mode   . (lambda () (flymake-mode -1)))
  (c++-ts-mode . (lambda () (flymake-mode -1)))
  (c++-mode    . (lambda () (flymake-mode -1)))
  (cc-mode     . (lambda () (flymake-mode -1)))
  :config
  (message "Somebody loaded flymake!!!"))

;; (use-package flycheck             :ensure t
;;   :preface
;;   (defvar flycheck-mode--set-explicitly nil)
;;   :custom
;;   (flycheck-mode-line
;;    '(:eval
;;      (pcase flycheck-last-status-change
;;        (`not-checked nil)
;;        (`no-checker (propertize " -" 'face 'warning))
;;        (`running    (propertize " ✷" 'face 'success))
;;        (`errored    (propertize " !" 'face 'error))
;;        (`finished
;;         (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
;;                (no-errors    (cdr (assq 'error   error-counts)))
;;                (no-warnings  (cdr (assq 'warning error-counts)))
;;                (face (cond (no-errors   'error)
;;                            (no-warnings 'warning)
;;                            (t           'success))))
;;           (propertize (format " %s/%s" (or no-errors 0) (or no-warnings 0))
;;                       'face face)))
;;        (`interrupted " -")
;;        (`suspicious '(propertize " ?" 'face 'warning)))))
;;   :config
;;   (setq flycheck-emacs-lisp-load-path 'inherit)
;;   (global-flycheck-mode)
;;   (message "Config flycheck...done"))

(use-package helpful              :ensure t
  :commands  (helpful-callable helpful-variable helpful-key)
  :config
  (global-set-key (kbd "C-h   f") 'helpful-callable)
  (global-set-key (kbd "C-h   v") 'helpful-variable)
  (global-set-key (kbd "C-h   k") 'helpful-key)
  (global-set-key (kbd "C-h C-k") 'describe-key)
  (message "Config helpful...done"))

(use-package langtool             :ensure t   :commands langtool-check
  :custom
  (langtool-language-tool-jar (concat tjf:user/dir-home "Documents/LanguageTool-4.1/languagetool-commandline.jar")))

(use-package loccur               :ensure t   :commands loccur-current)

(use-package mapreplace           :ensure nil)

(use-package pos-tip              :ensure t   :after flycheck
  :config
  (message "Config pos-tip...done"))

(use-package powerthesaurus       :ensure t   :commands powerthesaurus-lookup-dwim)

(use-package shift-number         :ensure t   :commands (shift-number-up shift-number-down))

(use-package smartparens          :ensure t
  :diminish smartparens-mode
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
  (sp-local-pair sp-lisp-modes "`" nil :actions nil)
  (message "Config smart-parens...done"))

(use-package sdcv                 :ensure t   :commands sdcv-search)

(use-package treemacs             :ensure t   :commands treemacs
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
  (message "Config treemacs-magit...done"))

(use-package treesit-auto         :ensure t
  :preface
  (defvar treesit-auto-mode--set-explicitly nil)
  :custom
  (treesit-auto-install t)
  :config
  (setq treesit-font-lock-level 4)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package ws-butler            :ensure t
  :diminish ws-butler-mode
  :preface
  (defvar ws-butler-mode--set-explicitly nil)
  :config
  (ws-butler-global-mode)
  (message "Config ws-butler...done"))


;; ================================ MODES =========================================

(use-package anaconda-mode        :ensure t   :after python
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package bazel                :ensure t   :commands bazel-mode
  :hook
  (bazel-mode . (lambda ()
                  (setq-local completion-at-point-functions
                              (cons #'bazel-completion-at-point completion-at-point-functions))))
  :config
  (message "Config bazel...done"))

(use-package bash-completion      :ensure t   :after shell
  :config
  (bash-completion-setup)
  (message "Config bash-completion...done"))

(use-package c-ts-mode            :ensure nil :after cc-mode
  :config
  (message "Config c-ts-mode...done"))

(use-package c++-ts-mode          :ensure nil :after cc-mode
  :config
  (message "Config c++-ts-mode...done"))

(use-package cc-mode              :ensure nil :commands (c-mode c++-mode)
  :config
  (message "Config cc-mode...done"))

(use-package cperl-mode           :ensure nil :commands cperl-mode
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
  (defun cperl-define-key () nil)

  (define-key cperl-mode-map [(control ?h) ?f] nil)
  (define-key cperl-mode-map [(control ?h) ?v] nil)

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
  (message "Config cperl-mode...done"))

(use-package cpp-auto-include     :ensure t   :after tjf-cpp
  :config
  (cpp-auto-include)
  (message "Config cpp-auto-include...done"))

(use-package csharp-mode          :ensure nil :commands csharp-mode
  :config
  (message "Config csharp-mode...done"))

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
  (csv-mode . csv-highlight)
  :config
  (message "Config csv-mode...done"))

(use-package executable           :ensure nil :commands shell-script-mode
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package face-remap           :ensure nil :commands (buffer-face-mode text-scale-mode)
  :diminish face-remap-mode
  buffer-face-mode)

(use-package hideshow             :ensure nil :commands hs-minor-mode
  :diminish hs-minor-mode
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package jinx                 :ensure t   :commands jinx-mode
  :hook
  (markdown-mode              . jinx-mode)
  (paragraph-indent-text-mode . jinx-mode)
  ;; (text-mode                  . jinx-mode)
)

(use-package json-ts-mode         :ensure nil :commands json-ts-mode)

(use-package make-mode            :ensure nil :commands makefile-gmake-mode
  :hook
  (makefile-gmake-mode . (lambda ()
                           (setq-local completion-at-point-functions
                                       (cons #'makefile-completions-at-point completion-at-point-functions)))))

(use-package magit                :ensure t   :commands magit-status)

(use-package markdown-mode        :ensure nil :commands markdown-mode
  :hook
  (markdown-mode . (lambda ()
                     (setq-local completion-at-point-functions (cons #'markdown-complete-at-point completion-at-point-functions)))))

(use-package markdown-ts-mode     :ensure t   :commands markdown-ts-mode)

(use-package modern-cpp-font-lock :ensure t   :after cc-mode :disabled
  :diminish modern-c++-font-lock-mode
  :hook
  (c++-ts-mode . modern-c++-font-lock-mode)
  (c++-mode    . modern-c++-font-lock-mode)
  :config
  (message "Config modern-cpp-font-lock...done"))

(use-package modern-sh            :ensure t
  :commands modern-sh-mode
  :hook
  (sh-mode . modern-sh-mode)
  :config
  (message "Config modern-sh...done"))

(use-package org                  :ensure nil :commands org-mode
  :hook
  (org-mode . visual-line-mode))

(use-package paradox              :ensure t   :commands paradox-list-packages)

(use-package perl-ts-mode         :ensure t   :commands perl-ts-mode)

(use-package python               :ensure nil :commands python-mode :disabled
 :custom
  (python-indent-guess-indent-offset-verbose nil)
  :hook
  (python-mode . (lambda ()
                   (setq-local completion-at-point-functions (cons #'python-completion-at-point completion-at-point-functions)))))

(use-package python-ts-mode       :ensure nil :commands python-ts-mode)

(use-package rainbow-mode         :ensure nil :commands rainbow-mode)

(use-package tetris               :ensure nil :commands tetris
  :custom
  (tetris-score-file "/dev/null"))

(use-package tex-mode             :ensure nil :commands tex-mode
  :config
  (define-key latex-mode-map [(control return)] 'tjf:edit/insert-newline-after))

(use-package text-mode            :ensure nil :commands text-mode
  :hook
  (text-mode . turn-on-auto-fill))

(use-package textsize             :ensure t   :commands textsize-mode)

(use-package tjf-c                :ensure nil :after tjf-cc
  :hook
  (c-ts-mode . tjf:c/hook)
  :config
  (tjf:c/config))

(use-package tjf-cc               :ensure nil :after c-ts-mode)

(use-package tjf-clips            :ensure nil :after clips-mode :disabled
  :hook
  (clips-mode-hook . tjf:clips/setup))

(use-package tjf-cpp              :ensure nil :after tjf-cc
  :hook
  (c++-ts-mode . tjf:cpp/hook)
  :config
  (tjf:cpp/config))

(use-package tjf-csharp           :ensure nil :after csharp-mode
  :hook
  (csharp-mode 'tjf:csharp/setup))

(use-package tjf-perl             :ensure nil :after perl-ts-mode
  :hook
  (cperl-mode   . tjf:perl/hook)
  (perl-ts-mode . tjf:perl/hook)
  :config
  (tjf:perl/config)
  ;; (add-to-list 'eglot-server-programs '(cperl-mode . ("perl" "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run")))
)

(use-package tjf-python           :ensure nil :after python
  :hook
  (python-mode . tjf:python/hook)
  :config
  (tjf:python/config))

(use-package whitespace           :ensure nil :commands whitespace-mode
  :diminish whitespace-mode
  :custom
  (whitespace-display-mappings
   '((space-mark 32 [183] [46])                       ; 32 SPACE     => "·" "."
     (newline-mark 10 [182 10])                       ; 10 LINE FEED => "¶ <LINE FEED>"
     (tab-mark 9 [9654 32 91 84 65 66 93 9] [92 9]))) ;  9 TAB       => "▶ [TAB]<TAB>"
  (whitespace-style (quote (tabs spaces space-before-tab newline indentation empty
                                 space-after-tab space-mark tab-mark newline-mark))))

(use-package yaml-mode            :ensure t   :commands yaml-mode)

;;
(message "Configuring from default.el ...done")
(emacs-init-time)

;;; default.el ends here
