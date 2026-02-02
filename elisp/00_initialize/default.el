;;; default.el --- Global initialization -*-lexical-binding: t-*- ;; --*-no-byte-compile: t ;; *-Emacs-Lisp-*-

;;         Copyright © 2000-2025 Tom Fontaine

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

;; Revision: 01-Mar-2001 revised to support Emacs and xemacs
;;           05-Apr-2001 changed autoload for new version of ‘tinysearch’
;;           29-Nov-2001 changed ‘message-log-max’ from 50 to 512
;;           03-Dec-2001 added ‘usr-toolbar’
;;           06-Dec-2001 removed ‘ishl’ (now included as ‘isearch-lazy-highlight’) and fixed ‘isearch’ faces
;;                       removed ‘show-paren-mode’ that conflicted with ‘mic-paren’
;;           04-Feb-2005 added comint hook for ‘usr-comint-setup’
;;                       added ‘eval-after-load’ for ‘comint’ (usr-comint)
;;           21-Mar-2007 reworked mouse set up
;;           09-Sep-2009 added ‘develock’
;;           02-Feb-2010 removed ‘develock’
;;           11-Feb-2011 removed ‘redo’ - added ‘redo+’
;;                       added ‘recentf-mode’
;;                       updated default setting for Emacs 22 changes (many obsolete settings removed)
;;           30-Aug-2012 removed ‘c-mode-hook’ and ‘c++-mode-hook’ (usr-cc-setup)
;;                       added ".cpp" to ‘auto-mode-alist’
;;           26-Dec-2012 added ‘eshell-mode-hook’
;;           13-Aug-2013 added ‘csharp-mode’ support
;;           26-Sep-2013 moved mouse button definitions to ‘keys.el’
;;           21-Oct-2013 added ‘csharp-mode’ to ‘msb-menu-cond’
;;           19-Feb-2014 added ‘matlab-mode’ support
;;                       added ‘matlab-mode’ to ‘msb-menu-cond’
;;                       added loading messages for elisp/ext files
;;                       added xz support to ‘jka-compr-compression-info-list’
;;           03-Mar-2014 added ‘clips-mode’ support
;;                       added ‘clips-mode’ to ‘msb-menu-cond’
;;           09-Apr-2014 added .proto to c++ ‘auto-mode-alist’ entry
;;           02-May-2014 added python to ‘msb-menu-cond’, sorted mode specific buffers
;;           06-May-2014 added ‘log-mode’ support
;;                       added ‘log-mode’ to ‘msb-menu-cond’
;;           09-May-2014 added ‘nxml-mode’ to ‘msb-menu-cond’
;;           23-May-2014 added ‘cua-mode’ minor mode
;;                       removed .pl, .pm and sh-mode entries from ‘auto-mode-alist’
;;                       added ‘undo-tree’
;;           24-May-2014 added ‘rainbow-delimiters’
;;           25-May-2014 added ‘uniquify’
;;           27-May-2014 removed ‘scroll-in-place’ in favor of setting ‘scroll-preserve-screen-position’
;;                       added ‘electric-pair-mode’
;;                       added ‘scroll-error-top-bottom’
;;                       added ‘smooth-scrolling’
;;           30-May-2014 changed ‘rainbow-delimiters’ to use specific mode hooks
;;           13-Jun-2014 changed to ‘usr-clips-mode’
;;           07-Jul-2014 removed ‘rainbow-delimiters’ from cperl-mode-hook
;;                       added .debug as a suffix for log-mode in ‘auto-mode-alist’
;;           31-Jul-2014 added .proto as a suffix for c++-mode in ‘auto-mode-alist’
;;           01-Aug-2014 added ‘before-save-hook’ to untabify buffer
;;           17-Sep-2014 added .xaml to nxml ‘auto-mode-alist’ entry
;;                       added ‘eval-after-load’ for ‘flymake’ to ‘csharp-mode’
;;           13-Nov-2014 added ‘atim-unscroll’
;;           26-Mar-2015 added java entry to ‘msb-menu-cond’
;;           27-Mar-2015 now using aliases in place of ‘usr-cc-setup’
;;           01-Apr-2015 set ‘left-fringe’ and ‘right-fringe’ to 1 in ‘after-make-frame-functions’
;;           02-Apr-2015 cleaned up loading messages
;;           10-Apr-2015 added ruby support
;;                       added ‘folding-mode’ support
;;           17-Apr-2015 added ‘haskell-mode’ but haskell support is incomplete
;;           22-Apr-2015 updated ‘after-make-frame-functions’
;;           12-May-2015 changed ‘eval-after-load’ for ‘csharp-mode’ to require ‘usr-csharp’
;;           01-Jul-2015 added ‘js-mode’ to ‘msb-menu-cond’
;;           18-Aug-2015 added ‘tabbar’ support
;;           12-Jan-2016 added ‘eval-after-load’ for ‘smart-compile’
;;           13-Jan-2016 added ‘eval-after-load’ for ‘whitespace’
;;                       added ‘eval-after-load’ for ‘hideshow’
;;           03-Feb-2016 reorganized variables - used available customizations
;;                       fixed errors in ‘msb-menu-cond’
;;                       added ".tpp" to c++ ‘auto-mode-alist’
;;                       added ‘isearch-mode-map’ keys
;;                       removed ‘ffap-file-finder setting’
;;           08-Feb-2016 reworked auto-mode code
;;                       added ‘php-mode’
;;           22-Feb-2016 fixed error with sh-script autoload/add-hook wrt ‘rainbow-delimiters’
;;           25-Feb-2016 used ‘u-require’
;;           28-Feb-2016 fixed ‘msb-menu-cond’
;;           29-Feb-2016 changed from ‘usr-’ to ‘u-’
;;           01-Mar-2016 removed face proprty settings in favor of customization set in .emacs
;;           02-Mar-2016 added autoloads for ‘buffer-face-mode’, ‘text-scale-mode’, and ‘ergoemacs-select-text-in-quote’
;;           03-Mar-2016 added html modes to ‘msb-menu-cond’
;;                       added more javascript modes to ‘msb-menu-cond’
;;                       removed ‘filladapt’
;;           30-Mar-2016 removed ‘u-mode-line’ in favor of u-powerline'
;;           08-Apr-2016 major revision for ‘use-package’
;;           14-Apr-2016 added "(fset 'yes-or-no-p #'y-or-n-p)"
;;           15-Apr-2016 fixed package usage
;;           19-Apr-2016 added ‘flycheck’
;;                       added ‘smartparens’
;;           26-Apr-2016 added ‘shift-number’
;;           27-Apr-2016 added ‘flycheck-pos-tip’
;;           28-Apr-2016 added ‘jedi’ (python autocomplete)
;;           01-May-2016 added ‘paradox’ support
;;           22-Jun-2016 removed ‘lib-complete’
;;                       using ‘global-hl-line-mode’
;;                       added ‘u-msb’
;;           23-Jun-2016 converted to rigorous usage of ‘use-package’
;;           14-Aug-2016 added ‘ac-lang’
;;           12-Sep-2016 removed ‘hideshow’
;;                       added ‘origami’
;;           14-Sep-2016 added ‘async’
;;           15-Sep-2016 added ‘ac-dabbrev’
;;           27-Sep-2016 added org-mode hook for ‘visual-line-mode’
;;           16-Nov-2016 added ‘cperl-mode’ to ‘autopair’
;;           05-Dec-2016 added ‘linum-relative’
;;           12-Jan-2017 added ‘plsense-server-start’ to ‘cperl’ config
;;                       added ‘winner-mode’
;;           13-Jan-2017 added ‘fancy-narrow’ and ‘loccur’
;;                       added ‘bm’
;;           14-Jan-2017 added check for ‘window-system’
;;                       added ‘global-auto-revert-mode’
;;           16-Jan-2017 added ‘langtool’
;;                       removed ‘tinysearch-*’
;;           17-Jan-2017 added ‘web-mode’
;;           19-Jan-2017 disabled ‘fancy-narrow’ and ‘origami’
;;                       added ‘hideshow’
;;                       added ‘flycheck-emacs-lisp-load-path’ to flycheck
;;           21-Jan-2017 added ‘ssh’
;;           29-Jun-2017 added ‘ycmd’, ‘company-ycmd’, and ‘flycheck-ycmd’
;;           19-Sep-2017 added ‘sdcv’
;;           09-Jan-2018 removed ‘timestamp’
;;           09-Feb-2018 added ‘treemacs’
;;           16-Mar-2018 removed ‘jedi’
;;                       fixed ‘cperl-mode’/‘smartparens’ conflict
;;           02-Apr-2018 disabled ‘nlinum’ and ‘nlinum-relative’
;;                       added ‘display-line-numbers’
;;           12-Apr-2018 added ‘helpful’
;;           17-Apr-2018 changed ‘lisp-mode’ to ‘elisp-mode’
;;           13-Jun-2018 updated for performance
;;           20-Jun-2018 changed some minor-modes to use :hook
;;           16-Jan-2019 added ‘clang-format’
;;           24-Jan-2019 changed from ‘ycmd’ to ‘eglot’ (still need omnisharp server for c#?)
;;           30-Apr-2019 added ‘treemacs-magit’
;;           18-Jun-2019 added ‘rtags’, ‘company-rtags’, and ‘flycheck-rtags’
;;           24-Jun-2019 added ‘groovy’
;;           10-Jul-2019 fixed compiler warnings
;;                       added ‘clean-aindent’
;;           13-Jul-2019 added ‘centaur-tabs’
;;           20-Jul-2019 added conditional for Emacs 27
;;                       added ‘filladapt’
;;           09-Aug-2019 added ‘git-gutter’, ‘git-gutter-fringe’, ‘fringe-helper’, and ‘ws-butler’
;;           28-Aug-2019 added ‘u-cpp’
;;           03-Sep-2019 added ‘minions’
;;           05-Oct-2020 added ‘rainbow-mode’ and ‘display-line-numbers’
;;           28-Oct-2020 fixed ‘tabbar' and ‘powerline' initialization relationship
;;           10-Nov-2020 added ‘emojify' and ‘unicode-fonts'
;;           15-Dec-2020 moved defconts to .emacs
;;                       removed defer constants (use let variables)
;;           24-Jan-2021 added ‘dash'
;;           03-Feb-2021 ‘tjf’ overhaul
;;           11-Mar-2021 removed ‘tinysearch’ and ‘tinyeat’
;;           24-Mar-2021 added ‘orderless’, ‘selectrum’, and ‘selectrum-prescient’
;;                       fixed loading sequence error
;;           08-May-2021 using ‘straight’
;;           01-Jul-2021 added ‘-no-byte-compile’
;;                       added ‘julia-mode’
;;                       redid undo/redo enable for Edit menu
;;           07-Mar-2022 fixed ‘tjf-python’
;;                       removed ‘package-archives’ redundancies
;;           17-Apr-2022 fixed python mode
;;           26-Apr-2022 removed ‘unicode-fonts-setup’
;;           18-May-2022 removed dead code and reorganized
;;           12-Sep-2022 added ‘bazel’
;;           13-Sep-2022 added ‘cape’ and ‘corfu’
;;                       removed ‘company’
;;           24-Sep-2022 added ‘symbol-to-string’ and ‘string-to-symbol’
;;           15-Nov-2022 added ‘cmake-mode’
;;           04-Jan-2023 fixed ‘tjf-c’ and ‘tjf-cpp’ usage
;;           10-Jan-2023 added ‘csv-mode’
;;           05-Apr-2023 changed from ‘straight’ to ‘elpaca’
;;           06-Apr-2023 simplfied ‘auto-mode-alist’
;;           10-Apr-2023 use built-in ‘eglot’
;;                       separated completion setup
;;                       changed from ‘selectrum’ to ‘vertico’
;;           18-Apr-2023 added ‘csv-highlight’
;;           24-May-2023 added ‘textsize’
;;           02-Jan-2024 fixed cperl problem with ‘eglot-server’
;;           01-Jun-2024 added ‘jinx-mode’
;;           06-Jun-2024 disabled ‘corfu’ while in ‘shell-mode’
;;           05-Dec-2024 elpaca update
;;                       added ‘indent-bars’
;;                       added ‘bash-completion’
;;                       added ‘ctrlf’
;;                       added ‘modern-sh’
;;                       added ‘noccur’
;;           09-Jan-2025 added ‘ultra-scroll’
;;           04-Mar-2025 stopped using ‘elpaca’ out of frustration
;;                       removed ‘ultra-scroll’
;;

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

  (setq package-archive-priorities '(("gnu"          . 10)
									 ("non-gnu"      . 20)
									 ("melpa-stable" . 30)
									 ("melpa"        . 40)))
  (setq tjf:default/package-initialize t))

;; ================================================================================

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

;; ================================= BASE =========================================

(use-package tjf-macro            :ensure nil)

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

(use-package display-line-numbers :ensure nil :commands display-line-numbers-mode
  :hook
  (prog-mode . display-line-numbers-mode)
  (csv-mode  . display-line-numbers-mode)
  (org-mode  . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  :config
  (message "display-line-numbers config...done"))

(use-package eldoc                :ensure nil
  :diminish eldoc-mode
  :hook
  (emacs-lisp-mode . eldoc-mode)
  :config
  (message "eldoc config...done"))

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
          ("\\.ya?ml\\'"      . yaml-mode)))
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

(use-package treesit              :ensure nil
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

  (setq treesit-font-lock-level 4)
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
  (message "Config treesit...done"))

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

;; ================================================================================

(use-package diminish             :ensure t
  :config
  (message "Config diminish...done"))

(use-package f                    :ensure t
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

(use-package s                    :ensure t)

;; ================================= completion =======================================

(use-package cape                 :ensure t   :after consult-eglot
  :config
  (setq completion-at-point-functions
        '(cape-symbol cape-keyword cape-dabbrev cape-file consult-history))
  (message "Config cape...done"))

(use-package clang-capf           :ensure t   :after (cape cc-mode)
  :hook
  (c++-ts-mode . (lambda ()
                   (setq-local clang-capf-clang "g++")
                   (setq-local clang-capf-extra-flags '("-std=c++20"))
                   (setq-local completion-at-point-functions (cons #'clang-capf completion-at-point-functions))))
  :config
  (message "clang-capf...done"))

(use-package consult              :ensure t   :after minibuffer
  :config
  (message "Config consult...done"))

(use-package consult-eglot        :ensure t   :after eglot
  :config
  (message "Config consult-eglot...done"))

(use-package corfu-popupinfo      :ensure t   :disabled
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-prescient      :ensure t   :after vertico-prescient
  :config
  (message "Config corfu-prescient...done"))

(use-package eglot                :ensure t   :after orderless
  :custom
  (eglot-send-changes-idle-time 0.1)
  :config
  (add-to-list 'eglot-server-programs '((c-ts-mode cc-mode) .
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
  (message "Config eglot...done"))

(use-package kind-icon            :ensure t   :after marginalia
  :if
  (display-graphic-p)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (message "kind-icon...done"))

(use-package marginalia           :ensure t   :after corfu-prescient
  :config
  ;; (marginalia-mode 1)
  (message "Config marginalia...done"))

(use-package orderless            :ensure t   :after consult
  :custom
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-styles             '(orderless basic))
  :config
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles   '(orderless-literal orderless-regexp)))
  (message "Config orderless...done"))

(use-package vertico              :ensure t   :after cape
  :config
  (vertico-mode +1)
  (message "Config vertico...done"))

(use-package vertico-prescient    :ensure t   :after vertico
  :custom
  (prescient-history-length           1000)
  (vertico-prescient-enable-filtering t)
  :config
  (prescient-persist-mode +1)
  (vertico-prescient-mode +1)
  (message "Config vertico-prescient...done"))

;; ================================= TJF =========================================

(use-package tjf-mode             :ensure nil)

(use-package tjf-flags            :ensure nil)

(use-package bm                   :ensure t
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
  (message "Config bm...done"))

(use-package tjf-bookmark         :ensure nil)

(use-package tjf-color            :ensure nil)

(use-package tjf-clipboard        :ensure nil)

(use-package tjf-date             :ensure nil)

(use-package tjf-tools            :ensure nil)

(use-package tjf-duplicate        :ensure nil)

(use-package tjf-frame            :ensure nil
  :config
  (tjf:frame/reset-size))

(use-package tjf-file             :ensure nil)

(use-package msb                  :ensure nil
  :custom
  (msb-display-invisible-buffers-p t)
  (msb-max-menu-items              nil)
  :config
 (message "Config msb...done") )

(use-package tjf-msb              :ensure nil
  :config
  (msb-mode))

(use-package tjf-navigate         :ensure nil)

(use-package tjf-query-replace    :ensure nil)

(use-package tjf-search           :ensure nil)

(use-package tjf-sort             :ensure nil)

(use-package tjf-view             :ensure nil)

(use-package undo-fu              :ensure t)

(use-package xah                  :ensure nil)

(use-package tjf-edit             :ensure nil)

(use-package tjf-keys             :ensure nil)

(use-package tjf-lisp             :ensure nil
  :hook
  (emacs-lisp-mode       . tjf:lisp/hook)
  (lisp-interaction-mode . tjf:lisp/hook)
  :config
  (tjf:lisp/config)
  (message "tjf-lisp config...done"))
(require 'tjf-lisp)

(use-package tjf-menubar          :ensure nil
  :hook
  (menu-bar-update . tjf:navigate/menu))
(require 'tjf-menubar)                  

(use-package powerline            :ensure t
  :custom
   (powerline-gui-use-vcs-glyph t)
  :config
  (message "powerline config...done"))

(use-package tjf-powerline        :ensure nil
  :hook
  (post-command . tjf:powerline/update-modeline-vars)
  :config
  (alias-face powerline-red-face fontaine/powerline-red)
  (setq powerline-default-separator 'arrow)
  (tjf:powerline/theme)
  (message "tjf-powerline config...done"))
(require 'tjf-powerline)

(use-package tjf-tabline          :ensure nil
  :custom
  (tjf:tabline/separator  '(0.0))
  (tjf:tabline/use-images nil)
  :config
  ;; (setq tjf:tabline/tab-label-function #'tjf:tabline/label-function)
  (tjf:tabline/mode 1)
  (message "tjf-tabline config...done"))

(use-package tjf-toolbar          :ensure nil)

;; ================================== UI =========================================

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

(use-package clean-aindent-mode   :ensure t
  :custom
  (clean-aindent-is-simple-indent t)
  :config
  (electric-indent-mode -1)
  (message "Config clean-aindent-mode...done"))

(use-package cua-base             :ensure nil
  :config
  (cua-mode)
  (message "Config cua...done"))

(use-package git-gutter           :ensure t
  :config
  (global-git-gutter-mode t)
  (message "Config git-gutter...done"))

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
  (message "Config indent-bars...done"))

(use-package mic-paren            :ensure t
  :config
  (paren-activate)
  (message "Config mic-paren...done"))

(use-package rainbow-delimiters   :ensure t   :commands rainbow-delimiters-mode
  :hook
  (eshell-mode . rainbow-delimiters-mode)
  (latex-mode  . rainbow-delimiters-mode)
  (prog-mode   . rainbow-delimiters-mode)
  (shell-mode  . rainbow-delimiters-mode)
  :config
  (message "Config rainbow-delimiters...done"))

(use-package smooth-scrolling     :ensure t
  :config
  (message "Config smooth-scrolling...done"))

(use-package volatile-highlights  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (message "Config volatile-highlights...done"))

;; ================================ STUFF =========================================

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

(use-package ffap                 :ensure nil :commands ffap-at-mouse)

(use-package flycheck             :ensure t
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
  (message "Config flycheck...done"))

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

(use-package minions              :ensure t   :after anzu
  :config
  (minions-mode 1)
  (message "Config minions...done"))

(use-package pos-tip              :ensure t   :after flycheck
  :config
  (message "Config pos-tip...done"))

(use-package powerthesaurus       :ensure t   :commands powerthesaurus-lookup-dwim)

(use-package pretty-column        :ensure nil :commands (pretty-column pretty-rectangle)
  :config
  (setq pcol-column-separator "[ \t]+")
  (setq pcol-str-separator    " "))

(use-package shift-number         :ensure t   :commands (shift-number-up shift-number-down))

(use-package sdcv                 :ensure nil :commands sdcv-search)

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

(use-package ws-butler            :ensure t
  :diminish ws-butler-mode
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

(use-package c-ts-mode            :ensure nil
  :config
  (message "Config c-ts-mode...done"))

(use-package c++-ts-mode            :ensure nil
  :config
  (message "Config c++-ts-mode...done"))

(use-package cc-mode              :ensure nil :commands (c-mode c++-mode)
  :config
  (message "!!!!!!! HOLA !!!!!!"))

(use-package comint               :ensure nil :commands (shell-mode eshell-mode tjf:tools/open-new-shell)
  :custom
  (comint-input-ignoredups t)
  (comint-input-ring-size  64)
  :hook
  (comint-mode . (lambda ()
                   (setq-local completion-at-point-functions (cons #'comint-completion-at-point completion-at-point-functions))
                   (corfu-mode -1))))

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

(use-package ediff                :ensure nil :commands ediff
  :custom
  (ediff-keep-variants                    nil)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-show-clashes-only                t)
  (ediff-split-window-function            'split-window-horizontally)
  (ediff-window-setup-function            'ediff-setup-windows-plain))

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
  (message "Config ergoemacs-functions...done"))

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
  (text-mode                  . jinx-mode))

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

(use-package modern-cpp-font-lock :ensure t   :after cc-mode
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

(use-package python               :ensure nil :commands python-mode
 :custom
  (python-indent-guess-indent-offset-verbose nil)
  :hook
  (python-mode . (lambda () 
                   (setq-local completion-at-point-functions (cons #'python-completion-at-point completion-at-point-functions))))
  (python-ts-mode . (lambda () 
                      (setq-local completion-at-point-functions (cons #'python-completion-at-point completion-at-point-functions)))))

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

(use-package tjf-perl             :ensure nil :after cperl-mode :commands tjf:perl/convert
  :hook
  (cperl-mode . tjf:perl/hook)
  :config
  (tjf:perl/config)
  (add-to-list 'eglot-server-programs '(cperl-mode . ("perl" "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run"))))

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

;; ================================================================================

;; (use-package ctrlf                :ensure t   :disabled
;;   :config
;;   (ctrlf-mode +1)
;;   (message "Config ctrlf...done"))

;; (use-package dash                 :ensure t
;;   :config
;;   (message "Config dash...done"))

;; (use-package emojify              :ensure t   :commands emojify-mode
;;   :config
;;   (message "Config emojify...done"))

;; (use-package ergoemacs-mode       :ensure t   :disabled
;;   :config
;;   (message "Config ergoemacs-mode...done"))

;; (use-package flycheck-pos-tip     :ensure t   :after (flycheck pos-tip) :disabled
;;   :config
;;   (flycheck-pos-tip-mode)
;;   (message "Config flycheck-pos-tip...done"))

;; (use-package treemacs-projectile  :ensure t   :after (treemacs projectile) :disabled)

;; ;; Commands for using ts-fold.
;; ;; Commands                 Description
;; ;; ts-fold-close            fold the current syntax node.
;; ;; ts-fold-open             open the outermost fold of the current syntax node. Keep the sub-folds close.
;; ;; ts-fold-open-recursively open all folds inside the current syntax node.
;; ;; ts-fold-close-all        close all foldable syntax nodes in the current buffer.
;; ;; ts-fold-open-all         open all folded syntax nodes in the current buffer.
;; ;; ts-fold-toggle           toggle the syntax node at `point'.
;; ;;
;; (use-package treesit-fold         :ensure t   :after treesit :disabled
;;   :config
;;   (global-treesit-fold-indicators-mode)
;;   (message "Config treesit-fold...done"))

;; (use-package unicode-fonts        :ensure t   :disabled
;;   :init
;;   (defun tjf:unicode/emoji-fonts ()
;;     (set-fontset-font t1'symbol "Noto Color Emoji")
;;     (set-fontset-font t 'symbol "Symbola" nil 'append))
;;   :config
;;   (tjf:unicode/emoji-fonts)
;;   (message "Config unicode-fonts...done"))

;; ;; (use-package ultra-scroll         :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
;; (use-package ultra-scroll         :ensure t :disabled
;;   :init
;;   (setq scroll-conservatively 101 ; important!
;;     scroll-margin 0)
;;   :config
;;   (ultra-scroll-mode 1))

;; ================================================================================

;;
(message "Configuring from default.el ...done")

(emacs-init-time)

;;; default.el ends here
