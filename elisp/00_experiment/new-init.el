;;; default.el --- Global initialization -*-lexical-binding: t-*- ;; --*-no-byte-compile: t ;; *-Emacs-Lisp-*-

(defvar ligature-def '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                       "\\\\" "://"))

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

(message "Configuring from default.el...")

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

(use-package diminish             :ensure t   :defer t
  :config
  (message "Loading diminish...done"))

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

(use-package tjf-macro            :ensure nil)

;; ================================= completion =======================================

(use-package cape                 :ensure t   :after consult-eglot
  :config
  (setq completion-at-point-functions
        '(cape-symbol cape-keyword cape-dabbrev cape-file consult-history))
  (message "Loading cape...done"))

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
  (message "Loading consult...done"))

(use-package consult-eglot        :ensure t   :after eglot
  :config
  (message "Loading consult-eglot...done"))

(use-package corfu-popupinfo      :ensure t   :disabled
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-prescient      :ensure t   :after vertico-prescient
  :config
  (message "Loading corfu-prescient...done"))

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

(use-package kind-icon            :ensure t   :after marginalia
  :if
  (display-graphic-p)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (message "kind-icon...done"))

(use-package marginalia           :ensure t   :after corfu-prescient
  :config
  (marginalia-mode 1)
  (message "Loading marginalia...done"))

(use-package orderless            :ensure t   :after consult
  :custom
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-styles             '(orderless basic))
  :config
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles   '(orderless-literal orderless-regexp)))
  (message "Loading orderless...done"))

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

;; ================================== UI =========================================

(use-package anzu                 :ensure t   :after powerline
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

(use-package powerline            :ensure t
  :custom
   (powerline-gui-use-vcs-glyph t)
  :config
  (message "Loading powerline...done"))

(use-package tjf-powerline        :ensure nil
  :hook
  (post-command . tjf:powerline/update-modeline-vars)
  :config
  (alias-face powerline-red-face fontaine/powerline-red)
  (setq powerline-default-separator 'arrow)
  (tjf:powerline/theme))

(use-package tjf-menubar          :ensure nil
  :hook
  (menu-bar-update . tjf:navigate/menu))

(use-package tjf-toolbar          :ensure nil)

(use-package tjf-tabline          :ensure nil
  :custom
  (tjf:tabline/separator  '(0.0))
  (tjf:tabline/use-images nil)
  :config
  (setq tjf:tabline/tab-label-function #'tjf:tabline/label-function)
  (tjf:tabline/mode 1))

(use-package tjf-keys             :ensure nil)

(use-package tjf-frame            :ensure nil
  :config
  (tjf:frame/reset-size))

