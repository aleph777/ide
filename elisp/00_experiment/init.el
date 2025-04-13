;;; default.el --- Global initialization -*- lexical-binding: t; -*- ;; -*-no-byte-compile: t; -*- ;; -*-Emacs-Lisp-*-

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

;;
(enable-theme 'fontaine)

(add-to-list 'package-archives '("melpa"   . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("non-gnu" . "https://elpa.nongnu.org/nongnu/") t)

(setq package-archive-priorities '(("gnu" . 10)("non-gnu" . 20)("melpa" . 30)))

;; ================================================================================

(use-package diminish             :ensure t
  :functions diminish
  :init   (message "Loading diminish...")
  :config (message "Loading diminish...done"))

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

(use-package emacs                :ensure nil
  :config
  (mapc
   (lambda (command)
     (put command 'disabled nil))
   '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))
  (mapc
   (lambda (command)
     (put command 'disabled t))
   '(overwrite-mode))

  (setq current-language-environment "UTF-8")
  (setq locale-coding-system   'utf-8)

  (set-language-environment    'utf-8)
  (set-default-coding-systems  'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system        'utf-8)

  (setopt sentence-end-double-space nil)

  ;; (setq default-frame-alist '((undecorated . t))) ; TODO - provide minimize/maximize functionality to menu & toolbar before enabling this
  (setq auto-window-vscroll             nil)
  (setq fast-but-imprecise-scrolling    t)

  (setq-default scroll-error-top-bottom         t)
  (setq-default scroll-preserve-screen-position t)

  (setq-default case-fold-search                t)
  (setq-default create-lockfiles                nil)
  (setq-default cursor-type                     '(bar . 2))
  (setq-default font-lock-maximum-decoration    t)
  (setq-default frame-title-format              "%b")
  (setq-default indent-tabs-mode                nil)
  (setq-default line-spacing                    0)
  (setq-default tab-always-indent               'complete)

  (setq auto-save-file-name-transforms      `((".*"   ,tjf:user/dir-autosave t)))
  (setq backup-directory-alist              `((".*" . ,tjf:user/dir-backup)))
  (setq blink-cursor-blinks                 0)
  (setq buffers-menu-max-size               nil)
  (setq byte-compile-warnings               '(not free-vars obsolete unresolved noruntime lexical make-local))
  (setq colon-double-space                  nil)
  (setq comint-input-ignoredups             t)
  (setq comint-input-ring-size              64)
  (setq completion-cycle-threshold          3)
  (setq disabled-command-function           nil)
  (setq echo-keystrokes                     0.25)
  (setq explicit-shell-file-name            "/bin/bash")
  (setq fill-column                         8192)
  (setq frame-resize-pixelwise              t)
  (setq gnutls-min-prime-bits               80)
  (setq imenu-sort-function                 'imenu--sort-by-name)
  (setq indent-tabs-mode                    nil)
  (setq inhibit-startup-echo-area-message   nil)
  (setq initial-scratch-message             nil)
  (setq kill-do-not-save-duplicates         t)
  (setq max-image-size                      1024)
  (setq mode-require-final-newline          'visit-save)
  (setq mouse-drag-copy-region              t)
  (setq mouse-yank-at-point                 t)
  ;; (setq mouse-wheel-scroll-amount           '(3 ((shift) . 1) ((control))))
  (setq mouse-wheel-scroll-amount           '(1 ((shift) . 5) ((meta)) ((control))))
  (setq recenter-positions                  '(top middle bottom))
  (setq ring-bell-function                  '(lambda () (let ((visible-bell t)))))
  (setq save-interprogram-paste-before-kill t)
  (setq scroll-bar-mode                     'right)
  (setq sentence-end-double-space           nil)
  (setq sentence-end-without-period         nil)
  (setq use-hard-newlines                   nil)
  (setq warning-suppress-log-types '((comp) (bytecomp)))
  (setq which-func-modes                    '(emacs-lisp-mode c-mode c++-mode cperl-mode python-mode diff-mode))
  (setq x-underline-at-descent-line         t)

  (defalias 'yes-or-no-p 'y-or-n-p)

  (put 'downcase-region  'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'overwrite-mode   'disabled t)
  (put 'upcase-region    'disabled nil)

  (random t))
