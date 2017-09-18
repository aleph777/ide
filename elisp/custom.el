;;; custom.el --- customization file -*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(anzu-mode-lighter " ")
 '(anzu-mode-line-update-function (quote anzu--update-mode-line-local))
 '(buffers-menu-max-size nil)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 64)
 '(custom-enabled-themes (quote (fontaine)))
 '(custom-safe-themes t)
 '(explicit-shell-file-name "/bin/bash")
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(msb-display-invisible-buffers-p t)
 '(msb-max-menu-items nil)
 '(package-selected-packages
   (quote
    (ergoemacs-functions u-clips u-cc company-ycmd flycheck-ycmd ycmd window-purpose windows-purpose e2wm ssh web-mode langtool bm loccur fancy-narrow nlinum nlinum-relative paradox linum-relative melancholy-theme ac-dabbrev async origami-mode origami plsense ereader clips-log-mode ac-clang afternoon-theme nxml-mode log-mode comint yaml-mode volatile-highlights use-package undo-tree tabbar sr-speedbar spu smooth-scrolling smartparens shift-number rainbow-delimiters powerline popwin php-mode perl6-mode org-cua-dwim neotree modern-cpp-font-lock minimap mic-paren matlab-mode lua-mode json-mode jedi highlight-operators highlight-escape-sequences folding flycheck-pos-tip f es-windows ergoemacs-mode delight cursor-in-brackets cuda-mode csharp-mode color-theme-solarized clips-mode autopair anzu ac-python)))
 '(paradox-github-token t)
 '(recentf-max-menu-items 25)
 '(recentf-menu-before "Open in New Window...")
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (enable-local-variables: . all))))
 '(scroll-bar-mode (quote right))
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position t)
 '(ssh-directory-tracking-mode t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
