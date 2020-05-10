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
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode c++-mode erlang-mode haskell-mode jde-mode lua-mode python-mode)))
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
   '(treemacs treemacs-magit treemacs-projectile minions filladapt u-cc ide-cpp clean-aindent-mode groovy ccls company-rtags flycheck-rtags rtags ggtags makefile-executor groovy-mode company-lsp lsp-ui eglot declutter org-mode tinyeat tetrist pretty-column elisp--witness--lisp magit async anzu bm clang-format clips-mode company-jedi company-plsense company-ycmd csharp-mode cuda-mode delight ergoemacs-functions ergoemacs-mode f flycheck-pos-tip flycheck-ycmd highlight-escape-sequences highlight-operators json-mode langtool loccur lua-mode matlab-mode mic-paren modern-cpp-font-lock neotree nxml-mode org-cua-dwim paradox perl6-mode plsense popwin powerline rainbow-delimiters shift-number smartparens smooth-scrolling ssh tabbar undo-tree use-package volatile-highlights web-mode yaml-mode ycmd))
 '(paradox-github-token t)
 '(powerline-gui-use-vcs-glyph t)
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

(message "setting faces...")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-default ((t (:inherit tabbar-default))))
 '(centaur-tabs-modified-marker-selected ((t (:background "nil" :foreground "red"))))
 '(centaur-tabs-modified-marker-unselected ((t (:inherit tabbar-unselected :foreground "red"))))
 '(centaur-tabs-selected ((t (:inherit tabbar-selected))))
 '(centaur-tabs-selected-modified ((t (:inherit tabbar-selected-modified))))
 '(centaur-tabs-unselected ((t (:inherit tabbar-unselected))))
 '(centaur-tabs-unselected-modified ((t (:inherit tabbar-unselected-modified))))
 '(line-number-current-line ((t (:background "white")))))

;;; custom.el ends here
