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
 '(anzu-mode-line-update-function 'anzu--update-mode-line-local)
 '(company-dabbrev-code-modes
   '(prog-mode batch-file-mode csharp-mode c++-mode erlang-mode haskell-mode jde-mode lua-mode python-mode))
 '(custom-enabled-themes '(fontaine))
 '(custom-safe-themes t)
 '(flycheck-flake8-maximum-line-length 200)
 '(package-selected-packages
   '(dash consult consult-flycheck consult-selectrum marginalia selectrum ivy-posframe wgrep smex flx counsel ivy-rich ivy-hydra ivy unicode-fonts emojify powerline vterm anti-zenburn-theme acme-theme fontaine-theme rainbow-mode ws-butler treemacs treemacs-magit treemacs-projectile minions filladapt u-cc ide-cpp clean-aindent-mode groovy ccls company-rtags flycheck-rtags rtags ggtags makefile-executor groovy-mode company-lsp lsp-ui eglot declutter org-mode tinyeat tetrist pretty-column elisp--witness--lisp magit async anzu bm clang-format clips-mode company-jedi company-plsense company-ycmd csharp-mode cuda-mode delight ergoemacs-functions ergoemacs-mode f flycheck-pos-tip flycheck-ycmd highlight-escape-sequences highlight-operators json-mode langtool loccur lua-mode matlab-mode mic-paren modern-cpp-font-lock neotree nxml-mode org-cua-dwim paradox perl6-mode plsense popwin rainbow-delimiters shift-number smartparens smooth-scrolling ssh tabbar undo-tree use-package volatile-highlights web-mode yaml-mode ycmd))
 '(paradox-github-token t)
 '(powerline-gui-use-vcs-glyph t)
 '(rainbow-x-colors nil)
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (enable-local-variables: . all)))
 '(tab-width 4)
 '(undo-tree-history-directory-alist '(("~undo-tree~" . "~/.config/emacs/undo-tree"))))

(message "setting faces...")


;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
