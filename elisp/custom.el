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
